{config, pkgs, rust-overlay, filter}: let

  tools = config.outputs.packages.tools;

  grammarSrc = filter {
    root = ../.;
    include = [
      "grammar"
      (filter.matchExt "js")
    ];
  };

  parserSrc = pkgs.stdenv.mkDerivation {
    name = "tree-sitter-haskell-parser-sources";
    version = "1.0.0";
    src = grammarSrc;
    nativeBuildInputs = [pkgs.nodejs pkgs.tree-sitter];

    buildPhase = ''
    runHook preBuild
    tree-sitter generate
    runHook postBuild
    '';

    installPhase = ''
    runHook preInstall
    install -Dt $out/src src/parser.c src/grammar.json src/node-types.json
    runHook postInstall
    '';

  };

  libSrc = filter {
    root = ../.;
    include = [
      "src"
      (filter.matchExt "h")
      "src/scanner.c"
    ];
  };

  parserLib = pkgs.stdenv.mkDerivation {
    name = "tree-sitter-haskell-parser-lib";
    version = "1.0.0";
    src = pkgs.symlinkJoin { name = "lib-source"; paths = [parserSrc libSrc]; };
    stripDebugList = ["haskell.so"];

    buildPhase = ''
    runHook preBuild
    $CC -fPIC -shared -Isrc -O2 -o haskell.so src/parser.c src/scanner.c
    runHook postBuild
    '';

    installPhase = ''
    runHook preInstall
    install -Dt $out/lib haskell.so
    runHook postInstall
    '';

  };

  parserWasm = pkgs.stdenv.mkDerivation {
    name = "tree-sitter-haskell-parser-wasm";
    version = "1.0.0";
    src = pkgs.symlinkJoin { name = "wasm-source"; paths = [parserSrc libSrc]; };
    nativeBuildInputs = [pkgs.nodejs pkgs.tree-sitter pkgs.emscripten];

    buildPhase = ''
    runHook preBuild
    mkdir -p .emscriptencache
    export EM_CACHE=$(pwd)/.emscriptencache
    tree-sitter build-wasm
    runHook postBuild
    '';

    installPhase = ''
    runHook preInstall
    mkdir -p $out/
    cp tree-sitter-haskell.wasm $out/
    runHook postInstall
    '';
  };

  tree-sitter-haskell = pkgs.symlinkJoin {
    name = "tree-sitter-haskell";
    paths = [parserLib parserWasm];
  };

  rustSrc = filter {
    root = ../.;
    include = [
      "Cargo.toml"
      "Cargo.lock"
      "test/rust"
      "bindings"
      "package.json"
      "package-lock.json"
      "queries"
    ];
  };

  rustPkgs = rust-overlay.inputs.nixpkgs.legacyPackages.${pkgs.system};

  rust = rustPkgs.rustPlatform.buildRustPackage {
    pname = "tree-sitter-haskell";
    version = "1.0.0";
    src = pkgs.symlinkJoin { name = "rust-source"; paths = [parserSrc libSrc rustSrc]; };

    cargoLock.lockFile = ../Cargo.lock;

    nativeBuildInputs = [pkgs.nodejs pkgs.tree-sitter];
  };

  bitmap-test = pkgs.stdenv.mkDerivation {
    name = "bitmap-test";
    src = ../tools/UnicodeData.txt;
    dontUnpack = true;
    doCheck = true;
    nativeBuildInputs = [tools];

    buildPhase = ''
    runHook preBuild
    mkdir $out
    tools bitmap-test --unicode-data=$src --file=$out/bitmap-test.c --gap-size=10000
    runHook postBuild
    '';

    checkPhase = ''
    runHook preCheck
    gcc -o ./test-bitmap $out/bitmap-test.c
    ./test-bitmap
    runHook postCheck
    '';

  };

  gen-bitmaps = pkgs.writeScript "gen-bitmaps" ''
  set -e
  gap_size=''${1-10000}
  ${tools}/bin/tools bitmap --preset=id --file=$PWD/src/id.h --gap-size=$gap_size
  ${tools}/bin/tools bitmap --preset=varid-start --file=$PWD/src/varid-start.h --gap-size=$gap_size
  ${tools}/bin/tools bitmap --preset=conid-start --file=$PWD/src/conid-start.h --gap-size=$gap_size
  ${tools}/bin/tools bitmap --preset=symop --file=$PWD/src/symop.h --gap-size=$gap_size
  ${tools}/bin/tools bitmap --category=Zs --file=$PWD/src/space.h --gap-size=$gap_size --name=space
  '';

  collectFiles = pkgs.writeScript "collect-files" ''
  known="test/known-failures/''${1##*/}.txt"
  if [[ -e $known ]]
  then
    ${pkgs.coreutils-full}/bin/comm -13 <(sort $known) <(find test/libs/$1 -name '*.hs' | sort)
  else
    find test/libs/$1 -name '*.hs' | sort
  fi
  '';

  collectFilesBench = pkgs.writeScript "collect-files-bench" ''
  ${collectFiles} $1 > $2
  '';

  parseFiles = pkgs.writeScript "parse-files" ''
  tree-sitter parse -q $(< $1)
  '';

  compileQuiet = pkgs.writeScript "tree-sitter-haskell-compile-quiet" ''
  export TREE_SITTER_LIBDIR="$PWD/.lib"
  tree-sitter test -f 'only compile the parser' >/dev/null
  '';

  resolveLib = ''
  ghc_lib="tsh-test-ghc/libraries/$lib"
  if [[ $lib == compiler ]]
  then
    lib="tsh-test-ghc/compiler"
  elif [[ -d "test/libs/$ghc_lib" ]]
  then
    lib=$ghc_lib
  fi
  '';

  benchWith = {warmup ? 3, max ? null}: libs: let
    extra = if max == null then "" else "--max-runs ${toString max}";
  in pkgs.writeScript "bench" ''
  files=$(mktemp --tmpdir)
  trap "rm -f $files" EXIT
  ${compileQuiet}
  for lib in ${libs}
  do
    ${resolveLib}
    ${pkgs.hyperfine}/bin/hyperfine \
      --warmup ${toString warmup} \
      --command-name "$lib" \
      --min-runs ''${1-10} \
      --setup "${collectFilesBench} $lib $files" \
      ${extra} \
      "${parseFiles} $files"
  done
  '';

  bench = benchWith {};

  bench-libs = pkgs.writeScript "bench-specified" ''
  export test_libs=$@
  ${bench "$test_libs"} 10
  '';

  unit-tests = pkgs.writeScript "tree-sitter-haskell-unit-tests" ''
  set -e
  tree-sitter test
  test/parse/run.bash
  test/query/run.bash
  '';

  tests = pkgs.writeScript "tree-sitter-haskell-tests" ''
  set -e
  ${unit-tests}
  test/parse-libs
  '';

  time = "${pkgs.time}/bin/time";

  report-mem = let
    parseQuiet = pkgs.writeScript "parse-hls-quiet" ''
    tree-sitter parse -q "$@" >/dev/null
    '';
  in pkgs.writeScript "tree-sitter-haskell-report-mem" ''
  set -Eeo pipefail
  export TREE_SITTER_LIBDIR="$PWD/.lib"
  ${compileQuiet}
  files=$(${collectFiles} tsh-test-ghc/compiler)
  echo -en '\e[35m>>>\e[m \e[34mMemory\e[m: '
  ${time} -f '%M rss | %c invol | %w vol' ${parseQuiet} $files
  '';

  report-quick = pkgs.writeScript "tree-sitter-haskell-report-quick" ''
  set -eo pipefail
  export TREE_SITTER_LIBDIR="$PWD/.lib"
  echo -en '\e[35m>>>\e[m \e[34mCompile\e[m: '
  ${time} -f '%es (%c, %w)' ${compileQuiet}
  echo -e "\e[35m>>>\e[m \e[34mFile size\e[m: $(stat -c %s $TREE_SITTER_LIBDIR/haskell.so)"
  ${unit-tests} >/dev/null || (echo -e '\e[35m>>>\e[m \e[31mTests failed!\e[m' && false)
  ${report-mem}
  '';

  report-size = pkgs.writeScript "tree-sitter-haskell-report-size" ''
  set -eo pipefail
  export TREE_SITTER_LIBDIR="$PWD/.lib"
  echo -en '\e[35m>>>\e[m \e[34mGenerate\e[m: '
  ${time} -f '%es (%c, %w)' tree-sitter generate
  ${report-quick}
  '';

  report = pkgs.writeScript "tree-sitter-haskell-report" ''
  set -eo pipefail
  export TREE_SITTER_LIBDIR="$PWD/.lib"
  ${report-size}
  test/parse-libs
  ${bench "postgrest haskell-language-server semantic"}
  '';

in {
  inherit
    parserSrc
    parserLib
    parserWasm
    bitmap-test
    gen-bitmaps
    tree-sitter-haskell
    bench
    bench-libs
    benchWith
    unit-tests
    tests
    rust
    report
    report-mem
    report-size
    report-quick
    ;
}
