{
  description = "The Haskell grammar for tree-sitter";

  inputs = {
    hix.url = "https://flakehub.com/f/tek/hix/~0.6.tar.gz";
    rust-overlay.url = "github:oxalica/rust-overlay";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = {hix, rust-overlay, nix-filter, ...}: hix.lib.pro ({config, lib, util, ...}: {

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "incipit-base";
          version = ">= 0.5";
        };
        module = "IncipitBase";
      };
      paths = false;
    };

    packages.tools = {
      src = ./tools;
      library = {
        enable = true;
        dependencies = [
          "exon >= 1.4 && < 1.7"
          "optparse-applicative ^>= 0.17"
          "path ^>= 0.9"
          "path-io >= 1.7 && < 1.9"
          "transformers"
        ];
      };
      executable.enable = true;
      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.3"
          "path ^>= 0.9"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };
      override = {fast, ...}: fast;
    };

    envs.dev.buildInputs = pkgs: [pkgs.nodejs pkgs.tree-sitter pkgs.emscripten pkgs.python3];

    outputs = let
      inherit (config) pkgs;

      outputs = import ./nix/outputs.nix { inherit config pkgs rust-overlay; filter = nix-filter.lib; };

    in {
      packages = {
        default = lib.mkForce outputs.tree-sitter-haskell;
        inherit (outputs) bitmap-test tree-sitter-haskell rust;
        parser-src = outputs.parserSrc;
        parser-lib = outputs.parserLib;
        parser-wasm = outputs.parserWasm;
      };

      apps = {
        gen-bitmaps = util.app outputs.gen-bitmaps;
        bench-all = util.app (outputs.bench "effects postgrest polysemy ivory haskell-language-server");
        bench-hls = util.app (outputs.bench "haskell-language-server");
        bench-ghc = util.app (pkgs.writeScript "bench-ghc" "${outputs.benchWith {warmup = 0; max = 3;} "tsh-test-ghc/compiler"} 3");
        bench-libs = util.app (outputs.bench-libs);
        files = util.app outputs.collectFiles;
        tests = util.app outputs.tests;
        unit-tests = util.app outputs.unit-tests;
        parse = util.app "${outputs.rust}/bin/parse";
        report = util.app outputs.report;
        report-mem = util.app outputs.report-mem;
        report-size = util.app outputs.report-size;
        report-quick = util.app outputs.report-quick;
      };

      devShells = {

        parser = pkgs.mkShell {
          name = "tree-sitter-haskell";
          packages = [pkgs.tree-sitter];
          shellHook = ''
          export TREE_SITTER_LIBDIR="$PWD/.lib"
          mkdir -p $TREE_SITTER_LIBDIR
          '';
        };

        rust = pkgs.mkShell {
          name = "tree-sitter-haskell rust";
          packages = [pkgs.rustc pkgs.cargo];
        };

      };

    };

  });
}
