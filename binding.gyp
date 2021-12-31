{
  "targets": [
    {
      "target_name": "tree_sitter_haskell_binding",
      "include_dirs": [
        "<!(node -e \"require('nan')\")",
        "src"
      ],
      "sources": [
        "src/parser.c",
        "bindings/node/binding.cc",
        "src/scanner.cc",
      ],
      "cflags_c": [
        "-std=c++14",
      ],
      'xcode_settings': {
        'CLANG_CXX_LANGUAGE_STANDARD': 'c++14',
      },
    }
  ]
}