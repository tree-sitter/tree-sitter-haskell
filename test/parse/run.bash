#!/usr/bin/env bash

base=$(dirname $0)

source $base/../common.bash

parse_test_file() {
  if [[ $mode == 'native' ]]
  then
    tree-sitter parse $2 | sed -re "s#${2}.*bytes/ms#${2}#"
  elif [[ $mode == 'wasm' ]]
  then
    ./test/parse-wasm.mjs $2
  else
    message "Invalid mode: $mode"
    exit 1
  fi
}

test_files 'parse' parse_test_file
