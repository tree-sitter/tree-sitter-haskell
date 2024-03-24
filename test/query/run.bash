#!/usr/bin/env bash

base=$(dirname $0)

source $base/../common.bash

query() {
  echo $base/${1}.query
}

query_test_file() {
  if [[ $mode == 'native' ]]
  then
    tree-sitter query $(query $1) $2
  elif [[ $mode == 'wasm' ]]
  then
    message "Query tests can't be run in wasm."
    exit 1
  else
    message "Invalid mode: $mode"
    exit 1
  fi
}

test_files 'query' query_test_file
exit
