#!/usr/bin/env bash

mode=${1:-native}
root=$(cd "$(dirname "$0")/.."; pwd -P)

message() {
  echo -e "\e[35m>>>\e[m $*"
}

if [[ "$mode" == 'native' ]]
then
  tree-sitter parse -q /dev/null
  success=$(( $? == 0 ))
elif [[ "$mode" == 'wasm' ]]
then
  # Using temp dir because TS leaves some build products in the working dir
  tmp=$(mktemp --tmpdir -d tsh-wasm-XXXX)
  pushd "$tmp" >/dev/null
  tree-sitter build --wasm -o ${TREE_SITTER_LIBDIR-$root}/haskell.wasm $root >/dev/null
  success=$(( $? == 0 ))
  popd >/dev/null
  rm -rf $tmp
fi

if [[ $success == 0 ]]
then
  message "Compiling the parser failed."
  if [[ $mode == 'wasm' ]]
  then
    message "Try running \e[34mnix develop\e[m to set up wasm dependencies."
  fi
  exit 1
fi
