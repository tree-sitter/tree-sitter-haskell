#!/usr/bin/env bash

set -e

base=$(dirname $0)

src() {
  echo $base/${1}.hs
}

query() {
  echo $base/${1}.query
}

target() {
  echo $base/${1}.target
}

if [[ $1 == 'update' ]]
then
  update=1
fi

  tree-sitter test -f 'just compile it' >/dev/null

test_file() {
  local name=$1
  if ! diff $(target $name) <(tree-sitter query $(query $name) $(src $name))
  then
    if [[ -n $update ]]
    then
      echo ""
      echo -e "\e[35m>>>\e[m Updating query test \e[34m${name}\e[m."
      echo ""
      tree-sitter query $(query $name) $(src $name) > $(target $name)
      unset update
    else
      echo ""
      echo -e "\e[35m>>>\e[m \e[32m< expected\e[m  |  \e[31m> query output\e[m"
      echo -e "\e[35m>>>\e[m Query test \e[34m${name}\e[m failed."
      return 1
    fi
  fi
}

for n in $base/*.target
do
  test_file $(basename ${n%.*})
done

echo -e "\e[35m>>>\e[m All query tests succeeded."
