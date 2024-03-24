#!/usr/bin/env bash

set -e

base=$(dirname $0)

src() {
  echo $base/${1}.hs
}

target() {
  echo $base/${1}.target
}

if [[ $1 == 'update' ]]
then
  update=1
  shift
fi

  tree-sitter test -f 'just compile it' >/dev/null

test_file() {
  local name=$1
  if ! diff $(target $name) <(tree-sitter parse $(src $name))
  then
    if [[ -n $update ]]
    then
      echo ""
      echo -e "\e[35m>>>\e[m Updating parse test \e[34m${name}\e[m."
      echo ""
      tree-sitter parse $(src $name) > $(target $name)
      unset update
    else
      echo ""
      echo -e "\e[35m>>>\e[m \e[32m< expected\e[m  |  \e[31m> parse output\e[m"
      echo -e "\e[35m>>>\e[m Parse test \e[34m${name}\e[m failed."
      return 1
    fi
  fi
}

if [[ -n $1 ]]
then

  if [[ -f "$(target $1)" ]]
  then
    test_file $1
  else
    echo -e "\e[35m>>>\e[m \e[31mNo such test: \e[34m${1}\e[m"
    exit 1
  fi

else

  for n in $base/*.target
  do
    test_file $(basename ${n%.*})
  done

fi

echo -e "\e[35m>>>\e[m All parse tests succeeded."
