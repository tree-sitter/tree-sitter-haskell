src() {
  echo $base/${1}.hs
}

target() {
  echo $base/${1}.target
}

message() {
  echo -e "\e[35m>>>\e[m $*"
}

tests=()
update='false'
mode='native'

for arg in $@
do
  if [[ $arg == '--update' ]]
  then
    update='true'
  elif [[ $arg == '--wasm' ]]
  then
    mode='wasm'
  else
    tests+=($arg)
  fi
done

if ! ${base}/../ensure-parser.bash $mode
then
  exit 1
fi

test_file() {
  local sort=$1 name=$2 cmd=$3 parsed desc
  desc="$sort test \e[34m${name}\e[m"
  parsed=$(eval $cmd $name $(src $name))
  if [[ ! -f $(target $name) ]]
  then
    if [[ $update == 'true' ]]
    then
      echo ""
      message "Initializing target file for ${desc}."
      echo ""
      echo "$parsed" > $(target $name)
      unset update
    else
      echo "$parsed"
      echo ""
      message "Target file for ${desc} does not exist."
      message "Rerun with \e[34m--update\e[m to create it."
      exit 1
    fi
  elif ! diff $(target $name) <(echo "$parsed")
  then
    if [[ $update == 'true' ]]
    then
      echo ""
      message "Updating ${desc}."
      echo ""
      echo "$parsed" > $(target $name)
      unset update
    else
      echo ""
      message "\e[32m< expected\e[m  |  \e[31m> $sort output\e[m"
      message "Failure in ${desc}."
      exit 1
    fi
  fi
}

test_files() {
  local sort=$1 cmd=$2 arg

  if (( ${#tests[@]} > 0 ))
  then

    for arg in ${tests[@]}
    do
      if [[ -f "$(src $arg)" ]]
      then
        test_file $sort $arg $cmd
      else
        message "\e[31mNo such test: \e[34m${arg}\e[m"
        exit 1
      fi
    done

  else

    for n in $base/*.target
    do
      test_file $sort "$(basename ${n%.*})" $cmd
    done

  fi

  message "All $sort tests succeeded."
}
