#!/bin/sh

path=$(cd "$(dirname "$0")"; pwd)

filesToProcess() {
  local listFile=filelist
  cat $listFile
}

flags=" --bdd \
  --interface --serializeAST \
  -I $path "


filesToProcess|while read i; do
         echo "Analysing $path/$i"
         echo "With settings: $flags"
         /Users/andi/Dropbox/Masterarbeit/Coding/TypeChef/typechef.sh $path/$i $flags
done
