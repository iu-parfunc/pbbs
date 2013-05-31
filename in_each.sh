#!/bin/bash

# Run a command in EACH of the release directories.
CMDS=$*
root=`pwd`

# set -x
set -e

for dir in `cat $root/release`; do 
  for bench in `cat $root/$dir/release`; do 
    cd "$root/$dir/$bench/"
    echo;echo "  [Switched to $dir/$bench/]"
    $CMDS
  done
done

cd "$root"
