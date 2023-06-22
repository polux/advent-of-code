# Copyright 2023 Google LLC.
# SPDX-License-Identifier: Apache-2.0

function aoci {
  today=$( date +%d )
  day=${1:-$today}
  dir=day${day}-1
  if [ -d ${dir} ]; then
    echo "${dir} already exists, aborting"
  else
    cp -r ../template ${dir}
    cd ${dir}
    code src/Main.hs
    aocd $day > input
    ghcid --test=:main
  fi
}

function aocp {
  dir=$(basename $PWD)
  day=$(echo $dir | sed 's/day\(.*\)-1/\1/')
  cd ..
  cp -r day${day}-1 day${day}-2
  cd day${day}-2
  code src/Main.hs
  ghcid --test=:main
}
