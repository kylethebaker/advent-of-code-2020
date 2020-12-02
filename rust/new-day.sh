#!/bin/bash

set -e;

quit() { echo $1; exit 1; }

if [[ -z "$1" ]]; then
  quit "No day"
fi

day=$(printf day%02d "$1")

if [[ -d "./${day}" ]]; then
  quit "Directory already exists: ./${day}"
fi

cargo new --bin ${day}
touch "./${day}/input.txt"
