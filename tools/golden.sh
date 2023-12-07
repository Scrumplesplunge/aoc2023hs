#!/bin/bash

day=$((10#${1?}))
file="$(printf 'build/day%02d.output' "$day")"
if ! [[ -f "$file" ]]; then
  >&2 echo "No output for day $day"
  exit 1
fi
mkdir -p inputs
cp "$file" inputs
