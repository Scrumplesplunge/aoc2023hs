#!/bin/bash

day=$((10#${1?}))
if ! [[ -f .cookie ]]; then
  >&2 echo 'no cookie'
exit 1
fi
cookie="Cookie: $(cat .cookie)"
file="$(printf 'inputs/day%02d.txt' "$day")"
echo "day $day -> $file"
mkdir -p inputs
curl -H "$cookie" "https://adventofcode.com/2023/day/$day/input"  \
  > "$file.tmp" && mv "$file"{.tmp,}
