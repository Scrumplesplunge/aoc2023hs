#!/bin/bash

name="${1?}"
golden="${2?}"
actual="${3?}"

temp="$(mktemp)"
if diff --color=always "$golden" "$actual" > "$temp"; then
  echo -e "$name: \x1b[32mPASSED\x1b[0m"
else
  echo -e "$name: \x1b[31mFAILED\x1b[0m"
  cat "$temp"
fi
rm "$temp"
