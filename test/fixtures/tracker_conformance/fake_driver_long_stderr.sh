#!/bin/sh
read -r input || exit 0
count=0
while [ "$count" -lt 5000 ]; do
  printf 'x' >&2
  count=$((count + 1))
done
printf '\n' >&2
request_id=$(printf '%s' "$input" | sed -n 's/.*"request_id":"\([^"]*\)".*/\1/p')
printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[]}}\n' "$request_id"
