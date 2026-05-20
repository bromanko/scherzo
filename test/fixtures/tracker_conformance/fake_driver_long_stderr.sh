#!/bin/sh
read -r input || exit 0
count=0
while [ "$count" -lt 5000 ]; do
  printf 'x' >&2
  count=$((count + 1))
done
printf '\n' >&2
request_id=${input#*\"request_id\":\"}
if [ "$request_id" = "$input" ]; then
  request_id=
else
  request_id=${request_id%%\"*}
fi
printf '{"schema_version":1,"request_id":"%s","ok":true,"result":{"tasks":[]}}\n' "$request_id"
