#!/bin/sh
read -r _input || exit 0
printf '{"schema_version":1,"request_id":"stale-request","ok":true,"result":{"tasks":[]}}\n'
