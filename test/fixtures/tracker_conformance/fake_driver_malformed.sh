#!/bin/sh
read -r _input || exit 0
printf 'malformed SECRET_TOKEN diagnostic\n' >&2
printf '{not-json}\n'
