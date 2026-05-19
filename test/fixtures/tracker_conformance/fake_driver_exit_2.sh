#!/bin/sh
read -r _input || exit 2
printf 'exit-two SECRET_TOKEN diagnostic\n' >&2
exit 2
