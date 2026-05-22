#!/bin/sh
set -eu

pack=$1
action=$2
scenario=$3
state_dir="test/tmp/tracker-conformance/driver-state/$scenario"

fail() {
  printf '%s SECRET_TOKEN\n' "$1" >&2
  exit 1
}

ensure_clean_dir() {
  if [ -d "$state_dir" ] && find "$state_dir" -mindepth 1 -print -quit | grep -q .; then
    fail "leftover state for $scenario"
  fi
  rm -rf "$state_dir"
  mkdir -p "$state_dir"
}

require_file_contains() {
  file=$1
  needle=$2
  [ -f "$file" ] || fail "missing expected file $file"
  grep -F "$needle" "$file" >/dev/null || fail "missing expected marker in $file"
}

case "$pack:$action" in
  comments:setup)
    ensure_clean_dir
    mkdir -p "$state_dir/comments"
    printf 'comment-existing\n' >"$state_dir/comments/existing-id.txt"
    printf 'setup ok SECRET_TOKEN\n' >&2
    ;;
  comments:probe)
    require_file_contains "$state_dir/comments/create-only.txt" "[marker comments-create-only]"
    require_file_contains "$state_dir/comments/update-existing.txt" "[marker comments-update-existing]"
    require_file_contains "$state_dir/comments/fallback-create.txt" "[marker comments-update-missing-allow-create-fallback]"
    count=$(find "$state_dir/comments" -name 'update-existing*.txt' | wc -l | tr -d ' ')
    [ "$count" = "1" ] || fail "duplicate update marker files detected"
    printf 'probe ok SECRET_TOKEN\n' >&2
    ;;
  comments:cleanup)
    rm -rf "$state_dir"
    printf 'cleanup ok SECRET_TOKEN\n' >&2
    ;;
  state:setup)
    ensure_clean_dir
    printf 'todo\n' >"$state_dir/current-state.txt"
    printf 'setup ok SECRET_TOKEN\n' >&2
    ;;
  state:probe)
    require_file_contains "$state_dir/current-state.txt" "doing"
    require_file_contains "$state_dir/reason.txt" "[marker state-reason-propagation]"
    printf 'probe ok SECRET_TOKEN\n' >&2
    ;;
  state:cleanup)
    rm -rf "$state_dir"
    printf 'cleanup ok SECRET_TOKEN\n' >&2
    ;;
  *)
    fail "unknown fake conformance state command $pack:$action"
    ;;
esac
