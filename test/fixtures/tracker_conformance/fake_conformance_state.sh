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

require_line_count() {
  file=$1
  expected=$2
  [ -f "$file" ] || fail "missing expected file $file"
  actual=$(wc -l <"$file" | tr -d ' ')
  [ "$actual" = "$expected" ] || fail "expected $expected line(s) in $file but found $actual"
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

  remote:setup)
    ensure_clean_dir
    mkdir -p "$state_dir/remote"
    printf 'setup ok SECRET_TOKEN\n' >&2
    ;;
  remote:probe)
    require_file_contains "$state_dir/remote/ack-receipt.txt" "[marker remote-ack-receipt]"
    require_file_contains "$state_dir/remote/ack-visible.txt" "[marker remote-ack-failure-visible]"
    case "$scenario" in
      remote-ack-duplicate-visible)
        require_line_count "$state_dir/remote/ack-retry.txt" 2
        ;;
      remote-probe-failure)
        fail "remote probe forced failure"
        ;;
      *)
        require_line_count "$state_dir/remote/ack-retry.txt" 1
        ;;
    esac
    printf 'probe ok SECRET_TOKEN\n' >&2
    ;;
  remote:cleanup)
    case "$scenario" in
      remote-cleanup-failure)
        fail "remote cleanup forced failure"
        ;;
      *)
        rm -rf "$state_dir"
        printf 'cleanup ok SECRET_TOKEN\n' >&2
        ;;
    esac
    ;;

  handoff:setup)
    ensure_clean_dir
    mkdir -p "$state_dir/handoff"
    printf 'setup ok SECRET_TOKEN\n' >&2
    ;;
  handoff:probe)
    require_file_contains "$state_dir/handoff/claim.txt" "workspace/main/SECRET_TOKEN"
    require_file_contains "$state_dir/handoff/success.txt" "summary SECRET_TOKEN"
    case "$scenario" in
      handoff-defective)
        :
        ;;
      *)
        require_file_contains "$state_dir/handoff/failure.txt" "reason SECRET_TOKEN"
        ;;
    esac
    require_file_contains "$state_dir/handoff/park.txt" "release policy SECRET_TOKEN"
    require_file_contains "$state_dir/handoff/legacy-claim.txt" "workspace/legacy/SECRET_TOKEN"
    require_file_contains "$state_dir/handoff/legacy-success.txt" "legacy success SECRET_TOKEN"
    require_file_contains "$state_dir/handoff/legacy-failure.txt" "legacy failure SECRET_TOKEN"
    require_file_contains "$state_dir/handoff/legacy-park.txt" "legacy park SECRET_TOKEN"
    case "$scenario" in
      handoff-duplicate-visible)
        retry_lines=2
        ;;
      handoff-probe-failure)
        fail "handoff probe forced failure"
        ;;
      *)
        retry_lines=1
        ;;
    esac
    require_line_count "$state_dir/handoff/retry-claim.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-success.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-failure.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-park.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-legacy-claim.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-legacy-success.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-legacy-failure.txt" "$retry_lines"
    require_line_count "$state_dir/handoff/retry-legacy-park.txt" "$retry_lines"
    printf 'probe ok SECRET_TOKEN\n' >&2
    ;;
  handoff:cleanup)
    case "$scenario" in
      handoff-cleanup-failure)
        fail "handoff cleanup forced failure"
        ;;
      *)
        rm -rf "$state_dir"
        printf 'cleanup ok SECRET_TOKEN\n' >&2
        ;;
    esac
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
