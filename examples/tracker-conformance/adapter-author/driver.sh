#!/bin/sh
set -eu

case "$0" in
  */*) script_path=$0 ;;
  *) script_path=$(command -v "$0" 2>/dev/null || printf '%s\n' "$0") ;;
esac
script_dir=$(CDPATH= cd "$(dirname "$script_path")" && pwd -P)
repo_root=$(CDPATH= cd "$script_dir/../../.." && pwd -P)
cd "$repo_root"
fixture_dir="test/fixtures/tracker_conformance"

mode=pass
while [ $# -gt 0 ]; do
  case "$1" in
    --mode)
      shift
      mode="${1:-}"
      ;;
    *)
      ;;
  esac
  shift
done

case "$mode" in
  pass)
    exec "$fixture_dir/fake_task_source_driver.sh" --scenario pass
    ;;
  secret-transcripts)
    exec "$fixture_dir/fake_task_source_driver.sh" --scenario secret-transcripts
    ;;
  malformed)
    exec "$fixture_dir/fake_driver_malformed.sh"
    ;;
  stale)
    exec "$fixture_dir/fake_driver_stale_envelope.sh"
    ;;
  *)
    printf 'unknown mode: %s\n' "$mode" >&2
    exit 64
    ;;
esac
