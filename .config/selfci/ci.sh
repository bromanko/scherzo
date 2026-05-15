#!/usr/bin/env bash
set -euo pipefail

run_step() {
  local name="$1"
  shift

  selfci step start "$name"
  "$@"
}

run_step "direnv allow" direnv allow .
run_step "gleam format" direnv exec . gleam format --check src test
run_step "glinter" direnv exec . gleam run -m glinter
run_step "scherzo custom lint" direnv exec . gleam run -m scherzo_lint
run_step "clean test/tmp" rm -rf test/tmp
run_step "gleam unit test" direnv exec . scherzo-test-unit
run_step "gleam contract test" direnv exec . scherzo-test-contract
run_step "nix flake check" nix flake check --print-build-logs
