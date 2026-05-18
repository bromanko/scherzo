#!/usr/bin/env bash
set -euo pipefail

# The devenv shell includes pi from numtide/llm-agents.nix; make SelfCI use
# Numtide's binary cache instead of source-building the pi package.
numtide_nix_config="$(cat <<'NIX_CONFIG'
accept-flake-config = true
extra-substituters = https://cache.numtide.com
extra-trusted-public-keys = niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=
NIX_CONFIG
)"
if [ -n "${NIX_CONFIG:-}" ]; then
  export NIX_CONFIG="${NIX_CONFIG}"$'\n'"${numtide_nix_config}"
else
  export NIX_CONFIG="${numtide_nix_config}"
fi

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
run_step "structured output contract" direnv exec . scripts/scherzo-structured-output-contract check-workflows --output-dir tmp/scherzo-structured-output-contract/selfci
run_step "review lane contract offline (implementation)" direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/selfci/implementation
run_step "review lane contract offline (execplan implementation)" direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/execplan-implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/selfci/execplan-implementation
run_step "clean test/tmp" rm -rf test/tmp
run_step "gleam unit test" direnv exec . scherzo-test-unit
run_step "gleam contract test" direnv exec . scherzo-test-contract
run_step "nix flake check" nix flake check --print-build-logs
