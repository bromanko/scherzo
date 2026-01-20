#!/usr/bin/env bash
set -euo pipefail

# CI script for scherzo
# Runs all checks: formatting, tests, and build

echo "=== Scherzo CI ==="

# Initialize jj if not present (needed for jj_test.gleam tests)
# GitHub Actions checkout only creates .git, but tests expect .jj
if [[ ! -d ".jj" ]]; then
    echo "Initializing jj repository (colocated with git)..."
    jj git init --colocate
fi

echo ""
echo "=== Checking Gleam formatting ==="
gleam format --check src test

echo ""
echo "=== Checking Nix formatting ==="
nixfmt --check ./*.nix

echo ""
echo "=== Running tests ==="
gleam test

echo ""
echo "=== Building ==="
gleam build

echo ""
echo "=== All checks passed ==="
