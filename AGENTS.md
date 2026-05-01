# Agent Notes

## Direnv in workspaces

This repository uses `.envrc`/devenv to provide the expected toolchain and environment. In a fresh workspace or disposable checkout, `direnv exec . <command>` may fail with `.envrc is blocked` until the workspace-local `.envrc` has been approved.

When that happens:

1. Read or inspect `.envrc` if you have not already done so.
2. From the repository root, run `direnv allow .`.
3. Retry commands through direnv, for example:

   ```sh
   direnv exec . gleam test
   direnv exec . gleam format --check src test
   ```

Treat an unapproved `.envrc` as an environment setup issue, not as a code or test failure. Prefer the direnv-backed commands for validation. Use plain `gleam ...` only as a fallback when direnv is unavailable in the workspace and Gleam is already on `PATH`.
