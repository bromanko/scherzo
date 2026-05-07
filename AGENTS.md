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

## Production lint policy

`glinter` is the source of truth for the production safety policy in `src/`. The required gate is:

```sh
direnv exec . gleam run -m glinter
```

Do not add production `let assert`, `panic`, or `todo`; glinter enforces these as errors for `src/` via `gleam.toml`. Tests may still use `let assert`. If a production invariant is intentionally process-fatal, prefer explicit error handling first; any `// nolint:` suppression must be as narrow as possible and include a reason.

## Linear CLI

The devenv provides `linear` and `lc` wrappers for `linear-cli`. When an agent needs to read or update Linear, prefer the CLI over hand-written `curl` GraphQL calls:

```sh
direnv exec . lc issue view LIV-123 --json
direnv exec . lc issue query --team LIV --json
direnv exec . lc issue comment add LIV-123 --body "Done"
```

The wrappers use `LINEAR_API_KEY` and fall back to `SCHERZO_AGENT_LINEAR_API_KEY` when `LINEAR_API_KEY` is unset. Keep tokens out of logs. Use direct `curl https://api.linear.app/graphql` only when `lc`/`linear` lacks the required operation or for low-level Scherzo transport diagnostics.

## Async Gleam tests

Prefer deterministic handshakes over ad hoc sleeps. For fake actors/workers that need to stay alive, use `test/test_async.gleam` barriers (`new_barrier`, `block_until_released`, `release_barrier`) rather than long `process.sleep` calls. Use `release_barrier_if_waiting` for cleanup when the test intentionally kills the worker first. After a synchronization point, use `drain_subject` or `assert_no_extra_message(_within)` for no-extra-message checks instead of raw `process.receive(..., within: 20) == Error(Nil)`. See `test/README.md` for the full pattern.
