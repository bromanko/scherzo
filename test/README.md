# Test guidance

## Suite selection

`direnv exec . gleam test` and `direnv exec . scherzo-test-unit` run the deterministic unit suite. Shell-heavy helper-script, workflow, renderer, daemon/service, port/process, pi-client, CLI, tracker-conformance, and workspace-driver contract tests are excluded from that default loop and run with `direnv exec . scherzo-test-contract` (or `gleam test -- --suite contract`).

The local integration and real pi validation suites remain explicit: use `direnv exec . scherzo-test-local-integration` for local jj/workspace integration and `direnv exec . scherzo-test-real-pi-validation` only when `pi` plus provider credentials are available.

## Async process synchronization

Prefer explicit handshakes over sleeps in async tests. If a fake actor, daemon dependency, or worker needs to stay alive while the test sends a control command or inspects state, use `test/test_async.gleam` instead of `process.sleep/1`.

Recommended pattern:

1. Create a barrier with `test_async.new_barrier()`.
2. Have the fake worker call `test_async.block_until_released(barrier)` after it has sent a readiness message.
3. Wait for that readiness message or another deterministic synchronization point.
4. Make assertions or send the operator command.
5. Release the barrier with `test_async.release_barrier(barrier)` before shutdown. If the test intentionally verifies that the worker is killed before it can be released, use `release_barrier_if_waiting` as cleanup.

For subjects used as probes:

- Use `test_async.expect_message` / `expect_message_within` for positive receives.
- Use `test_async.drain_subject` after a synchronization point to inspect any already-produced messages.
- Use `test_async.assert_no_extra_message` / `assert_no_extra_message_within` only after the test has synchronized with the process under test.

Short polling sleeps are acceptable for external OS/process boundaries or eventually-consistent projections when there is no in-process handshake available. Long sleeps to keep a worker alive and raw negative assertions such as `process.receive(subject, within: 20) == Error(Nil)` should be replaced with the shared helpers above.
