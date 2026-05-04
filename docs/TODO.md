# Scherzo TODO

This file tracks follow-up improvements that are intentionally outside the initial implementation plan in `docs/plans/implement-scherzo.md`.

## Future improvements

- [ ] Design Linear communication protocol for agent results, comments, and issue edits.

  Scherzo currently posts operational handoff comments such as claim, success, and failure, but it does not post the actual task result from pi. A future plan should define the higher-level protocol for what Scherzo writes back to Linear and how humans communicate back to Scherzo.

  The Scherzo-to-Linear result-comment half is now planned in `docs/plans/linear-session-results.md`. The first Linear-to-Scherzo command/comment transport was implemented from `docs/plans/linear-command-transport.md`, and durable command receipts were added in `docs/plans/hardening-05-durable-linear-command-inbox.md`; webhook wake-up remains separate follow-up work below.

  Initial design notes to preserve for future plans:

  - Keep Scherzo, not pi, as the component that writes to Linear. pi may produce proposed result text, but Scherzo should redact, truncate, structure, and post it.
  - Capture the final assistant response or a turn summary from pi and include it in `runner.WorkerSuccess`, probably as `final_response: Option(String)` or a richer result artifact type.
  - Add a success/result comment that contains the Scherzo run ID, the useful task result, and concise metadata such as token totals and changed files when available.
  - Keep operational comments separate from task-result comments, or deliberately combine completion metadata with result content in one well-structured comment to reduce noise.
  - Prefer append-only Linear comments before editing issue descriptions. Issue edits are harder to audit and should require a separate explicit design.
  - Keep Linear human-control syntax aligned with `scherzoctl` commands; avoid arbitrary `@scherzo ...` parsing and avoid Linear-only aliases unless a future command-model change explicitly justifies them.
  - Define how comment polling, edited issue descriptions, labels, and state changes interact with parked issues and running workers.
  - Add deterministic tests for final-response capture, result-comment formatting, redaction/truncation, and duplicate/retry behavior.

- [ ] Add Linear webhook wake-up.

  Linear command receipts are now durable in the local ledger, so restart dedupe and completed-unacked acknowledgement replay no longer depend only on process memory. The remaining transport limitation is wake-up and discovery: Scherzo still polls only observed issues and only the bounded `poll_limit_per_issue` comments returned by Linear.

  A future plan should add webhook delivery with signature verification, retry semantics, bounded coexistence with polling, and clear Linear-side idempotency rules. It should preserve the durable receipt replay rules and define how webhook deliveries interact with edited comments and old comments outside the polling window.

- [ ] Add tmux-backed live pi session access.

  Operators should be able to view and interact with live pi sessions by connecting to a tmux socket. Each running Scherzo worker should have its own tmux session so an operator can attach to one issue without disturbing others.

  Initial design notes to preserve for a future plan:

  - Add optional workflow/config keys for tmux support, probably under `pi` or a new `operator_ui` section.
  - Start each pi subprocess inside a dedicated tmux session when tmux mode is enabled.
  - Use deterministic session names based on the Linear issue identifier and run/session generation, sanitized with the same safety rules used for workspace names.
  - Store the tmux socket path and session name in the running-worker metadata and include them in structured logs.
  - Document attach commands, for example `tmux -S <socket-path> attach -t <session-name>`.
  - Ensure Scherzo can still terminate or clean up the underlying pi process and tmux session during cancellation, terminal issue cleanup, and shutdown.
  - Keep the default non-tmux RPC execution path available for simple deployments and tests.
  - Add deterministic tests with a fake tmux wrapper or dependency injection rather than requiring real tmux in the main test suite.
