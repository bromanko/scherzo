# LIV-556 Rank 4 operator and remote command runtime extraction review

This is a focused review document for the rank 4 slice of `docs/plans/LIV-523-daemon-decomposition-v2.md`. It plans the extraction only; it does not implement runtime changes.

## Purpose / Big Picture

Rank 4 should make Scherzo's daemon easier to maintain by moving operator-control and remote-command runtime decisions out of `src/scherzo/orchestrator/daemon.gleam` while preserving every operator-visible reply. After implementation, local control commands and remote commands should behave exactly as they do today, but the daemon should mostly receive mailbox messages, pass context into `operator_runtime.gleam` and `remote_command_runtime.gleam`, and send replies.

## Problem Framing and Constraints

The daemon remains a large compatibility actor; in the current tree `wc -l src/scherzo/orchestrator/daemon.gleam` reports 6565 lines. The rank 4 concern is the operator/remote command band: `ApplyOperatorCommand`, issue and parked-issue resolution, shell-only operator commands, synchronous worker and YAML-step command routing, last-result capture, remote dispatch pause reads, and remote-client callback wiring. `src/scherzo/orchestrator/control_command_handler.gleam` already owns command validation details such as prompt/UI size rejection and worker-reply-to-command-result shaping, and it must remain the command validation front door.

The implementation must preserve Linear command syntax, control API behavior, prompt and UI limits, worker command routing semantics, timeout behavior, remote command duplicate handling, and all current command statuses, reasons, and messages. This rank must not plan ranks 1-3 or 5-6 except as prerequisites or non-goals.

## Strategy Overview

Use a behavior-preserving strangler extraction. First add characterization tests around the current daemon behavior, then introduce `src/scherzo/orchestrator/operator_runtime.gleam` for daemon-state-backed operator resolution and synchronous shell command routing, and `src/scherzo/orchestrator/remote_command_runtime.gleam` for daemon-facing remote command application, completion shaping, transport action folding, and acknowledgement replay policy. The daemon should keep public startup, message receipt, remote-client lifecycle, and process ownership; the new modules should return state/results/actions that the daemon applies without changing public replies.

Remote protocol I/O and low-level routing already have code in `src/scherzo/control/remote/client.gleam` and `src/scherzo/control/remote_command_router.gleam`. Rank 4 should reuse or wrap that behavior rather than inventing a second protocol stack.

## Alternatives Considered

Leaving the code in `daemon.gleam` is the lowest-change option, but it does not solve the regrowth problem identified by LIV-523 and leaves operator command behavior scattered through the actor. Moving validation into `control_command_handler.gleam` was rejected because that module should remain a front door and reply mapper, not a daemon-state owner. Redesigning remote control or broadening supported remote commands was rejected because rank 4 is explicitly behavior-preserving.

## Risks and Countermeasures

The main risk is reply drift: a refactor can accidentally change a status, reason, or message that operators and remote clients depend on. The countermeasure is characterization tests before code motion and unchanged-command-reply assertions after each extraction step.

A second risk is duplicate or conflicting remote command acknowledgement behavior changing during the move. The countermeasure is explicit idempotence and conflict tests for same command id/same command replay, same command id/different command rejection, duplicate in-flight acknowledgement, and completed-result replay.

A third risk is timeout and routing drift for worker sessions and YAML step sessions. The countermeasure is targeted tests for worker-command timeout, abort fallback behavior, step-session routing, prompt/UI size rejection without routing, and the existing daemon control tests.

## Scope Boundaries

In scope: operator issue resolution, parked issue lookup, shell operator command execution, synchronous worker and YAML-step command routing, remote command completion shaping, remote transport action folding, and remote acknowledgement replay. The intended new files are `src/scherzo/orchestrator/operator_runtime.gleam` and `src/scherzo/orchestrator/remote_command_runtime.gleam`.

Out of scope: changing command syntax, adding remote command capabilities, changing prompt/UI limits, changing worker routing semantics, changing Linear tracker semantics, changing the control server/file API, changing remote wire envelopes, migrating documentation/helper infrastructure, changing provider-live or cache behavior, implementing ranks 1-3 or 5-6, or adding daemon boundary guardrails beyond what is needed to keep this extraction stable.

## Milestones

Milestone 1 is a characterization baseline. It is complete when the implementation branch has retained or added tests in `test/orchestrator_control_command_handler_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, `test/remote_command_router_test.gleam`, and `test/orchestrator_daemon_remote_client_test.gleam` or a new focused runtime test. The evidence must name tests for ambiguous issue refs, parked refs, prompt/UI size rejection with no worker route, remote acknowledgement replay idempotence, worker and remote timeout behavior, and unchanged command replies before any code is moved. If `test/orchestrator_daemon_linear_command_test.gleam` is absent, create it rather than dropping the Linear-command surface.

Milestone 2 extracts operator runtime. It is complete when `operator_runtime.gleam` owns issue lookup, parked lookup, shell commands, and synchronous worker/step routing while the daemon only builds context, invokes the runtime, stores returned state, and replies with the same `command.CommandResult` values. The implementer must re-run the retained control-command and daemon-control characterizations after delegation and record the exact evidence for `prompt_too_large`, `ui_response_too_large`, `worker_command_timeout`, ambiguous issue-ref rejection, parked issue lookup, and successful worker or YAML-step routing parity.

Milestone 3 extracts remote command runtime. It is complete when `remote_command_runtime.gleam` owns daemon-facing remote command application/completion/replay decisions, preserves the existing remote receipt/result/state ordering, and keeps low-level remote protocol behavior compatible. The implementer must keep `src/scherzo/control/remote/client.gleam` and `src/scherzo/control/remote_command_router.gleam` as protocol/router dependencies, not replacements, and record evidence for same id/same command duplicate in-flight handling, completed-result replay, same id/different command conflict rejection, unsupported remote command rejection, and `remote_command_timeout` behavior.

Milestone 4 removes duplicate daemon-local helpers and validates the boundary. It is complete when `daemon.gleam` no longer contains the extracted decision helpers, all targeted behavior tests pass, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` pass, and the implementation handoff records the deferred post-implementation human/operator dogfood check.

## Progress

- [x] (2026-05-28) Verified the prepared output target is the default directory `docs/plans`.
- [x] (2026-05-28) Read the LIV-523 v2 source plan and scoped this document to rank 4 only.
- [x] (2026-05-28) Checked current operator/remote command locations in `daemon.gleam`, `control_command_handler.gleam`, `daemon_remote_client.gleam`, `control/remote/client.gleam`, and `control/remote_command_router.gleam`.
- [x] (2026-05-28) Wrote this review document without implementing the extraction.
- [x] (2026-05-28) Incorporated review feedback by making acceptance evidence, milestone-specific test obligations, manual/operator dogfood timing, docs/helper migration scope, provider-live/cache non-goals, full validation, and linting explicit.
- [x] (2026-05-28) Re-checked that every required level-2 review section is present and non-empty with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-556-rank-4-operator-remote-command-runtime-extraction.md`.

## Decision Log

- Decision: Keep `control_command_handler.gleam` as the command validation front door. Rationale: it already owns prompt/UI size checks and worker-reply mapping, and moving daemon state into it would make the boundary less clear. Date: 2026-05-28.
- Decision: Use two new orchestrator runtime modules rather than one mixed module. Rationale: local operator state/routing and remote acknowledgement/replay policy have different dependencies and tests. Date: 2026-05-28.
- Decision: Preserve existing remote protocol modules and wrap or delegate to them from the new runtime. Rationale: rank 4 is an extraction, not a remote protocol redesign. Date: 2026-05-28.
- Decision: Defer live operator dogfood evidence until after the implementation workflow unless a reviewer explicitly requests it. Rationale: automated characterization tests are the pre-publish gate for this behavior-preserving extraction, while live remote/operator checks can be environment-dependent. Date: 2026-05-28.
- Decision: Make review-feedback obligations explicit in both this document and the implementation pack. Rationale: the extraction should not be accepted without visible characterization evidence, full validation and lint evidence, clear manual/operator-check timing, and explicit non-goals for docs/helper migration and provider-live/cache behavior. Date: 2026-05-28.

## Validation and Acceptance

The implementation must keep `test/orchestrator_control_command_handler_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, and `test/orchestrator_daemon_linear_command_test.gleam` as retained characterization surfaces. If the implementation branch does not already contain `test/orchestrator_daemon_linear_command_test.gleam`, create it at that path for the Linear-command daemon characterizations rather than omitting the required surface. Remote-command coverage must live in `test/orchestrator_daemon_remote_client_test.gleam`, `test/remote_command_router_test.gleam`, or a new focused runtime test; the implementer must not remove existing router/client tests as a substitute for the daemon-facing extraction tests.

Acceptance requires tests for ambiguous issue refs, parked refs, prompt/UI size rejection, remote ack replay idempotence, timeout behavior, and unchanged command replies. The implementation handoff must include the relevant test names or output from the characterization files above. Prompt/UI size tests must assert the existing `prompt_too_large` and `ui_response_too_large` rejection reasons and that no worker route is invoked. Timeout tests must assert existing `worker_command_timeout` and `remote_command_timeout` behavior. Remote replay tests must assert duplicate in-flight receipt behavior, completed-result replay, unsupported remote command rejection, and conflict rejection for the same command id with a different command. Unchanged-reply tests must compare the command name, status/reason, message, and routing side effects that operators or remote clients see; a shorter `daemon.gleam` alone is not acceptance evidence.

Before publish, run these gates from the repository root and record their successful output: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. Success means the format check exits zero, the full Gleam test suite reports no failures, and both lint commands exit zero with no new production policy errors. No pre-publish manual/browser/dogfood check is required for this rank; the implementation handoff should explicitly defer a human/operator smoke check of pause/resume and one worker command to after the automated gates. Do not leave implementation or validation work as unchecked progress TODOs in the final handoff.

## Rollout, Recovery, and Idempotence

Roll out additively: add tests, add the new runtime modules, delegate from the daemon, then remove daemon-local duplicates. Each step is reversible by reverting the delegation and leaving `control_command_handler.gleam` and the public daemon message/API unchanged. Re-running tests and validation gates is idempotent. Remote command replay must remain idempotent for duplicate command ids, and conflict handling must remain deterministic.

If extraction causes a regression, recover by reverting the module delegation commit while keeping the characterization tests that exposed the drift. Because this rank changes code organization only, rollback does not require data migration, cache cleanup, provider-live or cache invalidation, docs/helper rollback, or Linear-side cleanup. The only manual/operator action after implementation is the deferred human smoke check of pause/resume and one worker command; it is not a pre-publish gate unless a reviewer changes that decision.

## Open Questions and Clarifications Needed

No open questions block the implementation plan. The only branch-dependent normalization is whether `test/orchestrator_daemon_linear_command_test.gleam` already exists; if it does not, the implementer should create it as part of the characterization baseline.
