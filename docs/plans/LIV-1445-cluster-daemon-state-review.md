# LIV-1445 cluster daemon State into owned sub-records

This review defines a behavior-preserving implementation plan for clustering daemon `State` into owned sub-records. It is intentionally concise; concrete edit steps, exact test scenarios, interfaces, dependencies, and artifact notes are carried in the structured implementation pack for this issue. Review feedback about acceptance evidence, negative and duplicate/idempotent coverage, milestone proof anchors, docs/helper boundaries, provider-live/cache non-scope, full validation, and lint gates must be mirrored in both this document and the structured implementation-pack submission rather than living only in prose.

## Purpose / Big Picture

Scherzo's daemon should be a small actor shell that receives messages, calls subsystem owners, and publishes replies, not the only place where every runtime map, monitor, projection, timer, and correlation counter can be mutated. After the follow-up implementation, a maintainer should be able to work on scheduled timers, startup recovery waiters, operator replies, control/query handles, remote-client monitors, YAML step metrics, ledger projection, or effect-runner process state by reading the owner module for that sub-record. Operators should observe the same daemon startup, dispatch, recovery, control-command, query, remote-client, and worker behavior.

## Problem Framing and Constraints

`src/scherzo/orchestrator/daemon.gleam` still has a broad private `State` record whose remaining shell concerns are peers: workflow and tracker clients, scheduled runtime plus two timer dictionaries, transition core state, worker registry, YAML step token metrics, startup recovery data, event/effect handles, control/query handles, ledger projection, read model, remote-client handles, operator reply routing, queued control-operation de-duplication, and invariant status. Because helpers can take the whole record, code that should belong to `scheduled_runtime`, `startup_recovery`, `operator_runtime`, `remote_command_runtime`, query/projection ownership, or effect ownership can keep growing in the daemon.

This plan must land after the shared capability-records work and after transition-state unification. It must preserve the current public actor messages, command reply contracts, query outputs, ledger records, recovery sequencing, timer cancellation behavior, and process monitor semantics. No durable data migration, protocol change, workflow YAML change, provider-live/cache behavior change, or browser/UI change is intended.

## Strategy Overview

Keep the existing successful precedent: the daemon holds and threads opaque or owner-shaped records such as `poll_scheduler.State(TimerHandle)`, `retry_scheduler.State(TimerHandle)`, `worker_registry.Registry`, and the embedded `transition_types.State` core, while owner modules expose pure state functions plus narrow effect callbacks. The target decomposition is to retain those records and add owner records for the remaining clusters: `scheduled_runtime.State(TimerHandle)` for scheduled runtime plus retry/report timers; `startup_recovery.DaemonState` for phase, recovery metadata, and waiters; `operator_runtime.State` for dispatch pause, operator reply correlation, completed results, and work-item action receipts; `control_operation_runtime.State` for active queued operation de-duplication; `remote_command_runtime.State` for remote client handle, monitor, and managed-launch grant; `control_plane_runtime.State` for control server, monitor, control file, and query service handle; `query_projection.State` for query cache, read model, ledger projection, and snapshot publication; `session_metrics.State` for YAML step token aggregation; `effect_runtime.State` for the effect runner handle and monitor; and `daemon_transition_shell.InvariantState` for the invariant-violation latch.

Migration should be owner-by-owner and adapter-first. Each milestone should introduce or extend the owner module, move only the functions that can operate on the sub-record plus explicit inputs, update daemon call sites to thread the sub-record, then lower boundary guardrails only after behavior and validation are green. The final daemon record should contain mostly owner records, `subject`, `workflow`, tracker adapter/client compatibility fields if still needed, shared capabilities, and `RuntimeDependencies`.

## Alternatives Considered

Only nesting fields inside anonymous records in `daemon.gleam` was rejected because it would not move ownership or allow helper extraction. Moving all state into one new `daemon_runtime.State` was rejected because it would recreate the same god-record problem under another name. Rewriting actor messages, the transition core, or worker supervision was rejected as larger than necessary. Clustering before the capability and transition-core plans was rejected because those seams define which state is truly shell-owned and which state is core-owned.

## Risks and Countermeasures

The main risk is a behavior-preserving refactor that silently changes behavior through missed timer cancellation, duplicate queued operations, lost operator replies, stale query snapshots, stale or duplicate monitor-down messages, effect-runner restarts, or startup waiters that never receive a result. The countermeasure is to characterize each cluster before moving it and require success, negative, duplicate, stale, timeout, and idempotent paths for every moved owner.

A second risk is replacing the flat state with many records but leaving all helpers in the daemon. The countermeasure is that every milestone must move at least one meaningful helper into the owner module and update the boundary guardrails to prevent the helper prefix from returning.

A third risk is widening a structural refactor into unrelated workflow helper, provider-live, provider-cache, browser, protocol, or durable-data work. The countermeasure is an explicit docs/helper inventory at the end of implementation: `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam` are expected to change with daemon ownership, while `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, provider-live probes, cache behavior, workflow YAML, and browser/UI surfaces should remain unchanged. If any of those out-of-scope surfaces must change, split that work before publishing this refactor.

A fourth risk is review-doc and implementation-pack drift. The countermeasure is to keep acceptance evidence, exact test obligations, milestone proof anchors, provider-live/cache non-scope, full validation, and linting explicit here and mirrored in the structured implementation pack before Scherzo materializes follow-up work. A fifth risk is merge conflict with the predecessor daemon-structure plans. The countermeasure is to normalize on the landed `DaemonCapabilities` and embedded `transition_types.State` shapes before cutting new clusters.

## Scope Boundaries

For this planning issue, scope is exactly this Markdown review document under `docs/plans/` plus the structured implementation-pack submission captured by Scherzo. Do not write the canonical bundle by hand and do not change production code in this planning task.

For the follow-up implementation, scope is the in-memory daemon state shape, owner modules, tests, and daemon-boundary documentation/ratchets needed to move cluster-owned helpers out of `src/scherzo/orchestrator/daemon.gleam`. Expected documentation and guardrail scope is limited to `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam`, plus any nearby test files needed to characterize the moved owners. Out of scope are public protocol changes, durable ledger schema changes, workflow YAML semantics, tracker adapter semantics, UI/browser work, provider-live/cache behavior, `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, and any new feature behavior beyond preserving the existing daemon semantics. If implementation unexpectedly needs one of those out-of-scope surfaces, stop and split or roll back that portion before publish.

## Milestones

Milestone 1 normalizes on the already-landed capability and transition-core seams, inventories every remaining top-level state field, and adds characterization tests for cluster ownership risks before moving fields. Reviewers should see a field-to-owner table or notes in the implementation handoff, targeted tests that still pass before code motion, and confirmation that `state.capabilities` and `state.core` are the starting seams rather than new work.

Milestone 2 folds scheduled runtime timer dictionaries into the scheduled-runtime owner and moves scheduled retry/report timer helpers out of the daemon. Reviewers should see stale-generation retry ticks ignored, matching ticks promoted or retried once, timer replacement cancelling the old handle, timer deletion being idempotent, and no duplicate retry/report ledger side effect from repeated ticks.

Milestone 3 moves startup recovery phase, readiness waiters, recovery metadata, and YAML step token metrics into their owner records, keeping startup sequencing and token totals unchanged. Reviewers should see ready, failed, timeout, late-waiter, duplicate-timeout, and cleanup paths for startup waiters, plus YAML step token totals and run removal remaining identical before and after the move.

Milestone 4 moves operator reply routing, work-item action receipts, dispatch-pause state, and queued control-operation de-duplication into owner records with explicit daemon callbacks for transitions, logging, and ledger appends. Reviewers should see pause/resume parity, not-found or rejected operator responses, worker-command timeout and late-reply handling, duplicate completed-result reuse, conflicting or duplicate queued operation suppression, and work-item action receipt idempotency.

Milestone 5 moves remote-client and control/query handles into owner records, including monitor-down handling, control-file cleanup, query-cache publication, and read-model/projection snapshot ownership. Reviewers should see remote-client start/stop/restart parity, stale monitor-down messages ignored, managed-launch auth rejection still reported, control server down cleanup unchanged, query status/metrics/claims/outbox/workflow responses unchanged, and stale or timeout query paths still returning the same errors.

Milestone 6 clusters effect-runner process ownership, removes leftover top-level fields that now belong to owners, updates architecture guardrails, and records the final daemon line-count reduction. Reviewers should see effect-runner completion and DOWN handling unchanged, stale or duplicate completions handled once, `src/scherzo/orchestrator/daemon.gleam` line count lower than the prior `10_641` ratchet, matching updates in `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam`, and an explicit inventory that workflow helpers, provider-live probes, cache behavior, workflow YAML, and browser/UI surfaces were not changed.

## Progress

- [x] (2026-07-09) Read the repo-local ExecPlan workflow guidance and prepared review-doc target.
- [x] (2026-07-09) Inspected the current daemon state, predecessor LIV-1442 and LIV-1443 plan artifacts, scheduler precedents, and daemon-boundary guardrails.
- [x] (2026-07-09) Wrote this concise review document as a planning artifact only; production code was not changed.
- [x] (2026-07-09) Incorporated review feedback by making acceptance evidence, exact negative/duplicate/idempotent tests, milestone proof anchors, docs/helper boundaries, provider-live/cache non-scope, full validation, and linting explicit in this document and the updated structured implementation-pack obligations.
- [x] (2026-07-09) Revalidated this review document with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1445-cluster-daemon-state-review.md` and received `REVIEW_DOC_VALID=ok`.
- [x] (2026-07-11) Recovered the retained LIV-1469 workspace and removed the mirrored daemon fields, leaving scheduled runtime, startup recovery, session metrics, operator/runtime control state, control plane, query projection, remote runtime, effect runtime, and invariant state owned only by their records.
- [x] (2026-07-11) Added direct owner-state tests for timer replacement and duplicate ticks, waiter removal/completion idempotence, operator reply correlation, queued-operation de-duplication, stale process monitors, query snapshot publication, and effect-runner monitor identity.
- [x] (2026-07-11) Lowered the formatted daemon boundary from the failed attempt's 10,711 lines to 10,614 lines, below the then-current 10,617-line parent, and synchronized architecture and source guardrails.
- [x] (2026-07-11) Rebased onto LIV-1471, preserved its ledger-compaction behavior through the owner-state migration, and lowered the merged daemon boundary to 10,799 lines, below the new 10,802-line parent.
- [x] (2026-07-11) Ran formatting and the unit suite after the rebase; 2,339 unit tests passed.
- [x] (2026-07-11) Ran the contract suite and both required production lint gates after the rebase; 1,025 contract tests passed, and both lint commands exited zero with the existing 292-warning inventory and no errors.
- [x] (2026-07-11) Described the retained jj change as `Implement LIV-1469: cluster daemon state ownership`.
- [x] (2026-07-11) Completed the explicit operator publication path after retained-run finalization was rejected; published PR #655 and moved LIV-1469 to In Review.

## Surprises & Discoveries

The predecessor structure work has already partly landed in this tree: `daemon.gleam` now contains `capabilities: daemon_capabilities.DaemonCapabilities(State, Message, TimerHandle)` and `core: transition_types.State`, and the old `dispatch_recovery_cleared_pending_claims` side channel is no longer present. That makes this plan a true last-stage clustering pass rather than a first extraction pass.

The current guardrail already ratchets `src/scherzo/orchestrator/daemon.gleam` to its exact line count, so every implementation milestone must update `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam` in lockstep when the daemon shrinks.

The first implementation attempt added owner records alongside the old flat fields and routed most reads through compatibility accessors. That shape passed broad tests but did not establish ownership because every mutation still had two possible storage locations. Removing the mirrors exposed a finite set of compiler errors, making `gleam check` an effective inventory of every remaining flat-field mutation.

The failed attempt raised the daemon ratchet to 10,711 even though its parent was 10,617 lines. Completing owner wiring and moving query snapshot construction and cache cleanup into `query_projection.gleam` reduced the formatted shell to 10,614 lines without changing protocols or durable data. Before manual publication, LIV-1471 advanced the parent with ledger auto-compaction and a 10,802-line boundary. The rebase retained ledger-compaction state and effect dispatch alongside the new owner records while keeping the merged daemon at 10,799 lines.

The rebase exposed an existing race in an orphan-cleanup contract assertion: asynchronous outbox completion could append between the test's ledger snapshot and cleanup assertion. The test now waits for the durable `outbox_completed` synchronization point before asserting that cleanup itself appends no records.

A manual retained-run finalization dry-run could not publish this old run because its pinned workflow-interface fingerprint differed from the current workflow definition. Scherzo rejected it with `publication_route_discovery_unsafe`; the explicit operator path then used the configured jj commit-stack publisher to create PR #655 without bypassing the stale-route guard.

## Decision Log

- Decision: Sequence this plan after capability records and transition-state unification. Rationale: those plans decide the stable seams and the transition-owned core, avoiding re-cut clusters later. Date: 2026-07-09.
- Decision: Use owner modules and owner records rather than a single aggregate runtime record. Rationale: extraction payoff requires helpers to move to their owning module and stop accepting full daemon `State`. Date: 2026-07-09.
- Decision: Treat negative, duplicate, stale, timeout, and idempotent behavior as acceptance-critical for every cluster. Rationale: those are the paths most likely to change accidentally when timers, monitors, replies, and recovery waiters move. Date: 2026-07-09.
- Decision: Keep public actor messages and durable data unchanged. Rationale: this is a structure refactor; widening it into behavior or schema work would add risk without solving the flat-state ownership problem. Date: 2026-07-09.
- Decision: Treat review feedback about evidence, exact tests, docs/helper inventory, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations. Rationale: Scherzo materializes follow-up implementation instructions from the structured pack, so prose-only requirements would be easy for later implementers to miss. Date: 2026-07-09.
- Decision: Remove all mirrored cluster fields in one compiler-guided migration rather than preserve compatibility mirrors across more commits. Rationale: the owner APIs already existed in the retained workspace, and `gleam check` precisely identified every stale read and update; keeping mirrors would preserve the verifier's core failure mode. Date: 2026-07-11.
- Decision: Keep process creation, timer creation, actor replies, and ledger appends in the daemon shell while storing their handles and logical state only in owners. Rationale: these are composition-root effects, while owner records provide one mutation path and independently testable stale/duplicate decisions. Date: 2026-07-11.

## Outcomes & Retrospective

The retained implementation now has one source of truth for every planned runtime cluster. After rebasing onto LIV-1471, the daemon shell is 10,799 formatted lines, three lines below its 10,802-line parent; before that parent change, the same migration was 10,614 lines, 97 lines below the rejected attempt. Direct owner tests supplement existing daemon integration and query-contract coverage. Formatting passed, 2,339 unit tests passed, 1,025 contract tests passed, and both production lint gates exited zero with the existing warning inventory.

The final changed-surface inventory contains daemon and owner modules, daemon-focused tests, architecture guardrails, and this plan only. `.scherzo/workflows/scripts/*`, workflow schemas and YAML, provider-facing structured-output helpers, review-lane contract files, provider-live probes, cache behavior, and browser/UI surfaces remain unchanged.

Implementation, validation, and manual publication are complete. PR #655 contains the retained jj change, and LIV-1469 is In Review. The old run remains historical failure evidence rather than being rewritten to claim automatic completion.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-1445-cluster-daemon-state-review.md`, every required review-doc section is present and non-empty, `.scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1445-cluster-daemon-state-review.md` exits zero with `REVIEW_DOC_VALID=ok`, no forbidden mechanical sections appear in the review document, and Scherzo captures the structured implementation-pack submission for LIV-1445. The planning handoff must not include source/test implementation changes or manually written canonical bundle JSON.

The follow-up implementation is accepted only when the daemon holds the target owner records, cluster-owned helpers have moved out of `daemon.gleam`, actor-visible behavior remains unchanged, boundary guardrails reflect the new ownership and lower line count, and exact test evidence covers every moved cluster. Scheduled-runtime evidence must cover stale retry ticks, matching retry/report ticks, timer replacement cancellation, idempotent timer deletion, and repeated tick suppression. Startup-recovery evidence must cover ready, failed, timeout, late-waiter, duplicate-timeout, and cleanup paths. Operator/control evidence must cover pause/resume parity, not-found and rejected results, worker-command timeout and late replies, duplicate completed-result reuse, queued operation duplicate/conflict suppression, and work-item receipt idempotency. Remote/control/query evidence must cover start, stop, restart, stale monitor-down messages, managed-launch rejection, control-server down cleanup, query snapshot publication, status/metrics/claims/outbox/workflow parity, and timeout or stale-query errors. Effect-runner evidence must cover completion, crash/DOWN, stale completion, and duplicate completion behavior.

Docs/helper evidence must include updates to `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam` that reflect the new owners and lower daemon line-count ratchet. It must also include an explicit inventory stating that `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, provider-live probes, cache behavior, workflow YAML, and browser/UI surfaces were unchanged; if any of those surfaces change, the work must split or include the corresponding helper/contract tests and, for provider-live/cache changes, stale-read, invalidation, and TTL-disabling evidence before acceptance.

Full repository validation for the follow-up implementation must pass through direnv from the repository root: `direnv exec . gleam format --check src test`, `direnv exec . gleam test -- --suite unit`, `direnv exec . gleam test -- --suite contract`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv. If a gate fails for a pre-existing unrelated reason, capture the exact output and explain why the state-clustering work is not the cause; otherwise all gates must pass.

## Rollout, Recovery, and Idempotence

Rollout for this planning issue is a normal review of this Markdown file and the captured implementation pack; Scherzo owns bundle materialization. Recovery is to edit or revert this file and resubmit corrected structured output.

The follow-up implementation should roll out as small green commits by cluster. If a cluster migration changes behavior, revert that cluster while keeping characterization tests that describe the old behavior. No manual browser check, production dogfood, provider-live probe, or provider-cache exercise is pre-publish blocking for this pure in-memory refactor; optional live/operator dogfood may be recorded after implementation, but automated behavior and guardrail evidence are the publish gate.

The work is idempotent because it changes in-memory state ownership and tests only; repeated validation, daemon startup, control queries, startup-ready waits, duplicate control-operation messages, duplicate monitor-down messages, duplicate effect completions, and duplicate timer ticks must not require ledger repair, provider cache cleanup, workflow helper cleanup, or manual operator cleanup. If implementation discovers it must alter workflow helpers, provider-facing structured-output contracts, provider-live probes, cache behavior, workflow YAML, browser/UI behavior, public protocols, or durable ledger schema, that change should be split or explicitly rolled back before publishing this state-clustering slice.

## Open Questions and Clarifications Needed

No open question blocks implementation handoff. If implementation discovers a field cannot move without changing public protocol, durable schema, workflow YAML semantics, or provider behavior, stop and split that finding into a separate plan instead of expanding this refactor.
