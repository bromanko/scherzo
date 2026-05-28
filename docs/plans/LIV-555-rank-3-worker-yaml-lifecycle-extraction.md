# LIV-555 Rank 3 worker and YAML workflow lifecycle extraction review

This review is a planning artifact for the rank 3 derivative of LIV-523. It does not implement the extraction.

## Purpose / Big Picture

This rank should make Scherzo's daemon easier to maintain by moving worker process creation and YAML workflow-step lifecycle decisions out of `src/scherzo/orchestrator/daemon.gleam` without changing what operators, control clients, session timelines, workflow YAML files, or step artifacts observe. After implementation, the daemon should still be the public actor, mailbox, EventHub owner, logging context owner, and compatibility shell, while focused lifecycle modules own worker spawning, YAML step execution decisions, route registration, update handling, and worker-finish interpretation.

## Problem Framing and Constraints

The current tree already contains earlier decomposition prerequisites such as `src/scherzo/orchestrator/scheduled_runtime.gleam` and `src/scherzo/orchestrator/startup_recovery.gleam`, but `src/scherzo/orchestrator/daemon.gleam` is still 6,169 lines and still owns rank 3 lifecycle code. Verified daemon-local functions include `spawn_worker`, `spawn_scheduled_worker_for_pending`, `spawn_scheduled_worker_with_run_root`, `run_workflow_worker`, `run_scheduled_workflow_worker`, `yaml_workflow_dependencies`, `yaml_scheduled_workflow_dependencies`, `register_yaml_step_session`, `run_yaml_command_step`, `run_yaml_agent_step`, `publish_yaml_command_failure`, `handle_worker_command_ready`, `handle_worker_update`, `handle_scheduled_worker_finished`, `finish_scheduled_worker_success`, `finish_scheduled_worker_needs_human`, `finish_scheduled_worker_failure`, `worker_finished_to_transition`, `worker_down_to_transition`, and `handle_registry_down_resolution`.

The extraction must be behavior-preserving. It must not change workflow YAML semantics, step artifact shape, EventHub event shape, session ID generation, public daemon/control messages, operator command behavior, Linear/tracker behavior, provider-live/cache behavior, or the OTP/process architecture. It must extend `src/scherzo/orchestrator/worker_registry.gleam` for missing registry operations instead of introducing parallel maps that can drift.

Review feedback for this revision makes acceptance evidence part of the plan rather than an afterthought. The implementation must name the targeted tests it adds or extends, capture targeted output before relying on the extraction, run the full repository validation gates before publish, and preserve the already stated manual dogfood timing: no manual/browser/operator dogfood is a pre-publish requirement for this internal behavior-preserving rank, but a deferred human/operator check is allowed after implementation.

## Strategy Overview

Use a test-first strangler extraction. First, add characterization coverage around current daemon behavior for worker command readiness, YAML step sessions, scheduled worker outcomes, aborts, and monitor-down handling. Then introduce `src/scherzo/orchestrator/worker_lifecycle.gleam` and `src/scherzo/orchestrator/yaml_workflow_lifecycle.gleam` with callback-style boundaries so neither module imports `scherzo/orchestrator/daemon`. The daemon will pass its subject, EventHub subject, logger, clock, runtime dependencies, tracker client, workflow bundle, and secrets into the lifecycle modules and will continue to receive public mailbox messages.

`worker_lifecycle.gleam` should own normal and scheduled worker process spawning, worker command-ready registration, worker update publication/logging decisions, scheduled worker finish handling, and worker/down-to-transition decisions. `yaml_workflow_lifecycle.gleam` should own YAML workflow dependency assembly, command-step and agent-step execution wrappers, YAML step session registration, YAML step failure publication, and YAML workflow failure mapping. The daemon should shrink by delegating these decisions, but it should still start and stop the EventHub, own public daemon/control APIs, enqueue side effects, apply transition messages, and preserve top-level redaction/log context.

Each extraction step should be paired with a narrow command that proves the moved lifecycle area still behaves the same, followed by the full format, test, glinter, and Scherzo lint gates in the final milestone. Documentation/helper migration work is not part of the strategy; this rank only changes production lifecycle modules and their tests unless a test fixture must be adjusted to keep behavior-preserving coverage compiling.

## Alternatives Considered

Leaving rank 3 in the daemon and adding only boundary tests was rejected because it would document the problem without reducing the lifecycle ownership cluster. Moving all transition/effect code at the same time was rejected because that belongs to later LIV-523 ranks and would make regressions harder to localize. Letting the new modules import `daemon.gleam` was rejected because it would preserve the daemon as the architectural center and risks import cycles; callbacks keep the mailbox boundary explicit.

## Risks and Countermeasures

The highest risk is changed ordering of EventHub lifecycle events, token updates, ledger records, or worker completion messages. Counter this with characterization tests before movement and with daemon session-event tests after each milestone. The second risk is registry split-brain between active workers, scheduled workers, YAML step routes, and monitor resolution. Counter this by extending `worker_registry.gleam` and keeping `test/orchestrator_worker_registry_test.gleam` as a required gate. The third risk is scheduled-worker regression, especially success, needs-human, failure retry/report, and monitor-down paths. Counter this with targeted scheduled lifecycle tests and unchanged daemon scheduled tests. The fourth risk is creating oversized lifecycle modules. Counter this by keeping worker process lifecycle and YAML workflow-step lifecycle in separate modules and by deferring operator runtime, remote command runtime, transition/effect bridge work, and daemon boundary guardrails to their own ranks.

A review-specific risk is accepting an extraction that only compiles but lacks evidence for provider-live/cache preservation, lint policy compliance, and retained operator behavior. Counter this by making the implementation pack list the exact targeted tests, full validation gates, non-migration of docs/helpers, and the deferred post-implementation dogfood check separately from the pre-publish automated evidence.

## Scope Boundaries

In scope: `src/scherzo/orchestrator/daemon.gleam`, new `src/scherzo/orchestrator/worker_lifecycle.gleam`, new `src/scherzo/orchestrator/yaml_workflow_lifecycle.gleam`, targeted extensions to `src/scherzo/orchestrator/worker_registry.gleam`, and focused tests under `test/` for registry, daemon session events, worker/YAML daemon behavior, and lifecycle parity. The implementation may add helper test modules or fixtures only when needed to characterize rank 3 behavior.

Out of scope: implementing or replanning LIV-523 ranks 1, 2, 4, 5, or 6; changing workflow YAML semantics; changing step artifact JSON or retained artifact shape; changing EventHub event payload shape; changing public daemon/control command behavior; changing provider-live/cache behavior; docs/helper migrations; a new supervision tree; and source-boundary guardrails beyond any small tests needed to keep rank 3 from regressing during this extraction. Provider-live and provider-cache paths must continue to be exercised through the same daemon runtime dependencies and configuration surfaces; if a test needs a fake provider or cached provider fixture, it should assert unchanged delegation rather than introduce new cache semantics. Documentation or helper updates are allowed only when a moved symbol breaks an existing test helper import, and then the update must be mechanical and covered by the same test command.

## Milestones

Milestone 1 establishes characterization evidence before code motion. At the end of this milestone, current worker/YAML/scheduled lifecycle behavior is captured by tests for command-ready routing, YAML step failure publication, scheduled worker success, scheduled needs-human, scheduled failure, abort, and monitor-down parity. The proof for this milestone is targeted output from the new or extended tests in `test/orchestrator_worker_registry_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_retry_step_test.gleam`, `test/agent_worker_command_test.gleam`, and `test/yaml_step_session_test.gleam`; these tests should pass before code motion because they characterize existing behavior.

Milestone 2 introduces lifecycle module boundaries without changing behavior. At the end of this milestone, `worker_lifecycle.gleam` and `yaml_workflow_lifecycle.gleam` exist with daemon-independent callback interfaces, and the daemon can call them without the new modules importing `daemon.gleam`. The proof is that a targeted compile/test command for the touched orchestrator tests passes and that a source search shows no import from either new lifecycle module back to `scherzo/orchestrator/daemon`.

Milestone 3 moves YAML workflow lifecycle decisions. At the end of this milestone, YAML dependency assembly, command-step execution, agent-step execution, step session registration, failure publication, and YAML failure mapping live in `yaml_workflow_lifecycle.gleam`, while the daemon still owns the EventHub subject and mailbox messages. The proof is unchanged YAML/session event outputs from `test/yaml_step_session_test.gleam`, `test/orchestrator_daemon_retry_step_test.gleam`, and the daemon session-event tests, including the YAML command-step failure publication path.

Milestone 4 moves worker and scheduled-worker lifecycle decisions. At the end of this milestone, normal worker spawn, scheduled worker spawn, command-ready registration, worker update logging/publication decisions, worker finish mapping, scheduled finish handling, and monitor-down parity live in `worker_lifecycle.gleam` with registry changes made through `worker_registry.gleam`. The proof is that worker command-ready routing, abort behavior, scheduled success, scheduled needs-human, scheduled failure retry/report, and monitor-down tests pass without changing provider-live/cache behavior or public daemon/control messages.

Milestone 5 removes obsolete daemon-local helpers and collects acceptance evidence. At the end of this milestone, `daemon.gleam` remains the public actor shell, the old rank 3 helper functions are gone or reduced to thin adapters, all targeted tests pass, documentation/helper migration remains absent except for any mechanical test-helper import repair, and the full format, test, glinter, and Scherzo lint gates have passed. The deferred human/operator dogfood check, if performed, happens after implementation and is not a pre-publish blocker.

## Progress

- [x] (2026-05-28) Verified the prepared output target is the default directory `docs/plans` via `tmp/execplan-review-doc-target.json`.
- [x] (2026-05-28) Re-read `docs/plans/LIV-523-daemon-decomposition-v2.md` and inspected current daemon, registry, scheduled-runtime, startup-recovery, and relevant daemon/session tests.
- [x] (2026-05-28) Wrote this focused rank 3 review document without implementing the extraction.
- [x] (2026-05-28) Ran review-document validation; it reported `REVIEW_DOC_VALID=ok` for this file.
- [x] (2026-05-28) Incorporated review feedback by making milestone evidence, full validation, linting, provider-live/cache preservation, docs/helper migration scope, and manual dogfood timing explicit in this review document.
- [x] (2026-05-28) Re-ran review-document validation after review-feedback incorporation; it reported `REVIEW_DOC_VALID=ok`.

## Decision Log

- Decision: Keep this document focused only on LIV-523 rank 3. Rationale: the task explicitly excludes replanning ranks 1-2 and 4-6 except as prerequisites or non-goals. Date: 2026-05-28.
- Decision: Use callback-style lifecycle interfaces rather than importing `daemon.gleam` from new modules. Rationale: the daemon must remain the mailbox and compatibility shell, and avoiding daemon imports prevents circular ownership. Date: 2026-05-28.
- Decision: Extend `worker_registry.gleam` for lifecycle registration and monitor-resolution gaps. Rationale: a second worker/step route store would make abort, command routing, and monitor-down behavior harder to preserve. Date: 2026-05-28.
- Decision: Defer manual dogfood as non-blocking post-implementation operator evidence. Rationale: this is an internal behavior-preserving extraction, and automated daemon/control/session tests are the required pre-publish evidence. Date: 2026-05-28.
- Decision: Require full validation and lint gates as pre-publish evidence for the implementation rank. Rationale: the extraction touches production orchestrator modules, so passing targeted tests alone would not prove format, repository-wide test, glinter, or Scherzo lint compatibility. Date: 2026-05-28.
- Decision: Keep docs/helper migration and provider-live/cache behavior out of scope except for mechanical test-helper import repair if a moved symbol requires it. Rationale: review feedback called out these surfaces as risks, and the rank goal is behavior-preserving lifecycle extraction rather than user-facing documentation, helper, or provider-cache redesign. Date: 2026-05-28.

## Validation and Acceptance

The review artifact is acceptable when `test -f docs/plans/LIV-555-rank-3-worker-yaml-lifecycle-extraction.md` succeeds and `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-555-rank-3-worker-yaml-lifecycle-extraction.md` reports `REVIEW_DOC_VALID=ok`; this validation was re-run on 2026-05-28 after review-feedback incorporation and reported `REVIEW_DOC_VALID=ok`. The implementation plan is acceptable only when it keeps `test/orchestrator_worker_registry_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_retry_step_test.gleam`, `test/agent_worker_command_test.gleam`, and `test/yaml_step_session_test.gleam` green.

The implementation must add or extend lifecycle tests that prove command-ready routing, YAML command-step failure publication, scheduled worker success, scheduled needs-human handling, scheduled failure retry/report handling, abort behavior, and monitor-down parity. Concrete evidence must include targeted test output for the new lifecycle tests and existing worker/YAML daemon tests. The implementation must also show that provider-live/cache behavior and public daemon/control messages were not intentionally changed; the expected evidence is unchanged tests through existing daemon runtime dependencies, not a new provider-cache feature.

Before publish, run these full gates from the repository root and capture their passing output: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No operator-visible manual or browser dogfood check is required before publish for this rank because the accepted change is internal and behavior-preserving. A human/operator may perform a deferred post-implementation dogfood check by running a real YAML command/agent workflow and comparing `scherzoctl` session timelines with the automated expectations, but that evidence is not a blocking pre-publish gate.

## Rollout, Recovery, and Idempotence

Rollout should be additive: land characterization tests, add lifecycle modules, delegate one lifecycle area at a time, then remove daemon-local helpers only after tests pass. Recovery is to revert the lifecycle-module delegation commits; no state migration, cache invalidation, YAML migration, provider-cache invalidation, documentation migration, helper migration, or operator data cleanup is required. Test runs and lint gates are idempotent. If an intermediate extraction exposes a regression, keep the tests and back out only the delegation that changed behavior. If a deferred dogfood check later finds an operator-visible mismatch, restore the daemon-local adapter path from the previous commit while keeping the characterization test that exposed the mismatch.

## Open Questions and Clarifications Needed

No open questions block this plan. If implementation discovers that worker and scheduled-worker finish handling make one diff too large for review, split Milestone 4 into two commits while keeping both inside this rank 3 scope.
