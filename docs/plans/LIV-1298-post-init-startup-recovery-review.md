# LIV-1298 post-init startup recovery review

This ExecPlan review is documentation-only for LIV-1298. The generated follow-up implementation task owns the code changes.

## Purpose / Big Picture

Scherzo daemon startup should acknowledge actor readiness quickly, keep the control listener alive, and then complete recovery work after the actor is initialized. After the follow-up implementation, an operator who restarts a recovery-heavy daemon should see `daemon.start` either succeed quickly or fail with a specific startup diagnostic, while outbox replay, scheduled failure publication, startup park reports, recovered workflow resumption, and remote-client activation proceed asynchronously and idempotently.

## Problem Framing and Constraints

The current daemon startup path starts local services inside `src/scherzo/orchestrator/daemon.gleam` and then performs recovery work before calling `actor.initialised`. The observed failure reached `control_server_started`, logged startup recovery warnings, then returned the generic `daemon_start_failed` / `actor start failed`, which is consistent with the 60 second `actor.new_with_initialiser` timeout expiring while initializer-side recovery work continued. The plan must keep initialization bounded, preserve durable recovery semantics, avoid duplicate tracker publications across restarts, and make `actor.InitTimeout` distinguishable from other actor start failures.

The current initializer-side work to account for includes `apply_startup_recovery`, `apply_scheduled_startup_recovery`, recovered workflow resumption, the initial read-model refresh, replay of queued control operations, the initial poll timer, and the `StartRemoteClient` self-message. `startup_recovery.load` already runs before the actor initializer; this plan should audit it but keep the main scope on work that currently happens after the control plane starts and before `actor.initialised` returns.

## Strategy Overview

The right-sized approach is to introduce a post-initialization recovery phase in the daemon actor. The actor initializer should load durable state, start required local services, construct the actor state with a pending recovery plan, install monitors and selectors, enqueue a recovery-continuation message, and return `actor.initialised` promptly. The recovery-continuation handler should then process recovery stages in the existing order: startup recovery effects, scheduled recovery effects, recovered workflow registration and launch, invariant checks, read-model refresh, queued control-operation replay, initial polling, and remote-client startup.

During post-init recovery, read-only queries should continue to answer from the current durable snapshot. Mutating operator commands should be rejected with a stable `startup_recovery_in_progress` reason until deterministic local recovery work has finished and the daemon has reached the interactive phase. Network publications and cleanup remain asynchronous through the existing effect runner, and idempotence continues to rely on ledger, outbox, and dedupe keys rather than on in-memory flags.

## Alternatives Considered

Increasing the initializer timeout was rejected because it would hide the race, keep the control listener open while the actor is not ready, and still produce poor diagnostics under heavier ledgers. Only improving the `InitTimeout` log was rejected because it does not fix startup liveness. Starting all recovery in an untracked external process was rejected because recovery ordering, command gating, and daemon state updates must remain actor-owned. Accepting mutating commands immediately during recovery was rejected because scheduled recovery records, outbox attempts, and worker registrations can otherwise race with operator changes.

## Risks and Countermeasures

The main risk is changing recovery ordering. The countermeasure is to preserve the existing order explicitly and test each category: retry timers, cleanup, outbox replay, park reports, scheduled failure reporting, recovered workflows, read-model refresh, queued control operations, poll startup, and remote-client startup. Another risk is starving the actor mailbox after initialization; the recovery phase should process bounded batches and yield between batches so queries and shutdown can still be handled.

Duplicate publication is a safety risk. The implementation must keep deterministic outbox IDs and dedupe keys, append attempt records before enqueueing tracker effects, and prove restart idempotence with existing ledgers that contain pending outbox attempts and scheduled report retries. Operator confusion is also a risk, so command rejections and startup phase timing logs must be clear enough for an operator to retry after recovery completes.

## Scope Boundaries

In scope: moving initializer-side recovery effects in `src/scherzo/orchestrator/daemon.gleam` to a post-init daemon phase; adding bounded recovery progress state; gating mutating operator commands while deterministic recovery is incomplete; delaying initial poll and remote-client activation until recovery is ready; preserving existing ledger, projection, outbox, scheduled-runtime, EventHub, and workflow semantics; and adding a near-term `actor.InitTimeout` diagnostic.

Out of scope: changing ledger record formats, changing tracker provider APIs, changing workflow YAML semantics, replacing the effect runner, adding a new operator command, changing Linear command syntax, or migrating existing state files. Docs/helper migration, provider-live or provider-cache behavior, and browser UI behavior are not part of this daemon lifecycle refactor; if those surfaces appear during implementation, record the discovery and split it into a separate workflow rather than expanding this plan. If implementation discovers that the pre-actor `startup_recovery.load` tracker refresh is the dominant startup delay, that should be recorded and split into a follow-up unless it can be moved without expanding this initializer-timeout fix.

## Milestones

Milestone 1 delivers diagnostics and an exact side-effect inventory. The implementer must add a focused timeout diagnostic test in the daemon startup tests and record a source audit that names each initializer-side effect: `apply_startup_recovery`, `apply_scheduled_startup_recovery`, recovered workflow resumption, transition invariant checks, read-model refresh, queued control-operation replay, initial poll scheduling, and `StartRemoteClient`. Acceptance for this milestone is a forced `actor.InitTimeout` that maps to a specific startup code such as `daemon_actor_init_timeout`, logs phase timing, and leaves a reviewed inventory showing which effects move post-init and which remain pre-actor.

Milestone 2 introduces the post-init recovery state machine without moving side effects yet. The implementer must add daemon state that can represent recovery pending, running, ready, or failed; add one recovery-continuation message path; delay initial poll and remote-client activation until readiness; and route all mutating operator commands through a single gate. Acceptance is targeted tests showing read-only snapshot/query messages still return during recovery, mutating commands return `command.Rejected("startup_recovery_in_progress")`, and the same command can proceed after the recovery state becomes ready.

Milestone 3 moves startup and scheduled recovery effects after `actor.initialised`. The implementer must preserve the existing ordering of retry timers, cleanup enqueueing, outbox replay, startup park report publication, scheduled ledger appends, scheduled runtime actions, scheduled report retry timers, scheduled failure publication, and warning logs. Acceptance is parity evidence from targeted startup-recovery and scheduled-runtime tests, including ledgers with pending outbox attempts and scheduled report retries, proving no duplicate publication intents beyond the existing outbox and dedupe model.

Milestone 4 moves recovered workflow resumption and activation triggers into the post-init phase. The implementer must register recovered sessions and workers after actor readiness, refresh the read model after deterministic local recovery, replay queued control operations only after the command gate opens, and trigger initial polling and remote-client startup last. Acceptance is targeted tests showing recovered workflow resumption still launches, queued control operations are not replayed while recovery is blocked, and remote-client activation is not attempted before recovery readiness.

Milestone 5 completes resilience validation. The implementer must run the focused tests from the earlier milestones, the deterministic unit suite, the relevant orchestrator contract shard, format checking, and both production lint gates. Acceptance is pre-publish evidence that `direnv exec . gleam test`, `direnv exec . scherzo-test-contract orchestrator`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all exit successfully from the repository root.

## Progress

- [x] (2026-06-29) Read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`.
- [x] (2026-06-29) Read the prepared review-doc target and confirmed the default directory is `docs/plans`.
- [x] (2026-06-29) Inspected `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/startup_recovery.gleam`, transition/effect modules, query runtime, read model, and relevant daemon/recovery tests.
- [x] (2026-06-29) Wrote this concise review document for the follow-up implementation bundle.
- [x] (2026-06-29) Validated this review document with `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1298-post-init-startup-recovery-review.md`; it reported `REVIEW_DOC_VALID=ok`.
- [x] (2026-06-29) Incorporated review feedback by tightening milestone acceptance evidence, explicit test obligations, manual dogfood scope, non-applicable docs/helper and provider-live/cache scope, contract validation, and production lint expectations.
- [x] (2026-06-29) Implemented Milestone 1 startup-timeout diagnostics in `src/scherzo/orchestrator/daemon.gleam`, including a test seam for low initializer timeouts, specific `daemon_actor_init_timeout` mapping, and actionable timeout logging with the last known startup phase.
- [x] (2026-06-29) Recorded the initializer-side effect inventory for the follow-up post-init move: `apply_startup_recovery`, `apply_scheduled_startup_recovery`, recovered workflow resumption, transition invariant checks, read-model refresh, queued control-operation replay, initial poll scheduling, and `StartRemoteClient`.

## Surprises & Discoveries

- Observation: `startup_recovery.load` already runs before the actor initializer, but it performs ledger replay, tracker refresh, recovery record append, workflow finalization, and scheduled recovery planning before the control plane starts. Evidence: `src/scherzo/orchestrator/startup_recovery.gleam` loads the ledger, fetches recovery task states, appends recovery records, and returns `StartupRecovery`.
- Observation: the control server is started before `actor.initialised`, so a client can see the control file while daemon snapshot queries still depend on an actor that is not yet selecting messages. Evidence: `start_control_plane` logs `control_server_started` before the initializer applies recovery and returns the actor subject.
- Observation: the remote client is currently started by a self-message, but that self-message is queued before `actor.initialised`. Evidence: the initializer sends `StartRemoteClient` immediately before constructing the selector and returning `actor.initialised`.

## Decision Log

- Decision: Use an actor-owned post-init recovery phase rather than a separate recovery process. Rationale: ordering, command gating, timers, worker registry updates, and read-model refresh are daemon state transitions and should stay serialized by the daemon actor. Date: 2026-06-29.
- Decision: Reject mutating operator commands until deterministic post-init recovery is complete. Rationale: this avoids races with scheduled recovery records, outbox attempts, recovered worker registration, and queued control-operation replay. Date: 2026-06-29.
- Decision: Keep network publication idempotence in the existing outbox and ledger model. Rationale: in-memory startup flags cannot survive crashes, while current outbox IDs and dedupe keys are designed for restart safety. Date: 2026-06-29.
- Decision: Treat manual daemon restart dogfood as a deferred human/operator check, not as a pre-publish implementation gate. Rationale: the required pre-publish evidence is deterministic fake-dependency testing and repository validation; live daemon dogfood is still useful after handoff but should not require provider credentials or live tracker state for this implementation pack. Date: 2026-06-29.

## Implementation Notes

Initializer-side effect inventory for the post-init refactor:

- `apply_startup_recovery(startup_recovery)` moves post-init.
- `apply_scheduled_startup_recovery(startup_recovery.scheduled)` moves post-init.
- `spawn_recovered_workflow_resumptions(startup_recovery.workflow_resumptions)` moves post-init.
- `check_startup_transition_invariants` moves post-init.
- `refresh_read_model` moves post-init.
- `replay_incomplete_control_operations(subject, state)` moves post-init.
- The initial `poll_scheduler.start(...)` immediate tick remains initializer-side in the current tree and should be delayed in Milestone 2/4.
- `process.send(subject, StartRemoteClient)` moves post-init.
- `startup_recovery.load(...)`, `start_query_service(...)`, `start_control_plane(...)`, `effect_runner.start(...)`, and daemon identity loading remain pre-actor work in this milestone.

## Outcomes & Retrospective

Milestone 1 is now partially implemented: startup timeout failures no longer collapse into the generic `daemon_start_failed` path, and the daemon emits `daemon_startup_timeout` with `initialiser_timeout_ms` and `last_startup_phase` fields. The larger post-init recovery refactor remains to be implemented in later milestones.

## Validation and Acceptance

This review document is accepted when `test -f docs/plans/LIV-1298-post-init-startup-recovery-review.md` succeeds and `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1298-post-init-startup-recovery-review.md` reports `REVIEW_DOC_VALID=ok`.

The follow-up implementation is accepted only with pre-publish evidence for all required outcomes. Side-effect inventory is evidenced by a checked audit in the implementation notes and targeted tests that exercise startup retry timers, cleanup enqueueing, outbox replay, startup park reports, scheduled failure publication, recovered workflow resumption, read-model refresh, queued control-operation replay, initial polling, and remote-client startup. Fast readiness is evidenced by a daemon startup test using a low initializer timeout and a recovery-heavy ledger with fake, non-network dependencies; the test must show `daemon.start` returns before blocked post-init recovery work completes. Query/control behavior is evidenced by tests showing read-only queries succeed during recovery, mutating commands return `Rejected("startup_recovery_in_progress")` during recovery, and the same command can succeed after recovery readiness when otherwise valid.

Idempotence and backward compatibility are evidenced by restart tests against ledgers with pending outbox attempts, scheduled report retries, active scheduled runs, disabled scheduled jobs, and startup park reports; the tests must show no duplicate publication intents beyond the existing dedupe model. Diagnostics are evidenced by a negative test that forces `actor.InitTimeout` and expects startup code `daemon_actor_init_timeout` plus an actionable timeout log that includes the initializer timeout and last known startup phase. Tests that block fake actors or recovery dependencies must use deterministic handshakes from `test/test_async.gleam` rather than ad hoc sleeps. Full pre-publish validation requires `direnv exec . gleam test`, `direnv exec . scherzo-test-contract orchestrator`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` to pass from the repository root. No browser check, docs/helper migration check, or provider-live/cache check is applicable to this daemon lifecycle plan; any manual daemon restart dogfood is deferred to a human/operator after implementation handoff unless reviewers request it before publish.

## Rollout, Recovery, and Idempotence

Rollout is an internal lifecycle refactor with no data migration. Existing ledgers with pending scheduled runs, retry timers, outbox attempts, scheduled failure retries, and parked workflow records must replay through the same projection and recovery planners. The safe rollout path is additive: add diagnostics and tests, add the post-init phase, move one recovery category at a time, then remove initializer calls only after parity tests pass. The implementation should be published only after deterministic tests, the orchestrator contract shard, formatting, and both lint gates pass; live daemon restart dogfood is a useful post-handoff operator check but is not required before publishing the implementation pack.

Recovery is to revert the refactor commits; no ledger cleanup should be required because record formats and dedupe keys do not change. Re-running the post-init recovery phase after a crash must be safe because outbox attempts, park reports, and scheduled failure publications use deterministic outbox IDs and dedupe keys, and scheduled recovery continues to fold from durable projection state rather than trusting an in-memory completion flag.

## Open Questions and Clarifications Needed

No open question blocks implementation. The only follow-up scoping point is whether pre-actor tracker refresh inside `startup_recovery.load` should get its own later degraded-startup plan if it remains a user-visible delay after initializer-side work moves post-init.
