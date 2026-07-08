# LIV-1443 unify daemon and transition state

This review defines a follow-up implementation plan. It is intentionally concise; detailed implementation steps, tests, interfaces, dependencies, and artifact notes are carried in the structured implementation pack for this issue.

## Purpose / Big Picture

Scherzo should have one authoritative in-memory model for the task state owned by the pure orchestrator transition core. After the follow-up implementation, a maintainer can add or change a transition-owned field in one record and have the compiler force all required construction sites to account for it, instead of relying on hand-written copy and merge code in the daemon. Operators should observe unchanged dispatch, retry, recovery, worker, query, and control behavior; the visible win is reduced silent-data-loss risk and simpler state handoff between the daemon shell and the pure transition runner.

## Problem Framing and Constraints

The daemon currently stores several transition-owned fields directly in `src/scherzo/orchestrator/daemon.gleam`, copies them into `src/scherzo/orchestrator/transition_types.gleam` before running transitions, then merges them back field by field. This is hazardous because new fields in `transition_types.State` can be forgotten in both directions, and the merge rule treats an output equal to the input as unchanged, so a transition cannot intentionally reset a field to its input value while shell-side changes are also present.

The implementation must preserve the functional-core boundary. `src/scherzo/orchestrator/transition.gleam` and transition helpers must remain pure and must not import the daemon actor or process-specific modules. The daemon may keep shell-owned runtime concerns such as timers, process monitors, control/query handles, workflow reload state, tracker clients, and effect-runner handles, but logical transition state should have a single owner.

## Strategy Overview

Embed the transition-owned record directly inside the daemon state and make that record the single source of truth for logical orchestrator state. The daemon transition shell should pass this core state to the transition runner and store the resulting core state back without per-field copy or equality-based merge. To avoid interpreting effects against stale core state, the shell runner should set the embedded core to each transition output before interpreting that transition's effects, then use the embedded core after those effects as the input to the next transition message. Shell callbacks that mutate the same logical domains must update that embedded core state explicitly rather than separate top-level daemon fields.

The two values that are currently derived while constructing transition state need an explicit ownership decision. `retry_refresh_generations` should move into the core state because transitions already insert and delete refresh generations. `next_session_sequence` should also move into the core state as the logical sequence used by dispatch, while the worker registry remains responsible only for live worker handles, PIDs, monitors, and command subjects. This keeps pure transition decisions deterministic and keeps process resources in the daemon shell.

The dispatch-recovery cleared-claims side channel should disappear. Claim clearing must be represented as a normal transition-visible state change or transition effect so no daemon-only list can silently override transition output during merge.

## Alternatives Considered

Keeping both state records and adding comments or tests around the copy/merge layer was rejected because it still leaves correctness dependent on manual field synchronization and an equality heuristic. Replacing `merge_transition_field` with a smarter per-field conflict resolver was rejected because it preserves the same split-brain ownership and still requires every new field to be wired twice. Moving daemon process handles, timers, and clients into `transition_types.State` was rejected because that would pollute the pure transition boundary and break the architecture that separates pure decisions from impure effects.

Passing `retry_refresh_generations` and `next_session_sequence` as read-only transition inputs was considered. It is acceptable as a short-lived migration aid if it keeps early milestones small, but the preferred final state is core ownership for both values because transitions already mutate them or use them to reserve identifiers.

## Risks and Countermeasures

The main risk is changing merge semantics. Today shell effects can update the daemon's `runtime`, `workers`, `pending_claims`, `pending_dispatch_validations`, `pending_review_lane_preflights`, and `next_dispatch_validation_generation` while transitions update corresponding copied fields; if the transition output equals its input, the shell value wins. Removing that heuristic means every shell handler that logically changes core state must update the embedded core state directly, and tests must cover the flows where shell work and transition work happen in the same run.

A second risk is worker-start double mutation. Claim transitions currently create logical worker entries and runtime running entries, while the daemon worker spawn path also applies runtime start and registers process handles. The implementation must make logical start state transition-owned and keep daemon registry mutation process-owned, with tests proving that successful and failed worker starts do not duplicate or drop running state.

A third risk is retry refresh divergence. The retry scheduler owns timers and in-flight effect bookkeeping today, while transitions already carry `retry_refresh_generations`. The implementation must preserve timer cancellation and refresh completion behavior while making logical refresh generation state visible in the core record.

A fourth risk is accidentally weakening the pure boundary. The architecture guardrail tests must continue to reject orchestrator modules importing the daemon, and any helper introduced for embedded core state must avoid process, actor, tracker-client, or shell-only dependencies.

A fifth risk is proving only the happy path. The implementation must carry negative and duplicate-path coverage for failed worker starts, stale or duplicate effect completions, retry refresh begin/finish races, and repeated dispatch-recovery polls, because those are the cases most likely to expose a lost core-state update after the merge layer is removed.

## Scope Boundaries

For this planning issue, scope is exactly this review document under `docs/plans/` plus the structured implementation-pack submission captured by Scherzo. No production code, tests, generated canonical bundle, or workflow helper should be changed by this issue.

For the follow-up implementation, in scope are `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/daemon_transition_shell.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, transition-shell tests, transition tests, retry scheduler interaction points, worker registry interaction points, startup and dispatch recovery paths, and read-model/query snapshot callers that currently read duplicated daemon fields.

Out of scope are public control/query JSON changes, ledger schema changes, workflow YAML changes, tracker provider behavior changes, retry algorithm redesign, worker supervision redesign, UI/browser work, provider-live validation, cache behavior changes, and the separate capability-records refactor. If implementation discovers a required public protocol, durable data, or workflow-helper change, it should stop and rescope rather than fold that change into this refactor.

## Milestones

Milestone 1 characterizes the current seam before changing ownership. It records which daemon fields mirror transition state, which shell handlers mutate them, and where the equality-based merge changes behavior. The proof is focused tests or explicit characterization notes that fail if the copy/merge seam is accidentally changed without understanding it, including a case where the old equality heuristic would let a shell-side mutation win over an intentional transition reset.

Milestone 2 introduces the embedded core-state field in daemon state while keeping behavior equivalent. At the end of this milestone, initialization and direct daemon reads use `state.core` or the chosen embedded-field name, but the old top-level fields may still exist as temporary aliases only if tests remain green.

Milestone 3 changes `daemon_transition_shell.Context` so it no longer accepts both `transition_state_from_state` and `merge_transition_state`. The shell runner should operate on a getter and setter for the embedded core state, or on a specialized daemon context. For each handled message, it should set the shell state's embedded core to the transition output before interpreting effects, then read the embedded core after effects and use that value for the next message, so shell callbacks cannot be overwritten by a final direct replacement.

Milestone 4 migrates shell handlers that currently mutate mirrored fields. Worker start, worker removal, retry refresh, claim release, validation, review-lane preflight, shutdown, snapshot reply, and operator-command follow-up paths must update the embedded core state intentionally and leave process-owned daemon fields separate. This milestone is not complete until both success and error paths are covered for worker start, validation/preflight completion, retry refresh, claim release, and query snapshot parity.

Milestone 5 removes the dispatch-recovery cleared-claims list and expresses claim clearing through normal transition state. The proof is recovery coverage showing that repeated or duplicate recovery polls do not re-dispatch already cleared claims, that duplicate/conflicting claim-clearing attempts are no-ops, and that no daemon-side list is needed after transition execution.

Milestone 6 removes the old top-level duplicated daemon fields and the copy/merge helpers. At the end, adding a field to `transition_types.State` should require only normal record construction updates, and no `merge_transition_field`, `transition_state_from_daemon`, or `merge_transition_state` function remains.

Milestone 7 runs full validation and records outcomes. The result should be behavior-preserving: existing operator-visible daemon behavior remains unchanged, while the silent field-drop and equality-merge hazards are gone. No browser, provider-live, provider-cache, or workflow-helper migration evidence is pre-publish blocking for this refactor unless implementation expands the scope into those surfaces; if that happens, the work must stop and the plan must be respecified before publishing.

## Progress

- [x] (2026-07-08) Read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md` and used the split review-doc plus implementation-pack contract.
- [x] (2026-07-08) Confirmed the prepared review document target is the directory `docs/plans`.
- [x] (2026-07-08) Inspected the daemon state, transition state, transition shell context, merge helpers, retry scheduler, worker registry, and architecture guardrail surfaces relevant to this refactor.
- [x] (2026-07-08) Wrote this concise review document as a planning artifact only; production code was not changed.
- [x] (2026-07-08) Incorporated review feedback by tightening per-message core synchronization, negative/error-path, duplicate/idempotency, full-validation, and out-of-scope evidence obligations for the implementation handoff.

## Surprises & Discoveries

- Observation: `transition_types.State` already includes a `lifecycle` field that the daemon currently initializes as empty during copy rather than storing as daemon state.
  Evidence: `transition_state_from_daemon` constructs `lifecycle: transition_types.empty_lifecycle()` while other fields are copied or derived.
- Observation: `next_session_sequence` is currently stored in `worker_registry.Registry`, even though claim transitions read and increment it through `transition_types.State`.
  Evidence: `transition_state_from_daemon` reads `worker_registry.next_session_sequence(state.registry)`, and claim transition code increments `next_session_sequence` when reserving run/session IDs.
- Observation: The transition shell test harness already models a shell state that contains `transition_state`, which is close to the desired embedded-core shape.
  Evidence: `test/orchestrator_daemon_transition_shell_test.gleam` defines `ShellState(transition_state: transition_types.State, ...)` and merges by replacing that field.

## Decision Log

- Decision: Make the embedded transition-owned record the final source of truth rather than preserving top-level mirrored daemon fields. Rationale: this is the only option that lets the compiler catch new transition-owned fields instead of relying on manual copy and merge updates. Date: 2026-07-08.
- Decision: Prefer moving `retry_refresh_generations` and `next_session_sequence` into the core record for the final state. Rationale: transitions already mutate refresh generations and reserve session sequence values, while shell-owned schedulers and registries should retain only timers and process handles. Date: 2026-07-08.
- Decision: Keep daemon process resources out of the core state. Rationale: preserving the pure functional core requires process subjects, monitors, timers, tracker clients, control servers, and effect runners to remain daemon-shell concerns. Date: 2026-07-08.
- Decision: Treat dispatch-recovery claim clearing as normal transition state rather than a daemon merge escape hatch. Rationale: side-channel state that is applied only during merge recreates the silent overwrite hazard this plan is removing. Date: 2026-07-08.
- Decision: Make negative/error-path and duplicate/idempotency tests explicit acceptance obligations, while keeping browser, provider-live, provider-cache, and workflow-helper evidence out of scope unless implementation touches those surfaces. Rationale: review feedback emphasized that semantic acceptance must travel into the implementation pack rather than relying on keyword checks or happy-path validation. Date: 2026-07-08.
- Decision: Synchronize the embedded core before effect interpretation and continue from the post-effect embedded core. Rationale: a final direct replacement would remove the merge layer but could still overwrite shell callbacks that intentionally update logical core state during effect handling. Date: 2026-07-08.

## Outcomes & Retrospective

This planning issue produced a self-contained implementation direction for removing the daemon/transition copy-merge layer. Implementation outcomes remain to be filled in by the follow-up implementation task after each milestone, with special attention to whether merge-semantics risks were fully retired without changing operator-visible behavior.

## Validation and Acceptance

This planning issue is accepted when this file exists at `docs/plans/LIV-1443-unify-daemon-transition-state-review.md`, the review-doc validator reports it has all required non-empty sections and no forbidden mechanical sections, and Scherzo captures the structured implementation-pack submission for LIV-1443.

The follow-up implementation is accepted only when no per-field daemon-to-transition copy or equality-based merge remains, the daemon stores one embedded transition-owned state record, transition effects are interpreted with the latest embedded core and the next message continues from the post-effect embedded core, `dispatch_recovery_cleared_pending_claims` is removed, derived retry/session inputs have explicit final ownership, architecture guardrail tests still enforce the pure transition boundary, targeted transition-shell and daemon tests cover the migration, and the repository validation gates pass through direnv.

Targeted implementation tests must include negative/error-path and idempotency coverage. At minimum they must exercise successful and failed worker start, stale or duplicate worker/effect completion messages, retry refresh begin and finish behavior with generation mismatches, validation and review-lane preflight completion, claim release or ledger-append failure behavior, repeated dispatch-recovery polling that would previously use `dispatch_recovery_cleared_pending_claims`, and read-model/query snapshot parity for counts derived from runtime, workers, pending claims, validations, and preflights. One regression test must demonstrate the removed equality-merge hazard by proving that an intentional transition reset is not lost when a shell callback also changes the same logical domain.

Before publish, run `direnv exec . gleam format --check src test`, `direnv exec . gleam test -- --suite unit`, `direnv exec . gleam test -- --suite contract` if daemon contract or workflow/helper surfaces are touched, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

## Rollout, Recovery, and Idempotence

Rollout for this planning issue is a normal review of this Markdown file and the captured structured implementation pack. Recovery is to edit or revert this file and resubmit corrected structured output; the canonical bundle should be materialized by Scherzo, not written by hand.

Rollout for the follow-up implementation should use behavior-preserving commits at each milestone. Each commit should leave tests green and avoid durable migrations. If a milestone exposes a behavior change, revert that slice while keeping any characterization tests that document current behavior. The planned edits are idempotent because they change in-memory ownership and test coverage only; no ledger rewrite, provider cache invalidation, or operator data cleanup is required.

## Open Questions and Clarifications Needed

No open question blocks implementation handoff. If implementation discovers that `next_session_sequence` cannot move out of `worker_registry.Registry` without redesigning process-handle registration, use an explicit read-only transition input as a temporary bridge and record the reason before merging.
