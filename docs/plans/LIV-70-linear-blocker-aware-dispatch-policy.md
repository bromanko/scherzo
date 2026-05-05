# Add Linear blocker-aware dispatch policy

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo dispatches Linear issues to agents. Operators use Linear dependencies to express ordering: if issue B blocks issue A, humans expect Scherzo to wait on A until B reaches a completed state. After this change, Scherzo performs a fresh fail-closed dependency validation immediately before any handoff claim for a selected issue, and it never creates a Scherzo claim, handoff claim effect, workspace run, or worker start when that validation observes a non-terminal blocker, missing blocker state, or incomplete blocker data.

The visible outcome is intentionally log-based. A candidate issue with any observed non-terminal Linear blocker remains in its current Linear state, receives no Scherzo claim or worker run, and produces a structured daemon warning such as `linear_dependency_blocked_candidate`, `linear_dependency_claim_validation_blocked`, or `linear_dependency_retry_blocked`. The warning includes the issue id, issue identifier, blocker fingerprint, blocker summary, completeness flag, and the dispatch phase. Scherzo does not post Linear comments or move issues to a special blocked state for this feature. When all observed direct blockers move into Scherzo's configured terminal states and the relation page is complete, the same issue becomes eligible for the normal workflow-label, slot, parking, retry, handoff, and worker-start path.

The strongest guarantee in this plan is not atomic across Linear itself. If Linear cannot provide a mutation that says "claim this issue only if all blockers are terminal," a dependency could still be added after the final validation response and before the claim mutation reaches Linear. This plan closes the race Scherzo can control: it validates immediately before claim and fails closed on every uncertainty it observes.

## Problem Framing and Constraints

Today the operator workflow relies on humans adding Linear dependencies, but the dispatcher can treat a dependency-blocked issue like any other active candidate unless every dispatch path has complete and fresh blocker data. That is risky because Scherzo can claim work out of order, move an issue into a handoff state, or start an agent while prerequisite work is still unfinished. The problem is real in this repository because Linear candidate fetch, retry refresh, handoff claim, parking, workflow-label validation, and worker start are already integrated, so a missing dependency gate can lead directly to a real worker run.

This plan solves the dispatch policy, data shape, reporting, and tests for Linear direct `blocks` relations. It does not add a new operator command, a new Linear state, a new configuration switch, a new dependency-management UI, recursive dependency traversal, or Linear comments for dependency blocks. It preserves the intended workflow: humans express dependencies in Linear; Scherzo reads and enforces the direct blockers it observes.

The main constraint is safety. If Scherzo cannot determine whether the direct blocker list is complete, or if any blocker state is missing, it must fail closed and skip the issue rather than assume it is safe. This plan explicitly accepts a `first: 100` incoming-relation cap for blocker data. If Linear reports `inverseRelations.pageInfo.hasNextPage == true`, Scherzo treats the issue as dependency-blocked because the blocker list is incomplete. That can keep an issue with more than 100 incoming relations blocked forever, even if every fetched blocker is terminal. This is an intentional product tradeoff for this plan: the condition must be operator-visible through `incomplete=true` structured warnings and tests, rather than hidden or silently treated as safe.

Another constraint is avoiding Linear comment spam. Blocked dependency reporting must not create repeated comments. This plan chooses not to post Linear comments for dependency blocks at all, and instead uses structured daemon logs plus a bounded in-memory de-duplication cache.

## Strategy Overview

The implementation should treat Linear dependencies as a dispatch precondition. Linear candidate and refresh queries should include the incoming dependency relations needed to decide whether a candidate is blocked. The core policy should answer one question: are the fetched direct blockers complete enough to trust, and is every direct blocker terminal according to `config.tracker.terminal_states`? If the answer is no, the issue is not dispatchable.

The policy is checked in three places. First, candidate fetch responses carry blocker data so most blocked issues are skipped before workflow-label triage. Second, retry refresh responses use the same blocker policy so a scheduled retry cannot bypass the dependency gate. Third, immediately before Scherzo enqueues the handoff claim for a selected issue, the daemon refreshes that issue by id and reruns the same dispatch preconditions, workflow policy, route selection, and slot checks on the refreshed response. This final validation limits the race between the poll page and claim mutation and proves, in tests, that a non-terminal blocker observed at claim time prevents the claim.

This is proportionate because it reuses existing tracker, Linear, orchestrator, retry, parking, and handoff structure. It does not introduce a scheduler rewrite or a separate dependency graph. Scherzo only needs to know whether a candidate's direct blockers are terminal and complete under the chosen `first: 100` cap; Linear remains the source of truth for dependency relationships.

## Alternatives Considered

The simplest alternative is to filter only in the initial Linear candidate query and skip any issue whose fetched blocker list contains a non-terminal state. That catches common cases but is insufficient because a dependency can be added after the poll page is fetched and before Scherzo performs the handoff claim. The final per-issue validation is the smallest extra step that materially improves the guarantee.

Another alternative is to fetch every incoming relation page for every candidate and selected issue. That would avoid the accepted `first: 100` cap, but it would add pagination loops, extra Linear calls, and more failure modes to the broad candidate-fetch path. This plan chooses the smaller safe behavior: fetch up to 100 direct incoming relations, fail closed when Linear says there are more, and make incomplete data visible through logs and tests. If operators later need automatic dispatch for issues with more than 100 blockers, that should be a follow-up plan that implements full pagination deliberately.

A third alternative is to move blocked issues to a special Linear state or park them internally. That would be noisy and would fight the operator workflow. Linear dependencies already communicate why the issue is waiting; changing state or parking would add a second source of truth and could require humans to unpark issues manually. This plan leaves blocked issues in their current Linear state.

A fourth alternative is to post a comment every time Scherzo skips a blocked issue. That would make the behavior obvious but would create comment spam on every poll or daemon restart. This plan uses structured logs and an in-memory report cache instead, with no Linear comment for dependency blocks.

## Risks and Countermeasures

The largest risk is misinterpreting Linear's relation direction. In this plan, issue A is blocked by issue B only when A's `inverseRelations.nodes` contains a relation with `type == "blocks"` whose `issue` is B. Outgoing `relations.nodes` with `type == "blocks"` means A blocks another issue and must not block A itself. The countermeasure is a real-shape GraphQL fixture and parser tests for both incoming and outgoing directions before production query and decoder churn.

Another risk is failing open when Linear truncates relation data. The countermeasure is to request `pageInfo`, carry a completeness flag in the issue model, and treat `hasNextPage == true`, missing `pageInfo`, missing `inverseRelations`, or missing blocker state as blocked. Because this plan accepts the `first: 100` cap, the operator-visible countermeasure is a warning log with `incomplete=true`, not a background pagination success path.

A third risk is blocking issues forever because terminal-state names differ between Linear teams. The countermeasure is to use Scherzo's configured `tracker.terminal_states`, normalized through the existing issue-state comparison helpers, rather than hard-coding `Done` or using Linear's state type.

A fourth risk is delaying workflow-label feedback on blocked issues. That is intentional: dependency blocking is checked before workflow-label triage. Scherzo should not comment about a missing or invalid workflow label while an issue is not ready to run for dependency reasons. Once blockers are terminal and relation data is complete, existing workflow-label validation runs normally.

A fifth risk is duplicate or stale daemon work around final validation. The countermeasure is a dedicated pending-validation state keyed by issue id, a generation number on each validation request and result, duplicate suppression across poll ticks, stale-result checks, and a rule that session sequence reservation, workspace path creation, `PendingClaim`, and `effect_runner.ClaimIssue` are created only after a fresh validation result passes every pre-claim check.

A sixth risk is retry state getting stuck when a retry refresh becomes dependency-blocked. The countermeasure is a named transition that clears `RuntimeState.retry_attempts`, releases `RuntimeState.claimed`, cancels any retry timer idempotently, records a retry-cancel ledger entry with reason `linear_dependency_blocked`, emits no new Linear comment or state mutation, and leaves future dispatch to the normal poll after blockers change.

A final risk is the tiny race between a successful final validation query and the subsequent Linear handoff claim mutation. The plan reduces this window by validating immediately before claim and failing closed on any observed blocker uncertainty. See the open clarification about whether Linear can provide an atomic conditional claim.

## Progress

- [x] (2026-05-04 00:00Z) Drafted the ExecPlan for review.
- [x] (2026-05-04 00:00Z) Incorporated review feedback on final validation state, pagination policy, retry-blocked behavior, final-validation tests, reporting validation, and concrete step granularity.
- [ ] Confirm Linear's real GraphQL relation payload shape with the fixture described in this plan before changing production decoders.
- [ ] Add or normalize blocker data in the tracker issue model and Linear parser.
- [ ] Add core blocker policy tests and make blocker checks apply to every active candidate state.
- [ ] Add final pre-claim dispatch validation in the daemon and one-shot service paths.
- [ ] Add blocked-dependency reporting without Linear comments.
- [ ] Add retry, parking, workflow-label, handoff, and fixture tests.
- [ ] Run the validation commands and update Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: The current tree already has partial blocker-shaped data in the Linear and tracker layers, including `BlockerRef`, `Issue.blocked_by`, and Linear decoding from `inverseRelations`.
  Evidence: `src/scherzo/tracker/issue.gleam` defines blocker fields, and `src/scherzo/linear.gleam` decodes `inverseRelations` relations with `type == "blocks"`.
- Observation: The current core dispatch preconditions already call a blocker predicate, but the observed predicate is tied to the issue state named `Todo`.
  Evidence: `src/scherzo/orchestrator/core.gleam` contains `dispatch_preconditions_satisfied_without_slot_capacity` and `retry_candidate_preconditions_satisfied_without_slot_capacity` paths that call blocker logic, and the blocker logic branches on `issue_state.todo_key()`.
- Observation: Existing workflow-label invalid reporting already has an anti-spam cache pattern that can guide blocked-dependency reporting.
  Evidence: `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/state.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/linear_triage.gleam` implement cached invalid-workflow reporting.
- Observation: The daemon already has `PendingClaim`, `pending_claims`, `RetryRefreshFinished`, `RefreshRetry`, `retry_scheduler.finish_refresh`, `core.ReleaseClaim`, and ledger records for `RetryScheduled` and `RetryCancelled`.
  Evidence: `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/state.gleam`, and `src/scherzo/state/record.gleam` contain those types and handlers.

## Decision Log

- Decision: A Linear issue is blocked only by incoming `blocks` relations read from `inverseRelations`; outgoing `relations` are ignored for this policy.
  Rationale: In Linear, if B blocks A, A sees the inverse of B's outgoing `blocks` relation. Treating outgoing relations as blockers would prevent Scherzo from working on issues that are actually prerequisites for others.
  Date: 2026-05-04
- Decision: Direct blockers are sufficient for Scherzo's dispatch policy.
  Rationale: Linear already computes and presents dependency structure to humans. Scherzo only needs to avoid claiming an issue whose direct Linear blockers are not terminal; recursively walking dependency graphs would add cost and ambiguity without being requested.
  Date: 2026-05-04
- Decision: A blocker satisfies the policy only when its state is present and matches `config.tracker.terminal_states` after normal issue-state normalization.
  Rationale: Scherzo deployments configure terminal states, and fail-closed behavior is safer than assuming an unknown blocker state is complete.
  Date: 2026-05-04
- Decision: Blocked issues remain in their current Linear state and receive no Linear comment from this feature.
  Rationale: The Linear dependency itself is the operator-facing explanation. Additional comments or state changes create noise and duplicate the source of truth.
  Date: 2026-05-04
- Decision: Run dependency blocking before workflow-label validation and before handoff claim.
  Rationale: A blocked issue is not ready for workflow triage or claim. This ordering preserves the human dependency workflow and prevents claim comments or state transitions on blocked work.
  Date: 2026-05-04
- Decision: Accept the `inverseRelations(first: 100)` cap for this plan and fail closed when Linear reports more pages.
  Rationale: Fetching all relation pages for every candidate would be broader than the safety bug requires. Failing closed with explicit `incomplete=true` logging is safe, observable, and reversible; full pagination can be added later if operators need issues with more than 100 blockers to dispatch automatically.
  Date: 2026-05-04
- Decision: Pending final validation does not reserve a dispatch slot, but a successful validation must recheck current slot capacity before creating `PendingClaim`.
  Rationale: Holding slots for asynchronous validation can starve real dispatch capacity, while rechecking slots immediately before claim prevents over-capacity claims. If capacity disappears during validation, Scherzo skips the claim for that tick and continues safely.
  Date: 2026-05-04
- Decision: Retry refresh that observes a dependency block cancels the retry attempt instead of scheduling exponential retry.
  Rationale: A human Linear dependency is not a transient system failure. Linear dependency changes are the release mechanism, so repeated retry timers would create noise and could keep stale claimed state alive.
  Date: 2026-05-04

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam service. The repository root contains `gleam.toml`; source files live under `src/`, and tests live under `test/`. The tracker abstraction is in `src/scherzo/tracker.gleam`, and the Linear GraphQL implementation is in `src/scherzo/linear.gleam`. A tracker issue is represented by `src/scherzo/tracker/issue.gleam`.

A daemon poll fetches candidate issues from the tracker, sorts them, applies dispatch preconditions, selects a workflow, performs handoff claim work, and then starts a worker. The current daemon path is in `src/scherzo/orchestrator/daemon.gleam`; the synchronous or one-shot service path is in `src/scherzo/orchestrator/service.gleam`; shared dispatch predicates are in `src/scherzo/orchestrator/core.gleam`. Handoff means Scherzo's Linear-side claim behavior, including optional claim comments or state transitions, and it is represented by `effect_runner.ClaimIssue` in `src/scherzo/orchestrator/effect_runner.gleam`.

A Linear blocking relation is a dependency between two issues. In this plan, if issue B blocks issue A, then A must not be claimed while B is not terminal. Terminal means that B's state name matches one of `config.tracker.terminal_states` in `src/scherzo/config/types.gleam`. Active means A's state matches one of `config.tracker.active_states`; active controls which issues Scherzo considers, but it does not make a blocker terminal.

Workflow-label validation is the policy in `src/scherzo/workflow_policy.gleam` that requires exactly one allowed workflow label when enabled. Invalid workflow reporting can post comments or move issues according to `LinearContractConfig`. Dependency blocking must run before that reporting so a blocked issue does not get workflow-label comments until it is actually eligible to run.

Parking is Scherzo's internal hold mechanism for issues that should not dispatch until an operator unblocks them or an observed issue fingerprint changes. Parking state lives in `src/scherzo/orchestrator/state.gleam`, and parking decisions are applied in `src/scherzo/orchestrator/core.gleam`. Retry is Scherzo's scheduled reattempt mechanism after failures or continuation needs, and retry refresh paths are in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/core.gleam`.

## Preconditions and Verified Facts

The repository is a Gleam project. From the repository root, `direnv exec . gleam test` is the expected test command when direnv is allowed. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command through direnv.

At drafting time, `src/scherzo/tracker/issue.gleam` already defines `BlockerRef` with optional `id`, `identifier`, and `state`, and `Issue` already contains a `blocked_by` list. `src/scherzo/linear.gleam` already has candidate and state-refresh GraphQL query strings that request `inverseRelations` and decode `type == "blocks"` into `Issue.blocked_by`. `test/linear_test.gleam` already has tests showing incoming `inverseRelations` decode as blockers and outgoing `relations` do not.

At drafting time, `src/scherzo/orchestrator/core.gleam` already has dispatch predicates named `dispatch_preconditions_satisfied_without_slot_capacity`, `dispatch_preconditions_satisfied`, `should_dispatch`, `retry_candidate_preconditions_satisfied_without_slot_capacity`, and `retry_candidate_preconditions_satisfied`. These are the right central policy points, but the future implementation must verify and change any state-specific blocker check so blockers apply to every active candidate state, not only to issues named `Todo`.

At drafting time, `src/scherzo/orchestrator/daemon.gleam` fetches candidates, checks core dispatch preconditions before workflow-label validation, enqueues `effect_runner.ClaimIssue`, and only starts workers after handoff claim succeeds. This is the right place to insert final pre-claim validation because a blocked issue must not reach `ClaimIssue`.

At drafting time, `src/scherzo/orchestrator/daemon.gleam` contains a private `PendingClaim` type with `issue`, `workspace_path`, `run_id`, `session_sequence`, `recovery`, and `remaining_candidates`, plus a `pending_claims: Dict(String, PendingClaim)` field on daemon `State`. The new final-validation state should mirror this continuation style but must sit before `PendingClaim`.

At drafting time, `src/scherzo/orchestrator/effect_runner.gleam` contains `RefreshRetry(issue_id, generation, client)` and `RetryRefreshFinished(issue_id, generation, Result(List(tracker_issue.Issue), error.TrackerError))`. The final validation effect should follow this effect/result pattern but normalize missing, duplicate, and id-mismatched responses into non-claiming validation failures.

At drafting time, `src/scherzo/orchestrator/service.gleam` has a one-shot `dispatch_candidates` path that calls `core.should_dispatch`, selects a workflow, and then calls `workflow_run.execute` in `dispatch_issue`. This path also needs a fresh validation before execution because it can otherwise use stale candidate data.

## Scope Boundaries

In scope: Linear candidate and refresh GraphQL query shape; parsing and carrying blocker completeness; core blocker policy; daemon and one-shot service pre-claim validation; retry handling; parking interaction; workflow-label ordering; non-spam structured log reporting; tests and fixtures.

Out of scope: fetching more than 100 incoming blocker relations, adding a new Linear state for blocked issues, posting dependency-block comments, changing how humans create Linear dependencies, adding recursive dependency traversal, changing workflow selection semantics, adding new operator commands, or changing the meaning of configured active and terminal states.

The implementation should touch these files or close equivalents if names drift before implementation: `src/scherzo/tracker/issue.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/state.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/state/record.gleam`, `test/linear_test.gleam`, `test/orchestrator_core_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_service_test.gleam` if it exists or the nearest service test file if it does not, and a new fixture under `test/fixtures/linear/`.

## Milestones

Milestone 1 establishes the Linear data contract before production churn. At the end, a real-shape fixture proves whether Linear returns the expected `inverseRelations(first: 100) { nodes { type issue { id identifier state { name } } } pageInfo { hasNextPage endCursor } }` shape. Parser tests prove direction mapping and fail-closed behavior for missing, unknown, or truncated blocker data.

Milestone 2 centralizes the dispatch policy. At the end, core functions expose a blocker decision that says either the issue is dependency-ready or names why it is blocked. Tests prove that every non-terminal, unknown, missing-state, or incomplete blocker prevents dispatch for all active issue states, while terminal blockers allow normal dispatch.

Milestone 3 adds final validation before claim in the daemon. At the end, the daemon has explicit pending-validation state, validation effect/result constructors, stale-result handling, duplicate suppression, no slot reservation while validation is pending, and continuation through `remaining_candidates`. Tests prove no claim effect, pending claim, session sequence, workspace run, or worker start occurs when final validation fails any refreshed precondition.

Milestone 4 adds retry and one-shot service safety. At the end, retry refresh uses the same blocker decision, dependency-blocked retries are canceled and internally released without exponential retry, and the one-shot service path refreshes a selected issue before `workflow_run.execute`.

Milestone 5 adds reporting and interaction tests. At the end, blocked issues remain in Linear's current state, receive no Linear comment, appear in structured logs without repeated spam for the same observed blocker fingerprint, and interact correctly with workflow-label validation, retry, parking, active states, and handoff.

Milestone 6 validates the full behavior. At the end, fake tracker tests and the real-shape GraphQL fixture demonstrate that Scherzo will not claim an issue while non-terminal, unknown, or incomplete blockers are observed, and the full Gleam test suite and formatter pass.

## Plan of Work

Start by proving the Linear schema assumption. Create `test/fixtures/linear/blocked_issue_candidate_response.json` from a real or captured GraphQL shape, anonymized but with exact field names and nesting. If the field name, pagination shape, or relation direction differs from the assumption in this plan, update the fixture, query prose, and Decision Log before changing production decoders.

In `src/scherzo/tracker/issue.gleam`, keep `BlockerRef` as the direct blocker representation, and extend `Issue` with `blocked_by_complete: Bool`. The field must be `True` only when the incoming blocker relation page is complete and all fields needed for dispatch were present. Fake tracker data and test helpers should set it to `True` unless a test is explicitly modeling truncated or missing relation data.

In `src/scherzo/linear.gleam`, update `candidate_query` and `state_refresh_query` to request incoming relations with the accepted cap and page info. The intended shape is `inverseRelations(first: 100) { nodes { type issue { id identifier state { name } } } pageInfo { hasNextPage endCursor } }`. Decode only `type == "blocks"` from `inverseRelations`. Ignore outgoing `relations` entirely for blocker policy. Decode `blocked_by_complete` as `!pageInfo.hasNextPage`. If `inverseRelations`, `pageInfo`, or a blocker state field is absent in a response used for dispatch, either return `LinearUnknownPayload` or set `blocked_by_complete` to `False`; do not silently treat it as no blockers.

In `src/scherzo/orchestrator/core.gleam`, replace any Todo-specific blocker check with a state-independent policy. Add a public decision helper named `blocker_decision(config, issue)`. The helper should return satisfied only when `issue.blocked_by_complete` is `True` and every direct blocker has `Some(state)` where that state is terminal under `config.tracker.terminal_states`. Existing boolean precondition functions may call this helper. The same helper must be used by normal dispatch and retry dispatch.

In `src/scherzo/orchestrator/state.gleam`, add a small in-memory report cache for blocked dependency observations. Model it after the existing invalid workflow report cache, but keep it separate. The record should include issue id, identifier, reporting phase, blocker fingerprint, observed issue `updated_at`, terminal-state policy fingerprint, attempted timestamp, and last result. The first implementation only suppresses repeated logs; it must not post Linear comments.

In `src/scherzo/orchestrator/core.gleam`, add helpers to compute the blocked-dependency fingerprint, decide whether a blocked dependency observation was already reported, mark it as reported, and clear it when an issue becomes dependency-ready. The fingerprint should include reporting phase, blocker id, identifier, state, `blocked_by_complete`, whether the decision is incomplete, and the normalized configured terminal states. If a blocker state changes from non-terminal to terminal, or completeness changes from incomplete to complete, the fingerprint changes and Scherzo can log a new observation or proceed normally.

In `src/scherzo/orchestrator/effect_runner.gleam`, add a final dispatch-validation effect. The effect should call `tracker.Client.fetch_issue_states_by_ids([issue_id])` or the current equivalent tracker refresh method and return exactly one refreshed `Issue` for the requested id. Any tracker error, missing issue, duplicate issue, or id mismatch must be represented as a validation failure and must not lead to claim. Add the effect kind string `validate_dispatch_claim`, add crash-result handling, and add unit tests or daemon tests that can observe the effect being enqueued.

In `src/scherzo/orchestrator/daemon.gleam`, add pending-validation state before `PendingClaim`. Define a private type equivalent to:

    type PendingDispatchValidation {
      PendingDispatchValidation(
        issue: tracker_issue.Issue,
        remaining_candidates: List(tracker_issue.Issue),
        generation: Int,
        requested_at_ms: Int,
      )
    }

Add daemon `State` fields equivalent to:

    pending_dispatch_validations: Dict(String, PendingDispatchValidation)
    next_dispatch_validation_generation: Int

Initialize the dict to `dict.new()` and the generation to `1`. Include `pending_dispatch_validations` in active-or-pending checks that suppress duplicate dispatch for the same issue. Do not include pending validations in `dispatch_slots_used` and do not count them as per-state slot usage. Pending validation is a soft wait only; the successful validation handler must recheck slots before claim.

Change `handle_valid_workflow_candidate` so that, after clearing invalid workflow reports and confirming `can_reserve_dispatch_slot`, it calls a new `begin_dispatch_validation(state, issue, remaining_candidates)` instead of `dispatch_issue_with_continuation`. `begin_dispatch_validation` must skip and continue `dispatch_candidates(remaining_candidates, state)` if the issue already has an active run, runtime claim, pending claim, or pending validation. Otherwise it stores `PendingDispatchValidation`, increments `next_dispatch_validation_generation`, enqueues `effect_runner.ValidateDispatchClaim(issue_id: issue.id, generation: generation, client: state.tracker_client)`, logs `linear_dependency_claim_validation_started` at `info`, and returns without dispatching lower-priority remaining candidates until the result arrives.

Handle the new result `DispatchClaimValidationFinished(issue_id, generation, result)` in the daemon effect-result switch. The handler must first look up `pending_dispatch_validations[issue_id]`. If none exists or the generation does not match, log `dispatch_validation_stale` at `info` and ignore the result. For a matching result, delete the pending-validation entry before any branch that continues. On tracker error, missing issue, duplicate issue, or id mismatch, log `linear_dependency_claim_validation_failed` at `warn` with `issue_id`, `generation`, and a reason code, then call `dispatch_candidates(pending.remaining_candidates, state)`.

For a successful refreshed issue, the daemon validation handler must use the refreshed issue, not the stale candidate, for every following decision. First call `unpark_if_issue_changed_state` with the refreshed issue. Then call `core.blocker_decision`. If it returns blocked or incomplete, log `linear_dependency_claim_validation_blocked` at `warn` unless the report cache says this exact fingerprint and phase were already reported, mark the report cache, and continue with `pending.remaining_candidates` without claim. If blockers are satisfied, clear the blocked report for that issue and phase.

After dependency readiness, rerun dispatch preconditions on the refreshed issue through a new private daemon helper named `dispatch_validation_precondition_failure(state, issue)`. The helper should return `None` when the refreshed issue passes required fields, active state, non-terminal state, not-running, not-claimed, and parking checks; otherwise it should return `Some("missing_required_fields")`, `Some("inactive_state")`, `Some("terminal_state")`, `Some("already_running")`, `Some("already_claimed")`, or `Some("parked")`. If it returns a reason, log `dispatch_validation_precondition_failed` at `info` with `issue_id`, `generation`, and `reason`, then continue remaining candidates without claim. An unrecognized failure must be treated as non-claimable.

Still using the refreshed issue, classify workflow labels with `workflow_policy.classify_issue`. If workflow is invalid, call the existing invalid-workflow path only after dependency readiness, using the refreshed issue and `pending.remaining_candidates`. If the workflow label changes to a different valid workflow, select and run the refreshed workflow, not the stale candidate's workflow. Then call `can_reserve_dispatch_slot` again. If no global or per-state slot is available, log `dispatch_validation_slot_unavailable` at `info`, leave no pending validation or pending claim, and continue remaining candidates without claim. Finally call `can_route_issue_for_dispatch` and `workspace.workspace_path` using the refreshed issue. Only after all of these checks pass may the daemon reserve a worker-registry session sequence, build a run id, create `PendingClaim`, and enqueue `effect_runner.ClaimIssue`.

This is the daemon transition table that the implementation must preserve:

    Candidate passes initial dependency and workflow checks
      -> store PendingDispatchValidation and enqueue ValidateDispatchClaim
      -> do not reserve a slot, session sequence, workspace path, PendingClaim, or ClaimIssue

    Validation result has no matching pending entry or wrong generation
      -> log dispatch_validation_stale
      -> leave state unchanged

    Validation result is tracker error, missing, duplicate, or id mismatch
      -> delete PendingDispatchValidation
      -> log linear_dependency_claim_validation_failed
      -> dispatch_candidates(remaining_candidates, state)

    Validation result has blocked or incomplete dependencies
      -> delete PendingDispatchValidation
      -> log linear_dependency_claim_validation_blocked once per fingerprint
      -> mark blocked-dependency report cache
      -> dispatch_candidates(remaining_candidates, state)

    Validation result passes dependencies but fails another refreshed precondition
      -> delete PendingDispatchValidation
      -> optionally clear dependency report cache
      -> run invalid-workflow reporting only if the failure is workflow invalid
      -> otherwise log dispatch_validation_precondition_failed or dispatch_validation_slot_unavailable
      -> dispatch_candidates(remaining_candidates, state)

    Validation result passes all refreshed preconditions, workflow, route, and slot checks
      -> delete PendingDispatchValidation
      -> clear blocked-dependency report cache for the issue
      -> reserve session sequence
      -> build run id from refreshed issue
      -> create PendingClaim from refreshed issue
      -> enqueue ClaimIssue
      -> let handle_handoff_claim_finished continue remaining_candidates after claim result

In `src/scherzo/orchestrator/service.gleam`, mirror the daemon safety for the one-shot service path. Before `workflow_run.execute` or any handoff claim equivalent, refresh the selected issue by id using the tracker client. Normalize the response exactly as the daemon final validation does: one matching issue is success; tracker error, missing issue, duplicate issue, or id mismatch is a non-dispatching validation failure. Rerun core preconditions, blocker decision, workflow classification, workflow selection, and slot checks on the refreshed issue. Skip execution and continue the remaining candidates when blockers are not satisfied or any refreshed precondition fails.

For retry handling in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/core.gleam`, ensure the refresh result for a retry uses the same blocker policy. Add `core.stop_retry_for_dependency_blocked(state, issue_id)` returning a transition equivalent to `core.stop_retry_for_policy_invalid`, but with a distinct cancel reason. The transition must remove `issue_id` from `RuntimeState.retry_attempts`, remove `issue_id` from `RuntimeState.claimed`, emit `CancelRetry(issue_id, generation, "linear_dependency_blocked")`, and emit `ReleaseClaim(issue_id)`. The daemon already calls `retry_scheduler.finish_refresh` before handling the candidate; `CancelRetry` must still call `cancel_retry_timer` idempotently and append `record.RetryCancelled(issue_id, generation, "linear_dependency_blocked")`. No `ScheduleRetry`, `Dispatch`, `PendingClaim`, worker start, Linear comment, or Linear state update should happen in this path. After applying the transition, delete `state.recovery_by_issue[issue_id]` so a later normal poll does not inherit stale retry recovery context.

For retry errors that are tracker failures, keep the existing exponential retry behavior. For retry refreshes that return missing issue, inactive issue, terminal issue, invalid workflow, or dependency-blocked issue, stop the retry, release internal claim state, and rely on future normal polling only if the issue later appears as an eligible active candidate.

For parking in `src/scherzo/orchestrator/core.gleam`, keep explicit parks stronger than dependency readiness. An explicitly parked issue must not dispatch even if all blockers are terminal. For auto-parks that release on issue changes, include `blocked_by_complete` and blocker identifiers and states in `issue_fingerprint` so a blocker state change or completeness change can release the park and allow normal dispatch if every other precondition passes.

## Concrete Steps

1. From the repository root, inspect the working copy with `jj status --color=never` and confirm there are no unrelated changes to source or tests before implementing.

       jj status --color=never

   Expected result: either a clean working copy or only this plan file if implementation starts from the planning commit.

2. Before production code changes, create `test/fixtures/linear/blocked_issue_candidate_response.json` from a real-shape Linear GraphQL response. The fixture should contain candidate issue A whose `inverseRelations.nodes` contains an incoming `blocks` relation from blocker B with a non-terminal state. Redact ids and titles if needed, but keep field names and nesting exactly as Linear returns them.

3. Add a fixture-read test in `test/linear_test.gleam` that parses `test/fixtures/linear/blocked_issue_candidate_response.json`. Assert A has one blocker B, B's state is the fixture's non-terminal state, and the relation came from `inverseRelations` rather than outgoing `relations`. Run the single Linear test target if the file has a focused test command; otherwise run `direnv exec . gleam test` and expect this new test to fail until production decoding is updated.

4. If the fixture disproves the assumed `inverseRelations(first: 100) ... pageInfo` shape, update this ExecPlan's Decision Log and query examples before continuing. Do not update production decoders until the fixture and plan agree.

5. Update `src/scherzo/tracker/issue.gleam` to add `blocked_by_complete: Bool` to `Issue` after `blocked_by`. Update any constructor helper in the same file if one exists.

6. Run `direnv exec . gleam test` from the repository root. Expected result at this red step: compilation failures naming `Issue` constructors that now need the new `blocked_by_complete` field.

7. Update production constructors and test helpers to pass `blocked_by_complete: True` for normal fake or decoded complete issues. Pass `False` only in tests that model truncated or missing relation data.

8. Update `src/scherzo/linear.gleam` query strings for candidate and state refresh to request `inverseRelations(first: 100)` with relation `nodes` and `pageInfo`. Keep relation direction comments near the query or decoder so future maintainers do not confuse incoming and outgoing relations.

9. Update `src/scherzo/linear.gleam` decoders so dispatch issue payloads require `inverseRelations` and decode `blocked_by_complete`. Preserve existing behavior that only `type == "blocks"` contributes to `blocked_by`. Missing `pageInfo`, missing `inverseRelations`, or `pageInfo.hasNextPage == true` must produce incomplete data or a parser error, never a safe empty blocker list.

10. Extend `test/linear_test.gleam` with a test where outgoing `relations.nodes` contains `type == "blocks"` but `inverseRelations.nodes` has no blockers. Assert no blocker is created.

11. Extend `test/linear_test.gleam` with a payload where `inverseRelations.pageInfo.hasNextPage == true`. Assert the decoded issue has `blocked_by_complete == False` or the response is rejected with the chosen parser error.

12. Run `direnv exec . gleam test` from the repository root. Expected result after the Linear data work: all existing tests and the new Linear fixture tests pass.

13. In `src/scherzo/orchestrator/core.gleam`, add `pub type BlockerDecision` with constructors equivalent to `BlockersSatisfied` and `BlockedByDependency(open_blockers: List(tracker_issue.BlockerRef), incomplete: Bool)`.

14. In `src/scherzo/orchestrator/core.gleam`, add `pub fn blocker_decision(config, issue)` using normalized `config.tracker.terminal_states`. It must return blocked when `issue.blocked_by_complete == False`, when any blocker state is `None`, or when any blocker state is non-terminal.

15. In `src/scherzo/orchestrator/core.gleam`, update `dispatch_preconditions_satisfied_without_slot_capacity` and `retry_candidate_preconditions_satisfied_without_slot_capacity` to call `blocker_decision` and remove any condition that limits blocker enforcement to `Todo`.

16. Extend `test/orchestrator_core_test.gleam` with direct policy tests for active `Todo` issues. With terminal states configured as `Done` and `Canceled`, assert no blockers dispatches, `Todo` blocker blocks, `In Progress` blocker blocks, `Backlog` blocker blocks even if `Backlog` is not active, `Done` blocker satisfies, `Canceled` blocker satisfies, missing blocker state blocks, and incomplete relation data blocks.

17. Extend `test/orchestrator_core_test.gleam` with at least one candidate issue in active `In Progress` state and a non-terminal blocker. Assert `core.should_dispatch` is false to prove the policy is not Todo-only.

18. Update `src/scherzo/orchestrator/core.gleam` so `issue_fingerprint` includes `blocked_by_complete` as well as blocker ids, identifiers, and states.

19. Run `direnv exec . gleam test` from the repository root. Expected result: the core policy tests pass.

20. In `src/scherzo/orchestrator/state.gleam`, add `pub type BlockedDependencyReport` with fields `issue_id`, `identifier`, `phase`, `blocker_fingerprint`, `observed_updated_at`, `terminal_state_policy_fingerprint`, `attempted_at_ms`, and `last_result`.

21. In `src/scherzo/orchestrator/state.gleam`, add `blocked_dependency_reports: Dict(String, BlockedDependencyReport)` to `RuntimeState` and initialize it in `core.new_state`.

22. In `src/scherzo/orchestrator/core.gleam`, add `const blocked_dependency_report_cache_limit = 1024` and helpers named `blocked_dependency_fingerprint`, `already_reported_blocked_dependency`, `mark_blocked_dependency_reported`, and `clear_blocked_dependency_report`. The report key must include issue id and phase so candidate, claim-validation, retry, and service observations do not suppress each other accidentally.

23. Add core tests in `test/orchestrator_core_test.gleam` for report caching: first blocked observation is not already reported; marking it makes the identical observation reported; changing blocker state, completeness, phase, or terminal-state policy changes the fingerprint; clearing after dependency readiness removes the entry.

24. In `src/scherzo/orchestrator/effect_runner.gleam`, add `ValidateDispatchClaim(issue_id: String, generation: Int, client: tracker.Client)` to `Effect`.

25. In `src/scherzo/orchestrator/effect_runner.gleam`, add a validation error type or result representation that distinguishes tracker error, missing issue, duplicate issue, and id mismatch. Add `DispatchClaimValidationFinished(issue_id: String, generation: Int, result: Result(tracker_issue.Issue, DispatchClaimValidationError))` to `EffectResult`.

26. In `src/scherzo/orchestrator/effect_runner.gleam`, update `effect_kind`, `run_side_effect`, and `crash_result_for_effect` handling so the new effect refreshes exactly one issue by id and returns a non-claiming validation failure for tracker error, missing issue, duplicate issue, or id mismatch.

27. In `src/scherzo/orchestrator/daemon.gleam`, add the private `PendingDispatchValidation` type and the `pending_dispatch_validations` and `next_dispatch_validation_generation` fields to daemon `State`. Initialize both where `State` is created.

28. In `src/scherzo/orchestrator/daemon.gleam`, update helper checks such as `has_active_run` or `has_active_or_pending_issue` so an issue with pending validation is treated as pending for duplicate suppression. Do not add pending validations to `dispatch_slots_used`.

29. In `src/scherzo/orchestrator/daemon.gleam`, add `begin_dispatch_validation`. It stores the pending validation, increments the generation, enqueues `effect_runner.ValidateDispatchClaim`, logs `linear_dependency_claim_validation_started`, and returns without scanning `remaining_candidates`.

30. In `src/scherzo/orchestrator/daemon.gleam`, change `handle_valid_workflow_candidate` to call `begin_dispatch_validation` instead of `dispatch_issue_with_continuation` after initial dependency, workflow, and slot checks pass.

31. In `src/scherzo/orchestrator/daemon.gleam`, add `handle_dispatch_claim_validation_finished` and route `DispatchClaimValidationFinished` to it from the effect-result switch.

32. In `src/scherzo/orchestrator/daemon.gleam`, implement stale-result handling in `handle_dispatch_claim_validation_finished`: no pending entry or generation mismatch logs `dispatch_validation_stale` and does nothing else.

33. In `src/scherzo/orchestrator/daemon.gleam`, implement validation-error handling: delete the pending validation, log `linear_dependency_claim_validation_failed`, and call `dispatch_candidates(pending.remaining_candidates, state)`.

34. In `src/scherzo/orchestrator/daemon.gleam`, implement blocked-dependency handling: delete the pending validation, log `linear_dependency_claim_validation_blocked` once per report-cache fingerprint, mark the report cache, and continue remaining candidates without claim.

35. In `src/scherzo/orchestrator/daemon.gleam`, implement refreshed precondition handling after dependency readiness. Rerun dispatch preconditions, workflow classification, route selection, and slot checks on the refreshed issue. All failures must leave no pending validation and no pending claim.

36. In `src/scherzo/orchestrator/daemon.gleam`, implement the successful validation branch. Only here reserve the worker-registry session sequence, build the run id, compute the workspace path, create `PendingClaim` from the refreshed issue, and enqueue `effect_runner.ClaimIssue`.

37. Add daemon tests in `test/orchestrator_daemon_test.gleam` for the final-validation blocker race. Candidate fetch returns A with no blockers, final validation returns A blocked by B in `Todo`, and assertions prove no `ClaimIssue` effect, no `PendingClaim`, no reserved session sequence, no workspace run, and no worker start.

38. Add a daemon test where final validation returns only terminal blockers. Assert the normal claim path continues and the `PendingClaim` uses the refreshed issue data.

39. Add daemon stale-result tests. A validation result with no pending entry or an older generation must log `dispatch_validation_stale`, must not claim, and must not clear a newer pending validation.

40. Add daemon duplicate-suppression tests. While issue A has pending validation, a new poll containing A must not enqueue another `ValidateDispatchClaim` for A.

41. Add daemon refreshed-precondition tests. Cover these cases in `test/orchestrator_daemon_test.gleam`: refreshed issue is terminal or inactive means no claim; refreshed issue has missing required fields means no claim; refreshed issue is explicitly parked means no claim; refreshed issue has invalid or missing workflow label after dependencies are ready means no claim and the existing invalid-workflow path runs; refreshed issue changes to a different valid workflow label means route selection and claim use the refreshed workflow; refreshed slot capacity failure means no claim and no pending claim remains.

42. In `src/scherzo/orchestrator/service.gleam`, add a helper that refreshes a selected issue by id before one-shot execution and normalizes tracker error, missing issue, duplicate issue, and id mismatch as validation failures.

43. In `src/scherzo/orchestrator/service.gleam`, update the one-shot dispatch path so the refreshed issue, not the stale candidate, is used for blocker decision, core preconditions, workflow classification, workflow selection, and `workflow_run.execute`.

44. Add or extend service tests in `test/orchestrator_service_test.gleam` or the nearest existing service test file. A selected issue whose refresh returns a non-terminal blocker must produce `dispatched: 0`; a selected issue whose refresh returns terminal blockers must execute normally.

45. In `src/scherzo/orchestrator/core.gleam`, change `core.CancelRetry` to carry a retry-cancel generation and reason string. Preserve existing call sites by passing their current reason, such as `cancel_retry` or `reschedule_retry`, and by using generation `0` when no current retry entry exists.

46. In `src/scherzo/orchestrator/core.gleam`, add `stop_retry_for_dependency_blocked(state, issue_id)`. It must clear `RuntimeState.retry_attempts`, delete `RuntimeState.claimed[issue_id]`, emit `CancelRetry(issue_id, generation, "linear_dependency_blocked")`, and emit `ReleaseClaim(issue_id)`.

47. In `src/scherzo/orchestrator/daemon.gleam`, update `apply_effect` for `core.CancelRetry` so it appends `record.RetryCancelled(issue_id, generation, reason)` and cancels any timer idempotently.

48. In `src/scherzo/orchestrator/daemon.gleam`, update retry refresh handling. If `core.blocker_decision` on the refreshed retry issue is blocked or incomplete, log `linear_dependency_retry_blocked` once per fingerprint, mark the report cache, apply `stop_retry_for_dependency_blocked`, delete `state.recovery_by_issue[issue_id]`, and do not call `schedule_retry_with_backoff`.

49. Add retry tests. Start with a scheduled retry, `RuntimeState.claimed` containing the issue, and a retry refresh that returns a non-terminal blocker. Assert `retry_attempts` no longer contains the issue, `claimed` no longer contains the issue, the retry timer is canceled, a `RetryCancelled` ledger entry with reason `linear_dependency_blocked` is appended, no `ScheduleRetry` occurs, no `PendingClaim` exists, and no worker starts.

50. Add the passing retry test. A retry refresh with terminal blockers, valid workflow labels, and available slots should proceed through the normal retry dispatch path.

51. Add workflow-label interaction tests. Use an issue with a missing workflow label and a non-terminal blocker. Assert no `ReportInvalidWorkflow` effect and no Linear triage call while blocked. Then change the blocker to terminal and assert the existing invalid workflow report path runs for the missing label.

52. Add parking tests. Assert explicit park blocks dispatch even when blockers are terminal. Assert an auto-park fingerprint changes when blocker state or `blocked_by_complete` changes, and after all blockers are terminal the issue can dispatch if no other precondition blocks it.

53. Add blocked-dependency log tests in `test/orchestrator_daemon_test.gleam` or the nearest logger-capturing test file. Assert the first blocked observation logs once; repeated identical observation does not log again; blocker state change logs a new fingerprint; completeness change logs a new fingerprint; becoming dependency-ready clears the blocked report entry.

54. Run focused tests as they are added, then run the full suite from the repository root.

       direnv exec . gleam test

   Expected result after implementation: all tests pass, including the new Linear fixture tests, core policy tests, daemon validation tests, retry tests, workflow-label tests, service tests, reporting tests, and parking tests.

55. Run formatting from the repository root.

       direnv exec . gleam format --check src test

   Expected result: the formatter reports no changes needed.

56. Commit at logical boundaries. Suggested commits are: Linear blocker data contract and parser tests; core blocker policy and report cache; daemon final validation; service and retry safety; reporting and interaction tests. Only commit after `direnv exec . gleam test` passes for the current milestone.

## Testing and Falsifiability

The behavior is false if any test can observe a claim, handoff state change, pending claim, session sequence reservation, workspace creation, or worker start for an issue whose refreshed blocker data contains a non-terminal blocker, missing blocker state, or incomplete blocker page.

Parser tests in `test/linear_test.gleam` must include a real-shape fixture from `test/fixtures/linear/blocked_issue_candidate_response.json`. The fixture should exercise this shape: issue A is active, `inverseRelations.nodes` contains `type: "blocks"`, the related blocker issue B has state `Todo`, and relation `pageInfo.hasNextPage` is false. The test asserts A's `blocked_by` contains B and that `blocked_by_complete` is true. A separate payload with outgoing `relations.nodes` and no `inverseRelations` blockers asserts no blocker is created. A truncated relation payload with `pageInfo.hasNextPage` true asserts `blocked_by_complete == False` or a parser error, and downstream core policy must treat that issue as not dispatchable.

Core tests in `test/orchestrator_core_test.gleam` must directly call `core.should_dispatch`, `core.dispatch_preconditions_satisfied_without_slot_capacity`, `core.retry_candidate_preconditions_satisfied`, and `core.blocker_decision`. Concrete cases are: no blockers dispatches; a `Todo` blocker blocks; an `In Progress` blocker blocks; a `Backlog` blocker blocks even if `Backlog` is not an active state; a `Done` blocker satisfies when `Done` is configured terminal; a `Canceled` blocker satisfies when `Canceled` is configured terminal; a missing blocker state blocks; and incomplete relation data blocks. Repeat at least one non-terminal blocker case with the candidate issue itself in `In Progress` to prove the policy is not Todo-only.

Final-validation daemon tests in `test/orchestrator_daemon_test.gleam` must prove the entire refreshed-precondition matrix. In the dependency failing case, candidate fetch returns A with no blockers, but final validation returns A blocked by B in `Todo`. Expected assertions are no handoff claim effect, no pending claim, no session sequence reservation, no workspace run, and no worker start. In the dependency passing case, final validation returns A blocked only by B in `Done`, and the existing claim path continues with refreshed issue data.

The final-validation matrix must also include non-blocker stale-candidate cases. A refreshed issue that is terminal, inactive, missing required fields, already running, already claimed, explicitly parked, or over slot capacity must not claim and must leave no pending validation or pending claim. A refreshed issue with invalid or missing workflow labels must not claim and must run the existing invalid-workflow reporting only after dependency readiness. A refreshed issue whose workflow label changes must select the workflow from the refreshed issue, not the stale candidate.

Stale and duplicate validation tests must prove that generation checks work. A validation result for a missing pending entry or old generation logs `dispatch_validation_stale` and cannot clear a newer pending validation. A second poll while validation is pending cannot enqueue a duplicate `ValidateDispatchClaim` for the same issue.

Retry tests must prove a retry refresh cannot bypass the policy. A retry candidate with a non-terminal or incomplete blocker clears `RuntimeState.retry_attempts`, deletes `RuntimeState.claimed[issue_id]`, cancels the retry timer idempotently, appends `RetryCancelled` with reason `linear_dependency_blocked`, emits no `ScheduleRetry`, no `Dispatch`, no `PendingClaim`, no `ClaimIssue`, and no worker start. A retry candidate with terminal blockers can proceed when slots and workflow labels are valid.

Service tests must prove the one-shot path has the same safety as the daemon. A selected one-shot candidate whose refresh returns a non-terminal blocker must produce no `workflow_run.execute` call and `dispatched: 0`. A selected candidate whose refresh returns terminal blockers should execute normally.

Workflow-label tests must prove blocking runs first. A blocked issue with invalid labels should produce neither `ReportInvalidWorkflow` nor a Linear triage call. After the same issue becomes unblocked, invalid workflow reporting should behave exactly as it did before this feature.

Parking tests must prove explicit parks still block and auto-park release uses blocker changes in the issue fingerprint. This falsifies the claim that blocker changes are observable to the parking policy.

Reporting tests must prove log de-duplication and new-signal behavior. The first blocked observation logs exactly one warning with event `linear_dependency_blocked_candidate`, `linear_dependency_claim_validation_blocked`, or `linear_dependency_retry_blocked`. Repeating the same issue, phase, blocker fingerprint, observed `updated_at`, and terminal-state policy logs nothing new. Changing blocker state, changing `blocked_by_complete`, changing terminal-state policy, or becoming dependency-ready clears or changes the cache so the next meaningful observation can log.

Run the full validation command from the repository root:

    direnv exec . gleam test

The expected successful output is Gleam's normal all-tests-passed result. Any failure in the new tests means the plan's central safety claim is not met.

## Validation and Acceptance

Acceptance is behavioral. Scherzo must not claim or start work on an issue while any observed direct Linear blocker is non-terminal, missing a state, or hidden behind truncated relation pagination.

A reviewer can validate the implementation by running `direnv exec . gleam test` from the repository root and checking that these named scenarios pass: Linear parser maps incoming `inverseRelations` `blocks` relations to blockers; outgoing `relations` are ignored; truncated relation pages are incomplete and not dispatchable; core dispatch blocks non-terminal blockers across all active issue states; final daemon validation prevents `ClaimIssue` when a blocker appears after candidate fetch; final daemon validation reruns active, terminal, required-field, parking, workflow, route, and slot checks on refreshed issue data; retry refresh cannot claim a blocked issue; one-shot service dispatch cannot execute a blocked issue; workflow-label triage waits until blockers are terminal; explicit parking remains stronger than dependency readiness; duplicate blocked logs are suppressed while meaningful blocker changes log again.

The plan is accepted only if the tests prove that the following sequence never claims: candidate A is fetched, final validation refresh for A returns blocker B in a non-terminal state, and Scherzo produces no handoff claim, no claim state mutation, no claim comment, no pending claim, no session sequence reservation, no workspace run, and no worker start for A.

The plan is also accepted only if incomplete relation data is operator-visible. A candidate or validation response with `inverseRelations.pageInfo.hasNextPage == true` must skip the issue and log a warning containing `incomplete=true`, the event phase, issue id, issue identifier, and blocker fingerprint. No Linear comment or state change may be created for this condition.

The plan is also accepted only if blocked issues remain in their current Linear state and this feature posts no Linear comments for blocked dependencies. Structured logs may mention blocked candidates, claim-validation blocks, incomplete pages, and retry blocks, but repeated logs for the same issue, phase, blocker fingerprint, observed `updated_at`, and terminal-state policy must be suppressed by the in-memory cache.

Expected log content for the main observable cases is:

    level=warn event=linear_dependency_blocked_candidate issue_id=A-id issue_identifier=A-1 phase=candidate blockers=B-1:Todo incomplete=false
    level=warn event=linear_dependency_claim_validation_blocked issue_id=A-id issue_identifier=A-1 phase=claim_validation blockers=B-1:Todo incomplete=false
    level=warn event=linear_dependency_retry_blocked issue_id=A-id issue_identifier=A-1 phase=retry blockers=B-1:Todo incomplete=false
    level=warn event=linear_dependency_blocked_candidate issue_id=A-id issue_identifier=A-1 phase=candidate blockers=B-1:Done incomplete=true

Exact logger formatting may differ, but tests must assert event names and structured fields rather than relying only on a rendered string.

## Rollout, Recovery, and Idempotence

This change is additive and safe to roll out with normal deployment. It changes dispatch decisions but does not migrate stored data and does not write new Linear comments or states for blocked issues. If the Linear relation query fails or returns an unexpected shape, Scherzo should fail closed for that tick by skipping the affected candidate or failing candidate fetch, rather than claiming uncertain work.

Because this plan accepts the `first: 100` relation cap, the main rollout risk is over-blocking an issue with more than 100 incoming relations. Operators diagnose this through `incomplete=true` warnings. Recovery options are to complete or remove enough dependencies so Linear no longer truncates the incoming relation page, adjust configured terminal states when the blocker state names are wrong, or revert the implementation commit. There is no Linear issue-history cleanup because the feature writes no dependency-block comments or state changes.

Repeated polls are idempotent. Seeing the same blocked issue with the same phase and blocker fingerprint should not create new Linear side effects or repeated warnings. When blocker states change, relation completeness changes, or terminal-state configuration changes, the fingerprint changes; Scherzo can log a new observation, clear the blocked report once satisfied, and proceed through the normal dispatch path.

Retry recovery is idempotent. If a retry refresh becomes dependency-blocked, Scherzo clears the runtime retry attempt, cancels the timer idempotently, releases the internal claimed map entry, appends a retry-cancel ledger record with reason `linear_dependency_blocked`, and does not schedule another retry for the dependency condition. A later normal candidate poll after blockers are terminal can dispatch the issue if every other precondition passes.

Rollback is straightforward. Reverting the implementation restores the prior dispatch behavior. Because this plan does not add stored schema migrations or Linear comments, rollback does not require data cleanup outside the normal working tree and ledger files already used by Scherzo.

## Artifacts and Notes

The important current query shape is in `src/scherzo/linear.gleam`: candidate and state refresh GraphQL requests already include issue id, identifier, title, state, labels, and `inverseRelations`. The implementation should preserve the single candidate-fetch call and enrich it with explicit relation pagination metadata under the accepted `first: 100` cap.

An example structured log for a candidate skipped because of a non-terminal blocker should contain:

    event=linear_dependency_blocked_candidate issue_id=A-id issue_identifier=A-1 phase=candidate blockers=B-1:Todo incomplete=false

An example final-validation skip should be distinguishable from normal candidate filtering:

    event=linear_dependency_claim_validation_blocked issue_id=A-id issue_identifier=A-1 phase=claim_validation blockers=B-1:Todo incomplete=false

An example incomplete-page skip should make the accepted cap diagnosable:

    event=linear_dependency_blocked_candidate issue_id=A-id issue_identifier=A-1 phase=candidate blockers=B-1:Done incomplete=true

The real-shape fixture under `test/fixtures/linear/blocked_issue_candidate_response.json` should be short and anonymized. It must preserve the real GraphQL field names and nesting. Do not include local filesystem paths or private tokens in the fixture.

## Interfaces and Dependencies

No new package dependency is required. Use existing Gleam modules, existing JSON decode helpers in `src/scherzo/linear.gleam`, existing issue-state normalization in `src/scherzo/tracker/state.gleam`, existing orchestrator effect patterns, existing retry scheduler, and existing ledger record machinery.

In `src/scherzo/tracker/issue.gleam`, the issue type should end with a blocker-completeness field equivalent to:

    pub type Issue {
      Issue(
        id: String,
        identifier: String,
        title: String,
        description: Option(String),
        priority: Option(Int),
        state: issue_state.IssueState,
        branch_name: Option(String),
        url: Option(String),
        labels: List(String),
        blocked_by: List(BlockerRef),
        blocked_by_complete: Bool,
        created_at: Option(Time),
        updated_at: Option(Time),
      )
    }

In `src/scherzo/orchestrator/core.gleam`, expose a blocker decision equivalent to:

    pub type BlockerDecision {
      BlockersSatisfied
      BlockedByDependency(open_blockers: List(tracker_issue.BlockerRef), incomplete: Bool)
    }

    pub fn blocker_decision(
      config: config_types.EffectiveConfig,
      issue: tracker_issue.Issue,
    ) -> BlockerDecision

The exact constructor names may differ, but callers must be able to distinguish satisfied, non-terminal blockers, missing blocker state, and incomplete data for logging and tests.

In `src/scherzo/orchestrator/state.gleam`, add blocked dependency report state equivalent to:

    pub type BlockedDependencyReport {
      BlockedDependencyReport(
        issue_id: String,
        identifier: String,
        phase: String,
        blocker_fingerprint: String,
        observed_updated_at: Option(Time),
        terminal_state_policy_fingerprint: String,
        attempted_at_ms: Int,
        last_result: String,
      )
    }

    pub type RuntimeState {
      RuntimeState(
        ...,
        invalid_workflow_reports: Dict(String, InvalidWorkflowReport),
        blocked_dependency_reports: Dict(String, BlockedDependencyReport),
        completed: Dict(String, tracker_issue.Issue),
        ...,
      )
    }

In `src/scherzo/orchestrator/effect_runner.gleam`, add final validation effect and result shapes equivalent to:

    pub type Effect {
      ...
      ValidateDispatchClaim(issue_id: String, generation: Int, client: tracker.Client)
      ...
    }

    pub type DispatchClaimValidationError {
      DispatchValidationTrackerError(error.TrackerError)
      DispatchValidationMissingIssue
      DispatchValidationDuplicateIssue
      DispatchValidationIdMismatch(expected: String, actual: String)
    }

    pub type EffectResult {
      ...
      DispatchClaimValidationFinished(
        issue_id: String,
        generation: Int,
        result: Result(tracker_issue.Issue, DispatchClaimValidationError),
      )
      ...
    }

If the implementation uses a local validation error type in `src/scherzo/orchestrator/daemon.gleam` instead of exporting one from the effect runner, it must still preserve the semantics that errors, missing issues, duplicate issues, and id mismatches do not claim and are separately testable.

In `src/scherzo/orchestrator/daemon.gleam`, add pending validation state equivalent to:

    type PendingDispatchValidation {
      PendingDispatchValidation(
        issue: tracker_issue.Issue,
        remaining_candidates: List(tracker_issue.Issue),
        generation: Int,
        requested_at_ms: Int,
      )
    }

    type State {
      State(
        ...,
        pending_claims: Dict(String, PendingClaim),
        pending_dispatch_validations: Dict(String, PendingDispatchValidation),
        next_dispatch_validation_generation: Int,
        ...,
      )
    }

Pending validation is keyed by issue id. A validation result is fresh only when the dict contains the issue id and the stored generation equals the result generation. `PendingClaim`, workspace path creation, run id creation, and session sequence reservation must remain downstream of fresh validation success.

In `src/scherzo/orchestrator/core.gleam`, make retry cancellation reason visible to the ledger with an effect equivalent to:

    pub type Effect {
      ...
      CancelRetry(issue_id: String, generation: Int, reason: String)
      ...
    }

    pub fn stop_retry_for_dependency_blocked(
      state: orchestrator_state.RuntimeState,
      issue_id: String,
    ) -> Transition

`stop_retry_for_dependency_blocked` must use reason `linear_dependency_blocked`, clear runtime retry and claimed state, and emit `ReleaseClaim(issue_id)`.

## Open Questions and Clarifications Needed

- [CLARIFY] Linear may not provide an atomic mutation that says "claim this issue only if all blockers are terminal." This plan validates immediately before the handoff claim and fails closed on any observed uncertainty, but a dependency added between the validation response and the claim mutation may still be possible unless Linear supports a conditional update or transaction.
