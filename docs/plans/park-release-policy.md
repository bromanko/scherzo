# Add explicit and fingerprint-based park release policies

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a Scherzo park has an explicit release policy. Operator safety parks, such as `scherzoctl park`, `/scherzo park`, `scherzoctl abort`, and `/scherzo abort`, remain parked until an operator explicitly un-parks or retries the issue. System parks, such as hitting retry or session caps, can still auto-release, but only when a core Linear issue field changes. Linear comments and Scherzo acknowledgement comments will no longer accidentally unpark an issue merely because Linear advanced `updatedAt`.

A human can see the fix working by aborting a running issue through a Linear command comment, watching Scherzo post the acknowledgement comment, and observing that the issue does not redispatch on the next poll. A human can also edit the issue title or description after a system cap park and observe that the auto park clears because the work input truly changed.

## Problem Framing and Constraints

Scherzo currently stores parked issues in `domain.ParkedEntry` with `observed_updated_at: Option(Time)`. Dispatch checks use `is_parked_without_update` in `src/scherzo/orchestrator/core.gleam`: if the candidate issue has the same `updated_at` value as the parked entry, dispatch is blocked; if `updated_at` differs, the park is treated as stale. `core.unpark_if_updated` clears a park during retry handling when `updated_at` changes.

Manual Linear command validation exposed that Linear comments also advance issue `updatedAt`. The sequence was: Scherzo dispatched test issue `LIV-10`, a real `/scherzo prompt` comment was queued and acknowledged, an edited command comment was ignored, a new `/scherzo abort` comment was applied, and then Scherzo posted an abort acknowledgement. Because the issue's `updatedAt` changed due to comments and acknowledgements, the abort-created park was considered stale and the same issue redispatched. That is unsafe: an explicit operator abort or park must not be undone by a comment side effect.

The constraint is that Scherzo has no durable scheduler state in this phase. Park state is in memory, so there is no stored-data migration. The solution must be deterministic, covered by unit and daemon tests, and must not require new package dependencies. It should preserve the useful behavior where system parks can be cleared by core issue changes, but it must stop using Linear's broad `updatedAt` timestamp as the sole signal.

## Strategy Overview

Introduce a park release policy in `src/scherzo/domain.gleam`. A `ParkedEntry` will carry either `ExplicitUnparkOnly` or `AutoUnparkOnIssueChange(fingerprint: String)`. `ExplicitUnparkOnly` means the park blocks dispatch until an operator issues `unpark` or `retry`. `AutoUnparkOnIssueChange` means the park blocks dispatch while the current issue fingerprint equals the stored fingerprint, and clears when the fingerprint changes.

Add a deterministic issue fingerprint function in `src/scherzo/orchestrator/core.gleam`. The fingerprint represents only core issue fields: issue id, identifier, title, description, priority, state, and branch name. It deliberately does not include `created_at`, `updated_at`, comments, Scherzo acknowledgement comments, Linear comment metadata, url, labels, or blocker relations. This makes auto-release less noisy: a title, description, priority, state, or branch-name change can release a system park, while url-only, label-only, blocker-only, timestamp-only, and comment-only changes do not.

Use `ExplicitUnparkOnly` for operator-controlled parks in `src/scherzo/orchestrator/daemon.gleam`, including local and Linear park/abort paths that call `park_issue_state`. Use `AutoUnparkOnIssueChange(core.issue_fingerprint(issue))` for system cap parks in `src/scherzo/orchestrator/core.gleam`, including `max_retry_attempts` and `max_sessions_per_issue`. Replace `unpark_if_updated` with `unpark_if_issue_changed` and call it wherever candidates or retry candidates are evaluated so stale auto parks are removed before dispatch. Keep `retry` as an explicit operator action that clears a park, matching current behavior.

This approach is proportionate because it changes only the in-memory park model and the dispatch checks. It does not require Linear labels, state mutations, durable receipts, webhooks, or a new datastore.

## Alternatives Considered

One alternative is to make every park sticky and require explicit unpark for all parked issues. That is the safest and simplest rule, but it removes the existing convenience where a system-created cap park can clear after a human meaningfully edits the issue. Because Scherzo now has explicit command comments, all-sticky would be acceptable, but it is stricter than necessary.

Another alternative is to keep a single park type and replace `updatedAt` with a fingerprint. That fixes comment-triggered auto-unparks but still lets a title or description edit override an explicit `/scherzo park` or `/scherzo abort`. Explicit operator intent should not be released implicitly.

A third alternative is to add Linear state or label mutations for parked issues. That would make parks visible and durable in Linear, but it requires workflow-specific state ids or label configuration, failure handling, and a migration story. It is a good future enhancement, not the minimal in-memory correctness fix.

A fourth alternative is to suppress redispatch only for recent command comments or acknowledgement comments. That is a timing heuristic and is hard to explain or test. It would still be vulnerable to other comments and delayed poll ordering.

## Risks and Countermeasures

The main safety risk is accidentally allowing an operator-aborted issue to run again. Countermeasure: operator parks use `ExplicitUnparkOnly`, and tests must prove they remain parked across `updated_at`, title, and description changes until explicit unpark or retry.

The main regression risk is losing auto-unpark behavior for system parks. Countermeasure: system cap parks use `AutoUnparkOnIssueChange`, and tests must prove a core issue content change clears the park while comment-only and non-core-only changes do not.

The main correctness risk is a fingerprint that is either unstable or too broad. Countermeasure: implement a deterministic, length-prefixed string encoding in `core.gleam` for only the core issue fields. Add tests that changing `created_at`, `updated_at`, url, labels, blocker order, or blocker state does not change the fingerprint, while changing id, identifier, title, description, priority, state, or branch name does.

The main implementation risk is creating an auto park without a real baseline issue and storing an empty or placeholder fingerprint. That would make the next valid candidate look changed and could immediately release a cap park. Countermeasure: change `core.apply_worker_failure` to accept a concrete `domain.Issue` baseline, prefer `runner.WorkerFailure.final_issue` when the runner supplies one, change the private core park helper to require that issue, and do not use an empty-fingerprint fallback.

The main integration risk is leaving stale auto park entries in `runtime.parked` after candidate dispatch decides the fingerprint changed. Countermeasure: add a state-transforming helper such as `core.unpark_if_issue_changed` and call it before candidate dispatch checks and in retry candidate handling. Tests should assert the parked map and issue counters are cleared, not just that `should_dispatch` returns true.

The main daemon cleanup risk is clearing `runtime.retry_attempts` during candidate dispatch without also canceling the daemon-owned retry timer handle in `State.retry_timers`. Countermeasure: add a daemon helper that installs the runtime returned by `core.unpark_if_issue_changed` and calls `cancel_retry_timer(issue.id)` when a retry entry existed before the unpark and is absent afterward. Add a test that the snapshot has no retry attempt for the issue and that a later retry tick is treated as stale without causing redispatch.

The main documentation risk is stale operational guidance. Countermeasure: update `README.md` statements that currently say parking clears when Linear reports a newer `updated_at` value, and document the new sticky-vs-auto behavior.

## Progress

- [x] (2026-04-29 03:20Z) Created this plan after real Linear validation showed `/scherzo abort` acknowledgement comments can advance Linear `updatedAt` and cause accidental redispatch under the current park model.
- [x] (2026-04-29 03:21Z) Reviewed the plan against the current tree, confirmed `direnv exec . gleam test` passes with 200 tests, and tightened the plan around baseline issue selection, daemon retry timer cleanup, one-shot service call sites, and obsolete timestamp fields.
- [x] (2026-04-29 03:22Z) Incorporated stakeholder decisions to remove `IssueCounter.observed_updated_at`, keep retry as an explicit release path, and limit auto-release fingerprints to core issue fields only.
- [x] (2026-04-29 05:38Z) Baseline validation before implementation passed with `direnv exec . gleam test`: 215 passed, no failures.
- [x] (2026-04-29 05:48Z) Added `ParkReleasePolicy`, replaced timestamp release state, removed `IssueCounter.observed_updated_at`, and added deterministic core issue fingerprints.
- [x] (2026-04-29 05:48Z) Updated pure scheduler semantics so explicit parks stay blocked, auto parks clear only on core issue fingerprint changes, and system cap parks store concrete issue fingerprints.
- [x] (2026-04-29 05:48Z) Updated daemon and one-shot integration so operator park/abort paths create explicit parks, stale auto parks are cleared before candidate dispatch, and worker failure callers pass the latest issue baseline.
- [x] (2026-04-29 05:48Z) Added daemon regression coverage for Linear `/scherzo abort` followed by an acknowledgement/timestamp-only candidate poll not redispatching the same issue.
- [x] (2026-04-29 05:48Z) Updated README parking guidance and validated with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`: 222 passed, no failures.

## Surprises & Discoveries

- Observation: Linear comments and Scherzo acknowledgement comments update the issue's `updatedAt` value.
  Evidence: Manual validation on `LIV-10` showed `/scherzo abort` was applied and acknowledged, then the issue redispatched because the existing park logic treated the comment-driven `updatedAt` change as an unpark signal.

- Observation: Candidate dispatch currently treats a stale park as non-blocking through `core.should_dispatch`, but it does not necessarily remove the stale park entry from `runtime.parked` in the candidate path.
  Evidence: `src/scherzo/orchestrator/core.gleam` uses `is_parked_without_update` inside `should_dispatch`; `core.unpark_if_updated` is called in `handle_retry_candidate`, not in `daemon.dispatch_candidates`.

- Observation: Operator retry already acts as an explicit release path for parks.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` function `retry_resolved_issue` deletes `state.runtime.parked`, `state.runtime.retry_attempts`, and `state.runtime.issue_counters` for the issue before trying to dispatch.

- Observation: The one-shot orchestrator path still calls the pure scheduler APIs that this plan changes.
  Evidence: `src/scherzo/orchestrator/service.gleam` calls `core.should_dispatch` in its private `dispatch_candidates` helper and calls `core.apply_worker_failure` in the failed worker branch.

- Observation: `domain.IssueCounter.observed_updated_at` is currently only initialized, assigned, and tested; no scheduler decision reads it.
  Evidence: Grepping `observed_updated_at` shows reads in `ParkedEntry` logic and `test/domain_test.gleam`, while `IssueCounter.observed_updated_at` is only set in `domain.new_issue_counter` and `core.continue_or_park`.

- Observation: Daemon retry timers are stored outside `domain.RuntimeState`.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` stores timer handles in `State.retry_timers`; `core.unpark_if_issue_changed` can delete `RuntimeState.retry_attempts`, but only daemon code can cancel and remove the matching timer handle.

## Decision Log

- Decision: Use two release policies: `ExplicitUnparkOnly` and `AutoUnparkOnIssueChange(fingerprint)`.
  Rationale: Explicit operator intent and automatic recovery from system cap parks are different semantics. Keeping them separate prevents implicit Linear edits from overriding human safety commands while preserving useful auto-release behavior where intended.
  Date: 2026-04-29

- Decision: Do not include `updated_at`, `created_at`, or comments in the issue fingerprint.
  Rationale: Linear comments and acknowledgement comments can advance timestamps without changing the work input. The fingerprint should model core issue content, not audit metadata.
  Date: 2026-04-29

- Decision: Limit `issue_fingerprint` to core issue fields: id, identifier, title, description, priority, state, and branch name.
  Rationale: The fingerprint should be conservative enough to avoid noisy auto-release from metadata-only changes. Url, labels, and blocker relations are useful context, but stakeholder direction is to keep auto-release focused on the core work input; operators can still use explicit unpark or retry when a label, blocker, or url-only change should trigger new work.
  Date: 2026-04-29

- Decision: Keep `retry` as an explicit park release path.
  Rationale: Operators already use `retry` to say "try this issue now." It is reasonable for retry to clear either explicit or auto parks, subject to existing active/paused/dispatchability checks.
  Date: 2026-04-29

- Decision: Require a concrete issue baseline for every auto park.
  Rationale: An empty or placeholder fingerprint is unsafe because it makes the next valid candidate appear meaningfully changed. Failure paths already have an issue available from the worker handle or `runner.WorkerFailure.final_issue`, so the pure API should make that requirement explicit instead of hiding a fallback.
  Date: 2026-04-29

- Decision: Remove the obsolete `IssueCounter.observed_updated_at` field as part of this change.
  Rationale: After park release uses explicit policies and issue fingerprints, this timestamp field has no scheduling reader and would preserve the misleading idea that broad Linear `updatedAt` is still a release signal.
  Date: 2026-04-29

## Outcomes & Retrospective

Completed on 2026-04-29. The implementation adds explicit and auto park release policies, uses deterministic fingerprints for system cap auto-release, removes the obsolete broad `updatedAt` release state, and keeps operator park/abort semantics sticky until explicit `unpark` or `retry`. Pure tests now prove fingerprints ignore timestamps, url, labels, and blocker relations while changing for id, identifier, title, description, priority, state, and branch name. Daemon tests cover explicit operator parks, stale auto park clearing before candidate dispatch, and a Linear `/scherzo abort` acknowledgement/timestamp-only candidate poll that does not redispatch the same issue. README now documents the sticky-vs-auto behavior. Final deterministic validation passed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`: 222 passed, no failures. Credential-gated manual Linear validation was not run in this implementation session.

## Context and Orientation

Scherzo is a Gleam Erlang-target project. Runtime source lives under `src/scherzo/`, tests live under `test/`, and validation is run from the repository root with `direnv exec . gleam test`. The daemon actor in `src/scherzo/orchestrator/daemon.gleam` owns runtime state and calls pure scheduler functions from `src/scherzo/orchestrator/core.gleam`.

A park is an in-memory decision to suppress work on an issue. Parked issues are stored in `domain.RuntimeState.parked`, a `Dict(String, ParkedEntry)` keyed by Linear issue id. Local `scherzoctl` commands and Linear `/scherzo` command comments both map into `control/command.OperatorCommand` and are applied by the daemon's shared command helper. The daemon currently uses a private `park_issue_state` helper for operator park and abort behavior.

The existing `ParkedEntry` in `src/scherzo/domain.gleam` contains `issue_id`, `identifier`, `reason`, `observed_updated_at`, and `parked_at_ms`. The field `observed_updated_at` is the value this plan replaces with a release policy. `domain.IssueCounter` also contains an `observed_updated_at` field, but current scheduler code does not read it; this plan removes it so broad Linear timestamps are no longer represented as park release state.

## Preconditions and Verified Facts

Before implementation, `direnv exec . gleam test` passed with 215 tests. After implementation, `direnv exec . gleam format --check src test` succeeds and `direnv exec . gleam test` passes with 222 tests.

Verified current source facts after implementation:

- `src/scherzo/domain.gleam` defines `ParkReleasePolicy` with `ExplicitUnparkOnly` and `AutoUnparkOnIssueChange(issue_fingerprint: String)`, and `ParkedEntry(issue_id, identifier, reason, release_policy, parked_at_ms)`.
- `src/scherzo/domain.gleam` defines `IssueCounter(failure_attempts, worker_sessions)`; `IssueCounter.observed_updated_at` no longer exists.
- `src/scherzo/orchestrator/core.gleam` defines `issue_fingerprint`, policy-aware dispatch blocking, `unpark_if_issue_changed`, and a private system `park` helper that requires a concrete `domain.Issue` baseline.
- `core.apply_worker_failure` now accepts `baseline_issue: domain.Issue`; daemon and one-shot service callers pass `runner.WorkerFailure.final_issue` when present and otherwise pass the original worker or candidate issue.
- `core.continue_or_park` creates `AutoUnparkOnIssueChange(core.issue_fingerprint(issue))` parks for `max_sessions_per_issue`; `core.apply_worker_failure` creates the same policy for `max_retry_attempts`.
- `src/scherzo/orchestrator/daemon.gleam` creates `ExplicitUnparkOnly` parks in `park_issue_state`, which covers local operator park/abort paths and Linear command park/abort paths.
- `daemon.dispatch_candidates` installs `core.unpark_if_issue_changed` results before dispatch checks and cancels the daemon retry timer if the runtime retry entry disappeared.
- `daemon.retry_resolved_issue` continues to explicitly clear `runtime.parked`, `runtime.retry_attempts`, and `runtime.issue_counters` for an operator retry.
- `src/scherzo/orchestrator/service.gleam` runs `core.unpark_if_issue_changed` before one-shot candidate dispatch checks and uses the updated `core.apply_worker_failure` signature.
- `test/domain_test.gleam`, `test/orchestrator_core_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, and `test/orchestrator_daemon_linear_command_test.gleam` cover the new release-policy model and regression behavior.

## Scope Boundaries

In scope: in-memory park release policy types; deterministic issue fingerprinting; removal of obsolete timestamp release state from `ParkedEntry` and `IssueCounter`; scheduler behavior for explicit and auto parks; daemon operator park/abort policy selection; daemon retry-timer cleanup when stale auto parks clear; one-shot service compatibility with changed pure APIs; candidate and retry candidate stale-auto-park clearing; tests for comment-only timestamp changes, core issue changes, explicit unpark, operator retry, and Linear command abort regression; README documentation.

Out of scope: durable park storage; Linear state transitions or labels for parked issues; webhook processing; broad issue history scans; changing retry backoff policy; changing command syntax; changing how Scherzo posts Linear acknowledgements; distributed coordination across multiple daemons.

## Milestones

Milestone 1 introduces the model and pure fingerprint helper. At the end, `domain.ParkedEntry` can represent explicit and auto release policies, obsolete timestamp release state is gone from `IssueCounter`, and pure tests can prove the fingerprint ignores metadata and relation noise but changes for core issue fields.

Milestone 2 updates pure scheduler semantics. At the end, system cap parks are auto parks keyed by a concrete latest-known issue fingerprint, comment-only and metadata-only changes do not clear them, core issue changes do clear them, and explicit parks block dispatch regardless of timestamp or issue content changes.

Milestone 3 updates daemon and one-shot service integration. At the end, `scherzoctl park`, `/scherzo park`, `scherzoctl abort`, and `/scherzo abort` create explicit parks, operator retry remains an explicit release path, daemon candidate dispatch clears stale auto parks before evaluating candidates without leaking retry timer handles, and the one-shot service path compiles against the same pure scheduler semantics.

Milestone 4 adds the Linear regression and documentation. At the end, deterministic daemon tests show a Linear abort acknowledgement cannot cause same-issue redispatch, README documents the new release behavior, and the full suite passes.

## Plan of Work

In `src/scherzo/domain.gleam`, add a public type near `ParkedEntry`:

    pub type ParkReleasePolicy {
      ExplicitUnparkOnly
      AutoUnparkOnIssueChange(issue_fingerprint: String)
    }

Change `ParkedEntry` to replace `observed_updated_at: Option(Time)` with `release_policy: ParkReleasePolicy`. Remove `observed_updated_at` from `IssueCounter`, remove the assignment in `core.continue_or_park`, and update `domain.new_issue_counter` and `test/domain_test.gleam` accordingly. Because runtime state is not durable, no migration code is needed. Every direct constructor in tests and source must be updated.

In `src/scherzo/orchestrator/core.gleam`, add `pub fn issue_fingerprint(issue: domain.Issue) -> String`. It must be deterministic and must not depend on `updated_at`, `created_at`, url, labels, or blocker relations. Use no new dependencies. A simple length-prefixed encoding is sufficient: encode each string field as `int.to_string(string.length(value)) <> ":" <> value`, encode `None` as `"none"`, and encode `Some(value)` as `"some:" <> encoded_value`. Include only `issue.id`, `issue.identifier`, `issue.title`, `issue.description`, `issue.priority`, `issue.state`, and `issue.branch_name`.

Replace private `is_parked_without_update` with a helper that understands release policy, for example `park_blocks_dispatch(parked: domain.ParkedEntry, issue: domain.Issue) -> Bool`. For `ExplicitUnparkOnly`, it returns `True`. For `AutoUnparkOnIssueChange(stored)`, it returns `stored == issue_fingerprint(issue)`.

Replace public `unpark_if_updated` with `pub fn unpark_if_issue_changed(state: domain.RuntimeState, issue: domain.Issue) -> domain.RuntimeState`. If there is no park, return the state. If the park is `ExplicitUnparkOnly`, return the state. If it is `AutoUnparkOnIssueChange(stored)` and `stored == issue_fingerprint(issue)`, return the state. If it is `AutoUnparkOnIssueChange(stored)` and the fingerprint differs, delete the issue from `parked`, `claimed`, `retry_attempts`, and `issue_counters`. Because retry timer handles live in the daemon rather than in `RuntimeState`, daemon call sites that install this changed runtime must also cancel the corresponding retry timer when a retry entry was removed.

Update private core system park creation so auto parks always have a real baseline issue. Change `core.apply_worker_failure` to accept the latest known `domain.Issue` instead of only an issue id, and update `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/service.gleam`, and tests to pass `failure.final_issue` when present or the original issue otherwise. Change the private core `park` helper to accept `baseline_issue: domain.Issue` and store `domain.AutoUnparkOnIssueChange(issue_fingerprint(baseline_issue))`. Do not use `AutoUnparkOnIssueChange("")` or any other placeholder fallback; an auto park without a baseline issue is unsafe because it will clear as soon as a real issue is observed. `continue_or_park` already has the current active issue and should pass it directly.

Update `should_dispatch` and `should_dispatch_retry_candidate` to call the new policy-aware blocking helper. These functions may still return `True` for a stale auto park, but the daemon should clear stale auto parks before dispatch so runtime state remains coherent.

In `src/scherzo/orchestrator/daemon.gleam`, change `park_issue_state` to create `ParkedEntry(..., release_policy: domain.ExplicitUnparkOnly, ...)`. This covers explicit operator park and abort paths, including Linear commands because they use the shared daemon command helper.

Also in `daemon.gleam`, call `core.unpark_if_issue_changed` before candidate dispatch checks through a small daemon helper, for example `unpark_if_issue_changed_state(state: State, issue: domain.Issue) -> State`. The helper should remember whether `state.runtime.retry_attempts` contained the issue before the core call, install the returned runtime, and call `cancel_retry_timer(issue.id)` if the retry entry disappeared. Use this helper inside `dispatch_candidates` before evaluating `core.should_dispatch` and `can_reserve_dispatch_slot`. This ensures stale auto parks are removed from runtime before dispatch without leaking daemon timer handles. Keep explicit parks untouched. `core.handle_retry_candidate` should call the same pure helper internally; by the time retry candidate handling runs, the retry timer that triggered the refresh has already been removed from `State.retry_timers`.

In `src/scherzo/orchestrator/service.gleam`, update the one-shot `dispatch_candidates` helper to run `core.unpark_if_issue_changed(state, issue)` before `core.should_dispatch`. There is no daemon timer map in this path, so installing the returned `RuntimeState` is enough. Also update the failed worker branch to pass the latest known issue into `core.apply_worker_failure`, using `failure.final_issue` when present and the original candidate issue otherwise.

Update tests that construct `ParkedEntry` directly. Use `release_policy: domain.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))` for old auto-park tests and `release_policy: domain.ExplicitUnparkOnly` for operator-style park tests. Update `test/domain_test.gleam` to assert `ParkedEntry.release_policy` and to stop asserting `IssueCounter.observed_updated_at`.

Update `README.md` to replace the statement that parking clears when Linear reports a newer `updated_at`. Document that operator parks and abort parks require explicit unpark or retry, while system cap parks can clear when core issue fields change. Also document that url-only, label-only, blocker-only, timestamp-only, and comment-only changes do not release auto parks. Update the Linear command comments section to mention that `/scherzo abort` leaves the issue explicitly parked.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the pass count in this plan's Progress section. Expect the current suite to pass before making changes.

2. In `src/scherzo/domain.gleam`, add `ParkReleasePolicy`, update `ParkedEntry` to use `release_policy: ParkReleasePolicy` instead of `observed_updated_at: Option(Time)`, and remove `observed_updated_at` from `IssueCounter` and `new_issue_counter`.

3. Run `direnv exec . gleam test`. Expect compile errors in files that construct `ParkedEntry`, access `ParkedEntry.observed_updated_at`, or access `IssueCounter.observed_updated_at`, including `test/domain_test.gleam`. This confirms all timestamp-release call sites have been exposed.

4. In `src/scherzo/orchestrator/core.gleam`, add `pub fn issue_fingerprint(issue: domain.Issue)` and private helper functions for stable encoding of strings, options, and ints.

5. Add tests in `test/orchestrator_core_test.gleam` named `issue_fingerprint_ignores_timestamps_and_non_core_fields_test` and `issue_fingerprint_changes_for_core_fields_test`. The first should assert that two issues differing only by `updated_at`, `created_at`, url, labels, blocker order, or blocker state have equal fingerprints. The second should assert that changing id, identifier, title, description, priority, state, or branch name changes the fingerprint.

6. Run `direnv exec . gleam test`. Fix fingerprint implementation until the new tests pass or until only expected `ParkedEntry` constructor errors remain.

7. Replace `is_parked_without_update` with policy-aware `park_blocks_dispatch` in `core.gleam`. Update `should_dispatch` and `should_dispatch_retry_candidate` to use it.

8. Replace `unpark_if_updated` with `unpark_if_issue_changed` in `core.gleam`. Update `handle_retry_candidate` to call `unpark_if_issue_changed`.

9. Change `core.apply_worker_failure` to accept `baseline_issue: domain.Issue` instead of only `issue_id: String`, derive the id from `baseline_issue.id`, and update its callers in `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/service.gleam`, and `test/orchestrator_core_test.gleam`. In daemon and service failure handling, pass `failure.final_issue` when it is `Some(issue)` and otherwise pass the original worker/candidate issue.

10. Update private core system park creation so `max_retry_attempts` and `max_sessions_per_issue` parks use `domain.AutoUnparkOnIssueChange(issue_fingerprint(baseline_issue))`. The private `park` helper should accept `baseline_issue: domain.Issue`; do not create placeholder fingerprints.

11. Update existing `test/domain_test.gleam` and `test/orchestrator_core_test.gleam` parked constructors to use `release_policy`. For old system-like parked entries, use `AutoUnparkOnIssueChange(core.issue_fingerprint(issue))`. In `test/domain_test.gleam`, rename `parked_issue_records_observed_updated_at_test` to a release-policy assertion and remove the `counter.observed_updated_at` assertion from `default_issue_counter_is_zero_test`.

12. Add `explicit_park_blocks_even_when_issue_changes_test` in `test/orchestrator_core_test.gleam`. Build a runtime state with `domain.ExplicitUnparkOnly`, then create an issue with changed `updated_at`, title, and description. Assert `core.unpark_if_issue_changed` leaves the parked entry in place and `core.should_dispatch` returns `False`.

13. Add `auto_park_ignores_comment_and_non_core_changes_test`. Build an auto park using `core.issue_fingerprint(issue)`, then create variants that change only `updated_at`, url, labels, and blocker relations. Assert `unpark_if_issue_changed` leaves the park in place and `should_dispatch` returns `False` for each variant.

14. Add `auto_park_clears_on_core_issue_change_test`. Build an auto park using `issue`, then create `changed = domain.Issue(..issue, title: "New title", updated_at: Some(birl.from_unix(2)))`. Assert `unpark_if_issue_changed` deletes `parked`, `claimed`, `retry_attempts`, and `issue_counters`, and assert `core.should_dispatch` returns `True` for the changed issue when slots are available.

15. Add or update tests for `max_retry_attempts` and `max_sessions_per_issue` parks to assert their `release_policy` is `AutoUnparkOnIssueChange(core.issue_fingerprint(issue))`. For `max_retry_attempts`, add a case where `runner.WorkerFailure.final_issue` or the direct pure baseline has a changed title and assert the stored fingerprint matches that latest issue rather than the original issue.

16. In `src/scherzo/orchestrator/daemon.gleam`, change `park_issue_state` to use `domain.ExplicitUnparkOnly`.

17. In `daemon.dispatch_candidates`, before checking `core.should_dispatch`, update the daemon state through the new daemon helper that wraps `core.unpark_if_issue_changed(state.runtime, issue)` and cancels the retry timer if the helper removed `runtime.retry_attempts[issue.id]`. Keep the rest of the dispatch logic unchanged.

18. In `src/scherzo/orchestrator/service.gleam`, update the one-shot `dispatch_candidates` helper to bind `let state = core.unpark_if_issue_changed(state, issue)` before `core.should_dispatch`, and update the failed worker branch to pass the latest known issue into the new `core.apply_worker_failure` signature.

19. Add a daemon control test in `test/orchestrator_daemon_control_test.gleam` or a new focused test file that applies `command.ParkIssue` and asserts the snapshot's parked entry has `release_policy == domain.ExplicitUnparkOnly`.

20. Add a daemon regression test for Linear abort redispatch in `test/orchestrator_daemon_linear_command_test.gleam`. Use a fake Linear command client to return `/scherzo abort` for a running issue, then have the fake tracker return the same issue as a candidate with only `updated_at` changed. Assert the daemon logs one `linear_operator_command ... abort ... applied`, posts the ack, and does not log a second `dispatch_started` for the same issue after the acknowledgement/candidate poll. If the existing fake-pi abort path is too slow, use an agent runner that exposes a command subject and returns only after it receives abort.

21. Add a daemon or core test proving an auto park with a changed title is cleared before candidate dispatch and that the stale park entry is removed from the snapshot after dispatch begins. If the test creates a retry attempt for the same issue, assert the daemon snapshot no longer has a retry attempt for that issue after candidate dispatch and that no retry-driven redispatch occurs later.

22. Update `README.md` parking and Linear command sections. Replace the old `updated_at` wording with the explicit-vs-auto policy. Document that auto parks release only on core issue field changes and not on url-only, label-only, blocker-only, timestamp-only, or comment-only changes. Include the warning that `/scherzo abort` parks explicitly and requires `/scherzo unpark` or `/scherzo retry` before Scherzo will work the issue again.

23. Run `direnv exec . gleam format`.

24. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record the final pass count in Progress.

25. Optional credential-gated validation: with a private Linear test issue, enable Linear commands, run daemon mode with fake pi, post `/scherzo abort`, wait for the ack, and verify no same-issue redispatch happens on later polls even though Linear comments advanced `updatedAt`. Then post `/scherzo unpark` or `/scherzo retry` to verify explicit release still works.

26. Commit with a message such as `fix(scheduler): add explicit park release policies`.

## Testing and Falsifiability

This plan is falsified if a comment-only Linear `updatedAt` change clears any park. It is also falsified if an explicit operator park or abort park clears because the issue title, description, labels, blocker relations, url, or state changed without an explicit unpark or retry command. For auto parks, it is falsified if url-only, label-only, blocker-only, timestamp-only, or comment-only changes clear the park.

Pure tests in `test/orchestrator_core_test.gleam` must cover fingerprint stability, ignored non-core fields, fingerprint changes for core fields, explicit park blocking, auto park ignoring timestamp-only and non-core-only changes, auto park clearing on core issue changes, system cap park policy construction, and `max_retry_attempts` using the latest baseline issue supplied to `core.apply_worker_failure`.

Domain tests in `test/domain_test.gleam` must cover the new `ParkedEntry.release_policy` field and must stop asserting `IssueCounter.observed_updated_at`, because that field should no longer exist.

Daemon tests must cover at least one operator path creating an explicit park, one candidate-dispatch path clearing a stale auto park before dispatch, and retry timer cleanup when candidate dispatch removes `runtime.retry_attempts`. A Linear-command regression test must model `/scherzo abort` followed by an issue candidate whose only difference is `updated_at`; the issue must not redispatch. The one-shot service path is covered by the full compile/test run after updating its `core.apply_worker_failure` call site.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests that refer to `ParkReleasePolicy`, `issue_fingerprint`, and `unpark_if_issue_changed` should fail to compile. After implementation, `src/scherzo/orchestrator/service.gleam` should also compile against the new pure scheduler signatures, and the full deterministic suite should pass.

## Validation and Acceptance

Accept the deterministic implementation when:

- `domain.ParkedEntry` has an explicit release policy instead of `observed_updated_at`.
- `domain.IssueCounter` no longer has an `observed_updated_at` field.
- `core.issue_fingerprint` ignores timestamps, url, labels, and blocker relations but changes for core issue fields: id, identifier, title, description, priority, state, and branch name.
- Operator park and abort paths create `ExplicitUnparkOnly` parks.
- System retry/session cap parks create `AutoUnparkOnIssueChange` parks based on a concrete latest-known issue, not an empty placeholder fingerprint.
- `core.apply_worker_failure` callers pass the latest known issue, including `runner.WorkerFailure.final_issue` when present.
- Comment-only `updated_at` changes do not clear any park.
- Core issue field changes clear auto parks and remove stale entries from runtime state.
- Daemon candidate dispatch cancels retry timer handles when stale auto-park clearing removes a retry attempt.
- Explicit `unpark` and `retry` still release parked issues intentionally.
- The Linear abort acknowledgement regression test proves no same-issue redispatch after `/scherzo abort` and ack comments.
- `direnv exec . gleam test` passes.

Credential-gated manual acceptance, if run, should use a private Linear issue and fake pi. Post `/scherzo abort`, wait for the acknowledgement, wait for at least two further poll intervals, and verify logs contain no new `dispatch_started` for the same issue. Then post `/scherzo unpark` or `/scherzo retry` and verify explicit release behavior.

## Rollout, Recovery, and Idempotence

This change affects only in-memory runtime state. Restarting the daemon still loses parked state, as before. There is no database or file migration.

The rollout is safe because it makes operator parks stricter. If an issue remains parked unexpectedly after the change, the recovery action is explicit and already supported: use `scherzoctl unpark <issue>`, `/scherzo unpark`, `scherzoctl retry <issue>`, or `/scherzo retry` depending on desired behavior.

If the fingerprint is too sensitive and auto parks clear too often, tests should identify the noisy field and the implementation can remove that field from the fingerprint. If the fingerprint is not sensitive enough and auto parks do not clear after meaningful core issue edits, add the missing core field to the fingerprint and update tests. Do not add url, labels, or blocker relations without a new stakeholder decision because this plan intentionally excludes them from auto-release.

## Artifacts and Notes

Manual Linear validation evidence that motivated this plan:

    level=info service=scherzo event=linear_operator_command comment_id=e6f8d0f9-e2cb-424e-bd76-b9084b29f20a command=abort status=applied
    level=warn service=scherzo event=issue_parked issue_id=226ba516-7ca2-4c77-bdc7-ddd6ebd70678 reason=operator_abort
    level=info service=scherzo event=candidates_fetched count=1
    level=info service=scherzo event=dispatch_started issue_id=226ba516-7ca2-4c77-bdc7-ddd6ebd70678 issue_identifier=LIV-10 run_id=...

The desired post-fix evidence is the same abort and park log without the later `dispatch_started` until an explicit unpark or retry command appears.

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, define:

    pub type ParkReleasePolicy {
      ExplicitUnparkOnly
      AutoUnparkOnIssueChange(issue_fingerprint: String)
    }

    pub type ParkedEntry {
      ParkedEntry(
        issue_id: String,
        identifier: String,
        reason: String,
        release_policy: ParkReleasePolicy,
        parked_at_ms: Int,
      )
    }

In `src/scherzo/domain.gleam`, `IssueCounter` should become:

    pub type IssueCounter {
      IssueCounter(failure_attempts: Int, worker_sessions: Int)
    }

In `src/scherzo/orchestrator/core.gleam`, expose:

    pub fn issue_fingerprint(issue: domain.Issue) -> String

    pub fn unpark_if_issue_changed(
      state: domain.RuntimeState,
      issue: domain.Issue,
    ) -> domain.RuntimeState

Modify the existing worker failure API to require the latest issue baseline:

    pub fn apply_worker_failure(
      state: domain.RuntimeState,
      config: domain.EffectiveConfig,
      baseline_issue: domain.Issue,
      now_ms: Int,
    ) -> Transition

No new package dependency should be required. Use existing `gleam/list`, `gleam/string`, `gleam/int`, `gleam/option`, and `scherzo/domain` modules.
