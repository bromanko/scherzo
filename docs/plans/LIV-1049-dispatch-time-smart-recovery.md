# LIV-1049 dispatch-time smart recovery for requeued triage tasks

## Purpose / Big Picture

When an operator moves a failed Scherzo issue from `Triage` or another failure-handling state back to `Todo`, Scherzo should treat that as an explicit request to recover retained work safely instead of silently starting over. The visible outcome is that Scherzo resumes from the safest retained workflow step when a verified step boundary exists, retries only artifact publication when the workflow outputs already exist, or rejects the redispatch with clear operator evidence when recovery cannot be proven safe. A successful implementation lets operators use the normal Linear board workflow while preserving retained artifacts, ledger evidence, and existing explicit `scherzoctl retry-step` and `scherzoctl artifact publication retry` controls.

## Problem Framing and Constraints

Today the safe recovery paths exist mostly behind explicit commands such as `scherzoctl retry-step` and `scherzoctl artifact publication retry`, while the natural Linear workflow is to fix an external problem and drag the issue back to a dispatchable state. A naive redispatch can discard useful retained artifacts, duplicate expensive work, publish the same output twice, or loop between `Todo` and failure. The change must fit the current daemon, transition/effect runner, ledger projection, workflow recovery, publication retry, and Linear polling architecture; it must not invent a new scheduler, mutate provider-live or browser/cache state, or make whole-task retry the default fallback. Automated validation is a pre-publish obligation; the staging/dogfood board exercise is deferred human/operator evidence after implementation and before broad dogfood enablement.

## Strategy Overview

Add a dispatch-time recovery preflight after the dispatcher has refreshed a candidate issue and rechecked dispatch preconditions, but before it reserves a fresh worker claim. The preflight uses retained Scherzo ledger history for the candidate issue to classify the requeue as one of four outcomes: safe step recovery, safe publication-only recovery, already-active or parked state that must be skipped, or unsafe recovery that must fail closed. Step recovery must reuse the existing retry-step planner and finalizer, including workflow fingerprint, issue fingerprint, task identity, artifact hash, active-worker, and parking checks. Publication-only recovery must reuse the existing artifact publication retry path and must not rerun the producing workflow. Unsafe recovery must leave operator-visible diagnostics, attempt to move the issue back to the configured workflow failure state or `Triage`, and locally suppress immediate redispatch if that tracker update fails. This is proportionate because it adds a preflight and reuse layer around existing recovery gates instead of replacing dispatch, retry, or publication infrastructure.

## Alternatives Considered

The simplest alternative is to keep requiring explicit `scherzoctl retry-step` or `scherzoctl artifact publication retry`. That is safe but misses the common operator workflow and leaves expensive mistakes easy to make. Another alternative is to always run `scherzoctl retry` when a failed issue reappears in `Todo`; that is rejected because it redoes all work and can overwrite the signal preserved in retained artifacts. A broader state-machine rewrite is also rejected because the existing dispatch, repair, and publication paths already contain the required safety checks. A documentation-only or helper-script migration is insufficient because the unsafe behavior happens inside daemon dispatch, not merely in operator instructions.

## Risks and Countermeasures

The main risk is retrying the wrong retained run after issue, workflow, or artifact drift. The countermeasure is to require the same drift checks as explicit retry-step and publication retry before any recovery starts, and to test stale workflow fingerprints, issue fingerprints, missing artifacts, active workers, parked issues, and non-retryable publication attempts. A second risk is a publication retry that succeeds but leaves the issue in `Todo`; the implementation must report the publication recovery and move the issue out of the dispatch state or fail closed if no safe completion target exists. A third risk is comment or state-transition failure during fail-closed handling; the implementation must log the failure and locally suppress immediate redispatch rather than looping. A fourth risk is accidentally coupling dispatch recovery to cleanup, provider-live state, local caches, browser state, or helper migration; the implementation must keep this change within daemon dispatch, ledger recovery, artifact publication retry, and tracker comments/state transitions only.

## Scope Boundaries

In scope are dispatch-time detection of requeued failed or triaged tasks, safe retry-step resumption, publication-only retry from retained outputs, rejection instead of automatic whole-task retry, loop prevention, operator-visible diagnostics, and focused automated tests. The implementation should touch dispatcher/recovery modules and tests such as `src/scherzo/orchestrator/transition.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/effects/interpreter.gleam`, `src/scherzo/workflow_repair.gleam`, `src/scherzo/ctl/artifact_publication_retry.gleam`, `test/orchestrator_transition_dispatch_test.gleam`, `test/orchestrator_daemon_retry_step_test.gleam`, and `test/artifact_publication_runtime_test.gleam` or nearby new test files. Out of scope are changing explicit `scherzoctl retry` semantics, resuming arbitrary in-flight work without a safe step boundary, adding UI surfaces beyond comments/logs/ledger evidence, replacing existing workflow recovery or artifact publication implementations, migrating docs/helpers unrelated to this behavior, and mutating provider-live, remote-store, browser, or generic cleanup caches.

## Milestones

Milestone 1 establishes a pure recovery classifier and its fixtures. At the end, unit tests can distinguish fresh candidates, failed-step candidates, publication-only candidates, unsafe candidates, active candidates, and parked candidates from retained ledger projection without starting workers or touching Linear.

Milestone 2 wires step recovery into dispatch after successful claim validation and before fresh claim reservation. At the end, a requeued issue with an interrupted retained workflow run produces the same repair records and recovered worker start as an accepted explicit `retry-step` command, and the test proves no `ClaimIssue` or fresh full worker claim is emitted.

Milestone 3 wires publication-only recovery. At the end, a requeued issue whose workflow already produced retained outputs but whose publication failed causes only the retryable publication route to run, records a new publication attempt, reports the result, and does not rerun the producing workflow.

Milestone 4 adds fail-closed reporting and loop prevention. At the end, unsafe recovery appends diagnostics, comments and moves the issue back to the configured failure state or `Triage` when possible, and suppresses immediate redispatch locally when tracker reporting fails.

Milestone 5 completes regression, lint, and operator evidence. At the end, fresh dispatch, explicit retry commands, startup recovery, retry-step, artifact publication retry, provider-live/cache invariants, and workflow validation still behave as before; the full validation commands pass; and a deferred dogfood/staging transcript shows a seeded failed issue moved from `Triage` to `Todo` recovering without a fresh full dispatch.

## Progress

- [x] (2026-06-11) Reviewed current dispatch, retry, retry-step, workflow recovery, publication retry, Linear polling, and retained-state paths.
- [x] (2026-06-11) Authored the initial review document and separated concrete implementation detail into the structured implementation pack.
- [x] (2026-06-11) Incorporated review feedback by making acceptance evidence, test obligations, milestone specificity, manual/dogfood evidence, provider-live/cache boundaries, full validation, and linting explicit.

## Decision Log

- Decision: Put smart recovery at dispatch time after candidate refresh and before a fresh claim, not inside worker startup.
  Rationale: The candidate is still cheap to reject, no new workspace or Linear claim has been created yet, and claim validation has already confirmed the issue is still dispatchable.
  Date: 2026-06-11

- Decision: Reuse retry-step and publication retry safety checks instead of inventing a separate recovery engine.
  Rationale: Those paths already validate drift, artifacts, run roots, and retry eligibility, making the scope proportionate and easier to prove with existing regression fixtures.
  Date: 2026-06-11

- Decision: Do not perform automatic whole-task retry when step and publication recovery are unavailable.
  Rationale: A fresh full run is expensive and can hide the retained failure; operators can still request it explicitly when they intentionally want to discard retained recovery context.
  Date: 2026-06-11

- Decision: Treat automated tests, format, `glinter`, and `scherzo_lint` as pre-publish requirements, while staging/dogfood board evidence is deferred operator evidence after implementation.
  Rationale: The code must be mechanically safe before publication, but a real Linear board exercise may require credentials and daemon access that are not always available in the implementation workspace.
  Date: 2026-06-11

- Decision: Exclude docs/helper migration and provider-live/cache mutation from this issue.
  Rationale: The failure mode is dispatch recovery, and widening the task to cleanup ownership, browser state, remote stores, or unrelated helpers would increase blast radius without improving recovery correctness.
  Date: 2026-06-11

## Validation and Acceptance

Acceptance requires automated evidence for safe step recovery, publication-only recovery, no automatic whole-task fallback, fail-closed rejection, idempotence, active/parked skip behavior, unchanged fresh dispatch, explicit command regressions, and provider-live/cache non-interference. The implementation must add focused unit and daemon integration tests that exercise retained ledger fixtures and assert the absence of fresh `ClaimIssue` effects on recovery paths. Pre-publish validation must run `direnv exec . gleam test -- --suite unit`, `direnv exec . gleam test -- --suite contract`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root and expect success. Deferred human/operator evidence should then be collected in a staging or dogfood workspace by moving a seeded failed issue from `Triage` to `Todo` and capturing the resulting comment, ledger entries, publication retry or recovered-worker event, and absence of a fresh full dispatch.

## Rollout, Recovery, and Idempotence

Roll out as an additive dispatcher behavior that leaves fresh candidates and explicit operator commands unchanged. If the change misbehaves, operators can pause dispatch, move affected issues back to `Triage`, and use the existing explicit retry commands while the implementation is reverted or disabled. Recovery actions must be idempotent: an accepted step recovery is not duplicated while the recovered run is active, publication retry checks the latest attempt before retrying, fail-closed handling removes the issue from the dispatchable state when tracker updates succeed, and local suppression prevents immediate redispatch when tracker updates fail. The change must not mutate provider-live state, remote stores, browser state, or generic cleanup caches; rollback is therefore limited to dispatcher/recovery code and any ledger records intentionally appended during attempted recovery.

## Open Questions and Clarifications Needed

No open questions. The implementation should prefer the configured workflow failure state for fail-closed moves and fall back to `Triage` when no configured state is available.
