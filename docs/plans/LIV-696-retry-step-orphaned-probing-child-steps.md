# Prevent retry-step recovery from orphaning probing child steps

## Purpose / Big Picture

After this change, an operator who uses `scherzoctl retry-step` to recover a retained workflow run will either get a clear fail-closed rejection before repair records are appended and before any descendant step starts, or Scherzo will keep the recovered parent and its child step sessions terminally consistent. A stopped recovered parent must not leave native review lanes or other YAML workflow agent steps looking alive in `probing`, and an operator must have a dry-run-first cleanup path for already-wedged retained runs. The observable result is that `scherzoctl ps --json`, `scherzoctl session --json`, and `scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run` tell the same story about parent run state, child step state, and the safe next action.

## Problem Framing and Constraints

LIV-689 showed a recovered `workflow:execplan-implementation` parent session stopping with `non_active` while four native review lane child sessions remained visible as probing. The issue had drifted out of the configured active state before `retry-step`, and later moving it back to active did not restart the stopped parent. The current retry-step path also treats the issue's current state as additionally active during recovery finalization, which can make a non-active retained run look recoverable long enough to spawn a recovered worker. The fix must preserve retry-step's fail-closed recovery checks, avoid manual ledger edits, keep operator commands explicit, avoid browser UI work, avoid provider-live or cache behavior changes, and expose enough session state through `scherzoctl ps --json` and `scherzoctl session --json` to diagnose the wedge.

## Strategy Overview

The right-sized approach is to close the two gaps that allow the wedge without changing the workflow YAML lanes or Linear state policy. First, `retry-step` should check the refreshed issue state against the configured active states before planning, appending repair records, or spawning the recovered worker; if the issue is not active, it should reject with a message naming the run id, current issue state, and next action. Second, YAML workflow parent-stop paths should share an orphan detector that knows which step sessions belong to a run and which retained step attempts are still unfinished. That detector should finish active child sessions, clear their command routes, and append missing `StepAttemptInterrupted` facts with a distinct reason such as `orphaned_parent_stopped` only when the parent workflow run is already terminal, stopped, interrupted, or absent from the active worker registry. The same detector should power an additive, idempotent `scherzoctl recovery cleanup-orphan-steps run:<run-id>` command that defaults to dry run for historical retained cases.

## Alternatives Considered

Automatically moving the Linear issue to an active state during `retry-step` would be convenient, but it hides operator policy and could reclaim work that was intentionally triaged or parked. Only marking the parent stopped is insufficient because `ps` would still show child lanes as active. Relying on manual `abort` commands is also insufficient because orphaned step sessions may have no live command route and operators should not have to infer ledger edits. Changing provider-live checks, cache refresh behavior, or workflow lane definitions would be a broader migration than this incident requires and would add risk without addressing the orphaned session consistency bug.

## Risks and Countermeasures

The main risk is interrupting a legitimate live child step. The countermeasure is to clean up only descendants whose parent workflow run is already terminal, stopped, interrupted, or otherwise absent from the active worker registry, and to make dry-run output the default for the operator cleanup command. A second risk is duplicate cleanup records on repeated attempts; the command and live stop helper must skip attempts that are already finished, interrupted, or superseded, and tests must run cleanup twice to prove the second run is a no-op. A third risk is hiding issue-state drift; retry-step rejection must include the current state and recommended action instead of silently claiming the issue. A fourth risk is an incomplete operator migration where code exists but runbooks and CLI help do not explain the new recovery command; the documentation and helper-facing usage text are therefore pre-publish obligations. A fifth risk is accidental provider-live or cache regression; the implementation should not touch provider-live or cache modules, and if review discovers such a touch then provider-live/cache tests become required before publish.

## Scope Boundaries

In scope are retry-step issue-state preflight, live parent-stop cleanup for YAML workflow child sessions, session JSON/orphan reporting, an idempotent recovery cleanup command, operator-facing output, tests for recovered native review fan-out, CLI/protocol/schema tests for the cleanup command, and updates to the recovery runbook and getting-started operator guidance. Out of scope are browser UI changes, provider-live or cache behavior changes, changing workflow YAML lane definitions, changing Linear state configuration semantics, and inventing bundle references in this review document.

## Milestones

Milestone 1 captures the failure with targeted tests before implementation. Update the existing non-active retry-step test in `test/orchestrator_daemon_retry_step_test.gleam` so a retained run in `Triage` is rejected before spawn, before repair records are appended, and with a message naming `run-1`, `Triage`, and the operator action to move the issue to an active state before retrying. Add a recovered native review fan-out test that starts child step sessions, forces the recovered parent to stop, and initially observes the bad pre-fix behavior: child sessions remain active or unfinished attempts lack interruption records. The evidence is that the targeted test command fails before the implementation and passes after it.

Milestone 2 adds the shared orphan detector and JSON metadata. Add a small, pure module or helper near the orchestrator transition code that takes the projection, worker registry child-session map, parent run id, and current parent status, then returns the unfinished child step ids, attempt indexes, session ids, and whether cleanup is safe. Extend session JSON in the existing control API, not a new browser surface, so `ps --json` and `session --json` expose parent run id, workflow step id, attempt index, parent session id when known, orphan status, issue state, and recommended cleanup or retry action.

Milestone 3 changes retry-step preflight and live parent-stop cleanup. Remove the behavior that makes the current non-active issue state temporarily active for retry-step recovery validation, reject non-active states before appending repair records, and reuse the orphan detector from parent stop, standard recovered-worker failure, recovery validation failure, non-active refresh failure, and worker-down paths. At the end of the milestone, a stopped recovered parent finishes registered child sessions, appends missing `StepAttemptInterrupted` records for unfinished attempts, and clears child command routes from the registry.

Milestone 4 adds the dry-run-first retained cleanup command. Add the `scherzoctl recovery cleanup-orphan-steps run:<run-id>` CLI/protocol/operator command with `--dry-run` as the default and `--yes` as the only mutating mode. The command should list affected step ids, attempt indexes, session ids when present, current parent state, and the exact records it would append; `--yes` should append only missing interruption records and finish any still-retained active sessions. Re-running `--dry-run` and `--yes` against the same run must report that nothing remains to clean.

Milestone 5 updates operator documentation and runs targeted plus full validation gates before publish. Update `docs/runbooks/workflow-recovery.md`, `docs/GETTING_STARTED.md`, and CLI usage/help tests so operators see retry-step's non-active rejection, the dry-run cleanup workflow, idempotence expectations, and recovery guidance for `ps --json` / `session --json`. The retained LIV-689 dogfood cleanup is useful post-implementation operator evidence and remains deferred until a human/operator has an affected retained run available; it is not a pre-publish blocker. No browser validation is required.

## Progress

2026-05-28: Reviewed retry-step handling, recovered workflow spawning, YAML step session registration, parent stop transitions, session JSON, ledger projection, and existing recovery runbooks; drafted this review document. No implementation code has been changed.

2026-05-28: Incorporated review feedback by making pre-publish acceptance evidence explicit, separating the deferred LIV-689 dogfood check from required validation, requiring docs/help migration, documenting provider-live/cache as out of scope, and spelling out full test/format/lint gates. No implementation code has been changed.

## Decision Log

2026-05-28: Retry-step should reject non-active issue states by default instead of auto-claiming, because issue movement is operator policy and rejection satisfies the acceptance criterion without hidden state changes.

2026-05-28: The cleanup command should append normal `StepAttemptInterrupted` records with a distinct reason such as `orphaned_parent_stopped`, because projection and retry-step already understand interrupted attempts.

2026-05-28: Orphan reporting should be visible in existing `ps` and `session` JSON rather than only in logs, because operators diagnose this class of wedge from the control API.

2026-05-28: The LIV-689 retained-run cleanup is a deferred dogfood/operator check, not a pre-publish requirement, because automated tests can prove the behavior without depending on the availability of a particular retained production run.

2026-05-28: Provider-live and cache behavior should remain unchanged for this plan; if implementation review finds those modules touched, the scope must be revised or provider-live/cache validation must be added before publish.

## Validation and Acceptance

Pre-publish evidence must include `direnv exec . gleam test test/orchestrator_daemon_retry_step_test.gleam` showing non-active retry-step rejection, no recovered-worker spawn, no descendant step start, no repair-record append, and an operator message naming the run id, issue state, and next action.

Pre-publish evidence must include targeted workflow/session tests for native review fan-out during recovered parent stop. The tests must prove child lane sessions are no longer `probing`, unfinished attempts receive interruption records with the chosen orphan reason, repeated cleanup is a no-op, child command routes are cleared, and `ps --json` plus `session --json` expose parent run id, affected step/session ids, attempt index, orphan status, issue state, and recommended cleanup or retry action.

Pre-publish evidence must include CLI, protocol, schema, and help/usage tests for `scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run` and `--yes`, including negative coverage for active parents, unknown runs, already-clean runs, and duplicate cleanup attempts. The documentation evidence is that the recovery runbook and getting-started operator guidance describe dry-run-first cleanup and non-active retry-step rejection in terms an operator can follow.

Final validation must run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No browser validation is required. Provider-live/cache validation is not required if the implementation does not touch provider-live or cache modules; if those modules are touched, publish must stop until this review document and the implementation pack add the corresponding provider-live/cache checks. If a retained LIV-689-style run is available after implementation, the deferred operator check is to run the cleanup command first with `--dry-run`, then with `--yes`, and confirm the former probing lanes become terminal or visibly non-running.

## Rollout, Recovery, and Idempotence

The rollout is additive: new cleanup facts are appended to the ledger, existing records are not rewritten, and the cleanup command defaults to dry run. If the change misbehaves, reverting the code restores the old runtime behavior while retained interruption records remain valid projection facts. Repeating parent-stop cleanup or the explicit cleanup command must not append duplicate interruptions, re-finish already terminal sessions, or mutate anything in dry-run mode. The safe operator recovery path is to inspect `ps --json` or `session --json`, run `scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run`, review the listed step/session ids, and only then rerun with `--yes` if the parent is terminal or stopped.

## Open Questions and Clarifications Needed

No blocking clarification is needed. This plan chooses explicit non-active rejection over automatic claim, applies cleanup to all YAML workflow child steps rather than only native review lanes, keeps provider-live/cache and browser behavior out of scope, and uses best-effort live session finishing plus ledger interruption records so orphaned sessions become visibly non-running even when no Pi command route remains.
