# LIV-1206 output-only workflow contract recollection repair

This ExecPlan review document is paired with a structured implementation pack. The review document stays concise and human-reviewable; the implementation pack contains the mechanical file-by-file steps, interfaces, tests, and command details.

## Purpose / Big Picture

After this change, an operator can repair a retained workflow run whose DAG steps already completed but whose final workflow contract outputs were not recorded correctly. The visible behavior is a new output-only recollection path such as `scherzoctl recollect-outputs run:<run-id> --json` that recomputes workflow outputs from retained step artifacts and retained workspaces, records a fresh output manifest when valid, and does not rerun completed steps. The required evidence is automated pre-publish test and lint output plus, optionally after implementation, a dogfood operator check against the historical retained run if it is still available.

## Problem Framing and Constraints

The motivating failure is a retained run where step-level recovery succeeded, but final workflow output collection failed with `workflow_output_artifact_set_invalid:artifact_descriptor_missing_ref_artifact`. Existing `retry-step` correctly rejects the run because no failed step remains, but that leaves operators with no safe way to republish or repair the workflow-level output contract except whole-workflow retry or manual state surgery. The repair path must be additive, auditable through ledger records, fail closed on missing retained evidence, and must not change existing step attempts, restart a worker, alter provider-live/cache behavior, or silently publish outputs.

## Strategy Overview

Add an explicit output-only operator command rather than overloading `retry-step`. The command should validate current workflow and issue identity, prove that the retained run has no remaining failed, pending, running, or interrupted step boundary, recover only the retained artifacts and workspaces needed by the workflow output source specs, and then ask the existing contract output materialization logic to produce a new manifest in recollection mode. Successful recollection appends a new `workflow_run_outputs_recorded` record and returns an operator message; it does not append a new successful terminal workflow outcome and it does not trigger publication retry. The implementation should reuse existing command, daemon, artifact-store, ledger, and runbook conventions, while adding narrow recollection helpers where existing normal-output writers are intentionally idempotent and cannot append a fresh record.

## Alternatives Considered

Extending `retry-step` was rejected because a run with all steps successful has no step boundary to retry, and mixing step repair with output publication repair would make rejection reasons confusing. Restarting the whole workflow was rejected because it repeats completed work and can duplicate external side effects. Manual ledger editing was rejected because it is not auditable or safe. Automatically retrying publication after recollection remains out of scope because output repair and publication retry have different failure modes and should be composed explicitly after the output manifest exists. Changing provider-live/cache semantics or documentation helper migration behavior was rejected as unrelated scope for this operator repair.

## Risks and Countermeasures

The main risk is recording a manifest from stale or incomplete evidence. The countermeasure is to reuse the same workflow and issue drift checks as recovery, verify retained step artifacts by ledger hash, require source workspaces to still exist, and reject with source-specific reasons for missing artifacts, source workspaces, invalid JSON, or artifact-set descriptor failures. A second risk is producing ambiguous terminal history; the countermeasure is to append only the fresh output record on success and leave workflow terminal outcome unchanged. A third risk is overwriting previous output artifacts; the countermeasure is to write recollected manifests and blobs under a recollection-specific artifact namespace before appending the latest manifest record. A fourth risk is under-evidenced acceptance; the countermeasure is to require targeted red/green tests, command/protocol tests, daemon tests, runbook coverage, full test/format/lint gates, and a recorded statement that any live dogfood check is deferred human/operator evidence rather than a pre-publish blocker.

## Scope Boundaries

In scope are the operator command, control protocol and CLI parsing, pure recollection planning and validation, output-manifest recollection storage, daemon command handling, focused tests, and recovery runbook updates in `docs/runbooks/workflow-recovery.md`. Existing `retry-step`, whole-task retry, workflow DAG execution, publication execution, cleanup policy, provider-live/cache behavior, Linear state transitions, and docs/helper migration behavior stay unchanged. Automatic publication retry, browser validation, and a new recovered-success terminal outcome are deferred. The only manual or dogfood check in this plan is an optional post-implementation operator check against the historical LIV-1202 retained run if that local evidence still exists.

## Milestones

Milestone 1 proves the current gap and adds the command surface. At the end, `src/scherzo/control/command.gleam`, `src/scherzo/ctl.gleam`, `src/scherzo/ctl/command_registry.gleam`, usage text, and `test/ctl_test.gleam` parse, encode, decode, and document `recollect-outputs run:<run-id>`, while an initial recollection test demonstrates that a terminal run with successful steps and no valid output manifest has no existing output-only repair path.

Milestone 2 adds the recollection planner and fail-closed validation. At the end, a focused production module, expected to be `src/scherzo/workflow_output_recollection.gleam`, can select one retained run by id, validate current workflow and issue identity, prove all DAG steps are complete, recover required source step artifacts and workspaces from ledger-proven evidence, and reject missing evidence with stable reasons that tests assert exactly.

Milestone 3 adds fresh output recording. At the end, recollection writes outputs in a recollection-specific artifact namespace, appends a fresh `workflow_run_outputs_recorded` record only after all required outputs validate, and reports idempotent already-valid output manifests without mutating the ledger. Tests must verify the historical artifact-set descriptor failure shape by restoring retained artifact evidence and proving the fresh manifest includes the expected retained artifact descriptor.

Milestone 4 integrates daemon behavior and operator documentation. At the end, `scherzoctl recollect-outputs run:<run-id> --json` routes through the daemon, returns clear applied or rejected output, appends no step attempts and starts no worker, and `docs/runbooks/workflow-recovery.md` explains when to use recollection versus `retry-step`, publication retry, or full retry. This milestone also records that no docs/helper migration, provider-live/cache, or browser work is required.

Milestone 5 completes regression validation. At the end, targeted recollection tests, command/protocol tests, daemon tests, runbook checks, full Gleam tests, formatting, glinter, and Scherzo lint pass. Pre-publish acceptance is the automated evidence from those commands; live dogfood on the historical retained run is explicitly deferred optional operator evidence.

## Progress

- [x] (2026-06-21) Read the repo-local ExecPlan guidance, inspected the current workflow contract output path, retry-step repair path, ledger projection, control command surface, and recovery runbook, and drafted this concise review document.
- [x] (2026-06-21) Incorporated review feedback by making acceptance evidence, test obligations, milestone-specific files, deferred manual/dogfood checks, documentation scope, provider-live/cache non-scope, full validation, and linting explicit in the review document and structured implementation pack.

## Surprises & Discoveries

Existing output recording is idempotent for the normal workflow runner: `workflow_checkpoint.ledger_writer` skips `workflow_run_outputs_recorded` when a projection already has an output manifest for the run. Recollection therefore needs an explicit fresh-record path instead of calling the normal writer unchanged.

The current output artifact store already has generation-aware paths for repair output manifests and blobs, but this plan requires recollection-specific naming or generation semantics so an output-only repair cannot accidentally overwrite either the original output path or step-repair artifacts.

## Decision Log

- Decision: Add a new `recollect-outputs` operator command rather than extending `retry-step`.
  Rationale: Output repair has no failed step boundary and needs different success and rejection semantics.
  Date: 2026-06-21.

- Decision: Do not append a new successful workflow terminal outcome in this slice.
  Rationale: The acceptance criteria require no duplicate or ambiguous successful terminal state; a fresh output record is sufficient and auditable.
  Date: 2026-06-21.

- Decision: Do not automatically run publication retry after recollection.
  Rationale: Publication retry should remain a separate explicit operator action until a later design proves safe composition.
  Date: 2026-06-21.

- Decision: Treat live dogfood on the historical retained run as optional post-implementation evidence, not a pre-publish gate.
  Rationale: The retained local run may not exist in every workspace, so pre-publish acceptance must be reproducible from automated tests while still preserving a useful operator check when evidence is available.
  Date: 2026-06-21.

## Outcomes & Retrospective

Implementation has not started. The expected outcome is an additive, operator-visible output repair path that closes the retained-run gap without expanding step retry semantics, changing workflow terminal outcome policy, changing provider-live/cache behavior, or adding browser/manual pre-publish obligations. At completion, the implementer should paste concise automated validation evidence here or in the paired workflow artifacts before marking the plan complete.

## Validation and Acceptance

Acceptance is pre-publish automated evidence unless explicitly noted otherwise. Targeted tests must show that `recollect-outputs run:<run-id>` records a fresh output manifest for a run whose steps succeeded but whose output manifest is missing or invalid, that restored retained artifacts allow a previously rejected artifact-set output to pass, that missing source step artifacts or source workspaces reject with stable reasons, and that no new step attempts, workflow worker starts, successful terminal `workflow_run_finished` records, provider-live/cache writes, or publication attempts are produced. Command and protocol tests must show CLI parsing, JSON encoding/decoding, help text, applied output, rejected output, and idempotent already-valid behavior. Documentation acceptance is a runbook update that tells operators to use recollection only for output contract repair, to use `retry-step` only for failed or interrupted step boundaries, and to run publication retry separately after a valid output manifest exists. Final validation must run `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No browser validation is required; live dogfood on the historical LIV-1202 run is optional post-implementation operator evidence if that retained run is still available.

## Rollout, Recovery, and Idempotence

Rollout is additive: old ledgers continue to project normally, and only an explicit operator command appends fresh output records. If the change misbehaves, reverting the code leaves existing records readable because the new success artifact is still a normal `workflow_run_outputs_recorded` record; any recollection-specific artifact paths remain inert retained evidence. Repeating recollection after a valid latest manifest should be a no-op with a clear message that no fresh record was needed; repeating after missing evidence should produce the same rejection until the operator restores the evidence. Because publication retry remains separate, a failed recollection cannot accidentally publish stale data, and an operator can recover by restoring artifacts/workspaces, rerunning recollection, or falling back to full retry when retained evidence is insufficient.

## Open Questions and Clarifications Needed

No blocking open questions for the MVP. Future work can decide whether output recollection should participate in an automatic publication retry flow or introduce a durable recovered-success terminal outcome distinct from existing `completed` and `succeeded_after_recovery`.
