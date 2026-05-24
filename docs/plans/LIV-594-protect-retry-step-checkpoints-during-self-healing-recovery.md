# Protect retry-step checkpoints during self-healing recovery

This ExecPlan v2 review document frames the design for LIV-594. Mechanical implementation details, file-by-file edits, commands, and test instructions are intentionally kept out of this human-reviewable document and supplied through the structured implementation pack.

## Purpose / Big Picture

After this change, a self-healing recovery worker can still repair the retained workspace, but it cannot leave the durable checkpoint evidence for a later `scherzoctl retry-step run:<run-id> --step <step-id>` unusable. If recovery tries to edit or delete a ledger-addressed artifact such as `runs/<run>/<step>/attempt-1.json`, Scherzo restores the original bytes before the recovery result is accepted and records an operator-visible diagnostic.

The visible operator outcome is narrow: ordinary workspace edits remain allowed, while the step artifacts and workflow manifests that retry-step validates remain byte-for-byte consistent with the sha256 values already recorded in the local ledger.

## Problem Framing and Constraints

The LIV-582 dogfood run showed that self-healing recovery can make a useful local repair and also accidentally mutate a retained artifact that the ledger already addressed by sha256. Later, retry-step correctly rejected the run with `artifact_recovery_failed`, but by then the recovery attempt had damaged the evidence needed to retry safely.

This plan is constrained to checkpoint protection, not a new artifact platform. It must preserve recovery's ability to edit retained workspaces, avoid read-only mounts or copy-on-write workspace redesign, and fail closed with a specific recovery diagnostic if Scherzo cannot restore a protected file.

## Strategy Overview

Add a targeted preflight/postflight guard around the existing workflow step recovery execution in `src/scherzo/workflow_run.gleam`. Before the nested recovery worker starts, Scherzo loads the current projection from the local ledger and snapshots only the already-recorded artifacts and manifests for the same workflow run that retry recovery can rely on: `StepAttemptFinished` artifact refs plus `WorkflowRunInputsRecorded` and, when already recorded, `WorkflowRunOutputsRecorded` manifest refs. Each protected entry carries the ledger sha256, current hash, local path, and backup bytes.

After the recovery worker exits and before Scherzo records a retry decision or starts the retried original step, the guard re-hashes only those protected paths. Changed or deleted files are restored from the backup and reported as `protected_checkpoint_restored`. If a restore cannot be completed and verified, recovery stops with `recovery_artifact_restore_failed` instead of letting a later retry-step command discover a generic `artifact_recovery_failed`.

If the projection presents the same protected ref more than once, the guard treats identical ref/sha256 pairs as one protected checkpoint with one backup and one possible diagnostic. If the same ref appears with conflicting sha256 expectations, recovery fails closed before the recovery worker starts with `recovery_artifact_restore_failed`, because Scherzo cannot know which historical bytes are safe to preserve.

This is sufficient because the failure mode is not arbitrary workspace mutation; it is mutation of files whose refs and sha256 values are already durable ledger facts. Protecting that small set preserves retry-step validation without changing workspace semantics or making all retained artifacts immutable.

## Alternatives Considered

Read-only artifact mounts would prevent this class of mutation, but they require broader runtime isolation and would complicate workflows that still expect local filesystem artifacts.

A global immutable artifact-store redesign would be stronger, but it is disproportionate for the immediate smart-retry problem and risks changing many unrelated write paths.

Copy-on-write recovery workspaces would also protect checkpoints, but they solve a broader isolation problem and could accidentally discard the ordinary workspace edits recovery is supposed to make.

Doing only better documentation was rejected because it would still allow a recovery worker to corrupt a retry checkpoint in a retained run.

## Risks and Countermeasures

The main risk is protecting too much and undoing valid recovery edits. The countermeasure is to guard only ledger-addressed artifact and manifest paths under `.scherzo-state/artifacts`, never the step workspace path where recovery repairs code or documents.

A second risk is missing a checkpoint that retry-step validates. The countermeasure is to build the protected set from the same projection facts used by `src/scherzo/workflow_repair.gleam` and `src/scherzo/state/recovery.gleam`: finished step attempts and recorded workflow contract manifests for the selected run.

A third risk is masking a restoration failure. The countermeasure is to verify the sha256 after every restore and to stop recovery with `recovery_artifact_restore_failed` if the restored bytes do not match or cannot be written.

A fourth risk is making non-filesystem artifact stores harder to support. The MVP may require `local_path` for restoration because the current production store is filesystem-backed; if a future store lacks local paths, the guard should fail closed before recovery rather than silently running without protection.

A fifth risk is duplicate protected refs causing multiple backups or duplicate diagnostics for the same file. The countermeasure is deterministic de-duplication by ref and expected sha256, with a fail-closed `recovery_artifact_restore_failed` preflight result if the same ref has conflicting expected hashes.

## Scope Boundaries

In scope are a small checkpoint snapshot/hash/restore guard, integration with self-healing recovery in `src/scherzo/workflow_run.gleam`, exact operator diagnostics, focused tests, and documentation in `docs/runbooks/workflow-step-recovery.md` distinguishing protected checkpoint paths from safe workspace paths.

Protected checkpoint paths are ledger-addressed retained artifacts such as `.scherzo-state/artifacts/runs/<run>/<step>/attempt-<n>.json`, `.scherzo-state/artifacts/runs/<run>/inputs.v1.json`, and already-recorded output manifests for the same run. Safe recovery paths are ordinary retained workspace directories and files passed to the recovery worker through `StepContext.workspace_path`.

Out of scope are read-only mounts, copy-on-write workspaces, a new immutable artifact API, historical artifact metadata edits, broad workspace-change auditing, structured-output salvage, browser UI work, and changing retry-step selection policy.

## Milestones

Milestone 1 defines the protected checkpoint inventory and pure guard behavior. The outcome is focused test evidence that a clean snapshot is a no-op; protected step artifacts and input/output manifests are included; duplicate identical refs collapse to one protected entry and one diagnostic; conflicting duplicate refs fail closed before recovery; a preflight read or hash mismatch records `recovery_artifact_restore_failed` without invoking the recovery worker; changed or deleted protected files are restored; running postflight again after a successful restore is a no-op with no duplicate `protected_checkpoint_restored`; and an impossible restore returns `recovery_artifact_restore_failed`.

Milestone 2 integrates the guard with self-healing workflow step recovery. The outcome is runtime evidence that recovery still edits the workspace and can request retry, while any attempted mutation of an already-recorded checkpoint is restored before the retry attempt starts.

Milestone 3 proves smart-retry compatibility. The outcome is a retained-run fixture where recovery tries to mutate an upstream artifact, Scherzo restores it, the run later remains acceptable to `workflow_repair.plan` and `recovery.finalize_workflow_candidates_with_config`, and no later `artifact_recovery_failed` is produced for that protected artifact.

Milestone 4 updates operator documentation and completes validation. The outcome is runbook text naming protected checkpoint paths, safe workspace paths, `protected_checkpoint_restored`, and `recovery_artifact_restore_failed`, plus passing targeted tests, full tests, format, glinter, and `scherzo_lint` gates.

## Progress

- [x] (2026-05-24) Reviewed the current self-healing recovery, checkpoint writer, artifact store, retry-step planner, recovery finalization, and operator runbook surfaces; drafted this review document for LIV-594. No implementation code has been changed.
- [x] (2026-05-24) Incorporated review feedback by adding preflight fail-closed evidence, postflight idempotency evidence, duplicate-ref handling, and manifest restoration acceptance to this review document and the updated implementation-pack submission.

## Decision Log

- Decision: Use a targeted snapshot/hash/restore guard rather than read-only mounts or a global artifact immutability redesign.
  Rationale: The observed failure is limited to ledger-addressed retry checkpoints, and the current filesystem artifact store can restore those paths without changing workspace behavior.
  Date: 2026-05-24

- Decision: Make restore failure a recovery-time diagnostic named `recovery_artifact_restore_failed`.
  Rationale: Operators should see the specific cause while the recovery attempt is still being finalized, not discover a later generic retry-step `artifact_recovery_failed`.
  Date: 2026-05-24

- Decision: Keep ordinary recovery workspace edits outside the protected set.
  Rationale: Recovery is useful because it can change the workspace before retrying the original step.
  Date: 2026-05-24

- Decision: Treat duplicate protected refs deterministically by collapsing identical ref/sha256 pairs and failing closed on conflicting expected hashes.
  Rationale: Duplicate refs should not create conflicting backups or duplicate operator diagnostics, and conflicting hashes mean the guard cannot safely decide which bytes are authoritative.
  Date: 2026-05-24

- Decision: Make preflight fail-closed behavior, postflight idempotency, and manifest restoration explicit pre-publish acceptance evidence.
  Rationale: These are operator-safety claims made by the plan and must be proven before implementation handoff rather than left as implied behavior.
  Date: 2026-05-24

## Validation and Acceptance

Pre-publish evidence must include targeted tests that name and prove all required guard outcomes: no mutation leaves bytes and hashes unchanged; mutation of a protected step artifact is restored and reports `protected_checkpoint_restored`; deletion of a protected step artifact is restored; mutation or deletion of protected workflow input and already-recorded output manifests is restored; simulated restore failure stops recovery with `recovery_artifact_restore_failed`; and a recovery attempt that mutates an upstream protected artifact still leaves retry-step validation viable afterward.

Preflight fail-closed evidence must prove that an unreadable protected checkpoint, a preflight hash mismatch, or conflicting duplicate expected hashes records `recovery_artifact_restore_failed`, does not invoke the recovery worker, and does not start or prepare the retried original step. Duplicate-ref evidence must prove identical duplicate refs produce one protected entry and at most one `protected_checkpoint_restored` diagnostic without conflicting backups. Idempotency evidence must restore a mutated checkpoint once, run postflight again against the same snapshot, and observe unchanged bytes plus no second `protected_checkpoint_restored` event.

The retry-step compatibility evidence must exercise `workflow_repair.plan` and `recovery.finalize_workflow_candidates_with_config` against a retained-run fixture after the recovery mutation attempt. The expected result is an accepted recovery candidate/resumption, not `artifact_recovery_failed`.

Documentation acceptance requires `docs/runbooks/workflow-step-recovery.md` to name protected checkpoint paths, safe workspace paths, and the exact diagnostic names. Full implementation validation before handoff is `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. No browser evidence is required. A live dogfood check against a retained LIV-582-shaped run is useful deferred post-implementation operator evidence, not a pre-publish blocker if the deterministic fixture passes.

## Rollout, Recovery, and Idempotence

The rollout is additive and affects only future self-healing recovery attempts. Existing retained runs are not rewritten, and retry-step's current fail-closed validation remains intact. If the implementation is reverted, artifacts and ledger records keep their existing shape.

The guard is idempotent: re-running postflight after a successful restore observes matching hashes, emits no second `protected_checkpoint_restored`, and does nothing. If preflight cannot read, hash, or de-duplicate a ledger-recorded protected file, recovery should not start and no retry attempt should be prepared. If postflight cannot restore, Scherzo should leave the run failed or parked with `recovery_artifact_restore_failed` and preserve the original backup evidence where available; operators can then choose manual salvage or full retry without Scherzo having appended a misleading successful recovery decision.

## Open Questions and Clarifications Needed

No blocking clarification is needed. The MVP assumes the default filesystem artifact store with `local_path` is the production path to protect; broader store-neutral restoration can be a later artifact-store issue if a non-filesystem backend becomes active.
