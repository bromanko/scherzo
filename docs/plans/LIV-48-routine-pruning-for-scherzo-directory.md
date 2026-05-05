# Plan safe routine pruning for `.scherzo`

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Operators running Scherzo need a boring, safe way to understand and reduce the size of the repository-local `.scherzo/` directory. After this plan is implemented, an operator can run a dry-run pruning command from the repository root, see every relevant `.scherzo/` category classified as durable state, retained work, stale output, or disposable cache, and then execute only the low-risk non-workspace deletions that pass explicit guardrails. The most important visible invariant is that active daemon state, intentionally retained workspaces, jj-backed run roots, and useful recovery evidence are not deleted unexpectedly.

The desired operator experience is:

    direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml

The command prints a stable inventory, explains why each path is kept, blocked, manually reviewable, or eligible for automatic pruning, and ends with `No files were deleted.` A later explicit execution mode deletes only `prune automatically` rows that still pass the same safety checks immediately before deletion. In this first implementation, run-root directories below `.scherzo/workspaces/<workflow>/<issue>/<run>/` are report-only and manual-review targets; automatic run-root deletion is deliberately deferred until a separate checked-cleanup and jj-publication-safety design exists.

## Problem Framing and Constraints

Scherzo dogfoods itself by storing workflow configuration, runtime state, jj workspaces, control files, ledgers, step artifacts, and local operator files below `.scherzo/`. The checked-in pieces are useful source files, while the ignored runtime pieces can grow over time. Without a clear policy, operators cannot quickly tell whether a directory is active, retained for recovery, failed and worth debugging, completed and disposable, or just stale output from an old run.

This plan is constrained by safety. The pruning implementation must fail closed: if a run cannot be classified from the ledger, if the daemon may still be alive, if a jj workspace may contain unpublished work, if a `.scherzo-keep-workspace` marker is present, if the latest ledger event for a run is less than 24 hours old, or if a path is outside the configured workspace root, the command must not delete that path. It is acceptable for the first implementation to leave too much behind. It is not acceptable to delete active, intentionally retained, or possibly unrecoverable work.

This plan does not implement pruning now. It specifies the future work, the policy, the tests, and the guardrails so later implementation issues can execute it. The first destructive implementation is intentionally limited to non-jj runtime files such as expired ledger archive segments, stale control discovery data, expired artifacts under `.scherzo-state/artifacts/`, stale population markers, old prune manifests, and stale temporary files under recognized state roots. Whole run roots and files inside run-root workspaces remain `prune manually` until a later plan proves checked cleanup and jj unpublished-work detection.

## Strategy Overview

Implement pruning as a Scherzo maintenance command with dry-run as the default mode, supported by a small pruning policy module, a ledger-derived run index, a crash-tolerant-enough prune manifest, and a documented operator runbook. The command should read `.scherzo/scherzo.yaml`, resolve the configured workspace root, replay the durable ledger, inspect known `.scherzo/` subtrees, and classify paths into five actions: `never prune`, `keep`, `prune manually`, `prune automatically`, and `blocked`.

This is the right size because manual documentation alone would not reliably classify active runs or ledger-backed state, while a scheduled background janitor would be too risky before the policy has been observed in real dogfood runs. A maintenance command gives operators repeatable dry-run evidence first, then guarded execution for non-workspace files when the report is trusted. A doctor check should be added only as a warning surface after the dry-run engine exists; doctor should not delete anything.

The implementation should be additive and staged. First land dry-run inventory, the exact ledger run index, and tests. Second collect at least one real dogfood dry-run report and review it for false positives and false negatives before enabling deletion. Third add destructive execution only for non-workspace automatic candidates. Fourth add doctor warning and documentation. Automatic run-root deletion is not part of this first implementation; it requires a separate follow-up that adds a checked cleanup API and a dedicated jj safety helper before any run root can move from `prune manually` to `prune automatically`.

## Alternatives Considered

The simplest alternative is a runbook that says which `.scherzo/` paths are safe to delete by hand. That is insufficient because active workflow state is recorded in the ledger and control files, and a human-only checklist is likely to miss retained markers or recent run activity. A runbook remains useful as operator documentation, but it should not be the only source of truth.

A shell script under `scripts/` would be quick, but it would duplicate path resolution, ledger replay, artifact reference validation, and path containment behavior that already exists in Gleam modules. It would also be harder to test thoroughly.

A doctor-only check would surface stale state but would not provide the detailed inventory or controlled deletion path operators need. Doctor should warn, not mutate.

An automatic daemon job that periodically deletes old state was rejected for the first version. The daemon is exactly the process whose active state must be protected, and scheduled deletion before repeated dry-run evidence would increase the blast radius. A future scheduled mode can be considered only after the manual command has been safe in dogfood use.

A broader first version that automatically deletes completed run roots was also rejected. Existing workspace cleanup is best-effort around remove hooks, and a clean jj working copy is not proof that changes are published or recoverable. Reporting run roots for manual review still solves the immediate visibility problem while avoiding destructive jj mistakes.

## Risks and Countermeasures

The largest risk is deleting active or retained work. Countermeasures are strict: destructive mode refuses to run while the instance lock exists or the control API is reachable; active ledger statuses block deletion; `.scherzo-keep-workspace` blocks deletion; recent population markers block deletion; recent ledger activity blocks deletion; and unknown run directories are reported as `prune manually`, not deleted.

A second risk is deleting a clean-looking jj workspace that contains unpublished or otherwise unrecoverable work. This plan avoids that risk by not automatically deleting run roots in the first implementation. A later run-root pruning issue must treat unpublished-work detection as a prerequisite, not a nice-to-have follow-up. That later issue must specify a dedicated helper contract, command outcomes that count as safe, and tests proving dirty, unpublished, unknown, hook-failing, and hook-warning cases block deletion.

A third risk is relying on the existing workspace cleanup path for safety it does not provide. Current `src/scherzo/workspace.gleam` runs the `before_remove` hook with `hooks.run_best_effort(...)`, discards the result, and then deletes the target. The first pruning executor must not call `workspace.cleanup_stored_path` or `workspace.cleanup` for automatic deletion. If a later run-root pruning implementation is approved, it must first add a checked cleanup API that runs the hook with fatal semantics, returns hook failures, refuses deletion on hook failure or unsafe warning, and has tests that prove deletion is not called when the hook blocks.

A fourth risk is trusting corrupt or incomplete durable state. If the ledger cannot be replayed from `.scherzo/workspaces/.scherzo-state/ledger/snapshot.json` and `current.jsonl`, the command must refuse to delete runtime state. It may still report obvious checked-in files as `never prune`, but execution should stop before deleting artifacts, archive segments, stale control files, logs, temp files, or markers.

A fifth risk is deleting evidence too soon because the plan uses only a terminal timestamp. The classifier must compute `latest_event_at_ms` for every run from the replayed ledger records, not from filesystem modification time alone. A run whose terminal record is old but whose latest associated ledger record is less than 24 hours old must remain `keep` or `blocked`.

A sixth risk is confusing dry-run output. The report must include paths, reasons, ages, sizes when available, run identifiers when known, and the guard that allows or blocks deletion. It must also include an execution summary that says exactly how many paths would be deleted and that no files were deleted during dry-run.

A seventh risk is promising an audit trail that does not survive partial execution. The prune manifest must be written before deletion begins and updated after each target is attempted, so a crash or mid-run failure leaves evidence of planned, completed, failed, and pending targets. The manifest is evidence, not a content backup.

## Progress

- [x] (2026-05-05 00:00Z) Read the repository-local ExecPlan authoring skill and confirmed this is an authoring task.
- [x] (2026-05-05 00:00Z) Inspected the `.scherzo/` configuration, ignore rules, runtime README, and relevant state/control/workspace source modules.
- [x] (2026-05-05 00:00Z) Drafted this pruning policy and implementation plan.
- [x] (2026-05-05 00:00Z) Incorporated adversarial review findings by deferring automatic run-root deletion, specifying exact ledger run indexing, adding the latest-event guard, and defining manifest update semantics.
- [ ] Create follow-up Linear issues for the implementation slices listed in this plan.
- [ ] Implement the dry-run inventory command and save dogfood dry-run evidence.
- [ ] Implement guarded destructive pruning for non-workspace automatic candidates only.
- [ ] Add the doctor warning and operator documentation.
- [ ] Decide, in a separate follow-up, whether automatic run-root pruning is still desired after checked cleanup and jj safety are designed.

## Surprises & Discoveries

- Observation: `.scherzo/` intentionally mixes checked-in workflow definitions with ignored runtime state.
  Evidence: `.gitignore` ignores `.scherzo/*` but re-includes `.scherzo/README.md`, `.scherzo/scherzo.yaml`, and `.scherzo/workflows/**`.

- Observation: Runtime state for the configured dogfood workspace root is stored below `.scherzo/workspaces/.scherzo-state/`, not directly beside each workflow definition.
  Evidence: `src/scherzo/control/file.gleam` uses `.scherzo/workspaces/.scherzo-state/control.json` as the default discovery path, and `src/scherzo/state/ledger.gleam` stores ledgers under `<workspace-root>/.scherzo-state/ledger/`.

- Observation: The existing workspace cleanup path performs containment checks but does not enforce remove-hook success.
  Evidence: `src/scherzo/workspace.gleam` resolves the root and target, rejects paths outside the workspace root, then calls `hooks.run_best_effort("before_remove", ...)` and proceeds to `simplifile.delete(target_abs)` regardless of hook result.

- Observation: The checked-in jj remove hook forgets jj workspaces but does not prove that work is published or recoverable.
  Evidence: `scripts/scherzo-jj-workspace before-remove` calls `forget_run_workspaces`, which runs `jj workspace forget` for child workspaces when `jj` is available; it does not inspect dirty status, unpublished changes, remote bookmarks, or handoff state.

- Observation: Current workflow terminal outcome strings are concrete strings, not generic success and failure labels.
  Evidence: `src/scherzo/workflow_run.gleam` writes workflow-run outcome `"completed"` for success and `"failed_fatal"` for fatal failures; `src/scherzo/orchestrator/daemon.gleam` can write cancellation outcome `"cancelled"`; `src/scherzo/state/record.gleam` defines `WorkflowRunInterrupted` and `WorkflowRunSuperseded` records separately.

## Decision Log

- Decision: Build pruning as a Scherzo maintenance command with dry-run default, not as a manual-only runbook or scheduled daemon job.
  Rationale: The command can reuse config parsing, ledger replay, and repository-local path handling while keeping deletion operator-initiated and reviewable.
  Date: 2026-05-05

- Decision: Treat active, retained, unknown, corrupt, recently updated, or possibly unpublished work as non-deletable by default.
  Rationale: False negatives leave extra files behind, but false positives can delete unrecoverable work.
  Date: 2026-05-05

- Decision: Defer automatic run-root deletion from the first implementation.
  Rationale: The current cleanup path ignores remove-hook failures, and clean jj status alone does not prove work has been published or handed off. Run roots will be reported as `prune manually` until a later issue adds checked cleanup and jj safety semantics.
  Date: 2026-05-05

- Decision: Do not use `workspace.cleanup_stored_path` or `workspace.cleanup` from pruning execution.
  Rationale: Those functions are appropriate for best-effort workflow cleanup but are not safe enough for a maintenance command that promises guarded deletion. Non-workspace deletion will use narrow contained file deletion helpers; future run-root deletion must introduce a checked cleanup API first.
  Date: 2026-05-05

- Decision: Define pruning run state through a ledger-derived `RunIndex` with exact outcome mapping.
  Rationale: Deletion decisions must not depend on directory names or invented outcome strings. The implementation must classify `"completed"` as successful, `"failed_fatal"` and `"cancelled"` as failed or cancelled terminal outcomes, `WorkflowRunInterrupted` as interrupted, `WorkflowRunSuperseded` as superseded, and unknown terminal outcomes as non-automatic.
  Date: 2026-05-05

- Decision: Require a per-run `latest_event_at_ms` guard before deleting any run-owned state.
  Rationale: Terminal status can be old while later ledger records or artifacts are recent. A 24-hour latest-event guard reduces the chance of deleting evidence from in-flight handoff, delayed writes, or clock skew.
  Date: 2026-05-05

- Decision: Keep the durable ledger current segment and snapshot indefinitely, and prune only old archived ledger segments after conservative retention.
  Rationale: The current segment and snapshot are needed for recovery and active projection loading; archived segments are mostly historical evidence once compaction has succeeded.
  Date: 2026-05-05

- Decision: Write and update a prune manifest during destructive execution.
  Rationale: A pre-delete manifest alone cannot prove what happened after a partial failure. Updating the manifest after each target gives operators an audit trail without promising content restoration.
  Date: 2026-05-05

- Decision: Add doctor integration after the dry-run engine and non-workspace executor exist.
  Rationale: Doctor can reuse the same classification logic to warn about stale state without gaining its own deletion behavior.
  Date: 2026-05-05

## Outcomes & Retrospective

This plan has been authored and revised after adversarial review but not implemented. During implementation, update this section after each milestone with what shipped, what was deferred, whether the dry-run output proved the retention policy safe enough for destructive pruning, and whether automatic run-root deletion remains worth a separate plan.

## Context and Orientation

Scherzo is a daemon-driven workflow runner. In this plan, daemon means the long-running Scherzo process that polls Linear, starts agent workers, writes durable state, and exposes a local control API for `scripts/scherzoctl`. A workspace root is the directory where Scherzo creates runtime jj workspaces. A run root is the directory for one workflow run below the workspace root, normally shaped like `.scherzo/workspaces/<workflow-name>/<issue-identifier>/<run-id>/`. A ledger is the append-only JSON-lines event log that records workflow runs, step attempts, known workspaces, parked issues, retries, commands, and outbox records. An artifact is a JSON file containing a step result, command output, or agent result referenced from the ledger.

A `RunIndex` is the pruning-specific index built from the ledger before classification. It maps a `run_id` to the run root, workflow id, issue id, issue identifier when known, terminal state, latest associated ledger event timestamp, artifact references, active step attempts, and supersession relationship. The pruning code must build this index while replaying ledger records because the existing projection alone does not specify every safety fact this plan needs.

The checked-in dogfood config is `.scherzo/scherzo.yaml`. Its `workspace.root: workspaces` value is resolved relative to `.scherzo/scherzo.yaml`, so this repository's runtime workspace root is `.scherzo/workspaces`. The README documents the intended runtime shape as `.scherzo/workspaces/<workflow-name>/<issue>/<run>/`.

The durable state directory below the workspace root is `.scherzo/workspaces/.scherzo-state/`. Current source files establish these important paths and behaviors:

- `src/scherzo/control/file.gleam` defines `.scherzo-state/control.json` and the default control discovery path `.scherzo/workspaces/.scherzo-state/control.json`.
- `src/scherzo/instance_lock.gleam` acquires `.scherzo-state/instance.lock` below the workspace root and warns that a stale lock must be removed manually only after verifying no Scherzo process is active.
- `src/scherzo/state/ledger.gleam` stores `.scherzo-state/ledger/current.jsonl`, `.scherzo-state/ledger/snapshot.json`, and `.scherzo-state/ledger/archive/`.
- `src/scherzo/state/artifact_store.gleam` stores step artifacts under `.scherzo-state/artifacts/runs/<run-id>/<step-id>/attempt-<n>.json`.
- `src/scherzo/state/record.gleam` defines ledger records for workflow run start, finish, interruption, supersession, step attempt preparation, step attempt finish, known workspaces, parked issues, commands, and outbox.
- `src/scherzo/state/projection.gleam` folds those records into statuses such as active, finished, interrupted, superseded, pending step, running step, and finished step.
- `src/scherzo/workflow_run.gleam` currently writes workflow outcome `"completed"` for a successful workflow run and `"failed_fatal"` for fatal workflow failure.
- `src/scherzo/orchestrator/daemon.gleam` can write cancellation outcome `"cancelled"` when an operator cancellation becomes terminal.
- `src/scherzo/workspace.gleam` contains workspace preparation and cleanup functions. Its current cleanup path is not safe enough for prune execution because remove-hook failures are best-effort.
- `scripts/scherzo-jj-workspace` contains the repository-local jj workspace lifecycle hook. Its `before-remove` command forgets jj workspaces; it does not prove that work is published or recoverable.
- `src/scherzo/doctor.gleam` currently has checks for workflow config, Linear contract, Linear smoke, instance lock, workspace hooks, and pi probe; it has no pruning check yet.

## Preconditions and Verified Facts

The following facts were checked in the current tree and the implementation should re-check them before coding if the repository has changed:

- `.scherzo/README.md` states that checked-in workflows live in `.scherzo/workflows/**`, local workflow variants use `.local.yaml` or `.local.yml`, runtime jj workspaces live under `.scherzo/workspaces/<workflow-name>/`, and `.scherzo/workspaces` is ignored by git.
- `.gitignore` ignores runtime `.scherzo/*` state while explicitly allowing `.scherzo/README.md`, `.scherzo/scherzo.yaml`, and `.scherzo/workflows/**`.
- `.scherzo/scherzo.yaml` configures `workspace.root: workspaces`, uses `scripts/scherzo-jj-workspace` for workspace lifecycle hooks, and exposes operator control through `scripts/scherzoctl`.
- `src/scherzo/control/file.gleam` makes the control file discoverable through `SCHERZO_CONTROL_FILE` or `.scherzo/workspaces/.scherzo-state/control.json`.
- `src/scherzo/instance_lock.gleam` treats an existing instance lock as evidence that another Scherzo process may be running or that a stale lock needs manual verification.
- `src/scherzo/state/ledger.gleam` can replay, load, and compact ledger state using `current.jsonl`, `snapshot.json`, and `archive/`.
- `src/scherzo/state/artifact_store.gleam` validates artifact references so artifacts cannot escape `.scherzo-state/artifacts` through absolute or parent-directory paths.
- `src/scherzo/hooks.gleam` has both fatal `run_hook(...) -> Result(Nil, HookError)` and best-effort `run_best_effort(...) -> String` helpers.
- `src/scherzo/workspace.gleam` has a cleanup function that refuses to delete the workspace root itself and refuses paths outside the workspace root, but it uses `run_best_effort` for `before_remove` and deletes even when the hook fails.
- `scripts/scherzo-jj-workspace before-remove` forgets jj workspaces and does not perform dirty-work, unpublished-work, or remote-handoff checks.
- `test/workspace_test.gleam` exists and is the right place to add tests for any future checked workspace cleanup API.
- `test/doctor_test.gleam` exists and is the right place to add prune doctor behavior tests unless implementation discovers a narrower existing test file.
- `jj status --color=never` reported a clean working copy before the original plan file was created.

## Scope Boundaries

In scope for the first implementation:

- Define a repository-local pruning policy for `.scherzo/`.
- Add a dry-run maintenance command that inventories known `.scherzo/` categories.
- Build an explicit ledger-derived `RunIndex` with exact terminal outcome mapping and latest-event timestamps.
- Add guarded destructive deletion for non-workspace categories explicitly classified as `prune automatically`.
- Write and update prune manifests for destructive execution.
- Add tests that prove active, retained, unknown, locked, corrupt, recent-ledger, and manual-review cases are not deleted.
- Add a doctor warning that reuses the dry-run classifier and points operators to the pruning command.
- Document operator-facing dry-run, execution, manifest, and recovery guidance.

Out of scope for the first implementation:

- Scheduled background pruning by the daemon.
- Automatically deleting `.scherzo-keep-workspace` runs.
- Automatically deleting any `.scherzo/workspaces/<workflow>/<issue>/<run>/` run-root directory.
- Deleting files inside a run root as an automatic action.
- Deleting or rewriting active ledger current state or snapshots.
- Guessing how to recover unpublished jj work.
- Pruning files outside `.scherzo/`.
- Managing jj workspaces from this planning workflow.

A future run-root deletion issue may be created only after dry-run dogfood evidence is reviewed. That future issue must add a checked cleanup path and a jj safety helper before changing run-root classifications from `prune manually` to `prune automatically`.

## Inventory and Pruning Classification

The pruning command should use the following categories. `Never prune` means the command must not delete the path. `Keep` means retain by default unless a narrower rule later marks a child path as automatically prunable. `Prune manually` means show the path and reason but do not delete it. `Prune automatically` means eligible for deletion in `--execute` mode only after all guardrails pass. `Blocked` means the path might otherwise look stale, but a safety guard such as daemon liveness, ledger corruption, retained marker, path containment, or recent activity prevents deletion.

| Path or category | Classification | Default action | Suggested retention | Reason |
| --- | --- | --- | --- | --- |
| `.scherzo/README.md` | Checked-in durable documentation | Never prune | Indefinite | Source-controlled operator documentation. |
| `.scherzo/scherzo.yaml` | Checked-in durable config | Never prune | Indefinite | Required to resolve workspace root and workflows. |
| `.scherzo/workflows/**` | Checked-in workflow definitions and prompts | Never prune | Indefinite | Source-controlled workflow behavior. |
| `.scherzo/scherzo.local.yaml`, `.scherzo/scherzo.local.yml` | Local operator config | Prune manually | Indefinite unless operator removes | May contain machine-specific settings. |
| `.scherzo/workflows/**/*.local.yaml`, `.scherzo/workflows/**/*.local.yml` | Local workflow overrides | Prune manually | Indefinite unless operator removes | Ignored local behavior should not be deleted by a generic janitor. |
| `.scherzo/gh-agent/**` | Local GitHub CLI state for the agent profile | Prune manually | Indefinite unless operator removes | May contain credentials or auth cache. |
| `.scherzo/jj-agent.toml` | Local jj/git identity material generated by agent checks | Prune manually | Indefinite unless operator removes | Local identity config, not disposable cache. |
| `.scherzo/workspaces/.scherzo-state/instance.lock` | Daemon liveness guard | Never prune by command default | Indefinite while present | Existing source says stale locks require manual process verification. |
| `.scherzo/workspaces/.scherzo-state/control.json` | Control API endpoint and token | Keep while reachable or lock exists; prune automatically only when stale and unreachable | 24 hours after failed control probe and no lock | Safe to remove stale discovery data only when no daemon appears alive. |
| `.scherzo/workspaces/.scherzo-state/ledger/current.jsonl` | Durable current ledger segment | Never prune | Indefinite | Needed for recovery and projection loading. |
| `.scherzo/workspaces/.scherzo-state/ledger/snapshot.json` | Durable projection snapshot | Never prune | Indefinite | Needed for fast recovery and compaction correctness. |
| `.scherzo/workspaces/.scherzo-state/ledger/archive/**` | Archived compacted ledger segments | Prune automatically | 180 days, while always keeping the newest 10 archive files | Historical evidence; less critical after snapshot and current replay succeed. |
| `.scherzo/workspaces/.scherzo-state/artifacts/runs/<run-id>/**` | Step artifacts and command outputs | Prune automatically when the owning run is terminal, unretained, not recently updated, and not unknown | 30 days for `completed` or superseded runs; 90 days for `failed_fatal`, `cancelled`, or interrupted runs | Useful for debugging recent runs, but large and reconstructable from retained work or external handoff after retention. |
| `.scherzo/workspaces/.scherzo-state/*.populating` | Workspace population marker | Keep if recent or daemon may be alive; prune automatically only when stale | 24 hours and no live daemon | Recent markers may represent an in-progress workspace creation. |
| `.scherzo/workspaces/.scherzo-state/prune-manifests/**` | Prune audit manifests | Keep, then prune automatically | 365 days | Manifests prove what was planned, attempted, and deleted. |
| `.scherzo/workspaces/<workflow>/<issue>/<run>/` with active ledger status | Active run root | Never prune | Until terminal | Active work must not be disturbed. |
| Any run root containing `.scherzo-keep-workspace` | Intentionally retained workspace | Never prune by default | Indefinite | Explicit operator or workflow marker for recovery. |
| Terminal successful run roots without retain marker | Completed run output | Prune manually in this first implementation | Report after 14 days after terminal ledger record and latest ledger event older than 24 hours | Whole run-root deletion is jj-backed and requires a future checked cleanup and jj safety helper. |
| Superseded run roots without retain marker | Superseded output | Prune manually in this first implementation | Report after 14 days after supersession, only when successor is terminal and latest ledger event is older than 24 hours | Superseded retries may be disposable later, but not until workspace deletion safety exists. |
| Failed, cancelled, or interrupted run roots without retain marker | Debugging evidence | Prune manually in this first implementation | Report after 90 days and latest ledger event older than 24 hours | Failures are useful for recovery and diagnosis, and whole-directory deletion is too risky without jj safety. |
| Run-like directories with no ledger record | Orphan runtime directories | Prune manually | Report after 30 days; do not auto-delete in first version | Unknown provenance should not be guessed. |
| `.scherzo/**/*.tmp`, `.scherzo/**/*.partial`, and abandoned temporary files under known non-run-root state directories | Temporary files | Prune automatically | 24 hours and no live daemon | Disposable partial output, but only under recognized state roots and never inside run roots in the first implementation. |
| `.scherzo/**/*.log` under non-run-root state directories | Runtime logs | Prune automatically | 30 days for normal logs; 90 days if associated with failed or interrupted run state | Logs are useful evidence for recent incidents; logs inside run roots remain manual in the first implementation. |

## Identifying Run State

The classifier should prefer durable ledger state over file names. File names are hints; ledger state is evidence. The implementation must build a `RunIndex` or equivalent structure before making any run-owned deletion decision.

Each `RunIndexEntry` must contain these fields, even if some values are `None` when the ledger cannot provide them:

    run_id: String
    workflow_id: Option(String)
    issue_id: Option(String)
    issue_identifier: Option(String)
    run_root: Option(String)
    terminal_state: Option(TerminalState)
    terminal_outcome: Option(String)
    terminal_at_ms: Option(Int)
    latest_event_at_ms: Option(Int)
    superseded_by_run_id: Option(String)
    successor_terminal_state: Option(TerminalState)
    artifact_refs: List(String)
    has_pending_step: Bool
    has_running_step: Bool
    has_retained_marker: Bool

The `TerminalState` mapping must be exact:

- A `WorkflowRunFinished` record with outcome `"completed"` is a successful terminal workflow run.
- A `WorkflowRunFinished` record with outcome `"failed_fatal"` is a failed terminal workflow run.
- A `WorkflowRunFinished` record with outcome `"cancelled"` is a cancelled terminal workflow run and uses the failed/interrupted retention window.
- A `WorkflowRunInterrupted` record is interrupted and uses the failed/interrupted retention window.
- A `WorkflowRunSuperseded` record is superseded and is eligible for artifact pruning only when `superseded_by_run_id` points to a successor that is also terminal.
- Any unrecognized `WorkflowRunFinished.outcome` value is not automatically prunable. The classifier should return `Blocked` or `PruneManually` with reason `unknown_terminal_outcome`.

A run is active if the projection contains `WorkflowRunActive` for its run id, if any step attempt for the run is pending or running, or if the live control API reports an operator session or worker for that run. Active run roots are `never prune`, and artifacts for active runs are `keep` or `blocked`.

A run is completed if the index contains `terminal_state: Completed` from outcome `"completed"` and no later active, interrupted, or superseded state. Completed artifacts are eligible after 30 days when no retain marker exists and `latest_event_at_ms` is older than 24 hours. The completed run root itself is only `prune manually` in this first implementation.

A run is failed, cancelled, or interrupted if the index contains outcome `"failed_fatal"`, outcome `"cancelled"`, or a `WorkflowRunInterrupted` record. These run-owned artifacts are retained for 90 days by default. If a `.scherzo-keep-workspace` marker exists anywhere in the run root, the run becomes intentionally retained and its artifacts and run root are not automatically deleted by default.

A run is superseded if the index contains `WorkflowRunSuperseded`. Superseded artifacts are eligible after 30 days only when the successor run is also terminal and the latest event for the superseded run is older than 24 hours. If the successor is still active or unknown, keep the superseded run-owned state.

A run is stale if it has no active ledger state, no retain marker, no recent file modification, and is older than its retention window. Stale is not enough by itself for deletion. The run must still be classified into a known terminal state. If no ledger record maps to the directory, classify it as an orphan and report it for manual review.

The implementation must compute `latest_event_at_ms` by scanning the replayed ledger records used to build the projection. For every record that explicitly names a `run_id`, update that run's latest event timestamp to the record's ledger timestamp. This includes workflow run records, step attempt records, run interruption and supersession records, known workspace records that can be mapped to a run root, and artifact-producing step finish records. If the ledger loader cannot expose record timestamps, add a small helper in the ledger-reading path to return timestamped records for pruning. If the implementation cannot compute `latest_event_at_ms` for a run-owned candidate, it must block automatic deletion with reason `latest_event_unknown`.

## Safety Workflow and Guardrails

Dry-run is the default and must not mutate the filesystem. It should be safe to run while the daemon is live, but the report should show active and locked state as blocked.

Execution mode must repeat all discovery and checks immediately before deletion. It must refuse to execute if `.scherzo/workspaces/.scherzo-state/instance.lock` exists. If `control.json` exists and a control probe succeeds, execution must refuse because the daemon is live. If `control.json` exists but the probe fails and no lock exists, only the stale control file itself may be considered for deletion after its retention window.

Ledger replay is mandatory before deleting runtime state. If `src/scherzo/state/ledger.gleam` returns corrupt, unsupported, or I/O errors, execution must stop before deleting artifacts, archive segments, stale control files, logs, temp files, markers, or manifests. Dry-run should render the error and classify runtime candidates as blocked.

Every deletion target must be normalized relative to `.scherzo/` or the configured workspace root. The pruning code must reject paths that escape those roots, use parent-directory traversal, are absolute input paths from a report, or are the workspace root itself. The report may display repository-relative paths only.

The first pruning executor must not delete whole run roots and must not delete files inside run roots. It also must not call `workspace.cleanup_stored_path` or `workspace.cleanup`, because those functions currently treat `before_remove` as best-effort. Non-workspace deletion should use narrow helpers that delete a single contained file or a contained artifact directory after classification has tied that directory to a known terminal run.

Before any destructive deletion, the executor must create a planned manifest under `.scherzo/workspaces/.scherzo-state/prune-manifests/`. The manifest protocol is:

1. Write a manifest file with status `planned` before deleting any target. It must include command options, current time, Scherzo config path, manifest version, every target path, classification, reason, retention rule, ledger run id if known, size estimate if known, and an initial per-target status of `pending`.
2. Flush or close the manifest file before the first deletion call.
3. After each target is attempted, update the same manifest or append a manifest event so the target records `deleted`, `skipped`, or `failed`, with an error reason when relevant.
4. If execution stops after a failure, leave remaining targets as `pending` and mark the manifest status `partial_failure` when possible.
5. If all targets finish, mark the manifest status `completed`.
6. Re-running `--execute` must create a new manifest and treat missing already-deleted targets as `skipped` only after re-running discovery and containment checks.

Recent ledger records should block deletion even if a terminal status appears. The first version requires the latest ledger event for a run to be older than 24 hours before deleting any run-owned artifact directory. This catches clock skew, in-flight handoff, and late artifact writes.

Future automatic run-root deletion must not be added by small edits to the executor. It needs a separate design update that specifies a checked cleanup API and jj safety helper. The checked cleanup API must run the remove hook with `hooks.run_hook`, return hook errors, and refuse deletion on hook failure or unsafe warning. The jj safety helper must be able to return at least `Safe`, `Dirty`, `Unpublished`, `Unknown`, and `CommandFailed`; only `Safe` may allow run-root deletion.

## Operator-Facing Dry-Run Output

Human dry-run output should be concise but complete. It should group rows by action and include reason codes stable enough for tests and support docs. A representative transcript is:

    Scherzo prune dry run
    Config: .scherzo/scherzo.yaml
    Workspace root: .scherzo/workspaces

    Summary:
      never prune: 4 paths
      keep: 3 paths
      prune manually: 2 paths
      prune automatically: 2 paths
      blocked: 1 path
      bytes eligible: 125000000

    NEVER  .scherzo/scherzo.yaml
           reason=checked_in_config

    KEEP   .scherzo/workspaces/.scherzo-state/ledger/current.jsonl
           reason=durable_current_ledger

    BLOCK  .scherzo/workspaces/workflow-name/ISSUE-1/run-id
           reason=retained_marker marker=.scherzo-keep-workspace

    PRUNE  .scherzo/workspaces/.scherzo-state/ledger/archive/segment-name.jsonl
           reason=ledger_archive_expired age_days=220 retention_days=180

    MANUAL .scherzo/workspaces/workflow-name/ISSUE-2/run-id
           reason=run_root_manual_review terminal_outcome=completed age_days=45

    No files were deleted. Re-run with --execute to delete PRUNE rows that still pass guard checks.

A JSON format should also be available for dogfood reports and future automation:

    direnv exec . gleam run -- prune --dry-run --format json .scherzo/scherzo.yaml

The JSON must contain the same action, path, reason, age, size, run id, issue identifier, latest event timestamp, terminal outcome, and guard fields as the human report. JSON path fields must be repository-relative display paths, not absolute local paths.

## Retention Windows

The default retention policy should be named `default` and hard-coded in the first implementation. Configuration can be added later only if real dogfood use proves the defaults wrong.

Use these initial windows:

- Stale control file: 24 hours, only when no lock exists and the control probe fails.
- Population markers and temporary partial files under non-run-root state directories: 24 hours, only when no daemon appears live.
- Completed successful run roots: report for manual review after 14 days after terminal ledger record and latest ledger event older than 24 hours; do not auto-delete in the first implementation.
- Superseded run roots: report for manual review after 14 days after supersession, only when the successor is terminal and latest ledger event is older than 24 hours; do not auto-delete in the first implementation.
- Failed, cancelled, and interrupted run roots: report for manual review after 90 days after terminal or interrupted record and latest ledger event older than 24 hours; do not auto-delete in the first implementation.
- Successful and superseded artifacts: 30 days after owning run terminal state, only when the latest event for that run is older than 24 hours and no retain marker exists.
- Failed, cancelled, and interrupted artifacts: 90 days after terminal or interrupted record, only when the latest event for that run is older than 24 hours and no retain marker exists.
- Runtime logs under non-run-root state directories: 30 days for normal logs, 90 days if associated with a failed, cancelled, or interrupted run.
- Ledger archive segments: 180 days, always keeping the newest 10 archive files regardless of age.
- Prune manifests: 365 days.
- Current ledger, snapshot, instance lock, active state, retained workspaces, run roots, checked-in config, local credentials, and local overrides: indefinite for automatic pruning in the first implementation.

## Milestones

Milestone 1 adds dry-run policy, ledger indexing, discovery, and reporting. At the end, `direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml` exists, prints an inventory, and deletes nothing. This milestone retires the largest design risk because operators can compare the report against actual `.scherzo/` state before any destructive behavior exists.

Milestone 2 is a dogfood observation gate. At the end, at least one real dry-run report from a dogfood repository has been saved in a Linear comment or PR note, reviewed for false positives and false negatives, and summarized in this plan's Outcomes & Retrospective section. No destructive code should be merged before this gate is complete.

Milestone 3 adds guarded execution for non-workspace automatic categories only. At the end, `--execute` deletes expired ledger archive files, stale control discovery data, expired artifacts, old prune manifests, and stale non-run-root temp or log files only after re-running discovery, refusing live daemon state, checking ledger health, checking path containment, and writing a manifest. Run roots remain manual-review rows.

Milestone 4 adds doctor and documentation. At the end, `direnv exec . gleam run -- doctor --check prune .scherzo/scherzo.yaml` warns when expired automatic candidates exist and points to the dry-run command. `.scherzo/README.md` or a new runbook documents the retention policy, manifest behavior, manual run-root review, and recovery guidance.

Milestone 5 is a future decision milestone for run-root pruning. The team reviews dogfood evidence and decides whether automatic run-root deletion is still worth pursuing. If yes, create a separate implementation plan that adds checked cleanup, jj safety, and tests before any run root can become an automatic deletion target.

## Plan of Work

Add a new pruning module family under `src/scherzo/prune/`. Keep policy, run indexing, discovery, report rendering, manifest handling, and execution separate so tests can exercise them without deleting real files.

In `src/scherzo/prune/types.gleam`, define shared types: `PruneAction`, `RunClassification`, `TerminalState`, `CandidateKind`, `Candidate`, `RunIndexEntry`, `PruneDecision`, `PrunePlan`, `PruneError`, `Options`, `ReportFormat`, `Manifest`, `ManifestTarget`, `ManifestTargetStatus`, and `ExecutionReport`. Keep these types boring and explicit; do not hide safety-relevant fields in unstructured strings.

In `src/scherzo/prune/policy.gleam`, define retention constants, action types if not in `types.gleam`, reason codes, and pure functions that classify a candidate from metadata and a `RunIndexEntry`. This file should not touch the filesystem. It must classify all run roots as `PruneManually`, `NeverPrune`, or `Blocked`; no run root may return `PruneAutomatically` in this first implementation.

In `src/scherzo/prune/run_index.gleam`, build the run index from replayed ledger records and projection state. This module owns exact outcome mapping, active-step detection, supersession successor checks, artifact reference collection, retained-marker annotation, and `latest_event_at_ms` calculation. If existing ledger APIs do not expose timestamped records, add the smallest helper needed in `src/scherzo/state/ledger.gleam` and test it through pruning fixtures.

In `src/scherzo/prune/discovery.gleam`, read `.scherzo/scherzo.yaml`, resolve the workspace root, load the ledger projection and timestamped records, scan known `.scherzo/` paths, annotate run-index entries with retained-marker facts, and produce candidates. Discovery should record errors as blocked decisions instead of panicking.

In `src/scherzo/prune/report.gleam`, render human and JSON reports. Keep reason codes stable and include a dry-run footer that explicitly says no files were deleted.

In `src/scherzo/prune/manifest.gleam`, implement manifest creation and per-target updates. The module should expose functions to write the planned manifest before deletion and to update one target result at a time.

In `src/scherzo/prune/executor.gleam`, implement execution for non-workspace automatic candidates. It should accept dependencies for filesystem deletion, manifest writing, control probing, ledger loading, and clock access so tests can fake failures. The default executor must re-run discovery before deleting and must reject any run-root candidate even if a bug classified it as automatic.

In `src/scherzo/main.gleam`, add a `prune` command with this interface:

    gleam run -- prune [--dry-run] [--execute] [--format human|json] .scherzo/scherzo.yaml

Dry-run should be the default if neither `--dry-run` nor `--execute` is present. Passing both flags should be a usage error.

In `src/scherzo/doctor.gleam`, add a `Prune` check after the pruning classifier exists. The check should warn when automatic candidates exist, fail only when state corruption prevents safe classification, and never delete files.

In `.scherzo/README.md` or a new `docs/runbooks/scherzo-pruning.md`, document the command, the classifications, retention windows, stale lock guidance, retained workspace behavior, run-root manual review, manifest interpretation, and recovery expectations. The implementation issue should choose one documentation location and keep `.scherzo/README.md` as the quick-entry pointer if a separate runbook is added.

## Concrete Steps

1. From the repository root, run `jj status --color=never` and confirm either a clean tree or only intentional implementation changes. Do not manage jj workspaces.

2. Add `src/scherzo/prune/types.gleam` with the shared pruning types named in the Plan of Work. Include `CandidateKind` variants for checked-in files, local config, control file, ledger current, ledger snapshot, ledger archive, artifact directory, population marker, prune manifest, run root, temp file, log file, and orphan path.

3. Add `test/prune_policy_test.gleam` with a first failing test that classifies `.scherzo/scherzo.yaml` as `NeverPrune` with reason `checked_in_config`.

4. Add `src/scherzo/prune/policy.gleam` with the minimal pure classifier needed for checked-in config and workflow files. Run `direnv exec . gleam test` and expect the new checked-in-file test to pass.

5. Extend `test/prune_policy_test.gleam` with tests for active runs, retained markers, terminal successful artifacts, failed artifacts, stale control files, expired archive segments, orphan directories, and run roots. Use exact outcome strings `"completed"`, `"failed_fatal"`, and `"cancelled"`. Assert every run-root candidate returns `PruneManually`, `NeverPrune`, or `Blocked`, never `PruneAutomatically`.

6. Extend `src/scherzo/prune/policy.gleam` until those policy tests pass. Include reason codes `active_workflow_run`, `retained_marker`, `run_root_manual_review`, `unknown_terminal_outcome`, `latest_event_recent`, `latest_event_unknown`, `artifact_expired`, `ledger_archive_expired`, `stale_control_file`, and `orphan_run_manual_review`.

7. Add `src/scherzo/prune/run_index.gleam` with a pure builder that accepts replayed timestamped records and returns entries keyed by run id. Include fields for terminal state, terminal outcome, terminal timestamp, latest event timestamp, artifact refs, active step flags, superseded successor, and issue identifier.

8. Add `test/prune_run_index_test.gleam`. Create records with the real constructors from `src/scherzo/state/record.gleam` where possible. Assert that outcome `"completed"` maps to successful, `"failed_fatal"` maps to failed, `"cancelled"` maps to cancelled, unknown outcomes map to unknown terminal, `WorkflowRunInterrupted` maps to interrupted, and `WorkflowRunSuperseded` records capture `superseded_by_run_id`.

9. In `test/prune_run_index_test.gleam`, add a latest-event guard test where a run has an old `WorkflowRunFinished` record and a later associated step or artifact record within 24 hours. Assert the run index records the later `latest_event_at_ms`.

10. Run `direnv exec . gleam test` and expect the policy and run-index tests to pass.

11. Add `src/scherzo/prune/discovery.gleam` with config loading, workspace root resolution, ledger loading, retained-marker scanning, and candidate creation. Use fixtures under `test/tmp/prune/` in tests; do not scan the developer's real `.scherzo/` tree from tests.

12. Add `test/prune_discovery_test.gleam` to create fixture directories and ledger records. Assert that discovery maps ledger run ids to run roots and artifacts, treats missing ledger data as manual review, reports ledger corruption as a blocker, and annotates `.scherzo-keep-workspace` markers.

13. In `test/prune_discovery_test.gleam`, add a path containment test with a candidate that tries to escape the fixture `.scherzo/` root through parent-directory traversal. Assert discovery or policy returns `Blocked` with a path-containment reason and no deletion candidate.

14. Add `src/scherzo/prune/report.gleam` and `test/prune_report_test.gleam`. Assert that human output contains stable action labels `NEVER`, `KEEP`, `MANUAL`, `PRUNE`, and `BLOCK`, and contains `No files were deleted.` for dry-run.

15. Extend `test/prune_report_test.gleam` to assert JSON output includes action, path, reason, guards, run id, issue identifier, terminal outcome, and latest event timestamp. Assert JSON path fields are repository-relative display paths.

16. Wire the dry-run command in `src/scherzo/main.gleam`. Add command parsing tests in the existing CLI test area, or create `test/prune_cli_test.gleam` if the current CLI tests are not a good fit. Assert dry-run is default and `--dry-run --execute` is a usage error.

17. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Commit the dry-run milestone after both pass.

18. Run `direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml` in a real dogfood repository. Save the output in a Linear comment or PR note. Review whether any row marked `PRUNE` is a false positive and whether any large stale path is merely missing from inventory. Record the result in this plan's Outcomes & Retrospective section before starting destructive execution.

19. Add `src/scherzo/prune/manifest.gleam` with manifest types and functions to create a planned manifest and update individual target statuses. Write `test/prune_manifest_test.gleam` first and assert a planned manifest contains all targets as `pending` before deletion.

20. Extend `test/prune_manifest_test.gleam` with a simulated partial execution. Assert that after one target succeeds and the second target fails, the manifest records the first as `deleted`, the second as `failed`, remaining targets as `pending`, and overall status as `partial_failure`.

21. Add `src/scherzo/prune/executor.gleam` with dependency injection for filesystem deletion, manifest writing, control probing, ledger loading, discovery, and clock access. The executor must re-run discovery before deleting and must compare current automatic candidates with the operator-visible plan.

22. Add `test/prune_executor_test.gleam`. Test that dry-run performs no deletion, execution deletes only automatic non-workspace candidates, execution refuses when an instance lock exists, execution refuses when control probe succeeds, execution refuses when ledger replay fails, and execution rejects every run-root candidate even if a fake plan marks it automatic.

23. In `test/prune_executor_test.gleam`, add a changed-candidate test where the dry-run saw one target but execute discovery returns a different path, reason, or guard set. Assert execution refuses or skips the changed target and does not call delete for it.

24. In `test/prune_executor_test.gleam`, add a manifest ordering test. The fake manifest dependency should record calls and assert the planned manifest is written before the first delete call, and that a target result update is written after each attempted deletion.

25. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Commit the guarded non-workspace execution milestone after both pass.

26. Add a prune check to `src/scherzo/doctor.gleam`, update doctor parsing and report tests in `test/doctor_test.gleam`, and make the check warn rather than delete.

27. Add the operator documentation in `.scherzo/README.md` or `docs/runbooks/scherzo-pruning.md`, including dry-run, execute, stale lock, retained workspace, run-root manual review, manifest, and recovery sections.

28. Run `direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml` and verify the report classifies checked-in `.scherzo` files as `NEVER`, durable ledger files as `KEEP` or `NEVER`, run roots as `MANUAL`, retained runtime work as `BLOCK` or `NEVER`, and only non-workspace expired state as `PRUNE`.

29. Run `direnv exec . gleam run -- doctor --check prune .scherzo/scherzo.yaml` and verify it warns when stale candidates exist and passes or skips cleanly when none exist.

30. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Commit the documentation and doctor milestone after both pass.

31. If stakeholders later request automatic run-root deletion, create a separate ExecPlan. That plan must start by adding tests in `test/workspace_test.gleam` proving a checked cleanup API refuses deletion when a remove hook fails, and tests proving a jj safety helper blocks `Dirty`, `Unpublished`, `Unknown`, and `CommandFailed` states.

## Testing and Falsifiability

The implementation is falsified if any test can make active, retained, unknown, recently updated, or run-root work appear in the automatic deletion set for the first implementation.

`test/prune_policy_test.gleam` should include these exact scenarios:

- A candidate for `.scherzo/scherzo.yaml` returns `NeverPrune` with reason `checked_in_config`.
- A candidate under `.scherzo/workflows/prompts/example.md` returns `NeverPrune` with reason `checked_in_workflow`.
- A run with `WorkflowRunActive` returns `NeverPrune` or `Blocked` with reason `active_workflow_run`.
- An artifact for a terminal successful run with outcome `"completed"`, older than 30 days, without a retain marker, and with latest ledger event older than 24 hours returns `PruneAutomatically`.
- An artifact for a terminal successful run newer than 30 days returns `Keep`.
- An artifact for a failed run with outcome `"failed_fatal"`, older than 90 days, without a retain marker, and with latest ledger event older than 24 hours returns `PruneAutomatically`.
- An artifact for a cancelled run with outcome `"cancelled"` uses the failed/interrupted retention window.
- Any run with `.scherzo-keep-workspace` returns `NeverPrune` or `Blocked` with reason `retained_marker` for run root and run-owned artifacts.
- A run root for a completed, superseded, failed, cancelled, or interrupted run returns `PruneManually` with reason `run_root_manual_review`, not `PruneAutomatically`.
- Any unrecognized terminal outcome returns `Blocked` or `PruneManually` with reason `unknown_terminal_outcome`.
- A terminal record that is older than the retention window but has `latest_event_at_ms` within 24 hours returns `Keep` or `Blocked` with reason `latest_event_recent`.
- A run-owned candidate with missing `latest_event_at_ms` returns `Blocked` with reason `latest_event_unknown`.
- A ledger archive older than 180 days returns `PruneAutomatically` unless it is one of the newest 10 archive files.
- A stale control file returns `PruneAutomatically` only when the control probe failed and no lock exists.

`test/prune_run_index_test.gleam` should use real ledger record constructors where possible. It should assert the exact outcome mapping, active step detection, artifact reference collection, supersession successor relationship, and `latest_event_at_ms` calculation. Include a test where a terminal record is old enough but a later associated record is within 24 hours, and assert the policy blocks the artifact.

`test/prune_discovery_test.gleam` should build fixture trees under `test/tmp/prune/` and write minimal ledger records using the existing record encoder where possible. It should assert that discovery maps ledger run ids to run roots and artifacts, treats missing ledger data as manual review, reports ledger corruption as a blocker, marks run roots with `.scherzo-keep-workspace` as retained, and never returns absolute display paths.

`test/prune_report_test.gleam` should assert stable human and JSON output. The human output must include the dry-run no-deletion footer. The JSON output must be deterministic enough for future dogfood automation and must include latest event timestamps and terminal outcomes for run-owned candidates.

`test/prune_manifest_test.gleam` should assert that a manifest is written before deletion and updated after each target. It should simulate a mid-run failure and assert completed, failed, and pending target evidence remains available.

`test/prune_executor_test.gleam` should use fake filesystem dependencies where possible. It should assert that execution re-runs discovery, refuses when candidates changed between dry-run and execute, writes a manifest before deletion, updates the manifest after every target, refuses when daemon liveness is possible, refuses on ledger corruption, and never calls delete for `NeverPrune`, `Keep`, `PruneManually`, `Blocked`, or run-root decisions.

Future run-root deletion tests are intentionally out of scope for this first implementation, but the follow-up must include them before enabling that behavior. Required future tests include hook failure preventing deletion, hook warning preventing deletion, dirty jj workspace preventing deletion, clean-but-unpublished jj work preventing deletion, unknown jj state preventing deletion, and only a proven `Safe` jj state allowing checked cleanup.

Doctor tests should verify that the prune doctor check warns for expired automatic candidates, fails for corrupt state only when classification cannot be trusted, and never mutates fixture files.

For integration validation, from the repository root run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml

Expected result: formatting succeeds, all tests pass, the prune command exits successfully in dry-run mode, run roots are not automatic deletion candidates, and the final line says no files were deleted.

## Validation and Acceptance

The implementation is accepted when all of these are true:

- The dry-run command inventories checked-in `.scherzo` files, local overrides, workspace state, control files, lock files, ledger files, artifacts, run roots, retained markers, logs, temporary files, and prune manifests.
- Every inventory row is classified as `never prune`, `keep`, `prune manually`, `prune automatically`, or `blocked`, with a stable reason code.
- The classifier builds a ledger-derived run index containing exact terminal outcome mapping and `latest_event_at_ms` for run-owned candidates.
- Active workflow runs identified from ledger projection or live control state are never automatic deletion candidates.
- Run roots containing `.scherzo-keep-workspace` are never automatic deletion candidates.
- No run root is an automatic deletion candidate in the first implementation.
- Destructive execution requires `--execute`, repeats discovery, writes and updates a manifest, and deletes only non-workspace candidates that remain automatic after all guards pass.
- Execution refuses when the daemon may be live, when an instance lock exists, when ledger replay fails, when latest run-owned ledger activity is recent or unknown, or when a path is outside the allowed root.
- The doctor prune check warns but never deletes.
- Documentation tells operators how to dry-run, execute, interpret classifications, handle stale locks, preserve retained work, manually review run roots, read manifests, and recover from mistaken deletion as far as possible.

Run the validation commands:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- prune --dry-run .scherzo/scherzo.yaml
    direnv exec . gleam run -- doctor --check prune .scherzo/scherzo.yaml

Expect the format check and tests to pass. Expect the dry-run command to print an inventory and no-deletion footer. Expect run roots to appear only as `NEVER`, `KEEP`, `MANUAL`, or `BLOCK`, never `PRUNE`. Expect doctor to pass, skip, or warn based on local stale candidates, but not to delete anything.

## Rollout, Recovery, and Idempotence

Roll out in four stages. First merge dry-run only. Run it on dogfood repositories and inspect reports in PR comments or Linear notes. Second merge `--execute` only for conservative non-workspace automatic categories after that dry-run evidence has been reviewed. Third add doctor warnings and documentation. Fourth decide separately whether automatic run-root deletion is worth a checked-cleanup and jj-safety implementation. Do not add scheduled deletion until dry-run and manual execution have been boring across multiple real runs.

Dry-run is idempotent and safe to repeat. Execute is also idempotent for successfully deleted non-workspace candidates: a later dry-run should either omit those paths or report that no expired candidates remain.

Recovery is intentionally conservative. The command writes manifests but does not promise to restore deleted content. Operators who want a content backup before execution should copy candidate paths before running `--execute`. If an expired artifact is deleted and later needed, recovery is through retained run roots, pushed branches, pull requests, Linear comments, or re-running the workflow. If a retained, active, or run-root workspace would be needed for recovery, the guardrails and first-version scope should have prevented automatic deletion in the first place.

Stale locks require manual recovery. If the command reports `.scherzo/workspaces/.scherzo-state/instance.lock`, the operator must verify that no Scherzo daemon is active before removing the lock. The prune command should not remove that file by default.

If execution fails halfway, the manifest records which targets were planned, which were attempted, which succeeded, which failed, and which remained pending. Re-running `--execute` should create a new manifest, skip already-deleted paths only after re-running discovery, re-check guards for remaining paths, and continue only where safe.

## Artifacts and Notes

Repository facts used while drafting and revising:

    jj status --color=never
    # The working copy had no changes before this plan file was created.

    .scherzo/scherzo.yaml
    # workspace.root: workspaces

    src/scherzo/control/file.gleam
    # default discovery path: .scherzo/workspaces/.scherzo-state/control.json

    src/scherzo/state/ledger.gleam
    # ledger files: current.jsonl, snapshot.json, archive/

    src/scherzo/state/artifact_store.gleam
    # artifact root: .scherzo-state/artifacts/runs/<run-id>/...

    src/scherzo/workspace.gleam
    # cleanup uses run_best_effort for before_remove, so pruning must not rely on it for guarded deletion.

    src/scherzo/workflow_run.gleam
    # workflow outcomes observed in source include "completed" and "failed_fatal".

    src/scherzo/orchestrator/daemon.gleam
    # operator cancellation can produce workflow outcome "cancelled".

## Interfaces and Dependencies

The pruning implementation should depend on existing repository modules before adding new abstractions. Reuse config loading, path helpers, ledger replay, projection types, control file discovery, artifact reference validation, and existing filesystem helpers where they are safe. Do not reuse best-effort workspace cleanup for destructive pruning.

In `src/scherzo/prune/types.gleam`, define types equivalent to:

    pub type PruneAction {
      NeverPrune
      Keep
      PruneManually
      PruneAutomatically
      Blocked
    }

    pub type TerminalState {
      Completed
      FailedFatal
      Cancelled
      Interrupted
      Superseded
      UnknownTerminal
    }

    pub type RunClassification {
      ActiveRun
      RetainedRun
      CompletedRun
      FailedRun
      CancelledRun
      InterruptedRun
      SupersededRun
      OrphanRun
      UnknownRun
    }

    pub type CandidateKind {
      CheckedInConfig
      CheckedInWorkflow
      LocalConfig
      ControlFile
      InstanceLock
      LedgerCurrent
      LedgerSnapshot
      LedgerArchive
      ArtifactDirectory
      PopulationMarker
      PruneManifestFile
      RunRoot
      TempFile
      LogFile
      OrphanPath
    }

    pub type RunIndexEntry {
      RunIndexEntry(
        run_id: String,
        workflow_id: Option(String),
        issue_id: Option(String),
        issue_identifier: Option(String),
        run_root: Option(String),
        terminal_state: Option(TerminalState),
        terminal_outcome: Option(String),
        terminal_at_ms: Option(Int),
        latest_event_at_ms: Option(Int),
        superseded_by_run_id: Option(String),
        successor_terminal_state: Option(TerminalState),
        artifact_refs: List(String),
        has_pending_step: Bool,
        has_running_step: Bool,
        has_retained_marker: Bool,
      )
    }

    pub type PruneDecision {
      PruneDecision(
        path: String,
        kind: CandidateKind,
        action: PruneAction,
        reason: String,
        age_ms: Option(Int),
        bytes: Option(Int),
        run_id: Option(String),
        issue_identifier: Option(String),
        terminal_outcome: Option(String),
        latest_event_at_ms: Option(Int),
        guards: List(String),
      )
    }

In `src/scherzo/prune/discovery.gleam`, expose:

    pub fn dry_run(config_path: String, options: Options, now_ms: Int) -> Result(PrunePlan, PruneError)

In `src/scherzo/prune/executor.gleam`, expose:

    pub fn execute(config_path: String, options: Options, now_ms: Int) -> Result(ExecutionReport, PruneError)

In `src/scherzo/prune/manifest.gleam`, expose functions equivalent to:

    pub fn write_planned(path: String, manifest: Manifest) -> Result(Nil, PruneError)
    pub fn update_target(path: String, target_path: String, status: ManifestTargetStatus) -> Result(Nil, PruneError)
    pub fn finalize(path: String, status: ManifestStatus) -> Result(Nil, PruneError)

The command-line interface should be:

    gleam run -- prune [--dry-run] [--execute] [--format human|json] .scherzo/scherzo.yaml

Do not add new package dependencies unless a repository-local filesystem feature is impossible with existing modules. If sizes are expensive to compute, make size optional in the first version rather than adding a dependency.

A later automatic run-root deletion plan must add an interface equivalent to `workspace.checked_cleanup_stored_path(...) -> Result(Nil, CleanupBlocked)` or a prune-local checked cleanup function with the same fatal semantics. That later interface is not required for the first implementation because this plan does not automatically delete run roots.

## Follow-up Linear Issues

Split implementation into follow-up issues so review stays focused:

- Implement dry-run `.scherzo` prune inventory, exact run indexing, and report. This includes `src/scherzo/prune/types.gleam`, `src/scherzo/prune/policy.gleam`, `src/scherzo/prune/run_index.gleam`, `src/scherzo/prune/discovery.gleam`, `src/scherzo/prune/report.gleam`, CLI parsing, and dry-run tests. Acceptance must include saved dry-run output from at least one real dogfood repository reviewed for false positives and false negatives.
- Implement guarded non-workspace prune execution. This includes deletion guards, manifest writing and per-target updates, executor tests, and explicit rejection of run-root deletion.
- Add prune doctor check and operator documentation. This includes `doctor --check prune`, docs, stale-lock guidance, manifest guidance, and manual run-root review guidance.
- Design automatic run-root pruning only if stakeholders still want it after dogfood evidence. This must be a separate safety issue or ExecPlan that adds checked cleanup, remove-hook failure semantics, jj unpublished-work detection, and tests before enabling deletion.
- Evaluate scheduled pruning only after manual dry-run and execution have been observed safely in dogfood use.

## Open Questions and Clarifications Needed

- [CLARIFY] Confirm whether the proposed default retention windows are acceptable after the first real dry-run report shows actual `.scherzo/` volume. Until then, prefer the conservative defaults in this plan.
- [CLARIFY] Confirm whether automatic run-root deletion is still desired after operators have dry-run evidence. If yes, it must be implemented as a separate safety-focused plan with checked cleanup and jj unpublished-work detection; it is not part of this first implementation.
