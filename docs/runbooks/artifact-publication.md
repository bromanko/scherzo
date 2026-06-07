# Artifact publication operator runbook

Use Scherzo's retained publication ledger to inspect and retry publication work without rerunning the producing workflow.

## Inspect retained publication state

List the latest publication attempt for each configured publication on a run:

    direnv exec . gleam run -m scherzo ctl artifact publication list --run <run-id> --root <workspace-root>

Show the full attempt history for one publication:

    direnv exec . gleam run -m scherzo ctl artifact publication show --run <run-id> --publication <publication-id> --root <workspace-root>

Both commands also accept `--json`.

## Retry publication

Retry one failed retryable publication:

    direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id> --publication <publication-id>

For ExecPlan review docs, use `--publication execplan_review_doc` to replay the
retained canonical plan artifact without rerunning the authoring or revision steps.

Retry every latest failed retryable publication for the run:

    direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id>

When a daemon control file is available, retry goes through the control/daemon operator path. When running against retained local state with `--root`, retry validates that the run still has a retained output manifest and that the current workflow publication config still matches the retained retry target.

## Common retry failures

- `publication_run_not_found`: the run id is unknown in the local ledger.
- `publication_not_found`: the publication id is unknown for that run.
- `publication_not_retryable`: the latest attempt is not a failed retryable attempt.
- `publication_retry_output_manifest_missing`: the run no longer has the retained output manifest required for replay.
- `publication_retry_config_drift`: the current workflow publication route/config no longer matches the retained failed attempt.

## Managed GitHub checkout scope

Current same-repository artifact publication still uses a Scherzo-managed Git checkout under `.scherzo-state/artifact-repositories/github/<hash>`. That checkout is separate from the active workflow workspace on purpose: the active workspace may contain unrelated agent edits, so LIV-908 must not publish artifacts from it.

The bundled `dogfood-jj` workspace driver does advertise `publish-change`, and its configured `publish_remote` and `github_repo` remain relevant evidence when diagnosing publication problems. LIV-908 does not switch artifact publication to that driver capability yet. A future migration must provide a driver-owned clean publication lane or worktree that preserves retained-output retry semantics for `artifact publication retry`; it must not reuse the active dirty workflow workspace.

Live GitHub or browser verification is a deferred human/operator follow-up after implementation. The pre-publish gate for this hardening work remains deterministic tests, retained-manifest inspection, and the required format/lint commands.

## Safe managed-checkout recovery

If a retained publication shows `dirty_checkout` or failed cleanup diagnostics for the managed GitHub checkout:

1. Stop concurrent Scherzo publication for that repository/check-out key.
2. Inspect and save evidence before changing anything:
   - `git -C <managed-checkout> status --porcelain`
   - `git -C <managed-checkout> diff --stat`
   - any relevant retained `cleanup_diagnostics` from `artifact publication show --json`
3. Confirm the path is the Scherzo-owned managed checkout under `.scherzo-state/artifact-repositories/github/<hash>`, not an active workflow workspace. If no publication process is running and a stale sibling lock file named `<managed-checkout>.publication.lock` remains, save its contents with the evidence above and remove only that lock file before retrying.
4. Reset and clean only that managed checkout:
   - `git -C <managed-checkout> reset --hard HEAD`
   - `git -C <managed-checkout> clean -fd`
5. Retry the retained publication with `direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id> --publication <publication-id> --root <workspace-root>`.

Do not run reset/clean against the active agent workspace. If the checkout keeps getting dirty again, collect the saved evidence and open a follow-up instead of broadening manual cleanup.
