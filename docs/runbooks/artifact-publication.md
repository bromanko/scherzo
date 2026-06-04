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
