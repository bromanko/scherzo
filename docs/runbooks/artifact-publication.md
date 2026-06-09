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

Dogfood ExecPlan review documents are retained as workflow artifacts, not
published to GitHub through `execplan_review_doc` single-file routes.

Retry every latest failed retryable publication for the run:

    direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id>

When a daemon control file is available, retry goes through the control/daemon operator path. When running against retained local state with `--root`, retry validates that the run still has a retained output manifest and that the current workflow publication config still matches the retained retry target.

## Common retry failures

- `publication_run_not_found`: the run id is unknown in the local ledger.
- `publication_not_found`: the publication id is unknown for that run.
- `publication_not_retryable`: the latest attempt is not a failed retryable attempt.
- `publication_retry_output_manifest_missing`: the run no longer has the retained output manifest required for replay.
- `publication_retry_config_drift`: the current workflow publication route/config no longer matches the retained failed attempt.

## GitHub publication scope

Current dogfood GitHub publication of source changes uses declarative
`mode: commit_stack` routes and the retained workflow workspace. Scherzo core
invokes a workspace driver that advertises `publish-commit-stack`; it must not
fall back to workflow-local `publish-change`, direct `gh pr create` helpers, or
managed checkouts under `.scherzo-state/artifact-repositories/github/<hash>`.

When diagnosing a failed commit-stack publication, inspect:

1. the retained publication manifest from `artifact publication show --json`,
2. the retained workflow workspace status through its workspace driver,
3. the selected `commit_stack` artifact and head/base metadata, and
4. the driver failure diagnostics, with secrets redacted.

Live GitHub or browser verification remains a deferred human/operator follow-up
after implementation. The pre-publish gate remains deterministic tests,
retained-manifest inspection, and the required format/lint commands.
