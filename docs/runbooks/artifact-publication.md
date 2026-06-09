# Artifact publication operator runbook

Use Scherzo's retained publication ledger to inspect and retry driver-backed publication work without rerunning the producing workflow.

## Inspect retained publication state

List the latest publication attempt for each configured publication on a run:

    direnv exec . gleam run -m scherzo ctl artifact publication list --run <run-id> --root <workspace-root>

Show the full attempt history for one publication:

    direnv exec . gleam run -m scherzo ctl artifact publication show --run <run-id> --publication <publication-id> --root <workspace-root>

Both commands also accept `--json`.

## Retry publication

Retry one failed retryable publication:

    direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id> --publication <publication-id>

Retry every latest failed retryable publication for the run:

    direnv exec . gleam run -m scherzo ctl artifact publication retry --run <run-id>

When a daemon control file is available, retry goes through the control/daemon operator path. When running against retained local state with `--root`, retry validates that the run still has a retained output manifest and that the current workflow publication config still matches the retained retry target.

## Supported publication boundary

Same-repository GitHub publication is workspace-driver-backed. Active publication routes should use workflow `mode: commit_stack` and a retained workflow workspace whose selected driver advertises `publish-commit-stack` or the migration-compatible `publish-change` capability.

GitHub file artifact publication is no longer an active production path. Scherzo does not create or recover hidden managed GitHub checkouts under `.scherzo-state/artifact-repositories/github/<hash>`, and operators should not remove sibling `.publication.lock` files as a normal recovery step. If a retained legacy attempt references such a checkout, treat it as historical evidence only and open a follow-up rather than cleaning or recreating the path.

Live GitHub or browser verification is a deferred human/operator follow-up after implementation. The pre-publish gate for publication hardening remains deterministic tests, retained-manifest inspection, and the required format/lint commands.

## Common retry failures

- `publication_run_not_found`: the run id is unknown in the local ledger.
- `publication_not_found`: the publication id is unknown for that run.
- `publication_not_retryable`: the latest attempt is not a failed retryable attempt.
- `publication_retry_output_manifest_missing`: the run no longer has the retained output manifest required for replay.
- `publication_retry_config_drift`: the current workflow publication route/config no longer matches the retained failed attempt.
- `file_artifact_publication_unsupported`: the retained route is a GitHub file publication route; migrate it to `mode: commit_stack` with workspace-driver publication before retrying.

Do not run reset/clean against an active agent workspace as part of artifact publication recovery.
