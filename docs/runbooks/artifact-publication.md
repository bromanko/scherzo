# Artifact publication operator runbook

Use Scherzo's retained publication ledger to inspect and retry publication work without rerunning the producing workflow.

## Inspect retained publication state

List the latest publication attempt for each configured publication on a run:

    direnv exec . gleam run -- artifact publication list --run <run-id> --root <workspace-root>

Show the full attempt history for one publication:

    direnv exec . gleam run -- artifact publication show --run <run-id> --publication <publication-id> --root <workspace-root>

Both commands also accept `--json`.

## Retry publication

Retry one failed retryable publication:

    direnv exec . gleam run -- artifact publication retry --run <run-id> --publication <publication-id> --root <workspace-root>

For same-repository GitHub repository-change publication, retry the configured
`mode: commit_stack` publication. Review-doc helper output is outside the
artifact-publication retry lane; use retained workflow output manifests and driver
diagnostics rather than a single `execplan_review_doc` publication route.

Retry every latest failed retryable publication for the run:

    direnv exec . gleam run -- artifact publication retry --run <run-id> --root <workspace-root>

Retry is an offline retained-state command. It requires `--root`, acquires the workspace instance lock before mutating retained publication state, validates that the run still has a retained output manifest, and checks that the current workflow publication config still matches the retained retry target.

## Common retry failures

- `publication_run_not_found`: the run id is unknown in the local ledger.
- `publication_not_found`: the publication id is unknown for that run.
- `publication_not_retryable`: the latest attempt is not a failed retryable attempt.
- `publication_retry_output_manifest_missing`: the run no longer has the retained output manifest required for replay.
- `publication_retry_config_drift`: the current workflow publication route/config no longer matches the retained failed attempt.

## Publication boundary

Same-repository repository-change publication is workspace-driver-backed. Commit-stack routes publish from the retained workflow workspace through the selected driver using `publish-commit-stack` only, and retry reuses that retained workspace boundary. Retained runs or custom drivers that still mention the removed `publish-change` name must be migrated with `docs/runbooks/workspace-driver-migration.md#migrating-from-publish-change-to-publish-commit-stack` before publication can proceed.

GitHub file artifact publication no longer has a Scherzo-managed checkout implementation. Routes that still use `files:` for a GitHub repository fail with `file_publication_unsupported` and do not clone, reset, clean, or lock hidden repositories. Publish file-style review documents from an explicit workflow command/driver step, or convert repository changes to `mode: commit_stack`.

The old Scherzo-managed checkout path under `.scherzo-state/artifact-repositories/github/<hash>` is legacy state, not an active same-repository GitHub publication model. Do not manually reset or clean active agent workspaces as publication recovery. For commit-stack failures, inspect the retained workspace and driver diagnostics, then retry or abandon through the artifact publication commands.
