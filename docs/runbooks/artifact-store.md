# Artifact store developer runbook

Scherzo treats retained workflow artifacts as objects in `src/scherzo/state/artifact_store.gleam`.
The current production implementation is the default filesystem store, but the
module is now a port that can host another backend through
`artifact_store.custom`.

## Default behavior

`artifact_store.new(workspace_root)` and `artifact_store.filesystem(workspace_root)`
both construct the default filesystem store.

That store must preserve these compatibility surfaces:

- Artifact refs stay stable relative strings such as `runs/<run>/<step>/attempt-1.json`.
- Files remain under `<workspace-root>/.scherzo-state/artifacts/<ref>`.
- Operator-facing display paths remain `.scherzo-state/artifacts/<ref>`.
- Structured-output metadata keeps the legacy `path` field for compatibility;
  new code should prefer `ref`, `uri`, `display_path`, and `local_path`.

## Artifact location contract

A store location is described by:

- `ref`: durable relative artifact ref.
- `uri`: store-neutral identifier for operator/debug use.
- `display_path`: human-facing inspectable location.
- `local_path`: `Some(path)` only when a local file path exists.

Compatibility expectations:

- `ref` is the durable contract used by ledger records, checkpoint data, and recovery.
- `uri` must be non-empty. Filesystem stores should prefer a percent-encoded
  `file://` URI and may fall back to a stable store URI when absolute resolution
  is unavailable.
- `display_path` should be the best human-facing location to show in prompts,
  diagnostics, and operator output.
- `local_path` is optional. Core artifact reads and inline structured-output reads
  must work by ref even when `local_path` is `None`.
- `path` is retained only as a legacy compatibility field. For the default
  filesystem store it remains a local path; backend-neutral consumers should not
  use it for reads.
- For old structured artifacts that only stored `path`, decoders treat that value
  as the default `display_path`, `uri`, and `local_path` compatibility source.

## Adding another store with `artifact_store.custom`

A custom store provides `StoreCallbacks` for:

- `write(ref, contents)` for UTF-8/text artifacts
- `read(ref)` for UTF-8/text artifacts
- `write_immutable_bytes(ref, contents)` for content-addressed byte artifacts
- `read_bytes(ref)` for exact byte reads
- `locate(ref)`

Implementation checklist:

1. Keep refs stable and relative. Do not reinterpret them as absolute paths.
2. Accept that Scherzo validates refs before callbacks run.
3. Make `read` return the text previously written for the ref.
4. Make `read_bytes` return exact bytes previously written for the ref.
5. Make `write_immutable_bytes` idempotent for identical bytes and report a
   conflict for a pre-existing ref with different bytes.
6. Make `locate` return a stable `uri` and useful `display_path`.
7. Return `local_path: None` when the backend does not expose a local file.
8. Preserve checksum and byte-count behavior by letting Scherzo hash the exact
   stored bytes.
9. Add focused tests proving step-artifact round trips and inline structured-output
   reads work without a local path.

## Known filesystem-only compatibility surfaces

Some command-based workflows and helper scripts still expect a real filesystem
artifact directory through `SCHERZO_RUN_ARTIFACT_DIR`, which today points into the
default filesystem store under `.scherzo-state/artifacts/runs/<run-id>`.

A future service-backed store must either:

- materialize compatible local files for those command steps, or
- migrate those workflow commands and helper scripts in a separate rollout.

Do not change workflow YAML, helper scripts, or durable refs as part of merely
adding a new store backend.

## Publication inspection

Artifact publication state is retained locally in the state ledger plus immutable
publication manifest artifacts under `.scherzo-state/artifacts/runs/<run-id>/publications/`.
Operators can inspect that state without a running daemon by using:

- `scripts/scherzoctl artifact publication list --run <run-id> --root <workspace-root>`
- `scripts/scherzoctl artifact publication show --run <run-id> --publication <publication-id> --root <workspace-root>`
- `scripts/scherzoctl artifact publication retry --run <run-id> --publication <publication-id> --root <workspace-root>`

ExecPlan authoring and revision retain the `exec_plan_bundle` and its `plan` entry
as workflow outputs only; checked-in dogfood workflows no longer publish the review
doc as a single GitHub file. Same-repository GitHub publication is represented by
`mode: commit_stack` routes and executed through the selected workspace driver.

The old Scherzo-owned managed checkout under
`.scherzo-state/artifact-repositories/github/<hash>` is legacy state for same-repo
GitHub publication, not the active path for current dogfood workflows.

`retryable` reports whether a failed planning or execution attempt can be replayed
from retained artifacts. `retry_execution_available` becomes `true` once Scherzo has
recorded the retained manifest and current publication config needed for replay.

When operators encounter a historical dirty managed-checkout attempt, do not reset
or clean the active workflow workspace as part of recovery. Preserve the retained
manifest diagnostics and retry through the current commit-stack publication route
when the workflow still has one configured.
