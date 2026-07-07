Recover base drift for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

{% include "fragments/execplan-identity-model.md" %}
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

You are running only because the combined refresh-and-validate step failed and Scherzo step recovery asked you to make the smallest safe local repair before rerunning that same command unchanged.

Read before editing:

- `workflow_step_recovery_input`
- `tmp/scherzo-implementation-refresh-base-before-validation.json` when it exists; otherwise `tmp/scherzo-implementation-refresh-base-latest.json`
- `$SCHERZO_RUN_ROOT/state/implementation/scherzo-implementation-validation.json` when it exists
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`
- `$SCHERZO_WORKSPACE_DRIVER status --human`
- `$SCHERZO_WORKSPACE_DRIVER diff --human` when needed

Rules:

- Do not create, forget, finish, switch, push, bookmark, commit, squash, abandon, or otherwise manage workflow workspaces, branches, bookmarks, pushes, or pull requests.
- Repair only mechanical base-drift fallout. Do not add features, broaden scope, or change product decisions.
- Read `failure_summary`, `stdout_excerpt`, and `stderr_excerpt` from the validation artifact before reading full diagnostics.
- Never treat a validation failure as repairable base drift unless the refresh status is `rebased_clean` or `conflicts`.
- If the chosen branch does not require a failure marker, remove any stale `tmp/scherzo-implementation-base-drift-failure.md` before submitting recovery.

State table:

- `fresh` plus validation success: no repair should be needed; usually return `gave_up`.
- `rebased_clean` plus validation success: no tracked-file edit should be needed; usually return `gave_up`.
- `fresh` plus validation failure: write `tmp/scherzo-implementation-base-drift-failure.md` and return `gave_up`.
- `conflicts`: resolve only the listed conflicted files and only when the fix is mechanical.
- `rebased_clean` plus validation failure: make only the smallest mechanical edits needed to adapt to the new base.
- `fetch_failed`, `base_not_found`, or `rebase_failed`: write `tmp/scherzo-implementation-base-drift-failure.md` and return `gave_up`.

When you repair something, write `tmp/scherzo-implementation-base-drift-repair.md` with the refresh status, validation status, files changed, and why each edit is mechanical.

Process:

1. Read the refresh JSON and determine the branch of the state table.
2. Confirm there is no blocking review-doc/implementation-pack conflict beyond the expected handoff/source split.
3. Make only the smallest safe mechanical base-drift repair when allowed.
4. Write the repair summary or failure marker.
5. Call `submit_workflow_step_recovery_result` with `recheck` when the original combined command should pass if rerun unchanged, otherwise `gave_up`.

Final response format:

## Summary
One short paragraph stating whether this was a repaired conflict, a repaired `rebased_clean` validation failure, or a required workflow failure.

## Files touched
- `path`: short note, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to strict final workflow validation`.

## Decision
- `recheck` or `gave_up`.
