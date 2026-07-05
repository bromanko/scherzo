Recover base drift for Scherzo's implementation workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

You are running only because the combined refresh-and-validate step failed and Scherzo step recovery asked you to make the smallest safe local repair before rerunning that same command unchanged.

Read before editing:

- `workflow_step_recovery_input`
- `tmp/scherzo-implementation-refresh-base-before-validation.json` when it exists; otherwise `tmp/scherzo-implementation-refresh-base-latest.json`
- `$SCHERZO_RUN_ROOT/state/implementation/scherzo-implementation-validation.json` when it exists
- `.scherzo/command-step-diagnostics/<failed-step>.txt` when it exists
- `$SCHERZO_WORKSPACE_DRIVER status --human`
- `$SCHERZO_WORKSPACE_DRIVER diff --human` when needed

Workflow contract:

- Do not create, forget, finish, switch, push, bookmark, commit, squash, abandon, or otherwise manage workflow workspaces, branches, bookmarks, pushes, or pull requests.
- This recovery repairs only base drift caused by rebasing the implementation change onto the latest configured PR base.
- Read `failure_summary`, `stdout_excerpt`, and `stderr_excerpt` from the structured validation artifact before reading full diagnostics.
- Never treat a validation failure as repairable base drift unless the refresh status is `rebased_clean` or `conflicts`.
- If the chosen branch does not require a failure marker, remove any stale `tmp/scherzo-implementation-base-drift-failure.md` before submitting recovery.
- Use `submit_workflow_step_recovery_result` exactly once.

State table:

- If refresh status is `fresh` and validation succeeded, no recovery should have been needed. Return `gave_up` unless the failure was a clearly local artifact glitch.
- If refresh status is `rebased_clean` and validation succeeded, no tracked-file edit should be needed. Return `gave_up` unless a clearly local artifact glitch is safe to repair.
- If refresh status is `fresh` and validation failed, do not repair. Write `tmp/scherzo-implementation-base-drift-failure.md` and return `gave_up`.
- If refresh status is `conflicts`, resolve only the listed conflicted files and only when the fix is mechanical.
- If refresh status is `rebased_clean` and validation failed, make only the smallest mechanical edits needed to adapt to the new base.
- If refresh status is `fetch_failed`, `base_not_found`, or `rebase_failed`, do not edit tracked files. Write `tmp/scherzo-implementation-base-drift-failure.md` and return `gave_up`.

Mechanical-only examples:

- conflict-marker cleanup
- renamed imports or moved modules
- test fixture or formatting fallout caused by the new base

Do not add features, broaden scope, or rewrite behavior.

When you make a repair, write `tmp/scherzo-implementation-base-drift-repair.md` summarizing the refresh status, validation status, files changed, and why each edit is mechanical.

Process:

1. Read the refresh JSON and determine whether the refresh status is `fresh`, `rebased_clean`, `conflicts`, or nonrepairable.
2. If validation ran, inspect `failure_summary`, `stdout_excerpt`, and `stderr_excerpt` first.
3. Make only the smallest safe mechanical repair when the state table allows it.
4. Write `tmp/scherzo-implementation-base-drift-repair.md` for repairs or `tmp/scherzo-implementation-base-drift-failure.md` for nonrepairable cases.
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
