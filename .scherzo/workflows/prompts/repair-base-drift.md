Repair base drift for Scherzo's implementation workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Source preparation output:
{{ steps.source_preparation.stdout }}

Refresh stdout:
{{ steps.refresh_base_before_validation.stdout }}

Refresh stderr:
{{ steps.refresh_base_before_validation.stderr }}

Refresh exit code:
{{ steps.refresh_base_before_validation.exit_code }}

Validation stdout:
{{ steps.validate_after_refresh.stdout }}

Validation stderr:
{{ steps.validate_after_refresh.stderr }}

Validation exit code:
{{ steps.validate_after_refresh.exit_code }}

Workflow contract:

- You are already inside a dedicated jj workspace. Do not create, forget, finish, switch, push, bookmark, commit, squash, abandon, or otherwise manage jj workspaces, branches, bookmarks, pushes, or pull requests.
- Do not use `gh` to create, edit, close, or comment on pull requests. Later deterministic command steps validate and publish.
- This step repairs only base drift, meaning problems caused by rebasing the implementation change onto the latest configured pull request base.
- Read `tmp/scherzo-implementation-refresh-base-before-validation.json` when it exists. If it does not exist, read `tmp/scherzo-implementation-refresh-base-latest.json`.
- In this prompt, validation succeeded means the `validate_after_refresh` command exited `0`; validation failed means it exited nonzero.
- Never treat a validation failure as repairable base drift unless the refresh status is `rebased_clean` or `conflicts`.
- If you cannot prove a fix is mechanical, write `tmp/scherzo-implementation-base-drift-failure.md` and stop.

State table:

- If refresh status is `fresh` and validation succeeded, do not edit tracked files and do not write the failure marker. You may optionally write `tmp/scherzo-implementation-base-drift-repair.md` as a no-op summary.
- If refresh status is `rebased_clean` and validation succeeded, do not edit tracked files and do not write the failure marker. You may optionally write `tmp/scherzo-implementation-base-drift-repair.md` as a no-op summary explaining that `rebased_clean` plus validation success required no repair.
- If refresh status is `fresh` and validation failed, do not repair. Write `tmp/scherzo-implementation-base-drift-failure.md` saying validation failed without recorded base drift, and leave source files unchanged.
- If refresh status is `conflicts`, inspect only the conflicted files listed in the refresh JSON and the smallest nearby context needed to resolve mechanical conflicts. Resolve conflict markers, preserve the implementation's intended behavior, and write `tmp/scherzo-implementation-base-drift-repair.md` summarizing the resolution. If a behavior decision is needed, write `tmp/scherzo-implementation-base-drift-failure.md` and stop.
- If refresh status is `rebased_clean` and validation failed, inspect the validation output and the changed files. Make only the smallest mechanical edits needed to adapt the implementation to the new base, such as renamed functions, moved modules, changed imports, formatting expectations, or test fixture updates that preserve intended behavior. Do not add features or change requirements.
- If refresh status is `fetch_failed`, `base_not_found`, or `rebase_failed`, do not edit source files. Write `tmp/scherzo-implementation-base-drift-failure.md` with the nonrepairable reason.

Repair policy:

- Preserve the implementation's intended behavior while incorporating mechanical base-side changes needed for the code to compile and tests to pass.
- Do not add new features, refactor opportunistically, rename unrelated code, update snapshots broadly, or rewrite tests to fit changed product behavior.
- If both sides require an incompatible behavior choice, fail by writing `tmp/scherzo-implementation-base-drift-failure.md`.
- If the agent edits any tracked source, test, workflow, or documentation file, it must write `tmp/scherzo-implementation-base-drift-repair.md` with the refresh status, validation exit code, exact files changed, and why each edit is mechanical rather than a product decision.
- Run targeted checks only if cheap and directly relevant. The strict final validation command is responsible for the full suite.

Expected repair summary format:

```markdown
# Base drift repair summary

## Outcome
Resolved repairable base drift before final validation.

## Refresh status
`conflicts` or `rebased_clean`

## Validation status
`validate_after_refresh` exited <code>.

## Files changed
- `path`: why the edit is mechanical rather than a product decision.

## Validation run by agent
- Command and result, or `Not run; strict final validation is handled by the workflow`.

## Remaining ambiguity
None.
```

Expected no-op summary format:

```markdown
# Base drift repair summary

## Outcome
No base-drift repair was needed.

## Refresh status
`fresh` or `rebased_clean`

## Validation status
`validate_after_refresh` succeeded, so no tracked files were edited.
```

Expected failure marker format:

```markdown
# Base drift repair failure

## Reason
Validation failed, but the latest refresh status was `fresh`, so this is not classified as repairable base drift.

## Required human decision
Inspect the validation failure and decide whether it is an implementation bug or an unrecorded base-drift case.
```

Process:

1. Read the refresh JSON and determine the refresh status.
2. Use the refresh status and validation exit code to choose exactly one state-table branch above.
3. If editing is allowed, inspect only the files and nearby context needed for a mechanical base-drift repair.
4. Write either `tmp/scherzo-implementation-base-drift-repair.md` or `tmp/scherzo-implementation-base-drift-failure.md` as required by the chosen branch.
5. Run targeted checks only if cheap and relevant.
6. Summarize the outcome.

Final response format:

## Summary
One short paragraph stating whether this was a no-op, a repaired base-drift conflict, a repaired clean-rebase validation failure, or a required workflow failure.

## Files touched
- `path`: short note, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to strict final workflow validation`.

## Ambiguity
- `None` or the exact reason you wrote `tmp/scherzo-implementation-base-drift-failure.md`.
