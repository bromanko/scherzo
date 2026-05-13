You are running Scherzo's checked-in `workflow:merge-conflict-resolution` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Preparation output:
{{ steps.prepare_target.stdout }}

Workflow contract:

- This workflow is manually triggered for one existing same-repository GitHub PR or branch. The Linear issue must name the target PR or branch; the prepare step has already fetched it.
- The only allowed goal is to resolve merge conflicts caused by merging the printed base branch into the printed target branch.
- Do not intentionally change functionality, public behavior, requirements, tests, or documentation beyond what is strictly necessary to remove conflict markers and preserve the combined intent of the two parents.
- If you cannot resolve a conflict without making a behavioral choice, write `tmp/scherzo-merge-conflict-failure.md` explaining the ambiguity and stop. The validation step will fail the workflow as requested.
- You are already inside a dedicated workflow workspace prepared by Scherzo. Do not create, forget, finish, switch, push, bookmark, commit, squash, abandon, or otherwise manage workflow workspaces or branches. Later deterministic command steps validate and publish through the configured workspace driver.
- Do not use `gh` to post comments. The publish step posts one PR comment when the target is a PR.
- Read the `METADATA_PATH=...` file printed by the prepare step for target metadata and the exact conflicted file list.
- Read the `BRIEF_PATH=...` file printed by the prepare step for the normalized target brief.
- Edit only files listed under `CONFLICTED_FILES` in the preparation output by default.
- Exception: you may edit a non-conflicted tracked file only when a targeted check or compiler error shows a small, mechanical fallout from the conflict resolution and the edit is required to preserve the combined intent of both parents. Examples: callback arity updates, import/module rename fixes, constructor field shape updates with ignored/defaulted values, or test helper signature updates that do not change assertions or expected behavior.
- Do not use the exception to change test expectations, update broad snapshots, add/remove tracked files, refactor, change public behavior, or choose between incompatible semantics. If the needed edit is not obviously mechanical, write `tmp/scherzo-merge-conflict-failure.md` and stop.
- If you edit any non-conflicted tracked file under this exception, write `tmp/scherzo-merge-conflict-mechanical-edits.json` with a `non_conflicted_edits` list. Each entry must include `path` and a behavior-preserving `reason`. Validation hashes every other tracked file and fails if non-conflicted changes are not manifested.
- Remove all VCS conflict markers. Jujutsu-backed workspaces may show conflict marker lines starting with `<<<<<<<`, `+++++++`, `%%%%%%%`, `\\\\\\\`, or `>>>>>>>`.
- If `CONFLICT_COUNT=0`, make no source changes. You may still read the metadata and finish with a no-op summary.

Conflict-resolution policy:

- Parent 1 is the target branch/PR head; parent 2 is the base branch being merged in.
- Prefer resolutions that preserve the target branch's intended behavior while incorporating base-side mechanical changes needed for the code to compile and tests to pass.
- Use nearby code and tests to understand renamed symbols, moved modules, formatting, imports, and data-shape changes.
- Do not add new features, refactor opportunistically, rename unrelated code, update snapshots broadly, or rewrite tests to fit a changed behavior.
- If both sides made incompatible behavior changes and no purely mechanical reconciliation is obvious, fail by writing `tmp/scherzo-merge-conflict-failure.md`.
- If a test expectation conflicts semantically with changed code behavior, do not choose a new behavior. Fail and explain the choice needed.

Process:

1. Read the `METADATA_PATH` and `BRIEF_PATH` files printed in the prepare output.
2. If no conflicts were recorded, do not edit tracked files.
3. Inspect only the conflicted files and the smallest nearby context needed to understand mechanical moves/renames.
4. Resolve conflict markers in the conflicted files.
5. Run targeted checks if cheap and relevant. If they expose mechanical fallout in a non-conflicted tracked file, make only the minimum behavior-preserving edit and write `tmp/scherzo-merge-conflict-mechanical-edits.json`:

```json
{
  "schema_version": 1,
  "non_conflicted_edits": [
    {
      "path": "test/example_test.gleam",
      "reason": "Mechanical callback arity update after the resolved source API added an ignored profile argument; assertions unchanged."
    }
  ]
}
```

6. The workflow validation step runs the final project validation.
7. Write `tmp/scherzo-merge-conflict-resolution.md` when conflicts were resolved, with this exact structure:

```markdown
# Merge conflict resolution summary

## Outcome
Resolved conflicts without intentional functionality changes.

## Files resolved
- `path`: what was mechanically reconciled.

## Mechanical fallout edits
- `path`: reason from `tmp/scherzo-merge-conflict-mechanical-edits.json`, or `None`.

## Behavior-preservation rationale
- Why the result preserves target/base intent without choosing new behavior.

## Validation run by agent
- Command and result, or `Not run; deferred to workflow validation`.

## Remaining ambiguity
- None.
```

If you cannot safely resolve, write this instead and stop without continuing edits:

```markdown
# Merge conflict resolution failure

## Reason
A behavior-preserving resolution is not possible without a human decision.

## Ambiguous files
- `path`: the behavior choice required.

## Suggested human decision
- Question to answer.
```

Final response format:

## Summary
One short paragraph stating whether conflicts were resolved, no conflicts were present, or the workflow must fail due to ambiguity.

## Files touched
- `path`: short note, or `None`.

## Mechanical fallout edits
- `None`, or each non-conflicted tracked path plus why the edit is mechanical and behavior-preserving.

## Validation
- Commands you ran, or `Not run; deferred to workflow validation`.

## Ambiguity
- `None` or the exact reason you wrote `tmp/scherzo-merge-conflict-failure.md`.
