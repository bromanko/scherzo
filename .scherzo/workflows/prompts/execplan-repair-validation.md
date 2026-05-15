You are running the validation-repair step for Scherzo's `workflow:execplan` workflow.

Task:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}

Initial draft validation status: {{ steps.validate_draft.status }}
Initial draft validation exit code: {{ steps.validate_draft.exit_code }}

Initial draft validation stdout:

{{ steps.validate_draft.stdout }}

Initial draft validation stderr:

{{ steps.validate_draft.stderr }}

Workflow contract:

- You are already inside the same dedicated workflow workspace prepared by Scherzo as the draft plan. Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- This step exists only to repair mechanical ExecPlan validation failures from the root-resolved `scherzo-execplan validate` helper.
- If `Initial draft validation status` is `success`, do not edit anything. Finish immediately with a concise no-op response.
- If validation failed, locate the single changed Markdown plan artifact under `docs/plans/` using the validation output above or `$SCHERZO_WORKSPACE_DRIVER changed-files --json`.
- Make the smallest edits needed for the validator to pass. Do not broaden scope, rewrite the plan, or add new design content beyond what is required to satisfy validation.
- Revise only the `docs/plans/*.md` plan artifact. Do not create a tracked HTML artifact, and do not edit source code, tests, config, workflow files, prompt files, existing docs, or temporary review files.
- Preserve the ExecPlan as a living document and keep all required sections intact in the Markdown source file. The only tracked plan artifact must remain `docs/plans/*.md`.
- Use repository-relative paths only. Do not write absolute local paths, even as examples or negative test data. Do not include literal prefixes such as `/Users/`, `/home/`, `/private/`, or `/var/folders/`; use placeholders like `<absolute-local-path>` when discussing forbidden path shapes.
- If the validation error cannot be fixed safely in one bounded pass, leave the plan unchanged and explain the blocker in your final response.

Dogfood time budget:

- Use at most 6 tool calls before applying a repair.
- After editing, run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan" validate` if possible within the budget; otherwise make the smallest obvious fix and rely on the following `validate_after_repair` command step.

Repair process:

1. Check the initial validation status above.
2. If it succeeded, stop with no edits.
3. If it failed, read only the validation output and the plan file sections needed to fix the reported problem.
4. Edit the plan file minimally.
5. Run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan" validate` if practical.
6. Finish with a concise response naming the plan file, whether you edited it, and the expected validation result.

Final response format:

## Summary
One short paragraph stating whether this step was a no-op or what validation problem was repaired.

## Changes
- `None` if no-op, or bullets naming the minimal edits made.

## Validation
- The validation command run and result, or `Not run; validate_after_repair will enforce it.`
