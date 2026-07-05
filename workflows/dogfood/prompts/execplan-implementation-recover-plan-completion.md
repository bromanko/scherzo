Recover plan-completion failure for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

You are running only because a plan-completion verifier step failed and Scherzo step recovery asked you to make the smallest safe repair before the verifier is rerun unchanged.

Read before editing:

- `workflow_step_recovery_input`
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`
- `$SCHERZO_RUN_ROOT/state/implementation/scherzo-plan-completion-verdict.json`
- `$SCHERZO_WORKSPACE_DRIVER status --human`
- `$SCHERZO_WORKSPACE_DRIVER diff --human` when needed

Identity and safety rules:

- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected.
- Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.
- Do not create, forget, finish, switch, push, bookmark, commit, squash, abandon, or otherwise manage workflow workspaces, branches, bookmarks, pushes, pull requests, or Linear state.
- Do not rewrite the plan, change workflow retry policy, bypass the verifier/gate contract, or broaden scope.
- Make only the smallest safe local edit for explicit blocking findings that the failed verifier recorded.

Recovery policy:

- Fix only concrete missing behavior, tests, validation, docs/helper migration, or artifact evidence required by the canonical plan and implementation pack.
- If artifacts are missing, provenance is ambiguous, the required fix needs a product decision, or the change would be broad or unsafe, return `gave_up`.
- If you repair the issue and the original verifier should pass when rerun unchanged, call `submit_workflow_step_recovery_result` exactly once with `decision: "recheck"`.
- Otherwise call `submit_workflow_step_recovery_result` exactly once with `decision: "gave_up"` and explain why.

Process:

1. Read `workflow_step_recovery_input` and the stamped plan-completion verdict artifact.
2. Confirm the failure is a safe local repair rather than a scope/provenance conflict.
3. Make the smallest safe edit needed.
4. Run only cheap targeted checks when directly helpful.
5. Submit `submit_workflow_step_recovery_result` with `recheck` or `gave_up`.

Final response format:

## Summary
One short paragraph naming the blocking finding you repaired or why you gave up.

## Files touched
- `path`: short note, or `None`.

## Validation
- Commands you ran, or `Not run`.

## Recovery decision
- `recheck` or `gave_up`.
