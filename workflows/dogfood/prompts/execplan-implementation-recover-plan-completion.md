Recover plan-completion failure for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

You are running only because a plan-completion verifier step failed. Make a serious bounded attempt to complete the verifier's explicit canonical-plan findings in the retained workspace before Scherzo reruns that verifier unchanged.

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
- Do not rewrite the plan, change workflow retry policy, bypass or edit the verifier/gate contract, or expand beyond canonical plan scope.
- Breadth alone is not scope expansion: canonical work may touch many files, require substantive implementation, or need full validation. In this prompt, the appended repair-and-recheck contract's "smallest safe local change" means no more than the current canonical backlog requires; it does not impose one-file or narrow-patch limits.

Recovery policy:

- Treat the current stamped verifier findings as an actionable implementation backlog. Fix all concrete missing behavior, tests, validation, docs/helper migration, or artifact evidence required by the canonical plan and implementation pack that can be safely completed during this attempt.
- Inspect the current workspace before acting. A later recovery attempt follows a fresh unchanged-verifier recheck, so use its newly stamped verdict and the current tree to identify only the findings that remain; do not repeat or undo already-completed work unless the recheck shows it regressed.
- Continue implementing within canonical scope until the original unchanged verifier should pass. Do not return `gave_up` merely because the work is broad, touches multiple files, or requires full validation.
- Scherzo enforces this step's configured two-attempt smart-recovery budget and reruns the original verifier unchanged after `recheck`. Do not extend, reset, or otherwise circumvent that bound.
- Return `gave_up` only for a concrete blocker: missing required input or artifact; a provenance or intent conflict; unsafe ambiguity; a required product decision; an infrastructure failure that prevents progress; or exhausted attempt budget with remaining findings documented. Explain the blocker and the operator action needed.
- If all current findings are completed and the original verifier should pass when rerun unchanged, call `submit_workflow_step_recovery_result` exactly once with `decision: "recheck"`.
- Otherwise call `submit_workflow_step_recovery_result` exactly once with `decision: "gave_up"`; report completed work, every remaining finding, the concrete blocker, and why another safe change is not possible in the available budget.

Process:

1. Read `workflow_step_recovery_input`, its attempt context, the current stamped plan-completion verdict, and the canonical handoff artifacts.
2. Reconcile each current blocking finding with the current tree and turn the still-valid findings into the attempt backlog.
3. Implement every backlog item that can be completed safely within canonical scope; breadth or a multi-file change is not by itself a reason to stop.
4. Run validation proportionate to the edits, including focused checks while iterating and required full gates when necessary to establish that the unchanged verifier should pass.
5. Re-read the current verdict backlog and inspect the final diff so the recovery decision names what was completed and what, if anything, remains.
6. Submit `submit_workflow_step_recovery_result` with `recheck` or `gave_up`.

Final response format:

## Summary
One short paragraph naming the blocking finding you repaired or why you gave up.

## Files touched
- `path`: short note, or `None`.

## Validation
- Commands you ran, or `Not run`.

## Recovery decision
- `recheck` or `gave_up`.
