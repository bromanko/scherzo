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
- Scherzo enforces this step's configured bounded smart-recovery budget and reruns the original verifier unchanged after `recheck`. Read `recovery_attempt_number` and `max_recovery_attempts` from `workflow_step_recovery_input`; do not extend, reset, or otherwise circumvent that bound.
- This workflow-specific prompt uses `recheck` as both a completion check and a bounded continuation checkpoint. On a non-final recovery attempt (`recovery_attempt_number < max_recovery_attempts`), return `recheck` after making meaningful safe progress even when required findings remain. The unchanged verifier will stamp a fresh, smaller backlog before the next recovery attempt. In this case, `recheck` does not claim that completion already passes; state completed and remaining work truthfully in `summary` and `reason`.
- On the final configured recovery attempt, return `recheck` only when all current findings are completed and the original verifier should pass unchanged. Return `gave_up` when required findings remain.
- Return `gave_up` before the final attempt only for a concrete blocker or inability to make meaningful safe progress: missing required input or artifact; a provenance or intent conflict; unsafe ambiguity; a required product decision; or an infrastructure failure that prevents progress. Explain the blocker and operator action needed.
- Never describe the attempt budget as exhausted when `recovery_attempt_number < max_recovery_attempts`. Broad remaining work, elapsed turn time, or an incomplete canonical backlog is not by itself a reason to give up on a productive non-final attempt.
- Call `submit_workflow_step_recovery_result` exactly once. Use `decision: "recheck"` under the productive non-final or completed-final rules above; otherwise use `decision: "gave_up"` and report completed work, every remaining finding, the concrete blocker or final-attempt exhaustion, and required operator action.

Process:

1. Read `workflow_step_recovery_input`, its attempt context, the current stamped plan-completion verdict, and the canonical handoff artifacts.
2. Reconcile each current blocking finding with the current tree and turn the still-valid findings into the attempt backlog.
3. Implement every backlog item that can be completed safely within canonical scope; breadth or a multi-file change is not by itself a reason to stop.
4. Run validation proportionate to the edits, including focused checks while iterating and required full gates when necessary to establish that the unchanged verifier should pass.
5. Re-read the current verdict backlog and inspect the final diff so the recovery decision names what was completed and what remains.
6. Compare `recovery_attempt_number` with `max_recovery_attempts`. If this is non-final and you made meaningful safe progress, submit `recheck` to obtain a fresh verifier backlog even if work remains. On the final attempt, submit `recheck` only if the verifier should pass; otherwise submit `gave_up`.

Final response format:

## Summary
One short paragraph naming the blocking finding you repaired or why you gave up.

## Files touched
- `path`: short note, or `None`.

## Validation
- Commands you ran, or `Not run`.

## Recovery decision
- `recheck` or `gave_up`.
