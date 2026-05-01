# ExecPlan Revision Smoke Plan

## Purpose / Big Picture

This temporary ExecPlan exists only to smoke test Scherzo's ExecPlan revision workflow. It describes a fictional change that validates that GitHub PR feedback can revise a plan document and update the existing PR branch.

## Problem Framing and Constraints

The workflow must collect review comments from a GitHub PR, reason about which feedback is current, and revise this plan without editing implementation files. The smoke test intentionally leaves several details underspecified so PR comments can request targeted revisions.

## Strategy Overview

Use a small plan-only PR as the target. Reviewers leave feedback. The revision workflow fetches the PR head, reads the feedback bundle, edits this file, validates the scope, pushes the branch, and posts one summary comment.

## Alternatives Considered

- Manual plan edits: useful as a fallback but does not test Scherzo automation.
- Revising a real feature PR: more realistic but riskier and noisier for a first smoke test.

## Risks and Countermeasures

- The workflow might edit files outside the plan. Countermeasure: validation should reject any changed file other than this plan.
- The workflow might miss inline comments. Countermeasure: the smoke PR includes an inline comment on this file.
- The workflow might revise the plan locally but fail to push the existing PR branch. Countermeasure: keep the revision limited to this file, let the deterministic push step surface the exact failure, and recover by retrying the push after the remote or credential problem is fixed. If the push failed because the PR branch advanced, rerun the preparation step against the latest PR head before pushing again rather than creating a new branch.

## Progress

- [ ] Smoke PR created.
- [ ] Review feedback added.
- [ ] Revision workflow run.
- [ ] PR branch updated or no-op summary posted.

## Surprises & Discoveries

None yet.

## Decision Log

- Created a synthetic plan-only PR so the revision workflow can be tested without touching production work.
- Added explicit validation evidence, push-failure recovery guidance, and a rollout ownership clarification in response to trusted PR feedback.
  Date: 2026-05-01.

## Outcomes & Retrospective

Pending smoke test completion.

## Context and Orientation

The target file is `docs/plans/LIV-000-execplan-revision-smoke.md`. The workflow under test is `workflow:execplan-revision`.

## Preconditions and Verified Facts

- The PR contains exactly one plan file.
- The workflow will be run from a Linear issue that references the PR with a human-friendly phrase.

## Scope Boundaries

In scope:

- Revising this ExecPlan based on PR feedback.
- Pushing the existing PR branch.
- Posting one acknowledgement comment.

Out of scope:

- Implementing source-code changes.
- Editing Scherzo workflow configuration.
- Replying to every review thread individually.

## Milestones

1. Prepare a synthetic PR.
2. Add representative PR feedback.
3. Run the revision workflow.
4. Verify that this plan was updated and no other files changed.

## Plan of Work

The revision workflow should make only targeted documentation edits requested by trusted PR feedback.

## Concrete Steps

1. Fetch the latest PR head.
2. Read normalized review feedback.
3. Edit this plan if feedback is actionable.
4. Validate that only this file changed.
5. From the repository root, run `direnv exec . gleam test` and keep the passing result as acceptance evidence.
6. Push the PR branch.
7. Post a concise PR acknowledgement.

## Testing and Falsifiability

The smoke test passes if the PR branch receives a new commit that changes only this file, the resulting plan addresses the trusted review feedback, and `direnv exec . gleam test` passes from the repository root. The test is falsified if validation reports any changed file outside this plan, if the Gleam test command fails, or if trusted feedback remains unaddressed without an explicit no-change explanation.

## Validation and Acceptance

- `scripts/scherzo-execplan-revision validate` passes.
- From the repository root, `direnv exec . gleam test` passes and is cited as acceptance evidence that the revised branch still satisfies the Gleam test suite.
- The Linear issue reaches `Done`.
- The PR contains one Scherzo acknowledgement comment.

## Rollout, Recovery, and Idempotence

This is a temporary smoke-test artifact. Delete the PR branch and base branch after the test.

If pushing the revised PR branch fails, do not create a replacement branch or broaden the document change. Keep the local plan revision intact, inspect the push failure, and retry the existing PR branch push after resolving a transient network, authentication, or remote-service problem. If the failure is non-fast-forward because new commits appeared on the PR branch, rerun the preparation step so the workspace is based on the latest PR head, confirm that only `docs/plans/LIV-000-execplan-revision-smoke.md` remains changed, rerun validation, and then push the same PR branch again.

## Artifacts and Notes

- PR URL: to be created.
- Linear issue: to be created.

## Interfaces and Dependencies

- GitHub PR comments and review APIs.
- Linear issue title, description, and comments.
- jj workspace and bookmark operations.

## Open Questions and Clarifications Needed

- [CLARIFY] The final rollout decision owner for revised ExecPlans is not named. Confirm whether the owner is the PR author, the Linear issue assignee, or the Scherzo workflow operator before treating a revised plan as ready to merge or close.

## Revision Note

On 2026-05-01, this plan was revised to address trusted PR feedback by adding push-failure recovery guidance, explicit `direnv exec . gleam test` acceptance evidence, and a `[CLARIFY]` item about final rollout ownership.
