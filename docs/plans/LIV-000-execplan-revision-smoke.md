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

## Progress

- [ ] Smoke PR created.
- [ ] Review feedback added.
- [ ] Revision workflow run.
- [ ] PR branch updated or no-op summary posted.

## Surprises & Discoveries

None yet.

## Decision Log

- Created a synthetic plan-only PR so the revision workflow can be tested without touching production work.

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
5. Push the PR branch.
6. Post a concise PR acknowledgement.

## Testing and Falsifiability

The smoke test passes if the PR branch receives a new commit that changes only this file and the resulting plan addresses the trusted review feedback.

## Validation and Acceptance

- `scripts/scherzo-execplan-revision validate` passes.
- The Linear issue reaches `Done`.
- The PR contains one Scherzo acknowledgement comment.

## Rollout, Recovery, and Idempotence

This is a temporary smoke-test artifact. Delete the PR branch and base branch after the test.

## Artifacts and Notes

- PR URL: to be created.
- Linear issue: to be created.

## Interfaces and Dependencies

- GitHub PR comments and review APIs.
- Linear issue title, description, and comments.
- jj workspace and bookmark operations.

## Open Questions and Clarifications Needed

None.
