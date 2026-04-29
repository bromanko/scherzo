# Enforce Linear workflow-label dispatch policy

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo will not launch pi for a Linear issue unless the issue has exactly one configured workflow label, such as `workflow:bugfix` or `workflow:research`. The observable behavior is that an issue in a dispatchable Linear state but missing a workflow label, carrying multiple workflow labels, or carrying an unknown workflow label is skipped before workspace preparation and before pi launch. When configured, Scherzo posts a concise Linear comment explaining the problem and moves the issue to a configured non-dispatch state such as `Needs Workflow`.

This plan handles per-issue validity and is self-contained for dispatch enforcement. It reuses the `linear_contract` config from the read-only board-contract work if that config already exists; otherwise this plan adds the minimal `linear_contract` fields needed for workflow-label enforcement. It does not create Linear labels or workflow states, and it does not implement the full read-only board contract checker. If a contract-check command exists when this plan is implemented, extend it to validate the optional invalid-workflow state ID; if it does not exist yet, document that operators must validate that state ID out of band before enabling state movement. This plan also does not infer workflow labels automatically. Inference remains a human or upstream-triage responsibility.

## Problem Framing and Constraints

The desired operating rule is: a Linear task is dispatchable only when Linear state says it is ready for the agent and the task has exactly one explicit workflow label. State gates readiness, while labels choose the task workflow. This avoids relying on the model to infer workflow from ambiguous issue text after the task has already been dispatched.

The current Scherzo implementation already reads issue labels from Linear, normalizes them to lowercase, includes them in the prompt template, and decides dispatch eligibility in `src/scherzo/orchestrator/core.gleam`. However, the current dispatch predicate does not enforce a workflow label. Any issue with required fields, active state, open slots, and satisfied blockers can be dispatched even if no workflow is explicit.

Invalid workflow labels should not be treated as agent failures. If Scherzo launched pi and asked it to report that the issue needs workflow triage, the run would consume a workspace, a session, tokens, retries, and possibly handoff comments for work that should have been rejected by scheduler policy. The scheduler can cheaply detect the problem from the issue labels it already has.

The first implementation should be conservative. It should require explicit opt-in config for enforcement, skip invalid candidates before dispatch, avoid repeated comment spam for the same unchanged issue, and use existing Linear mutation primitives when optional triage comments or state moves are enabled. Board auto-reconciliation remains out of scope.

## Strategy Overview

Add a pure workflow-policy module that selects a workflow from Linear labels. It takes the configured label prefix and allowed workflow names, examines `domain.Issue.labels`, and returns one of four outcomes: policy disabled, selected workflow, missing workflow label, multiple workflow labels, or unknown workflow label. The valid case returns the suffix without the prefix, for example `bugfix`.

Add or extend a `linear_contract` config section with the fields needed by dispatch policy. The dispatch-relevant fields are `workflow_label_prefix`, `workflow_labels`, `enforce_issue_workflow_labels`, `invalid_workflow_state_id`, and `comment_on_invalid_workflow`. Enforcement means exactly one label with the configured prefix must appear and its suffix must be in `workflow_labels`. If enforcement is enabled, `workflow_labels` and `workflow_label_prefix` must be non-empty. If a separate `linear_contract.enabled` field exists, it controls the read-only board contract check only; dispatch enforcement is controlled solely by `enforce_issue_workflow_labels`.

Make all Linear mutations opt in. `comment_on_invalid_workflow` defaults to `False`, and `invalid_workflow_state_id` defaults to `None`. With only `enforce_issue_workflow_labels: true`, invalid issues are skipped and logged but Scherzo does not mutate Linear. Operators must explicitly enable comments and separately provide a state ID for state movement.

Use the pure policy in two places. First, add it to the pure dispatch guard so no invalid issue can reach pi through ordinary candidate dispatch or retry dispatch. Second, add daemon-side handling for issues that are otherwise dispatchable so Scherzo can produce an operator-visible triage action instead of silently skipping them. The daemon must not report invalid workflow for an issue that is already parked, running, claimed outside the retry path, terminal, blocked, missing required fields, or otherwise skipped for a non-workflow reason. This preserves the current command-processing order: a same-tick `/scherzo park` should still suppress invalid-workflow triage because the issue is no longer dispatchable. The daemon path records that it has already attempted to report the same unchanged invalid issue, so the same issue does not receive the same comment or log event on every poll.

Add a small Linear triage reporter side effect. For an invalid issue, it formats a deterministic comment explaining the exact problem, expected labels, and how a human can fix it. If `invalid_workflow_state_id` is set, it also updates the issue to that Linear state ID. The side effect uses existing Linear mutation request helpers through a dedicated client so tests can fake it without hitting real Linear.

Keep prompt behavior simple in this phase. `issue.labels` are already available to the prompt. Documentation should instruct workflows to include workflow-specific guidance keyed off the labels, but this plan does not add a new `issue.workflow` template variable. A later prompt ergonomics change can expose the selected workflow explicitly if needed.

## Alternatives Considered

One alternative is to rely only on the prompt and tell pi to stop if labels are missing. That is rejected because the scheduler already has the information needed to reject invalid inputs before pi is launched. The invalid task is a triage problem, not an agent task.

Another alternative is to infer the workflow label from title, description, or issue type at dispatch time. That is deliberately out of scope. Inference can help during triage, but the dispatch invariant should be explicit and auditable in Linear before Scherzo runs.

Another alternative is to use Linear custom fields instead of labels. Custom fields may be useful later, but the current provider already reads labels and exposes them to the prompt. Labels are the smallest implementation that works with the current Linear provider.

Another alternative is to make missing workflow labels globally block the daemon. That would let one bad issue stop unrelated ready work. This plan treats a missing workflow label as per-issue invalidity, while the separate board-contract plan handles global board drift.

Another alternative is to immediately add a selected workflow field to `domain.Issue` or template context. That is useful but not required for the safety invariant. The prompt can already inspect `issue.labels`, and changing template signatures would widen the first implementation. This plan defers that ergonomic improvement.

## Risks and Countermeasures

The main dispatch risk is that invalid issues are silently skipped forever. Countermeasure: when configured, Scherzo posts a Linear comment and moves the issue to a non-active `Needs Workflow` state using `invalid_workflow_state_id`. If only logging is configured, structured logs still include issue ID, identifier, and violation code.

The main spam risk is posting the same invalid-workflow comment on every poll. Countermeasure: store an in-memory invalid-workflow report-attempt record keyed by issue ID, violation fingerprint, observed `issue.updated_at`, and a stable fingerprint of the observed labels. Do not attempt the same report again until the issue timestamp, labels, or violation details change. If a configured Linear mutation fails, still suppress the same unchanged report attempt to avoid a tight failure loop; recovery is to fix the configuration or Linear permission and then change the issue, reload/restart the daemon, or wait for a later durable receipt feature. If the daemon restarts, the memory resets; durable invalid-triage receipts are deferred.

The main safety risk is mutating Linear unexpectedly or moving an issue to the wrong Linear state ID. Countermeasure: enforcement alone never mutates Linear; comments require `comment_on_invalid_workflow: true`, and state movement requires `invalid_workflow_state_id`. If the read-only board contract check exists, extend it so `invalid_workflow_state_id` is validated as an existing state ID and, when bound to the `needs_workflow` role, points to the configured `Needs Workflow` state name. If the check does not exist yet, the documentation must tell operators to validate the ID manually before enabling state movement. If no state ID is configured, the first version skips and logs, and optionally comments only when comments are explicitly enabled.

The main compatibility risk is breaking existing deployments that do not use workflow labels. Countermeasure: enforcement defaults to false, comment mutation defaults to false, and state movement defaults to absent. Existing workflows continue to dispatch as before until `linear_contract.enforce_issue_workflow_labels: true` is set.

The main retry risk is an issue that was previously claimed or scheduled for retry becoming invalid after a human edits labels. Countermeasure: apply the same policy to retry refresh results after confirming the refreshed issue still matches the retry and is otherwise ready for retry dispatch. If that retry candidate is invalid only because of workflow policy, clear the retry entry, cancel the retry timer, release any retained claim, attempt the invalid-workflow report once, and do not schedule a tight retry loop for an issue that policy forbids.

The main ordering risk is reporting invalid workflow before Linear command comments that would park or alter the issue. Countermeasure: keep the current daemon ordering in mind: command polling runs before candidate dispatch. The invalid-workflow check belongs inside dispatch after command processing, so same-tick `/scherzo park` can still prevent triage handling if it removes the issue from dispatch consideration.

## Progress

- [x] (2026-04-28 00:00Z) Agreed on the operating policy: use Linear labels for explicit workflow selection, use Linear state as the dispatch gate, and reserve blocked status for true external blockers.
- [x] (2026-04-28 00:00Z) Reviewed current dispatch code in `src/scherzo/orchestrator/core.gleam` and `src/scherzo/orchestrator/daemon.gleam`, current issue label normalization in `src/scherzo/linear.gleam`, and current config parsing in `src/scherzo/config.gleam`.
- [x] (2026-04-28 00:00Z) Ran the current baseline from the repository root with `direnv exec . gleam test`; it passed with `200 passed, no failures`.
- [x] (2026-04-28 00:00Z) Revised this plan after review to remove the hard dependency on the board-contract plan, make Linear mutations explicit opt-in, avoid reporting parked or otherwise non-dispatchable issues, and strengthen duplicate-report suppression.
- [ ] Add workflow-policy config fields and parser tests.
- [ ] Add pure workflow label selection tests.
- [ ] Add dispatch and retry enforcement.
- [ ] Add optional invalid-workflow Linear comment/state side effects.
- [ ] Update README, example workflow, and this plan's retrospective after validation.

## Surprises & Discoveries

- Observation: The daemon already has a command phase between candidate fetch and candidate dispatch.
  Evidence: `handle_candidate_fetch_finished` in `src/scherzo/orchestrator/daemon.gleam` calls `begin_linear_command_fetch_or_finish`, and `finish_linear_command_phase` dispatches carried candidates only after comment commands are processed.

- Observation: Current `core.should_dispatch` is a boolean predicate and cannot explain why an issue was skipped.
  Evidence: `dispatch_candidates` calls `core.should_dispatch(...) && can_reserve_dispatch_slot(...)`; false simply recurses to the next candidate.

- Observation: Runtime state already tracks parked issues by observed update time to avoid repeating work until an issue changes.
  Evidence: `domain.ParkedEntry` stores `observed_updated_at`, and `core.is_parked_without_update` prevents dispatch until the issue's `updated_at` differs.

- Observation: Reporting invalid workflow before checking the existing dispatch predicate would report issues that a same-tick Linear command has just parked.
  Evidence: `finish_linear_command_phase` calls `dispatch_candidates` after command processing, and `core.should_dispatch` currently suppresses parked issues through `is_parked_without_update`.

## Decision Log

- Decision: Enforce workflow labels before pi launch rather than asking pi to self-triage.
  Rationale: Missing or ambiguous workflow labels are scheduler input validation errors and should not consume agent resources.
  Date: 2026-04-28

- Decision: Make enforcement opt-in at first.
  Rationale: Existing users may not have workflow labels yet. Opt-in lets teams add labels, validate the board when that tooling is available, then turn on enforcement.
  Date: 2026-04-28

- Decision: Make Linear triage mutations separately opt-in.
  Rationale: Enabling scheduler validation should not unexpectedly create comments or move issues. Comments require `comment_on_invalid_workflow: true`; state movement requires `invalid_workflow_state_id`.
  Date: 2026-04-28

- Decision: Keep this plan self-contained even if the board-contract plan has not landed.
  Rationale: A future implementer should be able to enforce per-issue workflow labels from this plan alone. If `linear_contract` config already exists, extend it; otherwise add the minimal config shape here without implementing the full board-contract checker.
  Date: 2026-04-28

- Decision: Use labels as the first workflow carrier, not custom fields.
  Rationale: The current Linear provider already fetches labels and the prompt renderer already exposes `issue.labels`.
  Date: 2026-04-28

- Decision: Suppress repeated invalid-workflow report attempts by issue update timestamp plus label and violation fingerprints.
  Rationale: Operators need one clear message, not one comment per poll. `updated_at` alone is not enough when a provider supplies `None`, and a violation code alone cannot distinguish one unknown label from another.
  Date: 2026-04-28

- Decision: Report invalid workflow only for issues that are otherwise dispatchable.
  Rationale: Workflow triage should not override existing scheduler decisions such as parking, running, terminal state, blockers, or missing required fields. This keeps operator commands authoritative in the same poll tick.
  Date: 2026-04-28

- Decision: Keep automatic board reconciliation out of this phase.
  Rationale: Per-issue triage comments and state moves are issue handling; creating or changing Linear board labels/states is administrative reconciliation and needs a separate approval model.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled after implementation. Include the final test count, whether invalid issues were skipped without pi launch in daemon tests, whether duplicate invalid comments were suppressed, and any follow-up needed for exposing `issue.workflow` in templates.)

## Context and Orientation

Scherzo polls Linear, prepares one workspace per issue, and runs a pi coding-agent session in that workspace. Source code lives under `src/scherzo/`; tests live under `test/`; validation is run from the repository root.

The issue model is `domain.Issue` in `src/scherzo/domain.gleam`. It has `labels: List(String)` and `updated_at: Option(Time)`. Linear issue decoding in `src/scherzo/linear.gleam` normalizes label names to lowercase. The prompt renderer in `src/scherzo/template.gleam` exposes `issue.labels` to `WORKFLOW.md` templates.

The pure dispatch predicate is `core.should_dispatch` in `src/scherzo/orchestrator/core.gleam`. It currently checks required fields, active state, non-terminal state, not already running or claimed, not parked without update, available slots, and blockers. It does not check workflow labels.

The daemon dispatch loop is in `src/scherzo/orchestrator/daemon.gleam`. Candidate issues are fetched, Linear command comments are optionally processed, and then `dispatch_candidates` walks the sorted candidate list. `dispatch_issue_with_continuation` prepares the pending claim and enqueues a claim side effect before starting the worker. The invalid-workflow check must happen before this point.

Linear mutations already exist. `src/scherzo/linear.gleam` can build comment-create and issue-update-state mutations. `src/scherzo/handoff.gleam` uses those for run claim/success/failure. Linear command acknowledgements use `linear.CommandClient.post_ack`. This plan can add a small triage client instead of making pi or local hooks mutate Linear.

## Preconditions and Verified Facts

This plan may be implemented before or after the read-only board contract plan. If `domain.LinearContractConfig` and `effective.linear_contract` already exist, extend them with the dispatch-policy fields described below. If they do not exist, add `LinearContractConfig` and `effective.linear_contract` in this plan with the full field set listed in Interfaces and Dependencies. In that case, only the workflow-label dispatch fields are used at runtime; the full read-only contract-check command remains out of scope.

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

On 2026-04-28 while writing this plan, `direnv exec . gleam test` ended with `200 passed, no failures`.

Current repository facts this plan depends on:

- In the tree reviewed for this plan, `domain.EffectiveConfig` did not yet contain `linear_contract`; if that differs when implementation begins, extend the existing config instead of adding a duplicate section.
- `domain.Issue.labels` is a list of strings and Linear labels are lowercased by the decoder.
- `domain.Issue.updated_at` is available for in-memory duplicate suppression.
- `core.should_dispatch` and `core.handle_retry_candidate` are pure and currently return only boolean or transition results, not detailed skip reasons.
- `daemon.dispatch_candidates` is the place where candidate issues are walked and dispatch side effects are enqueued.
- `daemon.handle_retry_refresh_finished` is the place where retry issue refresh results are turned into retry dispatch decisions.
- `linear.build_comment_create_request` and `linear.build_issue_update_state_request` already exist.
- Handoff comments and Linear command acknowledgements demonstrate existing patterns for enqueueing Linear mutation side effects.

If these facts differ when implementation begins, update this plan first.

## Scope Boundaries

In scope: adding or extending `linear_contract` config fields for issue workflow enforcement; pure workflow label selection; dispatch guard integration; retry guard integration; optional invalid-workflow comment; optional invalid-workflow state update by configured Linear state ID; duplicate suppression for unchanged invalid issues; extending the read-only contract check only if it already exists; deterministic tests; README and example workflow updates.

Out of scope: implementing the full read-only Linear board contract checker if it has not landed yet; creating Linear labels or states; inferring workflow labels; editing Linear issue descriptions; custom fields; durable invalid-workflow receipts across daemon restart; adding a template variable for selected workflow; changing pi behavior; webhooks; blocking all dispatch because one issue is invalid; replacing existing handoff success/failure behavior.

## Milestones

Milestone 1 makes configuration self-contained and adds pure policy. At the end, tests can parse enforcement config whether or not the board-contract plan already added `linear_contract`, and pure tests can classify issues as selected, missing, multiple, or unknown without running a daemon.

Milestone 2 adds non-mutating dispatch enforcement. At the end, `core.should_dispatch` and retry dispatch refuse invalid issues when enforcement is enabled, daemon reporting is limited to otherwise-dispatchable issues, and tests prove invalid issues cannot reach `Dispatch` effects or workspace preparation.

Milestone 3 adds invalid-workflow report-attempt state. At the end, daemon tests can show one invalid candidate produces one report attempt for a given update timestamp plus label/violation fingerprint, repeated polls do not duplicate it, and changing the issue permits a new report or valid dispatch.

Milestone 4 adds Linear triage comments and optional state movement. At the end, fake Linear mutation tests prove Scherzo formats the right comment, posts it only when configured, updates the configured state ID when set, treats a no-op report configuration as successful, and logs mutation failures without launching pi.

Milestone 5 updates documentation and validates the whole phase. At the end, README and `examples/WORKFLOW.md` explain how to configure workflow labels, how invalid issues are handled, how to run the board contract check if that command exists, and how to manually validate state IDs before enabling state movement if it does not.

## Plan of Work

Add or extend the `LinearContractConfig` type. If the board-contract work has already introduced it, add only these dispatch-policy fields:

    enforce_issue_workflow_labels: Bool
    invalid_workflow_state_id: Option(String)
    comment_on_invalid_workflow: Bool

If `LinearContractConfig` does not exist yet, add it with the complete field set listed in Interfaces and Dependencies so this plan remains compatible with the later board-contract work. Defaults for the dispatch-policy fields are `False`, `None`, and `False`. If `enforce_issue_workflow_labels` is true, config validation must require at least one allowed workflow label in `workflow_labels` and a non-empty `workflow_label_prefix`. Normalize `workflow_label_prefix` and `workflow_labels` by trimming whitespace and lowercasing them during config resolution. `invalid_workflow_state_id` is optional because some operators may want log-only or comment-only enforcement before granting Scherzo state mutation privileges.

Parse YAML equivalent to:

    linear_contract:
      enabled: true
      workflow_label_prefix: "workflow:"
      workflow_labels: [bugfix, feature, research, review, docs, chore]
      support_labels: [needs-workflow, needs-clarification]
      required_states:
        needs_workflow: "Needs Workflow"
        ready: "Ready for Agent"
        in_progress: "In Progress"
        done: "Done"
      handoff_state_bindings:
        claim: in_progress
        success: done
      enforce_issue_workflow_labels: true
      invalid_workflow_state_id: "linear-state-id-for-needs-workflow"
      comment_on_invalid_workflow: true

When adding `LinearContractConfig` from scratch, parse `enabled` as a boolean defaulting to false, `workflow_label_prefix` as a trimmed lowercase string defaulting to `workflow:`, `workflow_labels` as trimmed lowercase suffixes, `support_labels` as trimmed lowercase full label names, and `required_states` plus `handoff_state_bindings` as string maps. Reject malformed non-string entries in these lists or maps with `error.InvalidConfig(_)` rather than silently dropping them. The semantic validation required by this plan is limited to dispatch policy: enforcement requires a non-empty prefix and at least one workflow label. Full board-contract semantics, such as whether a handoff binding points at a known required state, belong to the board-contract checker if that checker is implemented.

If `src/scherzo/linear_contract.gleam` and the `--linear-contract-check` mode exist when this plan is implemented, update that check so `invalid_workflow_state_id`, when configured, is validated as an existing state ID. If `required_states.needs_workflow` exists, the check should report a name mismatch when `invalid_workflow_state_id` points at a different state name. If the contract-check module and CLI do not exist yet, do not implement the full checker in this plan; instead document that operators must manually verify the state ID in Linear before enabling state movement.

Create `src/scherzo/workflow_policy.gleam`. Define variants equivalent to:

    pub type IssueWorkflowDecision {
      WorkflowPolicyDisabled
      WorkflowSelected(name: String, label: String)
      WorkflowInvalid(violation: IssueWorkflowViolation)
    }

    pub type IssueWorkflowViolation {
      MissingWorkflowLabel
      MultipleWorkflowLabels(labels: List(String))
      UnknownWorkflowLabel(label: String)
    }

Expose `classify_issue(config: domain.LinearContractConfig, issue: domain.Issue) -> IssueWorkflowDecision`, `violation_code`, `violation_fingerprint`, `violation_message`, and `observed_labels_fingerprint`. The classifier should trim and lowercase labels, match labels that begin with the normalized configured prefix, remove the prefix to get the workflow suffix, and compare suffixes to normalized configured allowed workflow labels. If no workflow-like labels are present, return missing. If more than one workflow-like label is present, return multiple, even if one is allowed and one is unknown. If exactly one workflow-like label is present but its suffix is not allowed, return unknown. If enforcement is disabled, return `WorkflowPolicyDisabled`. The violation fingerprint should include the violation code and offending labels so `unknown workflow:foo` and `unknown workflow:bar` are not treated as the same unchanged problem when `updated_at` is absent.

Add tests in `test/workflow_policy_test.gleam`. Include selected workflow, missing label, multiple workflow labels, unknown workflow label, case normalization, prefix-boundary behavior, and disabled policy. Prefix-boundary behavior means `workflowish:bugfix` does not match prefix `workflow:`, while `workflow:bugfix` does.

Integrate the policy into `src/scherzo/orchestrator/core.gleam`. Import `scherzo/workflow_policy` and factor the existing non-workflow checks into helpers such as `dispatch_preconditions_satisfied(state, config, issue)` and `retry_candidate_preconditions_satisfied(state, config, issue_id, issue)`. Then make `core.should_dispatch` and the internal retry dispatch guard require both the non-workflow preconditions and `workflow_policy_satisfied(config, issue)`. `workflow_policy_satisfied` returns true for `WorkflowPolicyDisabled` and `WorkflowSelected`, false for `WorkflowInvalid`. This ensures no caller can accidentally dispatch an invalid issue while still giving the daemon a way to ask whether an issue was skipped for a non-workflow reason before reporting triage. Add core tests in `test/orchestrator_core_test.gleam` showing missing, multiple, and unknown workflow labels return false when enforcement is enabled, selected labels return true when all other preconditions pass, parked issues remain false without requiring workflow reporting, and enforcement disabled preserves current behavior.

Add daemon-side reporting before ordinary candidate dispatch but after non-workflow eligibility is known. In `dispatch_candidates`, first check `core.dispatch_preconditions_satisfied(state.runtime, state.effective, issue)` and `can_reserve_dispatch_slot(state, issue)`. If either is false, recurse to the remaining candidates without reporting invalid workflow. If both are true, call `workflow_policy.classify_issue(state.effective.linear_contract, issue)`. If it returns `WorkflowInvalid`, call a new helper such as `handle_invalid_workflow_candidate(state, issue, violation, remaining_candidates)`. That helper records the invalid observation, enqueues reporting side effects if this issue/update/label/violation has not already been attempted, logs a structured first-seen invalid-workflow event, and then continues dispatching the remaining candidates. If the decision is disabled or selected, dispatch the issue through the existing `dispatch_issue_with_continuation` path.

Add an invalid-workflow observation map to runtime or daemon state. Prefer adding it to `domain.RuntimeState` so it is visible in snapshots and tested with pure transition helpers if needed. The entry should store `issue_id`, `identifier`, `violation_code`, `violation_fingerprint`, `observed_updated_at`, `observed_labels_fingerprint`, `attempted_at_ms`, and `last_result`. A helper such as `already_attempted_invalid_workflow(state.runtime, issue, violation)` should return true only when the stored `observed_updated_at`, observed label fingerprint, and violation fingerprint all match the current issue and violation. If the issue timestamp changes, labels change, or the offending labels change while `updated_at` is `None`, Scherzo may report again.

Handle retry refresh results. In `daemon.handle_retry_refresh_finished`, after building `candidate = Ok(Some(issue))` and before `retry_candidate_needs_slot_retry` or `core.handle_retry_candidate`, use `core.retry_candidate_preconditions_satisfied(state.runtime, state.effective, issue_id, issue)` to confirm the refreshed issue is the retry target and is otherwise eligible apart from workflow policy and slot availability. If those preconditions are false, keep the existing retry handling. If they are true and `workflow_policy.classify_issue` returns `WorkflowInvalid`, call a pure helper such as `core.stop_retry_for_policy_invalid(state.runtime, issue_id)` that clears the retry entry, emits `CancelRetry(issue_id)`, releases any retained claim with `ReleaseClaim(issue_id)`, records/reports the invalid workflow violation once, and does not schedule another retry. The issue should be eligible again only when it appears as a fresh candidate after Linear state or labels change.

Create a reporting side effect. Add a triage client type, either in a new `src/scherzo/linear_triage.gleam` module or in `src/scherzo/linear.gleam`, with a function equivalent to:

    report_invalid_workflow: fn(domain.Issue, workflow_policy.IssueWorkflowViolation, domain.LinearContractConfig) -> Result(Nil, error.TrackerError)

The implementation should, depending on config, post a comment, update the issue state, both, or neither. When both are configured, post the explanatory comment first and then update the state; if the state update fails after the comment succeeds, return an error so the daemon logs `invalid_workflow_report_failed`, but keep dispatch suppressed and keep duplicate-attempt suppression for the unchanged issue. It should use `linear.build_comment_create_request`, `linear.build_issue_update_state_request`, and `linear.parse_mutation_response`. The daemon should hold a fakeable client in `RuntimeDependencies`, similar to `make_handoff` and `make_linear_commands`.

The invalid-workflow comment body should be deterministic and concise. For a missing label:

    Scherzo did not dispatch this issue because it has no workflow label.

    Expected exactly one of:
    - workflow:bugfix
    - workflow:feature
    - workflow:research
    - workflow:review
    - workflow:docs
    - workflow:chore

    Add exactly one workflow label, then move the issue back to Ready for Agent.

For multiple labels, include the labels found. For an unknown label, include the unknown label and the allowed labels. Do not include secrets or large issue descriptions.

Update side-effect handling in `src/scherzo/orchestrator/daemon.gleam` to run the report and log either `invalid_workflow_reported`, `invalid_workflow_report_noop`, or `invalid_workflow_report_failed`. A failed report must not dispatch the issue. It should be logged and suppressed for the same unchanged issue to avoid a tight failure loop unless the operator changes the issue, reloads/restarts the daemon, or a later durable receipt mechanism is added.

Update README and `examples/WORKFLOW.md`. The example should include `linear_contract` and prompt instructions that tell pi to follow the workflow label. Document that enforcement is opt-in, comments and state moves are separately opt-in, labels must already exist in Linear, and `--linear-contract-check` should be run before enabling state moves only if that command exists. If the command does not exist yet, document manual state-ID verification before setting `invalid_workflow_state_id`.

## Concrete Steps

From the repository root, run the baseline:

    direnv exec . gleam test

Expect the final line to be similar to:

    200 passed, no failures

Implement the config extension in `src/scherzo/domain.gleam` and `src/scherzo/config.gleam`. If `LinearContractConfig` already exists, add only the dispatch-policy fields. If it does not exist, add the full `LinearContractConfig` shape from Interfaces and Dependencies and add `linear_contract` to `domain.EffectiveConfig`. Add tests to `test/config_test.gleam` for defaults, valid enforcement parsing, lowercasing and trimming of prefix and labels, `comment_on_invalid_workflow` defaulting to false, and invalid `enforce_issue_workflow_labels: true` with an empty allowed list or empty prefix.

Create `src/scherzo/workflow_policy.gleam` and `test/workflow_policy_test.gleam`. Run:

    direnv exec . gleam test

Implement the hard dispatch guard and the non-workflow precondition helpers in `src/scherzo/orchestrator/core.gleam`. Update `test/orchestrator_core_test.gleam` with cases proving invalid issues do not dispatch when enforcement is enabled, selected workflow labels dispatch when all other preconditions pass, enforcement disabled preserves current behavior, and parked issues remain suppressed through the non-workflow precondition helper.

Add invalid-workflow report-attempt state to `src/scherzo/domain.gleam` and initialize it in `core.new_state`. Add pure helpers needed to mark attempts, store `violation_fingerprint` and `observed_labels_fingerprint`, and check duplicate-attempt suppression.

Modify `src/scherzo/orchestrator/daemon.gleam` so candidate dispatch reports invalid workflow issues only after the non-workflow dispatch preconditions and daemon slot reservation check pass. Add daemon integration tests using existing fake dependency patterns to prove no worker is started for invalid candidates and no invalid-workflow report is attempted for a parked or otherwise non-dispatchable issue.

Modify retry handling so invalid retry candidates that are otherwise ready for retry dispatch do not schedule another retry loop. Add tests for an invalid retry refresh if the current test harness supports retry ticks; otherwise add a pure core/helper test for `stop_retry_for_policy_invalid` and a daemon test for candidate dispatch.

Create the Linear triage reporting client and side effect. Add fake transport tests proving no mutation is built when both comment and state movement are disabled, one comment mutation is built when `comment_on_invalid_workflow` is true, the optional state update request is built after the comment when a state ID is configured, and failures return `Error(_)` without leaking secrets.

Wire the client into daemon dependencies and side-effect execution. Add daemon tests for no-op, comment, state actions, failure logging, and duplicate suppression across repeated polls with the same issue `updated_at`, labels, and violation fingerprint.

Update README and `examples/WORKFLOW.md` with the new config and operational guidance, including the fact that comments and state movement are separate opt-ins and that state IDs must be checked manually if `--linear-contract-check` is not available.

Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Commit after tests pass. Suggested commit message:

    Enforce workflow labels before Linear dispatch

Update this plan's Progress and Outcomes sections.

## Testing and Falsifiability

Add pure workflow policy tests. Inputs should construct `domain.Issue` values with label lists such as `[]`, `["workflow:bugfix"]`, `["workflow:bugfix", "workflow:research"]`, `["workflow:surprise"]`, `["Workflow:Bugfix"]`, and a case with `updated_at: None`. Assertions should check selected names, violation codes, violation fingerprints, and observed label fingerprints. A disabled config should return `WorkflowPolicyDisabled` and never invalid.

Add config tests. A valid enforcing config should parse prefix `workflow:`, allowed names, `invalid_workflow_state_id`, and `comment_on_invalid_workflow`. Defaults should set `enforce_issue_workflow_labels == False`, `invalid_workflow_state_id == None`, and `comment_on_invalid_workflow == False`. Config parsing should trim and lowercase the prefix and workflow labels. An invalid enforcing config with no allowed workflow labels or an empty prefix should return `error.InvalidConfig(_)`. A non-enforcing config should allow empty workflow label lists for backward compatibility.

Add core dispatch tests. With enforcement enabled and active state `Ready for Agent`, a candidate with `workflow:bugfix` should satisfy `core.should_dispatch` when all other preconditions pass. The same candidate without labels, with two workflow labels, or with `workflow:unknown` should not satisfy `core.should_dispatch`. With enforcement disabled, the existing behavior should remain unchanged. Add a parked or already-running issue with no workflow label and assert the non-workflow precondition helper returns false so daemon reporting will skip it rather than triage it.

Add daemon tests. One test should feed a candidate in an active state with no workflow label and assert no worker is started, no workspace is prepared, and one invalid-workflow report side effect is requested. A second test should park the same candidate through existing state or command-processing helpers and assert no invalid-workflow report is requested because the issue is not otherwise dispatchable. A third test should run another poll with the same issue ID, same `updated_at`, same labels, and same violation fingerprint and assert no second comment is posted. A fourth test should update the issue `updated_at` and keep it invalid, then assert a second report is allowed. A fifth test should keep `updated_at: None` but change `workflow:surprise` to `workflow:other` and assert a second report is allowed because the violation fingerprint changed. A sixth test should add `workflow:bugfix` and assert the issue can dispatch normally.

Add Linear triage client tests. For `comment_on_invalid_workflow: false` and no state ID, assert the report is a no-op that returns `Ok(Nil)` and builds no mutation while the daemon still skips dispatch. For `comment_on_invalid_workflow: true` and no state ID, assert exactly one comment mutation is built. For a configured `invalid_workflow_state_id`, assert an issue update mutation is also built after the comment when comments are enabled, or as the only mutation when comments are disabled. Add a failure test where the state update fails after the comment succeeds and assert the client returns `Error(_)` so the daemon logs failure without dispatching.

The plan is falsified if any invalid issue can reach `dispatch_issue_with_continuation`, workspace preparation, or pi launch while enforcement is enabled. It is also falsified if a parked or otherwise non-dispatchable issue receives invalid-workflow triage, if a repeated unchanged invalid issue posts a new comment every poll, if a changed violation with `updated_at: None` is suppressed forever, if enforcement alone mutates Linear without explicit comment or state config, or if disabling enforcement changes current dispatch behavior.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expect all tests to pass. The test count should be higher than the baseline `200 passed, no failures`.

Run help to ensure unrelated modes still work:

    direnv exec . gleam run -- --help

If `--linear-contract-check` exists in the tree where this plan is implemented, also run it against a workflow configured for a safe test board:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check path/to/WORKFLOW.md

On a real test Linear board, create an issue in `Ready for Agent` with no `workflow:*` label. Run Scherzo once against a workflow with enforcement enabled and a fake-safe workspace hook. Expected behavior: Scherzo does not launch pi, posts or logs the invalid workflow message according to config, and moves the issue to the configured `Needs Workflow` state if `invalid_workflow_state_id` is set. Then add exactly one allowed workflow label and move the issue back to `Ready for Agent`; the next run should dispatch normally.

## Rollout, Recovery, and Idempotence

Roll out in stages. First add the `linear_contract` allowed workflow labels. If `--linear-contract-check` exists, run it until the board contains all required labels and states; otherwise manually verify in Linear that the labels exist and that any state ID to be configured points to the intended `Needs Workflow` state. Then enable `enforce_issue_workflow_labels: true` with `comment_on_invalid_workflow: false` and no `invalid_workflow_state_id` to observe log-only behavior. Next enable `comment_on_invalid_workflow: true` if comments are desired. Finally configure `invalid_workflow_state_id` only after verifying it points to the intended non-dispatch state.

If enforcement blocks too much work, set `enforce_issue_workflow_labels: false` and reload the workflow. Existing running workers are unaffected by this validation because it happens before dispatch. If the optional state update moves an issue incorrectly, move it back manually in Linear and correct `invalid_workflow_state_id`; no board structure was changed by this plan.

Reporting is idempotent only within one daemon run and one unchanged issue update timestamp plus unchanged label and violation fingerprints. After daemon restart, Scherzo may report an unchanged invalid issue again. Durable invalid-workflow receipts are intentionally deferred.

## Artifacts and Notes

Representative structured logs for an invalid issue should look like:

    level=warn service=scherzo event=issue_workflow_invalid issue_id=... identifier=ABC-123 violation=missing_workflow_label
    level=info service=scherzo event=invalid_workflow_reported issue_id=... identifier=ABC-123 action=comment_and_state

A failed report should look like:

    level=warn service=scherzo event=invalid_workflow_report_failed issue_id=... identifier=ABC-123 error=linear_api_status

The Linear comment should be human-readable and avoid raw JSON, secrets, large descriptions, or stack traces.

Plan revision note, 2026-04-28: this plan was revised after review to remove the hard dependency on the board-contract plan, make comment and state mutations explicit opt-ins, prevent invalid-workflow reporting for parked or otherwise non-dispatchable issues, specify retry cancellation and claim release for policy-invalid retries, and strengthen duplicate suppression with label and violation fingerprints. These changes reduce mutation surprise, preserve operator command ordering, and make the plan executable against the current repository shape.

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, add or extend `LinearContractConfig`. If the type already exists from the board-contract work, add the dispatch-policy fields shown at the end of this block. If the type does not exist yet, define it with the full shape below and add `linear_contract: LinearContractConfig` to `EffectiveConfig`:

    pub type LinearContractConfig {
      LinearContractConfig(
        enabled: Bool,
        workflow_label_prefix: String,
        workflow_labels: List(String),
        support_labels: List(String),
        required_states: Dict(String, String),
        handoff_state_bindings: Dict(String, String),
        enforce_issue_workflow_labels: Bool,
        invalid_workflow_state_id: Option(String),
        comment_on_invalid_workflow: Bool,
      )
    }

Defaults are `enabled: False`, `workflow_label_prefix: "workflow:"`, empty lists/maps for board-contract fields, `enforce_issue_workflow_labels: False`, `invalid_workflow_state_id: None`, and `comment_on_invalid_workflow: False`.

Add `src/scherzo/workflow_policy.gleam` with public functions equivalent to:

    pub fn classify_issue(domain.LinearContractConfig, domain.Issue) -> IssueWorkflowDecision
    pub fn violation_code(IssueWorkflowViolation) -> String
    pub fn violation_fingerprint(IssueWorkflowViolation) -> String
    pub fn observed_labels_fingerprint(domain.Issue) -> String
    pub fn violation_message(IssueWorkflowViolation, domain.LinearContractConfig) -> String
    pub fn allowed_label_names(domain.LinearContractConfig) -> List(String)

Add invalid-workflow report-attempt state to `domain.RuntimeState`, for example:

    invalid_workflow_reports: Dict(String, InvalidWorkflowReport)

    pub type InvalidWorkflowReport {
      InvalidWorkflowReport(
        issue_id: String,
        identifier: String,
        violation_code: String,
        violation_fingerprint: String,
        observed_updated_at: Option(Time),
        observed_labels_fingerprint: String,
        attempted_at_ms: Int,
        last_result: String,
      )
    }

Add a fakeable triage reporting client, either in a new `src/scherzo/linear_triage.gleam` module or in `src/scherzo/linear.gleam`, with functions equivalent to:

    pub type TriageClient {
      TriageClient(report_invalid_workflow: fn(domain.Issue, workflow_policy.IssueWorkflowViolation, domain.LinearContractConfig) -> Result(Nil, error.TrackerError))
    }

    pub fn triage_client(domain.TrackerConfig, linear.Transport) -> TriageClient
    pub fn real_triage_client(domain.TrackerConfig) -> TriageClient

Extend `daemon.RuntimeDependencies` with `make_triage` or an equivalent dependency so daemon tests can provide a fake reporter without network access.
