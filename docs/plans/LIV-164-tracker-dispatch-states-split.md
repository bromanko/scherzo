# Split Tracker Dispatch States from Lifecycle Active States

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo operators can use Linear board states for both human work and Scherzo work without Scherzo claiming or triaging human-managed issues by mistake. In particular, the dogfood configuration can keep `tracker.active_states: [Todo, In Progress]` so Scherzo still understands that already-owned work in **In Progress** is lifecycle-active, while adding `tracker.dispatch_states: [Todo]` so Scherzo only discovers and claims new work from **Todo**. A human moving an issue to **In Progress** should no longer cause invalid-workflow triage or workflow dispatch.

The visible operator outcome is a breaking but safe configuration migration. Existing configurations that omit `tracker.dispatch_states` must fail startup and `scherzo doctor --check workflow-config` must explain exactly what to add. Once the operator adds `dispatch_states`, Scherzo should query Linear, dispatch, and invalid-workflow-triage only issues in those dispatch states, while retry and final-state classification continue to treat all `active_states` as active lifecycle states.

## Problem Framing and Constraints

Scherzo currently overloads `tracker.active_states` for two different concepts. The first concept is lifecycle activity: states where Scherzo-owned work is still not terminal and retry or recovery logic should continue to reason about it as active. The second concept is dispatch eligibility: states from which Scherzo may discover unclaimed work and start agents. Those concepts diverge in the dogfood board because `Todo` and `In Progress` are both lifecycle-active, but only `Todo` should be a source of new Scherzo dispatch candidates.

Today, if a human manually moves an issue to **In Progress**, Scherzo can interpret it as a candidate. If the issue has no `workflow:*` label, invalid-workflow enforcement may comment and move it to Triage. If the issue has a valid workflow label, Scherzo may dispatch it. That makes normal human board operations unsafe.

This plan must not implement the change. It specifies a later implementation. The implementation must be a deliberate breaking configuration migration: `tracker.dispatch_states` is required, non-empty, and a normalized subset of `tracker.active_states`. It must not silently default to `active_states`, because silent defaulting would preserve the unsafe behavior for existing installs.

The implementation must preserve invalid-workflow enforcement for true dispatch candidates. An unlabeled **Todo** issue should still be triaged according to the existing invalid-workflow policy. An unlabeled **In Progress** issue should be ignored for dispatch and invalid-workflow triage when `dispatch_states` is `[Todo]`.

## Strategy Overview

Add a new `dispatch_states` field to the tracker configuration model and make the YAML loader require it. Use this field for every path that fetches or filters new dispatch candidates. Keep `active_states` for lifecycle decisions such as retry eligibility, state recovery, and final classification.

The right-sized implementation is a focused split at the configuration boundary plus targeted call-site changes. It does not remove `active_states`, change Linear state names, redesign the dispatcher, or remove invalid-workflow enforcement. It adds exactly one new operator-facing configuration setting and then routes existing decisions through the correct state set.

The hardest correctness risk is not the Linear query itself; it is defensive behavior when a tracker implementation returns extra issues. Even after `linear.fetch_candidate_issues` is changed to query `dispatch_states`, daemon/service/core logic must independently refuse to dispatch or invalid-workflow-triage initial candidates whose current state is outside `dispatch_states`. That guard makes the behavior safe if Linear pagination, tests, fakes, or future trackers return a broader active-state list.

## Alternatives Considered

The simplest possible alternative is to tell operators to remove **In Progress** from `active_states`. That is insufficient because retry, recovery, and final classification still need to know that Scherzo-owned work in **In Progress** is lifecycle-active.

Another alternative is to leave configuration unchanged and add ad hoc code that excludes **In Progress** from candidate queries. That is too specific to the dogfood board and would make state policy implicit in code instead of explicit in operator configuration.

A third alternative is to introduce `dispatch_states` but default it to `active_states` for backward compatibility. This is intentionally rejected. The current default would keep existing unsafe behavior after upgrade, so the migration must fail closed and force operators to choose their dispatch states.

## Risks and Countermeasures

The main rollout risk is a breaking configuration change. Existing installations will fail until they add `tracker.dispatch_states`. This is desired, but only safe if the error is specific and actionable. The implementation must add tests proving both config loading and doctor output mention `tracker.dispatch_states`, explain that it is required, and show a remediation such as `dispatch_states: [Todo]` under `tracker`.

A second risk is accepting malformed YAML during a fail-closed migration. The existing permissive list helper in `src/scherzo/config.gleam` filters non-string list entries and treats wrong node types like missing values. `tracker.dispatch_states` must instead use strict list-of-strings parsing: a scalar such as `dispatch_states: Todo` must fail with `tracker.dispatch_states must be a string list`, and a mixed list such as `dispatch_states: [Todo, 123]` must fail with `tracker.dispatch_states entries must be strings`.

A third risk is allowing normalized spelling to pass validation while sending a non-canonical state name to Linear. `src/scherzo/tracker/state.gleam` keeps both a trimmed raw state string and a normalized key, and `src/scherzo/linear.gleam` serializes state filters from the raw strings. The implementation must canonicalize each accepted `dispatch_states` entry to the matching `active_states` raw value before storing it in `TrackerConfig`, so `dispatch_states: [" todo "]` with `active_states: [Todo]` loads as the canonical `Todo` and Linear requests send `Todo`, not `todo` or ` todo `.

A fourth risk is accidentally changing lifecycle behavior. If every use of `active_states` is mechanically replaced with `dispatch_states`, Scherzo could misclassify **In Progress** as inactive and mishandle retries or final classification. The plan explicitly keeps `active_states` in lifecycle code, including `src/scherzo/agent/run_attempt.gleam`, and adds a regression test proving **In Progress** remains active for retry/final classification.

A fifth risk is only changing the Linear query and forgetting invalid-workflow enforcement. If a fake tracker, a future tracker, or a stale page returns **In Progress**, Scherzo might still triage or dispatch it. The daemon/service candidate loops and any shared core predicate they call must add an explicit `dispatch_states` guard before invalid-workflow reporting, workflow selection, claim validation, or workflow dispatch.

A sixth risk is incomplete test fixture migration after adding a field to `TrackerConfig`. Many tests construct `config_types.TrackerConfig` directly or embed YAML strings containing only `active_states`. The implementation should expect compile failures and use them as an inventory. The migration is complete only when every direct tracker config constructor and YAML fixture either includes `dispatch_states` or intentionally tests the missing-field failure.

## Progress

- [x] (2026-05-08 00:00Z) Drafted this ExecPlan from LIV-164 and inspected the current repository for the main edit points.
- [x] (2026-05-08 01:00Z) Incorporated adversarial review findings about canonical state names, strict YAML parsing, daemon-level invalid-workflow coverage, candidate-loop call sites, and exact doctor acceptance.
- [x] (2026-05-08 16:15Z) Implemented the configuration model and validation changes: `TrackerConfig.dispatch_states`, required strict YAML parsing, non-empty and subset validation, and canonicalization against `active_states`.
- [x] (2026-05-08 16:15Z) Updated doctor/startup error reporting so `tracker.dispatch_states` failures preserve actionable messages such as the required setting and `dispatch_states: [Todo]` remediation.
- [x] (2026-05-08 16:15Z) Switched dispatch candidate fetching and Linear smoke sampling to `dispatch_states`, and added daemon/service/core dispatch guards before invalid-workflow triage, claim validation, workflow selection, or dispatch.
- [x] (2026-05-08 16:15Z) Updated Linear contract validation to check `tracker.dispatch_states` and migrated the dogfood configuration to `dispatch_states: [Todo]` while keeping `active_states: [Todo, In Progress]`.
- [x] (2026-05-08 16:15Z) Added and updated parser, doctor, Linear request, daemon guard, lifecycle, contract, fixture, and constructor tests; final formatting, test, glinter, and Scherzo lint validation is recorded below in Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: `src/scherzo/linear.gleam` currently implements `fetch_candidate_issues` by calling `fetch_pages(config, config.active_states, None, transport, [])`.
  Evidence: repository inspection of `src/scherzo/linear.gleam`.
- Observation: `src/scherzo/smoke.gleam` also references `config.active_states` when fetching one Linear page, so the Linear smoke/doctor path must be reviewed when the dispatch split is implemented.
  Evidence: repository search for `active_states` in `src`.
- Observation: `src/scherzo/linear_contract.gleam` validates Linear state existence for `tracker.active_states`; the implementation must add equivalent validation for `tracker.dispatch_states`.
  Evidence: repository search for `active_states` in `src/scherzo/linear_contract.gleam`.
- Observation: The dogfood config currently has `tracker.active_states: [Todo, In Progress]` and no `tracker.dispatch_states`.
  Evidence: repository search found `.scherzo/scherzo.yaml` with that active state list.
- Observation: `src/scherzo/tracker/state.gleam` stores a trimmed raw state string and a normalized key. `issue_state.to_strings` emits the raw strings, so normalized subset validation alone is not enough for Linear queries.
  Evidence: repository inspection of `src/scherzo/tracker/state.gleam` and the existing `linear.gleam` request serialization path.
- Observation: `src/scherzo/config.gleam` has both a permissive `get_string_list` helper and stricter helpers such as `get_contract_string_list` and `read_string_values`.
  Evidence: repository inspection of `src/scherzo/config.gleam`.
- Observation: Initial candidate loops exist in both `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/service.gleam`; daemon invalid-workflow reporting is performed through `handle_invalid_workflow_candidate`.
  Evidence: repository search for `dispatch_candidates` and `handle_invalid_workflow_candidate`.
- Observation: Plan-completion verification failed closed even though implementation files already contained the required dispatch-state split, because this living ExecPlan still showed required implementation items unchecked and had no completion retrospective.
  Evidence: `tmp/scherzo-plan-completion-verdict.json` reported `verdict: "fail"` and named only the stale Progress checklist and placeholder Outcomes & Retrospective as blocking findings.

## Decision Log

- Decision: `tracker.dispatch_states` is required in YAML and must not default to `tracker.active_states`.
  Rationale: A silent default would keep the unsafe behavior that this work is meant to stop.
  Date: 2026-05-08
- Decision: Accepted `dispatch_states` entries are canonicalized to the matching `active_states` raw state value before storage and before Linear query serialization.
  Rationale: Operators should get normalized comparison for harmless case or whitespace differences, but Linear should receive the board's canonical state name instead of a non-canonical spelling from YAML.
  Date: 2026-05-08
- Decision: `tracker.dispatch_states` uses strict YAML parsing and rejects wrong node types and non-string list entries.
  Rationale: A breaking safety migration must fail clearly rather than silently filtering malformed operator input.
  Date: 2026-05-08
- Decision: Invalid-workflow dispatch-state guard tests must exercise the daemon-level side effect path, not only a pure core predicate.
  Rationale: The safety property is that Scherzo does not enqueue or perform `ReportInvalidWorkflow`, claim validation, or dispatch for non-dispatch-state initial candidates.
  Date: 2026-05-08
- Decision: Keep `tracker.active_states` as the lifecycle-active set and use `tracker.dispatch_states` only for candidate discovery, initial dispatch, and invalid-workflow triage.
  Rationale: The board needs **In Progress** to remain active for existing Scherzo-owned work, retries, and final classification, but not eligible as new work.
  Date: 2026-05-08
- Decision: Add defensive daemon/service/core guards even though Linear candidate fetching will query only dispatch states.
  Rationale: Initial candidate loops should fail safe if a tracker implementation or test fake returns extra active-state issues.
  Date: 2026-05-08
- Decision: Treat the plan-completion repair as a living-document-only change.
  Rationale: The verifier's blocking findings identified stale ExecPlan Progress and Outcomes sections, not missing production or test behavior; broadening into code cleanup would violate the repair scope.
  Date: 2026-05-08

## Outcomes & Retrospective

Completed on 2026-05-08. The implementation split lifecycle-active states from dispatch-eligible states as intended. `tracker.dispatch_states` is now a required, strictly parsed tracker configuration field; it must be non-empty, must be a normalized subset of `tracker.active_states`, and accepted entries are canonicalized to the active-state spelling before storage so Linear queries use canonical board names.

The operator-facing migration is fail-closed. Old configs without `tracker.dispatch_states` now fail configuration loading and doctor workflow-config checks with actionable messages that name `tracker.dispatch_states` and show the `dispatch_states: [Todo]` remediation. The dogfood config was migrated to keep `active_states: [Todo, In Progress]` while adding `dispatch_states: [Todo]`.

Candidate discovery now uses dispatch states: Linear candidate fetching and the Linear smoke sample use `config.dispatch_states`, and the daemon and service initial candidate loops defensively skip non-dispatch-state issues before invalid-workflow triage, claim validation, workflow selection, or dispatch. Lifecycle behavior remains separated: active-state checks still use `tracker.active_states`, preserving **In Progress** as active for retry, recovery, and final classification.

Linear contract validation now checks `tracker.dispatch_states` and reports failures against that source. Tests were added or updated for strict config parsing, doctor error messages, canonicalized Linear request state filters, daemon-level invalid-workflow and dispatch guards, lifecycle-active regression behavior, contract validation, YAML fixtures, and direct `TrackerConfig` constructors.

Validation was run from the repository root through direnv after the plan-completion repair: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` all exited successfully. `gleam test` reported 921 passed and no failures. The lint commands reported the existing warning inventory with 0 errors.

No known implementation gaps remain for this ExecPlan. The only plan-completion failure found after implementation was that this living document had not been updated to reflect completed work; this repair updates the Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections without changing production code.

## Context and Orientation

This repository is a Gleam project. The relevant configuration types live in `src/scherzo/config/types.gleam`, and the YAML-to-config resolver lives in `src/scherzo/config.gleam`. The tracker configuration type is `config_types.TrackerConfig`. It currently contains `kind`, `endpoint`, `api_key`, `project_slug`, `active_states`, and `terminal_states`. The default tracker config in `src/scherzo/config.gleam` currently sets `active_states` to `Todo` and `In Progress`.

Issue state values live in `src/scherzo/tracker/state.gleam`. An `IssueState` contains a trimmed raw string and a normalized key. Normalized comparisons use the key, but Linear query serialization uses raw strings through `issue_state.to_strings`. Because of that split, the config loader must canonicalize accepted `dispatch_states` entries to the matching `active_states` raw state value before storing them.

Linear integration lives mainly in `src/scherzo/linear.gleam`. The public function `fetch_candidate_issues` is the important entry point for candidate discovery. It currently passes `config.active_states` to the internal pagination helper. `build_candidate_request` serializes the selected state names into the GraphQL variables for the candidate query.

Core orchestration lives under `src/scherzo/orchestrator/`. The file `src/scherzo/orchestrator/core.gleam` contains shared decision helpers for candidate issues, workflow labels, dispatch, and invalid-workflow handling. It currently has an active-state helper around the existing `contains_normalized(config.tracker.active_states, state)` logic. The implementation should add a parallel dispatch-state helper and use it only for initial dispatch and invalid-workflow triage.

The daemon candidate loop is `dispatch_candidates` in `src/scherzo/orchestrator/daemon.gleam`. It calls `core.dispatch_preconditions_satisfied_without_slot_capacity`, then `workflow_policy.classify_issue`, then either `handle_invalid_workflow_candidate` or `handle_valid_workflow_candidate`. The service one-shot candidate loop is `dispatch_candidates` in `src/scherzo/orchestrator/service.gleam`; it calls `core.should_dispatch`, then `runtime_bundle.select_workflow`, then `dispatch_issue`. The dispatch-state guard must run in these initial candidate loops before workflow-label validation, invalid-workflow reporting, claim validation, workflow selection, or dispatch start.

Lifecycle and retry/final classification logic also references `active_states`. One known location is `src/scherzo/agent/run_attempt.gleam`, where state classification checks whether the current issue state is in `config.tracker.active_states`. That logic should continue to use `active_states`, not `dispatch_states`.

Doctor and startup configuration loading are routed through `src/scherzo/runtime_bundle.gleam`, `src/scherzo/orchestrator/service.gleam`, and `src/scherzo/doctor.gleam`. The doctor workflow-config check should surface the specific config validation message for missing or invalid `tracker.dispatch_states`, rather than a generic `config error`.

Linear contract validation lives in `src/scherzo/linear_contract.gleam`. It currently includes `tracker.active_states` in the state existence checks. Add `tracker.dispatch_states` so doctor/contract validation catches misspelled dispatch states before daemon operation.

The dogfood configuration file is `.scherzo/scherzo.yaml`. It currently has `tracker.active_states: [Todo, In Progress]`; after implementation it should also have `tracker.dispatch_states: [Todo]`.

## Preconditions and Verified Facts

The working copy was clean before this plan was written, as shown by `jj status --color=never` reporting no changes.

No existing `docs/plans/LIV-164-*` plan file existed before this plan was created.

The repository currently uses `.envrc`/devenv for commands. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the command through `direnv exec .`.

The implementation must preserve the production lint policy documented by the repository: do not add production `let assert`, `panic`, or `todo`; run `glinter` and `scherzo_lint` before completion.

## Scope Boundaries

In scope:

- Add `dispatch_states` to the tracker config model.
- Make YAML config loading require `tracker.dispatch_states`.
- Parse `dispatch_states` strictly as a YAML list of strings; reject missing, wrong-type, empty, and non-string-entry values with actionable messages.
- Validate that `dispatch_states` is non-empty and a normalized subset of `active_states`, then canonicalize stored dispatch states to the matching `active_states` raw state spelling.
- Improve config/doctor UX so missing, wrong-type, non-string-entry, empty, and out-of-subset dispatch states produce specific actionable messages.
- Use `dispatch_states` for Linear candidate fetching.
- Add defensive daemon/service/core guards so initial dispatch and invalid-workflow triage apply only to dispatch-state issues.
- Keep lifecycle logic using `active_states`.
- Include `dispatch_states` in Linear contract state existence checks.
- Migrate `.scherzo/scherzo.yaml` and test fixtures to include `dispatch_states`.
- Add tests for the migration and behavior listed in this plan.

Out of scope:

- Removing invalid-workflow enforcement.
- Changing Linear workflow labels or routing semantics.
- Changing the meaning of `active_states` for existing Scherzo-owned work.
- Adding a compatibility fallback from missing `dispatch_states` to `active_states`.
- Redesigning tracker abstractions beyond the minimum needed to carry a new config field.

## Milestones

Milestone 1 adds the config field and validation. At the end of this milestone, code can represent `dispatch_states`, YAML config must fail without it, malformed YAML must fail strictly, normalized dispatch spelling must be canonicalized to active-state raw spelling, and invalid values must produce precise messages. This comes first because all later code needs a reliable config model.

Milestone 2 improves doctor/startup error reporting. At the end of this milestone, an operator running doctor against an old config sees a specific remediation instead of a generic failure. This comes before dispatch behavior because the rollout is intentionally breaking and the operator experience is part of the safety story.

Milestone 3 switches candidate discovery and defensive dispatch guards. At the end of this milestone, Linear candidate queries use only dispatch states, and the daemon/service/core initial candidate path ignores non-dispatch-state issues even if a tracker returns them.

Milestone 4 preserves lifecycle behavior and updates Linear contract validation. At the end of this milestone, **In Progress** remains lifecycle-active for retry/final classification, and contract validation checks that dispatch state names exist in Linear.

Milestone 5 migrates dogfood config and completes the test fixture migration. At the end of this milestone, the repository's own config is valid under the new required setting and the full validation suite passes.

## Plan of Work

### Config model and parser

In `src/scherzo/config/types.gleam`, add a `dispatch_states: List(issue_state.IssueState)` field to `TrackerConfig`. Place it immediately after `active_states` so the type reads as lifecycle-active states followed by dispatch-candidate states, then terminal states.

In `src/scherzo/config.gleam`, update `default_tracker_config()` to include a programmatic default `dispatch_states` value of `[Todo]`. This default is only for code that explicitly asks for the default config; it must not be used to satisfy a missing YAML key. Keep the existing default `active_states` value of `[Todo, In Progress]`.

In `src/scherzo/config.gleam`, update `resolve_tracker`. Keep existing behavior for `active_states` unless separate tests say otherwise: if `active_states` is omitted, it may continue to default to `[Todo, In Progress]`. Add a required strict read for `dispatch_states` from the tracker YAML node. Do not pipe it through `list_default` and do not use the permissive `get_string_list` helper for this field. Reuse or adapt `get_contract_string_list` and `read_string_values`, or add a similarly strict helper, so the logic distinguishes these cases:

- Missing `tracker.dispatch_states`: return a config error whose message includes `tracker.dispatch_states is required` and a remediation such as `add dispatch_states: [Todo] under tracker`.
- Wrong node type such as `tracker.dispatch_states: Todo`: return a config error whose message includes `tracker.dispatch_states must be a string list`.
- Non-string list entry such as `tracker.dispatch_states: [Todo, 123]`: return a config error whose message includes `tracker.dispatch_states entries must be strings`.
- Empty `tracker.dispatch_states: []`: return a config error whose message includes `tracker.dispatch_states must contain at least one state`.
- A dispatch state that is not in `active_states` after normalization: return a config error whose message includes `tracker.dispatch_states must be a subset of tracker.active_states`, the invalid state name, and guidance to either remove it from `dispatch_states` or add it to `active_states` only if it is truly lifecycle-active.

Normalization means state comparisons must ignore harmless differences such as case and leading/trailing whitespace, but the external Linear query must use canonical board spelling. After parsing `active_states` and the strict `dispatch_states` string list, convert each dispatch entry to an `IssueState`, find the first `active_states` entry with the same normalized key, and store that active-state value in `TrackerConfig.dispatch_states`. For example, `active_states: [Todo]` with `dispatch_states: [" todo "]` must store and later serialize `Todo`. If no active-state entry matches, return the subset error above using the original dispatch entry text. `src/scherzo/tracker/state.gleam` already exposes normalized keys through `key` and `equals_normalized`; add small helpers there if they make this single definition clearer, for example `pub fn contains_normalized(states: List(IssueState), state: IssueState) -> Bool` and `pub fn canonicalize_against(states: List(IssueState), candidate: IssueState) -> Result(IssueState, IssueState)`.

In `src/scherzo/error.gleam`, inspect the existing `ConfigError` type. Prefer the existing `InvalidConfig(String)` variant if it is already used for detailed validation failures. Add narrower variants only if the current error formatting cannot produce the actionable messages above. If new variants are added, update `config_error_message` in `src/scherzo/config.gleam` or the appropriate formatter so both human output and logs receive specific text.

### Doctor and runtime bundle UX

Add tests first in `test/orchestrator_service_doctor_test.gleam` for the workflow-config doctor check against configs that are missing, wrong-type, non-string-entry, empty, and out-of-subset for `tracker.dispatch_states`. The tests should assert that the `doctor.WorkflowConfig` result is `Fail`, that the human output mentions `tracker.dispatch_states`, and that the output contains actionable remediation text. The missing-field test should be the most explicit: it should include `required` and `dispatch_states: [Todo]`. The malformed YAML tests should preserve the strict parser messages `must be a string list` and `entries must be strings`.

Then inspect `src/scherzo/runtime_bundle.gleam` and `src/scherzo/orchestrator/service.gleam`. If the new doctor tests fail because the error is only `config error`, change the config-error mapping so the specific config validation message is preserved in the service error detail and doctor result message. The desired operator-visible behavior is a report along these lines:

    Scherzo doctor
    workflow-config: FAIL
      tracker.dispatch_states is required; add dispatch_states: [Todo] under tracker

The exact report formatting can follow existing doctor conventions, but it must contain the specific setting name and remediation. The manual acceptance command from the repository root is:

    direnv exec . gleam run -m scherzo -- doctor --check workflow-config test/tmp/missing-dispatch-states.yaml

Use a repository-relative temporary fixture path such as `test/tmp/missing-dispatch-states.yaml`. The command should exit non-zero or report a failing check according to existing doctor conventions, and its human output must contain `workflow-config: FAIL`, `tracker.dispatch_states`, `required`, and `dispatch_states: [Todo]`.

### Linear candidate fetching and smoke path

In `src/scherzo/linear.gleam`, change `fetch_candidate_issues` so it passes `config.dispatch_states` to `fetch_pages`. The current call passes `config.active_states`; that is the core bug for Linear candidate discovery.

In `src/scherzo/linear.gleam`, update `build_candidate_request` and the candidate GraphQL query variable names if needed. The minimal behavior requirement is that the serialized state list in the request contains `dispatch_states` and not all `active_states`. For clarity, prefer renaming the GraphQL variable from an internal `activeStates` name to `dispatchStates` if the current query string makes that straightforward; otherwise, keep the variable name but add tests that prove the values are dispatch states. The external Linear API only cares about the state names in the filter, not the local variable name.

In `src/scherzo/smoke.gleam`, review the use of `config.active_states` in the one-page Linear smoke path. The smoke check is meant to prove Scherzo can inspect the candidate queue, so it should use `config.dispatch_states` after this split. If implementation discovers that `smoke.gleam` is only intended to validate lifecycle-active visibility and not dispatch candidates, record that discovery in this plan before choosing otherwise; the expected default is to use `dispatch_states`.

### Daemon/service/core dispatch and invalid-workflow guards

In `src/scherzo/orchestrator/core.gleam`, add a helper that answers whether an issue state is in `config.tracker.dispatch_states`, using normalized state comparison. Keep any existing helper that answers whether a state is in `config.tracker.active_states` for lifecycle decisions.

Apply the new dispatch helper to initial candidate evaluation, not retry or lifecycle recovery. The two initial candidate loops to inspect are `dispatch_candidates` in `src/scherzo/orchestrator/daemon.gleam` and `dispatch_candidates` in `src/scherzo/orchestrator/service.gleam`. If all candidate loops call a shared core function before any side effect, it is acceptable to put the guard in that core function. If either loop performs side effects first, add an explicit loop-level guard immediately after selecting the candidate issue.

For the daemon loop in `src/scherzo/orchestrator/daemon.gleam`, the guard must run before `core.blocker_decision`, `core.dispatch_preconditions_satisfied_without_slot_capacity`, `workflow_policy.classify_issue`, `handle_invalid_workflow_candidate`, `handle_valid_workflow_candidate`, claim validation, or dispatch start. For the service loop in `src/scherzo/orchestrator/service.gleam`, the guard must run before `core.blocker_decision`, `core.should_dispatch`, `runtime_bundle.select_workflow`, and `dispatch_issue`. A non-dispatch-state initial candidate should be skipped quietly or debug-level according to existing logging conventions; it must not create an invalid-workflow comment, pending invalid-workflow report, state transition, claim validation, or dispatch.

Invalid-workflow triage should still apply to unlabeled **Todo** issues when `dispatch_states` is `[Todo]`, but it must not apply to unlabeled **In Progress** issues when `active_states` is `[Todo, In Progress]` and `dispatch_states` is `[Todo]`. This must be proven with daemon-level tests in `test/orchestrator_daemon_test.gleam` or the existing daemon invalid-workflow harness, because the actual side effect is `ReportInvalidWorkflow` enqueued through `handle_invalid_workflow_candidate`.

Do not change lifecycle checks that decide whether in-flight work is active. In particular, do not replace `active_states` in `src/scherzo/agent/run_attempt.gleam` unless the surrounding code is actually dispatch-candidate discovery. The known use around final/retry classification should remain `active_states`. Retry refresh handling such as `handle_retry_candidate_after_refresh` in `src/scherzo/orchestrator/daemon.gleam` is not an initial candidate loop and should continue to follow the existing retry/lifecycle policy unless a separate test demonstrates otherwise.

### Linear contract validation

In `src/scherzo/linear_contract.gleam`, add `effective.tracker.dispatch_states` to the state existence validation. The existing active-state validation records source text like `tracker.active_states`; the new validation should record `tracker.dispatch_states` so doctor output points to the correct setting when a dispatch state is misspelled or missing from the Linear board.

Update `test/linear_contract_test.gleam` to include `dispatch_states` in its config helper type and tests. Add a negative test where active states are valid but `dispatch_states` contains a nonexistent state, and assert that the failure source is `tracker.dispatch_states`.

### Dogfood config and fixtures

In `.scherzo/scherzo.yaml`, add:

    tracker:
      active_states: [Todo, In Progress]
      dispatch_states: [Todo]
      terminal_states: [Canceled, Duplicate, Done]

Keep the existing state names and surrounding config. This migration is the dogfood example for operators.

Update YAML fixtures and embedded YAML strings in tests to include `dispatch_states`. Known files from repository inspection include `test/fixtures/schema/orchestrator_config_complete.yaml`, `test/ctl_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, `test/orchestrator_service_test.gleam`, and tests under `test/orchestrator_*` that embed tracker YAML. Use compile errors and `grep` for `active_states:` to find the rest. Do not update the deliberate missing-dispatch config tests.

Update all direct `config_types.TrackerConfig(...)` construction sites to pass `dispatch_states`. Known test files with direct tracker configs include `test/linear_test.gleam`, `test/state_recovery_test.gleam`, `test/linear_comments_test.gleam`, `test/workflow_run_test.gleam`, `test/linear_triage_test.gleam`, `test/local_integration/workflow_jj_workspace_smoke_test.gleam`, `test/orchestrator_core_test.gleam`, `test/linear_contract_test.gleam`, `test/agent_runner_test.gleam`, `test/handoff_test.gleam`, `test/linear_attachment_test.gleam`, `test/linear_http_test.gleam`, `test/orchestrator_daemon_*`, `test/orchestrator_service_lifecycle_test.gleam`, `test/scheduled_failure_reporter_test.gleam`, and related helpers. For most tests, use `dispatch_states: issue_state.list_from_strings(["Todo"])` when `active_states` includes both `Todo` and `In Progress`, and use the same single state as `active_states` when the test only has one active state.

## Concrete Steps

1. From the repository root, run `jj status --color=never` and confirm the working copy is clean or contains only intentional changes for this implementation.

       $ jj status --color=never
       The working copy has no changes.

2. Add failing config parser tests in `test/config_test.gleam`:
   - `missing_dispatch_states_fails_test`: load a minimal otherwise-valid config with `active_states` and `terminal_states` but no `dispatch_states`; assert an error message contains `tracker.dispatch_states` and `required`.
   - `wrong_type_dispatch_states_fails_test`: load `dispatch_states: Todo`; assert an error message contains `tracker.dispatch_states must be a string list`.
   - `non_string_dispatch_states_entry_fails_test`: load `dispatch_states: [Todo, 123]`; assert an error message contains `tracker.dispatch_states entries must be strings`.
   - `empty_dispatch_states_fails_test`: load `dispatch_states: []`; assert an error message contains `must contain at least one state`.
   - `dispatch_states_outside_active_states_fails_test`: load `active_states: [Todo]` and `dispatch_states: [In Progress]`; assert an error message contains `subset`, `tracker.active_states`, and `In Progress`.
   - `dispatch_states_normalized_subset_canonicalizes_test`: load `active_states: [Todo, In Progress]` and `dispatch_states: [" todo "]`; assert the effective config stores a single dispatch state whose raw string is exactly `Todo`.

3. Run a targeted test command and expect the new tests to fail before implementation:

       $ direnv exec . gleam test test/config_test.gleam
       ... failing assertions or compile errors mentioning dispatch_states ...

   If the project test runner does not accept a single file argument, run `direnv exec . gleam test` and focus on the new config failures.

4. Update `src/scherzo/config/types.gleam` to add `dispatch_states` to `TrackerConfig`.

5. Update `src/scherzo/tracker/state.gleam` with normalized comparison and canonicalization helpers if equivalent exported helpers do not already exist. Add small tests in the nearest existing state/config test file if helpers are new.

6. Update `src/scherzo/config.gleam` defaults, `resolve_tracker`, and config error formatting so the config tests from step 2 pass.

7. Update direct `TrackerConfig` constructors and YAML test fixtures enough for the project to compile. Do not mask the deliberate missing-dispatch tests.

8. Re-run the config tests and expect them to pass.

9. Commit the first green unit of work after formatting the touched files. Suggested commit message: `Require tracker dispatch states in config`.

10. Add failing doctor tests in `test/orchestrator_service_doctor_test.gleam` for missing, wrong-type, non-string-entry, empty, and out-of-subset `tracker.dispatch_states`. Assert `doctor.WorkflowConfig` fails and human output includes the setting name plus remediation.

11. Run the doctor tests and observe whether they fail with generic `config error` or with missing remediation.

12. Update `src/scherzo/runtime_bundle.gleam`, `src/scherzo/orchestrator/service.gleam`, or `src/scherzo/doctor.gleam` so the specific config error message reaches doctor output.

13. Re-run the doctor tests and commit when they pass. Suggested commit message: `Surface dispatch state config errors in doctor`.

14. Add failing tests in `test/linear_test.gleam` proving `linear.fetch_candidate_issues` sends only canonicalized `dispatch_states`. First build a tracker config with `active_states: [Todo, In Progress]` and `dispatch_states: [Todo]`; use the existing fake transport/request capture pattern; assert the GraphQL variables include `Todo` and do not include `In Progress` in the candidate state filter. Then add a request-serialization test that starts from YAML or an effective config loaded from YAML with `active_states: [Todo]` and `dispatch_states: [" todo "]`; assert the candidate request variable contains exactly `Todo`, not `todo` or a whitespace-padded value.

15. Update `src/scherzo/linear.gleam` so `fetch_candidate_issues` uses `config.dispatch_states`. Update `build_candidate_request` variable names and expected request JSON only if needed by the chosen implementation.

16. Update or add tests for `src/scherzo/smoke.gleam` if the repository has smoke tests. The smoke path should fetch one page using `dispatch_states`, not `active_states`.

17. Re-run `direnv exec . gleam test test/linear_test.gleam` or the nearest supported targeted test command, then commit. Suggested commit message: `Fetch Linear candidates from dispatch states`.

18. Add failing daemon-level tests in `test/orchestrator_daemon_test.gleam` or the existing daemon invalid-workflow test file:
   - `unlabeled_todo_still_gets_invalid_workflow_triage_test`: with `active_states: [Todo, In Progress]`, `dispatch_states: [Todo]`, and invalid-workflow enforcement enabled, feed an unlabeled **Todo** candidate from the fake tracker and assert the daemon enqueues or performs the existing `ReportInvalidWorkflow` path.
   - `unlabeled_in_progress_is_not_triaged_test`: feed an unlabeled **In Progress** initial candidate from the fake tracker and assert no `ReportInvalidWorkflow` effect or pending invalid-workflow report exists, no state transition is requested, and no dispatch is started.
   - `workflow_labeled_in_progress_is_not_dispatched_test`: feed an **In Progress** initial candidate with a valid `workflow:*` label and assert no claim validation and no agent/workflow dispatch is started.

19. Add a small `test/orchestrator_core_test.gleam` predicate test only if the implementation puts the dispatch-state predicate in `core.gleam`; this test is optional and does not replace the daemon side-effect tests.

20. Update `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/orchestrator/service.gleam` as needed so the dispatch-state guard runs before workflow label validation, invalid-workflow triage, claim validation, workflow selection, and dispatch start for initial candidates.

21. Re-run the daemon and core tests and commit when they pass. Suggested commit message: `Guard dispatch decisions by dispatch states`.

22. Inspect `src/scherzo/orchestrator/daemon.gleam` retry handling, especially `handle_retry_candidate_after_refresh`, and confirm the new guard was not applied to retry/lifecycle paths by accident. Record any surprising discovery in this plan before changing retry behavior.

23. Add or update a lifecycle regression test in `test/state_recovery_test.gleam`, `test/agent_runner_test.gleam`, or the existing test file that already covers final/retry classification. The test must configure `active_states: [Todo, In Progress]` and `dispatch_states: [Todo]`, then prove **In Progress** is still treated as active for retry/final classification. The assertion should fail if `src/scherzo/agent/run_attempt.gleam` or equivalent lifecycle logic is changed to use `dispatch_states`.

24. Ensure lifecycle code still uses `active_states`. If any mechanical replacement changed `src/scherzo/agent/run_attempt.gleam` lifecycle checks to `dispatch_states`, revert that replacement.

25. Update `src/scherzo/linear_contract.gleam` and `test/linear_contract_test.gleam` so `tracker.dispatch_states` is included in state existence checks and failures report source `tracker.dispatch_states`.

26. Update `.scherzo/scherzo.yaml` to add `dispatch_states: [Todo]` under `tracker`.

27. Update schema guardrail fixtures and embedded YAML strings, including `test/fixtures/schema/orchestrator_config_complete.yaml` and `test/schema_guardrail_test.gleam`, so complete configs include `dispatch_states`.

28. Run a repository search to ensure no candidate-dispatch path still uses `active_states` incorrectly:

       $ grep -R "active_states" src test

   Review every remaining production use. It is correct for lifecycle-active logic, terminal/retry classification, and tests that intentionally check active states. It is suspect in candidate discovery, initial dispatch, invalid-workflow triage, and Linear smoke/candidate checks.

29. Run the manual doctor acceptance command from the repository root using a repository-relative temporary fixture that intentionally omits `dispatch_states`:

       $ direnv exec . gleam run -m scherzo -- doctor --check workflow-config test/tmp/missing-dispatch-states.yaml
       Scherzo doctor
       workflow-config: FAIL
       ... tracker.dispatch_states ... required ... dispatch_states: [Todo] ...

30. Run the full validation commands from the repository root:

       $ direnv exec . gleam format --check src test
       $ direnv exec . gleam test
       $ direnv exec . gleam run -m glinter
       $ direnv exec . gleam run -m scherzo_lint

31. If formatting fails, run the repository's normal formatter, then re-run the check. If lint fails, fix only findings introduced by this work or nearby mechanical issues needed for this change.

32. Commit the final migration and test updates. Suggested commit message: `Migrate configs to dispatch states`.

## Testing and Falsifiability

The implementation is falsified if any old config without `tracker.dispatch_states` loads successfully. Add `test/config_test.gleam` coverage that asserts a missing field fails clearly.

The implementation is falsified if malformed dispatch YAML loads successfully or produces misleading missing-field errors. Add `test/config_test.gleam` coverage for `dispatch_states: Todo` and assert `tracker.dispatch_states must be a string list`; add coverage for `dispatch_states: [Todo, 123]` and assert `tracker.dispatch_states entries must be strings`.

The implementation is falsified if an empty dispatch list loads successfully. Add `test/config_test.gleam` coverage for `dispatch_states: []` and assert failure.

The implementation is falsified if `dispatch_states` can contain states outside `active_states`. Add `test/config_test.gleam` coverage for `active_states: [Todo]` and `dispatch_states: [In Progress]` and assert failure.

The implementation is falsified if normalized dispatch spelling is accepted but not canonicalized before storage and Linear queries. Add config coverage for `active_states: [Todo]` with `dispatch_states: [" todo "]` and assert the effective config stores raw `Todo`. Add `test/linear_test.gleam` coverage that captures the candidate request body from `linear.fetch_candidate_issues` and asserts this canonicalized config sends `Todo`, not `todo` or a whitespace-padded value.

The implementation is falsified if Linear candidate fetching still asks for **In Progress** when `dispatch_states` is `[Todo]`. Add `test/linear_test.gleam` coverage that captures the candidate request body from `linear.fetch_candidate_issues` and asserts the candidate state variable contains `Todo` but not `In Progress`.

The implementation is falsified if invalid-workflow enforcement is removed or too broad. Add daemon-level coverage in `test/orchestrator_daemon_test.gleam` or the existing daemon invalid-workflow harness that an unlabeled **Todo** initial candidate still invokes `ReportInvalidWorkflow`, while an unlabeled **In Progress** initial candidate invokes neither `ReportInvalidWorkflow` nor dispatch.

The implementation is falsified if workflow-labeled **In Progress** initial candidates are dispatched. Add daemon-level coverage with a valid `workflow:*` label on an **In Progress** candidate and assert no claim validation and no dispatch session is started.

The implementation is falsified if lifecycle code stops treating **In Progress** as active. Add a regression test in the existing retry/final classification test area, likely `test/state_recovery_test.gleam` or `test/agent_runner_test.gleam`, with `active_states: [Todo, In Progress]` and `dispatch_states: [Todo]`; assert the existing retry/final active-state behavior remains unchanged for **In Progress**.

The implementation is falsified if Linear contract validation ignores dispatch states. Add `test/linear_contract_test.gleam` coverage where `active_states` are valid but `dispatch_states` contains a nonexistent Linear state, and assert the contract result fails with source `tracker.dispatch_states`.

Doctor UX is falsified if missing, malformed, empty, or out-of-subset `dispatch_states` produces only a generic `config error`. Add `test/orchestrator_service_doctor_test.gleam` coverage that inspects human output for `tracker.dispatch_states`, `required`, `must be a string list`, `entries must be strings`, or `subset` as appropriate, plus remediation text.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

All commands should exit successfully. `gleam test` should include the new tests described above. The exact pass count may change as the suite evolves, so acceptance is based on a clean exit and the named tests passing.

Manual operator acceptance can be checked with a temporary copy of a config that omits `tracker.dispatch_states`. From the repository root, run `direnv exec . gleam run -m scherzo -- doctor --check workflow-config test/tmp/missing-dispatch-states.yaml` against a repository-relative temporary fixture under `test/tmp`. The output should contain `workflow-config: FAIL`, `tracker.dispatch_states`, `required`, and `dispatch_states: [Todo]`. Do not use an absolute local path in committed tests or docs; use repository-relative test fixture paths or temporary paths under `test/tmp`.

Behavioral acceptance is:

- With `active_states: [Todo, In Progress]` and `dispatch_states: [Todo]`, Linear candidate requests include `Todo` and exclude `In Progress`.
- With `active_states: [Todo]` and `dispatch_states: [" todo "]`, effective config and Linear candidate requests use canonical raw state `Todo`.
- An unlabeled **Todo** candidate still receives invalid-workflow triage according to existing policy.
- An unlabeled **In Progress** issue is ignored for dispatch and invalid-workflow triage.
- A workflow-labeled **In Progress** issue is ignored for new dispatch.
- Retry/final classification still treats **In Progress** as lifecycle-active.
- Linear contract validation checks dispatch state existence and reports failures against `tracker.dispatch_states`.

## Rollout, Recovery, and Idempotence

This is a breaking configuration migration by design. Operators upgrading an existing install must add `tracker.dispatch_states` before starting the upgraded daemon. For the dogfood board, the migration is:

    tracker:
      active_states: [Todo, In Progress]
      dispatch_states: [Todo]
      terminal_states: [Canceled, Duplicate, Done]

General operator migration note: choose `dispatch_states` as the subset of `active_states` from which Scherzo is allowed to discover and claim new issues. For a board where humans move work into **In Progress**, do not include **In Progress** in `dispatch_states` unless Scherzo should claim human-moved issues from that state. Keep any state where Scherzo-owned work may still be running in `active_states` so retry and recovery behavior remains correct.

If rollout fails because a config is missing `dispatch_states`, the safe recovery is to add the setting and rerun doctor. If rollout fails because `dispatch_states` references a state missing from Linear, either correct the spelling or create the state in Linear, then rerun doctor. If the code change must be backed out, remove `dispatch_states` from config only after reverting to a version that does not require it.

The implementation steps are idempotent at the config level: rerunning doctor after fixing YAML should not mutate remote state. Tests should use fake transports and test fixtures for candidate and triage behavior.

## Artifacts and Notes

Important repository facts observed while drafting:

    src/scherzo/config/types.gleam defines TrackerConfig without dispatch_states.
    src/scherzo/config.gleam resolve_tracker currently defaults active_states and terminal_states.
    src/scherzo/linear.gleam fetch_candidate_issues currently uses config.active_states.
    src/scherzo/smoke.gleam references config.active_states for a one-page Linear fetch.
    src/scherzo/linear_contract.gleam validates tracker.active_states for state existence.
    .scherzo/scherzo.yaml currently has active_states: [Todo, In Progress].

Review incorporation note: the review found that normalized state equality, strict YAML parsing, daemon-level invalid-workflow side effects, candidate-loop call sites, and doctor acceptance needed to be more explicit. This revision chooses canonicalization to active-state raw names, requires strict list-of-strings parsing, moves invalid-workflow safety assertions to daemon-level tests, and names the doctor command.

Expected old-to-new config shape:

    # Before upgrade, now invalid:
    tracker:
      active_states: [Todo, In Progress]
      terminal_states: [Canceled, Duplicate, Done]

    # After upgrade:
    tracker:
      active_states: [Todo, In Progress]
      dispatch_states: [Todo]
      terminal_states: [Canceled, Duplicate, Done]

## Interfaces and Dependencies

At the end of implementation, `src/scherzo/config/types.gleam` must expose a tracker config equivalent to:

    pub type TrackerConfig {
      TrackerConfig(
        kind: tracker_kind.TrackerKind,
        endpoint: String,
        api_key: Option(String),
        project_slug: Option(String),
        active_states: List(issue_state.IssueState),
        dispatch_states: List(issue_state.IssueState),
        terminal_states: List(issue_state.IssueState),
      )
    }

At the end of implementation, `src/scherzo/config.gleam` must load YAML so `dispatch_states` is required, parsed strictly as a list of strings, and validated independently of defaults. The parser may keep a programmatic default in `default_tracker_config`, but `resolve_tracker` must not use that default when the YAML key is missing. Accepted `dispatch_states` entries must be canonicalized to the matching `active_states` raw state before storage so downstream Linear requests receive canonical board state names.

At the end of implementation, `src/scherzo/linear.gleam` must have `fetch_candidate_issues(config, transport)` query `config.dispatch_states`. The lower-level `fetch_issues_by_states` should remain a generic helper that uses the explicit states passed by its caller.

At the end of implementation, `src/scherzo/orchestrator/core.gleam` must have separate helper concepts for dispatch-state membership and active-state membership. Dispatch-state membership gates initial dispatch and invalid-workflow triage in the initial candidate loops in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/service.gleam`. Active-state membership remains for lifecycle decisions.

At the end of implementation, `src/scherzo/linear_contract.gleam` must include `tracker.dispatch_states` in remote state existence validation and must report the source as `tracker.dispatch_states`.

No new package dependency should be required. Use existing Gleam standard library modules and existing repository modules for state normalization, config parsing, doctor reports, and test fake transports.

## Open Questions and Clarifications Needed

None.
