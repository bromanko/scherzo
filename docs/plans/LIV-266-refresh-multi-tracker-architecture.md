# Refresh the multi-tracker architecture around tasks, adapters, DAG workflows, and recovery

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo currently works as a Linear-first automation daemon: it polls Linear issues, chooses workflows from labels, runs pi workers in workspaces, posts handoff comments, accepts operator commands from Linear comments, and reports scheduled-job failures as Linear issues. Operators want the same daemon architecture to support other task systems, such as Jira or Trello, without weakening the existing Linear workflow or forcing every new backend to reimplement daemon internals.

After this plan is implemented, Scherzo will have a backend-neutral task model and a capability-aware tracker adapter boundary. A task is a unit of work from an external system; a tracker adapter is the Scherzo component that reads and updates tasks in that external system. Linear will remain the first production adapter, all existing Linear behavior will continue to work through compatibility aliases, and future Jira or Trello work can start from a tested adapter contract instead of from Linear-shaped orchestrator code.

The observable outcome is deliberately more concrete than "the code is cleaner." A reviewer can prove success by running the existing Linear tests, new adapter contract tests, and a fake non-Linear adapter dispatch scenario that proves the orchestrator no longer needs direct Linear-specific command, handoff, scheduled failure, or workflow-routing dependencies. Milestone 6 also produces a checked adapter-readiness matrix in `docs/runbooks/tracker-adapters.md` and a coupling inventory that lists every remaining production import of Linear-specific modules. At acceptance time, Linear-specific imports are allowed in Linear modules, `src/scherzo/tracker/linear_adapter.gleam`, compatibility tests, and Linear setup docs; they are not allowed in the generic orchestrator side-effect path, workflow runner context, template task rendering, or scheduled failure dispatch path.

## Problem Framing and Constraints

The operator problem is not that file names contain the word `linear`. The problem is that Scherzo's behavior is spread across many Linear-specific surfaces: candidate discovery, workflow label routing, comments, remote command ingestion, state moves, handoff, board readiness checks, scheduled failure reports, prompt variables, workspace naming, environment variables, durable ledger records, CLI modes, docs, scripts, and dogfood prompts. A future backend cannot be added by implementing the current `tracker.Client`, because that client only reads issues and does not express the side effects Scherzo uses to operate safely.

The implementation must preserve the current Linear integration. It must not require a live Jira or Trello account, and it must not make the daemon support multiple production backends in one process yet. It must be safe for existing ledgers, recovery records, workflow DAG runs, scheduled jobs, and prompt templates that still use `issue.*` names. It must also reflect the current DAG-era workflow architecture: workflows are directed acyclic graphs, meaning ordered steps with dependencies, checkpoints, artifacts, workspace profiles, structured-output validation, pi session continuation, recovery, and scheduled invocations that may not have a task at all.

The main non-goal is a production Jira or Trello adapter. This plan creates the architecture that makes those adapters implementable later. The only non-Linear adapter required here is a fake or memory adapter used by tests to prove that the seam is real.

## Strategy Overview

Use `task` as the backend-neutral domain term for a unit of work from an external system. Keep `tracker` as the name of the adapter subsystem that tracks tasks in an external backend. Keep `issue` only as a compatibility term for existing Linear-shaped APIs, prompt variables, environment variables, durable record fields, and docs that cannot be renamed safely in one change. New code should say `task` for the work item, `task system` for the external service, and `tracker adapter` for the Scherzo component that talks to that service.

Implement this in additive increments with explicit stop/go gates. First freeze current Linear behavior with characterization tests. Then add task-domain types and adapter capability types without changing runtime behavior. Then wrap Linear behind the adapter boundary while delegating to existing Linear modules. This first cut is valuable even if the team stops there, because it leaves a documented contract and Linear wrapper tests without touching the production orchestrator. Only after that cut is green should the implementation route workflow context, remote command ingestion, handoff, invalid-workflow reporting, scheduled failure reporting, config validation, and CLI aliases through adapter capabilities.

This is proportionate because a simple rename of `tracker.Client` would hide the problem rather than solve it. Scherzo needs comments, commands, state transitions, labels or routing metadata, attachments, scheduled failure publication, readiness checks, and recovery-safe remote references. A capability-aware adapter is the smallest boundary that can express those behaviors while still letting Linear remain a straightforward wrapper over the existing modules.

## Alternatives Considered

One alternative is to rename the current `tracker.Client` to something Linear-specific and accept that Scherzo is a Linear-only daemon. That would be simpler, but it does not solve the operator goal of supporting other task systems, and it would leave scripts, docs, prompts, scheduled jobs, and remote commands coupled to Linear.

Another alternative is to add only a generic read interface for Jira or Trello while leaving comments, handoff, command ingestion, and scheduled failure reporting Linear-specific. That creates a partial port where candidates can be read but the daemon cannot safely claim work, report results, receive operator controls, or recover. It is insufficient for real operations.

A third alternative is to implement production Jira and Trello adapters immediately. That is too large and risky. Jira and Trello differ from Linear in identity, state transitions, comments, attachments, labels, blockers, permissions, and readiness checks. The safer path is to extract and test the adapter contract first, then add production backends in later plans.

A fourth alternative is to replace all issue-shaped durable records with new task-shaped record kinds in one migration. That would make terminology cleaner, but it would create unnecessary recovery risk. This plan instead keeps the existing ledger schema version and existing issue-shaped record kinds for compatibility, adds backend-neutral task fields to new writes where workflow recovery needs them, and introduces generic remote-command record kinds only where the runtime model is intentionally changing from Linear-specific commands to backend-neutral remote commands.

## Risks and Countermeasures

The largest risk is a subtle Linear regression. Counter it by preserving current Linear modules, adding adapter tests around them before changing call sites, and running the existing `gleam test` suite plus the production lint gates after every milestone.

A second risk is durable recovery breakage. Existing ledgers and checkpoints use issue-centric names such as `issue_id`, `issue_identifier`, and `linear_command_*`. Counter it by decoding legacy records indefinitely during this plan, adding task fields only in a backward-compatible way, translating old Linear command records into the new remote-command runtime projection, and testing recovery from old-only, new-only, and mixed fixtures.

A third risk is over-generalization. Counter it by making Linear the only production adapter in this plan and proving generality with one fake non-Linear adapter in tests. Do not add Jira/Trello-specific production code beyond documenting capability gaps and future config shapes in the adapter-readiness matrix.

A fourth risk is confusing operators with terminology churn. Counter it by introducing `task.*` as the preferred prompt/template surface while keeping `issue.*` as compatibility aliases. Public docs should say Linear issues are one kind of task, not that every task system must expose Linear issues.

A fifth risk is unsupported capability combinations. A backend may support task reads but not comments, state moves, labels, blockers, or attachments. Counter it with startup validation that rejects impossible configs before the daemon starts, such as remote commands enabled without remote-command and comment capabilities, handoff state moves configured without a state-transition capability, label-based workflow routing configured without a routing-metadata capability, or scheduled failure publication enabled without a scheduled-failure capability.

A sixth risk is a migration that becomes too large to review safely. Counter it with stop/go gates after Milestones 2, 3, and 4. At each gate, the tree must be green, the plan's Progress section must say what remains Linear-specific, and the team may stop without leaving production behavior half-migrated.

## Progress

- [x] (2026-05-13 00:00Z) Created this ExecPlan draft from the current repository, the current DAG-era workflow code, the scheduled failure reporter, the tracker modules, and the LIV-263 research findings.
- [x] (2026-05-13 00:00Z) Incorporated adversarial review findings by making the adapter API prescriptive, specifying recovery record shapes and fixture tests, splitting the work into stop/go gates, adding concrete startup validation messages, and rewriting the implementation path around file-scoped TDD steps.
- [x] (2026-05-13 20:53Z) Clarified that this plan is the canonical implementation plan for the core architecture work and mapped the existing Linear child issues to implementation slices instead of additional ExecPlan-writing tasks.
- [x] (2026-05-13 21:44Z) Confirmed the workspace was clean with `$SCHERZO_WORKSPACE_DRIVER status --human`; the working copy had no changes before implementation.
- [x] (2026-05-13 21:58Z) Milestone 0 added Linear characterization tests in `test/config_test.gleam`, `test/template_test.gleam`, `test/scheduled_failure_reporter_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, `test/state_recovery_test.gleam`, `test/recovery_workflow_checkpoint_test.gleam`, `test/handoff_format_test.gleam`, and `test/linear_triage_test.gleam`.
- [x] (2026-05-13 21:58Z) Milestone 0 validation passed with `direnv exec . gleam test` reporting 1299 passed and no failures. No commit was created because the Scherzo workflow contract for this workspace forbids agent commits; the publish step will handle integration.
- [x] (2026-05-13 22:31Z) LIV-268 added the task-domain half of Milestone 1: `test/task_test.gleam` and `src/scherzo/task.gleam`. Validation passed with `direnv exec . gleam format --check src test`, `direnv exec . gleam test` reporting 1306 passed and no failures, `direnv exec . gleam run -m glinter` reporting 0 errors, and `direnv exec . gleam run -m scherzo_lint` reporting 0 errors. No commit was created because the Scherzo workflow contract for this workspace forbids agent commits; the publish step will handle integration.
- [ ] Milestone 1: add `test/tracker_adapter_test.gleam` and `src/scherzo/tracker/adapter.gleam`.
- [ ] Milestone 1: run format and tests, then commit the task-domain and adapter-contract types.
- [ ] Milestone 2: add Linear adapter wrapper tests, `src/scherzo/tracker/linear_adapter.gleam`, a fake adapter under `test/support/`, and fake adapter dispatch tests.
- [ ] Milestone 2 stop/go gate: confirm production runtime behavior still uses the old Linear path except through additive wrapper tests, document remaining Linear-specific call sites, and commit.
- [ ] Milestone 3: add preferred `task.*` template variables, task reference fields for workflow context/checkpoints, compatibility environment variables, and recovery fixtures for old/new/mixed records.
- [ ] Milestone 3 stop/go gate: confirm workflow/template behavior is task-aware while remote commands, handoff, and scheduled failure side effects can still remain on the old Linear path.
- [ ] Milestone 4: route remote commands, handoff, invalid-workflow reporting, and scheduled failure publication behind adapter capabilities with startup validation tests.
- [ ] Milestone 4 stop/go gate: confirm the generic orchestrator side-effect path no longer imports Linear command, handoff, triage, or scheduled failure reporter modules directly.
- [ ] Milestone 5: add config and CLI aliases, precedence tests, help text, docs, prompts, and adapter-readiness matrix.
- [ ] Milestone 6: run final validation, run the coupling inventory, update `docs/runbooks/tracker-adapters.md` with remaining backend gaps, and prepare follow-up tickets for production Jira/Trello adapters.

## Surprises & Discoveries

- Observation: The current `tracker.Client` is intentionally small and only covers candidate and state-refresh reads.
  Evidence: `src/scherzo/tracker.gleam` exposes `fetch_candidate_issues`, `fetch_issues_by_states`, and `fetch_issue_states_by_ids` only.
- Observation: The orchestrator already runs DAG workflows and scheduled workflows, but many recovery and step contexts still use issue-centric field names.
  Evidence: `src/scherzo/workflow_dag.gleam` defines DAG steps and dependencies; `src/scherzo/workflow_run.gleam` carries `issue_id` and `issue_identifier` through `StepContext`; `src/scherzo/workflow_checkpoint.gleam` writes `WorkflowRunFinished` with an `issue_id`.
- Observation: Scheduled failure reporting has a generic-looking backend record, but its public behavior is still Linear issue creation/update semantics.
  Evidence: `src/scherzo/scheduled_failure_reporter.gleam` defines `Backend` operations such as `create_issue`, `comment_issue`, and `move_issue_to_state`, and its config path is `on_failure.linear` through `ScheduledLinearFailureConfig`.
- Observation: The review found that the first draft was directionally sound but unsafe to implement because adapter signatures, durable migration shapes, validation messages, and file-scoped test steps were underspecified.
  Evidence: `tmp/execplan-review.md` returned verdict `REVISE` with blocking findings for adapter API specificity, recovery compatibility, concrete steps, and test assertions.
- Observation: Current template rendering exposes `issue.labels` as a list for loops, but direct interpolation of `{{ issue.labels }}` renders an empty string.
  Evidence: `issue_template_variables_are_characterized_test` in `test/template_test.gleam` asserts the direct list slot is empty while `{% for label in issue.labels %}` renders `workflow:execplan;kind:feature;`.
- Observation: Current flat tracker config environment references use `$LINEAR_API_KEY`; the brace form `${LINEAR_API_KEY}` is not the existing parser convention.
  Evidence: `flat_linear_tracker_config_aliases_still_parse_test` in `test/config_test.gleam` uses the existing `$LINEAR_API_KEY` form and `direnv exec . gleam test` passed with 1299 tests.

## Decision Log

- Decision: Use `task` for backend-neutral work items, `tracker adapter` for backend integration, and `issue` only as a compatibility alias unless referring specifically to a Linear issue.
  Rationale: `task` maps better across Linear, Jira, Trello, and fake adapters. `tracker` is still useful as the subsystem name and preserves existing config vocabulary. `issue` is too Linear/Jira-shaped to be the new core term, but removing it immediately would break templates, env vars, tests, and durable records.
  Date: 2026-05-13
- Decision: Keep Linear as the only production adapter in this plan and require only a fake non-Linear adapter for tests.
  Rationale: The architecture must be proven before production Jira/Trello adapters are added. This avoids mixing extraction risk with backend API risk.
  Date: 2026-05-13
- Decision: Treat legacy durable record names and prompt variables as compatibility surfaces, not as cleanup targets for the first implementation.
  Rationale: Recovery safety is more important than perfect terminology. New writes can carry backend-neutral task fields while old ledgers continue to decode.
  Date: 2026-05-13
- Decision: Model optional backend behavior as capabilities rather than a single flat interface with many no-op functions.
  Rationale: Linear, Jira, and Trello differ in comments, transitions, labels, blockers, attachments, and scheduled failure publication. Capabilities let startup validation reject unsupported workflows early.
  Date: 2026-05-13
- Decision: Keep ledger `schema_version` at its current value for this plan and augment selected record kinds instead of replacing the full issue-shaped recovery model.
  Rationale: The review correctly identified recovery as high blast radius. Adding optional task fields and generic remote-command records is safer than renaming every `Issue*` record kind while the orchestrator migration is still in flight.
  Date: 2026-05-13
- Decision: Define stop/go gates after the Linear wrapper, workflow context migration, and side-effect routing migration.
  Rationale: The first draft attempted one large migration. Stop/go gates let the team land useful, reversible cuts and defer later cuts if risk or review cost becomes too high.
  Date: 2026-05-13
- Decision: Use this ExecPlan as the single source of truth for the core task-system-agnostic implementation work, and use Linear child issues as implementation slices rather than as additional ExecPlan drafting tasks.
  Rationale: The plan already contains the architecture, milestones, stop/go gates, tests, and rollback guidance needed for the core work. Creating separate plans for the same slices would duplicate context and create drift. Jira and Trello remain separate future planning tasks because they are production backend adapters outside this plan's scope.
  Date: 2026-05-13
- Decision: Convert legacy Linear `IssueState` values to `task.TaskState` with `category: Unknown` in `task.from_legacy_issue`.
  Rationale: The existing `src/scherzo/tracker/state.gleam` model stores display text and a normalized key, but it does not expose a backend-neutral category such as ready, active, done, or canceled. Preserving the state name while marking the category unknown keeps the conversion honest and avoids inventing Linear workflow semantics that are not present in the legacy type.
  Date: 2026-05-13

## Outcomes & Retrospective

Milestone 0 is complete. The repository now has a compatibility safety net for the current Linear behavior before adapter migration begins: flat Linear config parsing, issue template variables, scheduled failure dedupe, command acknowledgement replay, handoff formatting, invalid-workflow reporting, and old ledger/checkpoint recovery all have explicit characterization coverage. The only deviations from the written step examples are documented in Surprises & Discoveries: list-valued `issue.labels` renders through loops, not direct interpolation, and current env references use `$VAR` syntax.

LIV-268 completed the task-domain half of Milestone 1. `src/scherzo/task.gleam` now defines the backend-neutral task data model and pure Linear compatibility conversions, while the adapter capability contract remains for the next Milestone 1 implementation slice.

## Context and Orientation

Scherzo is a Gleam application. Production source lives under `src/scherzo/`, tests live under `test/`, examples live under `examples/`, dogfood workflows live under `.scherzo/workflows/`, and operational docs live under `docs/`.

The existing tracker boundary is small. `src/scherzo/tracker.gleam` defines `Client` with three read functions. `src/scherzo/tracker/kind.gleam` has one production variant, `LinearTracker`. `src/scherzo/tracker/issue.gleam` defines `Issue`, with fields `id`, `identifier`, `title`, `description`, `priority`, `state`, `branch_name`, `url`, `labels`, `blocked_by`, `blocked_by_complete`, `created_at`, and `updated_at`. `src/scherzo/tracker/state.gleam` defines `IssueState` and normalized keys.

The Linear adapter implementation is broader than `tracker.Client`. `src/scherzo/linear.gleam` builds GraphQL requests and parses responses for candidate discovery, state refresh, comments, mutations, contract checks, and attachments. Other Linear-specific modules include `src/scherzo/linear_contract.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/linear_attachment.gleam`, `src/scherzo/linear_body_data.gleam`, and `src/scherzo/linear_comment_format.gleam`.

The orchestrator currently injects several separate Linear-shaped dependencies. In `src/scherzo/orchestrator/daemon.gleam`, `RuntimeDependencies` includes `make_tracker`, `make_handoff`, `make_linear_commands`, `make_triage`, and `make_scheduled_failure_reporter`. The runtime `State` stores `tracker_client`, `handoff_client`, `linear_command_client`, `triage_client`, and `scheduled_failure_reporter`. `src/scherzo/orchestrator/effects/types.gleam` includes effects and continuations named `FetchLinearCommands`, `PostLinearCommandAck`, `ReplayLinearCommandAck`, `ApplyLinearCommand`, and `EnqueueLinearCommandAck`.

Workflow execution is now DAG-based. `src/scherzo/workflow_dag.gleam` parses `version: 1` workflow DAGs with steps, dependencies, workspace refs, command steps, agent steps, structured output, validators, and failure policies. `src/scherzo/workflow_run.gleam` runs these workflows and has both fresh and recovered run contexts. `src/scherzo/workflow_checkpoint.gleam` writes step and workflow checkpoint records into the ledger and artifact store. This means the adapter design must preserve run IDs, step IDs, workspace names, artifacts, structured-output metadata, and pi session continuation data, not only issue polling.

Scheduled workflows are taskless until they need to publish a failure. `src/scherzo/config/types.gleam` defines `ScheduledJobConfig`, `ScheduledFailureConfig`, and `ScheduledLinearFailureConfig`. `src/scherzo/scheduled_failure_reporter.gleam` deduplicates one visible failure issue per job using reserved labels and a body marker, then creates, comments on, or moves a Linear issue. The refreshed architecture must let scheduled runs remain taskless and let failure publication be an optional tracker capability.

Prompt rendering currently exposes issue variables. `src/scherzo/template.gleam` has `InvocationContext` variants for issue and scheduled invocations and resolves variables such as `issue.id`, `issue.identifier`, `issue.title`, `issue.description`, `issue.priority`, `issue.state`, `issue.branch_name`, `issue.url`, and `issue.labels`. New `task.*` variables should be added as preferred aliases while those `issue.*` names keep working.

## Preconditions and Verified Facts

From the repository root, use the direnv-backed commands in this plan. If `direnv exec . <command>` says the workspace `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. Treat that as environment setup, not as a test failure.

The plan assumes these files exist in the current tree: `src/scherzo/tracker.gleam`, `src/scherzo/tracker/issue.gleam`, `src/scherzo/tracker/kind.gleam`, `src/scherzo/tracker/state.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/main.gleam`, `src/scherzo/doctor.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/template.gleam`, and `src/scherzo/scheduled_failure_reporter.gleam`.

The current config model has `TrackerConfig` with `kind`, `endpoint`, `api_key`, `project_slug`, `active_states`, `dispatch_states`, and `terminal_states`. It also has `LinearContractConfig`, `LinearCommandConfig`, and `ScheduledFailureConfig(linear: ScheduledLinearFailureConfig)`. `EffectiveConfig` contains `tracker`, `handoff`, `linear_contract`, and `linear_commands`.

The current DAG workflow and recovery model passes issue identifiers through run execution. `workflow_run.StepContext` includes `issue_id` and `issue_identifier`, and `workflow_checkpoint.WorkflowFinished` includes `issue_id`. This plan must not delete or reinterpret those fields until compatibility decoding and recovery tests exist.

The current scheduled failure reporter has a `Backend` abstraction but its operations are named in issue terms: `find_open_issue_by_id`, `find_open_issues_by_labels`, `create_issue`, `comment_issue`, and `move_issue_to_state`. This can be adapted, but the Linear behavior must remain identical.

The existing test map relevant to this plan includes `test/linear_test.gleam`, `test/linear_comments_test.gleam`, `test/linear_comment_format_test.gleam`, `test/linear_attachment_test.gleam`, `test/linear_attachment_graphql_test.gleam`, `test/linear_contract_test.gleam`, `test/linear_triage_test.gleam`, `test/linear_command_transport_test.gleam`, `test/linear_command_parser_test.gleam`, `test/linear_command_config_test.gleam`, `test/config_test.gleam`, `test/config_types_test.gleam`, `test/doctor_test.gleam`, `test/main_test.gleam`, `test/orchestrator_config_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, `test/orchestrator_effect_runner_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, `test/state_record_test.gleam`, `test/state_recovery_test.gleam`, `test/state_projection_test.gleam`, `test/recovery_workflow_checkpoint_test.gleam`, `test/workflow_run_test.gleam`, `test/workflow_scheduler_test.gleam`, `test/template_test.gleam`, `test/workspace_run_test.gleam`, `test/session_name_test.gleam`, `test/handoff_test.gleam`, `test/handoff_format_test.gleam`, `test/scheduled_failure_reporter_test.gleam`, `test/smoke_test.gleam`, and `test/workspace_driver_docs_test.gleam`.

The LIV-263 research findings concluded that Scherzo is only partially abstracted today, that task-system capability differences are material, and that the recommended path is to wrap Linear behind a capability-aware task-system adapter, prove the seam with a fake non-Linear adapter in tests, and defer production Jira/Trello adapters.

## Scope Boundaries

In scope: backend-neutral task domain types; tracker adapter capability types; a Linear adapter that delegates to existing Linear modules; fake non-Linear adapter tests; orchestrator dependency consolidation; generic remote command events; generic handoff and invalid-workflow reporting capability calls; scheduled failure publication capability calls; config validation for unsupported capability combinations; `task.*` template variables; compatibility preservation for `issue.*` variables, existing Linear config, existing CLI aliases, and legacy ledger records; docs and prompt terminology updates needed to explain the new boundary; and a tracker adapter readiness matrix for future Jira/Trello follow-up work.

Out of scope: production Jira adapter, production Trello adapter, live backend integration tests in the default suite, multi-backend operation in one daemon, deleting all `linear` module names, deleting all `issue` compatibility names, rewriting Linear GraphQL internals, changing workspace driver behavior, changing pi RPC behavior, and changing the workflow DAG format except where task terminology is exposed through templates or runtime context.

The boundary for terminology is explicit. New domain records, new adapter APIs, new docs, new generic config keys, and new prompt variables use `task`. Existing external compatibility surfaces may keep `issue`: old prompt variables, old environment variable names, old durable record fields, current tests that verify Linear behavior, and Linear-specific docs. `tracker` remains the subsystem and config name for choosing a backend adapter.

The fake non-Linear adapter is a test tool, not a production backend. It may live under `test/support/fake_tracker_adapter.gleam` and may use an in-memory state record. It must not be advertised in production examples except as a clearly marked test-only adapter in `docs/runbooks/tracker-adapters.md`.

## Stop/Go Gates and Coupling Targets

Gate A is after Milestone 2. At this point, `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, `src/scherzo/tracker/linear_adapter.gleam`, and fake adapter tests exist, but the production orchestrator may still call old Linear-shaped dependencies. It is safe to stop here because runtime behavior has not moved. The value is a compiled, tested adapter contract and a Linear wrapper ready for later migration.

Gate B is after Milestone 3. At this point, workflow/template context is task-aware and recovery tests prove old and new records coexist, but remote commands, handoff, and scheduled failure publication may still use old Linear paths. It is safe to stop here because user-visible Linear behavior and side effects remain on the proven path while task terminology is available to workflows.

Gate C is after Milestone 4. At this point, generic orchestrator side effects use adapter capabilities and unsupported capability combinations fail at startup. It is safe to stop here because the runtime path has moved behind a validated adapter boundary, config and docs aliases can still land separately in Milestone 5, and Linear compatibility remains through `src/scherzo/tracker/linear_adapter.gleam`.

The final coupling target is measurable. At Milestone 6, run the coupling inventory command in Validation and Acceptance. It should find no direct imports of `scherzo/linear`, `scherzo/handoff`, `scherzo/linear_triage`, `scherzo/linear_contract`, `scherzo/smoke`, or `scherzo/scheduled_failure_reporter` from generic orchestrator side-effect modules, workflow run/checkpoint modules, or template task-rendering code. Remaining Linear imports must be documented in `docs/runbooks/tracker-adapters.md` as either Linear adapter internals, Linear compatibility tests, CLI compatibility aliases, or Linear-specific setup docs.

## Linear Issue Mapping and Implementation Slices

This ExecPlan is the canonical plan for the core architecture work. Do not create more ExecPlans for Milestones 0 through 6 unless a milestone discovers a materially new design problem that cannot be handled by updating this living document. Linear child issues under `LIV-265` are implementation tracking slices for this plan, not competing sources of design truth.

`LIV-266` tracks this canonical plan and its review. `LIV-267` implements Milestone 0, the Linear characterization safety net. `LIV-268` and `LIV-269` implement Milestone 1, the task domain and tracker adapter contract. `LIV-270` and `LIV-274` implement Milestone 2, the Linear wrapper and fake non-Linear seam proof. `LIV-272` implements the durable recovery and remote-command compatibility portion that starts in Milestone 3 and continues through Milestone 4. `LIV-271` implements the Milestone 4 orchestrator and side-effect routing migration once the durable command model is ready. `LIV-273` and `LIV-275` implement Milestone 5, covering config, CLI, doctor, smoke, contract, docs, prompts, scripts, and operator UX. `LIV-276` and `LIV-277` remain future ExecPlan tasks for production Jira and Trello adapters and should stay blocked until Gate C or final acceptance proves the adapter seam.

If implementation slices need to be split further, split them by a concrete validation boundary from this plan, such as a test file group or a stop/go gate. Do not split by creating a second architecture plan that restates this one. When a slice is completed, update this plan's Progress section and the corresponding Linear issue so the plan and board remain synchronized.

## Adapter API Contract

Milestone 1 must make `src/scherzo/task.gleam` and `src/scherzo/tracker/adapter.gleam` prescriptive. The implementer may split very large records into smaller records if Gleam formatting or import cycles require it, but names, fields, semantics, and validation messages must remain equivalent to this section. Do not leave new design choices for later milestones.

In `src/scherzo/task.gleam`, define these public types and helpers. Use `String` for `backend_kind` in `TaskRef` so task identity does not create an import cycle with `tracker/kind.gleam`.

    pub type TaskRef {
      TaskRef(
        backend_kind: String,
        remote_id: String,
        key: Option(String),
        url: Option(String),
      )
    }

    pub type TaskStateCategory {
      Backlog
      Ready
      Active
      Done
      Canceled
      Duplicate
      Unknown
    }

    pub type TaskState {
      TaskState(id: Option(String), name: String, category: TaskStateCategory)
    }

    pub type TaskLabel {
      TaskLabel(id: Option(String), name: String)
    }

    pub type TaskComment {
      TaskComment(
        id: String,
        task: TaskRef,
        author_id: Option(String),
        body: String,
        created_at: Option(birl.Time),
        updated_at: Option(birl.Time),
      )
    }

    pub type TaskAttachment {
      TaskAttachment(id: String, task: TaskRef, name: String, url: String)
    }

    pub type TaskLink {
      TaskLink(id: Option(String), task: TaskRef, title: String, url: String)
    }

    pub type Task {
      Task(
        ref: TaskRef,
        title: String,
        description: Option(String),
        priority: Option(Int),
        state: TaskState,
        branch_hint: Option(String),
        labels: List(TaskLabel),
        blockers: List(TaskRef),
        blockers_complete: Bool,
        created_at: Option(birl.Time),
        updated_at: Option(birl.Time),
      )
    }

Also define `display_key(ref: TaskRef) -> String`, which returns `key` when present and non-empty and otherwise returns `remote_id`; `label_names(task: Task) -> List(String)`; `from_legacy_issue(issue: tracker_issue.Issue) -> Task`; and `to_legacy_issue(task: Task) -> Result(tracker_issue.Issue, TaskConversionError)`. `from_legacy_issue` must preserve `id`, `identifier`, `title`, `description`, `priority`, `state.name`, `branch_name` as `branch_hint`, `url`, labels, blockers, blocker completeness, and timestamps. `to_legacy_issue` is only valid for a task whose `backend_kind` is `linear` and whose `key` is present; it returns `Error(RequiresLinearTask)` or `Error(MissingTaskKey)` otherwise.

    pub type TaskConversionError {
      RequiresLinearTask
      MissingTaskKey
    }

In `src/scherzo/tracker/adapter.gleam`, define a `TrackerAdapter` record and capability records with these semantics:

    pub type TrackerAdapter {
      TrackerAdapter(
        kind: String,
        display_name: String,
        task_source: TaskSourceCapability,
        comments: Option(CommentCapability),
        remote_commands: Option(RemoteCommandCapability),
        state_transitions: Option(StateTransitionCapability),
        routing_metadata: Option(RoutingMetadataCapability),
        links: Option(LinkCapability),
        handoff: Option(HandoffCapability),
        scheduled_failures: Option(ScheduledFailureCapability),
        readiness: Option(ReadinessCapability),
        smoke: Option(SmokeCapability),
        attachments: Option(AttachmentCapability),
      )
    }

    pub type TaskSearchRequest {
      TaskSearchRequest(
        active_states: List(String),
        dispatch_states: List(String),
        terminal_states: List(String),
        workflow_labels: List(String),
        limit: Int,
      )
    }

    pub type TaskSourceCapability {
      TaskSourceCapability(
        fetch_candidates: fn(TaskSearchRequest) -> Result(List(task.Task), TrackerError),
        refresh_by_refs: fn(List(task.TaskRef)) -> Result(List(task.Task), TrackerError),
        lookup_by_operator_ref: fn(String) -> Result(Option(task.Task), TrackerError),
      )
    }

`fetch_candidates` is the replacement for `tracker.Client.fetch_candidate_issues`. It returns tasks eligible for dispatch after applying backend-side filters where available. `refresh_by_refs` is the replacement for state refresh by IDs. `lookup_by_operator_ref` supports local operator commands that refer to a key such as `LIV-266` or a backend remote ID.

    pub type CommentWriteMode {
      CreateOnly
      UpdateExisting(comment_id: String, allow_create_fallback: Bool)
    }

    pub type CommentRequest {
      CommentRequest(task: task.TaskRef, body: String, mode: CommentWriteMode)
    }

    pub type CommentReceipt {
      CommentReceipt(id: String, task: task.TaskRef, url: Option(String), created: Bool)
    }

    pub type CommentCapability {
      CommentCapability(
        post_or_update: fn(CommentRequest) -> Result(CommentReceipt, TrackerError),
      )
    }

`CommentCapability.post_or_update` must update an existing comment only when the backend supports update and the request uses `UpdateExisting`. If update is unsupported and `allow_create_fallback` is `True`, it must create a new comment and return `created: True`. If update is unsupported and fallback is not allowed, it must return `Error(UnsupportedCapability("comments.update"))`.

    pub type RemoteCommandEvent {
      RemoteCommandEvent(
        event_id: String,
        task: task.TaskRef,
        author_id: String,
        body: String,
        command_name: String,
        excerpt: String,
        observed_at_ms: Int,
      )
    }

    pub type RemoteCommandFetch {
      RemoteCommandFetch(task_refs: List(task.TaskRef), since_event_ids: List(String))
    }

    pub type RemoteCommandAck {
      RemoteCommandAck(event: RemoteCommandEvent, body: String)
    }

    pub type RemoteCommandCapability {
      RemoteCommandCapability(
        fetch_events: fn(RemoteCommandFetch) -> Result(List(RemoteCommandEvent), TrackerError),
        post_ack: fn(RemoteCommandAck) -> Result(CommentReceipt, TrackerError),
      )
    }

For Linear, `RemoteCommandEvent.event_id` is the Linear comment ID, and `post_ack` must preserve the current Linear acknowledgement body exactly. A backend without comments cannot expose `RemoteCommandCapability`, because Scherzo's current remote command UX requires an acknowledgement posted back to the task.

    pub type StateTransitionRequest {
      StateTransitionRequest(task: task.TaskRef, target_state_id: Option(String), target_state_name: String, reason: String)
    }

    pub type StateTransitionReceipt {
      StateTransitionReceipt(task: task.TaskRef, state: task.TaskState)
    }

    pub type StateTransitionCapability {
      StateTransitionCapability(
        transition: fn(StateTransitionRequest) -> Result(StateTransitionReceipt, TrackerError),
      )
    }

    pub type RoutingMetadataCapability {
      RoutingMetadataCapability(
        workflow_labels: fn(task.Task) -> List(String),
        blocker_refs: fn(task.Task) -> List(task.TaskRef),
      )
    }

    pub type LinkCapability {
      LinkCapability(upsert_link: fn(task.TaskRef, task.TaskLink) -> Result(task.TaskLink, TrackerError))
    }

    pub type HandoffEvent {
      HandoffClaim(task: task.TaskRef, workspace_path: String, run_id: String)
      HandoffSuccess(task: task.TaskRef, run_id: String, summary: String)
      HandoffFailure(task: task.TaskRef, run_id: String, reason: String)
      HandoffPark(task: task.TaskRef, reason: String, release_policy: String)
    }

    pub type HandoffCapability {
      HandoffCapability(report: fn(HandoffEvent) -> Result(Nil, TrackerError))
    }

    pub type ScheduledFailurePublication {
      ScheduledFailurePublication(
        job_id: String,
        workflow_id: String,
        run_id: String,
        dedupe_key: String,
        title: String,
        body: String,
        labels: List(String),
        target_state_name: Option(String),
      )
    }

    pub type ScheduledFailureReceipt {
      ScheduledFailureReceipt(task: task.TaskRef, created: Bool, comment_id: Option(String))
    }

    pub type ScheduledFailureCapability {
      ScheduledFailureCapability(
        publish: fn(ScheduledFailurePublication) -> Result(ScheduledFailureReceipt, TrackerError),
      )
    }

`ScheduledFailureCapability.publish` must be idempotent for the same `dedupe_key`. For Linear, this means preserving the current reserved labels and body marker behavior from `src/scherzo/scheduled_failure_reporter.gleam`: one visible issue per job is created or updated, not one issue per failed run.

    pub type ReadinessFinding {
      ReadinessFinding(
        severity: String,
        code: String,
        message: String,
        config_path: Option(String),
      )
    }

    pub type ReadinessCapability {
      ReadinessCapability(check_contract: fn() -> Result(List(ReadinessFinding), TrackerError))
    }

    pub type SmokeReport {
      SmokeReport(
        candidate_count: Int,
        refreshed_count: Int,
        terminal_sample_count: Int,
        messages: List(String),
      )
    }

    pub type SmokeCapability {
      SmokeCapability(run_smoke_check: fn() -> Result(SmokeReport, TrackerError))
    }

    pub type AttachmentCapability {
      AttachmentCapability(
        upload: fn(task.TaskRef, task.TaskAttachment) -> Result(task.TaskAttachment, TrackerError),
      )
    }

    pub type TrackerError {
      Unauthorized(message: String)
      NotFound(ref: task.TaskRef)
      Transient(message: String)
      Permanent(message: String)
      UnsupportedCapability(capability: String)
      DecodeFailed(message: String)
    }

Startup validation belongs in `src/scherzo/tracker/adapter.gleam` as pure functions first, then is called from config/service startup. Define `validate_required_capabilities(adapter: TrackerAdapter, requirements: TrackerRequirements) -> Result(Nil, List(CapabilityValidationError))`. `TrackerRequirements` is derived from `EffectiveConfig` and contains these fields. `workflow_label_paths` contains config paths such as `workflows.execplan.label` only when label-based routing is configured. `scheduled_failure_paths` contains config paths such as `scheduled_jobs.nightly.on_failure` only when scheduled failure publication is configured.

    pub type TrackerRequirements {
      TrackerRequirements(
        remote_commands_enabled: Bool,
        remote_commands_config_path: Option(String),
        handoff_comments_enabled: Bool,
        handoff_state_moves_enabled: Bool,
        handoff_config_path: Option(String),
        workflow_label_paths: List(String),
        scheduled_failure_paths: List(String),
        readiness_checks_enabled: Bool,
        smoke_checks_enabled: Bool,
      )
    }

    pub type CapabilityValidationError {
      CapabilityValidationError(
        feature: String,
        capability: String,
        config_path: String,
        backend_kind: String,
        message: String,
      )
    }

The exact startup error messages to test are:

- `tracker_capability_missing feature=remote_commands capability=remote_commands path=linear_commands.enabled backend=test-memory message="linear_commands.enabled requires tracker adapter test-memory to expose remote_commands"`
- `tracker_capability_missing feature=remote_command_ack capability=comments path=linear_commands.enabled backend=test-memory message="remote command acknowledgements require comments capability"`
- `tracker_capability_missing feature=handoff_state_moves capability=state_transitions path=handoff.states backend=test-memory message="handoff state moves require state_transitions capability"`
- `tracker_capability_missing feature=workflow_label_routing capability=routing_metadata path=workflows.<id>.label backend=test-memory message="workflow label routing requires routing_metadata capability"`
- `tracker_capability_missing feature=scheduled_failures capability=scheduled_failures path=scheduled_jobs.<id>.on_failure backend=test-memory message="scheduled failure publication requires scheduled_failures capability"`
- `tracker_capability_missing feature=tracker_contract capability=readiness path=doctor.checks.tracker-contract backend=test-memory message="tracker contract checks require readiness capability"`
- `tracker_capability_missing feature=tracker_smoke capability=smoke path=doctor.checks.tracker-smoke backend=test-memory message="tracker smoke checks require smoke capability"`

The exact `config_path` string may use a concrete workflow or scheduled job ID when available, such as `workflows.execplan.label` or `scheduled_jobs.nightly.on_failure`, but the message must include the generic path segment shown above so operators know where to edit config.

## Durable Recovery Migration Design

The recovery design is intentionally conservative. Do not bump `src/scherzo/state/record.gleam`'s ledger `schema_version` in this plan. Do not delete old record constructors. Do not require a one-time ledger rewrite. Recovery must accept old-only ledgers, new-only ledgers, and mixed ledgers where a run began before the migration and resumed after it.

Add a small shared record helper in `src/scherzo/state/record.gleam` or a nearby module if needed:

    pub type TaskRefFields {
      TaskRefFields(
        task_backend_kind: String,
        task_remote_id: String,
        task_key: Option(String),
        task_url: Option(String),
      )
    }

For workflow and worker records that currently require `issue_id` and `issue_identifier`, new writes for task-dispatched work must continue to write the old fields and must also write task fields. The compatibility mapping is: for Linear, `issue_id` equals `task_remote_id` and `issue_identifier` equals `task_key` when present; for fake non-Linear adapter tests, `issue_id` equals `task_remote_id` and `issue_identifier` equals `task.key` or `task_remote_id`. This keeps existing dictionaries keyed by `issue_id` stable while making backend identity explicit for new code.

Old `workflow_run_started` JSON shape:

    {"schema_version":2,"record_id":"old-1","at_ms":1,"kind":"workflow_run_started","run_id":"run-1","workflow_id":"execplan","workflow_fingerprint":"wf-old","issue_id":"issue-1","issue_identifier":"LIV-266","issue_fingerprint":"fp-old","observed_updated_at_ms":10,"run_root":"test/tmp/run-root"}

New `workflow_run_started` JSON shape:

    {"schema_version":2,"record_id":"new-1","at_ms":2,"kind":"workflow_run_started","run_id":"run-1","workflow_id":"execplan","workflow_fingerprint":"wf-new","issue_id":"issue-1","issue_identifier":"LIV-266","task_backend_kind":"linear","task_remote_id":"issue-1","task_key":"LIV-266","task_url":"https://linear.app/living-systems/issue/LIV-266","issue_fingerprint":"fp-new","observed_updated_at_ms":20,"run_root":"test/tmp/run-root"}

Old `workflow_run_finished` JSON shape:

    {"schema_version":2,"record_id":"old-2","at_ms":3,"kind":"workflow_run_finished","run_id":"run-1","workflow_id":"execplan","issue_id":"issue-1","outcome":"success","token_total":10,"turns":2}

New `workflow_run_finished` JSON shape:

    {"schema_version":2,"record_id":"new-2","at_ms":4,"kind":"workflow_run_finished","run_id":"run-1","workflow_id":"execplan","issue_id":"issue-1","task_backend_kind":"linear","task_remote_id":"issue-1","task_key":"LIV-266","task_url":"https://linear.app/living-systems/issue/LIV-266","outcome":"success","token_total":10,"turns":2}

Old `step_attempt_pi_session_recorded` JSON shape keeps `issue_id` and `issue_identifier`. New writes add the same four task fields. The test fixture should use `session_file":"state/sessions/run-1/step-1.json"` or another repository-relative fixture path, not an absolute local path.

For remote commands, add new generic record constructors while decoding old Linear records forever. Old records remain:

    {"schema_version":2,"record_id":"cmd-old-1","at_ms":5,"kind":"linear_command_seen","comment_id":"comment-1","issue_id":"issue-1","author_id":"user-1","command_name":"retry","excerpt":"/scherzo retry"}
    {"schema_version":2,"record_id":"cmd-old-2","at_ms":6,"kind":"linear_command_started","comment_id":"comment-1","issue_id":"issue-1","command_name":"retry"}
    {"schema_version":2,"record_id":"cmd-old-3","at_ms":7,"kind":"linear_command_completed","comment_id":"comment-1","issue_id":"issue-1","status":"ok","message_excerpt":"Retry queued"}
    {"schema_version":2,"record_id":"cmd-old-4","at_ms":8,"kind":"linear_command_acked","comment_id":"comment-1","issue_id":"issue-1"}

New records are:

    {"schema_version":2,"record_id":"cmd-new-1","at_ms":9,"kind":"remote_command_seen","backend_kind":"linear","event_id":"comment-1","task_remote_id":"issue-1","task_key":"LIV-266","author_id":"user-1","command_name":"retry","excerpt":"/scherzo retry"}
    {"schema_version":2,"record_id":"cmd-new-2","at_ms":10,"kind":"remote_command_started","backend_kind":"linear","event_id":"comment-1","task_remote_id":"issue-1","command_name":"retry"}
    {"schema_version":2,"record_id":"cmd-new-3","at_ms":11,"kind":"remote_command_completed","backend_kind":"linear","event_id":"comment-1","task_remote_id":"issue-1","status":"ok","message_excerpt":"Retry queued"}
    {"schema_version":2,"record_id":"cmd-new-4","at_ms":12,"kind":"remote_command_acked","backend_kind":"linear","event_id":"comment-1","task_remote_id":"issue-1"}

Outbox compatibility must follow the same rule. Existing `OutboxPendingV2` records with `outbox_kind: "linear_command_ack"` and payload `{"type":"linear_command_ack","body":"ack"}` continue to decode and replay through the Linear adapter. New remote command acknowledgements use `outbox_kind: "remote_command_ack"` and payload `{"type":"remote_command_ack","backend_kind":"linear","event_id":"comment-1","task_remote_id":"issue-1","body":"ack"}`. Mixed-ledger recovery must not post duplicate acknowledgements when both old and new completion records refer to the same Linear comment ID.

Affected files for durable recovery are `src/scherzo/state/record.gleam` for constructors, encoders, and decoders; `src/scherzo/state/projection.gleam` and `src/scherzo/state/recovery.gleam` for runtime projection; `src/scherzo/workflow_checkpoint.gleam` for new task fields on checkpoint writes; `src/scherzo/workflow_run.gleam` for recovered task context; `src/scherzo/orchestrator/effects/types.gleam` and `src/scherzo/orchestrator/transition_types.gleam` for generic remote command names; and `test/state_record_test.gleam`, `test/state_recovery_test.gleam`, `test/state_projection_test.gleam`, and `test/recovery_workflow_checkpoint_test.gleam` for fixtures and assertions.

Exact fixture tests to add are named in Testing and Falsifiability. The most important assertions are: old `workflow_run_started` decodes to `TaskRef("linear", "issue-1", Some("LIV-266"), None)` by compatibility mapping; new `workflow_run_started` decodes to the explicit task URL when present; a mixed old-start/new-finish run recovers one task identity; old `linear_command_*` records and new `remote_command_*` records project to one generic remote command state; and old `linear_command_ack` outbox payloads replay through the new adapter ack path.

## Test, Fixture, and Documentation Inventory

Milestone 0 changes or adds tests only. Use `test/config_test.gleam` for old flat Linear tracker config parsing; `test/template_test.gleam` for `issue.*` variable characterization; `test/scheduled_failure_reporter_test.gleam` for current Linear scheduled failure dedupe; `test/orchestrator_daemon_linear_command_test.gleam` for Linear command ack persistence; `test/handoff_test.gleam` and `test/handoff_format_test.gleam` for handoff comment formatting; `test/state_recovery_test.gleam` and `test/recovery_workflow_checkpoint_test.gleam` for old ledger/checkpoint recovery.

Milestone 1 adds `test/task_test.gleam` and `test/tracker_adapter_test.gleam`, then adds `src/scherzo/task.gleam` and `src/scherzo/tracker/adapter.gleam`.

Milestone 2 adds `test/tracker_linear_adapter_test.gleam`, `test/tracker_fake_adapter_test.gleam`, and `test/support/fake_tracker_adapter.gleam`, then adds `src/scherzo/tracker/linear_adapter.gleam`. If the fake adapter needs shared helpers, keep them under `test/support/` unless production code genuinely needs them.

Milestone 3 updates `test/template_test.gleam`, `test/workflow_run_test.gleam`, `test/recovery_workflow_checkpoint_test.gleam`, `test/state_record_test.gleam`, `test/state_projection_test.gleam`, `test/state_recovery_test.gleam`, `test/workspace_run_test.gleam`, and `test/session_name_test.gleam` as needed. It updates production files `src/scherzo/template.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/workspace_run.gleam`, and `src/scherzo/session/name.gleam`.

Milestone 4 updates `test/orchestrator_daemon_linear_command_test.gleam`, `test/orchestrator_effect_runner_test.gleam`, `test/orchestrator_config_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, `test/scheduled_failure_reporter_test.gleam`, `test/handoff_test.gleam`, and `test/linear_triage_test.gleam`. It updates production files `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effects/types.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam`, `src/scherzo/runtime_bundle.gleam`, and `src/scherzo/orchestrator/service.gleam`.

Milestone 5 updates `test/config_test.gleam`, `test/config_types_test.gleam`, `test/doctor_test.gleam`, `test/main_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, and a new `test/tracker_adapter_docs_test.gleam` if no existing docs test is a natural fit. Documentation and prompt updates are limited to `docs/GETTING_STARTED.md`, `docs/ARCHITECTURE.md`, `docs/runbooks/scheduled-jobs.md`, new `docs/runbooks/tracker-adapters.md`, `.scherzo/workflows/prompts/execplan-draft.md`, `.scherzo/workflows/prompts/execplan-implementation-implement.md`, `.scherzo/workflows/prompts/execplan-implementation-review.md`, and `.scherzo/workflows/prompts/execplan-incorporate-review.md`. Do not rewrite historical plan files.

If an exact test file named above has moved by implementation time, find the replacement with `grep` or `find`, record the discovery in Surprises & Discoveries, and update this inventory before editing code. Do not silently choose a different location.

## Milestones

Milestone 0 freezes existing behavior and inventories current coupling. At the end, tests prove the current Linear behavior still passes before architecture changes begin. This milestone comes first because it gives the later refactor a safety net.

Milestone 1 adds backend-neutral domain and capability types with no runtime behavior change. At the end, the codebase has concrete `Task`, `TaskRef`, state, label, comment, attachment, link, capability, validation-error, and conversion types that compile and are tested independently. This reduces design risk before any orchestrator call sites move.

Milestone 2 wraps Linear behind the adapter boundary and adds a fake non-Linear adapter for tests. At the end, Linear behavior still delegates to existing modules, and the fake adapter proves candidate discovery, task refresh, comments, and state transitions are expressed without Linear types. This milestone proves the seam before the daemon depends on it and is Gate A.

Milestone 3 routes issue-dispatched DAG workflow execution through task terminology while preserving recovery. At the end, workflow DAG runs, step contexts, template rendering, workspace/session naming, and pi session observations can carry a `TaskRef` and preferred `task.*` variables, while `issue.*`, `issue_id`, and `issue_identifier` remain compatibility aliases. This is Gate B because side effects can still be left on the old Linear path if needed.

Milestone 4 moves remote commands, handoff, invalid-workflow reporting, and scheduled failure reporting behind adapter capabilities. At the end, the orchestrator no longer has separate Linear command, triage, handoff, and scheduled-failure dependencies. Unsupported capability combinations fail at startup with actionable messages. This is Gate C because the high-risk side-effect migration is complete and testable independently from docs/CLI cleanup.

Milestone 5 migrates config, CLI, docs, prompts, and validation language. At the end, operators see task-system-agnostic names such as tracker smoke/contract commands and task prompt variables, while old Linear names remain aliases with compatibility notes.

Milestone 6 performs final validation and records remaining backend gaps. At the end, all tests and lint gates pass, a fake non-Linear dispatch scenario passes, legacy recovery fixtures still decode, the coupling inventory is documented, and follow-up tickets can be split for Jira, Trello, or removal of compatibility aliases.

## Plan of Work

In Milestone 0, add characterization tests before moving code. The tests should pass on the current implementation or expose a baseline surprise that must be recorded before refactoring. Cover old flat Linear config parsing, `issue.*` template variables, scheduled failure dedupe, Linear command acknowledgement persistence, invalid workflow reporting, handoff comment formatting, and recovery from old `linear_command_*` ledger records.

In Milestone 1, create `src/scherzo/task.gleam` and `src/scherzo/tracker/adapter.gleam` from the Adapter API Contract. Do not remove `src/scherzo/tracker/issue.gleam`; instead add conversion functions between `tracker/issue.Issue` and `task.Task` so old call sites can keep compiling while new call sites use the new domain. Validation functions are pure in this milestone and are not yet wired into daemon startup.

In Milestone 2, create `src/scherzo/tracker/linear_adapter.gleam`. Its implementation should call existing `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/smoke.gleam`, and `src/scherzo/scheduled_failure_reporter.gleam` rather than rewriting GraphQL. Add a fake adapter under `test/support/fake_tracker_adapter.gleam` and keep it out of production config examples.

In Milestone 3, change workflow-facing structures to understand task subjects. Add a `TaskInvocation` or equivalent new constructor alongside the existing issue invocation path in `src/scherzo/template.gleam`. Add `task.id`, `task.key`, `task.identifier`, `task.title`, `task.description`, `task.priority`, `task.state`, `task.branch_hint`, `task.url`, and `task.labels` variables. Keep every existing `issue.*` variable with identical rendered values. In `src/scherzo/workflow_run.gleam` and `src/scherzo/workflow_checkpoint.gleam`, carry backend-neutral task reference data for new records while preserving old `issue_id` fields for compatibility. In workspace/session code, add `SCHERZO_TASK_ID`, `SCHERZO_TASK_KEY`, and related env values while keeping `SCHERZO_ISSUE_ID` and `SCHERZO_ISSUE_IDENTIFIER`.

In Milestone 4, replace Linear-specific orchestrator dependencies with one adapter factory and capability lookups. `RuntimeDependencies` in `src/scherzo/orchestrator/daemon.gleam` should move from `make_tracker`, `make_handoff`, `make_linear_commands`, `make_triage`, and `make_scheduled_failure_reporter` toward a single `make_tracker_adapter`. `src/scherzo/orchestrator/effects/types.gleam` should add generic effect names such as `FetchRemoteCommands`, `PostRemoteCommandAck`, and `ApplyRemoteCommand`. Decode old ledger records and old recovery state into the generic model. Keep compatibility tests that prove existing Linear command comments still work.

In Milestone 5, update configuration in `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam` to accept a nested backend-specific shape while preserving current flat Linear fields. The preferred shape remains under `tracker` because `tracker` is the subsystem name. The immediate production backend is `linear`; fake/test backend config must not appear in production examples except where clearly marked as test-only. Add generic CLI aliases such as `--tracker-smoke` and `--tracker-contract-check` while keeping old `--linear-smoke` and `--linear-contract-check` aliases. Add doctor check names `tracker-smoke` and `tracker-contract` while keeping `linear-smoke` and `linear-contract` as aliases.

In Milestone 6, run the full validation commands and inspect user-facing output. The final state should have no generic orchestrator or scheduled-job code that directly needs a Linear command client, triage client, handoff client, or scheduled failure reporter outside the Linear adapter. It is acceptable for Linear-specific modules and tests to remain under Linear names.

## Concrete Steps

1. From the repo root, inspect source control status with the workspace driver:

        $SCHERZO_WORKSPACE_DRIVER status --human

   Expect a clean or intentionally modified working copy. Do not start implementation on top of unrelated changes.

2. Run the current test baseline:

        direnv exec . gleam test

   Expect all tests to pass. If tests fail before changes, record the failure in this plan under Surprises & Discoveries and stop to decide whether the failure is environmental or a real baseline problem.

3. In `test/config_test.gleam`, add a characterization test named `flat_linear_tracker_config_aliases_still_parse_test`. The input YAML should use the current flat shape under `tracker` with `kind: linear`, `endpoint: https://api.linear.app/graphql`, `api_key: ${LINEAR_API_KEY}`, `project_slug: example-project`, `active_states: [Todo, In Progress]`, `dispatch_states: [Todo]`, and `terminal_states: [Done, Canceled]`. Assert that the parsed effective config still contains those values. Run `direnv exec . gleam test`; the test should pass on the current implementation.

4. In `test/template_test.gleam`, add `issue_template_variables_are_characterized_test`. Render a task/issue context with id `issue-1`, identifier `LIV-266`, title `Refresh architecture`, branch name `liv-266-refresh`, URL `https://linear.app/living-systems/issue/LIV-266`, state `Todo`, priority `2`, and labels `workflow:execplan` and `kind:feature`. The template should be `{{ issue.id }}|{{ issue.identifier }}|{{ issue.title }}|{{ issue.branch_name }}|{{ issue.labels }}` and the expected output should contain `issue-1|LIV-266|Refresh architecture|liv-266-refresh|workflow:execplan`. Run `direnv exec . gleam test`; the test should pass before any template migration.

5. In `test/scheduled_failure_reporter_test.gleam`, add or strengthen `linear_scheduled_failure_dedupes_one_visible_issue_per_job_test`. Use the existing fake backend pattern in that file. Arrange two failed runs for `job_id: nightly` with the same dedupe marker. Assert that the first call creates one issue and the second call comments on or updates that same issue rather than creating a second issue. Run `direnv exec . gleam test`; the test should pass before adapter work.

6. In `test/orchestrator_daemon_linear_command_test.gleam`, add or strengthen `linear_command_ack_outbox_survives_recovery_test`. Seed a command comment ID `comment-1` for issue `issue-1`, arrange an acknowledgement body `Retry queued`, and assert that recovery replays one acknowledgement and marks it complete. Run `direnv exec . gleam test`; the test should pass before generic remote command work.

7. In `test/state_recovery_test.gleam` and `test/recovery_workflow_checkpoint_test.gleam`, add old-ledger fixtures using the JSON snippets from Durable Recovery Migration Design. Assert old `workflow_run_started`, old `workflow_run_finished`, old `step_attempt_pi_session_recorded`, and old `linear_command_*` records still decode. Run `direnv exec . gleam test`; the tests should pass on the current implementation or expose an existing gap that must be recorded before migration.

8. Commit Milestone 0 after the suite passes. Suggested commit message: `test: characterize current Linear tracker behavior`.

9. Create `test/task_test.gleam` with failing tests `task_display_key_prefers_key_test`, `task_display_key_falls_back_to_remote_id_test`, `issue_to_task_preserves_linear_fields_test`, and `non_linear_task_cannot_convert_to_legacy_issue_test`. Run `direnv exec . gleam test` and expect a compile failure because `scherzo/task` does not exist yet.

10. Add `src/scherzo/task.gleam` with the types and helpers from Adapter API Contract. Implement only pure helpers and conversion from `tracker/issue.Issue`; do not touch the orchestrator. Run `direnv exec . gleam test` and expect the new `test/task_test.gleam` tests to pass.

11. Create `test/tracker_adapter_test.gleam` with failing tests for `validate_required_capabilities`. Include the exact missing-capability messages from Adapter API Contract for remote commands, remote command acknowledgement comments, handoff state moves, workflow label routing, scheduled failures, tracker contract, and tracker smoke. Run `direnv exec . gleam test` and expect a compile failure because `scherzo/tracker/adapter` does not exist yet.

12. Add `src/scherzo/tracker/adapter.gleam` with `TrackerAdapter`, capability records, `TrackerRequirements`, `CapabilityValidationError`, `TrackerError`, and `validate_required_capabilities`. Run `direnv exec . gleam test` and expect `test/task_test.gleam` and `test/tracker_adapter_test.gleam` to pass.

13. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Commit Milestone 1. Suggested commit message: `feat: add task domain and tracker adapter types`.

14. Create `test/tracker_linear_adapter_test.gleam` with failing tests `linear_adapter_fetch_candidates_matches_linear_parser_test`, `linear_adapter_posts_comment_with_existing_linear_body_test`, and `linear_adapter_scheduled_failure_preserves_dedupe_marker_test`. Use existing Linear fake transport fixtures and assert parsed `task.Task` values have `backend_kind: "linear"`, `remote_id: "issue-1"`, `key: Some("LIV-266")`, title `Refresh architecture`, and label `workflow:execplan`. Run `direnv exec . gleam test` and expect a compile failure because `scherzo/tracker/linear_adapter` does not exist yet.

15. Add `src/scherzo/tracker/linear_adapter.gleam`. Delegate to existing Linear, handoff, triage, contract, smoke, and scheduled failure modules. The adapter must not duplicate GraphQL query construction. Run `direnv exec . gleam test` and expect the Linear adapter tests to pass.

16. Add `test/support/fake_tracker_adapter.gleam` and `test/tracker_fake_adapter_test.gleam`. The fake adapter test should define a task `TaskRef("test-memory", "card-1", Some("CARD-1"), None)`, one comment body `hello`, one transition to `Done`, and one scheduled failure dedupe key `nightly`. Assert fetch, refresh, comment, transition, and scheduled failure calls work without importing `scherzo/linear`. Run `direnv exec . gleam test` and expect the fake adapter tests to pass.

17. At Gate A, run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record in Progress that production runtime call sites have not yet moved except through additive wrappers. Commit Milestone 2. Suggested commit message: `feat: wrap Linear in tracker adapter boundary`.

18. In `test/template_test.gleam`, add failing tests `task_template_variables_match_issue_aliases_test`, `task_branch_hint_matches_issue_branch_name_alias_test`, and `scheduled_invocation_does_not_require_task_test`. The first test should render `{{ task.title }}|{{ issue.title }}|{{ task.key }}|{{ issue.identifier }}` for the same Linear task and expect `Refresh architecture|Refresh architecture|LIV-266|LIV-266`. Run `direnv exec . gleam test` and expect unknown-variable failures for `task.*`.

19. Update `src/scherzo/template.gleam` to add the task invocation path and preferred `task.*` variables. Keep `issue.*` variables as exact aliases for task-dispatched Linear runs. Run `direnv exec . gleam test` and expect the template tests to pass.

20. In `test/state_record_test.gleam`, add failing decode/encode tests for the new `task_backend_kind`, `task_remote_id`, `task_key`, and `task_url` fields on `workflow_run_started`, `workflow_run_finished`, and `step_attempt_pi_session_recorded`. Use the exact JSON snippets from Durable Recovery Migration Design. Run `direnv exec . gleam test` and expect decode failures for the new optional fields.

21. Update `src/scherzo/state/record.gleam` so the selected record kinds accept and encode optional task fields while old JSON remains valid. Keep `schema_version` unchanged. Run `direnv exec . gleam test` and expect old and new record tests to pass.

22. In `test/recovery_workflow_checkpoint_test.gleam` and `test/state_recovery_test.gleam`, add failing mixed-ledger tests `mixed_issue_and_task_workflow_records_recover_one_task_ref_test` and `old_linear_command_records_project_to_remote_command_state_test`. Use an old `workflow_run_started` followed by a new `workflow_run_finished`, and old `linear_command_*` records followed by a new `remote_command_acked`. Assert recovery produces one task identity and one acknowledged remote command. Run `direnv exec . gleam test` and expect projection failures before implementation.

23. Update `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/workflow_checkpoint.gleam` to carry task refs through workflow recovery and checkpoint writes. Run `direnv exec . gleam test` and expect mixed-ledger recovery tests to pass.

24. In `test/workspace_run_test.gleam` or the existing environment-variable test file that covers worker launches, add `worker_environment_includes_task_and_issue_aliases_test`. Assert the worker environment contains `SCHERZO_TASK_ID=issue-1`, `SCHERZO_TASK_KEY=LIV-266`, `SCHERZO_ISSUE_ID=issue-1`, and `SCHERZO_ISSUE_IDENTIFIER=LIV-266`. Run `direnv exec . gleam test` and expect missing task env vars before implementation.

25. Update `src/scherzo/workspace_run.gleam`, `src/scherzo/session/name.gleam`, and any direct worker-start environment builder so task env vars are added while old issue env vars remain. Run `direnv exec . gleam test` and expect the env var tests to pass.

26. At Gate B, run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record in Progress which side effects still use the old Linear path. Commit Milestone 3. Suggested commit message: `feat: expose task terminology in workflow runs`.

27. In `test/orchestrator_effect_runner_test.gleam`, add failing tests for generic effects `FetchRemoteCommands`, `PostRemoteCommandAck`, and `ReplayRemoteCommandAck`. The Linear compatibility assertion is that the ack body for comment `comment-1` remains exactly `Retry queued` and the posted target remains task remote ID `issue-1`. Run `direnv exec . gleam test` and expect missing constructors.

28. Update `src/scherzo/orchestrator/effects/types.gleam` and `src/scherzo/orchestrator/transition_types.gleam` to add generic remote command names while keeping old Linear names as decoder/projection compatibility where needed. Run `direnv exec . gleam test` and expect the generic effect tests to pass.

29. In `test/orchestrator_config_test.gleam` and `test/orchestrator_service_doctor_test.gleam`, add failing startup validation tests for each exact `tracker_capability_missing` message listed in Adapter API Contract. Use the fake `test-memory` adapter requirements rather than live Linear credentials. Run `direnv exec . gleam test` and expect validation to be unwired or messages missing.

30. Wire `validate_required_capabilities` into `src/scherzo/runtime_bundle.gleam`, `src/scherzo/orchestrator/service.gleam`, or the existing config validation path so daemon startup and doctor workflow-config checks fail before work starts. Run `direnv exec . gleam test` and expect validation tests to pass.

31. In `test/orchestrator_daemon_linear_command_test.gleam`, add failing `linear_comment_command_flows_through_tracker_adapter_test`. Arrange a Linear command comment `/scherzo retry LIV-266`, assert the same local operator command is applied as before, and assert the same ack body is posted. Run `direnv exec . gleam test` and expect the test to fail until daemon command fetching uses the adapter.

32. Update `src/scherzo/orchestrator/daemon.gleam` to replace `make_linear_commands` with `make_tracker_adapter` remote command capability calls. Keep the Linear adapter delegating to the old command transport. Run `direnv exec . gleam test` and expect Linear command tests to pass.

33. In `test/handoff_test.gleam` and `test/handoff_format_test.gleam`, add failing adapter-level tests for `HandoffClaim`, `HandoffSuccess`, `HandoffFailure`, and `HandoffPark`. Assert the Linear comment text and state transition requests are identical to current behavior. Run `direnv exec . gleam test` and expect failures until handoff uses the adapter capability.

34. Update `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/tracker/linear_adapter.gleam` so claim, success, failure, and park reports use `HandoffCapability`. Run `direnv exec . gleam test` and expect handoff tests to pass.

35. In `test/linear_triage_test.gleam` or `test/orchestrator_daemon_test.gleam`, add a failing test `invalid_workflow_report_uses_adapter_capability_test`. Use an issue with a missing workflow label and assert the same Linear invalid-workflow report body and fingerprint policy as current triage. Run `direnv exec . gleam test` and expect failure until invalid-workflow reporting uses the adapter.

36. Update the invalid-workflow reporting path to call the adapter's comment/routing capability through `src/scherzo/tracker/linear_adapter.gleam`. Run `direnv exec . gleam test` and expect invalid-workflow tests to pass.

37. In `test/scheduled_failure_reporter_test.gleam`, add failing adapter-level tests `scheduled_failure_capability_creates_or_updates_one_task_test` and `scheduled_failure_without_capability_fails_startup_test`. Assert dedupe key `scheduled:nightly` creates one task/card and a second failure updates/comments on it. Run `direnv exec . gleam test` and expect failures until scheduled failure publication uses the adapter.

38. Update `src/scherzo/scheduled_failure_reporter.gleam`, `src/scherzo/tracker/linear_adapter.gleam`, and the daemon startup path so scheduled failure publication goes through `ScheduledFailureCapability`. Run `direnv exec . gleam test` and expect scheduled failure tests to pass.

39. At Gate C, run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Run the coupling inventory command from Validation and Acceptance and record any remaining generic-path Linear imports in Progress. Commit Milestone 4. Suggested commit message: `feat: route orchestrator side effects through tracker capabilities`.

40. In `test/config_test.gleam` and `test/config_types_test.gleam`, add failing tests for the preferred nested tracker shape and old flat aliases. When both shapes specify the same setting, assert the preferred nested field wins and a warning is produced with event `legacy_tracker_field_ignored`, such as `legacy_tracker_field_ignored path=tracker.api_key replacement=tracker.credentials.api_key_env`. Run `direnv exec . gleam test` and expect parser failures or missing warnings.

41. Update `src/scherzo/config/types.gleam` and `src/scherzo/config.gleam` to parse the preferred nested shape, preserve old flat aliases, and apply this precedence: nested `tracker.credentials.api_key_env` wins over flat `tracker.api_key`; nested `tracker.linear.endpoint` wins over flat `tracker.endpoint`; nested `tracker.linear.project_slug` wins over flat `tracker.project_slug`; list fields `active_states`, `dispatch_states`, and `terminal_states` remain at `tracker` and have no nested duplicate in this plan. Run `direnv exec . gleam test` and expect config tests to pass.

42. In `test/doctor_test.gleam` and `test/main_test.gleam`, add failing tests for `tracker-smoke`, `tracker-contract`, `--tracker-smoke`, and `--tracker-contract-check`. Assert old `linear-smoke`, `linear-contract`, `--linear-smoke`, and `--linear-contract-check` still parse to the same service behavior. Run `direnv exec . gleam test` and expect parse failures for the new aliases.

43. Update `src/scherzo/doctor.gleam` and `src/scherzo/main.gleam` so new tracker names are preferred and old Linear names are compatibility aliases. Help text must include `--tracker-smoke`, `--tracker-contract-check`, `--linear-smoke (compatibility alias for --tracker-smoke)`, and `--linear-contract-check (compatibility alias for --tracker-contract-check)`. Run `direnv exec . gleam test` and expect CLI tests to pass.

44. Update docs and prompts listed in Test, Fixture, and Documentation Inventory. Add `docs/runbooks/tracker-adapters.md` with a capability matrix containing rows for Linear, Jira follow-up, Trello follow-up, and test-memory. For Linear, mark task source, comments, remote commands, state transitions, routing metadata, links, handoff, scheduled failures, readiness, smoke, and attachments according to current support. For Jira and Trello follow-ups, mark unknown or future rather than claiming support. Use `task` for backend-neutral behavior and `Linear issue` only for Linear-specific setup.

45. Add `test/tracker_adapter_docs_test.gleam` or extend an existing docs test to assert `docs/runbooks/tracker-adapters.md` mentions `task`, `tracker adapter`, `Linear issue`, `tracker-smoke`, `linear-smoke`, `scheduled_failures`, and `remote_commands`. Run `direnv exec . gleam test` and expect docs tests to pass.

46. Commit Milestone 5 after format and tests pass. Suggested commit message: `docs: describe task-system agnostic tracker architecture`.

47. Run final validation from the repo root:

        direnv exec . gleam format --check src test
        direnv exec . gleam test
        direnv exec . gleam run -m glinter
        direnv exec . gleam run -m scherzo_lint

   Expect format to pass, all tests to pass, and lint gates to report no new production errors. If warnings already exist, do not expand their count.

48. Run the coupling inventory from the repo root:

        grep -R "import scherzo/linear\|import scherzo/handoff\|import scherzo/linear_triage\|import scherzo/linear_contract\|import scherzo/smoke\|import scherzo/scheduled_failure_reporter" src/scherzo/orchestrator src/scherzo/workflow_run.gleam src/scherzo/workflow_checkpoint.gleam src/scherzo/template.gleam || true

   Expected output is empty for generic paths. If any match remains, either move that dependency behind `src/scherzo/tracker/linear_adapter.gleam` or record a precise compatibility rationale in `docs/runbooks/tracker-adapters.md` and this plan's Decision Log before review.

49. Inspect for accidental absolute local paths or compatibility removals before review. Use repository-relative paths in docs and comments. Do not introduce examples containing local absolute path prefixes; use a placeholder such as `<absolute-local-path>` only when discussing forbidden path shapes.

50. Commit Milestone 6. Suggested commit message: `chore: validate task-system adapter migration`.

## Testing and Falsifiability

Use full-suite validation while iterating unless the repository has gained an official narrower Gleam test runner by implementation time. The stable command in this plan is `direnv exec . gleam test`; expected successful output is the Gleam test runner's normal all-pass summary with no failures. If a targeted command exists later, record it in Progress before using it as a milestone gate.

Milestone 0 tests are the red-alert safety net. `flat_linear_tracker_config_aliases_still_parse_test` in `test/config_test.gleam` uses the old flat `tracker` YAML and asserts endpoint, API key source, project slug, active states, dispatch states, and terminal states survive. `issue_template_variables_are_characterized_test` in `test/template_test.gleam` renders `issue.id`, `issue.identifier`, `issue.title`, `issue.branch_name`, and `issue.labels` and asserts the current output. `linear_scheduled_failure_dedupes_one_visible_issue_per_job_test` in `test/scheduled_failure_reporter_test.gleam` asserts two failures for `job_id: nightly` produce one visible Linear issue/card. `linear_command_ack_outbox_survives_recovery_test` in `test/orchestrator_daemon_linear_command_test.gleam` asserts comment `comment-1` receives one ack after recovery.

For the domain model, `test/task_test.gleam` must include `task_display_key_prefers_key_test`, `task_display_key_falls_back_to_remote_id_test`, `issue_to_task_preserves_linear_fields_test`, and `non_linear_task_cannot_convert_to_legacy_issue_test`. The conversion test input is a `tracker_issue.Issue` with id `issue-1`, identifier `LIV-266`, title `Refresh architecture`, description `Some("body")`, priority `Some(2)`, branch name `Some("liv-266-refresh")`, URL `Some("https://linear.app/living-systems/issue/LIV-266")`, labels `workflow:execplan` and `kind:feature`, no blockers, `blocked_by_complete: True`, and no timestamps. It asserts the resulting `task.Task` has `TaskRef("linear", "issue-1", Some("LIV-266"), Some(...))`, `branch_hint: Some("liv-266-refresh")`, and the same labels.

For adapter capabilities, `test/tracker_adapter_test.gleam` must construct a read-only `TrackerAdapter` with only `task_source` and assert each validation scenario returns the exact `tracker_capability_missing` message listed in Adapter API Contract. It must also test comment update fallback: a comment capability that cannot update returns `UnsupportedCapability("comments.update")` when fallback is false, and creates a new comment receipt with `created: True` when fallback is true.

For the Linear adapter, `test/tracker_linear_adapter_test.gleam` preserves existing Linear tests and adds adapter-level tests that exercise the same fake transport. `linear_adapter_fetch_candidates_matches_linear_parser_test` asserts the same candidate response parsed by `test/linear_test.gleam` becomes one `task.Task` with key `LIV-266`. `linear_adapter_posts_comment_with_existing_linear_body_test` asserts the adapter sends the same GraphQL mutation body as the existing Linear comment helper. `linear_adapter_scheduled_failure_preserves_dedupe_marker_test` asserts the reserved labels and body marker from the current scheduled failure reporter remain present.

For the fake adapter, `test/tracker_fake_adapter_test.gleam` must not import `scherzo/linear`. It uses `TaskRef("test-memory", "card-1", Some("CARD-1"), None)`, title `Fake card`, label `workflow:execplan`, a comment body `hello`, a transition to `Done`, and dedupe key `scheduled:nightly`. It asserts candidate fetch returns the fake task, refresh returns updated state, comment post records `hello`, transition records `Done`, and scheduled failure publication is idempotent.

For template compatibility, `test/template_test.gleam` adds `task_template_variables_match_issue_aliases_test`, `task_branch_hint_matches_issue_branch_name_alias_test`, `scheduled_invocation_does_not_require_task_test`, and an unknown-variable negative test. `{{ task.title }}|{{ issue.title }}` must render identical text. `{{ task.branch_hint }}|{{ issue.branch_name }}` must render `liv-266-refresh|liv-266-refresh`. Scheduled invocations must still render scheduled variables without requiring a task, and `{{ task.nope }}` must return the same kind of template render error as other unknown variables.

For DAG and recovery, `test/state_record_test.gleam`, `test/state_recovery_test.gleam`, `test/state_projection_test.gleam`, and `test/recovery_workflow_checkpoint_test.gleam` use the old and new JSON snippets from Durable Recovery Migration Design. They assert old records decode, new records encode task fields, mixed old/new ledgers recover one task ref, old `linear_command_*` records project to generic remote command state, new `remote_command_*` records project to the same state, and old `linear_command_ack` outbox payloads replay through `RemoteCommandCapability.post_ack` without duplicate ack posts.

For orchestrator behavior, `test/orchestrator_daemon_linear_command_test.gleam` adds a Linear preservation test that remote `/scherzo retry LIV-266` command comments still apply the same local operator command and post the same ack body. A fake non-Linear dispatch test should dispatch `CARD-1` through `test/support/fake_tracker_adapter.gleam`, assert a worker starts, assert the worker receives `task.key` and `task.title`, and assert no test setup imports `scherzo/linear`.

For scheduled jobs, `test/workflow_scheduler_test.gleam` or `test/scheduled_failure_reporter_test.gleam` adds a scheduled workflow with no task and asserts scheduled variables still render. `scheduled_failure_capability_creates_or_updates_one_task_test` asserts the scheduled failure capability creates or updates one visible task/card per job. `scheduled_failure_without_capability_fails_startup_test` asserts startup fails with `tracker_capability_missing feature=scheduled_failures capability=scheduled_failures path=scheduled_jobs.<id>.on_failure` before daemon work begins.

For config and CLI rollout, `test/config_test.gleam` includes old flat config, new nested config, and mixed config. Mixed config asserts nested values win and warnings name the ignored legacy paths. `test/doctor_test.gleam` asserts `tracker-smoke` and `tracker-contract` are canonical check names while `linear-smoke` and `linear-contract` still parse. `test/main_test.gleam` asserts `--tracker-smoke` and `--tracker-contract-check` parse, and old `--linear-smoke` and `--linear-contract-check` parse to the same behavior with help text that marks them as compatibility aliases.

This plan is falsified if any of these are true: current Linear tests regress; a fake non-Linear adapter cannot dispatch a workflow without Linear command/triage/handoff dependencies; old recovery records cannot decode; mixed old/new ledgers duplicate remote command acknowledgements; `issue.*` prompt variables stop working; unsupported capability combinations are accepted at startup and fail later at runtime; config alias precedence silently picks old flat values over new nested values; or the final coupling inventory shows generic orchestrator side-effect paths still importing Linear-specific modules without a recorded compatibility rationale.

## Validation and Acceptance

Run these commands from the repo root at the end of implementation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Acceptance requires all commands to pass. The test output should report all tests passing with no failures. The lint commands should not report new production errors.

Run this coupling inventory from the repo root:

    grep -R "import scherzo/linear\|import scherzo/handoff\|import scherzo/linear_triage\|import scherzo/linear_contract\|import scherzo/smoke\|import scherzo/scheduled_failure_reporter" src/scherzo/orchestrator src/scherzo/workflow_run.gleam src/scherzo/workflow_checkpoint.gleam src/scherzo/template.gleam || true

Acceptance requires empty output for generic paths, or a Decision Log entry and `docs/runbooks/tracker-adapters.md` note explaining any remaining compatibility import. Do not accept unexplained imports in `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_checkpoint.gleam`, or `src/scherzo/template.gleam`.

Behavioral acceptance requires these observable outcomes: existing Linear candidate polling, handoff, command acknowledgements, contract checks, scheduled failure reporting, and CLI aliases still work in tests; new `task.*` prompt variables render the same values as the compatibility `issue.*` variables; a fake non-Linear adapter can drive a workflow dispatch test without direct Linear dependencies; legacy issue/Linear ledger fixtures still recover; mixed old/new ledger fixtures recover without duplicate acknowledgements; invalid unsupported backend capability combinations fail at startup with the exact `tracker_capability_missing` messages; and `docs/runbooks/tracker-adapters.md` contains the adapter capability matrix for Linear, Jira follow-up, Trello follow-up, and test-memory.

Manual operator-facing examples should be checked with repository-relative paths only. For an old config shape, `direnv exec . gleam run -- doctor --check linear-smoke examples/scherzo.yaml` should still select the Linear smoke behavior. For the new alias, `direnv exec . gleam run -- doctor --check tracker-smoke examples/scherzo.yaml` should select the same behavior. If the example requires a real Linear key, use existing repository conventions for dummy-key workflow-config tests and do not add live-backend tests to the default suite.

## Rollout, Recovery, and Idempotence

The rollout is additive. Keep Linear as the default production backend. Add new task-domain and adapter modules before replacing old call sites. Keep old config fields, old `issue.*` templates, old environment variables, old CLI flags, and old durable record decoders throughout this plan.

Recovery must be backward compatible. Old ledger and checkpoint records that name issues or Linear commands must continue to decode. New workflow records may include task fields, and new remote command records may use remote-command terminology, but the recovery projection must treat old and new forms as equivalent for Linear. If a milestone breaks recovery tests, stop and fix compatibility before continuing.

Rollback is straightforward at milestone boundaries because each milestone commits only after tests pass. If a later adapter-routing milestone causes instability, revert that milestone while keeping earlier pure domain and characterization tests. Since Linear modules remain intact, reverting the adapter call-site migration should restore the previous runtime path.

Repeated validation is safe. Running `gleam test`, lint, format, docs tests, and the coupling inventory does not mutate tracked source. Test fixtures should write under repository-relative `test/tmp` or test-controlled temporary directories only.

Config rollout is alias-based and deterministic. The preferred nested shape is:

    tracker:
      kind: linear
      credentials:
        api_key_env: LINEAR_API_KEY
      linear:
        endpoint: https://api.linear.app/graphql
        project_slug: example-project
      active_states: [Todo, In Progress]
      dispatch_states: [Todo]
      terminal_states: [Done, Canceled]

The old flat Linear shape remains valid for at least one migration window:

    tracker:
      kind: linear
      endpoint: https://api.linear.app/graphql
      api_key: ${LINEAR_API_KEY}
      project_slug: example-project
      active_states: [Todo, In Progress]
      dispatch_states: [Todo]
      terminal_states: [Done, Canceled]

When both shapes are present, nested values win and startup emits a warning for each ignored flat duplicate using event `legacy_tracker_field_ignored`. This is a warning, not a startup failure. Startup fails only for malformed config, missing required effective values, or unsupported capability combinations.

CLI rollout is also alias-based. New help text should prefer `--tracker-smoke`, `--tracker-contract-check`, `doctor --check tracker-smoke`, and `doctor --check tracker-contract`. Old `--linear-smoke`, `--linear-contract-check`, `doctor --check linear-smoke`, and `doctor --check linear-contract` remain compatibility aliases and must map to identical behavior for the Linear adapter.

## Artifacts and Notes

Current facts inspected for this plan include the tracker modules, config types, orchestrator daemon dependencies, orchestrator effect names, DAG parser, workflow run context, workflow checkpoint writer, template variables, scheduled failure reporter, source-control status, and the LIV-263 research summary. The important repository-relative paths are named in the Context, Preconditions, and Test Inventory sections.

The adapter-readiness artifact produced by implementation is `docs/runbooks/tracker-adapters.md`. It should contain a short plain-language definition of task, tracker adapter, and capability; a matrix for Linear, Jira follow-up, Trello follow-up, and test-memory; a list of remaining Linear compatibility aliases; and a section that says which production files may still import Linear-specific modules and why.

Do not treat the old issue-shaped terms in durable records as proof that the architecture failed. In this plan they are compatibility fields. The architecture outcome is that new behavior enters through `task.Task` and `tracker/adapter.TrackerAdapter`, unsupported capabilities fail at startup, and future production backends can implement the contract without changing the generic orchestrator side-effect path.

## Interfaces and Dependencies

Add `src/scherzo/task.gleam`, `src/scherzo/tracker/adapter.gleam`, and `src/scherzo/tracker/linear_adapter.gleam`. Add no new package dependency unless implementation discovers an unavoidable need and records the reason in Decision Log before adding it. Use existing Gleam standard library modules and existing repository modules for config parsing, logging, result handling, time, state encoding, doctor reports, and test fake transports.

`src/scherzo/tracker/linear_adapter.gleam` depends on existing Linear modules. It may import `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/smoke.gleam`, `src/scherzo/linear_attachment.gleam`, and `src/scherzo/scheduled_failure_reporter.gleam`. Generic orchestrator modules should depend on `src/scherzo/tracker/adapter.gleam` instead of those Linear modules after Milestone 4.

Use `branch_hint` instead of `branch_name` in new code because not every backend has a first-class branch field. Keep `issue.branch_name` as a template alias for compatibility.

The old flat Linear config fields under `tracker` must still parse for at least one migration window. New generic CLI modes should be added only as aliases around existing behavior at first. Keep old Linear flags working and mark them as compatibility aliases in help text, not as removed commands.

## Open Questions and Clarifications Needed

- [CLARIFY] Decide the support window for legacy public names after this plan lands. This plan keeps `issue.*`, old env vars, old CLI flags, and old durable record decoders, but it does not decide when or whether they should be removed.
- [CLARIFY] Decide the first production non-Linear backend after the adapter boundary exists. Jira and Trello have different capability gaps, so the next plan should choose one instead of attempting both.
- [CLARIFY] Decide whether backend-neutral scheduled failure reports should be required to create a visible task/card, or whether some adapters may report failures only through comments, logs, or local artifacts.
