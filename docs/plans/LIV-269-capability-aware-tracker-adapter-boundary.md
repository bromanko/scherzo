# Define the capability-aware tracker adapter boundary

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators configure the daemon to find tracker work, dispatch workflows, accept remote operator commands, report handoff status, and run doctor or smoke checks. Today those behaviors are wired through several Linear-specific clients rather than one declared adapter boundary. That makes it hard to add another tracker or to disable a Linear API surface safely, because a configuration can appear valid and then fail later when the daemon tries to read comments, move an issue, or post an acknowledgement.

After this plan is implemented, every tracker adapter will declare one mandatory task source capability and a precise set of optional capabilities. Startup validation will compare the operator's configuration with that declaration before the daemon starts. An impossible configuration, such as remote commands enabled for an adapter that cannot read comment or event activity, will fail fast with an actionable config error instead of producing runtime surprises.

## Problem Framing and Constraints

The user-facing problem is operational safety. A Scherzo deployment should not start in a configuration that it cannot honor. If an operator enables remote commands, Scherzo must know that the selected tracker adapter can supply remote command events and acknowledgements. If handoff or scheduled failure reporting is enabled, Scherzo must know that the adapter can perform the required tracker-side reporting. If doctor, contract, or smoke commands are run, Scherzo must know whether the adapter supports those checks before making network calls.

The implementation must be proportionate. This is not a plan to add a second tracker implementation. It is a plan to define the adapter boundary, move existing Linear behavior behind that boundary, and make config validation capability-aware. Existing Linear behavior should remain the parity baseline. Existing configuration keys should keep working during the migration, especially `linear_commands`, `linear_contract`, `handoff`, and scheduled failure settings. Adding generic public configuration names is out of scope for this plan; removal of legacy names is also out of scope.

The repository is a Gleam project. Production code lives under `src/`, tests live under `test/`, and the required production lint gates are `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint`. Production code must not add `let assert`, `panic`, or `todo`.

## Strategy Overview

Introduce a small set of Gleam records and constructors under `src/scherzo/tracker/`. The central runtime value is a tracker adapter: it always contains a task source and it may contain optional capabilities for comments, remote command events, remote command acknowledgements, state transitions, routing metadata, blockers and links, handoff reports, scheduled failure reports, invalid-workflow triage, contract reads, and smoke reads. A descriptor lists the capability keys without making any network call. Config validation compares the operator's resolved configuration with that descriptor before any daemon actor, worker, poll scheduler, or one-off tracker command starts.

The descriptor and adapter must not drift apart. The adapter should be constructed through a smart constructor in `src/scherzo/tracker/capabilities.gleam`; that constructor derives the descriptor from the actual optional capability fields. A pure `validate_adapter` helper must reject any adapter whose descriptor and fields disagree. The Linear descriptor used for validation and the Linear adapter used at runtime must be produced from the same capability-key helper, and tests must cover intentional mismatches.

Keep tracker-static support declaration separate from config-bound capability construction. The Linear adapter can truthfully declare that Linear supports handoff, invalid-workflow triage, scheduled failure reporting, contract reads, and smoke reads, but building those closures needs the resolved configuration, not only `TrackerConfig`. The adapter factory should therefore expose config-bound functions such as `adapter_for_effective_config(config_types.EffectiveConfig)` and `adapter_for_orchestrator_config(config_types.OrchestratorConfig)`. These functions may construct closures from `effective.tracker`, `effective.handoff`, and `effective.linear_contract`, but they must not perform HTTP requests, start actors, read files, or mutate state.

Use high-level capabilities for the current configured features. `HandoffReportsCapability` encapsulates the comments or state operations needed for handoff. `ScheduledFailureReportsCapability` encapsulates scheduled failure issue creation or update. `InvalidWorkflowTriageCapability` encapsulates the existing invalid-workflow comment and state-move behavior currently implemented by `src/scherzo/linear_triage.gleam`. `ContractReadCapability` and `SmokeReadCapability` cover the Linear contract and smoke doctor paths. Lower-level `IssueCommentsCapability` and `StateTransitionsCapability` still exist so the boundary can represent direct comment or state support, but current config validation should not require both a high-level capability and its lower-level implementation details for the same feature.

The first implementation adapter is Linear. It should wrap the existing modules rather than reimplementing the Linear GraphQL transport. The current task source functions in `src/scherzo/linear.gleam`, the current remote command comment polling in `src/scherzo/linear.gleam` and `src/scherzo/orchestrator/transitions/linear_commands.gleam`, the current invalid-workflow triage client in `src/scherzo/linear_triage.gleam`, the current handoff client in `src/scherzo/handoff.gleam`, the current scheduled failure reporter in `src/scherzo/scheduled_failure_reporter.gleam`, and the current smoke and contract readers should be exposed through the new boundary.

This approach is the right size because it avoids a broad rewrite of tracker behavior. The new layer makes capabilities explicit, then migrates existing call sites one at a time. It also creates a pure validation surface that tests can exercise with fake adapters, which proves the important safety behavior without needing another real tracker. This plan deliberately does not introduce a new public `remote_commands` configuration key or a fully tracker-generic remote contract schema; those are compatibility and product decisions for follow-up work after the adapter boundary is proven.

## Alternatives Considered

The simplest alternative is to add ad hoc checks beside each Linear-specific feature, for example checking `tracker.kind == LinearTracker` when `linear_commands.enabled` is true. That would catch some impossible combinations, but it would keep the real boundary implicit and would require new checks every time a feature is added. It also would not help a future adapter that supports some Linear-like features but not all of them.

Another option is to split the code into one large behavior module per tracker and move all orchestration logic behind that module. That is too large for the current problem. The orchestrator already has useful feature modules and test coverage; replacing them with a monolithic tracker plugin would make parity harder to prove.

The chosen approach uses explicit capability records and descriptors. It is additive, testable with fake adapters, and compatible with the existing function-record style already used in `src/scherzo/tracker.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, and `src/scherzo/scheduled_failure_reporter.gleam`.

## Risks and Countermeasures

The largest implementation risk is import cycles. The new adapter boundary must not import `src/scherzo/linear.gleam`, because the Linear adapter will import the boundary. If existing feature request or outcome types are needed in the boundary, extract type-only modules first, such as `src/scherzo/handoff/types.gleam`, `src/scherzo/scheduled_failure_reporter/types.gleam`, and `src/scherzo/tracker/invalid_workflow.gleam`, and make both the boundary and the feature implementation import those type modules.

A second risk is descriptor drift. If validation reads only `AdapterDescriptor`, but runtime code later sees `None` for the optional capability, Scherzo would recreate the same runtime-surprise failure mode under a formal-looking abstraction. Counter this by using a smart constructor that derives descriptors from adapter fields, by making `TrackerAdapter` opaque outside `src/scherzo/tracker/capabilities.gleam` if practical, by adding `validate_adapter`, and by testing mismatched adapters directly.

A third risk is under-configured adapter construction. Handoff and invalid-workflow triage cannot be built from `TrackerConfig` alone; they need `HandoffConfig` and `LinearContractConfig`. Counter this by making adapter factory build functions take `EffectiveConfig` or `OrchestratorConfig`, while keeping construction pure and network-free.

A fourth risk is creating a vague abstraction that merely renames Linear clients. Counter this by defining concrete records and validation functions, by writing tests that use fake non-Linear adapters, and by keeping the Linear adapter as only the first implementation of those records.

A fifth risk is accidentally weakening startup validation. Counter this with tests for every configured feature-to-capability dependency. The most important negative test is remote commands enabled with no remote command events capability; it must return an `InvalidConfig` before daemon startup, before network calls, and before worker or scheduler startup.

A sixth risk is breaking existing Linear behavior during migration. Counter this by introducing the adapter wrapper first, keeping existing modules callable during each sub-migration, and adding parity tests that compare old Linear clients and new Linear adapter capabilities using fake transports where possible. Invalid-workflow triage needs explicit parity tests for comment-only, state-only, and comment-and-state configurations.

A seventh risk is ambiguous blocker behavior for adapters that cannot provide complete blocker or link metadata. The current dispatcher already treats `blocked_by_complete: False` as blocked and incomplete rather than safe to dispatch. This plan preserves that conservative behavior: an adapter that lacks complete blocker metadata must set `blocked_by_complete: False` on fetched issues, and this plan does not add an operator opt-in to dispatch with incomplete blocker data.

## Progress

- [x] (2026-05-13 00:00Z) Read the repo-local ExecPlan authoring skill and drafted this checked-in plan artifact.
- [x] (2026-05-13 00:00Z) Inspected the current tracker, Linear, config, daemon dependency, handoff, scheduled failure, command, and smoke surfaces needed to make this plan concrete.
- [x] (2026-05-13 00:00Z) Incorporated review feedback about config-bound adapter construction, descriptor/adapter invariants, invalid-workflow triage, narrower config scope, Milestone 4 migration slices, and rollback guidance.
- [ ] Implement Milestone 1: add capability records, descriptors, adapter invariants, and pure validation tests.
- [ ] Implement Milestone 2: wrap existing Linear behavior in the adapter boundary without changing daemon behavior.
- [ ] Implement Milestone 3: run capability validation before daemon, run-once, doctor, contract, smoke, and remote-command startup paths.
- [ ] Implement Milestone 4a: migrate task-source reads in daemon and run-once service paths to the tracker adapter.
- [ ] Implement Milestone 4b: migrate remote command fetch and acknowledgement effects to the tracker adapter.
- [ ] Implement Milestone 4c: migrate handoff and scheduled failure effects to the tracker adapter.
- [ ] Implement Milestone 4d: migrate invalid-workflow triage effects to the tracker adapter.
- [ ] Implement Milestone 4e: migrate contract, smoke, and doctor service checks to the tracker adapter.
- [ ] Implement Milestone 5: finish migration notes, parity tests, and lint validation.

## Surprises & Discoveries

- Observation: Scherzo already uses function records for tracker-like dependencies, so a capability boundary can follow an existing style rather than introducing a new object model.
  Evidence: `src/scherzo/tracker.gleam` defines `tracker.Client` as a record of fetch functions; `src/scherzo/linear.gleam` defines separate `CommandClient` and `ContractClient`; `src/scherzo/handoff.gleam` and `src/scherzo/scheduled_failure_reporter.gleam` define feature clients as records of functions.

- Observation: Linear-specific optional features are currently wired separately in daemon and service dependency records.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` has `make_tracker`, `make_handoff`, `make_linear_commands`, `make_triage`, and `make_scheduled_failure_reporter`; `src/scherzo/orchestrator/service.gleam` has default doctor dependencies for Linear smoke and Linear contract checks.

## Decision Log

- Decision: Use Gleam records with function fields for capabilities.
  Rationale: This matches the current `tracker.Client`, `linear.CommandClient`, `linear.ContractClient`, `handoff.Client`, `linear_triage.TriageClient`, and `scheduled_failure_reporter.Client` style and keeps the change additive.
  Date: 2026-05-13.

- Decision: Make task source a mandatory field on the adapter record and also a mandatory descriptor key.
  Rationale: Every Scherzo daemon needs a way to discover work. Encoding the field as mandatory makes missing task source impossible in normal construction, while validating the descriptor catches malformed factories in tests.
  Date: 2026-05-13.

- Decision: Validate capabilities from a pure descriptor before daemon actor startup and before one-off commands that require optional capabilities.
  Rationale: The operator should see configuration errors before network clients run, before workers start, and before a background actor enters a partially usable state.
  Date: 2026-05-13.

- Decision: Build runtime adapters from `EffectiveConfig` or `OrchestratorConfig`, not from `TrackerConfig` alone.
  Rationale: Handoff reporting needs `HandoffConfig`, invalid-workflow triage needs `LinearContractConfig`, and future config-bound capabilities should not smuggle missing configuration through globals or partial closures.
  Date: 2026-05-13.

- Decision: Tie descriptors to adapter fields with a smart constructor and `validate_adapter`.
  Rationale: The safety claim fails if a descriptor says a capability exists while the runtime adapter stores `None`. Deriving the descriptor from fields and testing mismatches prevents that class of bug.
  Date: 2026-05-13.

- Decision: Add `InvalidWorkflowTriageCapability` as a high-level capability and migrate `make_triage` to it.
  Rationale: The current invalid-workflow path is not just generic comments or state moves; it combines `LinearContractConfig`, violation rendering, comment posting, optional state transitions, and outcome reporting. A high-level capability preserves parity without forcing each call site to reconstruct that policy.
  Date: 2026-05-13.

- Decision: Do not introduce a new public `remote_commands` config key in this plan.
  Rationale: The boundary can become capability-aware while keeping `linear_commands` as the only public configuration surface. Renaming or aliasing config keys is a separate compatibility decision and would widen this plan.
  Date: 2026-05-13.

- Decision: Keep the current `linear_contract.RemoteBoard` shape behind `ContractReadCapability` for this plan.
  Rationale: The immediate safety goal is capability validation, not a cross-tracker contract schema. If a cycle appears, extract the existing remote contract types to a type-only module, but do not redesign the data model here.
  Date: 2026-05-13.

- Decision: Preserve conservative blocker behavior for adapters without complete blocker metadata.
  Rationale: Current dispatch treats `blocked_by_complete: False` as incomplete and blocked. Future adapters can remain safe by setting that field to `False` rather than by requiring a new operator opt-in in this plan.
  Date: 2026-05-13.

## Outcomes & Retrospective

To be filled at major milestones and at completion. The expected outcome is a daemon startup path that rejects impossible tracker feature combinations before start, plus a Linear adapter that preserves current behavior through the new capability boundary.

## Context and Orientation

A tracker is the external task system Scherzo polls for work. In the current repository the only tracker kind is Linear, represented by `LinearTracker` in `src/scherzo/tracker/kind.gleam`. A task source is the minimum tracker capability Scherzo needs: it fetches candidate issues, fetches issues by state, and refreshes issue states by id. Optional capabilities are tracker operations that not every adapter can provide, such as reading comments, posting comments, moving issues between states, reporting handoff, or running smoke checks.

The current generic task source lives in `src/scherzo/tracker.gleam` as `pub type Client`. It has these functions:

    fetch_candidate_issues: fn() -> Result(List(tracker_issue.Issue), error.TrackerError)
    fetch_issues_by_states: fn(List(issue_state.IssueState)) -> Result(List(tracker_issue.Issue), error.TrackerError)
    fetch_issue_states_by_ids: fn(List(String)) -> Result(List(tracker_issue.Issue), error.TrackerError)

The issue record lives in `src/scherzo/tracker/issue.gleam`. It already carries fields that some adapters may not fully support, including labels, blockers, branch names, urls, and timestamps. This plan keeps the record but makes capability declarations say which fields are complete and reliable. The dispatcher in `src/scherzo/orchestrator/core.gleam` already treats `blocked_by_complete: False` as incomplete and not safe for dispatch.

Linear API functions live in `src/scherzo/linear.gleam`. That module currently builds `tracker.Client` for task source reads, `linear.CommandClient` for comment-backed remote commands, and `linear.ContractClient` for remote contract reads. Invalid-workflow triage lives in `src/scherzo/linear_triage.gleam`; it can post a comment, move an issue to an invalid-workflow state, do both, or no-op depending on `LinearContractConfig`. Handoff reporting lives in `src/scherzo/handoff.gleam`. Scheduled failure issue creation and update lives in `src/scherzo/scheduled_failure_reporter.gleam`. The Linear read smoke check lives in `src/scherzo/smoke.gleam`.

Configuration types live in `src/scherzo/config/types.gleam`. Existing relevant records include `TrackerConfig`, `HandoffConfig`, `LinearContractConfig`, `LinearCommandConfig`, `RoutingConfig`, and scheduled job failure config types. Configuration resolution and validation live in `src/scherzo/config.gleam`; that file already rejects invalid local combinations such as an empty `linear_commands.prefix`, non-positive command poll limits, and `handoff.attach_result_on_success` without `handoff.comment_on_success`.

The daemon runtime dependency boundary is currently in `src/scherzo/orchestrator/daemon.gleam`. Its `RuntimeDependencies` record has separate Linear-oriented factories for task source, handoff, Linear commands, invalid-workflow triage, and scheduled failure reporting. The effect values that carry these clients are in `src/scherzo/orchestrator/effect_runner.gleam`. The transition interpreter shell callbacks that enqueue those effects are in `src/scherzo/orchestrator/effects/interpreter.gleam`. The service-level doctor, run-once, smoke, and contract dependency boundary is in `src/scherzo/orchestrator/service.gleam`.

## Preconditions and Verified Facts

The implementation should start from a clean working tree. Use this command from the repository root to inspect source control state:

    $SCHERZO_WORKSPACE_DRIVER status --human

The following facts were verified against the current tree while authoring and revising this plan:

`src/scherzo/tracker.gleam` defines the current mandatory task-source shape as `tracker.Client`.

`src/scherzo/tracker/issue.gleam` defines `Issue` with `labels`, `blocked_by`, and `blocked_by_complete`, which are the fields most relevant to routing metadata and blockers or links.

`src/scherzo/tracker/state.gleam` defines normalized issue state helpers and should continue to be used in new signatures rather than introducing raw state strings except for external tracker ids.

`src/scherzo/tracker/kind.gleam` currently has only `LinearTracker` and `from_string("linear")`.

`src/scherzo/linear.gleam` constructs `tracker.Client`, `CommandClient`, and `ContractClient` from a `TrackerConfig` and a `Transport`.

`src/scherzo/linear_triage.gleam` constructs `TriageClient` from `TrackerConfig`, `LinearContractConfig`, and `linear.Transport`; its `report_invalid_workflow` behavior returns `InvalidWorkflowReportNoop`, `InvalidWorkflowReportComment`, `InvalidWorkflowReportState`, or `InvalidWorkflowReportCommentAndState`.

`src/scherzo/config/types.gleam` already contains the config records that capability validation must inspect, including scheduled job failure config and `LinearCommandConfig`.

`src/scherzo/config.gleam` already resolves `handoff`, `linear_contract`, and `linear_commands` before producing `EffectiveConfig` and `OrchestratorConfig`.

`src/scherzo/orchestrator/daemon.gleam` currently stores separate tracker, handoff, Linear command, triage, and scheduled failure clients in daemon state.

`src/scherzo/orchestrator/effect_runner.gleam` currently carries `tracker.Client`, `linear.CommandClient`, `handoff.Client`, `linear_triage.TriageClient`, and `scheduled_failure_reporter.Client` inside effect constructors.

`src/scherzo/orchestrator/service.gleam` currently wires Linear smoke and Linear contract clients for doctor and one-off commands, and it uses `tracker.Client` directly in run-once dispatch paths.

`src/scherzo/orchestrator/core.gleam` treats `blocked_by_complete: False` as an incomplete blocker decision that prevents dispatch rather than as proof that an issue is unblocked.

`src/scherzo/handoff.gleam`, `src/scherzo/scheduled_failure_reporter.gleam`, `src/scherzo/orchestrator/transitions/linear_commands.gleam`, `src/scherzo/linear_triage.gleam`, and `src/scherzo/smoke.gleam` are the feature modules the adapter migration must preserve.

## Scope Boundaries

In scope: define the capability records, descriptor records, adapter smart constructor, adapter factory functions, config validation functions, Linear adapter wrapper, tests, and migration of current daemon and service wiring to the adapter boundary.

In scope: support a mandatory task source capability and optional comments, remote command events, remote command acknowledgements, state transitions, routing metadata, blockers and links, handoff, scheduled failure, invalid-workflow triage, contract read, and smoke read capabilities.

In scope: reject impossible combinations before daemon start and before relevant one-off commands. The plan must cover at least remote commands enabled without remote command events, command acknowledgements enabled without acknowledgement support, handoff enabled without handoff reporting support, scheduled failure reporting enabled without scheduled failure support, exact workflow label routing without routing metadata support, contract checks without contract read support, invalid-workflow comment or state reporting without invalid-workflow triage support, and smoke commands without smoke read support.

In scope: preserve current conservative blocker behavior. Adapters that cannot provide complete blockers and links must set `blocked_by_complete: False`; dispatch will skip or defer those issues through the existing blocker-decision path. This plan does not add a configuration flag that allows dispatch with incomplete blocker data.

Out of scope: adding a second real tracker adapter, changing Linear GraphQL behavior, adding or renaming public configuration keys such as a new `remote_commands` alias, removing legacy `linear_*` configuration keys, redesigning workflow routing semantics, changing how pi workers execute, or modifying workspace driver behavior.

Out of scope: making the remote contract response fully tracker-generic. `ContractReadCapability` may expose the current `linear_contract.RemoteBoard` shape or a type-only extraction of that exact shape. Designing a tracker-neutral contract schema is a later plan.

Out of scope for the first implementation: making every existing ledger event name generic. If renaming ledger records such as `LinearCommandSeen` is desirable, keep that as a later compatibility migration unless a call site must change to support capability validation.

## Milestones

Milestone 1 creates the type model, descriptor/adapter invariant, and pure validation surface. At the end of this milestone, tests can construct fake adapter descriptors and fake adapters to prove that configuration requires the right capabilities and that descriptors cannot safely claim missing runtime fields. No production call site needs to use the adapter yet.

Milestone 2 creates the config-bound Linear adapter wrapper. At the end of this milestone, a single Linear adapter can expose the same behavior currently provided by `linear.real_client`, `linear.real_command_client`, `linear.real_contract_client`, `linear_triage.real_triage_client`, `handoff.linear_client`, `scheduled_failure_reporter.real_client`, and `smoke.real_linear_reader`. Existing daemon wiring may still call the old factories, but parity tests prove the wrapper can replace them and construction performs no network I/O.

Milestone 3 installs capability validation before startup. At the end of this milestone, daemon startup, run-once startup, and relevant one-off service commands build a pure adapter, validate the adapter invariant, and validate the selected context against the adapter descriptor before actors, workers, poll schedulers, or tracker network calls start.

Milestone 4 is split into file-specific migration slices so each path can be tested independently. Milestone 4a migrates task-source reads in `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, and the run-once paths in `src/scherzo/orchestrator/service.gleam`. Milestone 4b migrates remote command fetch and acknowledgement effects. Milestone 4c migrates handoff and scheduled failure effects. Milestone 4d migrates invalid-workflow triage. Milestone 4e migrates contract, smoke, and doctor service checks. At the end of each sub-milestone, the old Linear-specific field for that slice is removed or left unused with tests proving parity.

Milestone 5 completes migration polish. At the end of this milestone, no unnecessary legacy factories remain in daemon or service dependencies, no new public config alias has been introduced, all validation and parity tests pass, formatting and lints pass, and this ExecPlan has an updated retrospective.

## Plan of Work

Add `src/scherzo/tracker/capabilities.gleam`. This module should define capability keys, descriptors, an adapter smart constructor, accessors, `supports`, `supported_capabilities`, and `validate_adapter`. Keep it free of `scherzo/linear` imports. Prefer making `TrackerAdapter` opaque outside this module so production code must use accessors instead of constructing mismatched records directly.

The adapter constructor should take the actual optional capability fields and derive its descriptor from them. If opaque construction is too disruptive, keep the public constructor temporarily but still add `validate_adapter` and call it from every adapter factory. The descriptor must include `RemoteCommandEventsCapability` only when remote command events exist, and it must include `RemoteCommandAcknowledgementsCapability` only when an acknowledgement function exists.

Add type-only modules where needed. Move `ParkReport` from `src/scherzo/handoff.gleam` into `src/scherzo/handoff/types.gleam` and re-export or import it from the old module to keep call sites small. Move `FailureReportRequest`, `FailureReportOutcome`, and `ExistingFailureIssue` from `src/scherzo/scheduled_failure_reporter.gleam` into `src/scherzo/scheduled_failure_reporter/types.gleam`. Move `InvalidWorkflowReportOutcome` from `src/scherzo/linear_triage.gleam` into `src/scherzo/tracker/invalid_workflow.gleam` so the generic capability boundary and the Linear triage module can share it without importing each other. If `ContractCapability` creates a cycle by returning the current remote contract shape, extract the existing remote contract types from `src/scherzo/linear_contract.gleam` into `src/scherzo/linear_contract/types.gleam` without changing their fields.

Add `src/scherzo/tracker/capability_validation.gleam`. This module should convert config records into a list of capability requirements and compare those requirements with an adapter descriptor. It should return `Result(Nil, error.ConfigError)`, using `error.InvalidConfig` for operator-facing failures. It should be pure: no environment reads, YAML parsing, actor startup, file I/O, or HTTP transport calls.

Add `src/scherzo/tracker/linear_adapter.gleam`. This module should build a config-bound adapter for `LinearTracker` by wrapping existing Linear, handoff, triage, scheduled failure, contract, and smoke modules. The primary function should take `config_types.EffectiveConfig`; a convenience function may take `config_types.OrchestratorConfig` and use its `effective` field. Add a test helper that also accepts a fake `linear.Transport`. The wrapper must not perform network calls during descriptor or adapter construction; it should only create closures.

Add `src/scherzo/tracker/adapter_factory.gleam`. It should expose `adapter_for_effective_config(config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)`, `adapter_for_orchestrator_config(config_types.OrchestratorConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)`, and, if useful for diagnostics, `descriptor_for_effective_config(config_types.EffectiveConfig) -> Result(capabilities.AdapterDescriptor, error.ConfigError)`. For now it should support `LinearTracker` only and return a config error for any unknown kind if new kinds are later added. Every adapter-returning function must call `capabilities.validate_adapter` before returning `Ok(adapter)`.

Do not add a generic `RemoteCommandConfig` or parse a `remote_commands` key in this plan. Preserve `LinearCommandConfig` and the existing `linear_commands` behavior. Capability validation should mention `linear_commands.enabled`, `linear_commands.acknowledge_success`, and `linear_commands.acknowledge_rejection` in errors because those are the operator-facing keys for this implementation.

Update the daemon startup path. In the runtime bundle or service startup code that resolves `OrchestratorConfig`, build the selected adapter with `adapter_factory.adapter_for_orchestrator_config`, validate the adapter, and run `capability_validation.validate_orchestrator_config` before calling daemon start. If the exact loading function has drifted, find the call to `config.resolve_orchestrator_root` or the code that obtains `runtime_bundle.RuntimeBundle`, and place validation immediately after successful config resolution and before any daemon actor, worker, poll scheduler, or tracker network setup.

Update one-off service commands. `start_linear_smoke` should become a generic smoke path internally, even if the public command name remains for compatibility. It should validate `SmokeCommand` before calling the smoke reader. `start_linear_contract_check` should validate `ContractCheckCommand` before fetching the remote contract. Doctor checks in `src/scherzo/orchestrator/service.gleam` should map selected Linear contract and Linear smoke checks to `ContractReadCapability` and `SmokeReadCapability`; local-only doctor checks do not need a tracker capability.

Update `src/scherzo/orchestrator/daemon.gleam` in slices. Replace `make_tracker`, `make_handoff`, `make_linear_commands`, `make_triage`, and `make_scheduled_failure_reporter` with `make_tracker_adapter: fn(config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)` only after the relevant effect paths are migrated. The daemon state should store `tracker_adapter: capabilities.TrackerAdapter` and obtain feature clients through accessors that return a typed error rather than unwrapping `Option` unsafely. Do not use `let assert`, `panic`, or unchecked option unwraps in production code.

Update `src/scherzo/orchestrator/effect_runner.gleam` alongside each daemon slice so effect constructors carry capability records rather than old Linear-specific clients. Keep existing effect and ledger names where renaming would widen the migration; for example, it is acceptable for a `PostLinearCommandAck` effect to remain named that way while its client field becomes a remote-command acknowledgement capability.

Update transition and effect code that currently names Linear command clients. Keep the current ledger event names if changing them would widen the migration. Convert the adapter's `RemoteCommandsCapability` output into the existing command transition functions, or move parsing into the Linear adapter and pass generic `RemoteCommandEvent` values downstream.

Update tests after each production edit. Prefer focused tests for the pure validator first, then adapter invariant and Linear adapter parity tests, then startup tests that prove validation happens before daemon actor startup, then per-slice migration tests.

## Concrete Steps

1. From the repository root, run source-control inspection:

       $SCHERZO_WORKSPACE_DRIVER status --human

   Expect a clean working copy or only this plan file if implementation begins from this ExecPlan branch.

2. Create `src/scherzo/tracker/capabilities.gleam` with the capability key and descriptor types. The key type should include these constructors:

       pub type CapabilityKey {
         TaskSourceCapability
         IssueCommentsCapability
         RemoteCommandEventsCapability
         RemoteCommandAcknowledgementsCapability
         StateTransitionsCapability
         RoutingMetadataCapability
         BlockersLinksCapability
         HandoffReportsCapability
         ScheduledFailureReportsCapability
         InvalidWorkflowTriageCapability
         ContractReadCapability
         SmokeReadCapability
       }

   Add `pub type AdapterDescriptor { AdapterDescriptor(kind: tracker_kind.TrackerKind, capabilities: List(CapabilityKey), name: String) }`. Add `pub fn supports(descriptor: AdapterDescriptor, capability: CapabilityKey) -> Bool` and `pub fn supported_capabilities(descriptor: AdapterDescriptor) -> List(CapabilityKey)`.

3. In the same file, define adapter construction so descriptor keys are derived from the actual fields. Prefer this shape, adjusted only for Gleam syntax or import-cycle discoveries:

       pub type AdapterParts {
         AdapterParts(
           kind: tracker_kind.TrackerKind,
           name: String,
           task_source: TaskSourceCapability,
           comments: Option(CommentsCapability),
           remote_commands: Option(RemoteCommandsCapability),
           state_transitions: Option(StateTransitionsCapability),
           routing_metadata: Option(RoutingMetadataCapability),
           blockers_links: Option(BlockersLinksCapability),
           handoff: Option(HandoffCapability),
           scheduled_failure: Option(ScheduledFailureCapability),
           invalid_workflow_triage: Option(InvalidWorkflowTriageCapability),
           contract: Option(ContractCapability),
           smoke: Option(SmokeCapability),
         )
       }

       pub opaque type TrackerAdapter

       pub fn new(parts: AdapterParts) -> TrackerAdapter
       pub fn descriptor(adapter: TrackerAdapter) -> AdapterDescriptor
       pub fn task_source(adapter: TrackerAdapter) -> TaskSourceCapability
       pub fn remote_commands(adapter: TrackerAdapter) -> Option(RemoteCommandsCapability)
       pub fn invalid_workflow_triage(adapter: TrackerAdapter) -> Option(InvalidWorkflowTriageCapability)
       pub fn validate_adapter(adapter: TrackerAdapter) -> Result(Nil, error.ConfigError)

   If `pub opaque type TrackerAdapter` blocks too much incremental migration, use a public record temporarily but keep `new`, accessors, and `validate_adapter`, and migrate call sites to the accessors before Milestone 5.

4. Define the concrete capability records in `src/scherzo/tracker/capabilities.gleam`. Use the existing `tracker_issue.Issue`, `issue_state.IssueState`, and `error.TrackerError` types. The first version should include:

       pub type TaskSourceCapability {
         TaskSourceCapability(
           fetch_candidate_issues: fn() -> Result(List(tracker_issue.Issue), error.TrackerError),
           fetch_issues_by_states: fn(List(issue_state.IssueState)) -> Result(List(tracker_issue.Issue), error.TrackerError),
           fetch_issue_states_by_ids: fn(List(String)) -> Result(List(tracker_issue.Issue), error.TrackerError),
         )
       }

       pub type RemoteCommandsCapability {
         RemoteCommandsCapability(
           fetch_pending: fn(List(String), Int) -> Result(List(RemoteCommandEvent), error.TrackerError),
           acknowledge: Option(fn(CommandAcknowledgement) -> Result(Nil, error.TrackerError)),
         )
       }

       pub type InvalidWorkflowTriageCapability {
         InvalidWorkflowTriageCapability(
           report_invalid_workflow: fn(tracker_issue.Issue, workflow_policy.IssueWorkflowViolation) -> Result(invalid_workflow.InvalidWorkflowReportOutcome, error.TrackerError),
         )
       }

   Also define `CommentsCapability`, `StateTransitionsCapability`, `RoutingMetadataCapability`, `BlockersLinksCapability`, `HandoffCapability`, `ScheduledFailureCapability`, `ContractCapability`, and `SmokeCapability`. Define `IssueComment`, `IssueCommentAuthor`, `RemoteCommandEvent`, and `CommandAcknowledgement` in this module unless an existing type is already generic enough. `RemoteCommandEvent` should include the tracker event id, issue id, actor id, parsed `command.OperatorCommand`, command name, safe excerpt, and timestamp in milliseconds.

5. Implement descriptor derivation and adapter invariant checks. `validate_adapter` must reject any mismatch between descriptor keys and fields, including a descriptor with no `TaskSourceCapability`, a descriptor with `RemoteCommandEventsCapability` but no `remote_commands`, and a descriptor with `RemoteCommandAcknowledgementsCapability` but `remote_commands.acknowledge == None`. The error message for missing task source should be exactly:

       tracker adapter <name> must declare task source capability

6. Add type extraction commits before importing feature request types into the boundary. If `HandoffCapability` needs `ParkReport`, move that type to `src/scherzo/handoff/types.gleam` and update `src/scherzo/handoff.gleam` to import it. If `ScheduledFailureCapability` needs scheduled failure request and outcome types, move those to `src/scherzo/scheduled_failure_reporter/types.gleam` and update `src/scherzo/scheduled_failure_reporter.gleam` to import them. Move `InvalidWorkflowReportOutcome` to `src/scherzo/tracker/invalid_workflow.gleam` and update `src/scherzo/linear_triage.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/orchestrator/effect_runner.gleam` imports accordingly.

7. Add `src/scherzo/tracker/capability_validation.gleam`. Define:

       pub type StartupContext {
         DaemonStartup
         RunOnceStartup
         RemoteCommandPolling
         DoctorCommand(checks: List(doctor.CheckName))
         ContractCheckCommand
         SmokeCommand
       }

       pub type CapabilityRequirement {
         CapabilityRequirement(
           capability: capabilities.CapabilityKey,
           config_path: String,
           reason: String,
         )
       }

       pub fn validate_descriptor(descriptor: capabilities.AdapterDescriptor) -> Result(Nil, error.ConfigError)
       pub fn requirements_for_effective_config(config: config_types.EffectiveConfig, context: StartupContext) -> List(CapabilityRequirement)
       pub fn requirements_for_orchestrator_config(config: config_types.OrchestratorConfig, context: StartupContext) -> List(CapabilityRequirement)
       pub fn validate_effective_config(config: config_types.EffectiveConfig, descriptor: capabilities.AdapterDescriptor, context: StartupContext) -> Result(Nil, error.ConfigError)
       pub fn validate_orchestrator_config(config: config_types.OrchestratorConfig, descriptor: capabilities.AdapterDescriptor, context: StartupContext) -> Result(Nil, error.ConfigError)

8. Implement descriptor validation. `validate_descriptor` must reject a descriptor that does not include `TaskSourceCapability`. It must reject `RemoteCommandAcknowledgementsCapability` without `RemoteCommandEventsCapability`. It should deduplicate duplicate capability keys before checking or treat duplicates as harmless.

9. Implement configuration requirements using these rules as the first complete set:

   `TaskSourceCapability` is always required for daemon startup and run-once startup.

   `RemoteCommandEventsCapability` is required when `linear_commands.enabled == True`.

   `RemoteCommandAcknowledgementsCapability` is required when remote commands are enabled and either `linear_commands.acknowledge_success == True` or `linear_commands.acknowledge_rejection == True`.

   `HandoffReportsCapability` is required when `handoff.enabled == True`.

   `ScheduledFailureReportsCapability` is required when any scheduled job has tracker issue reporting enabled through the existing scheduled failure config shape in `src/scherzo/config/types.gleam`.

   `ContractReadCapability` is required when `linear_contract.enabled == True`, for the contract check command, and for the Linear contract doctor check.

   `InvalidWorkflowTriageCapability` is required when `linear_contract.comment_on_invalid_workflow == True` or `linear_contract.invalid_workflow_state_id` is present. Do not also require `IssueCommentsCapability` or `StateTransitionsCapability` for this path; the high-level triage capability encapsulates those details for current behavior.

   `RoutingMetadataCapability` is required when `routing.require_exactly_one_workflow_label == True` or when `linear_contract.enforce_issue_workflow_labels == True`.

   `BlockersLinksCapability` is not required for normal dispatch in this plan. Adapters without complete blocker metadata must set `blocked_by_complete: False`, which the existing dispatcher treats conservatively. Add a `BlockersLinksCapability` requirement only if a future explicit config flag demands complete blocker metadata.

   `SmokeReadCapability` is required for the smoke command and for the Linear smoke doctor check.

10. Add `test/tracker_capabilities_test.gleam`. Test `supports` with present and missing keys. Test `validate_descriptor` for a descriptor with no task source, a descriptor with task source only, and a descriptor that has acknowledgements without remote command events. Test adapter invariant behavior by constructing, through a test-only helper if the adapter is opaque, an adapter whose descriptor claims remote command events while the field is missing and asserting `validate_adapter` returns `InvalidConfig`.

11. Add `test/tracker_capability_validation_test.gleam`. Build minimal config records directly rather than relying only on YAML parsing. Add tests with fake descriptors for these cases:

   A task-source-only descriptor is accepted for basic daemon startup when all optional features are disabled.

   Remote commands enabled with no `RemoteCommandEventsCapability` returns `Error(error.InvalidConfig(...))` and the message contains `linear_commands.enabled`.

   Remote command acknowledgements enabled with events but no `RemoteCommandAcknowledgementsCapability` returns `InvalidConfig` and the message contains `acknowledge_success` or `acknowledge_rejection`.

   Handoff enabled without `HandoffReportsCapability` returns `InvalidConfig` and the message contains `handoff.enabled`.

   Scheduled failure reporting enabled without `ScheduledFailureReportsCapability` returns `InvalidConfig` and the message contains the scheduled failure config path.

   Contract enabled without `ContractReadCapability` returns `InvalidConfig` and the message contains `linear_contract.enabled`.

   Invalid-workflow comment or state reporting without `InvalidWorkflowTriageCapability` returns `InvalidConfig` and the message contains `linear_contract.comment_on_invalid_workflow` or `linear_contract.invalid_workflow_state_id`.

   Exact workflow label routing without `RoutingMetadataCapability` returns `InvalidConfig` and the message contains `routing.require_exactly_one_workflow_label`.

   Smoke command validation without `SmokeReadCapability` returns `InvalidConfig` and the message contains `smoke`.

12. Run the focused tests:

       direnv exec . gleam test test/tracker_capabilities_test.gleam test/tracker_capability_validation_test.gleam

   If the test runner does not accept file arguments in this repository, run:

       direnv exec . gleam test

   Expect the new tests to fail before production code exists and pass after Milestone 1 is complete.

13. Commit Milestone 1 after tests pass. Suggested commit message: `tracker: add capability descriptors and validation`.

14. Create `src/scherzo/tracker/linear_adapter.gleam`. Define `pub fn adapter(effective: config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)`, `pub fn adapter_for_orchestrator(config: config_types.OrchestratorConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)`, and a test helper that accepts a fake `linear.Transport`. If a descriptor-only helper is kept, ensure it uses the same capability-key helper as `adapter`.

15. In `linear_adapter.adapter`, build `TaskSourceCapability` from `linear.client(effective.tracker, transport)`. Build comments and remote command capabilities from the existing Linear comment APIs. Build invalid-workflow triage from `linear_triage.triage_client(effective.tracker, effective.linear_contract, transport)`. Build handoff from `handoff.linear_client(effective.tracker, effective.handoff)`, scheduled failure from `scheduled_failure_reporter.real_client(effective.tracker)`, contract from `linear.contract_client(effective.tracker, transport)` or the existing real helper, and smoke from `smoke.real_linear_reader(effective.tracker)` or a transport-injected helper.

16. Add `test/linear_adapter_test.gleam`. Assert that constructing the Linear adapter performs no network request until a capability function is called. Assert that the adapter descriptor supports task source, issue comments, remote command events, remote command acknowledgements, state transitions, routing metadata, blockers and links, handoff reports, scheduled failure reports, invalid-workflow triage, contract read, and smoke read.

17. In `test/linear_adapter_test.gleam`, add parity tests using a fake `linear.Transport`. Prove that calling the adapter's task source produces the same result as calling `linear.client` with the same fake transport. Add a fake comment response and prove the remote command capability emits the same parsed operator command that the current Linear parser would emit. Add invalid-workflow triage parity tests for comment-only, state-only, comment-and-state, and no-op configurations.

18. Commit Milestone 2 after tests pass. Suggested commit message: `linear: expose tracker capabilities through adapter`.

19. Add `src/scherzo/tracker/adapter_factory.gleam`. Define `adapter_for_effective_config`, `adapter_for_orchestrator_config`, and any descriptor convenience wrapper. For `LinearTracker`, return the Linear adapter. Every adapter-returning function must call `capabilities.validate_adapter` before returning. Add a small test that the factory rejects a deliberately malformed adapter when a test seam provides one.

20. Update daemon startup validation. In `src/scherzo/orchestrator/daemon.gleam`, add `make_tracker_adapter: fn(config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)` to `RuntimeDependencies` while leaving old factories in place for slices not yet migrated. In the startup path that has `effective` or `OrchestratorConfig`, call `dependencies.make_tracker_adapter(effective)`, then `capability_validation.validate_orchestrator_config(orchestrator_config, capabilities.descriptor(adapter), capability_validation.DaemonStartup)` before constructing daemon state or starting actors.

21. Add startup validation tests in `test/orchestrator_daemon_test.gleam` or the closest existing daemon startup test file. Inject a fake `make_tracker_adapter` that returns a task-source-only adapter while the config enables `linear_commands.enabled`. Assert that startup returns a startup/config error containing `linear_commands.enabled`, and assert fake worker, poll scheduler, and network counters remain zero. Also assert the fake adapter construction counter is one, proving the adapter was constructed only as a pure validation object.

22. Update one-off service validation in `src/scherzo/orchestrator/service.gleam`. Extend `ContractCheckDependencies` and `DoctorDependencies` with `make_tracker_adapter: fn(config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)` or an equivalent adapter factory seam. Before `start_linear_smoke` reads from the tracker, validate `SmokeCommand`. Before `start_linear_contract_check` fetches a remote contract, validate `ContractCheckCommand`. Before running selected doctor checks, validate only the selected adapter-specific checks: Linear contract requires `ContractReadCapability`, and Linear smoke requires `SmokeReadCapability`.

23. Add service tests in `test/orchestrator_service_doctor_test.gleam` or the closest existing service test file. Test that the smoke command with a descriptor lacking `SmokeReadCapability` returns a startup/config error and does not call the smoke reader. Test that the contract command lacking `ContractReadCapability` returns an error and does not fetch the remote contract. Test that local-only doctor checks do not require tracker capabilities, while selected Linear smoke or Linear contract checks do.

24. Commit Milestone 3 after focused startup and service validation tests pass. Suggested commit message: `config: validate tracker capabilities before startup`.

25. Implement Milestone 4a, task-source migration. In `src/scherzo/orchestrator/effect_runner.gleam`, change `FetchCandidates`, `RefreshRunning`, `RefreshRetry`, and `ValidateDispatchClaim` to carry `capabilities.TaskSourceCapability` instead of `tracker.Client`. In `src/scherzo/orchestrator/daemon.gleam`, store `tracker_adapter` in state and enqueue these effects with `capabilities.task_source(state.tracker_adapter)`. Update recovery helpers and run-once paths in `src/scherzo/orchestrator/service.gleam` to use the adapter task source. Run `test/orchestrator_effect_runner_test.gleam`, `test/orchestrator_daemon_test.gleam`, and run-once service tests.

26. Implement Milestone 4b, remote command migration. In `src/scherzo/orchestrator/effect_runner.gleam`, change `FetchLinearCommands` and `PostLinearCommandAck` to carry `capabilities.RemoteCommandsCapability` or the acknowledgement function from it. In `src/scherzo/orchestrator/daemon.gleam`, replace `state.linear_command_client` with `capabilities.remote_commands(state.tracker_adapter)` plus a helper that returns a config error if validation was bypassed. Keep existing effect and ledger names unless a rename is mechanically local. Run `test/linear_command_transport_test.gleam`, `test/orchestrator_effect_runner_test.gleam`, and daemon command polling tests.

27. Implement Milestone 4c, handoff migration. In `src/scherzo/orchestrator/effect_runner.gleam`, change `ClaimIssue`, `ReportSuccess`, `ReportFailure`, and `ReportPark` to carry `capabilities.HandoffCapability` and the extracted `handoff/types.gleam` report type. In `src/scherzo/orchestrator/daemon.gleam`, replace `state.handoff_client` usages with a validated handoff capability accessor. Run the handoff-related daemon tests and `test/orchestrator_effect_runner_test.gleam`.

28. Continue Milestone 4c, scheduled failure migration. In `src/scherzo/orchestrator/effect_runner.gleam`, change `ReportScheduledFailure` to carry `capabilities.ScheduledFailureCapability` and extracted scheduled failure request/outcome types. In `src/scherzo/orchestrator/daemon.gleam`, replace `state.scheduled_failure_reporter` with a scheduled failure capability accessor in `begin_scheduled_failure_report_for_job` and retry paths. Run scheduled failure daemon tests.

29. Implement Milestone 4d, invalid-workflow triage migration. In `src/scherzo/orchestrator/effect_runner.gleam`, change `ReportInvalidWorkflow` to carry `capabilities.InvalidWorkflowTriageCapability`. In `src/scherzo/orchestrator/daemon.gleam`, replace `state.triage_client` with the invalid-workflow triage capability accessor in the transition interpreter shell and in `handle_invalid_workflow_report_finished`. Update imports to use `src/scherzo/tracker/invalid_workflow.gleam` outcome constructors. Run `test/orchestrator_transition_test.gleam`, `test/orchestrator_effect_interpreter_test.gleam`, `test/orchestrator_effect_runner_test.gleam`, and new invalid-workflow triage parity tests.

30. Implement Milestone 4e, contract, smoke, and doctor migration. In `src/scherzo/orchestrator/service.gleam`, replace direct `linear.real_contract_client` and `smoke.real_linear_reader` usage in one-off and doctor paths with `ContractCapability` and `SmokeCapability` from the adapter. Keep public function names such as `start_linear_smoke` and `start_linear_contract_check` for compatibility. Run `test/orchestrator_service_doctor_test.gleam`, `test/doctor_test.gleam`, `test/linear_contract_test.gleam`, and `test/linear_adapter_test.gleam`.

31. Remove now-unused old factories from `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/service.gleam` only after all Milestone 4 slices pass. The remaining dependency records should expose adapter factories rather than separate Linear-specific clients. Commit Milestone 4 after tests pass. Suggested commit message: `orchestrator: use tracker adapter capabilities`.

32. Confirm that no `remote_commands` public config alias was added. If implementation touched `src/scherzo/config.gleam` or `src/scherzo/config/types.gleam`, add or update config tests to prove existing `linear_commands` behavior still works and no conflicting generic alias was introduced by accident.

33. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with implementation results.

34. Run final validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect all commands to pass. If `direnv exec .` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands through direnv.

35. Commit Milestone 5 after final validation passes. Suggested commit message: `tracker: complete capability adapter migration`.

## Testing and Falsifiability

The plan is falsified if Scherzo can still start a daemon or run an adapter-specific one-off command with a configured feature that the selected adapter descriptor does not support. The negative tests must prove this before any network call, actor start, worker start, or poll scheduler start can happen.

The plan is also falsified if a descriptor can claim a capability that the runtime adapter does not actually expose. Add pure unit tests in `test/tracker_capabilities_test.gleam` for descriptor helpers and adapter invariants. The important assertions are that task source is mandatory, duplicate keys are harmless or normalized, acknowledgements cannot be declared without remote command events, `supports` returns false for absent capabilities, and `validate_adapter` rejects mismatches between descriptor keys and optional fields.

Add pure validation tests in `test/tracker_capability_validation_test.gleam`. Construct config records directly for speed. For each optional feature, create a descriptor that intentionally lacks the required capability and assert `Error(error.InvalidConfig(message))`. The message must name the config path and the missing capability in operator language. Include at least these inputs:

A config with only the tracker and polling basics and all optional feature configs disabled, plus a descriptor with only `TaskSourceCapability`, should return `Ok(Nil)`.

A config with `linear_commands.enabled = True`, `acknowledge_success = False`, and `acknowledge_rejection = False`, plus a descriptor with only `TaskSourceCapability`, should return an error that names remote command events.

A config with `linear_commands.enabled = True` and `acknowledge_success = True`, plus a descriptor with `TaskSourceCapability` and `RemoteCommandEventsCapability` but no acknowledgement capability, should return an error that names acknowledgements.

A config with `handoff.enabled = True`, plus a descriptor without `HandoffReportsCapability`, should return an error that names handoff reporting.

A config with scheduled failure issue reporting enabled, plus a descriptor without `ScheduledFailureReportsCapability`, should return an error that names scheduled failure reporting.

A config with `linear_contract.enabled = True`, plus a descriptor without `ContractReadCapability`, should return an error that names contract read support.

A config with `linear_contract.comment_on_invalid_workflow = True` or `linear_contract.invalid_workflow_state_id = Some(...)`, plus a descriptor without `InvalidWorkflowTriageCapability`, should return an error that names invalid-workflow triage support.

A config with `routing.require_exactly_one_workflow_label = True`, plus a descriptor without `RoutingMetadataCapability`, should return an error that names routing metadata.

A smoke command validation context, plus a descriptor without `SmokeReadCapability`, should return an error that names smoke read support.

Add Linear adapter parity tests in `test/linear_adapter_test.gleam`. Use a fake `linear.Transport` to prove that calling the adapter's task source produces the same result as calling `linear.client` with the same fake transport. Add a fake comment response and prove the remote command capability emits the same parsed operator command that the current Linear parser would emit. Add invalid-workflow triage tests for comment-only, state-only, comment-and-state, and no-op configurations; assert both the outcome constructor and the fake GraphQL mutation sequence.

Add startup tests near the existing daemon or service tests. The daemon startup test should inject an adapter that has `TaskSourceCapability` but lacks remote command events while the config enables remote commands. The expected result is a startup error. Fake worker, poll scheduler, and network counters must remain zero, proving validation happened before daemon start. Service tests should do the same for smoke, contract, and selected doctor checks.

Run focused tests during implementation and the full suite at the end:

    direnv exec . gleam test test/tracker_capabilities_test.gleam test/tracker_capability_validation_test.gleam test/linear_adapter_test.gleam
    direnv exec . gleam test

If the file-targeted command is not supported, use the full `gleam test` command and rely on test names to find failures.

## Validation and Acceptance

Implementation is accepted when the repository contains concrete tracker capability records, an adapter smart constructor, a Linear adapter wrapper, capability validation functions, tests for validator and adapter invariants, migration tests for the current Linear paths, and startup wiring that rejects impossible combinations before daemon or one-off command start.

The key behavioral acceptance check is this: create or construct a config with `linear_commands.enabled = True` and use a fake selected adapter that has `TaskSourceCapability` but no `RemoteCommandEventsCapability`. Starting the daemon through the same service path operators use must return an invalid config or startup error. The error message must mention `linear_commands.enabled` and missing remote command event support. No poller, worker, actor, or network client may start.

A second acceptance check is this: construct a malformed adapter whose descriptor claims remote command events while the runtime field is missing. `capabilities.validate_adapter` must reject it, and `adapter_factory.adapter_for_effective_config` must never return it as `Ok`.

A third acceptance check is invalid-workflow triage parity. With fake Linear transport responses, comment-only config must produce the same comment mutation and `InvalidWorkflowReportComment` outcome as the existing triage client, state-only config must produce the same issue state update and `InvalidWorkflowReportState`, comment-and-state config must do both in the same order and return `InvalidWorkflowReportCommentAndState`, and a config with both disabled must perform no mutation and return `InvalidWorkflowReportNoop`.

A fourth acceptance check is this: the normal Linear adapter descriptor must pass validation for the current default Linear-supported feature set. Existing Linear task fetch, handoff, scheduled failure, contract, command acknowledgement, invalid-workflow triage, and smoke tests should still pass.

A fifth acceptance check is this: running `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` from the repository root should pass.

## Rollout, Recovery, and Idempotence

The migration should be additive until each dependency slice is ready to swap. First add types, invariant checks, and validation tests, then add the Linear adapter wrapper, then validate startup, and only then replace daemon and service state fields in the Milestone 4 slices. At every commit point, the Linear path must still work and tests must pass.

Rollback is milestone-specific. Through Milestone 3, if validation proves too strict for an existing production configuration, the safe rollback is to revert the startup or one-off validation call while keeping capability records and adapter wrappers. Because adapter construction is pure and network-free, those modules can remain in the tree without changing runtime behavior.

After any Milestone 4 slice, reverting only the validation call is no longer enough for that slice. The safe rollback is to revert the specific adapter dependency-swap commit for the affected path, such as the remote command slice or scheduled failure slice. Keep old factories in dependency records until the corresponding slice is green so that a failed slice can be backed out without reconstructing deleted code by hand.

Keep legacy config keys during rollout. `linear_commands` should continue to work, and this plan should not add `remote_commands`. A later plan can add, deprecate, or remove config names after operators have a release window.

Repeated validation is idempotent. It reads resolved config and an adapter descriptor and returns a result; it should not mutate files, start processes, or call the tracker network. Adapter construction for validation is also idempotent: it creates closures only.

## Artifacts and Notes

The current tree has a clean source-control state before this plan was written, with the workspace driver reporting no changes. The implementation should preserve that discipline by committing after each green milestone.

The most important operator-facing error shape should be concise. For example:

    InvalidConfig("linear_commands.enabled requires tracker adapter linear to support remote command events")

For the specific impossible combination mentioned in the Linear issue, the validator should reject an adapter with no remote command event capability before daemon startup:

    InvalidConfig("linear_commands.enabled requires tracker adapter example to support remote command events")

Do not include absolute local paths in errors, tests, or documentation. Use repository-relative paths in test fixtures and command examples.

## Interfaces and Dependencies

The new `src/scherzo/tracker/capabilities.gleam` module is the primary interface. It depends on existing base modules such as `scherzo/error`, `scherzo/tracker/issue`, `scherzo/tracker/state`, `scherzo/tracker/kind`, `scherzo/control/command`, `scherzo/workflow_policy`, and type-only feature modules. It must not import `scherzo/linear`.

The concrete capability records should expose these operations at the end of the implementation:

    pub type TaskSourceCapability {
      TaskSourceCapability(
        fetch_candidate_issues: fn() -> Result(List(tracker_issue.Issue), error.TrackerError),
        fetch_issues_by_states: fn(List(issue_state.IssueState)) -> Result(List(tracker_issue.Issue), error.TrackerError),
        fetch_issue_states_by_ids: fn(List(String)) -> Result(List(tracker_issue.Issue), error.TrackerError),
      )
    }

    pub type CommentsCapability {
      CommentsCapability(
        fetch_issue_comments: fn(List(String), Int) -> Result(List(IssueComment), error.TrackerError),
        add_issue_comment: fn(String, String) -> Result(Nil, error.TrackerError),
      )
    }

    pub type RemoteCommandsCapability {
      RemoteCommandsCapability(
        fetch_pending: fn(List(String), Int) -> Result(List(RemoteCommandEvent), error.TrackerError),
        acknowledge: Option(fn(CommandAcknowledgement) -> Result(Nil, error.TrackerError)),
      )
    }

    pub type StateTransitionsCapability {
      StateTransitionsCapability(
        move_issue_to_state: fn(issue_id: String, state_id: String) -> Result(Nil, error.TrackerError),
      )
    }

    pub type RoutingMetadataCapability {
      RoutingMetadataCapability(
        workflow_labels: fn(tracker_issue.Issue, prefix: String) -> List(String),
      )
    }

    pub type BlockersLinksCapability {
      BlockersLinksCapability(
        blockers: fn(tracker_issue.Issue) -> #(List(tracker_issue.BlockerRef), Bool),
      )
    }

    pub type HandoffCapability {
      HandoffCapability(
        claim_issue: fn(tracker_issue.Issue, String) -> Result(Nil, error.TrackerError),
        report_success: fn(tracker_issue.Issue, agent_types.WorkerSuccess, String) -> Result(Nil, error.TrackerError),
        report_failure: fn(tracker_issue.Issue, agent_types.WorkerFailure, String) -> Result(Nil, error.TrackerError),
        report_park: fn(handoff_types.ParkReport) -> Result(Nil, error.TrackerError),
      )
    }

    pub type ScheduledFailureCapability {
      ScheduledFailureCapability(
        report_failure: fn(scheduled_failure_types.FailureReportRequest) -> Result(scheduled_failure_types.FailureReportOutcome, error.TrackerError),
      )
    }

    pub type InvalidWorkflowTriageCapability {
      InvalidWorkflowTriageCapability(
        report_invalid_workflow: fn(tracker_issue.Issue, workflow_policy.IssueWorkflowViolation) -> Result(invalid_workflow.InvalidWorkflowReportOutcome, error.TrackerError),
      )
    }

    pub type ContractCapability {
      ContractCapability(
        fetch_remote_contract: fn() -> Result(linear_contract.RemoteBoard, error.TrackerError),
      )
    }

    pub type SmokeCapability {
      SmokeCapability(
        run_read_smoke: fn(List(issue_state.IssueState)) -> Result(SmokeResult, error.TrackerError),
      )
    }

If importing `agent_types`, `handoff_types`, scheduled failure types, invalid-workflow outcome types, or the current remote contract type creates a cycle, the implementer must extract smaller type modules before completing the capability file. The adapter boundary should remain independent of Linear transport.

The validation module should be pure. Its public functions should take config records and descriptors and should return config errors. It should not read environment variables, parse YAML, start actors, or call HTTP transports.

The Linear adapter module should be the only new module that imports both the generic capability boundary and the existing Linear implementation modules. That keeps the dependency direction clear: generic orchestrator code depends on capabilities, Linear adapter code depends on Linear API functions, and Linear API functions do not depend on the orchestrator.

The adapter factory should expose these signatures or their direct Gleam equivalent:

    pub fn adapter_for_effective_config(config_types.EffectiveConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)
    pub fn adapter_for_orchestrator_config(config_types.OrchestratorConfig) -> Result(capabilities.TrackerAdapter, error.ConfigError)
    pub fn descriptor_for_effective_config(config_types.EffectiveConfig) -> Result(capabilities.AdapterDescriptor, error.ConfigError)

Every factory function that returns an adapter must call `capabilities.validate_adapter` before returning `Ok(adapter)`.

## Open Questions and Clarifications Needed

None.
