# Build a real multi-tracker integration port

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo should have an honest tracker integration boundary that can support more than Linear. A developer adding a second tracker should implement one cohesive tracker port that covers issue reads, issue refresh, comments, command ingestion, command acknowledgements, state transitions, board/label contract semantics where supported, invalid-workflow reporting, handoff reporting, issue references, smoke checks, and config. The daemon, agent runner, workflow policy, handoff, and control-command transport should depend on that port rather than directly reaching into Linear-specific modules.

The visible proof is that production orchestration code no longer injects one generic `tracker.Client` plus separate Linear command and Linear triage clients. Instead, daemon startup builds a `tracker.Adapter` or `tracker.Integration` from runtime settings, and the daemon uses that one adapter for issue reads, command comments, acknowledgements, handoff, invalid workflow reporting, and optional contract checks. Linear remains the first implemented adapter, but a fake second adapter used in tests proves the port is real and not just renamed Linear code.

## Problem Framing and Constraints

The current `src/scherzo/tracker.gleam` exports a generic-looking `Client` with only three issue-fetch methods: `fetch_candidate_issues`, `fetch_issues_by_states`, and `fetch_issue_states_by_ids`. That is not enough to add another tracker. The rest of Scherzo is Linear-shaped. `src/scherzo/linear.gleam` owns Linear GraphQL transport, query strings, response parsing, comments, contract reads, and mutations. `src/scherzo/control/linear_transport.gleam` polls Linear comments and turns them into operator commands. `src/scherzo/handoff.gleam` posts Linear comments and updates Linear states. `src/scherzo/linear_triage.gleam` reports invalid workflow labels through Linear comments and Linear state updates. `src/scherzo/linear_contract.gleam` compares local config to Linear teams, states, and labels. `src/scherzo/orchestrator/daemon.gleam` injects a generic `make_tracker` but separately injects `make_linear_commands` and `make_triage`, which exposes the abstraction leak.

This is painful because a future GitHub Issues, Jira, or file-backed tracker could not be added by implementing `tracker.Client`. It would also have to bypass or reimplement Linear command comments, handoff, triage, contract checks, state id handling, workflow label semantics, issue references, smoke checks, and config parsing. Even Linear changes are coupled across daemon, control, handoff, smoke, and contract modules because there is no single Linear adapter boundary.

This plan chooses real multi-tracker support. It must not remove the Linear adapter or regress current Linear behavior. It should initially add a second fake or local in-memory adapter only for tests, not a full production GitHub/Jira implementation. The goal is to prove the port is real and to make a production second tracker implementable later.

## Strategy Overview

Define a real tracker port in `src/scherzo/tracker.gleam` or a new `src/scherzo/tracker/port.gleam`. The port should be cohesive and capability-aware. The core issue-read methods are mandatory. Mutating and optional features should be represented as capability records or `Option` fields so adapters can honestly say what they support. The daemon should query capabilities rather than importing Linear-specific clients.

The new port should separate generic tracker concepts from Linear details:

- Generic issue reads and refresh: candidates, states, by id, by reference.
- Generic comments/commands: fetch operator command comments or events, post acknowledgement or rejection where supported, and track processed remote command ids.
- Generic handoff: claim, report success, report failure, with adapter-defined behavior.
- Generic state transitions: move issue to a configured state id or state key when supported.
- Generic workflow policy support: classify workflow labels or route metadata in a tracker-neutral way, with Linear labels as one implementation.
- Generic contract or readiness checks: optional adapter capability for remote board contract diagnostics.
- Generic invalid-workflow reporting: optional capability that may comment, move state, both, or no-op.
- Generic smoke/readiness check: adapter-specific smoke check used by CLI.

Linear will implement every capability that currently exists. A new fake test adapter will implement enough capabilities to prove the daemon and service can run without importing Linear-specific command/triage clients. Later production adapters can implement a subset; unsupported capabilities must fail gracefully or be disabled by config validation.

## Alternatives Considered

One alternative is to rename `tracker.Client` to `linear_issue_source.Client` and admit Scherzo is Linear-only. The user has rejected that direction. This plan instead builds the real port required for multi-tracker support.

Another alternative is to add only issue-read abstraction and leave comments, handoff, commands, and contract checks Linear-specific. That is the current problem in another form. A tracker port that cannot express comments, commands, state transitions, and workflow metadata is not a real port for Scherzo.

A third alternative is to make every tracker support every Linear feature. That would make many adapters impossible. The port should be capability-aware: an adapter can say it supports command comments, handoff comments, state transitions, contract checks, or invalid-workflow reporting independently. Config validation then rejects impossible combinations, such as enabling remote command comments for a tracker that has no comment-command capability.

A fourth alternative is to implement GitHub or Jira production support immediately. That is too large for the first phase. The first phase should make the architecture real and prove it with a fake adapter and the existing Linear adapter. A production second adapter should be a follow-up plan.

## Risks and Countermeasures

The largest design risk is producing a port that is too vague to implement. Countermeasure: define concrete records, capability fields, and function signatures in the plan. The adapter must cover the actual surfaces Scherzo uses today, not abstract ideals.

The largest behavior risk is breaking Linear. Countermeasure: migrate Linear behind the new port without changing Linear query strings, mutation strings, response parsing, command parsing, handoff comments, contract diagnostics, or error codes. Existing Linear tests must keep passing.

The largest compatibility risk is config validation. Countermeasure: keep existing `tracker.kind: linear` config working. Add new tests for a fake `tracker.kind: memory` or `test` adapter only in test/dependency code. Validation should reject unsupported feature combinations with clear config errors before daemon start.

The largest incremental-delivery risk is trying to move every Linear-specific module at once. Countermeasure: introduce the new port alongside existing clients, then route one capability at a time through it: issue reads first, then command comments, then handoff, then triage, then contract/smoke checks.

The largest naming risk is creating another god interface. Countermeasure: group capabilities into nested records, not one flat record with dozens of methods. Use `Adapter(issue_source, command_transport, handoff, triage, contract, smoke, capabilities)` or similar so each subsystem can receive only the capability it needs.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current test baseline earlier in this work session with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 12:40Z) Fact-checked the current tracker/Linear boundary: `src/scherzo/tracker.gleam` has only a three-method issue-read client; Linear commands, triage, handoff, contract checks, comments, mutations, and smoke checks live in separate Linear-specific modules.
- [x] (2026-04-30 13:05Z) Revised the previous Linear-only plan after stakeholder clarification: Scherzo needs real multi-tracker support, so the design must build a capability-aware tracker port instead of renaming to Linear-only.
- [ ] Milestone 0: document current Linear surfaces and add capability-validation characterization tests.
- [ ] Milestone 1: define the tracker port and capability records without changing production behavior.
- [ ] Milestone 2: wrap the existing Linear implementation as a `tracker.Adapter`.
- [ ] Milestone 3: route daemon issue reads through the adapter.
- [ ] Milestone 4: route Linear command comments and acknowledgements through the adapter command capability.
- [ ] Milestone 5: route handoff and invalid-workflow reporting through adapter capabilities.
- [ ] Milestone 6: route smoke and contract checks through adapter capabilities.
- [ ] Milestone 7: add a fake non-Linear adapter for tests and prove daemon/service paths do not import Linear-specific clients.
- [ ] Milestone 8: remove direct Linear-specific dependencies from orchestration boundaries and write the retrospective.

## Surprises & Discoveries

- Observation: `error.TrackerError` is already Linear-specific in its variants.
  Evidence: `src/scherzo/error.gleam` defines variants such as `LinearApiRequest`, `LinearApiStatus`, `LinearGraphqlErrors`, `LinearUnknownPayload`, and `LinearMissingEndCursor` under the generic `TrackerError` name. The port needs an error story that supports both generic adapter errors and Linear-specific details.

- Observation: `workflow_policy.gleam` is named generic but currently depends on `domain.LinearContractConfig` and Linear-style labels.
  Evidence: `classify_issue` accepts `domain.LinearContractConfig` and checks `issue.labels` against `workflow_label_prefix` and configured workflow labels. A real port should expose a tracker-neutral workflow metadata policy or make this an optional label-capability owned by adapters.

- Observation: `control/linear_transport.gleam` already contains useful tracker-command concepts mixed with Linear comment names.
  Evidence: it keeps processed remote command ids, filters remote comments by created time, parses command text, submits `command.OperatorCommand`, and posts acknowledgements. Those behaviors can become a generic command-event transport with a Linear comment implementation.

## Decision Log

- Decision: Build a real capability-aware tracker port instead of renaming to Linear-only.
  Rationale: The stakeholder clarified that multi-tracker support is required. A three-method issue-read client is insufficient, so the port must cover commands, comments, state transitions, handoff, triage, contract/readiness, and workflow metadata capabilities.
  Date: 2026-04-30

- Decision: Keep Linear as the first adapter and add a fake non-Linear adapter for tests before implementing a production second tracker.
  Rationale: This proves the architecture without committing to the domain details of GitHub, Jira, or another tracker in the same refactor.
  Date: 2026-04-30

- Decision: Use capability records rather than one flat mega-record.
  Rationale: Adapters will not all support every feature. Capability records make unsupported features explicit and let config validation reject impossible combinations.
  Date: 2026-04-30

- Decision: Preserve legacy Linear config and behavior throughout the migration.
  Rationale: Existing users and tests rely on `tracker.kind: linear`, Linear smoke checks, Linear command comments, Linear contract checks, and handoff behavior. Multi-tracker support should be additive and architectural first.
  Date: 2026-04-30

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo currently targets Linear. The README says Scherzo polls one Linear project and uses Linear GraphQL over HTTPS. The current generic tracker module is `src/scherzo/tracker.gleam`, but it only covers issue reads. The large Linear implementation is `src/scherzo/linear.gleam`, which owns HTTP transport, GraphQL request/response types, issue queries, state refresh queries, comment queries, comment creation, issue state updates, contract queries, parsers, and real client constructors.

The daemon lives in `src/scherzo/orchestrator/daemon.gleam`. Its `RuntimeDependencies` currently has `make_tracker`, `make_handoff`, `make_linear_commands`, and `make_triage`. This should become one tracker adapter factory plus capability access. The daemon currently uses tracker issue reads for polling and refresh, Linear commands for remote operator comments, triage for invalid workflow reporting, and handoff for claim/success/failure reporting.

Handoff lives in `src/scherzo/handoff.gleam`. It has a generic `Client` but its real constructor is `linear_client`, which posts Linear comments and updates Linear issue states. Invalid workflow reporting lives in `src/scherzo/linear_triage.gleam`, which posts Linear comments and updates Linear issue states when an issue has invalid workflow labels. Linear command comments are handled by `src/scherzo/control/linear_transport.gleam`, which processes `linear.LinearComment` values.

Board contract checks live in `src/scherzo/linear_contract.gleam` and `src/scherzo/linear.gleam`. They query Linear project teams, workflow states, team labels, and workspace labels, then compare them to local config. A different tracker may not have the same contract model, so this should be an optional adapter capability.

## Preconditions and Verified Facts

Before implementation, run from the repository root:

    jj status
    direnv exec . gleam test
    grep -R "^import scherzo/tracker" -n src test --include='*.gleam'
    grep -R "tracker.Client\|make_tracker\|make_linear_commands\|make_triage\|linear.CommandClient\|linear_triage.TriageClient" -n src test --include='*.gleam'

At plan authoring, the prior baseline was:

    377 passed, no failures

At plan authoring, `src/scherzo/tracker.gleam` contained only a three-method `Client`. `src/scherzo/linear.gleam` contained `CommandClient`, `ContractClient`, GraphQL queries, comments, contract parsing, and mutations. `src/scherzo/handoff.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/control/linear_transport.gleam`, `src/scherzo/linear_contract.gleam`, and `src/scherzo/smoke.gleam` were Linear-shaped and must be accounted for in the real port.

This is a Jujutsu repository. Do not use mutating `git` commands. Use `jj status`, `jj describe -m "message"`, and `jj new` for milestone commit discipline.

## Scope Boundaries

In scope: defining a real tracker adapter port; wrapping Linear as one adapter; adding adapter capability validation; routing daemon issue reads, command comments, acknowledgements, handoff, invalid-workflow reporting, smoke checks, and contract checks through adapter capabilities; adding a fake non-Linear adapter for tests; preserving current Linear behavior.

Out of scope: implementing a production GitHub/Jira adapter; changing the pi agent runner; changing workspace management; changing Linear GraphQL query behavior; changing Linear command syntax; changing local control API; adding durable state; redesigning issue model fields beyond what the port requires.

If `docs/plans/domain-decomposition.md` lands first, put port model types in the new owner modules it defines. If not, keep this plan's new tracker modules alongside `domain.gleam` and migrate later.

## Milestones

Milestone 0 inventories current Linear surfaces and adds tests around capability validation. At the end, tests explicitly prove unsupported tracker capabilities fail at config/startup validation rather than later in the daemon loop.

Milestone 1 defines the tracker port. At the end, new types exist but production behavior is unchanged. The port shape is concrete enough for Linear and a fake test adapter.

Milestone 2 wraps Linear as an adapter. At the end, `linear_adapter.from_config` or equivalent returns a complete `tracker.Adapter` that delegates to existing Linear functions.

Milestone 3 routes issue reads through the adapter. At the end, polling, running refresh, retry refresh, agent final state refresh, workflow run dependencies, and one-shot service mode use `adapter.issue_source` instead of raw `tracker.Client`.

Milestone 4 routes remote command transport through the adapter. At the end, daemon remote operator commands do not directly depend on `linear.CommandClient` or `control/linear_transport.gleam`; they use a generic `CommandEventTransport` capability implemented by Linear comments.

Milestone 5 routes handoff and invalid-workflow reporting through adapter capabilities. At the end, daemon side effects no longer separately inject `handoff.Client` and `linear_triage.TriageClient`; they use adapter capabilities with no-op unsupported implementations where config disables the feature.

Milestone 6 routes smoke and contract checks through adapter capabilities. At the end, CLI smoke and contract-check modes ask the selected adapter for support and either run adapter-specific checks or return a clear unsupported error.

Milestone 7 adds a fake non-Linear adapter in tests. At the end, at least one daemon/service test runs through a non-Linear adapter with issue reads and no Linear command/handoff/triage capabilities, proving the orchestration boundary is not Linear-only.

Milestone 8 removes obsolete generic or Linear-specific boundary leaks and records the outcome.

## Plan of Work

Start with types and validation. Add a new adapter model while keeping existing clients. Then create a Linear adapter wrapper that composes existing Linear modules. Once Linear is available through the adapter, change daemon and service dependency injection from separate clients to an adapter factory. Migrate each capability in isolation so failures point to one boundary. Add a fake non-Linear adapter after the daemon can run through adapter capabilities; this proves the port is real. Finish by cleaning up imports and docs.

Do not rewrite `src/scherzo/linear.gleam` internals in this plan. It can remain the Linear GraphQL implementation behind the Linear adapter. Do not split `linear_contract.gleam` yet; instead expose it through a contract capability.

## Concrete Steps

1. From the repository root, run `jj status` and confirm there are no unrelated source changes. If unrelated changes exist, stop and record them before proceeding.

2. Run `direnv exec . gleam test`. Expect `no failures`. Record the pass count in Progress.

3. Add tests in `test/config_test.gleam` or a new `test/tracker_capability_test.gleam` for capability validation. Use current Linear config and assert existing Linear command/handoff/contract-enabled configs still validate. Add a fake unsupported tracker config in test-only parsing or dependency setup and assert enabling remote commands, handoff mutations, or contract checks rejects with a clear error. This test may fail until the adapter/capability validation exists.

4. Create `src/scherzo/tracker/port.gleam` or replace `src/scherzo/tracker.gleam` with a richer model. Prefer `src/scherzo/tracker/port.gleam` if the existing module must remain as a compatibility facade during migration.

5. Define the core issue-source capability:

    pub type IssueSource {
      IssueSource(
        fetch_candidate_issues: fn() -> Result(List(domain.Issue), error.TrackerError),
        fetch_issues_by_states: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
        fetch_issue_states_by_ids: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
        fetch_issue_by_ref: fn(command.IssueRef) -> Result(domain.Issue, TrackerCommandStatus),
      )
    }

    If adding `fetch_issue_by_ref` is too broad for the first compile, add it in the command capability milestone and record the decision.

6. Define generic remote command types in the tracker port:

    pub type RemoteCommandEvent {
      RemoteCommandEvent(
        id: String,
        issue_id: String,
        body: String,
        created_at_ms: Int,
        updated_at_ms: Int,
        author_id: String,
        author_label: String,
      )
    }

    pub type CommandEventTransport {
      CommandEventTransport(
        fetch_events: fn(List(String), Int) -> Result(List(RemoteCommandEvent), error.TrackerError),
        post_ack: fn(String, String) -> Result(Nil, error.TrackerError),
      )
    }

7. Define handoff and invalid-workflow capabilities:

    pub type HandoffCapability {
      HandoffCapability(
        claim_issue: fn(domain.Issue, String) -> Result(Nil, error.TrackerError),
        report_success: fn(domain.Issue, runner.WorkerSuccess, String) -> Result(Nil, error.TrackerError),
        report_failure: fn(domain.Issue, runner.WorkerFailure, String) -> Result(Nil, error.TrackerError),
      )
    }

    pub type InvalidWorkflowReporter {
      InvalidWorkflowReporter(
        report_invalid_workflow: fn(domain.Issue, workflow_policy.IssueWorkflowViolation) -> Result(InvalidWorkflowReportOutcome, error.TrackerError),
      )
    }

    Avoid importing `runner.gleam` if `docs/plans/agent-pi-rpc-decomposition.md` has already moved result types; use `agent/types` then.

8. Define contract and smoke capabilities:

    pub type ContractCapability {
      ContractCapability(check_remote_contract: fn(domain.EffectiveConfig) -> Result(List(linear_contract.ContractDiagnostic), error.TrackerError))
    }

    pub type SmokeCapability {
      SmokeCapability(run_smoke: fn(List(String)) -> Result(smoke.LinearSmokeResult, error.TrackerError))
    }

    If `smoke.LinearSmokeResult` is too Linear-named, introduce `tracker.SmokeResult` with the same count fields and migrate smoke later.

9. Define the adapter record and capabilities:

    pub type Adapter {
      Adapter(
        kind: String,
        issue_source: IssueSource,
        command_transport: Option(CommandEventTransport),
        handoff: Option(HandoffCapability),
        invalid_workflow_reporter: Option(InvalidWorkflowReporter),
        contract: Option(ContractCapability),
        smoke: Option(SmokeCapability),
        secrets: List(String),
      )
    }

    Add helpers such as `disabled_handoff`, `unsupported_command_transport`, or validation helpers as needed.

10. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. This milestone should be type additions only or mostly additions. Record it with `jj describe -m "Define tracker adapter port"` and start a new change with `jj new` if keeping milestones separate.

11. Create `src/scherzo/tracker/linear_adapter.gleam`. Implement a function like `from_config(effective: domain.EffectiveConfig, dependencies: LinearDependencies) -> tracker_port.Adapter`. It should compose existing `linear.client`, `linear.command_client`, `handoff.linear_client`, `linear_triage.triage_client`, `linear.contract_client`, and `smoke.linear_reader` behind port capabilities.

12. Convert `linear.LinearComment` to `tracker_port.RemoteCommandEvent` in the Linear adapter or in a new generic command transport module. Preserve comment id, issue id, body, timestamps, author id, and author label.

13. Add `test/tracker_linear_adapter_test.gleam`. Use fake Linear transport functions or existing fake clients to assert the adapter exposes expected capabilities when Linear commands, handoff, triage, contract, and smoke are enabled. Assert disabled config returns `None` or no-op capabilities where appropriate.

14. Run format and tests. Record the milestone.

15. Change daemon `RuntimeDependencies`. Replace `make_tracker`, `make_handoff`, `make_linear_commands`, and `make_triage` with a single `make_tracker_adapter: fn(domain.EffectiveConfig) -> tracker_port.Adapter` or a dependency record that receives the effective config and returns an adapter. Keep old dependency fields temporarily only if needed for a smaller green commit.

16. Update daemon state. Replace `tracker_client`, `handoff_client`, `linear_command_client`, and `triage_client` with `tracker_adapter` plus any cached capability state needed by command transport. Keep `linear_command_state` temporarily if command transport still uses the old Linear-specific state.

17. Route issue read side effects through `state.tracker_adapter.issue_source`. Update `FetchCandidates`, `RefreshRunning`, `RefreshRetry`, agent runner dependency calls, workflow run dependencies, and service one-shot dispatch to use `IssueSource` instead of `tracker.Client`.

18. Run:

    grep -R "tracker.Client\|make_tracker\|tracker_client" -n src/scherzo --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Production matches should be gone or limited to compatibility shims scheduled for removal. Record the milestone.

19. Generalize command transport. Create `src/scherzo/control/tracker_command_transport.gleam` by adapting `src/scherzo/control/linear_transport.gleam` to consume `tracker_port.RemoteCommandEvent` and a `CommandEventTransport` capability. Keep parsing through `control/linear_parser.gleam` at first if the command syntax stays `/scherzo`; rename the parser later if desired.

20. Update daemon polling to call `adapter.command_transport` if present and config remote commands are enabled. If config enables commands but the adapter has no command transport, validation must reject startup with a clear error. Convert `PostLinearCommandAck` side effects into generic `PostTrackerCommandAck` side effects.

21. Keep `control/linear_transport.gleam` as a facade or delete it after tests migrate. Add tests that process generic `RemoteCommandEvent` values and produce the same command results and ack bodies as current Linear comment tests.

22. Run:

    grep -R "linear_command_client\|FetchLinearCommands\|PostLinearCommandAck\|linear_transport" -n src/scherzo --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Remaining Linear-specific names in daemon should be gone before final acceptance. Record the milestone.

23. Route handoff through adapter capabilities. Replace daemon handoff side-effect clients with `adapter.handoff`. If handoff config is enabled but the adapter lacks handoff, config validation or adapter validation must reject startup. Disabled handoff should use no-op or `None` without side effects.

24. Route invalid-workflow reporting through `adapter.invalid_workflow_reporter`. Keep `workflow_policy` behavior initially, but make the reporter generic. If a tracker lacks workflow-label capability and enforcement is enabled, validation should reject startup or the adapter should provide a no-op policy only if enforcement is disabled.

25. Run:

    grep -R "handoff_client\|linear_triage\|triage_client\|ReportInvalidWorkflow" -n src/scherzo/orchestrator src/scherzo --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Daemon should not directly import `linear_triage` after this milestone. Record it.

26. Route smoke and contract checks. In `src/scherzo/orchestrator/service.gleam`, replace direct `linear.real_contract_client` and `smoke.real_linear_reader` construction with the selected adapter's `contract` and `smoke` capabilities. For unsupported adapters, return a clear `StartupError("tracker_capability_unsupported", ...)` or a more specific code.

27. Update CLI naming only if needed. Existing flags `--linear-smoke` and `--linear-contract-check` may remain as Linear-specific aliases while a future generic `--tracker-smoke` is added. Do not break existing CLI in this plan unless explicitly accepted. Add tests that unsupported adapters return clear errors.

28. Run format and tests. Record the milestone.

29. Add a fake non-Linear adapter for tests, for example `test/support/memory_tracker.gleam` or a test-local helper module. It should implement `IssueSource` and no command/handoff/triage/contract capabilities. Use it in at least one daemon or service test that starts with `tracker.kind: memory` or direct dependency injection and proves polling/dispatch can use the adapter without Linear clients.

30. Add a validation test that enabling remote commands or handoff with the fake adapter fails before daemon start. This proves unsupported capability combinations are explicit.

31. Run format and tests. Record the milestone.

32. Clean up old boundaries. Decide whether `src/scherzo/tracker.gleam` becomes the real port module or is deleted in favor of `src/scherzo/tracker/port.gleam`. Do not leave both with overlapping public names. Remove direct daemon dependencies on `linear.CommandClient`, `linear_triage.TriageClient`, and `handoff.Client` if they are now reachable through the adapter.

33. Run structural checks:

    ! grep -R "make_linear_commands\|make_triage\|linear_command_client\|triage_client" -n src/scherzo/orchestrator --include='*.gleam'
    ! grep -R "tracker.Client" -n src/scherzo --include='*.gleam'
    ! grep -R "linear_triage" -n src/scherzo/orchestrator --include='*.gleam'
    grep -R "CommandClient" -n src/scherzo/orchestrator --include='*.gleam'

    The final grep should return no daemon/orchestrator direct dependency on Linear command clients.

34. Update README/docs only after architecture supports multiple adapters. Document Linear as the first production adapter and the fake/memory adapter as test-only if no production second adapter exists. Be precise: Scherzo has a multi-tracker port, but only Linear is production-ready until another adapter ships.

35. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with `jj describe -m "Introduce capability-aware tracker adapter port"`.

## Testing and Falsifiability

This plan is falsified if adding a non-Linear adapter still requires changing daemon internals or importing Linear-specific command/triage clients. The fake non-Linear adapter test is mandatory. It must prove that issue reads and dispatch work through the adapter while unsupported remote-command or handoff capabilities fail at validation.

Linear behavior must remain covered by existing tests:

- `test/linear_test.gleam`, `test/linear_http_test.gleam`, and `test/linear_comments_test.gleam` must still cover Linear GraphQL requests, parsing, comments, and mutations.
- `test/linear_command_transport_test.gleam` must be migrated or mirrored to generic command-event transport tests without losing command parsing and ack assertions.
- `test/linear_contract_test.gleam` must still cover Linear contract diagnostics.
- `test/linear_triage_test.gleam` must still cover Linear invalid-workflow report outcomes through the Linear adapter capability.
- `test/handoff_test.gleam` must still cover Linear handoff comments and state updates through the adapter handoff capability.
- Daemon tests must still cover polling, retry refresh, Linear command comments, invalid-workflow reporting, and worker dispatch.

Add new tests:

- `tracker_adapter_exposes_linear_capabilities_test`: Linear adapter exposes issue source, command transport when enabled, handoff when enabled, invalid workflow reporter when configured, contract, smoke, and secrets.
- `memory_tracker_dispatches_without_linear_capabilities_test`: daemon or service dispatches from a fake non-Linear issue source with command/handoff/triage disabled.
- `unsupported_remote_commands_fail_validation_test`: fake adapter plus enabled remote commands returns a clear startup/config error.
- `unsupported_handoff_fails_validation_test`: fake adapter plus enabled handoff mutation returns a clear startup/config error.

The final test command remains:

    direnv exec . gleam test

At plan authoring, the prior baseline was `377 passed, no failures`; the final count may change but must report `no failures`.

## Validation and Acceptance

Acceptance requires these commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    ! grep -R "make_linear_commands\|make_triage\|linear_command_client\|triage_client" -n src/scherzo/orchestrator --include='*.gleam'
    ! grep -R "tracker.Client" -n src/scherzo --include='*.gleam'
    ! grep -R "linear_triage" -n src/scherzo/orchestrator --include='*.gleam'

It is acceptable for Linear adapter modules and Linear-specific tests to import `linear`, `linear_triage`, `linear_contract`, and `control/linear_parser`. It is not acceptable for the daemon or service orchestration boundary to directly assemble Linear command and triage clients outside the tracker adapter.

Acceptance also requires a fake non-Linear adapter test to pass. Without that test, the port may still be Linear-specific in practice.

External Linear behavior must not regress. Current Linear config files must still run, Linear command comments must still parse and acknowledge, handoff must still post comments/state updates when enabled, invalid-workflow reporting must still no-op/comment/state/comment-and-state as before, and contract/smoke CLI behavior must remain stable for Linear.

## Rollout, Recovery, and Idempotence

This is an architectural refactor with no data migration. Linear remains the only production adapter unless a production second adapter is added separately. Roll out after the full suite passes.

Keep commits by capability. If command transport migration fails, revert only that milestone while keeping the adapter and issue-source milestones. If handoff/triage migration fails, revert only those capability milestones. Do not leave daemon using both adapter capabilities and old separate Linear clients for the same action across a committed state.

Config changes should be backward compatible. Existing `tracker.kind: linear` workflows must continue to parse. If new test-only tracker kinds are added, gate them so production config cannot accidentally select an incomplete adapter unless explicitly intended.

## Artifacts and Notes

Current generic client at plan revision time:

    src/scherzo/tracker.gleam
    pub type Client {
      Client(
        fetch_candidate_issues: fn() -> Result(List(domain.Issue), error.TrackerError),
        fetch_issues_by_states: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
        fetch_issue_states_by_ids: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
      )
    }

Current Linear-specific surfaces to include in the real port:

- `src/scherzo/linear.gleam`: issue reads, comments, ack mutations, contract reads, state update mutations, HTTP transport, GraphQL parsers.
- `src/scherzo/control/linear_transport.gleam`: remote command event processing and ack body generation.
- `src/scherzo/handoff.gleam`: claim/success/failure comments and state transitions.
- `src/scherzo/linear_triage.gleam`: invalid workflow comments and state transitions.
- `src/scherzo/linear_contract.gleam`: board/team/state/label diagnostics.
- `src/scherzo/smoke.gleam`: read-only Linear smoke checks.

## Interfaces and Dependencies

The exact module paths may change if domain decomposition lands first. The target shape is:

    pub type Adapter {
      Adapter(
        kind: String,
        issue_source: IssueSource,
        command_transport: Option(CommandEventTransport),
        handoff: Option(HandoffCapability),
        invalid_workflow_reporter: Option(InvalidWorkflowReporter),
        contract: Option(ContractCapability),
        smoke: Option(SmokeCapability),
        secrets: List(String),
      )
    }

    pub type IssueSource {
      IssueSource(
        fetch_candidate_issues: fn() -> Result(List(domain.Issue), error.TrackerError),
        fetch_issues_by_states: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
        fetch_issue_states_by_ids: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
      )
    }

    pub type RemoteCommandEvent {
      RemoteCommandEvent(
        id: String,
        issue_id: String,
        body: String,
        created_at_ms: Int,
        updated_at_ms: Int,
        author_id: String,
        author_label: String,
      )
    }

    pub type CommandEventTransport {
      CommandEventTransport(
        fetch_events: fn(List(String), Int) -> Result(List(RemoteCommandEvent), error.TrackerError),
        post_ack: fn(issue_id: String, body: String) -> Result(Nil, error.TrackerError),
      )
    }

    pub type HandoffCapability {
      HandoffCapability(
        claim_issue: fn(domain.Issue, String) -> Result(Nil, error.TrackerError),
        report_success: fn(domain.Issue, runner.WorkerSuccess, String) -> Result(Nil, error.TrackerError),
        report_failure: fn(domain.Issue, runner.WorkerFailure, String) -> Result(Nil, error.TrackerError),
      )
    }

    pub type InvalidWorkflowReporter {
      InvalidWorkflowReporter(
        report_invalid_workflow: fn(domain.Issue, workflow_policy.IssueWorkflowViolation) -> Result(linear_triage.InvalidWorkflowReportOutcome, error.TrackerError),
      )
    }

Use existing `error.TrackerError` initially for minimal churn. A later refinement may split generic adapter errors from Linear-specific errors, but this plan's main goal is a real multi-capability port.
