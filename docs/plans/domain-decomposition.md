# Split domain.gleam into owned domain, config, workflow, control, and orchestrator types

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a developer should be able to tell where Scherzo concepts belong by looking at their module path. Tracker issue data should live under tracker modules, runtime scheduler state should live under orchestrator modules, agent session metadata should live under agent modules, workflow configuration should live under workflow modules, and operator/Linear command settings should live under control modules. The file `src/scherzo/domain.gleam` should no longer be the shared junk drawer that every subsystem imports.

The visible proof is structural and behavioral. Structurally, production source files should no longer import `scherzo/domain`, `src/scherzo/domain.gleam` should either be removed or reduced to a temporary compatibility module with no constructors used by production code, and `grep -R "domain\." src test --include='*.gleam'` should return no matches after tests are migrated. Behaviorally, from the repository root, `direnv exec . gleam test` must continue to pass, and tests that currently exercise config parsing, workflow routing, Linear command transport, tracker integration, agent execution, orchestrator runtime transitions, session events, and control protocol encoding must still cover the same public behavior.

This plan also addresses the misleading name `EffectiveConfig`. The record is not merely an effective config value; it is the runtime settings bundle used by daemon, agent, Linear, handoff, workspace, and operator-command paths. This plan renames it to `RuntimeSettings` in `src/scherzo/config/types.gleam` and narrows APIs opportunistically so modules accept the settings records they actually need instead of the entire runtime bundle.

## Problem Framing and Constraints

The current `src/scherzo/domain.gleam` contains unrelated concepts in one module: tracker issue models, legacy workflow definitions, all config records, Linear command config, DAG hook config, artifact limits, runtime scheduler state, session metadata, retry/park/completed bookkeeping, and a few helpers. It is 297 lines, which is not huge by line count, but it is imported by 29 production Gleam modules. The architectural problem is the dependency shape, not the file size. Because everything imports `scherzo/domain`, a module that only needs a tracker issue type also sees runtime scheduler records, config records, Linear command settings, and session metadata.

The current verified facts are: `domain.gleam` defines `BlockerRef`, `Issue`, `WorkflowDefinition`, config records from `TrackerConfig` through `EffectiveConfig`, workspace/run records, `TokenTotals`, `LiveSession`, retry/running/counter/park/invalid-workflow records, and `RuntimeState`. `EffectiveConfig` includes tracker, polling, workspace, hooks, agent, pi, handoff, Linear contract, and Linear command settings. `OrchestratorConfig` wraps `EffectiveConfig`. `RuntimeState` lives in the same file as the tracker issue and config types. A search of `src` shows 29 production modules importing `scherzo/domain`.

This refactor must preserve the Erlang-target Gleam codebase and current user-visible behavior. It must not become a config format migration, a protocol change, a workflow engine rewrite, or a daemon behavior change. It is acceptable to touch many imports because this is a type-ownership refactor, but each milestone must stay small enough to compile and test.

## Strategy Overview

Use an ownership-first migration with one type group moved at a time. Gleam type aliases can preserve type annotations, but they do not preserve record constructors such as `domain.Issue(...)`. Therefore this plan does not rely on a constructor-compatible shim. Each moved custom type must be moved to its owning module and every constructor and type reference for that moved type must be updated in the same green slice.

Create these new owner modules:

- `src/scherzo/tracker/issue.gleam` for `BlockerRef` and `Issue`.
- `src/scherzo/config/types.gleam` for runtime settings and config records, including the renamed `RuntimeSettings` aggregate that replaces `EffectiveConfig`.
- `src/scherzo/orchestrator/state.gleam` for scheduler/runtime bookkeeping currently represented by `RuntimeState`, `RetryEntry`, `RunningEntry`, `IssueCounter`, `ParkReleasePolicy`, `ParkedEntry`, and `InvalidWorkflowReport`.
- `src/scherzo/agent/session.gleam` for token totals, live session metadata, and result artifacts.
- `src/scherzo/workflow/types.gleam` for legacy workflow definitions, orchestrator workflow settings, routing config, DAG hooks, and artifact limits.
- `src/scherzo/control/types.gleam` for Linear/operator command settings, starting with `LinearCommandConfig`.

Keep the existing behavior modules where they are. For example, `src/scherzo/config.gleam` should keep parsing config but return `config/types.RuntimeSettings`; `src/scherzo/orchestrator/core.gleam` should keep pure runtime transitions but use `orchestrator/state.State`; `src/scherzo/control/linear_transport.gleam` should keep Linear command transport logic but use `control/types.LinearCommandConfig`; and `src/scherzo/tracker.gleam` should keep the tracker client record but return `tracker/issue.Issue` values.

## Alternatives Considered

One alternative is to leave `domain.gleam` in place and add comments grouping its sections. That does not change the dependency graph. Production modules would still import one junk-drawer namespace and the next refactor would still have to sort ownership out.

Another alternative is a big-bang rewrite that moves all types and updates all imports in one commit. That is risky because 29 production modules and many tests import `scherzo/domain`; a single mistake would generate a large compile failure with too many causes. This plan moves types in slices that can each be compiled and tested.

A third alternative is to duplicate types in new modules and add conversion functions. That would create two incompatible issue/config/runtime models and force adapters through the whole system. The goal is ownership, not parallel models, so each concept should have one canonical type.

A fourth alternative is to keep `EffectiveConfig` and only move it into `config/types.gleam`. That fixes part of the junk-drawer problem but leaves a misleading name. This plan renames it to `RuntimeSettings`, which is honest about the aggregate while avoiding an oversized behavioral split in the same change. Narrower APIs are added as follow-up cleanup where they are obvious and low risk.

## Risks and Countermeasures

The largest risk is a wide compile break from moving constructors. Countermeasure: move one owner group at a time, use direct search to update every `domain.TypeName` and `domain.Constructor` reference for that group, then run format and tests before moving the next group.

The second risk is import cycles. Countermeasure: owner modules must point inward, not sideways. `config/types.gleam` may import `control/types.gleam` for Linear command settings, but `control/types.gleam` must not import `config/types.gleam`. `workflow/types.gleam` may import `config/types.gleam` for `RuntimeSettings`, but `config/types.gleam` must not import `workflow/types.gleam`. `orchestrator/state.gleam` may import `tracker/issue.gleam` and `agent/session.gleam`, but those modules must not import orchestrator state.

The third risk is semantic drift while renaming `EffectiveConfig` to `RuntimeSettings`. Countermeasure: do not change field names or parsing behavior in the rename commit. The old fields `tracker`, `polling`, `workspace`, `hooks`, `agent`, `pi`, `handoff`, `linear_contract`, and `linear_commands` stay the same in the first rename. Narrowing APIs happens after the rename passes tests.

The fourth risk is weakening tests by moving test helpers rather than preserving behavior. Countermeasure: keep existing assertions in tests. Rename imports and helper return types, but do not delete coverage. Move `test/domain_test.gleam` assertions to owner-specific test files instead of dropping them.

The fifth risk is leaving a compatibility `domain.gleam` module that silently lets new code keep depending on it. Countermeasure: final acceptance requires no production imports of `scherzo/domain` and no `domain.` references in `src` or `test`. If a temporary compatibility module remains for a transition commit, delete it or make it empty before completion.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current test baseline with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 11:20Z) Fact-checked `src/scherzo/domain.gleam`: it is 297 lines, defines 30 public custom types plus 2 helper functions, and 29 production modules import `scherzo/domain`.
- [x] (2026-04-30 11:25Z) Drafted this plan after checking current domain type definitions, current domain import fan-out, and the practical limitation that Gleam type aliases do not preserve record constructors.
- [ ] Milestone 0: add owner-module skeletons and move dead or isolated domain tests.
- [ ] Milestone 1: move tracker issue types to `src/scherzo/tracker/issue.gleam`.
- [ ] Milestone 2: move agent session/result types to `src/scherzo/agent/session.gleam`.
- [ ] Milestone 3: move control command settings to `src/scherzo/control/types.gleam`.
- [ ] Milestone 4: move config records and rename `EffectiveConfig` to `RuntimeSettings` in `src/scherzo/config/types.gleam`.
- [ ] Milestone 5: move workflow types to `src/scherzo/workflow/types.gleam`.
- [ ] Milestone 6: move runtime scheduler state to `src/scherzo/orchestrator/state.gleam`.
- [ ] Milestone 7: narrow obvious APIs away from whole `RuntimeSettings` where low risk.
- [ ] Milestone 8: remove `src/scherzo/domain.gleam`, run structural checks, and write the retrospective.

## Surprises & Discoveries

- Observation: `WorkspaceRecord`, `RunAttempt`, `LiveSession`, and `ParkReleasePolicy` currently have no external `domain.` references outside `src/scherzo/domain.gleam` in a direct search.
  Evidence: `grep` for those names in `src` and `test` only found their definitions and uses inside other `domain.gleam` type definitions. The implementer must recheck before deleting or moving because the tree may change.

- Observation: The user-reported production import count is correct in the current tree.
  Evidence: `grep -R "^import scherzo/domain" -n src --include='*.gleam' | cut -d: -f1 | sort -u | wc -l` returned `29`.

- Observation: A direct compatibility alias cannot preserve constructor calls.
  Evidence: A local scratch Gleam check showed `pub type Thing = other.Thing` allows the type name but does not make `Thing(...)` available as a value constructor. This plan therefore updates all constructor call sites when each type moves.

## Decision Log

- Decision: Rename `EffectiveConfig` to `RuntimeSettings` rather than splitting the aggregate into many separate runtime records in the first pass.
  Rationale: The immediate name is misleading and broad. Renaming fixes the honesty problem without changing behavior. Splitting every consumer at the same time would combine a naming migration, dependency migration, and behavior-module refactor into one high-risk change.
  Date: 2026-04-30

- Decision: Put `LinearCommandConfig` in `src/scherzo/control/types.gleam` and keep `LinearContractConfig` in `src/scherzo/config/types.gleam` for the first pass.
  Rationale: Linear command settings are operator/control transport settings. Linear contract settings are parsed runtime settings used by workflow policy and Linear contract checks; keeping them in config types avoids an import cycle between workflow types and config types.
  Date: 2026-04-30

- Decision: Put `TokenTotals`, `LiveSession`, and `ResultArtifact` in `src/scherzo/agent/session.gleam`.
  Rationale: These records describe agent/pi session output and accounting. EventHub modules and orchestrator state can depend on agent session types without depending on all config and tracker types.
  Date: 2026-04-30

- Decision: Do not keep `domain.gleam` as a long-term compatibility facade.
  Rationale: A facade would preserve the misleading dependency shape. Final acceptance requires production code to import owned modules directly.
  Date: 2026-04-30

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam Erlang-target service. The central type junk drawer is `src/scherzo/domain.gleam`. It currently imports `birl.Time`, `gleam/dict.Dict`, `gleam/option.Option`, and `yay`, then defines tracker issues, workflow definitions, config records, runtime scheduler records, session records, and helper functions in one module.

The most important current users are:

- `src/scherzo/config.gleam`, which parses config and currently returns `domain.EffectiveConfig`.
- `src/scherzo/runtime_bundle.gleam`, which loads either legacy Markdown workflow config or orchestrator YAML config and stores `domain.EffectiveConfig` plus workflow routing data.
- `src/scherzo/orchestrator/core.gleam`, which owns pure runtime transitions and currently returns `domain.RuntimeState`.
- `src/scherzo/orchestrator/daemon.gleam`, which owns the top-level daemon actor and uses nearly every category of domain type.
- `src/scherzo/agent/runner.gleam`, which runs pi agents and currently accepts `domain.Issue` and `domain.EffectiveConfig`.
- `src/scherzo/control/linear_transport.gleam`, which processes Linear comments and currently accepts `domain.LinearCommandConfig`.
- `src/scherzo/session/event.gleam`, `src/scherzo/session/hub.gleam`, and `src/scherzo/session/json.gleam`, which currently use `domain.TokenTotals`.

The repository uses tests under `test/` and the normal validation command is `direnv exec . gleam test` from the repository root. The baseline at plan authoring passed with `377 passed, no failures`.

## Preconditions and Verified Facts

Before implementation starts, re-run these commands from the repository root:

    git status --short
    direnv exec . gleam test
    wc -l src/scherzo/domain.gleam
    grep -R "^import scherzo/domain" -n src --include='*.gleam' | cut -d: -f1 | sort -u | wc -l

This is a Jujutsu repository even though `git status --short` is safe for inspection. Do not use mutating `git` commands. Use `jj status` for detailed status and use `jj describe -m "message"` plus `jj new` at commit points.

Expected current facts at plan-authoring time are:

    src/scherzo/domain.gleam has 297 lines.
    29 production Gleam modules import scherzo/domain.
    direnv exec . gleam test reports 377 passed, no failures.

If the test count has changed because other tests were added, accept a new count only if the command exits zero and reports `no failures`. If `domain.gleam` has already been partially split, update this plan's Progress and continue from the first incomplete milestone that still applies.

## Scope Boundaries

In scope: moving types out of `domain.gleam`; updating imports and constructors; renaming `EffectiveConfig` to `RuntimeSettings`; updating tests to import owner modules; removing unused domain types if still unused; preserving config parsing, workflow loading, daemon runtime state, Linear command transport, agent execution, EventHub JSON shape, and control protocol behavior.

Out of scope: changing workflow file syntax; changing Linear API payloads; changing EventHub JSON field names; changing command protocol JSON; changing daemon scheduling behavior; splitting the daemon actor; adding durable state; changing pi session behavior; broad renames of every config subrecord beyond the `EffectiveConfig` aggregate.

If a type appears to deserve a new owner not listed in this plan, add it only if it avoids a clear import cycle or ownership lie. Record that decision in the Decision Log before implementing it.

## Milestones

Milestone 0 prepares tests and module skeletons. At the end, empty or minimal owner modules exist and `test/domain_test.gleam` has been reviewed so its assertions are ready to move to owner-specific test files. No production behavior changes.

Milestone 1 moves tracker issue types. At the end, `BlockerRef` and `Issue` live in `src/scherzo/tracker/issue.gleam`, tracker and Linear clients return `issue.Issue`, and no production code constructs `domain.Issue`.

Milestone 2 moves agent session and result types. At the end, token totals and result artifacts live in `src/scherzo/agent/session.gleam`, session/event modules no longer import all of `domain.gleam`, and token JSON behavior is unchanged.

Milestone 3 moves control command settings. At the end, Linear command transport depends on `src/scherzo/control/types.gleam` for `LinearCommandConfig`, not on `domain.gleam`.

Milestone 4 moves config records and renames `EffectiveConfig` to `RuntimeSettings`. At the end, `src/scherzo/config.gleam` returns `config_types.RuntimeSettings`, all old `domain.EffectiveConfig` references are gone, and runtime settings field names are unchanged.

Milestone 5 moves workflow types. At the end, `WorkflowDefinition`, routing, DAG hooks, artifact limits, and orchestrator workflow config live in `src/scherzo/workflow/types.gleam`, and workflow modules no longer need `domain.gleam`.

Milestone 6 moves orchestrator runtime state. At the end, `RuntimeState` and scheduler bookkeeping records live in `src/scherzo/orchestrator/state.gleam`, and `src/scherzo/orchestrator/core.gleam` uses that module for pure transitions.

Milestone 7 narrows obvious APIs. At the end, selected modules that only need one part of runtime settings accept that smaller settings record. This should be conservative: do not rewrite complex paths just to chase perfect purity.

Milestone 8 removes `domain.gleam` and validates. At the end, no production or test code imports `scherzo/domain`, structural grep checks pass, and this plan records the outcome.

## Plan of Work

First, add new type-owner modules without moving behavior. Because type constructors are not re-exported through aliases, each type move must update every call site at once. Use search before each move, not memory. The implementer should search for `domain.Issue`, `domain.EffectiveConfig`, `domain.RuntimeState`, and similar names before editing a slice, update every file in that slice, then run `gleam format` and tests.

Move low-dependency types before high-dependency types. Tracker issue types are foundational because many other records refer to issues. Agent session types are also low risk. Control command settings are narrow. Config settings and workflow settings come next because many modules depend on them. Runtime scheduler state moves late because `orchestrator/core.gleam`, `orchestrator/daemon.gleam`, and many tests manipulate it heavily.

Rename `EffectiveConfig` to `RuntimeSettings` in the same milestone that moves config records. The old field names remain unchanged to minimize semantic risk. After that milestone passes, update obvious function names and local variable names where helpful, but do not churn every local `effective` variable unless it improves clarity in nearby changed code.

Finally, remove `domain.gleam` only when no references remain. If removing it breaks generated test discovery or stale imports, fix the import rather than adding the junk drawer back.

## Concrete Steps

1. From the repository root, run `jj status` and `git status --short`. Confirm only expected plan files or current-task changes are present. If unrelated code changes exist, record them in this plan before proceeding or move to a clean workspace.

2. Run `direnv exec . gleam test`. Expect `no failures`. The count was `377 passed` at plan authoring.

3. Create empty owner modules with imports only as needed: `src/scherzo/tracker/issue.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/orchestrator/state.gleam`, `src/scherzo/agent/session.gleam`, `src/scherzo/workflow/types.gleam`, and `src/scherzo/control/types.gleam`. If empty modules trigger warnings or checks, add only a temporary comment or wait to create each module in the milestone that uses it.

4. Review `test/domain_test.gleam`. For each test, decide the future owner test file. Tests for issue helpers go to `test/tracker_issue_test.gleam`; token total tests go to `test/agent_session_test.gleam`; runtime state tests go to `test/orchestrator_state_test.gleam`; config record/default tests go to `test/config_test.gleam` or `test/config_types_test.gleam`.

5. Move `BlockerRef` and `Issue` from `src/scherzo/domain.gleam` to `src/scherzo/tracker/issue.gleam`. Include `import birl.{type Time}` and `import gleam/option.{type Option}` in the new module.

6. Update every source and test reference from `domain.BlockerRef` to `issue.BlockerRef` and from `domain.Issue` to `issue.Issue`. Add imports like `import scherzo/tracker/issue` where needed. Also update type fields still temporarily in `domain.gleam` so they refer to `issue.Issue` while later milestones remain unmoved.

7. Run `grep -R "domain\.Issue\|domain\.BlockerRef" -n src test --include='*.gleam'`. It should return no matches. Then run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`.

8. Record the milestone with `jj describe -m "Move tracker issue types out of domain"`, then start the next change with `jj new` if you are keeping milestone commits separate.

9. Move `TokenTotals`, `zero_token_totals`, `LiveSession`, and `ResultArtifact` to `src/scherzo/agent/session.gleam`. Import `gleam/option.{type Option}`. Update references from `domain.TokenTotals`, `domain.zero_token_totals`, `domain.LiveSession`, and `domain.ResultArtifact` to `session.TokenTotals`, `session.zero_token_totals`, `session.LiveSession`, and `session.ResultArtifact`. If `session` conflicts with existing imports in a file, alias it as `agent_session`.

10. Run `grep -R "domain\.TokenTotals\|domain\.zero_token_totals\|domain\.LiveSession\|domain\.ResultArtifact" -n src test --include='*.gleam'`. It should return no matches. Run format and tests. Record the milestone with `jj describe -m "Move agent session types out of domain"` and `jj new`.

11. Move `LinearCommandConfig` to `src/scherzo/control/types.gleam`. Update `src/scherzo/control/linear_transport.gleam`, `src/scherzo/config.gleam`, tests for Linear command config, and any daemon/service references to import `scherzo/control/types` and use `control_types.LinearCommandConfig`.

12. Run `grep -R "domain\.LinearCommandConfig" -n src test --include='*.gleam'`. It should return no matches. Run format and tests. Record the milestone.

13. Move config records to `src/scherzo/config/types.gleam`: `TrackerConfig`, `PollingConfig`, `WorkspaceConfig`, `HooksConfig`, `UiRequestPolicy`, `AgentConfig`, `PiConfig`, `HandoffConfig`, and `LinearContractConfig`. Move `EffectiveConfig` at the same time but rename it to `RuntimeSettings`. The new `RuntimeSettings` fields remain `tracker`, `polling`, `workspace`, `hooks`, `agent`, `pi`, `handoff`, `linear_contract`, and `linear_commands`; the `linear_commands` field type is `control_types.LinearCommandConfig`.

14. Update `src/scherzo/config.gleam` first. Import `scherzo/config/types as config_types` and `scherzo/control/types as control_types`. Change default functions to return `config_types.TrackerConfig`, `config_types.PollingConfig`, and so on. Change `resolve`, `resolve_with_env`, `resolve_root`, `validate_dispatch`, `resolved_secrets`, `ReloadState.last_known_good`, and `ReloadResult` usage from `domain.EffectiveConfig` to `config_types.RuntimeSettings`.

15. Update all production references to `domain.EffectiveConfig` as `config_types.RuntimeSettings`. Update constructors from `domain.EffectiveConfig(...)` to `config_types.RuntimeSettings(...)`. Keep local variable names as-is unless changing them is local and clarifying.

16. Update references to moved config subrecords: `domain.TrackerConfig`, `domain.PollingConfig`, `domain.WorkspaceConfig`, `domain.HooksConfig`, `domain.UiRequestPolicy` variants such as `domain.Cancel`, `domain.AgentConfig`, `domain.PiConfig`, `domain.HandoffConfig`, and `domain.LinearContractConfig`.

17. Run these searches and require no matches:

    grep -R "domain\.EffectiveConfig\|domain\.TrackerConfig\|domain\.PollingConfig\|domain\.WorkspaceConfig\|domain\.HooksConfig" -n src test --include='*.gleam'
    grep -R "domain\.UiRequestPolicy\|domain\.AgentConfig\|domain\.PiConfig\|domain\.HandoffConfig\|domain\.LinearContractConfig" -n src test --include='*.gleam'

    Then run format and tests. Record the milestone with `jj describe -m "Move config types and rename runtime settings"` and `jj new`.

18. Move workflow types to `src/scherzo/workflow/types.gleam`: `WorkflowDefinition`, `RoutingConfig`, `DagHooksConfig`, `ArtifactLimits`, and `OrchestratorConfig`. Change `OrchestratorConfig` so its field is named `runtime_settings: config_types.RuntimeSettings` instead of `effective: domain.EffectiveConfig`, unless this creates too large a diff. If the field rename is too large, keep the field name `effective` for one green commit, then rename it in a separate green commit.

19. Update `src/scherzo/workflow.gleam`, `src/scherzo/workflow_dag.gleam` if needed, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/config.gleam`, `src/scherzo/workflow_policy.gleam`, `src/scherzo/workflow_run.gleam`, daemon/service modules, and tests to use `workflow_types.WorkflowDefinition`, `workflow_types.OrchestratorConfig`, `workflow_types.RoutingConfig`, `workflow_types.DagHooksConfig`, and `workflow_types.ArtifactLimits`.

20. Run `grep -R "domain\.WorkflowDefinition\|domain\.RoutingConfig\|domain\.DagHooksConfig\|domain\.ArtifactLimits\|domain\.OrchestratorConfig" -n src test --include='*.gleam'`. It should return no matches. Run format and tests. Record the milestone.

21. Move runtime scheduler state to `src/scherzo/orchestrator/state.gleam`: `RetryEntry`, `RunningEntry`, `IssueCounter`, `new_issue_counter`, `ParkReleasePolicy`, `ParkedEntry`, `InvalidWorkflowReport`, and `RuntimeState`. Name the aggregate `State` in the new module unless that causes too much ambiguity in imports; if ambiguity is high, use `RuntimeState` for one commit and rename to `State` later.

22. Update `src/scherzo/orchestrator/core.gleam` first. It should import `scherzo/orchestrator/state as orch_state`, `scherzo/tracker/issue`, `scherzo/agent/session`, and `scherzo/config/types as config_types`. Change `core.new_state` to return `orch_state.State` or `orch_state.RuntimeState`. Update all record updates and constructors.

23. Update daemon/service modules and orchestrator tests to use `orch_state.State` or `orch_state.RuntimeState`. The daemon message `GetSnapshot` and public function `get_snapshot` should now return the orchestrator state type. Preserve external behavior; only the module path changes.

24. Recheck unused types `WorkspaceRecord` and `RunAttempt`. If `grep -R "WorkspaceRecord\|RunAttempt" -n src test --include='*.gleam'` still finds only their definitions, delete them instead of moving them. If they have become used, move `WorkspaceRecord` to a new `src/scherzo/workspace/types.gleam` and `RunAttempt` to `src/scherzo/agent/session.gleam`, then record why.

25. Run `grep -R "domain\.RuntimeState\|domain\.RetryEntry\|domain\.RunningEntry\|domain\.IssueCounter\|domain\.ParkedEntry\|domain\.InvalidWorkflowReport" -n src test --include='*.gleam'`. It should return no matches. Run format and tests. Record the milestone.

26. Run `grep -R "^import scherzo/domain" -n src test --include='*.gleam'`. For each remaining import, replace it with the owner module imports it actually needs. If a test imports `domain` only for constructors moved earlier, update the test helper to import the owner module.

27. Delete `src/scherzo/domain.gleam` and move or delete `test/domain_test.gleam`. Run `grep -R "domain\." -n src test --include='*.gleam'`; it should return no matches. Run format and tests.

28. Narrow obvious APIs. Start with low-risk functions that only need one settings record. For example, functions that only read `settings.linear_commands` should accept `control_types.LinearCommandConfig`; functions that only read tracker active/terminal states should accept `config_types.TrackerConfig`; functions that only read token totals should accept `agent_session.TokenTotals`. Do not change complicated daemon/core function signatures unless the change is obvious and tested.

29. Run the final structural checks:

    test ! -f src/scherzo/domain.gleam
    ! grep -R "^import scherzo/domain" -n src test --include='*.gleam'
    ! grep -R "domain\." -n src test --include='*.gleam'
    grep -R "EffectiveConfig" -n src test --include='*.gleam'

    The first three checks must pass. The final `EffectiveConfig` search should return no matches except possibly historical text in plan files; if it appears in source or test files, rename it to `RuntimeSettings`.

30. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Expect both commands to exit zero. Update Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with `jj describe -m "Split domain types by ownership"`.

## Testing and Falsifiability

This plan is falsified if production code still imports `scherzo/domain` at the end. It is also falsified if `EffectiveConfig` remains a source/test type name, if EventHub JSON changes unexpectedly, if Linear command protocol tests change behavior, or if config parsing returns different values for the same workflow files.

Every moved type group needs tests or existing test coverage:

- Tracker issue types: update issue construction helpers in `test/linear_test.gleam`, `test/orchestrator_core_test.gleam`, `test/orchestrator_daemon_test.gleam`, and related files so they use `tracker/issue.Issue`. Existing assertions about sorting, fingerprints, Linear parsing, and workflow policy must stay unchanged.
- Agent session types: move token total tests from `test/domain_test.gleam` to `test/agent_session_test.gleam`. Assert `session.zero_token_totals()` returns zeros for input, output, cache read, cache write, and total. Existing session event JSON tests must still pass.
- Control command settings: existing `test/linear_command_config_test.gleam` and `test/linear_command_transport_test.gleam` must pass after importing `control/types.LinearCommandConfig`.
- Config/runtime settings: existing `test/config_test.gleam`, `test/runtime_bundle_test.gleam`, and service/orchestrator tests must pass. Add one assertion in `test/config_test.gleam` that `config.resolve_root` returns `config_types.RuntimeSettings` with unchanged field values for tracker endpoint, polling interval, workspace root, and Linear command prefix.
- Workflow types: existing workflow parsing, workflow DAG, workflow policy, and workflow run tests must pass after importing `workflow/types`.
- Orchestrator state: move runtime-state construction tests from `test/domain_test.gleam` to `test/orchestrator_state_test.gleam`; existing `test/orchestrator_core_test.gleam` must pass without assertion changes.

The final validation command is:

    direnv exec . gleam test

At plan authoring the expected result was `377 passed, no failures`. The final count will likely be higher or lower depending on how `test/domain_test.gleam` is split, but it must report `no failures`.

## Validation and Acceptance

Acceptance requires all of these checks from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    test ! -f src/scherzo/domain.gleam
    ! grep -R "^import scherzo/domain" -n src test --include='*.gleam'
    ! grep -R "domain\." -n src test --include='*.gleam'
    ! grep -R "EffectiveConfig" -n src test --include='*.gleam'

The format and test commands must exit zero. The grep checks must find no production or test references. It is acceptable for plan documents under `docs/plans/` to mention old names as historical context.

Behavior acceptance requires existing user-facing surfaces to remain unchanged: config files parse the same way, daemon snapshot semantics are the same except for the module path of the type, Linear command comments parse and acknowledge the same way, EventHub JSON stays compatible, and agent runner behavior is unchanged.

Architecture acceptance requires each new owner module to be the canonical home of its types. New code should import `scherzo/tracker/issue`, `scherzo/config/types`, `scherzo/orchestrator/state`, `scherzo/agent/session`, `scherzo/workflow/types`, or `scherzo/control/types` directly instead of importing a catch-all module.

## Rollout, Recovery, and Idempotence

This is an internal source refactor with no data migration. It should be safe to roll out as a normal code deploy if the full test suite passes. Because the change touches many imports, keep commits by ownership slice. If a milestone causes confusing failures, revert that one milestone instead of debugging across multiple type moves.

Each milestone is idempotent in the sense that running format, tests, and grep checks repeatedly is safe. The only non-idempotent action is deleting `src/scherzo/domain.gleam`; do that only after all references are gone and tests are green.

If implementation stops halfway, the repository should still compile because each milestone ends green. Do not leave duplicated canonical types in both `domain.gleam` and owner modules across a commit unless the duplicate is unused and will be deleted before the commit is recorded.

## Artifacts and Notes

Current domain inventory at plan authoring:

    src/scherzo/domain.gleam: 297 lines
    29 production modules import scherzo/domain
    66 total src/test paths import scherzo/domain in a broad search

Current public types in `domain.gleam`:

    BlockerRef, Issue, WorkflowDefinition, TrackerConfig, PollingConfig,
    WorkspaceConfig, HooksConfig, UiRequestPolicy, AgentConfig, PiConfig,
    HandoffConfig, ResultArtifact, LinearContractConfig, LinearCommandConfig,
    RoutingConfig, DagHooksConfig, ArtifactLimits, OrchestratorConfig,
    EffectiveConfig, WorkspaceRecord, RunAttempt, TokenTotals, LiveSession,
    RetryEntry, RunningEntry, IssueCounter, ParkReleasePolicy, ParkedEntry,
    InvalidWorkflowReport, RuntimeState

Type alias note: Gleam supports type aliases, but a type alias does not re-export a record constructor. Therefore `pub type Issue = issue.Issue` would not make `domain.Issue(...)` compile. This is why each moved type requires updating constructor call sites.

## Interfaces and Dependencies

In `src/scherzo/tracker/issue.gleam`, define:

    pub type BlockerRef {
      BlockerRef(id: Option(String), identifier: Option(String), state: Option(String))
    }

    pub type Issue {
      Issue(
        id: String,
        identifier: String,
        title: String,
        description: Option(String),
        priority: Option(Int),
        state: String,
        branch_name: Option(String),
        url: Option(String),
        labels: List(String),
        blocked_by: List(BlockerRef),
        created_at: Option(Time),
        updated_at: Option(Time),
      )
    }

In `src/scherzo/agent/session.gleam`, define `TokenTotals`, `zero_token_totals`, `LiveSession`, and `ResultArtifact` with the same fields they have today.

In `src/scherzo/control/types.gleam`, define `LinearCommandConfig` with the same fields it has today.

In `src/scherzo/config/types.gleam`, define config records with the same fields they have today and define:

    pub type RuntimeSettings {
      RuntimeSettings(
        tracker: TrackerConfig,
        polling: PollingConfig,
        workspace: WorkspaceConfig,
        hooks: HooksConfig,
        agent: AgentConfig,
        pi: PiConfig,
        handoff: HandoffConfig,
        linear_contract: LinearContractConfig,
        linear_commands: control_types.LinearCommandConfig,
      )
    }

In `src/scherzo/workflow/types.gleam`, define `WorkflowDefinition`, `RoutingConfig`, `DagHooksConfig`, `ArtifactLimits`, and `OrchestratorConfig`. Prefer this shape after the field rename:

    pub type OrchestratorConfig {
      OrchestratorConfig(
        runtime_settings: config_types.RuntimeSettings,
        config_dir: String,
        routing: RoutingConfig,
        dag_hooks: DagHooksConfig,
        artifact_limits: ArtifactLimits,
      )
    }

In `src/scherzo/orchestrator/state.gleam`, define the runtime scheduler records. Prefer naming the aggregate `State` if imports remain readable:

    pub type State {
      State(
        poll_interval_ms: Int,
        max_concurrent_agents: Int,
        running: Dict(String, RunningEntry),
        claimed: Dict(String, String),
        retry_attempts: Dict(String, RetryEntry),
        issue_counters: Dict(String, IssueCounter),
        parked: Dict(String, ParkedEntry),
        invalid_workflow_reports: Dict(String, InvalidWorkflowReport),
        completed: Dict(String, issue.Issue),
        aggregate_pi_totals: agent_session.TokenTotals,
        latest_rate_limit_payload: Option(String),
      )
    }

If `State` creates ambiguity in daemon code, use `RuntimeState` as the exported name and record in the Decision Log why the more explicit name was kept.
