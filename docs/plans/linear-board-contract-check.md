# Add read-only Linear board contract checks

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can prove that the local Scherzo workflow configuration and the remote Linear project agree before allowing agents to work. The observable behavior is a new read-only check, run from the repository root as `direnv exec . gleam run -- --linear-contract-check path/to/WORKFLOW.md`, that queries Linear project and team metadata for every team associated with the configured project, compares it to the configured Scherzo contract, prints structured diagnostics, and exits non-zero when required Linear states are missing from any project team, when required issue labels are not assignable to issues in any project team, when configured handoff state IDs do not exist, or when a multi-team project cannot be safely checked with the current single-ID handoff configuration. No Linear labels, states, issues, or comments are created or modified in this plan.

This plan addresses board-level drift only. It does not decide whether a particular issue has the right workflow label before dispatch; that is covered by the separate dispatch-policy plan. It also deliberately defers auto-reconciliation. Future work may auto-create low-risk Linear labels or suggest state changes, but this phase is detection-only.

## Problem Framing and Constraints

Scherzo currently treats `WORKFLOW.md` as the local source of runtime policy and Linear as the remote source of work. That creates a new operational invariant: the Linear board must contain the states that the local workflow expects, and the labels that the local workflow expects must be assignable to issues in the configured project. If `WORKFLOW.md` says Scherzo should dispatch from `Ready for Agent`, move invalid issues to `Needs Workflow`, and require labels like `workflow:bugfix`, then every Linear team that can contribute issues to the configured project must actually contain or inherit those states, and the required labels must be available either as team labels for that team or as workspace-level labels. Otherwise Scherzo may silently fetch no work for one team, fail handoff mutations for issues whose team does not own the configured state ID, or ask pi to handle issues whose workflow is not explicit.

The current implementation already has a bounded `--linear-smoke` mode, but that mode reads candidate and terminal issues through the issue query path. It does not query project metadata, list workflow states, list labels, or validate configured state IDs. The current `domain.Issue` model includes normalized labels, and `linear.gleam` fetches issue labels with each issue, but there is no board contract model.

The first implementation must be safe. It must be read-only, bounded, testable with fake GraphQL responses, and should not require a workspace, hooks, the instance lock, or pi. It should fail closed for the check command: if the contract cannot be fetched or mismatches are found, the command exits with startup failure and prints enough detail for a human to fix Linear or `WORKFLOW.md` manually.

## Strategy Overview

Add a small, explicit `linear_contract` section to workflow front matter. This section describes the Linear states and labels Scherzo expects to exist, plus how configured handoff state IDs should be interpreted. Add typed config fields to `domain.EffectiveConfig` and parsing/validation in `config.gleam`.

Add a new pure module, `src/scherzo/linear_contract.gleam`, that compares a local contract against a remote board snapshot. The remote snapshot contains the Linear project, every team associated with that project, each team's workflow states and issue labels, and workspace-level issue labels if Linear exposes them separately from team labels. The comparison returns a list of stable diagnostic values such as missing state on a specific team, label not assignable to a specific team, unknown handoff state ID, multi-team handoff state unsupported, or handoff state name mismatch. It also formats those diagnostics for logs and CLI output.

Extend `src/scherzo/linear.gleam` with a bounded project metadata query. The production query uses the configured Linear endpoint and API key, resolves the configured `tracker.project_slug`, fetches exactly the matching Linear project, and reads that project's `teams` connection. For each project team it reads the team's workflow states and issue labels. It also reads workspace-level labels through the top-level `issueLabels` query so a label that is available to all teams does not falsely report as missing. The query is not added to `tracker.Client`; issue tracking and board metadata are different responsibilities.

Add a new CLI mode named `--linear-contract-check`. The service path loads and resolves `WORKFLOW.md`, creates a real Linear contract reader, runs the pure comparison, logs `linear_contract_ok` on success, or logs `linear_contract_mismatch` plus every diagnostic on failure. The command performs no hooks, no workspace preparation, no pi probe, and no Linear mutations.

## Alternatives Considered

One alternative is to fold this into `--linear-smoke` only. That would make smoke more comprehensive, but it would blur two different checks. Smoke proves issue read paths and refresh work. Contract check proves the remote board has the states and labels expected by local policy. This plan adds a dedicated mode and may later let smoke call it when `linear_contract.enabled` is true.

Another alternative is to rely on Linear errors during handoff or candidate fetch. That detects drift too late and with poor diagnostics. A missing state name in `active_states` might simply return zero candidates; a missing handoff state ID fails only after Scherzo has already run an agent. A contract check should fail before dispatch.

Another alternative is to make Scherzo create missing labels or states immediately. That is intentionally deferred. Creating labels is probably low risk, but creating or renaming Linear workflow states changes team process and needs a separate reconciliation design with dry-run output, approval, and rollback guidance.

Another alternative is to store Linear state IDs for every required state instead of names. The current workflow already uses state names for `tracker.active_states` and `tracker.terminal_states`, while Linear mutations require IDs for handoff. This plan accepts names for existence checks and separately validates configured mutation IDs. A future enhancement can add state-name-to-ID resolution if operators want fewer raw IDs in `WORKFLOW.md`.

## Risks and Countermeasures

The main schema risk is that the Linear GraphQL metadata shape differs from assumptions. Countermeasure: isolate the query and decoder in `src/scherzo/linear.gleam`, add tests for the exact GraphQL request body and fake response shapes, and keep errors explicit. The public Linear schema checked while revising this plan exposes `Project.teams`, not a singular `Project.team`; each `Team` exposes `states` and `labels`, and top-level `issueLabels` can return workspace labels whose `team` is null. The planned query therefore reads `projects(first: 2, filter: { slugId: { eq: $projectSlug } })`, each matching project's `teams(first: 25)`, every team's `states(first: 100)` and `labels(first: 250)`, and top-level workspace labels. If any of those connections reports `pageInfo.hasNextPage == True`, the reader must fail closed with `LinearUnknownPayload` instead of checking a partial board. If real Linear validation reveals a different label connection name, only the contract reader needs adjustment.

The main operational risk is failing a check because local config is stricter than a board actually needs. Countermeasure: `linear_contract.enabled` defaults to false, and required states or labels are explicit lists. Teams opt in by declaring the contract they want Scherzo to enforce. Once enabled, a failed check is intentional because the local file says those Linear objects are required. For labels, the check enforces assignability to issues in the project, not where the label is stored: a matching workspace-level issue label or a matching team-scoped issue label for each project team is acceptable.

The main ambiguity risk is checking handoff state IDs without knowing the intended state name. Countermeasure: the contract can define optional handoff bindings that map handoff fields to required-state roles. For example, `claim: in_progress` says `handoff.claim_state_id` must refer to the state named by `linear_contract.required_states.in_progress`. If no binding is configured, the check only verifies that the ID exists. Handoff state ID checks run only when `handoff.enabled` is true and the corresponding handoff ID is present, because disabled handoff configuration does not affect runtime behavior.

The main safety risk is accidentally mutating Linear during a check. Countermeasure: add no mutation helpers to the contract reader, use only GraphQL query operations, and test that the check path calls only `fetch_remote_contract`. Auto-reconciliation remains out of scope and should be tracked separately.

The main multi-team risk is that a Linear project can be associated with multiple teams while Scherzo currently has one global set of handoff state IDs. Linear workflow state IDs are team-scoped, so one `handoff.success_state_id` cannot be assumed safe for issues from every team on a multi-team project. Countermeasure: model all project teams in the remote snapshot, require state and label contracts to pass for every project team, and emit a `multi_team_handoff_state_unsupported` diagnostic if `handoff.enabled` is true, the project has more than one team, and any handoff state ID is configured. A future plan can add a `tracker.team_key` filter or per-team handoff IDs if multi-team dispatch with state mutations is required.

The main compatibility risk is breaking existing workflows that do not define `linear_contract`. Countermeasure: default config disables contract checking and leaves existing `--linear-smoke`, daemon, and once behavior unchanged until operators opt in or run the new check explicitly.

## Progress

- [x] (2026-04-28 00:00Z) Discussed the desired policy: workflow labels should be explicit, Linear state should gate dispatch, and board/config drift should be detected before auto-reconciliation exists.
- [x] (2026-04-28 00:00Z) Reviewed current files relevant to the plan: `src/scherzo/domain.gleam`, `src/scherzo/config.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/smoke.gleam`, `src/scherzo/main.gleam`, `src/scherzo/orchestrator/service.gleam`, and existing Linear/config tests under `test/`.
- [x] (2026-04-28 00:00Z) Ran the current baseline from the repository root with `direnv exec . gleam test`; it passed with `200 passed, no failures`.
- [x] (2026-04-28 00:00Z) Reviewed the Linear public GraphQL schema while revising this plan and corrected the metadata model from a singular project `team` to the project `teams` connection.
- [ ] Add contract config types and parser tests.
- [ ] Add pure contract comparison and report formatting tests.
- [ ] Add Linear project metadata query builders, decoders, and fake response tests.
- [ ] Add the `--linear-contract-check` CLI and service mode.
- [ ] Update README, example workflow, and this plan's retrospective after validation.

## Surprises & Discoveries

- Observation: Linear command transport already reads comments separately from `tracker.Client`.
  Evidence: `src/scherzo/linear.gleam` defines `CommandClient`, and `src/scherzo/orchestrator/daemon.gleam` stores both `tracker_client` and `linear_command_client`. Board metadata should follow the same separation instead of expanding the issue tracker client.

- Observation: The current issue query already normalizes issue labels to lowercase.
  Evidence: `issue_decoder` in `src/scherzo/linear.gleam` maps label names with `string.lowercase` before constructing `domain.Issue`.

- Observation: The current `--linear-smoke` mode does not require dispatch hooks or acquire the instance lock.
  Evidence: `start_linear_smoke` in `src/scherzo/orchestrator/service.gleam` loads and resolves config, then calls `smoke.linear_read_smoke`; it does not call `config.validate_dispatch` or `acquire_lock`.

- Observation: Linear project metadata is team-scoped through a `teams` connection rather than a singular `team` field.
  Evidence: The public Linear GraphQL schema exposes `Project.teams`, `Team.states`, `Team.labels`, and top-level `issueLabels`; it does not expose `Project.team`.

## Decision Log

- Decision: Add a dedicated read-only contract-check mode instead of making daemon startup mutate or repair Linear.
  Rationale: Detection is the immediate need, while reconciliation has broader safety and approval requirements.
  Date: 2026-04-28

- Decision: Keep board metadata outside `tracker.Client`.
  Rationale: `tracker.Client` is the issue-dispatch abstraction. Board metadata is a Linear-provider capability used for readiness checks and future reconciliation.
  Date: 2026-04-28

- Decision: Default `linear_contract.enabled` to false for compatibility.
  Rationale: Existing workflows should continue to run until operators deliberately add the stricter contract section.
  Date: 2026-04-28

- Decision: Validate handoff state IDs by existence, and optionally by expected role-to-name binding.
  Rationale: Existing handoff config stores Linear state IDs, while dispatch config stores state names. Optional bindings let teams verify that an ID points to the intended named state without forcing every workflow to configure all roles at once.
  Date: 2026-04-28

- Decision: Model the remote board as one project with a list of teams, not as one project with one team.
  Rationale: Linear workflow states and team labels are team-scoped, and Linear projects can be associated with more than one team. Checking only one team would miss real drift and could let handoff state mutations fail after an agent has already run.
  Date: 2026-04-28

- Decision: Fail closed rather than silently accepting paginated project/team/state/label metadata.
  Rationale: A bounded read-only check is safe only if it knows it saw the complete board contract surface. A partial label or state list could report false mismatches or false success.
  Date: 2026-04-28

- Decision: Require deterministic service-level tests for `--linear-contract-check` through an injectable contract reader.
  Rationale: CLI parsing and pure comparison tests are not enough to prove that startup maps mismatches to non-zero exits and structured logs without real Linear credentials.
  Date: 2026-04-28

- Decision: Fail closed for enabled handoff state mutations on multi-team projects in this phase.
  Rationale: The current handoff config stores one global state ID per transition, but Linear workflow state IDs are team-scoped. Failing closed is safer than allowing a check to pass for a project where issue state mutations can succeed for one team and fail for another.
  Date: 2026-04-28

- Decision: Validate `tracker.active_states` and `tracker.terminal_states` in the contract-check command even when `linear_contract.enabled` is false.
  Rationale: The command's purpose is to verify the board contract before dispatch, and those tracker states already affect runtime candidate and terminal issue reads. The `linear_contract.enabled` flag gates only the additional explicit required states and labels.
  Date: 2026-04-28

- Decision: Treat required issue labels as assignability requirements rather than storage-location requirements.
  Rationale: Operators only need the label to be assignable to issues in the configured project. A workspace-level label or a team-scoped label for each project team satisfies that need.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled after implementation. Include the final test count, whether `--linear-contract-check` succeeded against a fake response and any real-board validation, and any Linear GraphQL schema mismatch discovered during implementation.)

## Context and Orientation

Scherzo is a Gleam/Erlang project. Runtime source lives under `src/scherzo/`; tests live under `test/`; validation is run from the repository root with `direnv exec . gleam test` and `direnv exec . gleam format --check src test`.

`WORKFLOW.md` is parsed by `src/scherzo/workflow.gleam`. It contains optional YAML front matter and a prompt body. `src/scherzo/config.gleam` resolves that front matter into typed config stored in `domain.EffectiveConfig`. Unknown top-level YAML keys are currently ignored.

Linear issue reads live in `src/scherzo/linear.gleam`. The current Linear client can fetch candidate issues, fetch issues by configured states, refresh issues by ID, fetch command comments for observed issues, post command acknowledgements, create handoff comments, and update an issue's state by state ID. It does not fetch project metadata, workflow state lists, or label lists.

`src/scherzo/smoke.gleam` implements the existing read-only Linear smoke check. It samples candidate issues, terminal issues, and state refresh. `src/scherzo/orchestrator/service.gleam` exposes that through `start_linear_smoke`. `src/scherzo/main.gleam` maps CLI flags to service modes.

A Linear board contract in this plan means the remote Linear objects Scherzo expects to exist before dispatch on every Linear team associated with the configured project: configured active state names, configured terminal state names, optional required triage state names, optional workflow labels, optional support labels, and any configured handoff state IDs. Linear workflow states are team-scoped. Issue labels may be team-scoped or workspace-level. A required issue label passes when it is assignable to issues in every project team, either because it is a workspace-level label or because that team exposes a matching team-scoped or inherited label.

## Preconditions and Verified Facts

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-28 while writing this plan, `direnv exec . gleam test` ended with `200 passed, no failures`.

Current repository facts this plan depends on:

- `src/scherzo/domain.gleam` defines `EffectiveConfig` with `tracker`, `polling`, `workspace`, `hooks`, `agent`, `pi`, `handoff`, and `linear_commands` fields. It does not define a board contract config.
- `src/scherzo/config.gleam` has helper functions for reading strings, string lists, integers, booleans, and maps from YAML nodes.
- `src/scherzo/main.gleam` defines `RunMode` variants `Daemon`, `Once`, `LinearSmoke`, and `PiProbe`; it does not define `LinearContractCheck`.
- `src/scherzo/orchestrator/service.gleam` exposes `start_linear_smoke` and `start_pi_probe` as startup service modes.
- `src/scherzo/linear.gleam` builds GraphQL requests with `graphql_request`, validates HTTPS endpoints and API keys, and decodes GraphQL errors through existing helper patterns.
- Linear's public GraphQL schema exposes `Project.teams`, `Team.states`, `Team.labels`, and top-level `issueLabels`; it does not expose a singular `Project.team` field.
- `test/main_test.gleam` asserts recognized CLI flags and usage text.
- `test/config_test.gleam`, `test/linear_test.gleam`, and `test/linear_http_test.gleam` cover config parsing and fake Linear request/response behavior.

If these facts differ when implementation begins, update this plan first so the plan remains self-contained.

## Scope Boundaries

In scope: local contract config parsing; pure contract comparison; read-only Linear project/team state and issue-label metadata query for every project team; workspace-level issue-label handling; fail-closed handling for paginated metadata; handoff state ID existence and optional role-name validation when handoff is enabled; a multi-team handoff unsupported diagnostic for the current single-ID handoff configuration; a dedicated CLI mode for contract checking; structured logs and clear failure messages; README and example workflow documentation; deterministic tests with fake Linear responses.

Out of scope: automatic creation, renaming, deletion, or migration of Linear labels or workflow states; mutating issues during contract check; per-issue workflow label dispatch gating; moving invalid issues to `Needs Workflow`; changing `tracker.Client`; adding a `tracker.team_key` issue filter; per-team handoff state IDs; webhooks; durable receipts; a web UI; validating every historical issue on the board.

## Milestones

Milestone 1 adds local contract configuration. At the end, tests can parse a `linear_contract` section from `WORKFLOW.md`, defaults preserve old behavior, invalid values are rejected, and `domain.EffectiveConfig` carries the contract to later phases.

Milestone 2 adds pure board comparison. At the end, tests can construct a local contract and a remote board snapshot without network access and receive stable diagnostics for missing states on a specific project team, labels not assignable to a specific project team, unknown handoff state IDs, multi-team handoff state unsupported, and handoff ID/name mismatches.

Milestone 3 adds Linear metadata reads. At the end, fake transport tests can build the project metadata GraphQL request, parse a successful response with one project and one or more project teams into a remote board snapshot, reject zero or multiple projects for the configured slug, reject projects with no teams, reject paginated teams/states/labels/workspace labels, parse GraphQL and HTTP errors, and prove no API key appears in diagnostic strings.

Milestone 4 adds CLI/service integration. At the end, `main.parse_args` accepts `--linear-contract-check`, `service.start_linear_contract_check` runs the read-only comparison, and service tests cover success, mismatch, and fetch-error behavior without real Linear credentials by injecting a fake contract reader.

Milestone 5 updates documentation and validates. At the end, README and `examples/WORKFLOW.md` document the contract section and command, all deterministic tests pass, and this plan records final results.

## Plan of Work

Extend `src/scherzo/domain.gleam` with a `LinearContractConfig` type and add it to `EffectiveConfig`. Use fields equivalent to:

    enabled: Bool
    workflow_label_prefix: String
    workflow_labels: List(String)
    support_labels: List(String)
    required_states: Dict(String, String)
    handoff_state_bindings: Dict(String, String)

`workflow_label_prefix` defaults to `"workflow:"`. `workflow_labels` stores suffixes such as `bugfix` and `research`; the full required remote label names are prefix plus suffix. `support_labels` stores full label names such as `needs-workflow`. `required_states` maps local role names such as `ready`, `needs_workflow`, `in_progress`, and `done` to Linear state names. `handoff_state_bindings` maps handoff field names `claim`, `success`, and `failure` to keys in `required_states`.

Extend `src/scherzo/config.gleam` with `default_linear_contract_config` and `resolve_linear_contract`. Parse top-level YAML like:

    linear_contract:
      enabled: true
      workflow_label_prefix: "workflow:"
      workflow_labels: [bugfix, feature, research, review, docs, chore]
      support_labels: [needs-workflow, needs-clarification]
      required_states:
        triage: "Triage"
        needs_workflow: "Needs Workflow"
        ready: "Ready for Agent"
        in_progress: "In Progress"
        blocked: "Blocked"
        done: "Done"
      handoff_state_bindings:
        claim: in_progress
        success: done
        failure: needs_workflow

Normalize configured label names by trimming whitespace and lowercasing. Drop empty string list entries after trimming, but do not silently ignore malformed non-string entries in `linear_contract` lists or maps; reject them with `InvalidConfig` so a misspelled or malformed contract cannot appear to pass. Reject `enabled: true` when `workflow_label_prefix` is empty. Reject handoff binding keys other than `claim`, `success`, and `failure`. Reject handoff binding values that do not exist in `required_states`. Add config tests for defaults, valid parsing, normalization, malformed value rejection, and invalid bindings.

Create `src/scherzo/linear_contract.gleam`. Define remote snapshot types equivalent to `RemoteBoard(project_id, project_slug, project_name, teams, workspace_labels)`, `RemoteTeam(id, key, name, states, labels)`, `RemoteState(id, name, type_)`, and `RemoteLabel(id, name)`. Define diagnostic variants equivalent to `MissingState(team_key, name, source)`, `MissingLabel(team_key, name, source)`, `MissingHandoffStateId(field, id)`, `MultiTeamHandoffStateUnsupported(field, id, team_keys)`, and `HandoffStateNameMismatch(field, id, expected, actual, actual_team_key)`. Use strings for `source` so diagnostics can say `tracker.active_states`, `tracker.terminal_states`, `linear_contract.required_states.ready`, or `linear_contract.workflow_labels`.

In `linear_contract.gleam`, expose `check(effective: domain.EffectiveConfig, remote: RemoteBoard) -> List(ContractDiagnostic)`. When `effective.linear_contract.enabled` is false, the check should still validate configured `tracker.active_states` and `tracker.terminal_states` because those fields always affect candidate and terminal reads. Validate handoff state IDs only when `effective.handoff.enabled` is true and the specific ID field is present. When `linear_contract.enabled` is true, also check every configured required state and label. A state requirement passes only if every `RemoteTeam` associated with the project has a trimmed state with the exact configured name. A label requirement passes only if the label is assignable to issues in every project team, meaning each project team either has a matching team-scoped or inherited label in `RemoteTeam.labels`, or the label exists in `workspace_labels`. State name comparison should be case-sensitive after trimming because Linear state names are operator-facing names; label comparison should be case-insensitive because issue labels are already normalized to lowercase.

Also expose `is_ok(diagnostics)`, `diagnostic_code`, `diagnostic_message`, and `format_report`. The report should be stable and compact. Example failure lines:

    missing_state team=ENG source=tracker.active_states name="Ready for Agent"
    missing_label team=ENG source=linear_contract.workflow_labels name="workflow:research"
    missing_handoff_state_id field=claim id="state-claim"
    multi_team_handoff_state_unsupported field=success id="state-done" teams="ENG,OPS"
    handoff_state_name_mismatch field=success id="state-done" expected="Done" actual="Closed" actual_team=ENG

Extend `src/scherzo/linear.gleam` with a board metadata reader. Add a type such as:

    pub type ContractClient {
      ContractClient(fetch_remote_contract: fn() -> Result(linear_contract.RemoteBoard, error.TrackerError))
    }

Add `contract_client(config, transport)` and `real_contract_client(config)`. Add `build_contract_request(config)` that requires HTTPS endpoint, API key, and project slug, and creates a GraphQL request. Use a bounded query equivalent to:

    query ScherzoLinearContract($projectSlug: String!) {
      projects(first: 2, filter: { slugId: { eq: $projectSlug } }) {
        nodes {
          id
          name
          slugId
          teams(first: 25) {
            nodes {
              id
              key
              name
              states(first: 100) {
                nodes { id name type }
                pageInfo { hasNextPage endCursor }
              }
              labels(first: 250) {
                nodes { id name }
                pageInfo { hasNextPage endCursor }
              }
            }
            pageInfo { hasNextPage endCursor }
          }
        }
      }
      issueLabels(first: 250, filter: { team: { null: true } }) {
        nodes { id name }
        pageInfo { hasNextPage endCursor }
      }
    }

If Linear's real schema uses a different team label connection name, workspace-label filter shape, or pagination shape, update only this query and its decoder during implementation, record the discovery in this plan, and keep the returned `RemoteBoard` shape the same. Decode zero projects as `LinearUnknownPayload("project slug not found")`, more than one project as `LinearUnknownPayload("project slug is not unique")`, zero project teams as `LinearUnknownPayload("project has no teams")`, and any `hasNextPage == True` metadata connection as a `LinearUnknownPayload` explaining which connection was truncated.

Add tests in `test/linear_contract_test.gleam` for pure comparison. Add tests in `test/linear_test.gleam` or a new `test/linear_contract_http_test.gleam` for request construction and response decoding. Use fake responses, not real credentials.

Extend `src/scherzo/orchestrator/service.gleam` with `start_linear_contract_check(workflow_path)`. Factor the implementation through a testable helper, for example `start_linear_contract_check_with_dependencies(workflow_path, make_contract_client, logger)`, so service tests can inject fake board snapshots, fake fetch errors, and a log capture without real Linear credentials. The public function should choose and load the workflow path, resolve config, create `linear.real_contract_client(effective.tracker)`, fetch the remote contract, run `linear_contract.check`, and log success or failure. On success, log an info event like `linear_contract_ok` with `project_slug`, `project_id`, team count, state count, and label count. On mismatch, log a warn or error event `linear_contract_mismatch` with a diagnostic count, then log one event per diagnostic using stable codes. Return `Error(StartupError("linear_contract_mismatch", "Linear board contract mismatch"))` when diagnostics are non-empty. Redact the API key using `config.resolved_secrets(effective)`.

Extend `src/scherzo/main.gleam` with a `LinearContractCheck` run mode. Accept `--linear-contract-check` with or without a path, update usage text, and add assertions to `test/main_test.gleam`.

Update `README.md` and `examples/WORKFLOW.md`. Document that the contract check is read-only, no reconciliation occurs, and operators should run it before enabling stricter workflow label enforcement. Include a minimal example and explain that state IDs are still required for handoff mutations.

## Concrete Steps

From the repository root, run the baseline tests:

    direnv exec . gleam test

Expect the final line to be similar to:

    200 passed, no failures

Edit `src/scherzo/domain.gleam` to add `LinearContractConfig` and add a `linear_contract` field to `EffectiveConfig`.

Edit `src/scherzo/config.gleam` to add the default and resolver for `linear_contract`, call it from `resolve_with_env`, and add helper functions for string maps if the existing helpers are insufficient.

Edit `test/config_test.gleam` to add tests for contract defaults, parsing, normalization, invalid binding references, and malformed non-string `linear_contract` values. Run:

    direnv exec . gleam test

Expect the new config tests to pass after the implementation is complete.

Create `src/scherzo/linear_contract.gleam` with pure remote snapshot types, diagnostics, check logic, and formatting helpers.

Create `test/linear_contract_test.gleam` with cases for all diagnostic types, per-team state and label failures, multi-team handoff state unsupported, and an all-clear single-team contract. Run:

    direnv exec . gleam test

Extend `src/scherzo/linear.gleam` with the contract client, request builder, query string, decoder, and parser. Keep request construction consistent with existing `graphql_request` and error handling. Decode `Project.teams`, per-team `states`, per-team `labels`, top-level workspace labels, and every relevant `pageInfo` field.

Add fake response tests for contract request and response parsing, including multi-team success and fail-closed pagination cases. Run:

    direnv exec . gleam test

Extend `src/scherzo/orchestrator/service.gleam` with `start_linear_contract_check` and a dependency-injected helper for tests, and extend `src/scherzo/main.gleam` with the CLI mode and usage text.

Update `test/main_test.gleam` for argument parsing and usage. Add service-level tests in `test/orchestrator_service_test.gleam` or a new focused service test file that inject fake contract clients and assert success logs, mismatch startup failure, and fetch-error startup failure without network access.

Run formatting and tests:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Commit after the tree is green. Suggested commit message:

    Add read-only Linear board contract check

Update `README.md`, `examples/WORKFLOW.md`, and this plan's Progress and Outcomes sections. Run the test suite one final time.

## Testing and Falsifiability

Add config tests that parse a workflow with no `linear_contract` and assert `enabled == False`, `workflow_label_prefix == "workflow:"`, and empty required lists/maps. Add a test that parses the full example contract and asserts normalized workflow labels such as `bugfix`, support labels such as `needs-workflow`, and required states such as `ready -> Ready for Agent`. Add invalid tests for an empty prefix with `enabled: true`, an unknown handoff binding key, a binding value that does not exist in `required_states`, a non-string label list entry, and a non-string `required_states` map value.

Add pure comparison tests in `test/linear_contract_test.gleam`. One test should build a single-team remote board with states `Ready for Agent`, `In Progress`, `Done`, labels `workflow:bugfix`, `workflow:feature`, and `needs-workflow`, then assert no diagnostics. One test should build a two-team remote board where the second team lacks `Ready for Agent` and assert a `missing_state` diagnostic with that team's key and source `tracker.active_states` or `linear_contract.required_states.ready`. One test should omit `workflow:research` from one team while it is absent from workspace labels and assert a `missing_label` diagnostic with that team's key because the label is not assignable to issues in that team. One test should put `workflow:research` only in `workspace_labels` and assert no per-team missing label. One test should set `handoff.enabled == False` and stale handoff IDs and assert no handoff diagnostics. One test should set `handoff.enabled == True` and configure `handoff.claim_state_id: state-claim` but omit a remote state with that ID and assert `missing_handoff_state_id`. One test should configure `handoff.success_state_id` bound to `done` while the remote state with that ID is named `Closed`, and assert a mismatch with expected `Done`, actual `Closed`, and the actual team key. One test should build a two-team remote board with `handoff.enabled == True` and a configured state ID, and assert `multi_team_handoff_state_unsupported`.

Add Linear fake-response tests. A successful response should include one project, at least two teams, each team's states and labels, and at least one workspace-level label. The decoder should return a `RemoteBoard` with project ID, all team IDs and keys, per-team labels, per-team states, and workspace labels. A zero-project response should return `Error(error.LinearUnknownPayload(_))`. A two-project response should return `Error(error.LinearUnknownPayload(_))`. A project with zero teams should return `Error(error.LinearUnknownPayload(_))`. Responses where project teams, team states, team labels, or workspace labels have `pageInfo.hasNextPage == True` should return `Error(error.LinearUnknownPayload(_))` rather than a partial board. A GraphQL error response should return `Error(error.LinearGraphqlErrors(_))`. A non-200 response should return `Error(error.LinearApiStatus(status))`.

Add CLI tests in `test/main_test.gleam` asserting `main.parse_args(["--linear-contract-check", "WORKFLOW.md"]) == Ok(main.Run(main.LinearContractCheck, Some("WORKFLOW.md")))` and usage mentions `--linear-contract-check`. Add service tests that inject a successful fake contract client and assert a `linear_contract_ok` log, inject a mismatching board and assert `StartupError("linear_contract_mismatch", ...)` plus diagnostic logs, and inject `Error(error.LinearApiStatus(500))` and assert the tracker error is mapped to startup failure without leaking secrets.

The plan is falsified if the contract check reports success when a configured active state, terminal state, required state, required label, or handoff state ID is missing from the fake remote board for any project team that can dispatch issues. A required label counts as missing for a project team only when it is neither present in that team's labels nor present as a workspace-level issue label. The plan is also falsified if a workspace-level label is reported missing for every team, if a multi-team project with enabled handoff state IDs passes without a diagnostic, if paginated metadata is accepted as complete, if any test requires a real Linear API key, or if the check path can create, update, or delete any Linear object.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expect all tests to pass. The baseline before implementation was `200 passed, no failures`; the final count should be higher after new tests are added.

Run help and verify the new mode is shown:

    direnv exec . gleam run -- --help

Expect usage text to include `--linear-contract-check` and describe it as read-only.

With fake or real Linear credentials available, run:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check examples/WORKFLOW.md

For an example workflow that still contains placeholder `project_slug: YOUR_PROJECT_SLUG`, expect startup failure due to Linear lookup or credentials. For a real workflow whose contract matches Linear, expect a `linear_contract_ok` log and exit code 0. For a workflow with a deliberately missing required label such as `workflow:not-real`, expect `linear_contract_mismatch`, one `missing_label` diagnostic, and a non-zero exit.

## Rollout, Recovery, and Idempotence

This change is read-only. Running the check repeatedly is safe because it performs only GraphQL queries. There is no state to roll back in Linear.

Existing daemon and once workflows remain compatible because `linear_contract.enabled` defaults to false and this plan does not put the check into the dispatch hot path. Operators can adopt it by adding a `linear_contract` section to `WORKFLOW.md` and running `--linear-contract-check` in their preflight process. If a Linear project has multiple teams and handoff state IDs are enabled, this check intentionally fails until operators either disable handoff state mutations for that workflow, constrain dispatch to one team in a future plan, or introduce per-team handoff state IDs in a future plan.

If the new Linear metadata query is wrong for the real API, disable use of the new check by not running the new mode, fix the query/decoder behind tests, and retry. No workspaces or issue states are affected.

## Artifacts and Notes

The most important expected success transcript is:

    direnv exec . gleam test
    ...
    N passed, no failures

A representative mismatch report should contain stable event codes rather than prose-only diagnostics, for example:

    level=error service=scherzo event=linear_contract_mismatch diagnostic_count=2
    level=error service=scherzo event=linear_contract_diagnostic code=missing_state team=ENG source=tracker.active_states name="Ready for Agent"
    level=error service=scherzo event=linear_contract_diagnostic code=missing_label team=OPS source=linear_contract.workflow_labels name="workflow:research"

Do not include the Linear API key in any diagnostic, formatted report, or test assertion.

Plan revision note, 2026-04-28: this plan was revised after review to replace the invalid singular `Project.team` assumption with Linear's `Project.teams` connection, make multi-team behavior explicit, fail closed on paginated metadata, require deterministic service tests, avoid validating disabled handoff state IDs, always validate tracker active and terminal states in the explicit check command, and treat label requirements as issue-label assignability requirements rather than storage-location requirements. These changes prevent a future implementer from building a check that either fails against the real Linear schema or reports a false green result for multi-team projects.

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, add a public type equivalent to:

    pub type LinearContractConfig {
      LinearContractConfig(
        enabled: Bool,
        workflow_label_prefix: String,
        workflow_labels: List(String),
        support_labels: List(String),
        required_states: Dict(String, String),
        handoff_state_bindings: Dict(String, String),
      )
    }

In `src/scherzo/linear_contract.gleam`, expose pure types and functions equivalent to:

    pub type RemoteBoard
    pub type RemoteTeam
    pub type RemoteState
    pub type RemoteLabel
    pub type ContractDiagnostic
    pub fn check(domain.EffectiveConfig, RemoteBoard) -> List(ContractDiagnostic)
    pub fn diagnostic_code(ContractDiagnostic) -> String
    pub fn diagnostic_message(ContractDiagnostic) -> String
    pub fn format_report(List(ContractDiagnostic)) -> String

In `src/scherzo/linear.gleam`, expose a client equivalent to:

    pub type ContractClient {
      ContractClient(fetch_remote_contract: fn() -> Result(linear_contract.RemoteBoard, error.TrackerError))
    }

    pub fn contract_client(domain.TrackerConfig, Transport) -> ContractClient
    pub fn real_contract_client(domain.TrackerConfig) -> ContractClient
    pub fn build_contract_request(domain.TrackerConfig) -> Result(Request, error.TrackerError)
    pub fn parse_contract_response(Response) -> Result(linear_contract.RemoteBoard, error.TrackerError)

In `src/scherzo/orchestrator/service.gleam`, expose the public mode and a testable helper equivalent to:

    pub fn start_linear_contract_check(Option(String)) -> Result(Nil, StartupError)
    pub fn start_linear_contract_check_with_dependencies(
      Option(String),
      fn(domain.TrackerConfig) -> linear.ContractClient,
      fn(String) -> Result(Nil, Nil),
    ) -> Result(Nil, StartupError)

In `src/scherzo/main.gleam`, add `LinearContractCheck` to `RunMode` and route it to the new service function.
