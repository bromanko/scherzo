# Getting started with Scherzo

This guide is for humans and coding agents adapting Scherzo to a different repository. It starts from an empty repo-local Scherzo config and ends with a cautious `--once` run against one task. The production tracker adapter is Linear today, so the task is a Linear issue in this guide.

Keep this guide practical and use the normative specs when you need exact contracts:

- [Workspace Driver Specification](specs/WORKSPACE_DRIVER_SPEC.md)
- [Tracker Adapter Specification](specs/TRACKER_ADAPTER_SPEC.md)
- [Structured Output Validator Specification](specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md)

## Recommended adaptation order

Adapt Scherzo in this order so each layer can be checked before the next one adds risk:

1. `.scherzo/scherzo.yaml`
2. workspace profile / driver
3. tracker adapter setup, Linear workflow labels, and routing
4. workflow DAG
5. prompt templates
6. structured output schemas / validators
7. handoff and task-system comment policy
8. scheduled jobs, if needed

## 1. Prerequisites

Before configuring Scherzo for a repository, collect these inputs.

### Tracker adapter and Linear

- A Linear API key available as `LINEAR_API_KEY` or another environment variable referenced by config.
- The Linear project slug for tasks Scherzo should poll. In Linear, each task is a Linear issue.
- The state names used for dispatch and lifecycle decisions, for example `Todo`, `In Progress`, `Done`, `Canceled`, and `Duplicate`.
- The workflow labels you want to route, usually labels with the `workflow:` prefix such as `workflow:research` and `workflow:implementation`.
- Whether Scherzo should post comments or move states during handoff. Start with handoff disabled until `doctor` and a single `--once` run are understood.

See the [Tracker Adapter Specification](specs/TRACKER_ADAPTER_SPEC.md) for the normative adapter contract and [Tracker adapters](runbooks/tracker-adapters.md) for operator guidance, the capability matrix, and the compatibility aliases that still use Linear or issue vocabulary.

### `pi` and model/provider credentials

Scherzo executes agent steps through `pi`. The Nix-packaged `scherzo`/`scherzo-start` wrappers and the source checkout direnv/devenv shell provide Scherzo's pinned `pi` fork with RPC message update suppression. Non-Nix deployments should install a compatible `pi` or otherwise put it on `PATH`. Choose a model/provider, and make provider credentials available in the environment that will run Scherzo. A safe first config uses non-persistent pi RPC without streaming `message_update` records:

```yaml
pi:
  command: "pi --mode rpc --no-session --rpc-message-updates off"
  compatibility_probe: true
```

Set `pi.model` and `pi.thinking` only after you know the provider key names that work in your `pi` installation.

### Scherzo command and local tools

Use either the packaged Scherzo command or a source checkout:

- Packaged/devenv usage exposes `scherzo`, `scherzo-start`, `scherzoctl`, `scherzo-workspace-noop`, and `scherzo-workspace-jj` on `PATH`.
- Source-checkout usage normally runs `direnv exec . gleam run -- ...`, `direnv exec . scherzo-start ...`, and `direnv exec . scripts/scherzoctl ...` from the Scherzo repository.

Install any tools used by your workflows and drivers, such as `jj`, `git`, `gh`, `python3`, `node`, project test runners, or JSON-schema validator dependencies.

### Repository trust and safety

Scherzo is intended for trusted repositories and trusted workflow files. Workflow command steps, prompt templates, structured-output command validators, and workspace drivers are local operator policy. They may run arbitrary commands with your OS user permissions.

Scherzo checks workspace cwd/root containment, but it is not a VM or container sandbox. Do not point it at unreviewed workflow YAML, prompt templates, validators, or drivers from untrusted sources.

## 2. Install Scherzo in another repo

For a consuming devenv project, add the Scherzo flake input and package.

```yaml
# devenv.yaml
inputs:
  nixpkgs:
    url: github:NixOS/nixpkgs/nixos-25.11
  scherzo:
    url: github:scherzo-systems/scherzo
```

```nix
# devenv.nix
{ pkgs, inputs, ... }:

let
  system = pkgs.stdenv.hostPlatform.system;
in
{
  packages = [
    inputs.scherzo.packages.${system}.default
    inputs.scherzo.packages.${system}.pi
    # Add jj, gh, Python, Node, and project tools separately as needed.
  ];
}
```

Then verify the packaged command from the consuming repository:

```sh
direnv allow .
direnv exec . scherzo --version
direnv exec . scherzo --help
```

Run Scherzo from the repository that contains `.scherzo/scherzo.yaml`, so relative config, workflow, schema, script, and workspace paths resolve against that repository.

If no config path is provided, Scherzo looks for the first existing default config in this order:

1. `.scherzo/scherzo.yaml`
2. `.scherzo/scherzo.yml`
3. `scherzo.yaml`
4. `scherzo.yml`

## 3. Create `.scherzo/scherzo.yaml`

Create the repo-local Scherzo directory:

```sh
mkdir -p .scherzo/workflows/prompts schemas scripts
# Or consume Scherzo's packaged dogfood workflows:
# bundle=$(nix build --no-link --print-out-paths github:scherzo-systems/scherzo#scherzo-dogfood-workflows)
# ln -sfn "$bundle" .scherzo/workflows
```

Start with a minimal no-op workspace profile. This is useful for research, planning, and artifact-only workflows because it does not require a VCS-backed implementation workspace.

```yaml
# .scherzo/scherzo.yaml
version: 1
tracker:
  kind: linear
  credentials:
    api_key_env: LINEAR_API_KEY
  linear:
    endpoint: https://api.linear.app/graphql
    project_slug: YOUR_LINEAR_PROJECT_SLUG
  active_states: [Todo, In Progress]
  dispatch_states: [Todo]
  terminal_states: [Done, Canceled, Cancelled, Duplicate]

polling:
  interval_ms: 30000

# Daemon mode runs one immediate startup poll, then schedules recurring tracker polls
# at interval_ms plus bounded jitter. The jitter bound is 10% of interval_ms, with
# a 1 ms floor, and the effective delay is always positive.

workspace:
  # Relative to .scherzo/scherzo.yaml, so this becomes .scherzo/workspaces/.
  root: workspaces
  default_profile: noop
  profiles:
    noop:
      driver:
        command: scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000

agent:
  max_concurrent_agents: 1
  max_turns: 1
  max_retry_backoff_ms: 300000
  max_retry_attempts: 2
  max_sessions_per_issue: 2

pi:
  command: "pi --mode rpc --no-session --rpc-message-updates off"
  turn_timeout_ms: 3600000
  read_timeout_ms: 5000
  stall_timeout_ms: 300000
  auto_retry: true
  ui_request_policy: cancel
  compatibility_probe: true

handoff:
  enabled: false

routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    getting-started: workflows/getting-started.yaml

linear_contract:
  enabled: false
  workflow_label_prefix: "workflow:"
  workflow_labels: [getting-started]
  support_labels: [needs-workflow, needs-clarification]
  required_states:
    todo: Todo
    in_progress: In Progress
    done: Done
    needs_workflow: Needs Workflow
  handoff_state_bindings:
    claim: in_progress
    success: done
    failure: needs_workflow
  enforce_issue_workflow_labels: false
  comment_on_invalid_workflow: false

linear_commands:
  enabled: false
  prefix: "/scherzo"
  authorized_user_ids: []
```

Path rule of thumb:

- `routing.workflows` paths are relative to the config file directory. In `.scherzo/scherzo.yaml`, `workflows/getting-started.yaml` means `.scherzo/workflows/getting-started.yaml`.
- JSON Schema validator paths are repository-relative, for example `schemas/implementation_plan.schema.json`.
- Command validator `argv` entries run from `working_directory`, usually `repository` for scripts under `scripts/`.
- Packaged workspace drivers can be named directly, for example `scherzo-workspace-noop` or `scherzo-workspace-jj`. Repository-local drivers can use `scripts/...`, an absolute path, or `$SCHERZO_REPO_ROOT/scripts/...` where supported by the workspace driver spec.

## 4. Configure the Linear tracker adapter

### Project, API key, and states

Set `tracker.linear.project_slug` to the Linear project slug Scherzo should poll. Keep the API key in the environment rather than committing a secret:

```sh
export LINEAR_API_KEY=lin_api_...
```

Use `tracker.dispatch_states` for task states Scherzo may pick up. It must be a subset of `tracker.active_states`. Put completed or abandoned states in `tracker.terminal_states` so Scherzo can ignore those tasks and reason about recovery. Older flat fields such as `tracker.api_key`, `tracker.endpoint`, and `tracker.project_slug` remain compatibility aliases; prefer `tracker.credentials.api_key_env` and `tracker.linear.*` in new config.

### Workflow labels and routing

With this routing config:

```yaml
routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    getting-started: workflows/getting-started.yaml
    implementation: workflows/implementation.yaml
```

a task labeled `workflow:getting-started` routes to `.scherzo/workflows/getting-started.yaml`, and a task labeled `workflow:implementation` routes to `.scherzo/workflows/implementation.yaml`. With `require_exactly_one_workflow_label: true`, Scherzo rejects tasks with no workflow label or multiple workflow labels.

### Linear contract checks

`linear_contract` lets `doctor` compare your config to the real Linear board before you enforce routing or handoff policy. Enable it once the labels and states exist on the board:

```yaml
linear_contract:
  enabled: true
  workflow_label_prefix: "workflow:"
  workflow_labels: [getting-started, implementation]
  support_labels: [needs-workflow, needs-clarification]
  required_states:
    todo: Todo
    in_progress: In Progress
    done: Done
    needs_workflow: Needs Workflow
  handoff_state_bindings:
    claim: in_progress
    success: done
    failure: needs_workflow
  enforce_issue_workflow_labels: true
  comment_on_invalid_workflow: true
```

Run `doctor --check tracker-contract` before enabling enforcement on a real board. `linear-contract` remains a compatibility alias.

### Handoff policy

Keep handoff disabled for the first `doctor` runs and any dry-run-like exploration:

```yaml
handoff:
  enabled: false
```

Before daemon mode, decide whether Scherzo should post comments, move states, include result text inline, or attach the result as a Linear comment file:

```yaml
handoff:
  enabled: true
  comment_on_claim: true
  comment_on_success: true
  comment_on_failure: true
  comment_on_park: true
  include_result_on_success: false
  attach_result_on_success: true
  attachment_fallback_to_markdown_link: true
  result_max_chars: 8000
  # Optional direct Linear state ids. If omitted, Scherzo comments without
  # moving that state.
  # claim_state_id: <linear-in-progress-state-id>
  # success_state_id: <linear-done-state-id>
  # failure_state_id: <linear-needs-workflow-or-triage-state-id>
```

For long-running daemon operation, use some handoff or manual board process that prevents successful tasks from remaining eligible for dispatch forever.

Artifact-producing workflows should use completion-state policy instead of a blanket `success_state_id`. A successful implementation or ExecPlan run usually produces work that a human should review, so the standard policy moves those tasks to `In Review`, not `Done`. Failure and partial-success outcomes, including a workflow that was expected to produce an artifact but did not, should move to an attention state such as `Needs Attention`. A cancellation leaves the Linear state unchanged unless `cancellation_state` is configured.

```yaml
linear_contract:
  enabled: true

handoff:
  enabled: true
  completion_states:
    default_completion_state: In Review
    no_review_completion_state: Done
    failure_state: Needs Attention
    partial_success_state: Needs Attention
    workflows:
      execplan:
        produces_reviewable_artifacts: true
        requires_review: true
      no-review-maintenance:
        produces_reviewable_artifacts: false
        requires_review: false
        success_state: Done
```

Use `_id` keys such as `default_completion_state_id` or `failure_state_id` when your Linear team uses duplicate or localized state names. Before enabling daemon handoff, run `scherzo doctor --check tracker-contract`; policy-enabled handoff requires the configured review and attention states to exist. See [Linear completion states](runbooks/linear-completion-states.md) for migration and remediation details.

## 5. Choose workspace behavior

Workspace profiles are trusted operator policy under `workspace.profiles`. A workflow may select a named profile and require driver capabilities, but workflow YAML cannot define the shell command that creates or removes workspaces.

### `noop` for research and artifact-only workflows

Use the bundled no-op driver when a workflow should produce artifacts or comments without modifying a VCS-backed implementation workspace.

```yaml
workspace:
  root: workspaces
  default_profile: noop
  profiles:
    noop:
      driver:
        command: scherzo-workspace-noop
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

A workflow can make that choice explicit:

```yaml
version: 1
id: research
workspace_profile: noop
workspace_capabilities: [assert-only]
steps:
  - id: research
    kind: agent
    prompt: prompts/research.md
    workspace: main
```

### `jj` for implementation workflows

Use the bundled jj driver when implementation steps need isolated workspaces and later publication. Choose the base, fetch, and publish policy deliberately for your repository.

```yaml
workspace:
  root: workspaces
  default_profile: isolated
  profiles:
    isolated:
      driver:
        command: scherzo-workspace-jj
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
        env:
          # Local/offline starter policy: use the current jj change as base and
          # do not fetch. Replace this with your real base/remote policy before
          # unattended implementation work.
          SCHERZO_JJ_WORKSPACE_BASE: "@"
          SCHERZO_JJ_WORKSPACE_FETCH_BASE: "false"
          # Examples for remote-backed policy:
          # SCHERZO_JJ_WORKSPACE_REMOTE: upstream
          # SCHERZO_JJ_WORKSPACE_BASE_BRANCH: trunk
          # SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE: origin
```

Implementation workflows often require capabilities:

```yaml
version: 1
id: implementation
workspace_profile: isolated
workspace_capabilities: [status, diff, changed-files, baseline, refresh-base, publish-change]
```

### Custom workspace drivers

Provide a custom driver when neither bundled driver matches your repository. Typical reasons include a different VCS, a monorepo-specific checkout process, custom artifact assertions, or a publication path that does not use the bundled jj/gh assumptions.

Configure the trusted command in the orchestrator config:

```yaml
workspace:
  root: workspaces
  default_profile: myrepo
  profiles:
    myrepo:
      driver:
        command: scripts/scherzo-workspace-myrepo
        lifecycle: [create, before-step, after-step, remove]
        timeout_ms: 60000
```

Then let workflows select the profile and require capabilities:

```yaml
workspace_profile: myrepo
workspace_capabilities: [status, diff, changed-files]
```

A custom driver must implement discovery with `<driver> describe --json`, any configured lifecycle operations, and any advertised capabilities. See the [Workspace Driver Specification](specs/WORKSPACE_DRIVER_SPEC.md) for the exact command, JSON, exit-code, environment, and path-safety contract.

## 6. Add workflow routing

Add one routing key per workflow YAML file:

```yaml
routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    getting-started: workflows/getting-started.yaml
    implementation: workflows/implementation.yaml
    research: workflows/research.yaml
```

Create matching workflow labels in the tracker. With the Linear adapter, these are Linear issue labels:

- `workflow:getting-started`
- `workflow:implementation`
- `workflow:research`

Start with one route and one label. Add more after `doctor` and `--once` are passing.

## 7. Add a workflow YAML DAG

Create `.scherzo/workflows/getting-started.yaml`:

```yaml
version: 1
id: getting-started
description: Draft a small plan and verify that the expected artifact exists.
workspace_profile: noop
max_parallel_steps: 1
steps:
  - id: draft_plan
    kind: agent
    prompt: prompts/getting-started.md
    workspace: main

  - id: validate_plan_file
    kind: command
    depends_on: [draft_plan]
    run: |
      set -eu
      test -s implementation-plan.md
      printf 'implementation-plan.md exists and is non-empty\n'
    workspace: main
```

Important workflow rules:

- `id` should match the routing key.
- `depends_on` forms a DAG; cycles are rejected.
- Prompt paths are relative to the workflow YAML file and must stay inside that workflow directory.
- Steps sharing the same logical workspace are serialized.
- Different logical workspaces may run concurrently up to `max_parallel_steps` and `agent.max_concurrent_agents`.
- Agent steps inherit project-level `pi` settings unless the step overrides `model` or `thinking`.
- Command steps run shell commands in the prepared workspace.
- Retained artifacts use the default filesystem artifact store unless you add a
  different backend in code. Existing workflow commands that read retained
  files still rely on `SCHERZO_RUN_ARTIFACT_DIR`, which points at the run's
  `.scherzo-state/artifacts/...` directory for that default store.

## 8. Add prompt templates

Create `.scherzo/workflows/prompts/getting-started.md`:

```md
You are adapting this repository to Scherzo.

Task: {{ issue.identifier }} — {{ issue.title }}

Description:
{{ issue.description }}

Task:
Write a concise implementation plan to `implementation-plan.md`.
Include:

1. summary
2. risks
3. validation commands
4. next steps

Do not modify unrelated files.
```

Prompt templates are Markdown files rendered for `pi`. Prefer task language in prompt prose. The current compatibility variables are still `issue.*`, so templates can reference the source task and prior step artifacts like this:

```md
Previous validation output:
{{ steps.validate_plan_file.stdout }}
```

Markdown prompt templates are not runtime workflow definitions. Runtime workflows are YAML DAGs.

## 9. Add structured output and validators

Structured output is useful when later steps or scripts need a machine-readable contract rather than free-form Markdown. It is declared on agent steps only.

A minimal `structured_output` block:

```yaml
structured_output:
  artifact_name: implementation_plan
  required: true
  source:
    type: final_response
  schema:
    type: object
    required: [schema_version, summary, next_steps]
  validators:
    - name: implementation_plan_shape
      type: json_schema
      path: schemas/implementation_plan.schema.json
```

JSON Schema validators are built into Scherzo. A workflow author declares only `name`, `type: json_schema`, a repository-relative `path`, and optional `draft: "2020-12"`; Scherzo runs local draft 2020-12 validation, rejects schema paths that are blank, absolute, parent-traversing, or symlink outside the repository, and records schema path, schema SHA-256, draft, validator summary, and source type in retained artifacts. Structured-output metadata also carries the artifact `ref`, store-neutral `uri`, human-facing `display_path`, and optional `local_path`; the legacy `path` field remains for compatibility with existing filesystem-oriented consumers, but new code should read by `ref` and use `local_path` only when it is present. A schema mismatch is treated as retryable agent output when the artifact is required. Missing schema files, invalid schemas, unsupported drafts, helper failures, and path escapes are non-retryable configuration errors.

With `source.type: final_response`, the final assistant response must be exactly one JSON document. Do not wrap it in Markdown fences or add commentary. Your prompt should say that explicitly:

```md
When finished, respond only with JSON matching the implementation_plan schema.
Do not include Markdown fences, prose, or extra keys unless the schema allows them.
```

Add the block to an agent step when you want Scherzo to retain and validate the JSON artifact:

```yaml
- id: draft_plan_json
  kind: agent
  prompt: prompts/getting-started-json.md
  workspace: main
  structured_output:
    artifact_name: implementation_plan
    required: true
    source:
      type: final_response
    schema:
      type: object
      required: [schema_version, summary, next_steps]
    validators:
      - name: implementation_plan_shape
        type: json_schema
        path: schemas/implementation_plan.schema.json
```

Create the JSON Schema at the repository-relative path declared above:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "required": ["schema_version", "summary", "next_steps"],
  "properties": {
    "schema_version": { "type": "integer", "const": 1 },
    "summary": { "type": "string", "minLength": 1 },
    "next_steps": {
      "type": "array",
      "items": { "type": "string", "minLength": 1 },
      "minItems": 1
    }
  },
  "additionalProperties": true
}
```

For semantic checks that JSON Schema cannot express, add a command validator after the JSON Schema validator. Command validators receive the admitted JSON payload on stdin and accept or reject it by exit status. If an older workflow uses a command validator only to run JSON Schema shape validation, migrate that shape check to `type: json_schema` and keep command validators only for repository-specific semantic checks.

```yaml
validators:
  - name: implementation_plan_shape
    type: json_schema
    path: schemas/implementation_plan.schema.json
  - name: implementation_plan_semantics
    type: command
    argv:
      - python3
      - scripts/validate-implementation-plan.py
    timeout_ms: 30000
    working_directory: repository
```

Recommended locations:

- `schemas/` for repository-wide JSON Schemas.
- `.scherzo/workflows/schemas/` if schemas are tightly coupled to Scherzo workflows; remember to declare the repository-relative path, for example `.scherzo/workflows/schemas/implementation_plan.schema.json`.
- `scripts/` for trusted command validators and custom workspace drivers.

Use the [Structured Output Validator Specification](specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md) for exact source extraction, retry, validator, artifact, and diagnostic behavior.

## 10. Run readiness checks

Run `doctor` before any real task dispatch:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo doctor .scherzo/scherzo.yaml
```

From a Scherzo source checkout, use:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml
```

The default doctor run loads config, workflows, and prompts; checks the tracker contract and read-only tracker access; acquires the local instance lock; exercises workspace lifecycle; and launches a pi compatibility probe without sending a task prompt.

Use the read-only subset when you only want config and tracker validation:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo doctor \
  --check workflow-config \
  --check tracker-contract \
  --check tracker-smoke \
  .scherzo/scherzo.yaml
```

`linear-contract` and `linear-smoke` remain compatibility aliases for the Linear adapter.

List available checks:

```sh
direnv exec . scherzo doctor --list-checks
```

Common doctor failures:

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| Config file not found | Running from the wrong directory or no config path passed | Run from the target repo or pass `.scherzo/scherzo.yaml` |
| Linear auth failure | Missing/invalid `LINEAR_API_KEY` | Export a valid key in the Scherzo process environment |
| Project not found | Wrong `tracker.linear.project_slug` | Copy the slug from the Linear project URL/config |
| State/label mismatch | Board does not have configured states or labels | Create labels/states or adjust `linear_contract` and `tracker` config |
| No workflow route | Task label suffix does not match `routing.workflows` | Add the workflow YAML and routing key or fix the Linear label |
| Multiple workflow labels | `require_exactly_one_workflow_label: true` and task has more than one | Leave exactly one `workflow:*` label on the task |
| Workspace driver discovery fails | Driver not on `PATH`, not executable, or invalid `describe --json` | Fix the profile command or implement the driver spec |
| Workspace lifecycle fails | Driver cannot create/remove scratch workspace or selected jj base is unavailable | Fix driver env, VCS state, base branch, or local permissions |
| Legacy or unsupported shape | Config, workflow, driver, tracker, or local state uses an old shape | Read the diagnostic path/code and the [upgrade policy](runbooks/upgrades.md) or linked specific runbook |
| Pi probe fails | `pi` missing, provider credentials missing, or command incompatible | Run `pi --mode rpc --no-session --rpc-message-updates off` manually and fix credentials/config |
| Prompt/schema path missing | Paths are relative to different roots | Check workflow-relative prompt paths and repository-relative schema paths |

## 11. Run one task with `--once`

After doctor passes, create or choose one low-risk task. With the Linear adapter this means a Linear issue:

- State is in `tracker.dispatch_states`, for example `Todo`.
- Project matches `tracker.linear.project_slug`.
- Exactly one workflow label is present, for example `workflow:getting-started`.
- The task description is safe for the configured workflow and workspace profile.

Run one eligible task and exit:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo --once .scherzo/scherzo.yaml
```

Source-checkout equivalent:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml
```

Inspect the workspace root, retained artifacts, command output, and tracker comments before moving to daemon mode. If handoff is disabled, manually move or relabel the task so it does not remain eligible for repeated dispatch.

## 12. Start daemon mode with `scherzo-start`

Use `scherzo-start` for interactive daemon mode. It translates Ctrl-C/SIGINT into Scherzo's graceful SIGTERM shutdown path.

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo-start .scherzo/scherzo.yaml
```

Before daemon mode:

- Keep `agent.max_concurrent_agents: 1` until the workflow is proven.
- Enable handoff or establish a manual policy that removes completed tasks from dispatch states.
- Keep an operator watching the first several runs.
- Run only one Scherzo instance per tracker project and canonical workspace root.
- For multi-repo or multi-instance operations, rely on the built-in poll jitter; it spreads recurring tracker requests around `polling.interval_ms` and logs `next_poll_scheduled` with the effective next delay.

Set `agent.max_concurrent_agents: 0` to pause new dispatch while keeping daemon reload and reconciliation alive.

## 13. Observe and control with `scherzoctl`

From another terminal in the same repo/environment:

```sh
direnv exec . scripts/scherzoctl ping
direnv exec . scripts/scherzoctl ps
direnv exec . scripts/scherzoctl ps --json
direnv exec . scripts/scherzoctl session <session-id>
direnv exec . scripts/scherzoctl events --pretty <session-id>
direnv exec . scripts/scherzoctl attach <session-id>
```

Useful controls:

```sh
direnv exec . scripts/scherzoctl pause
direnv exec . scripts/scherzoctl resume
direnv exec . scripts/scherzoctl reload
direnv exec . scripts/scherzoctl retry ABC-123
direnv exec . scripts/scherzoctl park ABC-123 --reason "operator inspection" --yes
direnv exec . scripts/scherzoctl unpark ABC-123
direnv exec . scripts/scherzoctl prompt <session-id> "summarize current progress"
direnv exec . scripts/scherzoctl stop-after-turn <session-id> --yes
direnv exec . scripts/scherzoctl abort <session-id> --yes
```

Use `ps --json` and `session --json` when scripting or when an agent is acting as an operator. For mutating commands, use exact task/issue ids, session ids, and request ids from JSON inspection. See [workflow recovery](runbooks/workflow-recovery.md) for retained artifacts, recovery status, cleanup, and unsupported local state handling.

## 14. Adaptation checklist and troubleshooting

### Checklist

- [ ] Scherzo command is installed and `scherzo --version` works.
- [ ] `LINEAR_API_KEY` is set outside committed config.
- [ ] `.scherzo/scherzo.yaml` has the right tracker adapter kind, Linear project slug, states, and dispatch states.
- [ ] Workspace profile starts with `noop` for artifact-only workflows or a reviewed `jj`/custom driver for implementation workflows.
- [ ] Custom workspace driver, if any, passes `describe --json` and follows the workspace driver spec.
- [ ] Workflow labels exist in the tracker and match `routing.workflows` suffixes.
- [ ] One workflow YAML DAG exists and has a matching prompt template.
- [ ] Command validation steps run local deterministic checks.
- [ ] Structured-output schemas and validators are repository-local, reviewed, and declared with correct paths.
- [ ] Handoff comments/state moves are disabled until `doctor` and `--once` are understood, then enabled deliberately.
- [ ] `scherzo doctor` passes, or every warning/failure has an accepted explanation.
- [ ] A low-risk task succeeds with `--once`.
- [ ] Daemon mode starts with low concurrency and an operator watching `scherzoctl ps`/`attach`.
- [ ] Scheduled jobs are left disabled until normal task workflows are stable; see [scheduled jobs](runbooks/scheduled-jobs.md) when needed.

### Troubleshooting quick map

| Problem | First command | Notes |
| --- | --- | --- |
| Unsure if config loads | `scherzo doctor --check workflow-config .scherzo/scherzo.yaml` | Checks YAML, routed workflows, prompts, and local config shape |
| Unsure if the tracker board matches config | `scherzo doctor --check tracker-contract .scherzo/scherzo.yaml` | Requires API key and configured project slug |
| No tasks dispatch | `scherzo doctor --check tracker-smoke .scherzo/scherzo.yaml` | Check project, dispatch state, active state, terminal state, and workflow labels |
| Driver problem | `<driver> describe --json` | Then run the relevant driver lifecycle/capability command by hand |
| Upgrade or breaking-change diagnostic | `scherzo doctor .scherzo/scherzo.yaml` or `scherzoctl state status --root <workspace-root>` | Follow the [upgrade policy](runbooks/upgrades.md) and any specific runbook named by the diagnostic |
| jj workspace problem | `jj status` and driver env review | Verify base, remote, fetch policy, and publish remote before daemon mode |
| Agent cannot start | `pi --mode rpc --no-session --rpc-message-updates off` | Fix `pi` install, model/provider credentials, or `pi.command` |
| Structured output rejected | Read retained step diagnostics and schema validator stderr | Final-response source must be one JSON document; command validators should print concise stderr |
| Daemon appears stuck | `scherzoctl ps`, `scherzoctl session <id>`, `scherzoctl events --pretty <id>` | Use `attach` for live output and UI requests |
| Need to stop safely | `scherzoctl stop-after-turn <id> --yes` or Ctrl-C the `scherzo-start` terminal | Use `abort` only when you accept the interrupted-run implications |
| Recovery/cleanup confusion | `scherzoctl ps --json` and [workflow recovery runbook](runbooks/workflow-recovery.md) | Keep live worker status separate from durable recovery status |

If a workflow starts to require repository-specific policy that does not fit config, prompts, validators, or workflow YAML, prefer a custom workspace driver or local validation script over embedding more shell in every workflow step.
