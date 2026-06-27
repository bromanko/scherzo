# Getting started with Scherzo

This guide is for humans and coding agents adapting Scherzo to another repository. It starts with the smallest useful `.scherzo/scherzo.yaml`, adds one workflow, and ends with a cautious `--once` run against one tracker task. Linear is the production tracker adapter today, so the concrete task examples in this guide are Linear issues.

Keep this guide practical and use the normative specs when you need exact contracts:

- [Simplified Scherzo YAML v1 schema](specs/SCHERZO_YAML_SIMPLIFIED_V1.md)
- [Workspace Driver Specification](specs/WORKSPACE_DRIVER_SPEC.md)
- [Tracker Adapter Specification](specs/TRACKER_ADAPTER_SPEC.md)
- [Structured Output Validator Specification](specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md)

If you are updating an older config, start with the [simplified YAML migration guide](runbooks/simplified-yaml-migration.md) before copying examples from this page.

## Recommended adaptation order

Adapt Scherzo in this order so each layer can be checked before the next one adds risk:

1. minimal `.scherzo/scherzo.yaml`
2. one workflow route and matching tracker label
3. workflow DAG
4. prompt templates
5. workspace driver policy
6. structured output schemas / validators, if needed
7. task updates and tracker comments, if needed
8. schedules, if needed

## 1. Prerequisites

Before configuring Scherzo for a repository, collect these inputs.

### Tracker adapter and Linear

- A Linear API key available as `LINEAR_API_KEY` or another environment variable referenced by config.
- The Linear project slug or non-overlapping `tracker.linear.tasks_from` project scope Scherzo should poll (`tasks_from.project` for one project, `tasks_from.projects` for an explicit list).
- The state names used for dispatch and lifecycle decisions, for example `Todo`, `In Progress`, `In Review`, `Done`, `Canceled`, and `Duplicate`.
- The workflow labels you want to route, usually labels with the `workflow:` prefix such as `workflow:research` and `workflow:implementation`.
- Whether Scherzo should post tracker comments or move states after runs. Start with task updates disabled until `doctor` and a single `--once` run are understood.

See the [Tracker Adapter Specification](specs/TRACKER_ADAPTER_SPEC.md) for the normative adapter contract and [Tracker adapters](runbooks/tracker-adapters.md) for operator guidance.

### Agent runtime and model/provider credentials

Scherzo executes agent steps through `pi`. The Nix-packaged `scherzo` wrapper and the source checkout direnv/devenv shell provide Scherzo's pinned `pi` fork. `scherzo-start` remains as a deprecated compatibility alias for older daemon scripts. Non-Nix deployments should install a compatible `pi` or otherwise put it on `PATH`. Choose a model/provider, and make provider credentials available in the environment that will run Scherzo.

The minimal config uses the default `pi` executable. Add `agents.model`, `agents.thinking`, or an `agents.runtime` block only after you know the provider and runtime settings that work in your `pi` installation.

### Scherzo command and local tools

Use either the packaged Scherzo command or a source checkout:

- Packaged usage exposes `scherzo`, `scherzoctl`, `scherzo-workspace-noop`, and `scherzo-workspace-jj` on `PATH`; it also keeps deprecated `scherzo-start` for compatibility.
- Source-checkout usage normally runs `direnv exec . gleam run -- ...` for non-daemon commands and `direnv exec . scripts/scherzoctl ...` for local control. For Ctrl-C-friendly foreground daemon testing from the source tree, prefer the packaged launcher via `nix run .#scherzo -- ...`; the `direnv exec . scherzo-start ...` helper is the compatibility fallback.

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

Start with the minimal config. It names the Linear project and the workflow files Scherzo may route to:

```yaml
# .scherzo/scherzo.yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1

tracker:
  linear:
    tasks_from:
      project: YOUR_LINEAR_PROJECT_SLUG

workflows:
  getting-started: workflows/getting-started.yaml
```

Defaults make this useful for a cautious first pass: Scherzo reads the API key from `LINEAR_API_KEY`, polls `Todo` tasks every `30s`, derives the `workflow:getting-started` label from the `workflows` map, uses the built-in `noop` workspace driver, runs one `pi`-backed agent at a time, and does not move or comment on tasks after runs.

After the minimal config loads, expand deliberately. This starter keeps task updates disabled but makes the important defaults explicit:

```yaml
# .scherzo/scherzo.yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1

tracker:
  linear:
    tasks_from:
      project: YOUR_LINEAR_PROJECT_SLUG
    api_key_env: LINEAR_API_KEY
    endpoint: https://api.linear.app/graphql
    check_setup: true
    labels:
      support: [needs-workflow, needs-clarification]
  states:
    ready: [Todo]
    active: [Todo, In Progress]
    terminal: [Done, Canceled, Cancelled, Duplicate]
  polling:
    every: 30s

workflows:
  getting-started: workflows/getting-started.yaml

task_routing:
  labels:
    prefix: "workflow:"
    require_exactly_one: true
    on_invalid:
      state: Triage
      comment: true

control:
  # Local scherzoctl mutating command timeout. Defaults to 60s.
  command_timeout: 60s

workspace:
  # Relative to .scherzo/scherzo.yaml, so this becomes .scherzo/workspaces/.
  root: workspaces
  driver: noop

agents:
  concurrency: 1
  max_turns: 1
  sessions_per_task: 2
  runtime:
    type: pi
    sessions: ephemeral
    turn_timeout: 1h
    read_timeout: 5s
    stall_timeout: 5m
    auto_retry: true
    ui_requests: cancel
    compatibility_check: true

ui_server:
  enabled: false
  # Leave this off for local-only operation. Pair without changing YAML with:
  #   scherzo connect --pairing-token <pair_...> --server-url <https-url> --name "Project Foo"
  # Or explicitly pair and write non-secret ui_server fields with:
  #   scherzo connect --pairing-token <pair_...> --server-url <https-url> --credential-ref work-laptop --name "Project Foo" --activate
  # --activate is explicit so version-controlled configs and loopback development URLs are not mutated accidentally.
  # Then set or review the non-secret credential_ref and optional daemon_label here. The daemon loads the durable
  # credential from the owner-only credential store, sends daemon_hello /
  # heartbeat / daemon_state metadata with stable state (host, version, agentSlots)
  # and heartbeat event payloads over the outbound UI connection, retries temporary
  # outages in the background, and keeps local scherzoctl control as the
  # fallback. Command/result bridge work remains disabled by default; when enabled,
  # server_command frames receive correlated command_result replies for supported
  # operator commands such as pause, resume, and reload.
  # endpoint: https://ui.example.test
  # credential_ref: work-laptop
  # daemon_label: Project Foo

task_updates:
  enabled: false
  comment_on: [claim, success, failure, park]
  result:
    on_success: comment
    max_chars: 8000
```

Path rule of thumb:

- Top-level `workflows` paths are relative to the config file directory. In `.scherzo/scherzo.yaml`, `workflows/getting-started.yaml` means `.scherzo/workflows/getting-started.yaml`.
- Prompt paths are relative to the workflow YAML file.
- JSON Schema validator paths are repository-relative, for example `schemas/implementation_plan.schema.json`.
- Command validator `argv` entries run from `working_directory`, usually `repository` for scripts under `scripts/`.
- Packaged workspace drivers can be named directly, for example `noop` or `jj`. Repository-local drivers can use a named `workspace.drivers.<name>` entry with `type: custom` and `command: scripts/...`.

### YAML editor schema support

Scherzo now ships checked-in public YAML schemas at `schemas/scherzo.config.v1.schema.json` and `schemas/scherzo.workflow.v1.schema.json`. Treat those local files as the authoritative editor/CLI artifacts for now, even though their `$id` values are stable public identifiers.

You can attach a schema inline with a yaml-language-server modeline comment. For `.scherzo/scherzo.yaml` from the repository root:

```yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1
tracker:
  linear:
    tasks_from:
      project: YOUR_LINEAR_PROJECT_SLUG
```

For a workflow file such as `.scherzo/workflows/getting-started.yaml`:

```yaml
# yaml-language-server: $schema=../../schemas/scherzo.workflow.v1.schema.json
version: 1
id: getting-started
steps:
  - id: draft
    prompt: prompts/getting-started.md
```

Or configure yaml-language-server / VS Code once for the repository:

```json
{
  "yaml.schemas": {
    "./schemas/scherzo.config.v1.schema.json": [
      ".scherzo/scherzo.yaml",
      "scherzo.yaml"
    ],
    "./schemas/scherzo.workflow.v1.schema.json": [
      ".scherzo/workflows/*.yaml",
      "workflows/*.yaml"
    ]
  }
}
```

Use these schemas for editor completion, hover text, and structural validation. Scherzo's runtime parser remains the final authority for workflow graph semantics such as dependency existence, cycle detection, workspace lineage, and contract cross-reference checks.

## 4. Configure Linear and task routing

### Project, API key, and states

Set `tracker.linear.tasks_from.project` to the Linear project slug Scherzo should poll, or `tracker.linear.tasks_from.projects` to poll an explicit list of projects:

```yaml
tracker:
  linear:
    tasks_from:
      project: scherzo-core
```

```yaml
tracker:
  linear:
    tasks_from:
      projects: [scherzo-core, scherzo-bugs]
```

`tracker.linear.project: scherzo-core` remains accepted as compatibility syntax for the single-project form and doctor reports the canonical `tasks_from.project` predicate it desugars to, but new configs should prefer `tasks_from`. See [docs/specs/TRACKER_LINEAR_TASKS_FROM.md](specs/TRACKER_LINEAR_TASKS_FROM.md) for label-narrowed and boolean-composed scopes. Keep the API key in the environment rather than committing a secret:

```sh
export LINEAR_API_KEY=lin_api_...
```

Use `tracker.states.ready` for task states Scherzo may pick up for initial dispatch. Put in-progress states in `tracker.states.active` so retries and operator controls can reason about live work. Put completed or abandoned states in `tracker.states.terminal` so Scherzo ignores those tasks.

### Workflow labels and task routing

With this workflow map:

```yaml
workflows:
  getting-started: workflows/getting-started.yaml
  implementation: workflows/implementation.yaml
```

With the current production adapter, Scherzo derives these workflow labels on Linear tasks by default:

- `workflow:getting-started`
- `workflow:implementation`

A task labeled `workflow:getting-started` routes to `.scherzo/workflows/getting-started.yaml`, and a task labeled `workflow:implementation` routes to `.scherzo/workflows/implementation.yaml`.

Use `task_routing.labels` only when you need to override the defaults or configure invalid-routing behavior:

```yaml
task_routing:
  labels:
    prefix: "workflow:"
    require_exactly_one: true
    on_invalid:
      state: Triage
      comment: true
```

With `require_exactly_one: true`, Scherzo rejects tasks with no workflow label or multiple workflow labels.

### Linear setup checks

Set `tracker.linear.check_setup: true` once the project scope, states, and labels exist on the board. `doctor --check tracker-contract` then verifies that the Linear board matches config. Expected workflow labels are derived from top-level `workflows`, support labels come from `tracker.linear.labels.support`, and lifecycle states come from `tracker.states`, `task_routing.labels.on_invalid.state`, and `task_updates.states`.

```yaml
tracker:
  linear:
    tasks_from:
      project: YOUR_LINEAR_PROJECT_SLUG
    check_setup: true
    labels:
      support: [needs-workflow, needs-clarification]
  states:
    ready: [Todo]
    active: [Todo, In Progress]
    terminal: [Done, Canceled, Cancelled, Duplicate]

task_routing:
  labels:
    on_invalid:
      state: Triage
      comment: true
```

### Task update policy

Keep task updates disabled for the first `doctor` runs and any dry-run-like exploration:

```yaml
task_updates:
  enabled: false
```

Before daemon mode, decide whether Scherzo should post tracker comments, move states, include result text inline, or attach the result as a Linear comment file:

```yaml
task_updates:
  enabled: true
  states:
    claim: In Progress
    success: In Review
    no_review_success: Done
    failure: Triage
    partial_success: Triage
  comment_on: [claim, success, failure, park]
  result:
    on_success: attachment
    max_chars: 20000
```

For long-running daemon operation, use task updates or a manual board process that prevents successful tasks from remaining eligible for dispatch forever. See [Linear completion states](runbooks/linear-completion-states.md) for review-oriented state policy.

## 5. Choose workspace behavior

Workspace drivers are trusted operator policy under `workspace.driver` and `workspace.drivers`. A workflow may select a named driver and require driver capabilities, but workflow YAML cannot define the shell command that creates or removes workspaces.

### `noop` for research and artifact-only workflows

Use the built-in no-op driver when a workflow should produce artifacts or comments without modifying a VCS-backed implementation workspace. Missing `workspace.driver` also defaults to `noop`.

```yaml
workspace:
  root: workspaces
  driver: noop
```

A workflow can make that choice explicit with the workflow workspace block:

```yaml
version: 1
id: research
workspace:
  driver: noop
  requires: [assert-only]
steps:
  - id: research
    prompt: prompts/research.md
    run_in: main
```

### `jj` for implementation workflows

Use the built-in jj driver when implementation steps need isolated workspaces and later publication. Choose the base, fetch, and publish policy deliberately for your repository.

```yaml
workspace:
  root: workspaces
  driver: isolated
  drivers:
    isolated:
      type: jj
      timeout: 60s
      # Local/offline starter policy: use the current jj change as base and
      # do not fetch. Replace this with your real base/remote policy before
      # unattended implementation work.
      base: "@"
      fetch_base: false
      # Examples for remote-backed policy:
      # remote: upstream
      # base_branch: trunk
      # publish_remote: origin
      # github_repo: your-org/your-repo
```

Implementation workflows often require capabilities:

```yaml
version: 1
id: implementation
workspace:
  driver: isolated
  requires: [status, diff, changed-files, baseline, refresh-base, publish-commit-stack]
```

### Custom workspace drivers

Provide a custom driver when neither bundled driver matches your repository. Typical reasons include a different VCS, a monorepo-specific checkout process, custom artifact assertions, or a publication path that does not use the bundled jj/gh assumptions.

Configure the trusted command in the orchestrator config:

```yaml
workspace:
  root: workspaces
  driver: myrepo
  drivers:
    myrepo:
      type: custom
      command: scripts/scherzo-workspace-myrepo
      timeout: 60s
```

Then let workflows select the driver name and require capabilities:

```yaml
workspace:
  driver: myrepo
  requires: [status, diff, changed-files]
```

A custom driver must implement discovery with `<driver> describe --json`, the lifecycle protocol, and any advertised capabilities. See the [Workspace Driver Specification](specs/WORKSPACE_DRIVER_SPEC.md) for the exact command, JSON, exit-code, environment, and path-safety contract.

## 6. Add a workflow route

Add one top-level `workflows` key per workflow YAML file:

```yaml
workflows:
  getting-started: workflows/getting-started.yaml
  implementation: workflows/implementation.yaml
  research: workflows/research.yaml
```

Create matching workflow labels in the tracker. With the Linear adapter and the default prefix, these are Linear issue labels:

- `workflow:getting-started`
- `workflow:implementation`
- `workflow:research`

Start with one route and one label. Add more after `doctor` and `--once` are passing.

If you later copy Scherzo's packaged ExecPlan workflows, keep the canonical workflow keys in the same top-level map:

```yaml
workflows:
  execplan: workflows/execplan.yaml
  execplan-revision: workflows/execplan-revision.yaml
  execplan-implementation: workflows/execplan-implementation.yaml
```

Do not use retired `-v2` ExecPlan workflow names or old nested routing blocks.

## 7. Add a workflow YAML DAG

Create `.scherzo/workflows/getting-started.yaml`:

```yaml
# yaml-language-server: $schema=../../schemas/scherzo.workflow.v1.schema.json
version: 1
id: getting-started
description: Draft a small plan and verify that the expected artifact exists.
concurrency: 1

workspace:
  driver: noop

steps:
  - id: draft_plan
    prompt: prompts/getting-started.md
    run_in: main

  - id: validate_plan_file
    depends_on: [draft_plan]
    run: |
      set -eu
      test -s implementation-plan.md
      printf 'implementation-plan.md exists and is non-empty\n'
    timeout: 30s
    run_in: main
```

Important workflow rules:

- `id` should match the key in top-level `workflows`.
- `depends_on` forms a DAG; cycles are rejected.
- A step with `prompt` is an agent step; a step with `run` is a command step.
- Prompt paths are relative to the workflow YAML file and must stay inside that workflow directory.
- Steps sharing the same logical workspace are serialized.
- Different logical workspaces may run concurrently up to workflow `concurrency` and `agents.concurrency`.
- Agent steps inherit project-level `agents` settings, then workflow-level `model` / `thinking`, unless the step overrides them.
- Command steps run shell commands in the prepared workspace.
- `run_in` selects the workspace lane. Omit it only when the default `main` lane is intentional.
- Use `recovery`, not the old recovery spelling, when configuring bounded step remediation.

A derived lane uses the map form:

```yaml
run_in:
  name: review
  from: main
```

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

With `source.type: final_response`, the final assistant response must be exactly one JSON document. Do not wrap it in Markdown fences or add commentary. Your prompt should say that explicitly:

```md
When finished, respond only with JSON matching the implementation_plan schema.
Do not include Markdown fences, prose, or extra keys unless the schema allows them.
```

Add the block to an agent step when you want Scherzo to retain and validate the JSON artifact:

```yaml
- id: draft_plan_json
  prompt: prompts/getting-started-json.md
  run_in: main
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

For semantic checks that JSON Schema cannot express, add a command validator after the JSON Schema validator. Command validators receive the admitted JSON payload on stdin and accept or reject it by exit status.

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
    timeout: 30s
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

The default doctor run loads config, workflows, and prompts; checks the tracker contract and read-only tracker access; acquires the local instance lock; exercises workspace lifecycle; and launches a `pi` compatibility probe without sending a task prompt.

Use the read-only subset when you only want config and tracker validation:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo doctor \
  --check workflow-config \
  --check tracker-contract \
  --check tracker-smoke \
  .scherzo/scherzo.yaml
```

Use `tracker-contract` and `tracker-smoke`; the old Linear-named check aliases are retired.

List available checks:

```sh
direnv exec . scherzo doctor --list-checks
```

Common doctor failures:

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| Config file not found | Running from the wrong directory or no config path passed | Run from the target repo or pass `.scherzo/scherzo.yaml` |
| Linear auth failure | Missing/invalid `LINEAR_API_KEY` | Export a valid key in the Scherzo process environment |
| Project not found | Wrong `tracker.linear.tasks_from` project slug | Copy the slug from the Linear project URL/config |
| State/label mismatch | Board does not have configured states or labels | Create labels/states or adjust `tracker`, `workflows`, `task_routing`, and `task_updates` config |
| No workflow route | Task label suffix does not match a top-level `workflows` key | Add the workflow YAML and workflow key or fix the Linear label |
| Multiple workflow labels | `task_routing.labels.require_exactly_one: true` and task has more than one | Leave exactly one `workflow:*` label on the task |
| Workspace driver discovery fails | Driver not on `PATH`, not executable, or invalid `describe --json` | Fix `workspace.driver` / `workspace.drivers` or implement the driver spec |
| Workspace lifecycle fails | Driver cannot create/remove scratch workspace or selected jj base is unavailable | Fix driver env, VCS state, base branch, or local permissions |
| Legacy or unsupported shape | Config, workflow, driver, tracker, or local state uses an old shape | Read the diagnostic path/code and the [simplified YAML migration guide](runbooks/simplified-yaml-migration.md), [upgrade policy](runbooks/upgrades.md), or linked specific runbook |
| Pi probe fails | `pi` missing, provider credentials missing, or runtime config incompatible | Run `pi --mode rpc --no-session --rpc-message-updates off` manually and fix credentials/config |
| Prompt/schema path missing | Paths are relative to different roots | Check workflow-relative prompt paths and repository-relative schema paths |

## 11. Run one task with `--once`

After doctor passes, create or choose one low-risk task. With the current production adapter this means a Linear issue:

- State is in `tracker.states.ready`, for example `Todo`.
- Project matches `tracker.linear.tasks_from` (or compatibility `tracker.linear.project`).
- Exactly one workflow label is present, for example `workflow:getting-started`.
- The task description is safe for the configured workflow and workspace driver.

Run one eligible task and exit:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . scherzo --once .scherzo/scherzo.yaml
```

Source-checkout equivalent:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml
```

Inspect the workspace root, retained artifacts, command output, and tracker comments before moving to daemon mode. If task updates are disabled, manually move or relabel the task so it does not remain eligible for repeated dispatch.

## 12. Start daemon mode with `scherzo`

Use the packaged `scherzo` command for interactive foreground daemon mode. When the arguments select daemon mode, the launcher runs the existing signal-translation wrapper so terminal Ctrl-C/SIGINT becomes SIGTERM and Scherzo reaches its graceful daemon shutdown path.

```sh
LINEAR_API_KEY=lin_api_... scherzo .scherzo/scherzo.yaml
```

For source-checkout daemon testing without installing the package, use the same packaged launcher through Nix:

```sh
LINEAR_API_KEY=lin_api_... nix run .#scherzo -- .scherzo/scherzo.yaml
```

`gleam run -- .scherzo/scherzo.yaml` is still useful for low-level development, but direct Ctrl-C may terminate abruptly. Scherzo's current Erlang/Gleam signal FFI installs a SIGTERM handler for daemon lifecycle cleanup; it does not currently install or own a SIGINT handler, and adding native SIGINT handling is out of scope. The packaging wrapper remains necessary to translate interactive Ctrl-C into the already-tested SIGTERM path. `scherzo-start .scherzo/scherzo.yaml` remains available as a deprecated compatibility alias for existing scripts.

For systemd, launchd, or another service manager, use the same `scherzo .scherzo/scherzo.yaml` command. Service managers normally stop services with SIGTERM, so they can rely on Scherzo's daemon shutdown path directly.

Before daemon mode:

- Keep `agents.concurrency: 1` until the workflow is proven.
- Enable task updates or establish a manual policy that removes completed tasks from ready states.
- Keep an operator watching the first several runs.
- Run only one Scherzo instance per non-overlapping Linear task scope/root.
- Be especially careful with multi-project, `or`, and label-narrowed `tasks_from` scopes because two predicates can overlap even when their config does not look identical at a glance; `scherzo doctor --check tracker-scope` prints the canonical scope and static overlap warnings it can infer.
- For multi-repo or multi-instance operations, rely on the built-in poll jitter; it spreads recurring tracker requests around `tracker.polling.every` and logs `next_poll_scheduled` with the effective next delay.

Set `agents.concurrency: 0` to pause new dispatch while keeping daemon reload and reconciliation alive.

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
direnv exec . scripts/scherzoctl retry-step run:<run-id> --step <step-id>
direnv exec . scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run
direnv exec . scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --yes
direnv exec . scripts/scherzoctl park ABC-123 --reason "operator inspection" --yes
direnv exec . scripts/scherzoctl unpark ABC-123
direnv exec . scripts/scherzoctl prompt <session-id> "summarize current progress"
direnv exec . scripts/scherzoctl stop-after-turn <session-id> --yes
direnv exec . scripts/scherzoctl abort <session-id> --yes
```

Use `ps --json` and `session --json` when scripting or when an agent is acting as an operator. JSON responses include non-secret target context such as the resolved control file path and daemon workspace root so you can catch wrong-daemon targeting. `retry` returns after the daemon accepts the retry intent; inspect sessions, events, and ledger-backed failure reports for later claim, dispatch, or run failures. Recovery metadata for workflow child sessions now includes parent run/step/attempt details plus orphan cleanup hints. For mutating commands, use exact task/issue ids, session ids, and request ids from JSON inspection. The daemon waits up to `control.command_timeout` for local mutating command results before `scherzoctl` reports `command_timeout`; the default is `60s`, and longer tracker/ledger-heavy operator actions may need an explicit config value. Relative `--control-file`, `SCHERZO_CONTROL_FILE`, and `--root` paths are resolved from the directory where `scripts/scherzoctl` was invoked; direct `gleam run -- ctl` resolves them from its process working directory. Tracker comments are not an operator command transport; with the current production adapter that means old Linear command comments are ignored by Scherzo. See [workflow recovery](runbooks/workflow-recovery.md) for retained artifacts, recovery status, cleanup, and unsupported local state handling.

## 14. Adaptation checklist and troubleshooting

### Checklist

- [ ] Scherzo command is installed and `scherzo --version` works.
- [ ] `LINEAR_API_KEY` is set outside committed config.
- [ ] `.scherzo/scherzo.yaml` has `tracker.linear.tasks_from`, the right task states, and a top-level `workflows` map.
- [ ] Workspace driver starts with built-in `noop` for artifact-only workflows or a reviewed `jj`/custom driver for implementation workflows.
- [ ] Custom workspace driver, if any, passes `describe --json` and follows the workspace driver spec.
- [ ] Workflow labels exist in the tracker and match the top-level `workflows` keys.
- [ ] One workflow YAML DAG exists and has a matching prompt template.
- [ ] Command validation steps run local deterministic checks.
- [ ] Structured-output schemas and validators are repository-local, reviewed, and declared with correct paths.
- [ ] Task comments/state moves are disabled until `doctor` and `--once` are understood, then enabled deliberately.
- [ ] `scherzo doctor` passes, or every warning/failure has an accepted explanation.
- [ ] A low-risk task succeeds with `--once`.
- [ ] Daemon mode starts with low concurrency and an operator watching `scherzoctl ps`/`attach`.
- [ ] Scheduled jobs are left disabled until normal task workflows are stable; see [scheduled jobs](runbooks/scheduled-jobs.md) when needed.

### Troubleshooting quick map

| Problem | First command | Notes |
| --- | --- | --- |
| Unsure if config loads | `scherzo doctor --check workflow-config .scherzo/scherzo.yaml` | Checks YAML, routed workflows, prompts, and local config shape |
| Unsure if the tracker board matches config | `scherzo doctor --check tracker-contract .scherzo/scherzo.yaml` | Requires API key and configured project slug |
| No tasks dispatch | `scherzo doctor --check tracker-smoke .scherzo/scherzo.yaml` | Check project, ready state, active state, terminal state, and workflow labels |
| Driver problem | `<driver> describe --json` | Then run the relevant driver lifecycle/capability command by hand |
| Upgrade or breaking-change diagnostic | `scherzo doctor .scherzo/scherzo.yaml` or `scherzoctl state status --root <workspace-root>` | Follow the [simplified YAML migration guide](runbooks/simplified-yaml-migration.md), [upgrade policy](runbooks/upgrades.md), or any specific runbook named by the diagnostic |
| jj workspace problem | `jj status` and driver env review | Verify base, remote, fetch policy, and publish remote before daemon mode |
| Agent cannot start | `pi --mode rpc --no-session --rpc-message-updates off` | Fix `pi` install, model/provider credentials, or `agents.runtime` config |
| Structured output rejected | Read retained step diagnostics and schema validator stderr | Final-response source must be one JSON document; command validators should print concise stderr |
| Daemon appears stuck | `scherzoctl ps`, `scherzoctl session <id>`, `scherzoctl events --pretty <id>` | Use `attach` for live output and UI requests |
| Need to stop safely | `scherzoctl stop-after-turn <id> --yes` or Ctrl-C the foreground `scherzo` terminal | Use `abort` only when you accept the interrupted-run implications |
| Recovery/cleanup confusion | `scherzoctl ps --json` and [workflow recovery runbook](runbooks/workflow-recovery.md) | Keep live worker status separate from durable recovery status |

If a workflow starts to require repository-specific policy that does not fit config, prompts, validators, or workflow YAML, prefer a custom workspace driver or local validation script over embedding more shell in every workflow step.
