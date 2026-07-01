# Migrating to simplified Scherzo YAML

Use this guide when an existing repository config was written for the older public YAML shape. The normative reference is the [Simplified Scherzo YAML v1 schema](../specs/SCHERZO_YAML_SIMPLIFIED_V1.md); when in doubt, follow that spec.

Scherzo keeps `version: 1`, but the public shape is smaller and more task-oriented. Old keys should be removed rather than kept beside the new keys.

## Minimal target

A minimal current config needs only the Linear project and the workflow map:

```yaml
version: 1

tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG

workflows:
  research: workflows/research.yaml
```

Defaults read the API key from `LINEAR_API_KEY`, dispatch `Todo` tasks with `workflow:<name>` labels, use the built-in `noop` workspace driver, run one `pi`-backed agent at a time, and leave tracker task updates disabled.

## Before and after: root config

Before, a typical config spread routing, task updates, agent runtime, and workspace profile settings across several top-level sections:

```yaml
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

routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    research: workflows/research.yaml
    implementation: workflows/implementation.yaml

workspace:
  root: workspaces
  default_profile: isolated
  profiles:
    isolated:
      driver:
        command: scripts/scherzo-workspace-jj
        timeout_ms: 60000

agent:
  max_concurrent_agents: 2
  max_turns: 1
  max_sessions_per_issue: 2

pi:
  command: "pi --mode rpc --no-session --rpc-message-updates off"
  turn_timeout_ms: 3600000
  read_timeout_ms: 5000
  stall_timeout_ms: 300000
  ui_request_policy: cancel
  compatibility_probe: true

handoff:
  enabled: true
  comment_on_claim: true
  comment_on_failure: true
  success_state_id: <linear-done-state-id>
  failure_state_id: <linear-triage-state-id>
  attach_result_on_success: true
  result_max_chars: 8000

linear_commands:
  enabled: true
```

After, move the same policy into the simplified sections:

```yaml
version: 1

tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG
    api_key_env: LINEAR_API_KEY
    endpoint: https://api.linear.app/graphql
    check_setup: true
  states:
    ready: [Todo]
    active: [Todo, In Progress]
    terminal: [Done, Canceled, Cancelled, Duplicate]
  polling:
    every: 30s

workflows:
  research: workflows/research.yaml
  implementation: workflows/implementation.yaml

task_routing:
  labels:
    prefix: "workflow:"
    require_exactly_one: true
    on_invalid:
      state: Triage
      comment: true

workspace:
  root: workspaces
  driver: isolated
  drivers:
    isolated:
      type: jj
      timeout: 60s

agents:
  concurrency: 2
  max_turns: 1
  sessions_per_task: 2
  runtime:
    type: pi
    sessions: ephemeral
    turn_timeout: 1h
    read_timeout: 5s
    stall_timeout: 5m
    ui_requests: cancel
    compatibility_check: true

task_updates:
  enabled: true
  states:
    claim: In Progress
    success: In Review
    no_review_success: Done
    failure: Triage
    partial_success: Triage
  comment_on: [claim, failure]
  result:
    on_success: attachment
    max_chars: 8000
```

There is no replacement for `linear_commands` or `remote_commands`. Use local/operator controls through `scherzoctl`, for example `scherzoctl task retry`, `scherzoctl park`, `scherzoctl prompt`, and `scherzoctl abort`.

## Before and after: workflow YAML

Before, workflow files used profile-oriented workspace fields, numeric millisecond timeouts, and older recovery names:

```yaml
version: 1
id: implementation
max_parallel_steps: 2
workspace_profile: isolated
workspace_capabilities: [status, diff, changed-files]
recover:
  max_attempts: 1
  prompt: prompts/recover.md
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    workspace: main

  - id: validate
    kind: command
    depends_on: [implement]
    run: direnv exec . gleam test
    timeout_ms: 600000
    workspace: main
```

After, use the current workflow vocabulary:

```yaml
version: 1
id: implementation
concurrency: 2

workspace:
  driver: isolated
  requires: [status, diff, changed-files]

recovery:
  attempts: 1
  prompt: prompts/recover.md

steps:
  - id: implement
    prompt: prompts/implement.md
    run_in: main

  - id: validate
    depends_on: [implement]
    run: direnv exec . gleam test
    timeout: 10m
    run_in: main
```

A step with `prompt` is inferred to be an agent step, and a step with `run` is inferred to be a command step. Use `run_in` for the workspace lane. Use duration strings such as `30s`, `10m`, and `1h` instead of bare millisecond numbers.

## Common field moves

| Old key | Current key or behavior |
| --- | --- |
| `tracker.kind` | Removed; configure `tracker.linear` directly. |
| `tracker.credentials.api_key_env` | `tracker.linear.api_key_env` |
| `tracker.linear.project_slug` | `tracker.linear.tasks_from.project` (`tracker.linear.project` remains compatibility syntax for existing single-project configs) |
| `tracker.active_states` | `tracker.states.active` |
| `tracker.dispatch_states` | `tracker.states.ready` |
| `tracker.terminal_states` | `tracker.states.terminal` |
| `polling.interval_ms` | `tracker.polling.every` with a duration string |
| `routing.workflows` | top-level `workflows` |
| `routing.workflow_label_prefix` | `task_routing.labels.prefix` |
| `routing.require_exactly_one_workflow_label` | `task_routing.labels.require_exactly_one` |
| `linear_contract.enabled` | `tracker.linear.check_setup` |
| `linear_contract.workflow_labels` | Derived from top-level `workflows`. |
| `linear_contract.support_labels` | `tracker.linear.labels.support` |
| `workspace.default_profile` | `workspace.driver` |
| `workspace.profiles` | `workspace.drivers` |
| `workspace.profiles.<name>.driver.timeout_ms` | `workspace.drivers.<name>.timeout` |
| `agent.max_concurrent_agents` | `agents.concurrency` |
| `agent.max_turns` | `agents.max_turns` |
| `agent.max_retry_attempts` | Removed; automatic whole-workflow retries are no longer supported. |
| `agent.max_retry_backoff_ms` | Removed; automatic whole-workflow retries are no longer supported. |
| `agent.max_sessions_per_issue` | `agents.sessions_per_task` |
| top-level `pi` | `agents.runtime` |
| `pi.command` / `pi.argv` | `agents.runtime.pi.executable` and `agents.runtime.pi.args`; Scherzo owns protocol flags. |
| `pi.turn_timeout_ms` | `agents.runtime.turn_timeout` |
| `pi.read_timeout_ms` | `agents.runtime.read_timeout` |
| `pi.stall_timeout_ms` | `agents.runtime.stall_timeout` |
| `pi.ui_request_policy` | `agents.runtime.ui_requests` |
| `pi.compatibility_probe` | `agents.runtime.compatibility_check` |
| `handoff` | `task_updates` |
| `handoff.comment_on_*` | `task_updates.comment_on` |
| `handoff.*_state_id` | `task_updates.states.*` using the Linear state name, not the state id. |
| `handoff.include_result_on_success` / `handoff.attach_result_on_success` | `task_updates.result.on_success` |
| `handoff.result_max_chars` | `task_updates.result.max_chars` |
| `scheduled_jobs` | `schedules` |
| `artifact_limits` | `artifacts.limits` |
| `linear_commands` / `remote_commands` | Removed; use `scherzoctl` for operator control. |
| root `hooks` | Removed; use custom workspace drivers. |
| workflow `workspace_profile` | workflow `workspace.driver` |
| workflow `workspace_capabilities` | workflow `workspace.requires` |
| workflow `max_parallel_steps` | workflow `concurrency` |
| workflow `recover` | workflow `recovery` |
| step `workspace` | step `run_in` |
| step `timeout_ms` | step `timeout` with a duration string |

The full migration table is maintained in the [simplified YAML spec](../specs/SCHERZO_YAML_SIMPLIFIED_V1.md#migration-table).

## Migration checklist

1. Move `routing.workflows` to top-level `workflows` first. Keep the same workflow names and paths.
2. Rename tracker project and state fields under `tracker.linear`, `tracker.states`, and `tracker.polling`.
3. Replace workspace profiles with `workspace.driver` plus `workspace.drivers` entries. Use `type: noop`, `type: jj`, or `type: custom`.
4. Replace `agent` and top-level runtime settings with `agents` and `agents.runtime`.
5. Replace task comment/state policy with `task_updates`, or set `task_updates.enabled: false` while validating.
6. Remove `linear_commands`, `remote_commands`, and root `hooks` entirely.
7. Update workflow YAML: `concurrency`, `workspace.driver`, `workspace.requires`, `run_in`, `timeout`, and `recovery`.
8. Run the workflow-config doctor check before dispatching work:

   ```sh
   LINEAR_API_KEY=dummy scherzo doctor --check workflow-config .scherzo/scherzo.yaml
   ```

9. When config loads locally, make a real API key available through your normal secret path, such as an ignored env file, a secret manager, or a shell where history is disabled. The command expects `LINEAR_API_KEY` in the Scherzo process environment; do not paste real API keys into shell commands, command logs, or committed files.

   ```sh
   scherzo doctor \
     --check tracker-contract \
     --check tracker-smoke \
     .scherzo/scherzo.yaml
   ```

If a diagnostic names an old key, remove that key instead of trying to make old and new sections coexist. Diagnostics should name the old path, the replacement, and the [simplified YAML spec](../specs/SCHERZO_YAML_SIMPLIFIED_V1.md).
