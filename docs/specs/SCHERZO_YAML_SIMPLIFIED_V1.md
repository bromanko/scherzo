# Simplified Scherzo YAML v1 schema

Status: reference for the simplified public config schema. This schema keeps
`version: 1` while replacing the older public YAML shape with a smaller,
task-oriented schema. Existing users are expected to update their config files;
Scherzo should reject old keys with targeted migration messages rather than support
both schemas indefinitely.

For editor completion, hover text, and local structural validation, see the checked-in
JSON Schemas and setup examples in [docs/GETTING_STARTED.md#yaml-editor-schema-support](../GETTING_STARTED.md#yaml-editor-schema-support).

## Design goals

The root config should describe what operators want Scherzo to do, not every
internal subsystem involved in doing it. Common installs should need only a Linear
project and a workflow map. Advanced sections remain available for workspace driver
customization, agent runtime tuning, task updates, schedules, and artifact limits.

Workflow YAML should use the same vocabulary as root config: workspace driver config
lives under `workspace`, step execution lanes use `run_in`, durations are human
strings, and recovery uses the noun `recovery`.

## Minimal root config

```yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1

tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG

workflows:
  research: workflows/research.yaml
```

Defaults make this equivalent to a Linear tracker using `LINEAR_API_KEY`, polling
for `Todo` tasks every 30 seconds, routing `workflow:research` labels to the listed
workflow, using the built-in `noop` workspace driver, running one pi-backed agent at
a time, and not updating tracker tasks after runs.

## Typical implementation config

```yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1

tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG
    check_setup: true

workflows:
  research: workflows/research.yaml
  implementation: workflows/implementation.yaml

workspace:
  driver: jj

agents:
  concurrency: 4
  max_turns: 1

task_updates:
  enabled: true
  states:
    claim: In Progress
    success: In Review
    no_review_success: Done
    failure: Triage
  comment_on: [claim, failure]
  result:
    on_success: attachment
    max_chars: 20000
```

## Full root schema example

```yaml
# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json
version: 1

tracker:
  linear:
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
  driver: dogfood

  drivers:
    dogfood:
      type: jj
      remote: scherzo-agent
      base_branch: main
      fetch_base: true
      publish_remote: scherzo-agent
      github_repo: scherzo-systems/scherzo
      timeout: 1m

    custom:
      type: custom
      command: scripts/my-workspace-driver
      timeout: 1m
      env:
        FOO: bar

agents:
  concurrency:
    default: 4
    by_state:
      In Progress: 2

  max_turns: 1
  sessions_per_task: 2

  model: openai-codex/gpt-5.5:xhigh
  thinking: high

  retries:
    attempts: 2
    max_backoff: 5m

  recovery:
    attempts: 1
    prompt_char_limit: 40000

  runtime:
    type: pi
    sessions: persistent

    pi:
      executable: scripts/scherzo-pi
      args: []
      env:
        SCHERZO_PI_SESSION_PERSISTENCE: "1"

    turn_timeout: 1h
    read_timeout: 5s
    stall_timeout: 5m
    auto_retry: true
    ui_requests: operator
    ui_request_timeout: 10m
    compatibility_check: true

ui_server:
  enabled: false
  endpoint: https://ui.example.test
  credential_ref: work-laptop
  daemon_label: Project Foo

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

schedules:
  - id: github-pr-conflict-scout
    workflow: github-pr-conflict-scout
    enabled: true
    every: 15m
    overlap: skip
    catch_up: false
    on_failure:
      task:
        enabled: true
        state: Triage
        dedupe: open_task_per_schedule

artifacts:
  limits:
    command_output_chars: 20000
    template_field_chars: 8000
    workflow_summary_chars: 20000
```

## Root field reference

### `version`

Required integer. The simplified schema intentionally keeps `version: 1`.

### `tracker`

`tracker.linear.project` is the Linear project slug. `api_key_env` defaults to
`LINEAR_API_KEY`. `endpoint` defaults to `https://api.linear.app/graphql`.
`check_setup` defaults to `false`; when true, Scherzo checks that the configured
Linear project, states, and labels exist.

`tracker.states.ready` names task states Scherzo may pick up. It defaults to
`[Todo]`. `tracker.states.active` names non-terminal states Scherzo treats as
active, defaulting to `[Todo, In Progress]`. `tracker.states.terminal` names states
Scherzo will not dispatch, defaulting to `[Done, Canceled, Cancelled, Duplicate]`.

`tracker.polling.every` controls how often Scherzo checks the tracker for ready
tasks. It defaults to `30s`.

When `tracker.linear.check_setup` is true, Scherzo derives expected workflow labels
from top-level `workflows`, expected task states from `tracker.states` and
`task_updates.states`, support labels from `tracker.linear.labels.support`, and
invalid-routing state from `task_routing.labels.on_invalid.state`.

### `workflows`

Required map from workflow name to workflow YAML path. Paths are resolved relative to
the config file directory. Workflow labels are derived from these names and
`task_routing.labels.prefix`.

### `task_routing`

Defines how tracker tasks choose workflows. The current routing mechanism is labels.
`task_routing.labels.prefix` defaults to `workflow:`. `require_exactly_one` defaults
to `true`. `on_invalid.state` and `on_invalid.comment` control what Scherzo does when
a ready task has no workflow label or multiple workflow labels.

### `workspace`

`workspace.root` defaults to `workspaces` and is resolved relative to the config file.
`workspace.driver` defaults to the built-in `noop` driver. It can name a built-in
driver or a key under `workspace.drivers`.

Built-in drivers are `noop` and `jj`. Named driver configs have a `type` of `noop`,
`jj`, or `custom`.

A `type: jj` driver may use friendly fields that map to workspace driver environment
variables:

| Field | Environment variable |
| --- | --- |
| `remote` | `SCHERZO_JJ_WORKSPACE_REMOTE` |
| `base_branch` | `SCHERZO_JJ_WORKSPACE_BASE_BRANCH` |
| `base` | `SCHERZO_JJ_WORKSPACE_BASE` |
| `fetch_base` | `SCHERZO_JJ_WORKSPACE_FETCH_BASE` |
| `publish_remote` | `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE` |
| `github_repo` | `SCHERZO_GITHUB_REPO` |

A `type: custom` driver requires `command`. Driver `timeout` defaults to `1m`. `env`
adds literal environment variables for the driver process.

### `agents`

`agents.concurrency` may be a number or a map. A number is the global limit. A map may
set `default` and `by_state` limits:

```yaml
agents:
  concurrency:
    default: 4
    by_state:
      In Progress: 2
```

`max_turns`, `sessions_per_task`, `model`, and `thinking` define default agent
execution behavior. `agents.retries.attempts` and `agents.retries.max_backoff` define
agent retry behavior. `agents.recovery.attempts` and
`agents.recovery.prompt_char_limit` define context recovery behavior.

`agents.runtime.type` is required when the runtime block is present and currently only
supports `pi`. `agents.runtime.sessions` is `ephemeral` or `persistent` and defaults
to `ephemeral`.

`agents.runtime.pi.executable` defaults to `pi`. `args` are extra user arguments.
`env` adds environment variables to the pi process. Scherzo owns protocol and session
flags: RPC mode, `--session <file>` for persistent sessions, `--no-session` for
ephemeral sessions, and RPC message update behavior. User args must not contain
`--session`, `--no-session`, or `--mode`.

Runtime timeouts use duration strings: `turn_timeout`, `read_timeout`,
`stall_timeout`, and `ui_request_timeout`. `ui_requests` is one of `cancel`, `fail`,
`ignore`, or `operator`. `compatibility_check` controls whether Scherzo probes pi
compatibility at startup.

### `ui_server`

Disabled by default. `endpoint` is the remote UI server base URL and `credential_ref`
selects the owner-only durable daemon credential created by `scherzo connect`.
`daemon_label` is an optional non-secret friendly daemon name for UI display; it is
trimmed to 1-80 printable characters, allows spaces and punctuation, and rejects
newlines/control characters. `scherzo connect --name <friendly-name>` overrides the
config label for the pairing exchange.

### `task_updates`

Replaces the old handoff section. When disabled, Scherzo does not update tracker task
state or comments after runs.

`task_updates.states` names tracker states for lifecycle events: `claim`, `success`,
`no_review_success`, `failure`, and `partial_success`. `comment_on` is a list of
events to comment on; supported events are `claim`, `success`, `failure`, and `park`.

`task_updates.result.on_success` is `none`, `comment`, or `attachment`.
`task_updates.result.max_chars` limits result text included in tracker updates.

### `schedules`

Replaces `scheduled_jobs`. Each schedule names a workflow and interval:

```yaml
schedules:
  - workflow: origin-sync
    every: 15m
```

Defaults are `id` equal to `workflow`, `enabled: true`, `overlap: skip`,
`catch_up: false`, and `on_failure.task.enabled: false`.

When enabled, `on_failure.task` creates or updates a tracker task when the scheduled
run fails. The only current dedupe mode is `open_task_per_schedule`.

### `artifacts`

`artifacts.limits.command_output_chars`, `template_field_chars`, and
`workflow_summary_chars` replace the old `artifact_limits` section.

Artifact publication repository targets are configured under
`artifacts.repositories.github.<name>`:

```yaml
artifacts:
  repositories:
    github:
      docs:
        repo: scherzo-systems/scherzo
        base: main
        branch:
          strategy: stable_per_work
          template: scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}
        pull_request:
          enabled: true
          strategy: update_existing
          draft: false
```

`repo` is an `owner/repo` string. `base` is the target branch. `checkout` was removed; GitHub publication now uses workflow `mode: commit_stack` and a workspace-driver-backed publish operation rather than a Scherzo-managed repository checkout. Defaults are
`branch.strategy: stable_per_work`,
`branch.template: scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}`,
`pull_request.enabled: true`,
`pull_request.strategy: update_existing`, and `pull_request.draft: false`.
`pull_request.body_template`, when present, must be a repository-relative path.
Publication templates support interpolation variables (`{{ ... }}`) only; control
flow tags such as `{% if %}` are not accepted in this config surface.
This schema slice only parses and validates configuration; runtime publication,
GitHub mutation, durable publication state, retry commands, and migration away
from `publish-change` are deferred.

## Workflow YAML schema

Workflow files also keep `version: 1` while adopting simpler names.

```yaml
# yaml-language-server: $schema=../../schemas/scherzo.workflow.v1.schema.json
version: 1
id: implementation
description: Implement requested changes, validate, and review.
concurrency: 4

workspace:
  driver: jj
  requires: [status, diff, changed-files, baseline, refresh-base, publish-change]


steps:
  - id: implement
    prompt: prompts/implementation.md
    run_in: main

  - id: code_review
    depends_on: [implement]
    prompt: prompts/code-review.md
    run_in:
      name: code-review
      from: main

  - id: validate
    depends_on: [implement]
    run: direnv exec . gleam test
    timeout: 10m
    run_in: main
```

`kind` remains supported but should be omitted in examples when Scherzo can infer the
step kind: a step with `prompt` is an agent step, and a step with `run` is a command
step. A step with both `prompt` and `run` must be rejected unless an explicit kind
continues to disambiguate it.

`run_in` replaces step-level `workspace`. It selects the workspace lane where the step
executes. Missing `run_in` defaults to `main`. The map form supports a lane derived
from another lane:

```yaml
run_in:
  name: review
  from: main
```

`workspace.driver` selects a workflow-specific workspace driver override. If omitted,
the root `workspace.driver` is used. `workspace.requires` declares the driver
capabilities required by the workflow.

`concurrency` replaces `max_parallel_steps` and defaults to `1`.

`recovery` replaces `recover` at workflow and step level:

```yaml
recovery:
  attempts: 1
  prompt: prompts/recover-failed-step.md
  model: google/gemini-2.5-pro
```

Command step `timeout` replaces `timeout_ms` and uses a duration string.
Structured-output command validator `timeout` also replaces `timeout_ms`.
`structured_output.artifact_name`, validator `working_directory`, `depends_on`, and
other structured output fields remain unchanged.

Workflow artifact publication routes are configured under
`artifacts.publications`:

```yaml
artifacts:
  publications:
    - id: execplan_review_doc
      repository: github.docs
      required: true
      pull_request:
        title: "{{ work.identifier }} ExecPlan"
        body_template: prompts/execplan-pr-body.md
      files:
        - select:
            output: exec_plan_bundle
            entry: plan
          path: docs/plans/{{ work.identifier }}.md
```

`repository` references a root repository target as `<backend>.<name>`.
Non-empty `artifacts.publications` requires a workflow `contract.outputs` block so
selectors can be validated before dispatch. `required` defaults to `true`. Each
file route must declare `select.output`, may optionally declare `select.entry` for
aggregate-capable outputs such as
`artifact[]`, `exec_plan_bundle`, and `code_change_bundle`, and must write to a
repository-relative `path`. Branch names, PR titles, and destination paths may
use the publication template variables from the artifact publication PRD,
including `work.*`, `workflow.id`, `publication.*`, and artifact-scoped
variables such as `artifact.output`, `artifact.entry`, and the supported
publication metadata leaf `artifact.metadata.publication.destination_path` in
file paths.
Templates support interpolation variables (`{{ ... }}`) only; control flow tags
such as `{% if %}` are rejected. Unknown template variables, unsafe paths,
unsupported selector keys, and unknown contract outputs are rejected during parsing
or bundle load.

## Duration strings

Every public duration is a string with an explicit unit. Supported units are `ms`,
`s`, `m`, and `h`. Examples: `500ms`, `30s`, `5m`, `1h`. Bare numbers must be
rejected so users do not have to guess whether a value is milliseconds or seconds.

## Removed public functionality

Linear comment command controls are removed. There is no replacement for the old
`linear_commands` or `remote_commands` sections. Local and operator controls through
`scherzoctl` remain the supported control path.

Top-level `hooks` are removed. Workspace lifecycle customization belongs in workspace
drivers, especially `workspace.drivers.<name>.type: custom`.

## Migration table

| Old key | New key or behavior |
| --- | --- |
| `tracker.kind` | Removed; only `tracker.linear` is currently supported. |
| `tracker.credentials.api_key_env` | `tracker.linear.api_key_env` |
| `tracker.linear.project_slug` | `tracker.linear.project` |
| `tracker.active_states` | `tracker.states.active` |
| `tracker.dispatch_states` | `tracker.states.ready` |
| `tracker.terminal_states` | `tracker.states.terminal` |
| `polling.interval_ms` | `tracker.polling.every` |
| `routing.workflows` | top-level `workflows` |
| `routing.workflow_label_prefix` | `task_routing.labels.prefix` |
| `routing.require_exactly_one_workflow_label` | `task_routing.labels.require_exactly_one` |
| `linear_contract.enabled` | `tracker.linear.check_setup` |
| `linear_contract.workflow_labels` | Derived from top-level `workflows`. |
| `linear_contract.support_labels` | `tracker.linear.labels.support` |
| `linear_contract.required_states` | Derived from `tracker.states` and `task_updates.states`. |
| `linear_contract.handoff_state_bindings` | Derived from `task_updates.states`. |
| `linear_contract.invalid_workflow_state_id` | `task_routing.labels.on_invalid.state` by state name. |
| `linear_contract.comment_on_invalid_workflow` | `task_routing.labels.on_invalid.comment` |
| `workspace.default_profile` | `workspace.driver` |
| `workspace.profiles` | `workspace.drivers` |
| `workspace.profiles.<name>.driver.timeout_ms` | `workspace.drivers.<name>.timeout` |
| `workspace.profiles.<name>.driver.env` | `workspace.drivers.<name>.env` |
| `workspace.profiles.<name>.driver.lifecycle` | Removed from public config. |
| `agent.max_concurrent_agents` | `agents.concurrency` |
| `agent.max_concurrent_agents_by_state` | `agents.concurrency.by_state` |
| `agent.max_turns` | `agents.max_turns` |
| `agent.max_retry_attempts` | `agents.retries.attempts` |
| `agent.max_retry_backoff_ms` | `agents.retries.max_backoff` |
| `agent.max_sessions_per_issue` | `agents.sessions_per_task` |
| `agent.context_recovery_max_attempts` | `agents.recovery.attempts` |
| `agent.context_recovery_prompt_char_limit` | `agents.recovery.prompt_char_limit` |
| `pi` | `agents.runtime` |
| `pi.command` / `pi.argv` | `agents.runtime.pi.executable` and `agents.runtime.pi.args`; Scherzo owns protocol flags. |
| `pi.argv_env` | `agents.runtime.pi.env` |
| `pi.session_persistence.enabled` | `agents.runtime.sessions: persistent` |
| `pi.turn_timeout_ms` | `agents.runtime.turn_timeout` |
| `pi.read_timeout_ms` | `agents.runtime.read_timeout` |
| `pi.stall_timeout_ms` | `agents.runtime.stall_timeout` |
| `pi.ui_request_policy` | `agents.runtime.ui_requests` |
| `pi.ui_request_timeout_ms` | `agents.runtime.ui_request_timeout` |
| `pi.compatibility_probe` | `agents.runtime.compatibility_check` |
| `handoff` | `task_updates` |
| `handoff.comment_on_*` | `task_updates.comment_on` |
| `handoff.*_state_id` | `task_updates.states.*` by state name |
| `handoff.include_result_on_success` / `attach_result_on_success` | `task_updates.result.on_success` |
| `handoff.result_max_chars` | `task_updates.result.max_chars` |
| `scheduled_jobs` | `schedules` |
| `scheduled_jobs[].on_failure.linear` | `schedules[].on_failure.task` |
| `open_issue_per_job` | `open_task_per_schedule` |
| `artifact_limits` | `artifacts.limits` |
| `linear_commands` | Removed. |
| `remote_commands` | Removed. |
| root `hooks` | Removed. |
| workflow `workspace_profile` | workflow `workspace.driver` |
| workflow `workspace_capabilities` | workflow `workspace.requires` |
| workflow `max_parallel_steps` | workflow `concurrency` |
| workflow `recover` | workflow `recovery` |
| step `workspace` | step `run_in` |
| step `timeout_ms` | step `timeout` |
| structured-output validator `timeout_ms` | structured-output validator `timeout` |

## Diagnostic expectations

When Scherzo sees an old key, it should fail fast with a message naming the old key,
the replacement key, and this document. For example:

    routing.workflows was removed. Move the map to top-level workflows. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.

Diagnostics should prefer one targeted message over a cascade of secondary parse
failures.
