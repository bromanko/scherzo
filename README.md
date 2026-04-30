# Scherzo

Scherzo is a Gleam/Erlang orchestration daemon for running pi coding-agent workflows against Linear issues. It is now YAML-orchestrator / YAML-DAG only: runtime settings live in a `scherzo.yaml` orchestrator config, and issue workflows live in YAML DAG files that reference Markdown prompt templates.

Legacy Markdown runtime workflows (`WORKFLOW.md` or `.scherzo/workflows/*.md`) are no longer supported. Markdown remains supported for prompt templates only.

## Quick start

```sh
direnv allow
direnv exec . gleam test

# Optional validation before dispatching work
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe .scherzo/scherzo.yaml

# Run one eligible issue and exit
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml

# Run daemon mode
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- .scherzo/scherzo.yaml
```

If no path is provided, Scherzo looks for the first existing default config in this order:

1. `.scherzo/scherzo.yaml`
2. `.scherzo/scherzo.yml`
3. `scherzo.yaml`
4. `scherzo.yml`

Passing a `.md` path fails with a clear startup error.

## Repository conventions

A typical repo-local setup is:

```text
.scherzo/
  scherzo.yaml                 # orchestrator/runtime config
  workflows/
    research.yaml              # workflow DAG
    implementation.yaml        # workflow DAG
    prompts/
      research.md              # prompt template
      implement.md             # prompt template
```

This repository dogfoods that layout with:

- `.scherzo/scherzo.yaml`
- `.scherzo/workflows/research.yaml`
- `.scherzo/workflows/prompts/research.md`

Reusable examples live under `examples/`:

- `examples/scherzo.yaml`
- `examples/workflows/research.yaml`
- `examples/workflows/implementation.yaml`
- `examples/workflows/prompts/*.md`

## Orchestrator config

The orchestrator config owns runtime policy: tracker settings, polling, workspace hooks, pi command and timeouts, agent limits, handoff, workflow routing, artifact limits, Linear contract checking, and Linear comment command transport.

Minimal shape:

```yaml
version: 1
tracker:
  kind: linear
  endpoint: https://api.linear.app/graphql
  api_key: "$LINEAR_API_KEY"
  project_slug: YOUR_PROJECT_SLUG
  active_states: [Todo, In Progress]
  terminal_states: [Done, Canceled, Duplicate]

workspace:
  root: .scherzo/workspaces
  hooks:
    create: |
      mkdir -p "$SCHERZO_WORKSPACE_PATH"
      git clone "$REPO_URL" "$SCHERZO_WORKSPACE_PATH"
    before_step: |
      test -d "$SCHERZO_WORKSPACE_PATH/.git"
    after_step: |
      true
    remove: |
      rm -rf "$SCHERZO_WORKSPACE_PATH"
    timeout_ms: 60000

agent:
  max_concurrent_agents: 1
  max_turns: 20
  max_retry_attempts: 5
  max_sessions_per_issue: 3

pi:
  command: "pi --mode rpc --no-session"
  compatibility_probe: true

routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    research: workflows/research.yaml
    implementation: workflows/implementation.yaml
```

Relative paths are resolved from the orchestrator config file directory.

## Workflow DAG files

A workflow file describes one routed issue workflow. Steps may be `agent` steps that run pi with a Markdown prompt template, or `command` steps that run shell commands in a prepared workflow workspace.

```yaml
version: 1
id: implementation
description: Implement, test, review, apply feedback, and validate.
max_parallel_steps: 4
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    workspace: main

  - id: test_after_implement
    kind: command
    depends_on: [implement]
    run: gleam test
    workspace: main
    on_failure: continue

  - id: code_review
    kind: agent
    depends_on: [implement]
    prompt: prompts/code-review.md
    workspace:
      name: code-review
      from: main

  - id: apply_feedback
    kind: agent
    depends_on: [test_after_implement, code_review]
    prompt: prompts/apply-feedback.md
    workspace: main
```

Important rules:

- `id` must match the routing key used by `routing.workflows`.
- Step ids are unique within a workflow.
- `depends_on` forms a DAG; cycles are rejected.
- Prompt paths are relative to the workflow YAML file and must stay within that workflow directory.
- Steps sharing the same logical workspace are serialized.
- Steps using different logical workspaces may run concurrently up to `max_parallel_steps` and global agent limits.
- A derived workspace (`name` + `from`) is prepared from the named source workspace before the step runs.

## Prompt templates

Prompt files are Markdown templates rendered for pi. Templates can reference the issue and prior step artifacts. For example:

```md
Implement {{ issue.identifier }}: {{ issue.title }}

Description:
{{ issue.description }}

Previous test output:
{{ steps.test_after_implement.stdout }}
```

Markdown prompt templates are not runtime workflow files; they are only prompt bodies referenced by YAML DAG steps.

## Workspace hooks

Workspace hooks are trusted shell snippets from the orchestrator config. Scherzo creates and prepares per-issue/per-run workflow workspaces and calls hooks with environment such as:

- `SCHERZO_CONFIG_DIR`
- `SCHERZO_WORKFLOW_ID`
- `SCHERZO_RUN_ID`
- `SCHERZO_ISSUE_ID`
- `SCHERZO_ISSUE_IDENTIFIER`
- `SCHERZO_WORKSPACE_NAME`
- `SCHERZO_WORKSPACE_PATH`
- `SCHERZO_SOURCE_WORKSPACE_NAME`
- `SCHERZO_SOURCE_WORKSPACE_PATH`

Use `agent.max_concurrent_agents: 0` to pause new dispatch while keeping daemon reload and reconciliation alive.

## Control and observability

Daemon mode starts a local control server and writes a control file under the configured workspace root. The helper script can inspect and supervise sessions:

```sh
scripts/scherzoctl status
scripts/scherzoctl sessions
scripts/scherzoctl attach SESSION_ID
scripts/scherzoctl prompt SESSION_ID "continue with tests"
scripts/scherzoctl stop SESSION_ID
```

When YAML DAG agent steps run, Scherzo creates concrete step sessions such as `ABC-123-42-1-implement`. Operator prompts sent to the top-level issue session are routed to the active agent step when that step exposes a command subject.

## Linear workflow labels

Scherzo routes issues by label. With the default prefix `workflow:`, an issue labeled `workflow:research` is routed to the `research` workflow key in `routing.workflows`.

Use `--linear-contract-check` before enforcing labels or state handoff on a real board:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
```

## Legacy Markdown removal

The old Markdown runtime loader and tests have been removed. The following are no longer valid runtime entrypoints:

```sh
gleam run -- WORKFLOW.md
gleam run -- .scherzo/workflows/research.md
```

Use a YAML orchestrator config instead:

```sh
gleam run -- .scherzo/scherzo.yaml
```
