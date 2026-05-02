# Scherzo

Scherzo is a Gleam/Erlang orchestration daemon for running pi coding-agent workflows against Linear issues. It is now YAML-orchestrator / YAML-DAG only: runtime settings live in a `scherzo.yaml` orchestrator config, and issue workflows live in YAML DAG files that reference Markdown prompt templates.

Legacy Markdown runtime workflows (`WORKFLOW.md` or `.scherzo/workflows/*.md`) are no longer supported. Markdown remains supported for prompt templates only.

## Quick start

```sh
direnv allow
direnv exec . gleam test

# Readiness validation before dispatching work
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml

# Read-only readiness subset when you do not want workspace hooks or pi probing
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check workflow-config --check linear-contract --check linear-smoke .scherzo/scherzo.yaml

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

## Planning posture

Scherzo is still moving quickly, so implementation plans should not assume backward compatibility unless the plan or operator explicitly calls it out. Prefer the simpler clean change over compatibility shims: update schemas, tests, docs, examples, and local state handling together. Local development state such as `.scherzo-state/ledger` may be deleted or regenerated after breaking internal changes.

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
  # Optional project defaults for workflow agent steps. `model` is the full
  # pi model/provider selection key; `thinking` is configured separately.
  model: google/gemini-2.5-flash
  thinking: low
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
    # Per-step overrides are independent: this step keeps the project default
    # model/provider but asks pi for a higher thinking level.
    thinking: high
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
- Agent steps inherit `pi.model` and `pi.thinking` from the orchestrator config. An agent step can override `model`, `thinking`, or both; unspecified values continue to inherit the project default.
- Command steps do not run pi, so `model` and `thinking` are only valid on agent steps.
- `model` is passed to pi as one selection key (for example `google/gemini-2.5-flash` or `github-copilot/gpt-5.1-codex`). Do not include pi's `:<thinking>` shorthand in `model`; set `thinking` separately (`off`, `minimal`, `low`, `medium`, `high`, or `xhigh`).

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

Daemon mode starts a local control server and writes a control file under the configured workspace root. Use `scripts/scherzoctl` to inspect and supervise sessions:

```sh
scripts/scherzoctl ping
scripts/scherzoctl ps
scripts/scherzoctl session <session-id>
scripts/scherzoctl events --pretty <session-id>
scripts/scherzoctl attach <session-id>
scripts/scherzoctl attach --verbose <session-id>
scripts/scherzoctl prompt <session-id> "continue with tests"
scripts/scherzoctl stop-after-turn <session-id> --yes
scripts/scherzoctl abort <session-id> --yes
```

`attach` replays retained events and then follows new events with a human-readable renderer. It groups visible output by Scherzo pass, streams assistant deltas as thinking text, shows tool input and output as readable blocks, highlights blocking UI requests, and prints token summaries when available. Use `--verbose` for pi lifecycle and raw diagnostic lines, `--no-follow` for replay only, `--since-cursor <n>` to resume after a known cursor, and `--color=auto|always|never` to control ANSI styling.

Every command accepts `--control-file <path>`. Non-streaming commands accept `--json` for automation.

Available local mutating commands include:

```sh
scripts/scherzoctl pause
scripts/scherzoctl resume
scripts/scherzoctl reload
scripts/scherzoctl retry ABC-123
scripts/scherzoctl park ABC-123 --reason "manual cleanup" --yes
scripts/scherzoctl unpark ABC-123
scripts/scherzoctl abort <session-id> --yes
scripts/scherzoctl stop-after-turn <session-id> --yes
scripts/scherzoctl prompt <session-id> "summarize progress"
scripts/scherzoctl ui respond <session-id> ui-1 --cancel
scripts/scherzoctl ui respond <session-id> ui-1 --value ok
```

When YAML DAG agent steps run, Scherzo creates concrete step sessions such as `ABC-123-42-1-implement`. Operator prompts sent to the top-level issue session are routed to the active agent step when that step exposes a command subject.

## Doctor readiness checks

Run `doctor` before cautious real-board operation. The command loads the YAML orchestrator config, routed workflow DAGs, and prompt templates, then prints a human-readable readiness report inspired by tools such as `flutter doctor`: each selected check is marked with `✓`, `!`, `✗`, or `-`, followed by a readable summary and remediation hints for failures. The default check set runs in this order: `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`. Use `--logfmt` when you need the previous machine-readable `doctor_check_*` events and `doctor_summary` fields.

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml
direnv exec . gleam run -- doctor --list-checks
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --logfmt .scherzo/scherzo.yaml
```

A successful default run looks like:

```text
Scherzo doctor
Config: .scherzo/scherzo.yaml

✓ Workflow config
  Loaded YAML orchestrator config and 1 workflow DAG.

✓ Linear contract
  Project board matches configured states and labels.
  Team count: 1, states: 7, labels: 7.

✓ Linear smoke
  Read-only Linear API check succeeded.
  Candidates: 2, terminal sample: 3, refreshed: 1.

✓ Instance lock
  Local instance lock can be acquired and released.

✓ Workspace hooks
  Scratch workspace was prepared and cleaned up.
  Hooks: create,before_step,remove.

✓ Pi probe
  pi RPC launched successfully and no prompt was sent.

Summary: 6 passed, 0 warnings, 0 failed, 0 skipped

Ready for cautious real-board operation.
```

Use repeated `--check` flags to run a subset. This read-only subset loads config and queries Linear metadata/issues, but it does not acquire the local instance lock, run workspace hooks, prepare a scratch workspace, or launch pi:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check workflow-config --check linear-contract --check linear-smoke .scherzo/scherzo.yaml
```

The default doctor run includes local checks. `workspace-hooks` prepares and cleans up a scratch workflow-run workspace using the configured `workspace.hooks.create`, `workspace.hooks.before_step`, and `workspace.hooks.remove` snippets. `pi-probe` launches pi RPC in that scratch workspace and performs the compatibility probe without sending a task prompt.

The focused one-off readiness modes remain available for troubleshooting individual surfaces:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe .scherzo/scherzo.yaml
```

## Linear workflow labels

Scherzo routes issues by label. With the default prefix `workflow:`, an issue labeled `workflow:research` is routed to the `research` workflow key in `routing.workflows`.

Use `doctor --check linear-contract` or `--linear-contract-check` before enforcing labels or state handoff on a real board:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check linear-contract .scherzo/scherzo.yaml
```

## Linear command comments

Linear command comments are disabled by default. When `linear_commands.enabled: true`, Scherzo watches configured Linear issue comments for commands with the configured prefix, normally `/scherzo`.

Supported comments include:

```text
/scherzo retry
/scherzo park --reason waiting-for-review
/scherzo unpark
/scherzo abort
/scherzo stop-after-turn
/scherzo prompt Please continue with the smaller fix.
/scherzo ui respond ui-17 --cancel
/scherzo ui respond ui-17 --value approved
```

Authorization is by explicit Linear user id allowlist only. The transport is runtime-only; commands posted while Scherzo is down are not replayed on startup. Local `scherzoctl` remains the fallback control path.

## Local durable ledger

Scherzo includes a local durable state ledger under `workspace.root/.scherzo-state/ledger/`. Daemon startup now replays this ledger before the first poll tick and uses it for single-instance restart recovery for the same canonical workspace root. Recovery restores durable retry counters, worker-session counters, parked issues, retry timers, known workspace paths, and replayable pending Linear outbox entries that include bounded v2 payloads. Started runs that lack a finish record are marked interrupted because live pi sessions and Erlang ports cannot survive a BEAM restart.

The ledger layout is:

```text
.scherzo-state/ledger/current.jsonl
.scherzo-state/ledger/snapshot.json
.scherzo-state/ledger/archive/segment-<n>.jsonl
```

`current.jsonl` is append-only JSON Lines. Each line is one schema-versioned record with `schema_version`, `record_id`, `kind`, and `at_ms`, plus fields for run, retry, park, counters, known workspaces, Linear command, or outbox facts. Replay rejects unsupported schema versions and malformed middle records, while tolerating one truncated trailing JSON record from a crash during append. Compaction writes a projection snapshot through a temporary file and then archives the old current segment before starting a fresh `current.jsonl`.

Ledger records are operational state, not transcripts. They should contain identifiers, statuses, bounded excerpts, result codes, and redacted strings only. Outbox replay requires `outbox_pending_v2` records with bounded, redacted payload JSON; payload-less old pending outbox records fail startup clearly instead of being silently dropped. Do not store API keys, raw pi JSON, full prompts, or full Linear comment bodies in the ledger.

## Daemon behavior and restart recovery

On startup, Scherzo resolves config, builds Linear clients, replays the local ledger, fetches current Linear state only for ledger-known issue ids in chunks of at most 50, appends any recovery records with fsync, installs the recovered runtime state, schedules recovered retry timers, enqueues known terminal workspace cleanup, and only then allows polling. If Linear cannot refresh the known issue ids, startup fails before dispatching new work; starting from stale local facts would be less safe than refusing to start.

Interrupted active runs are counted as one failure using their run id as the durable counter source, so restarting repeatedly does not double-count the same interrupted run. If retry caps are exhausted, the issue remains parked with its release policy. Explicit operator parks survive issue edits until explicitly unparked. Auto-unpark parks are released only when the refreshed issue fingerprint has changed, matching the normal runtime policy.

This recovery is at-least-once rather than exactly-once. A crash after a real Linear side effect succeeds but before its `outbox_completed` record is written can replay the side effect on restart. Dedupe keys, run ids, and source comment ids are recorded to make duplicates auditable, but Linear-side idempotency is not guaranteed.

## Implemented coverage

The deterministic test suite covers ledger record roundtrips for counters, known workspaces, v2 parking, and v2 outbox payloads; projection helpers for retry due time and pending outbox replay; pure recovery for interrupted, parked, terminal, overdue-retry, future-retry, and payload-less outbox cases; and daemon startup ordering through the existing actor tests. Real Linear and real pi are not required for these recovery tests.

## Safety posture

Scherzo is intended for trusted repositories and trusted workflow files. Hooks are arbitrary shell. pi tool execution follows the operator's `pi.command` and host OS environment. Scherzo enforces workspace cwd and root containment, but it does not provide a VM or container sandbox.

Run only one Scherzo instance per Linear project and canonical workspace root. The local durable ledger supports single-instance restart recovery, not multi-host or multi-workspace exactly-once behavior. Daemon mode handles SIGTERM gracefully by shutting down workers, removing the control file, and releasing the local instance lock before exit. Ctrl-C/SIGINT, `kill -9`, host power loss, or BEAM VM crashes may leave a stale `workspace.root/.scherzo-state/instance.lock`; operators must remove it only after checking no Scherzo process remains active. Live pi sessions, EventHub history, and Linear command comments posted while Scherzo was down are still not recovered in this phase.

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
