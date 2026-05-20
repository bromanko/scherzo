# Scherzo

Scherzo turns tracker tasks into supervised, repeatable coding-agent workflows. The production tracker adapter is Linear today: Scherzo polls Linear issues, selects tasks by workflow labels, prepares per-run workspaces, executes YAML DAGs made of `pi` agent steps and shell command steps, retains artifacts, and hands results back to Linear for human review.

Scherzo is a Gleam/Erlang daemon and command-line tool. It is currently best suited for teams that are comfortable running their own local automation, reviewing agent output, and adapting repository-local YAML, prompts, schemas, and workspace policy.

## When to use Scherzo

Use Scherzo when you want to:

- run the same agent workflow for every eligible task instead of one-off chats;
- route tasks by labels such as `workflow:implementation` or `workflow:research`; Linear issues are the supported production task source today;
- isolate implementation attempts in workspace-driver-managed directories;
- combine agent steps, command validation steps, review steps, and handoff comments;
- retain artifacts and operator-visible events for inspection and recovery; and
- start cautiously with `doctor` and `--once` before daemon mode.

## When not to use Scherzo

Scherzo is not a hosted product, a sandbox, or a stable multi-tracker platform. It currently ships one production tracker adapter, Linear, and uses `pi` for agent execution. Do not use it as unattended production automation until your repository-specific workflow, workspace driver, Linear policy, credentials, and validation commands have been reviewed by an operator.

Workflow files and workspace drivers are trusted local configuration. Scherzo enforces workspace cwd/root containment, but it does not provide a VM or container boundary.

## Start here

If you are adapting Scherzo to another repository, start with the guided adopter path:

- [Getting started](docs/GETTING_STARTED.md) — from empty repo config to a cautious `--once` run.
- [Example orchestrator config](examples/scherzo.yaml) — complete source-tree example.
- [Packaged no-op profile example](examples/scherzo-packaged-noop.yaml) — artifact-only/research workflows.
- [Packaged jj profile example](examples/scherzo-packaged-jj.yaml) — implementation workflows with the bundled jj driver.
- [Example workflows](examples/workflows/) — YAML DAGs and prompt templates.

The usual first run is:

```sh
scherzo --version
LINEAR_API_KEY=lin_api_... scherzo doctor .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... scherzo --once .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... scherzo-start .scherzo/scherzo.yaml
scherzoctl ps
```

When working from this source checkout, run the same entrypoints through devenv, for example `direnv exec . gleam run -- doctor .scherzo/scherzo.yaml` or `direnv exec . scripts/scherzoctl ps`.

## Documentation map

| Topic | Where to go |
| --- | --- |
| End-to-end adoption path | [docs/GETTING_STARTED.md](docs/GETTING_STARTED.md) |
| Repository architecture and change checklist | [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) |
| Breaking-change upgrade policy | [docs/runbooks/upgrades.md](docs/runbooks/upgrades.md) |
| Workspace driver contract | [docs/specs/WORKSPACE_DRIVER_SPEC.md](docs/specs/WORKSPACE_DRIVER_SPEC.md) |
| Structured output and validators | [docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md](docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md) |
| Workspace driver migration notes | [docs/runbooks/workspace-driver-migration.md](docs/runbooks/workspace-driver-migration.md) |
| Tracker adapters and capability matrix | [docs/runbooks/tracker-adapters.md](docs/runbooks/tracker-adapters.md) |
| Operator control basics | [Observe/control with `scherzoctl`](docs/GETTING_STARTED.md#13-observe-and-control-with-scherzoctl) and [workflow recovery](docs/runbooks/workflow-recovery.md) |
| Recovery, retained artifacts, and cleanup | [docs/runbooks/workflow-recovery.md](docs/runbooks/workflow-recovery.md) |
| Scheduled jobs | [docs/runbooks/scheduled-jobs.md](docs/runbooks/scheduled-jobs.md) |
| Production lint policy | [docs/LINTING.md](docs/LINTING.md) |
| Test helpers and async test patterns | [test/README.md](test/README.md) |

Keep the specs as normative references. The getting-started guide intentionally links to them instead of duplicating the full command and schema contracts.

## Repository layout

A typical consuming repository uses this layout:

```text
.scherzo/
  scherzo.yaml                 # orchestrator/runtime config
  workflows/
    implementation.yaml        # workflow DAG
    research.yaml              # workflow DAG
    prompts/
      implement.md             # prompt template
      research.md              # prompt template
schemas/                       # optional JSON Schemas for structured output
scripts/                       # optional validators or custom workspace drivers
```

This repository dogfoods the same shape under `.scherzo/` and keeps reusable examples under `examples/`.

## Core concepts

- **Orchestrator config** (`.scherzo/scherzo.yaml`) owns tracker settings, polling, workspace profiles, pi settings, agent limits, handoff policy, routing, artifact limits, Linear contract compatibility checks, and optional Linear command comments.
- **Workspace profiles and drivers** decide where each step runs. Bundled packaged drivers include `scherzo-workspace-noop` for artifact-only workflows and `scherzo-workspace-jj` for jj-backed implementation workspaces. Custom drivers must follow the workspace driver spec.
- **Workflow DAGs** are YAML files routed by task metadata, currently Linear workflow labels. Steps may be `kind: agent` steps using Markdown prompt templates or `kind: command` steps running shell validation.
- **Structured output** lets an agent step return a required JSON artifact and validate it with baseline checks, JSON Schema validators, command validators, or both.
- **Operator control** is through daemon logs, retained artifacts, Linear comments, and `scherzoctl` commands such as `ps`, `session`, `events`, `attach`, `pause`, `resume`, `retry`, `park`, `abort`, and `prompt`.

## Workspace profiles and drivers

Workspace profiles and drivers decide where workflow steps run and what isolation/publish behavior they get. The normative contract is [docs/specs/WORKSPACE_DRIVER_SPEC.md](docs/specs/WORKSPACE_DRIVER_SPEC.md); migration notes from legacy workspace.hooks configuration are in [docs/runbooks/workspace-driver-migration.md](docs/runbooks/workspace-driver-migration.md).

In config, `workspace.profiles.<name>.driver.command` points at a driver such as `command: scherzo-workspace-noop` for artifact-only workflows or `command: scherzo-workspace-jj` for jj-backed implementation workflows. Workflows request `workspace_capabilities`, and Scherzo exposes driver context such as `SCHERZO_WORKSPACE_DRIVER` and `SCHERZO_WORKSPACE_CAPABILITIES` to steps. Driver-specific settings may live in `driver.env`, for example `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE` or `SCHERZO_PR_DRAFT`, but `driver.env` is not a secret store. `SCHERZO_PR_DRAFT` accepts only `true` or `false`; when unset, PR publication keeps the driver's default draft behavior.

## Using pi as an operator UI

Use the checked-in operator skill when supervising Scherzo from pi: `/skill:scherzo-operator` or `pi --skill .pi/skills/scherzo-operator`. Start with read-only summaries first, using `SCHERZO_CONTROL_FILE` when needed and exact task/issue ids or session ids from JSON inspection, for example `scripts/scherzoctl ps --json`.

## Local development

This repository uses `.envrc`/devenv. In a fresh checkout, approve the environment once:

```sh
direnv allow .
```

Common source-checkout commands:

```sh
# Deterministic unit suite
direnv exec . gleam test

# Shell-heavy script/workflow/driver contract suite
direnv exec . scherzo-test-contract

# Production lint gates
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint

# Source/build identity for bug reports and operator logs
direnv exec . gleam run -- --version

# Readiness validation before dispatching work
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml

# Cautious one-task run; with the Linear adapter this dispatches one eligible Linear issue
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml

# Ctrl-C-friendly daemon mode and control UI
LINEAR_API_KEY=lin_api_... direnv exec . scherzo-start .scherzo/scherzo.yaml
direnv exec . scripts/scherzoctl ps
```

## Test suites

Every PR should run the deterministic unit suite before review:

```sh
direnv exec . gleam test
# equivalent explicit wrapper:
direnv exec . scherzo-test-unit
```

Shell-heavy script, workflow-helper, renderer, and workspace-driver contract coverage is explicit so the default loop stays unit-scoped:

```sh
direnv exec . scherzo-test-contract
```

Run the contract suite when changing helper scripts such as `.scherzo/workflows/scripts/scherzo-review` or `.scherzo/workflows/scripts/scherzo-implementation`, ExecPlan HTML rendering, workspace driver scripts, or before relying on repository confidence from the final gate.

The explicit integration suites are opt-in because they have required dependencies outside the normal unit and contract loops: `scherzo-test-local-integration` exercises local jj/workspace behavior, and `scherzo-test-real-pi-validation` uses the devenv-provided `pi` plus working model/provider credentials.

For the full local gate used by dogfood implementation workflows, run SelfCI against the configured pull-request base. SelfCI runs the unit and contract suites; local-integration and real-pi-validation remain explicit because of their external dependency requirements.

```sh
direnv exec . selfci check --base main@origin --candidate @ --print-output
```

## Development status

Scherzo is in active development and is dogfooded for real project work. Expect rough edges:

- Runtime configuration and workflow definitions are YAML-only and may still change. Markdown is supported for prompt templates, not runtime workflow definitions.
- Linear and `pi` are the first-class integrations today.
- Workspaces, credentials, model/provider settings, schemas, tracker-adapter capabilities, and validators are intentionally explicit repository policy.
- Operators should expect to inspect logs, retained artifacts, `scherzoctl` output, and Linear comments when something goes wrong.

## License

Scherzo is licensed under Apache-2.0. See [LICENSE](LICENSE) for the full license text.
