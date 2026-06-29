# Scherzo

Scherzo turns tracker tasks into supervised, repeatable coding-agent workflows. The production tracker adapter is Linear today: Scherzo polls tasks from Linear, selects them by workflow labels, prepares per-run workspaces, executes YAML DAGs made of `pi` agent steps and shell command steps, retains artifacts, and hands results back through the tracker.

Scherzo is a Gleam/Erlang daemon and command-line tool. It is currently best suited for teams that are comfortable running their own local automation, reviewing agent output, and adapting repository-local YAML, prompts, schemas, and workspace policy.

## When to use Scherzo

Use Scherzo when you want to:

- run the same agent workflow for every eligible task instead of one-off chats;
- route tasks by labels such as `workflow:implementation` or `workflow:research`; the current production task source is the Linear adapter;
- isolate implementation attempts in workspace-driver-managed directories;
- combine agent steps, command validation steps, review steps, and tracker task updates;
- retain artifacts and operator-visible events for inspection and recovery; and
- start cautiously with `doctor` and `--once` before daemon mode.

## When not to use Scherzo

Scherzo is not a hosted product, a sandbox, or a stable multi-tracker platform. It currently ships one production tracker adapter, Linear, and uses `pi` for agent execution. Do not use it as unattended production automation until your repository-specific workflow, workspace driver, Linear policy, credentials, and validation commands have been reviewed by an operator.

Workflow files and workspace drivers are trusted local configuration. Scherzo enforces workspace cwd/root containment, but it does not provide a VM or container boundary.

## Start here

If you are adapting Scherzo to another repository, start with the guided adopter path:

- [Getting started](docs/GETTING_STARTED.md) — from minimal repo config to a cautious `--once` run, including YAML editor schema setup.
- [Simplified YAML migration guide](docs/runbooks/simplified-yaml-migration.md) — old/new config examples and upgrade checklist.
- [Example orchestrator config](examples/scherzo.yaml) — complete source-tree example.
- [Packaged no-op driver example](examples/scherzo-packaged-noop.yaml) — artifact-only/research workflows.
- [Packaged jj driver example](examples/scherzo-packaged-jj.yaml) — implementation workflows with the bundled jj driver.
- [Example workflows](examples/workflows/) — YAML DAGs and prompt templates.

The usual first run is:

```sh
scherzo --version
LINEAR_API_KEY=lin_api_... scherzo doctor .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... scherzo --once .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... scherzo .scherzo/scherzo.yaml
scherzoctl ps
```

The packaged `scherzo` launcher is the foreground daemon command and translates terminal Ctrl-C into Scherzo's graceful SIGTERM shutdown path. The deprecated `scherzo-start` alias has been removed; replace old `scherzo-start <config>` or `nix run .#scherzo-start -- <config>` usage with `scherzo <config>` or `nix run .#scherzo -- <config>`. When working from this source checkout, run non-daemon entrypoints through devenv, for example `direnv exec . gleam run -- doctor .scherzo/scherzo.yaml` or `direnv exec . scripts/scherzoctl ps`.

## Documentation map

| Topic | Where to go |
| --- | --- |
| End-to-end adoption path | [docs/GETTING_STARTED.md](docs/GETTING_STARTED.md) |
| Repository architecture and change checklist | [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) |
| Breaking-change upgrade policy | [docs/runbooks/upgrades.md](docs/runbooks/upgrades.md) |
| Simplified YAML schema, editor schema setup, and migration | [docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md](docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md), [docs/GETTING_STARTED.md#yaml-editor-schema-support](docs/GETTING_STARTED.md#yaml-editor-schema-support), and [docs/runbooks/simplified-yaml-migration.md](docs/runbooks/simplified-yaml-migration.md) |
| Workspace driver contract | [docs/specs/WORKSPACE_DRIVER_SPEC.md](docs/specs/WORKSPACE_DRIVER_SPEC.md) |
| Structured output and validators | [docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md](docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md) |
| Workspace driver migration notes | [docs/runbooks/workspace-driver-migration.md](docs/runbooks/workspace-driver-migration.md) |
| Tracker adapters and capability matrix | [docs/runbooks/tracker-adapters.md](docs/runbooks/tracker-adapters.md) |
| Operator control basics | [Observe/control with `scherzoctl`](docs/GETTING_STARTED.md#13-observe-and-control-with-scherzoctl) and [workflow recovery](docs/runbooks/workflow-recovery.md) |
| Recovery, retained artifacts, and cleanup | [docs/runbooks/workflow-recovery.md](docs/runbooks/workflow-recovery.md) |
| Step recovery groundwork | [docs/runbooks/workflow-step-recovery.md](docs/runbooks/workflow-step-recovery.md) |
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
schemas/                       # public YAML schemas and optional structured-output schemas
scripts/                       # optional validators or custom workspace drivers
```

This repository dogfoods the same shape under `.scherzo/` and keeps reusable examples under `examples/`.

## Core concepts

- **Orchestrator config** (`.scherzo/scherzo.yaml`) owns tracker settings, polling, top-level workflow routes, workspace drivers, agent runtime settings, task-update policy, artifact limits, and tracker readiness checks. In this repository those checks target Linear through `tracker.linear.check_setup`.
- **Workspace drivers** decide where each step runs. Bundled packaged drivers include `scherzo-workspace-noop` for artifact-only workflows and `scherzo-workspace-jj` for jj-backed implementation workspaces. Custom drivers must follow the workspace driver spec.
- **Workflow DAGs** are YAML files routed by task metadata, currently workflow labels on Linear tasks. Steps infer agent vs command behavior from `prompt` or `run`, run in lanes selected by `run_in`, and use `recovery` for bounded step remediation.
- **Structured output** lets an agent step return a required JSON artifact and validate it with baseline checks, JSON Schema validators, command validators, or both.
- **Operator observability** includes daemon logs, retained artifacts, and outbound tracker updates; with the current production adapter those updates are Linear comments. **Operator control** is local through `scherzoctl` commands such as `ps`, `session`, `events`, `attach`, `pause`, `resume`, `retry`, `park`, `abort`, and `prompt`.

## Workspace drivers

Workspace drivers decide where workflow steps run and what isolation/publish behavior they get. The normative contract is [docs/specs/WORKSPACE_DRIVER_SPEC.md](docs/specs/WORKSPACE_DRIVER_SPEC.md); migration notes for unsupported legacy workspace hooks/profile config are in [docs/runbooks/workspace-driver-migration.md](docs/runbooks/workspace-driver-migration.md).

In config, `workspace.driver` selects the built-in `noop` or `jj` driver, or a named entry under `workspace.drivers`. Named entries use `type: noop`, `type: jj`, or `type: custom`; custom entries provide `command` plus optional `timeout` and `env`, while `type: jj` supports friendly fields such as `publish_remote`, `github_repo`, and `fetch_base` that map to the driver environment. Workflows request capabilities with `workspace.requires`, and Scherzo exposes driver context such as `SCHERZO_WORKSPACE_DRIVER` and `SCHERZO_WORKSPACE_CAPABILITIES` to steps. Driver-specific settings may live in `env`, for example `SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE` or `SCHERZO_PR_DRAFT`, but driver env is not a secret store. `SCHERZO_PR_DRAFT` accepts only `true` or `false`; when unset, PR publication keeps the driver's default draft behavior.

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

# Shell-heavy script/workflow/daemon/process/driver contract suite
direnv exec . scherzo-test-contract
# CI-friendly shards are also available, for example:
direnv exec . scherzo-test-contract runtime

# Production lint gates
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint

# Source/build identity for bug reports and operator logs
direnv exec . gleam run -- --version

# Readiness validation before dispatching work
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml

# Cautious one-task run; with the current production adapter this dispatches one eligible Linear task
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml

# Ctrl-C-friendly packaged daemon mode and control UI
LINEAR_API_KEY=lin_api_... nix run .#scherzo -- .scherzo/scherzo.yaml
direnv exec . scripts/scherzoctl ps
```

## Test suites

Every PR should run the deterministic unit suite before review:

```sh
direnv exec . gleam test
# equivalent explicit wrapper:
direnv exec . scherzo-test-unit
```

Shell-heavy script, workflow-helper, renderer, daemon/service, port/process, pi-client, and workspace-driver contract coverage is explicit so the default loop stays unit-scoped:

```sh
direnv exec . scherzo-test-contract
```

For CI or local runners with per-command timeouts, run the contract shards separately. The test runner serializes suites with `test/.tmp-suite-lock` and resets `test/tmp` at suite start, so parallel suite invocations wait instead of sharing scratch space; do not clean `test/tmp` manually while another suite is active:

```sh
direnv exec . scherzo-test-contract runtime
direnv exec . scherzo-test-contract orchestrator
direnv exec . scherzo-test-contract tracker
direnv exec . scherzo-test-contract workflow
direnv exec . scherzo-test-contract repository
```

Run the contract suite or the relevant shards when changing helper scripts such as `.scherzo/workflows/scripts/scherzo-review` or `.scherzo/workflows/scripts/scherzo-implementation`, ExecPlan HTML rendering, daemon/service behavior, port/pi-client process boundaries, workspace driver scripts, or before relying on repository confidence from the final gate.

The explicit integration suites are opt-in because they have required dependencies outside the normal unit and contract loops: `scherzo-test-local-integration` exercises local jj/workspace behavior, and `scherzo-test-real-pi-validation` uses the devenv-provided `pi` plus working model/provider credentials.

For the full local gate used by dogfood implementation workflows, run `scripts/scherzo-ci`. It runs formatting, production lint, workflow contracts, the unit and contract suites, and `nix flake check`; local-integration and real-pi-validation remain explicit because of their external dependency requirements. Pass a target (for example `scripts/scherzo-ci unit`) to run a subset.

```sh
direnv exec . scripts/scherzo-ci
```

## Development status

Scherzo is in active development and is dogfooded for real project work. Expect rough edges:

- Runtime configuration and workflow definitions are YAML-only and may still change. Markdown is supported for prompt templates, not runtime workflow definitions.
- Linear and `pi` are the first-class integrations today.
- Workspaces, credentials, model/provider settings, schemas, tracker-adapter capabilities, and validators are intentionally explicit repository policy.
- Operators should expect to inspect logs, retained artifacts, `scherzoctl` output, and tracker comments when something goes wrong. In this repository those tracker comments are Linear comments.

## License

Scherzo is licensed under Apache-2.0. See [LICENSE](LICENSE) for the full license text.
