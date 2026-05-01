# Scherzo repo workflows

This directory contains checked-in Scherzo workflow definitions for dogfooding this repository.

## Convention

- Put the repo dogfood runtime config in `.scherzo/scherzo.yaml`.
- Put versioned YAML workflow DAGs in `.scherzo/workflows/*.yaml`.
- Put prompt templates referenced by YAML DAG agent steps in `.scherzo/workflows/prompts/*.md`.
- Put runtime jj workspaces under `.scherzo/workspaces/<workflow-name>/`; they are ignored by git.
- Config-relative paths are resolved from `.scherzo/scherzo.yaml`, so this repository uses `workspace.root: workspaces` to land at repo-root `.scherzo/workspaces`.
- Populate Scherzo workspaces with `jj workspace add`, not separate `git clone` checkouts, so dogfood runs share the local repository and can be integrated through normal jj workflow.
- Use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks` instead of inlining jj lifecycle shell; pass the workflow name as the second argument.
- Keep machine-specific variants as `.scherzo/workflows/**/*.local.yaml`, `.scherzo/workflows/**/*.local.yml`, `.scherzo/scherzo.local.yaml`, or `.scherzo/scherzo.local.yml`; they are ignored by git.
- Do not put secrets in workflow files. Use environment variables for secrets and deployment-specific values.

The repo `.gitignore` intentionally ignores runtime `.scherzo/*` state while allowing this README, `.scherzo/scherzo.yaml`, and `.scherzo/workflows/**` to be checked in.

## Required environment

The checked-in workflow expects:

```sh
export LINEAR_API_KEY=lin_api_...
# Optional. Defaults to the repository root inferred from .scherzo/scherzo.yaml.
export SCHERZO_REPO_ROOT=$(pwd)
```

The checked-in `tracker.project_slug` targets the Linear project `scherzo-f6f4bc92d6d7`. `SCHERZO_REPO_ROOT` is optional for checked-in workflows in this repository, but setting it makes the jj workspace hook independent of the current directory layout.

## Linear project contract

`linear_contract.enabled: true` makes Scherzo fail readiness checks if the Linear project drifts from this checked-in dogfood contract. The project must expose these states for every associated Linear team:

- `Todo`
- `In Progress`
- `Done`
- `Canceled`
- `Duplicate`
- `Needs Workflow`

The project must also make these issue labels assignable:

- `workflow:research`
- `workflow:implementation`
- `needs-workflow`
- `needs-clarification`

Run the contract check after changing workflow labels, states, or team membership:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check linear-contract .scherzo/scherzo.yaml
```

## Dogfood workflows

The checked-in workflows are:

- `workflow:research` — single-agent investigation and recommendation.
- `workflow:implementation` — implement, run format/tests, review, apply feedback, final validate, and summarize.

Use the research workflow for the first supervised run:

```sh
direnv exec . gleam run -- --linear-smoke .scherzo/scherzo.yaml
direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
direnv exec . gleam run -- --pi-probe .scherzo/scherzo.yaml
LINEAR_API_KEY=$LINEAR_API_KEY SCHERZO_REPO_ROOT=$(pwd) \
  direnv exec . gleam run -- .scherzo/scherzo.yaml
```

In another terminal, supervise through the local control API:

```sh
scripts/scherzoctl ps
scripts/scherzoctl attach <session-id>
scripts/scherzoctl prompt <session-id> "Please produce a Linear-ready result summary."
```

For the first run, create or select one Linear issue in an active state with exactly one workflow label such as `workflow:research` or `workflow:implementation`. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
