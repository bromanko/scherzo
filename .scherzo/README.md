# Scherzo repo workflows

This directory contains checked-in Scherzo workflow definitions for dogfooding this repository.

## Convention

- Put the repo dogfood runtime config in `.scherzo/scherzo.yaml`.
- Put versioned YAML workflow DAGs in `.scherzo/workflows/*.yaml`.
- Put prompt templates referenced by YAML DAG agent steps in `.scherzo/workflows/prompts/*.md`.
- Put runtime jj workspaces under `.scherzo/workspaces/<workflow-name>/`; they are ignored by git.
- Config-relative paths are resolved from `.scherzo/scherzo.yaml`, so this repository uses `workspace.root: workspaces/research` to land at repo-root `.scherzo/workspaces/research`.
- Populate Scherzo workspaces with `jj workspace add`, not separate `git clone` checkouts, so dogfood runs share the local repository and can be integrated through normal jj workflow.
- Use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks` instead of inlining jj lifecycle shell; pass the workflow name as the second argument.
- Keep machine-specific variants as `.scherzo/workflows/**/*.local.yaml`, `.scherzo/workflows/**/*.local.yml`, `.scherzo/scherzo.local.yaml`, or `.scherzo/scherzo.local.yml`; they are ignored by git.
- Do not put secrets in workflow files. Use environment variables for secrets and deployment-specific values.

The repo `.gitignore` intentionally ignores runtime `.scherzo/*` state while allowing this README, `.scherzo/scherzo.yaml`, and `.scherzo/workflows/**` to be checked in.

## Required environment

The checked-in workflow expects:

```sh
export LINEAR_API_KEY=lin_api_...
export LINEAR_PROJECT_SLUG=<linear-project-slug>
# Optional. Defaults to the repository root inferred from .scherzo/scherzo.yaml.
export SCHERZO_REPO_ROOT=$(pwd)
```

`LINEAR_PROJECT_SLUG` works because Scherzo resolves single-value `$ENV_VAR` references in `tracker.project_slug`. `SCHERZO_REPO_ROOT` is optional for checked-in workflows in this repository, but setting it makes the jj workspace hook independent of the current directory layout.

## First dogfood workflow

Use the research workflow for the first supervised run:

```sh
direnv exec . gleam run -- --linear-smoke .scherzo/scherzo.yaml
direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
direnv exec . gleam run -- --pi-probe .scherzo/scherzo.yaml
LINEAR_API_KEY=$LINEAR_API_KEY LINEAR_PROJECT_SLUG=$LINEAR_PROJECT_SLUG SCHERZO_REPO_ROOT=$(pwd) \
  direnv exec . gleam run -- .scherzo/scherzo.yaml
```

In another terminal, supervise through the local control API:

```sh
scripts/scherzoctl ps
scripts/scherzoctl attach <session-id>
scripts/scherzoctl prompt <session-id> "Please produce a Linear-ready result summary."
```

For the first run, create or select one Linear issue in an active state with exactly one `workflow:research` label. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
