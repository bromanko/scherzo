# Scherzo repo workflows

This directory contains checked-in Scherzo workflow definitions for dogfooding this repository.

## Convention

- Put legacy versioned workflow definitions in `.scherzo/workflows/*.md`.
- Put experimental YAML DAG workflows in `.scherzo/workflows/*.yaml` with prompt templates in sibling Markdown files.
- Put runtime jj workspaces under `.scherzo/workspaces/<workflow-name>/`; they are ignored by git.
- Workflow-relative paths are resolved from the workflow file directory, so a workflow in `.scherzo/workflows/` should use `workspace.root: ../workspaces/<workflow-name>`.
- Populate Scherzo workspaces with `jj workspace add`, not separate `git clone` checkouts, so dogfood runs share the local repository and can be integrated through normal jj workflow.
- Use `scripts/scherzo-jj-workspace` from workflow hooks instead of inlining jj lifecycle shell; pass the workflow name as the second argument.
- Keep machine-specific variants as `.scherzo/workflows/*.local.md`, `.scherzo/workflows/**/*.local.yaml`, or `.scherzo/scherzo.local.yaml`; they are ignored by git.
- Do not put secrets in workflow files. Use environment variables for secrets and deployment-specific values.

The repo `.gitignore` intentionally ignores runtime `.scherzo/*` state while allowing this README and `.scherzo/workflows/**` to be checked in. Do not add a repo-local `.scherzo/scherzo.yaml` until the dogfood migration is ready; copy from `examples/scherzo.yaml` for local experiments.

## Required environment

The checked-in workflows expect:

```sh
export LINEAR_API_KEY=lin_api_...
export LINEAR_PROJECT_SLUG=<linear-project-slug>
# Optional. Defaults to the repository root inferred from .scherzo/workspaces.
export SCHERZO_REPO_ROOT=$(pwd)
```

`LINEAR_PROJECT_SLUG` works because Scherzo resolves single-value `$ENV_VAR` references in `tracker.project_slug`. `SCHERZO_REPO_ROOT` is optional for checked-in workflows in this repository, but setting it makes the jj workspace hook independent of the current directory layout.

## First dogfood workflow

Use the research workflow for the first supervised run:

```sh
direnv exec . gleam run -- --linear-smoke .scherzo/workflows/research.md
direnv exec . gleam run -- --linear-contract-check .scherzo/workflows/research.md
direnv exec . gleam run -- --pi-probe .scherzo/workflows/research.md
LINEAR_API_KEY=$LINEAR_API_KEY LINEAR_PROJECT_SLUG=$LINEAR_PROJECT_SLUG SCHERZO_REPO_ROOT=$(pwd) \
  direnv exec . gleam run -- .scherzo/workflows/research.md
```

In another terminal, supervise through the local control API:

```sh
scripts/scherzoctl ps
scripts/scherzoctl attach <session-id>
scripts/scherzoctl prompt <session-id> "Please produce a Linear-ready result summary."
```

For the first run, create or select one Linear issue in an active state with exactly one `workflow:research` label. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
