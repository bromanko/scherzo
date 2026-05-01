# Scherzo repo workflows

This directory contains checked-in Scherzo workflow definitions for dogfooding this repository.

## Convention

- Put the repo dogfood runtime config in `.scherzo/scherzo.yaml`.
- Put versioned YAML workflow DAGs in `.scherzo/workflows/*.yaml`.
- Put prompt templates referenced by YAML DAG agent steps in `.scherzo/workflows/prompts/*.md`.
- Put workflow-required pi skills in `.pi/skills/` so jj workspaces and other operators use the same skill snapshot.
- Put runtime jj workspaces under `.scherzo/workspaces/<workflow-name>/`; they are ignored by git.
- Config-relative paths are resolved from `.scherzo/scherzo.yaml`, so this repository uses `workspace.root: workspaces` to land at repo-root `.scherzo/workspaces`.
- Populate Scherzo workspaces with `jj workspace add --revision @`, not separate `git clone` checkouts, so dogfood runs share the local repository and can see the coordinating workspace's current snapshot while still making their own child working-copy changes.
- Use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks` instead of inlining jj lifecycle shell; pass the workflow name as the second argument.
- Use `scripts/scherzo-pi` as the checked-in `pi.command` wrapper so workflows such as research, execplan, and execplan-revision can select `openai-codex/gpt-5.5:xhigh` while other workflows keep the default pi model.
- Keep machine-specific variants as `.scherzo/workflows/**/*.local.yaml`, `.scherzo/workflows/**/*.local.yml`, `.scherzo/scherzo.local.yaml`, or `.scherzo/scherzo.local.yml`; they are ignored by git.
- Do not put secrets in workflow files. Use environment variables for secrets and deployment-specific values.

The repo `.gitignore` intentionally ignores runtime `.scherzo/*` state while allowing this README, `.scherzo/scherzo.yaml`, and `.scherzo/workflows/**` to be checked in.

## Required environment

The checked-in workflow expects:

```sh
export LINEAR_API_KEY=lin_api_...
# Optional. Defaults to openai-codex/gpt-5.5:xhigh for workflow:research.
export SCHERZO_RESEARCH_PI_MODEL=openai-codex/gpt-5.5:xhigh
# Optional. Defaults to openai-codex/gpt-5.5:xhigh for workflow:execplan and workflow:execplan-revision.
export SCHERZO_EXECPLAN_PI_MODEL=openai-codex/gpt-5.5:xhigh
# Optional. Git remote used by workflow:execplan and workflow:execplan-revision.
export SCHERZO_PR_REMOTE=origin
# Optional. PR base used by workflow:execplan when creating a new PR.
export SCHERZO_PR_BASE=main
# Optional. Defaults to the owner/repo inferred from SCHERZO_PR_REMOTE.
export SCHERZO_PR_REPO=bromanko/scherzo
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
- `workflow:execplan`
- `workflow:execplan-revision`
- `needs-workflow`
- `needs-clarification`

Run the contract check after changing workflow labels, states, or team membership:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check linear-contract .scherzo/scherzo.yaml
```

## Dogfood workflows

The checked-in workflows are:

- `workflow:research` — investigates with `openai-codex/gpt-5.5:xhigh`, writes `research-findings.md`, verifies the file, and uses that Markdown as the inline Linear result text.
- `workflow:implementation` — implement, run format/tests, review, apply feedback, final validate, and summarize.
- `workflow:execplan` — uses repo-local exec-plan skills with `openai-codex/gpt-5.5:xhigh` to draft a plan in `docs/plans/`, adversarially review it, incorporate the review, push a jj bookmark, and open a ready GitHub PR.
- `workflow:execplan-revision` — finds an existing ExecPlan PR referenced by a human-friendly Linear issue phrase such as `Revise PR #51`, fetches the latest PR head, collects top-level/review/inline GitHub feedback, revises only the plan file, pushes the existing PR branch, and posts one concise PR acknowledgement.

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

For the first run, create or select one Linear issue in an active state with exactly one workflow label such as `workflow:research`, `workflow:implementation`, `workflow:execplan`, or `workflow:execplan-revision`. For revision issues, reference the PR in the title, description, or a comment with text like `Revise PR #51`, `bromanko/scherzo#51`, or a full GitHub PR URL; bare `#51` is intentionally not enough. Handoff moves claimed issues to `In Progress`, successful issues to `Done`, and failed issues to `Needs Workflow` using checked Linear state IDs, and success comments include the workflow result inline rather than as a Markdown attachment. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
