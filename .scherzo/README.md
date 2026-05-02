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
- Use `scripts/scherzo-jj-workspace` from YAML `workspace.hooks` instead of inlining jj lifecycle shell; pass the workflow name as the second argument. The helper runs `direnv allow .` only during initial trusted workspace creation when `.envrc` is present, so later direnv-backed validation does not fail on a blocked workspace-local `.envrc` without re-approving an `.envrc` an agent may have modified.
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
# Optional. Defaults to openai-codex/gpt-5.5:xhigh for workflow:execplan, workflow:execplan-revision, and workflow:execplan-implementation.
export SCHERZO_EXECPLAN_PI_MODEL=openai-codex/gpt-5.5:xhigh
# Optional. Git remote used by workflow:implementation, workflow:execplan, workflow:execplan-revision, and workflow:execplan-implementation.
export SCHERZO_PR_REMOTE=origin
# Optional. PR base used by workflow:implementation, workflow:execplan, and workflow:execplan-implementation when creating a new PR.
export SCHERZO_PR_BASE=main
# Optional. Defaults to the owner/repo inferred from SCHERZO_PR_REMOTE.
export SCHERZO_PR_REPO=bromanko/scherzo
# Optional. Defaults to the repository root inferred from .scherzo/scherzo.yaml.
export SCHERZO_REPO_ROOT=$(pwd)
```

The checked-in `tracker.project_slug` targets the Linear project `scherzo-f6f4bc92d6d7`. `SCHERZO_REPO_ROOT` is optional for checked-in workflows in this repository, but setting it makes the jj workspace hook independent of the current directory layout.

### Scherzo agent devenv profile

Use the optional `scherzo-agent` devenv profile when Scherzo should act through dedicated agent identities instead of the operator's personal GitHub, Linear, git, or jj identity. The profile is inactive unless it is selected with `devenv shell -P scherzo-agent`; the normal development shell remains unchanged.

Put local agent values in ignored `.env.local` or export them in the shell. Do not commit tokens, real private key paths, or machine-specific agent configuration.

```sh
SCHERZO_AGENT_GITHUB_TOKEN=github_pat_redacted
SCHERZO_AGENT_GITHUB_LOGIN=scherzo-agent-login
SCHERZO_AGENT_LINEAR_API_KEY=lin_api_redacted
SCHERZO_AGENT_GIT_NAME="Scherzo Agent"
SCHERZO_AGENT_GIT_EMAIL=agent-email@example.invalid
# Optional. Defaults to github-scherzo-agent.
SCHERZO_AGENT_SSH_HOST=github-scherzo-agent
# Optional. Defaults to scherzo-agent.
SCHERZO_AGENT_PR_REMOTE=scherzo-agent
# Optional. Defaults to bromanko/scherzo.
SCHERZO_AGENT_PR_REPO=bromanko/scherzo
```

`SCHERZO_AGENT_GITHUB_TOKEN` should be a fine-grained GitHub token limited to `bromanko/scherzo`. It needs metadata read access, pull request read/write access, and issue read/write access for PR creation, PR lookup, feedback collection, and PR comments. The scripts map this value to `GH_TOKEN` and `GITHUB_TOKEN` only inside the profile and set `GH_CONFIG_DIR` to ignored `.scherzo/gh-agent/`; they do not run `gh auth login` or write the token to GitHub CLI config.

`SCHERZO_AGENT_LINEAR_API_KEY` must belong to the Linear actor that should appear on Scherzo issue claims, state transitions, and comments. The scripts map it to `LINEAR_API_KEY` only inside the profile. If the agent-specific key is missing, inherited `LINEAR_API_KEY`, `GH_TOKEN`, and `GITHUB_TOKEN` values are unset so Scherzo does not silently fall back to personal credentials.

Configure SSH outside this repository so the agent remote uses the agent GitHub account. For example, add a host alias to `~/.ssh/config` and keep any 1Password, hardware-key, or private-key details there rather than in `.env.local`:

```sshconfig
Host github-scherzo-agent
  HostName github.com
  User git
  # Configure IdentityAgent / IdentityFile here if your system needs it.
  # With 1Password, this belongs in ~/.ssh/config, not in .env.local.
```

Add a separate remote for agent pushes instead of rewriting `origin`:

```sh
jj git remote add scherzo-agent git@github-scherzo-agent:bromanko/scherzo.git
```

If the remote already exists, inspect it with `jj git remote list --color=never` and make sure the `scherzo-agent` URL is SSH-based and uses the host from `SCHERZO_AGENT_SSH_HOST`, such as `git@github-scherzo-agent:bromanko/scherzo.git` or `ssh://git@github-scherzo-agent/bromanko/scherzo.git`.

Run the non-networked local check first. It reports whether credentials are configured without printing token values, writes ignored `.scherzo/jj-agent.toml` from the agent git name and email, uses ignored `.scherzo/gh-agent/` for GitHub CLI config isolation, and shows the effective git and jj identity:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check
```

With real credentials and the agent remote in place, run the live identity check before starting the daemon:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami
```

This command verifies that `gh api user` returns `SCHERZO_AGENT_GITHUB_LOGIN`, the token can read `SCHERZO_AGENT_PR_REPO`, the Linear viewer query succeeds with the agent API key, git and jj show the agent identity, the configured remote uses the expected SSH host alias, and `ssh -T git@$SCHERZO_AGENT_SSH_HOST` authenticates as the same GitHub login.

Start Scherzo through the guarded run command only when the operator is ready for the daemon to poll Linear and dispatch work:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-run
```

`scherzo-agent-run` performs the same strict checks as `scherzo-agent-whoami`, confirms `LINEAR_API_KEY` was derived from `SCHERZO_AGENT_LINEAR_API_KEY`, and then runs `gleam run -- .scherzo/scherzo.yaml` under the isolated agent environment.

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
- `workflow:execplan-implementation`
- `needs-workflow`
- `needs-clarification`

Run the contract check after changing workflow labels, states, or team membership:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check linear-contract .scherzo/scherzo.yaml
```

## Dogfood workflows

The checked-in workflows are:

- `workflow:research` — investigates with `openai-codex/gpt-5.5:xhigh`, writes `research-findings.md`, verifies the file, and uses that Markdown as the inline Linear result text.
- `workflow:implementation` — fetches Linear ticket context directly, implements without requiring an ExecPlan, detects changed Gleam files across the full workflow diff, runs vendored project-local Gleam review with medium fixes, validates format and tests through direnv, publishes a final jj bookmark as a GitHub PR, and lets Scherzo delete the workspace only after publication. If the workflow stops before publication, a `.scherzo-keep-workspace` marker keeps the run directory for operator recovery instead of deleting unpushed work.
- `workflow:execplan` — uses repo-local exec-plan skills with `openai-codex/gpt-5.5:xhigh` to draft a plan in `docs/plans/`, adversarially review it, incorporate the review, push a jj bookmark, and open a ready GitHub PR.
- `workflow:execplan-revision` — finds an existing ExecPlan PR referenced by a human-friendly Linear issue phrase such as `Revise PR #51`, fetches the latest PR head, collects top-level/review/inline GitHub feedback, revises only the plan file, pushes the existing PR branch, and posts one concise PR acknowledgement.
- `workflow:execplan-implementation` — finds exactly one `docs/plans/*.md` ExecPlan referenced by the Linear issue, implements it in an isolated jj workspace using the same shared implementation helper as `workflow:implementation`, detects changed Gleam files across the full workflow diff, runs vendored project-local Gleam review with medium fixes, validates format and tests through direnv, publishes a final jj bookmark as a GitHub PR, and lets Scherzo delete the workspace only after publication. If the workflow stops before publication, a `.scherzo-keep-workspace` marker keeps the run directory for operator recovery instead of deleting unpushed work.

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

For the first run, create or select one Linear issue in an active state with exactly one workflow label such as `workflow:research`, `workflow:implementation`, `workflow:execplan`, `workflow:execplan-revision`, or `workflow:execplan-implementation`. Use `workflow:implementation` for focused tickets whose title, description, labels, and recent comments are enough to implement directly. For revision issues, reference the PR in the title, description, or a comment with text like `Revise PR #51`, `bromanko/scherzo#51`, or a full GitHub PR URL; bare `#51` is intentionally not enough. For ExecPlan implementation issues, reference exactly one checked-in plan path such as `docs/plans/LIV-123-example.md` in the title, description, or a comment. Handoff moves claimed issues to `In Progress`, successful issues to `Done`, and failed issues to `Needs Workflow` using checked Linear state IDs, and success comments include the workflow result inline rather than as a Markdown attachment. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
