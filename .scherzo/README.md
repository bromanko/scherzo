# Scherzo repo workflows

This directory contains checked-in Scherzo workflow definitions for dogfooding this repository.

## Convention

- Put the repo dogfood runtime config in `.scherzo/scherzo.yaml`.
- Keep this repository's checked-in dogfood workflow bundle reachable at `.scherzo/workflows`; in this repository that path is the tracked symlink `.scherzo/workflows -> ../workflows/dogfood`.
- Keep workflow-internal file references pointed at `.scherzo/workflows/...` rather than the repository-root `workflows/dogfood/...` target so the bundle remains portable when copied or mounted into another repository.
- Put workflow-required agent guidance directly in checked-in prompt templates (or a workflow-owned bundled include mechanism when one exists) so consuming repositories do not have to vendor Pi skills.
- Put runtime jj workspaces under `.scherzo/workspaces/<workflow-name>/`; they are ignored by git.
- Config-relative paths are resolved from `.scherzo/scherzo.yaml`, so this repository uses `workspace.root: workspaces` to land at repo-root `.scherzo/workspaces`.
- Populate Scherzo workspaces with `jj workspace add`, not separate `git clone` checkouts. New root workspaces use the canonical `SCHERZO_JJ_WORKSPACE_*` driver env configured under `.scherzo/scherzo.yaml` (`main@scherzo-agent` for this dogfood profile) and fall back through the selected local base branch and finally `@` only when no canonical base remote/branch is configured; set `SCHERZO_JJ_WORKSPACE_BASE` to override this for deliberate local dogfooding.
- Keep dogfood workspace lifecycle policy explicit: `.scherzo/scherzo.yaml` defines `workspace.drivers.dogfood-jj` as the documented default, and implementation/review workflows select it with top-level `workspace.driver: dogfood-jj`. Command-only root-maintenance schedules use no-op drivers and explicitly resolve `SCHERZO_REPO_ROOT` before touching the root checkout; `origin-sync` has its own no-op driver so its configured remote is not launcher-only environment.
- The `dogfood-jj` workspace driver uses the trusted command `$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj` for lifecycle operations and self-describes the dogfood capabilities `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, and `publish-commit-stack` from `describe --json`. The legacy `publish-change` name has been removed; checked-in dogfood workflows that publish to GitHub must require `publish-commit-stack`. The normative driver contract is [`docs/specs/WORKSPACE_DRIVER_SPEC.md`](../docs/specs/WORKSPACE_DRIVER_SPEC.md); hook-backed profile configuration and `publish-change` migration guidance are covered by [`docs/runbooks/workspace-driver-migration.md`](../docs/runbooks/workspace-driver-migration.md). Do not add new dogfood hook snippets as the current convention.
- Use `scripts/scherzo-pi` through the checked-in `agents.runtime.pi.executable`/`args` wrapper so workflows such as research and bundle-based ExecPlan can select `openai-codex/gpt-5.5:xhigh` while other workflows keep the default pi model.
- Keep machine-specific variants as `.scherzo/workflows/**/*.local.yaml`, `.scherzo/workflows/**/*.local.yml`, `.scherzo/scherzo.local.yaml`, or `.scherzo/scherzo.local.yml`; they are ignored by git.
- Do not put secrets in workflow files. Use environment variables for secrets and deployment-specific values.

The repo `.gitignore` intentionally ignores runtime `.scherzo/*` state while allowing this README, `.scherzo/scherzo.yaml`, the tracked `.scherzo/workflows` symlink, and this repository's checked-in `workflows/dogfood/**` bundle target to be checked in.

## Workflow-packaged guidance and portability

Bundle-based ExecPlan workflows must not depend on a consuming repository's personal or repo-local Pi skill installation. The draft, review, review-incorporation, revision, and implementation prompts embed the required ExecPlan authoring, adversarial review, living-document, and implementation guidance directly in workflow prompt files reachable under `.scherzo/workflows/prompts/`. Review-lane JSON Schemas live under `.scherzo/workflows/schemas/`, so consuming repositories can use the workflows without copying a separate schema directory. A clean consuming repository that points `.scherzo/scherzo.yaml` at this workflow bundle can therefore prepare those prompts without committing local ExecPlan skill files.

Command steps that need Scherzo helpers should resolve the configured repository root before invoking scripts, for example:

```sh
bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}
if [ -z "$bundle_dir" ]; then
  bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"
fi
repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}
"$bundle_dir/scripts/scherzo-execplan" validate-review-doc --path docs/plans/example.md
```

When updating workflow-packaged guidance or helper invocations, run the workflow portability validation through the packaged CLI and positive runtime environment:

```sh
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability --print-build-logs
nix develop .#workflow-portability
python3 scripts/scherzo-workflow-portability check --repo-root . --scherzo scherzo --output-dir tmp/scherzo-workflow-portability/manual
```

The check writes `workflow-portability-report.v1.json` under the chosen output directory, stages the checked-in `.scherzo/workflows` bundle into a temporary runtime root, validates it with packaged `scherzo doctor --check workflow-config`, and records every checked-in workflow as either fake-executed or explicit `load-only` coverage with an expansion path. Keep nested workflow execution on packaged `scherzo workflow run`; do not restore `gleam run -- workflow run` to an active workflow path.

The implementation review workflows now use repo-local staged review artifacts from `.scherzo/workflows/scripts/scherzo-review` instead of local `/review` pi commands or language-specific local skills. There is no remaining routed-workflow dependency on local language-specific pi review skills; LIV-115 remains the tracking issue for the broader staged code review workflow cutover, so do not add language-specific review skills as required dogfood configuration.

## Required environment

The checked-in workflow expects:

```sh
export LINEAR_API_KEY=lin_api_...
# Optional. Defaults to Scherzo Core in devenv; used by the repo-local linear wrapper when creating issues without --project.
export LINEAR_DEFAULT_PROJECT="Scherzo Core"
# Optional. Defaults to openai-codex/gpt-5.5:xhigh for workflow:research.
export SCHERZO_RESEARCH_PI_MODEL=openai-codex/gpt-5.5:xhigh
# Optional. Defaults to openai-codex/gpt-5.5:xhigh for bundle-based ExecPlan drafting, revision, and implementation workflows.
export SCHERZO_EXECPLAN_PI_MODEL=openai-codex/gpt-5.5:xhigh
# Optional. Defaults to the GitHub owner/repo configured in workspace driver env.
# Set this only when running a local override profile or workflow bundle outside this checked-in config.
export SCHERZO_GITHUB_REPO=scherzo-systems/scherzo
# Optional. Defaults to the current non-draft PR behavior when unset.
# Set true to create draft PRs; set false to force ready-for-review PRs.
export SCHERZO_PR_DRAFT=false
# Optional. Defaults to the repository root inferred from .scherzo/scherzo.yaml.
export SCHERZO_REPO_ROOT=$(pwd)
# Optional local overrides for the scheduled origin-sync job. The checked-in
# origin-sync profile configures these to scherzo-agent/main.
export SCHERZO_ORIGIN_SYNC_REMOTE=scherzo-agent
export SCHERZO_ORIGIN_SYNC_BRANCH=main
# Optional. Defaults for the scheduled workspace-cleanup job.
export SCHERZO_CLEANUP_WORKSPACE_ROOT="$SCHERZO_REPO_ROOT/.scherzo/workspaces"
# Legacy alias also accepted by the workflow.
export SCHERZO_WORKSPACE_CLEANUP_ROOT="$SCHERZO_REPO_ROOT/.scherzo/workspaces"
```

The checked-in `tracker.linear.project` targets the Linear project `scherzo-f6f4bc92d6d7`. `SCHERZO_REPO_ROOT` is optional for checked-in workflows in this repository. The `dogfood-jj` driver can infer the repository root from `.scherzo/scherzo.yaml`, while setting `SCHERZO_REPO_ROOT` makes the driver command independent of the current directory layout.

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
# Optional. Defaults to scherzo-agent. This is the expected remote validated by
# the helper scripts; checked-in driver behavior comes from .scherzo/scherzo.yaml.
SCHERZO_AGENT_JJ_WORKSPACE_REMOTE=scherzo-agent
# Optional. Defaults to SCHERZO_AGENT_JJ_WORKSPACE_REMOTE.
SCHERZO_AGENT_JJ_WORKSPACE_PUBLISH_REMOTE=scherzo-agent
# Optional. Defaults to main.
SCHERZO_AGENT_JJ_WORKSPACE_BASE_BRANCH=main
# Optional. Defaults to scherzo-systems/scherzo.
SCHERZO_AGENT_GITHUB_REPO=scherzo-systems/scherzo
```

The older local aliases `SCHERZO_AGENT_PR_REMOTE` and `SCHERZO_AGENT_PR_REPO` are still accepted by the helper scripts when the new agent names are unset, but they no longer act as the primary jj driver configuration boundary. Keep non-secret driver policy in `.scherzo/scherzo.yaml` under `workspace.drivers.<name>` friendly fields or `env`.

`SCHERZO_AGENT_GITHUB_TOKEN` should be a fine-grained GitHub token limited to `scherzo-systems/scherzo`. It needs metadata read access, pull request read/write access, and issue read/write access for PR creation, PR lookup, feedback collection, and PR comments. The scripts map this value to `GH_TOKEN` and `GITHUB_TOKEN` only inside the profile and set `GH_CONFIG_DIR` to ignored `.scherzo/gh-agent/`; they do not run `gh auth login` or write the token to GitHub CLI config.

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
jj git remote add scherzo-agent git@github-scherzo-agent:scherzo-systems/scherzo.git
```

If the remote already exists, inspect it with `jj git remote list --color=never` and make sure the `scherzo-agent` URL is SSH-based and uses the host from `SCHERZO_AGENT_SSH_HOST`, such as `git@github-scherzo-agent:scherzo-systems/scherzo.git` or `ssh://git@github-scherzo-agent/scherzo-systems/scherzo.git`.

Run the non-networked local check first. It reports whether credentials are configured without printing token values, writes ignored `.scherzo/jj-agent.toml` from the agent git name and email, uses ignored `.scherzo/gh-agent/` for GitHub CLI config isolation, and shows the effective git and jj identity:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check
```

With real credentials and the agent remote in place, run the live identity check before starting the daemon:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami
```

This command verifies that `gh api user` returns `SCHERZO_AGENT_GITHUB_LOGIN`, the token can read `SCHERZO_AGENT_GITHUB_REPO`, the Linear viewer query succeeds with the agent API key, git and jj show the agent identity, the configured publication remote uses the expected SSH host alias, and `ssh -T git@$SCHERZO_AGENT_SSH_HOST` authenticates as the same GitHub login.

Start Scherzo through the guarded run command only when the operator is ready for the daemon to poll Linear and dispatch work:

```sh
direnv exec . devenv shell -P scherzo-agent scherzo-agent-run
```

`scherzo-agent-run` performs the same strict checks as `scherzo-agent-whoami`, confirms `LINEAR_API_KEY` was derived from `SCHERZO_AGENT_LINEAR_API_KEY`, and then runs `.scherzo/scherzo.yaml` through `scripts/scherzo-start-runner` under the isolated agent environment. The runner wraps `gleam run -- ...` so Ctrl-C is translated into SIGTERM and the daemon can follow its graceful shutdown path without relying on the removed `scherzo-start` alias.

## Linear project contract

`tracker.linear.check_setup: true` makes Scherzo fail readiness checks if the Linear project drifts from this checked-in dogfood contract. The project must expose these states for every associated Linear team:

- `Todo`
- `In Progress`
- `Done`
- `Canceled`
- `Duplicate`
- `Triage`

The Linear project must also make these workflow labels assignable:

- `workflow:research`
- `workflow:implementation`
- `workflow:execplan`
- `workflow:execplan-revision`
- `workflow:execplan-implementation`
- `workflow:merge-conflict-resolution`
- `needs-workflow`
- `needs-clarification`

The former `-v2` ExecPlan workflow labels are retired. Do not assign them to new issues; use the canonical labels above.

Run the contract check after changing workflow labels, states, or team membership:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check tracker-contract .scherzo/scherzo.yaml
```

Use `tracker-contract`; the old Linear-named doctor check alias is retired.

## Dogfood workflows

The checked-in workflows are:

- `workflow:research` — investigates with `openai-codex/gpt-5.5:xhigh`, writes `research-findings.md`, verifies the file, and uses that Markdown as the inline Linear result text.
- `workflow:implementation` — fetches Linear-backed task context directly, implements without requiring an ExecPlan, detects changed files across the full workflow diff, generates a local schema-versioned review brief under `$SCHERZO_RUN_ROOT/artifacts/review/`, runs repo-local staged review lanes through `.scherzo/workflows/scripts/scherzo-review`, validates format and tests through direnv, publishes a final jj bookmark as a GitHub PR, and lets Scherzo delete the workspace only after publication. If the workflow stops before publication, a `.scherzo-keep-workspace` marker keeps the run directory for operator recovery instead of deleting unpushed work.
- `workflow:execplan` — drafts a concise human-reviewable ExecPlan Markdown review document under `docs/plans/` by default, or under a repository-relative destination requested by the task (for example, `Create an execplan at doobar/docs/plans`), retains the mechanical implementation pack as a structured Scherzo artifact, publishes only the review surface for humans, and creates or reuses a Linear-backed implementation task in `Backlog` that references the retained bundle.
- `workflow:execplan-revision` — consumes a task containing `Bundle ref:` / `Bundle sha256:` plus actionable feedback, validates the retained ExecPlan bundle, revises the review document and implementation pack only when needed, and emits a superseding retained bundle.
- `workflow:execplan-implementation` — validates a retained ExecPlan bundle from task context, implements from the prepared review document plus implementation pack, fails closed if they conflict, publishes a PR, and emits a retained `code_change_bundle` artifact.
- `workflow:merge-conflict-resolution` — manually resolves merge conflicts for one same-repository GitHub PR or branch referenced by a Linear-backed task. The task should include an unambiguous target such as `Resolve conflicts for PR #51`, `scherzo-systems/scherzo#51`, a full GitHub PR URL, or `Branch: feature/name`. The workflow creates a merge commit from the target branch and the configured base branch, lets the agent edit only files that jj reports as conflicted, fails if non-conflicted tracked files change or ambiguity requires a behavior choice, validates through direnv, and fast-forwards the target branch only after validation passes.
- scheduled `origin-sync` — every 15 minutes, runs `.scherzo/workflows/scripts/scherzo-jj-origin-sync` from the repository root. The checked-in `origin-sync` profile sets `SCHERZO_ORIGIN_SYNC_REMOTE=scherzo-agent` and `SCHERZO_ORIGIN_SYNC_BRANCH=main`, so it fetches the same agent remote used by the jj workspace driver, rebases the local root stack onto `main@scherzo-agent` only when the working-copy change `@` has no file changes, skips successfully when `@` is dirty, and fails for existing or newly-created jj conflicts so the scheduled failure reporter can surface manual attention.
- scheduled `workspace-cleanup` — every hour, runs the canonical top-level `scherzo cleanup --root <workspace-root> --json --yes` from a no-op scratch workspace. It defaults to `.scherzo/workspaces`, honors `SCHERZO_CLEANUP_WORKSPACE_ROOT` and the legacy `SCHERZO_WORKSPACE_CLEANUP_ROOT` override, prefers packaged `SCHERZO_BIN`/`scherzo` when available, and keeps `SCHERZO_CTL`/`scherzoctl` as compatibility fallbacks without mixing their warnings into the JSON cleanup payload.

Use the research workflow for the first supervised run:

```sh
direnv exec . gleam run -- doctor --check tracker-smoke .scherzo/scherzo.yaml
direnv exec . gleam run -- doctor --check tracker-contract .scherzo/scherzo.yaml
direnv exec . gleam run -- doctor --check pi-probe .scherzo/scherzo.yaml
LINEAR_API_KEY=$LINEAR_API_KEY SCHERZO_REPO_ROOT=$(pwd) \
  nix run .#scherzo -- .scherzo/scherzo.yaml
```

In another terminal, supervise through the local control API:

```sh
scripts/scherzoctl ps
scripts/scherzoctl attach <session-id>
scripts/scherzoctl prompt <session-id> "Please produce a task-ready result summary."
```

For the first run, create or select one Linear-backed task in an active state with exactly one workflow label such as `workflow:research`, `workflow:implementation`, `workflow:execplan`, `workflow:execplan-revision`, `workflow:execplan-implementation`, or `workflow:merge-conflict-resolution`. Use `workflow:implementation` for focused tasks whose title, description, labels, and recent comments are enough to implement directly. For ExecPlan implementation tasks, keep the generated `Bundle ref:` and `Bundle sha256:` lines intact. For merge-conflict tasks, reference exactly one same-repository PR or branch in the title, description, or a comment with text like `Resolve conflicts for PR #51`, `scherzo-systems/scherzo#51`, a full GitHub PR URL, or `Branch: feature/name`; if the agent cannot resolve conflicts without choosing behavior, the workflow fails and leaves the workspace retained for operator recovery. The ExecPlan authoring workflow creates its follow-up implementation Linear tasks in `Backlog`; after the review doc PR is merged and the bundle is retained, move that task to `Todo` to make it eligible for dispatch. Handoff moves claimed tasks to `In Progress`, successful tasks to `Done`, and failed tasks to `Triage` using checked Linear state IDs, and success comments include the workflow result inline rather than as a Markdown attachment. Invalid workflow-label tasks are also commented on and moved to `Triage`; adding exactly one configured `workflow:*` label and moving the task back to `Todo` makes it eligible for dispatch. Keep `linear_commands.enabled: false`; use `scherzoctl` until the operator loop feels boring.
