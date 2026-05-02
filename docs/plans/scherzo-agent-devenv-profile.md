# Add a Scherzo agent devenv profile for GitHub, Linear, and git identity isolation

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can start Scherzo from a dedicated devenv profile and know that GitHub API calls, Linear API calls, GitHub token lookups, git and jj pushes, and commit metadata use agent accounts rather than the operator's personal accounts. The visible result is a small set of commands such as `direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami` and `direnv exec . devenv shell -P scherzo-agent scherzo-agent-run` that activate only the agent credentials, verify the effective identity without printing secrets, refuse to run when the GitHub token, Linear token, expected GitHub login, SSH host alias, agent remote, or jj identity do not line up, and run `.scherzo/scherzo.yaml` under that environment.

The goal is not to make Scherzo itself a credential manager. The goal is to make the safe path boring: keep secrets in ignored local environment files, keep permanent defaults in `devenv.nix`, and make the daemon process inherit an explicitly agent-scoped environment.

## Problem Framing and Constraints

Today Scherzo inherits the environment of the shell that starts it. That is flexible, but it is easy for the daemon and the agents it launches to inherit a human operator's `LINEAR_API_KEY`, `GH_TOKEN`, `GITHUB_TOKEN`, SSH agent, git identity, or jj user settings. The checked-in workflows claim Linear issues, move Linear issue states, post Linear comments, push jj bookmarks, and create or update GitHub pull requests through `.scherzo/scherzo.yaml`, `scripts/scherzo-execplan`, and `scripts/scherzo-execplan-revision`, so using the wrong identity is a real operational risk rather than a hypothetical future concern.

This plan must not commit any secret. It must not require operators to put GitHub tokens or Linear API keys in `devenv.nix`, workflow YAML, or plan files. It must work with the repository's existing direnv setup, because `.envrc` already loads ignored `.env` and `.env.local` files. It must keep the change small: one devenv profile, three devenv scripts, and documentation are enough.

The plan assumes the operator has already created a GitHub agent account, can provision a fine-grained GitHub token, can make SSH authentication for that account available through the operator's system SSH configuration or agent, and can provision a Linear API key whose activity should be attributed to the agent actor in Linear. Those account-management steps happen outside this repository. This repository should only make it easy to consume those credentials safely.

The GitHub token must be scoped to the `bromanko/scherzo` repository. It needs read access for metadata and pull requests, and write access for pull requests and issue comments so `gh pr create`, `gh pr view`, feedback collection, and `gh pr comment` can work. It does not need to be used for git transport because `jj git push` should use SSH through a dedicated host alias such as `github-scherzo-agent`.

The Linear API key must be issued for the Linear agent identity that should appear on issue comments, issue state transitions, and any other Linear activity performed by Scherzo. The checked-in `.scherzo/scherzo.yaml` reads `tracker.api_key` from `$LINEAR_API_KEY`, and `scripts/scherzo-execplan-revision` also reads `LINEAR_API_KEY` when it looks up the Linear issue that references a PR. The profile must therefore derive `LINEAR_API_KEY` from an agent-specific source variable, not inherit a personal `LINEAR_API_KEY` from the outer shell.

The profile must avoid silent fallback to personal credentials. If `SCHERZO_AGENT_GITHUB_TOKEN` is missing or wrong, the scripts must not continue with inherited `GH_TOKEN`, inherited `GITHUB_TOKEN`, or the operator's normal `gh` login. If `SCHERZO_AGENT_LINEAR_API_KEY` is missing, the scripts must not continue with an inherited personal `LINEAR_API_KEY`. If the configured SSH host alias or remote is missing, the scripts must fail before starting the daemon rather than letting a later push choose another credential source.

## Strategy Overview

Add a `scherzo-agent` profile to `devenv.nix`. The profile adds GitHub CLI, OpenSSH, Jujutsu, and curl tooling, defines non-secret defaults for the Scherzo PR remote and repository, and provides scripts that export the agent-specific runtime environment before running checks or starting Scherzo.

The profile will read secrets and machine-specific values from environment variables supplied by the operator's ignored `.env.local` or by the shell. The key variables are `SCHERZO_AGENT_GITHUB_TOKEN` and `SCHERZO_AGENT_LINEAR_API_KEY`. The scripts map `SCHERZO_AGENT_GITHUB_TOKEN` to both `GH_TOKEN` for the GitHub CLI and `GITHUB_TOKEN` for tools that expect that spelling, and they map `SCHERZO_AGENT_LINEAR_API_KEY` to `LINEAR_API_KEY` for Scherzo and the revision helper script. The scripts also set `GH_CONFIG_DIR` to an ignored `.scherzo/gh-agent` directory so `gh` does not read or write the operator's normal GitHub CLI config. They set `GIT_AUTHOR_NAME`, `GIT_AUTHOR_EMAIL`, `GIT_COMMITTER_NAME`, `GIT_COMMITTER_EMAIL`, `SCHERZO_PR_REMOTE`, `SCHERZO_PR_REPO`, and `JJ_CONFIG` so raw git, jj, and checked-in Scherzo workflow scripts share the same agent identity. By default they unset inherited `GIT_SSH_COMMAND` and let the selected remote's SSH host alias choose the key through the operator's system SSH configuration, including 1Password's SSH agent.

The generated jj config file must be the last path in `JJ_CONFIG`, after any pre-existing `JJ_CONFIG` value, because later jj config files override earlier ones. This preserves any intentionally supplied non-identity config while ensuring `[user] name` and `[user] email` come from `.scherzo/jj-agent.toml`.

The profile will not rewrite or mutate existing remotes automatically. Instead, documentation and the verification script require an explicit `scherzo-agent` remote whose SSH URL uses a dedicated host alias such as `git@github-scherzo-agent:bromanko/scherzo.git` or `ssh://git@github-scherzo-agent/bromanko/scherzo.git`. The host alias lives in the operator's personal `~/.ssh/config` and may point at 1Password's SSH agent, a hardware key, or any other local SSH setup. This is proportionate because it avoids surprising changes to the operator's normal `origin` remote, avoids storing any key path in the repository, and still makes the Scherzo workflow scripts use the agent remote through `SCHERZO_PR_REMOTE=scherzo-agent`.

## Alternatives Considered

The simplest alternative is to document a long shell command that exports every variable before running Scherzo. That works once, but it is easy to omit a variable, copy it into the wrong terminal, or accidentally reuse a personal token. A named devenv profile is easier to repeat and review.

Another alternative is to check in `.env` or enable devenv's dotenv integration. That is rejected because secrets must stay out of version control, and devenv dotenv-style loading can copy environment-file contents into the Nix store. The existing `.envrc` already loads `.env` and `.env.local` through direnv without requiring those secrets to be embedded in `devenv.nix`.

Another alternative is to modify Scherzo's Gleam configuration format to support per-workflow environment maps. That might be useful later, but it is larger than necessary for the current operator problem. Scherzo already inherits the daemon process environment, `.scherzo/scherzo.yaml` already reads `$LINEAR_API_KEY`, and the checked-in workflow scripts already read `SCHERZO_PR_REMOTE`, `SCHERZO_PR_REPO`, `LINEAR_API_KEY`, `GH_TOKEN`, and related process environment values.

Another alternative is to force all GitHub URLs through a global git URL rewrite in the profile. That could protect accidental HTTPS remote use, but it is too surprising for a first pass because it changes how every git command in the shell interprets GitHub URLs. The safer first pass is explicit remote selection plus a verification script that warns or fails when the target remote is missing or not SSH-based.

## Risks and Countermeasures

The main safety risk is printing or storing the GitHub token or Linear API key. Countermeasure: the profile scripts must never echo token values. They may print `configured` or `missing`, but not the token. The only checked-in variable names for these secrets are `SCHERZO_AGENT_GITHUB_TOKEN` and `SCHERZO_AGENT_LINEAR_API_KEY`; their values remain in `.env.local` or the operator's shell. The scripts must not run `gh auth login` or write a token to `GH_CONFIG_DIR`.

The main credential fallback risk is that `gh` might use a personal token from inherited `GH_TOKEN`, inherited `GITHUB_TOKEN`, or the operator's normal GitHub CLI config, or that Scherzo might use a personal Linear key from inherited `LINEAR_API_KEY`. Countermeasure: the scripts derive `GH_TOKEN` and `GITHUB_TOKEN` only from `SCHERZO_AGENT_GITHUB_TOKEN`, derive `LINEAR_API_KEY` only from `SCHERZO_AGENT_LINEAR_API_KEY`, unset those public credential names when the agent source variables are absent, set `GH_CONFIG_DIR=$PWD/.scherzo/gh-agent`, require `SCHERZO_AGENT_GITHUB_LOGIN` for live checks and runs, and compare `gh api user --jq .login` to that expected login.

The main identity risk is that raw `git push origin` might still use the human account if `origin` is an HTTPS remote backed by a credential helper, or if the agent remote uses the generic `github.com` SSH host that is configured for a personal key. Countermeasure: the checked-in workflows must use `SCHERZO_PR_REMOTE=scherzo-agent`, the verification script must inspect `jj git remote list --color=never` or `git remote get-url scherzo-agent`, fail if the target remote is absent or not SSH-based, and fail unless the remote host equals `SCHERZO_AGENT_SSH_HOST`, which defaults to `github-scherzo-agent`. Documentation must say that the host alias is responsible for selecting the agent key and that arbitrary pushes to HTTPS remotes, to `origin`, or to remotes other than `SCHERZO_PR_REMOTE` are outside the guarantee.

The main usability risk is making every interactive shell fail if a token is missing. Countermeasure: the profile itself should remain optional and inert until a profile script runs. `scherzo-agent-env-check` may report missing values without contacting GitHub, while `scherzo-agent-whoami` and `scherzo-agent-run` should fail fast with clear messages. This lets developers still enter the base devenv shell without agent credentials, while `scherzo-agent-run` refuses to start a daemon that would inherit an incomplete identity.

The main configuration drift risk is that `devenv.nix` changes could break normal development. Countermeasure: add the profile as an optional profile only; the default shell and existing `scripts.check` behavior must remain unchanged. Run the normal Gleam format and test commands after the change.

The main toolchain risk is that a clean agent profile might not have `jj` even though the workflow scripts require it, or might not have `curl` for the read-only Linear identity check. Countermeasure: add `pkgs.jujutsu` and `pkgs.curl` to the `scherzo-agent` profile packages alongside `pkgs.gh` and `pkgs.openssh`, and make the local check print the resolved `jj` and `curl` binaries before checking identities.

The main external-service risk is that GitHub CLI, SSH verification, or Linear verification depends on network, credentials, and repository or workspace permissions. Countermeasure: provide both a local environment check and required live identity checks before running the daemon. The local check verifies environment mapping without contacting GitHub or Linear. The live check proves the GitHub token login, read access to `SCHERZO_PR_REPO`, the SSH host alias's GitHub login, the SSH remote shape, and a read-only Linear API call before Scherzo starts work.

## Progress

- [x] (2026-05-01 00:00Z) Reviewed current repository setup relevant to agent credentials: `.envrc`, `.gitignore`, `devenv.nix`, `.scherzo/scherzo.yaml`, `.scherzo/README.md`, `scripts/scherzo-execplan`, `scripts/scherzo-execplan-revision`, and `scripts/scherzo-pi`.
- [x] (2026-05-01 00:00Z) Confirmed `devenv --help` supports manual profiles with `-P` / `--profile`.
- [x] (2026-05-01 00:00Z) Reviewed the plan for implementability and tightened profile validation around credential fallback, jj config ordering, SSH isolation, required GitHub login comparison, and profile evaluation commands.
- [x] (2026-05-01 00:00Z) Incorporated the operator decision to use a separate Linear API key so Linear comments and issue state activity come from the agent identity rather than the human operator.
- [x] (2026-05-01 00:00Z) Incorporated the operator decision to use system SSH configuration and 1Password-backed keys through a dedicated SSH host alias rather than requiring a private key path in repository-local configuration.
- [ ] Add the optional `scherzo-agent` profile and scripts to `devenv.nix`.
- [ ] Document local secret variables, agent remote setup, and run commands in `.scherzo/README.md`.
- [ ] Validate that normal development commands still pass without the profile.
- [ ] Validate the agent profile with local checks and, when real credentials are available, live `gh` and SSH identity checks.

## Surprises & Discoveries

- Observation: `.envrc` already loads `.env` and `.env.local`, and both files are already ignored by `.gitignore`.
  Evidence: `.envrc` contains `dotenv_if_exists .env` and `dotenv_if_exists .env.local`; `.gitignore` contains `.env`, `.env.local`, and `.env.*.local`.

- Observation: Local Scherzo runtime state under `.scherzo/` is ignored except for the checked-in README, top-level config, and workflow definitions.
  Evidence: `.gitignore` ignores `.scherzo/*` and then unignores `.scherzo/README.md`, `.scherzo/scherzo.yaml`, `.scherzo/workflows/`, and `.scherzo/workflows/**`.

- Observation: The checked-in ExecPlan scripts already support selecting a non-default PR remote and repository, and the revision helper already consumes the Scherzo Linear API key from process environment.
  Evidence: `scripts/scherzo-execplan` reads `SCHERZO_PR_REMOTE`, `SCHERZO_PR_BASE`, and `SCHERZO_PR_REPO`; `scripts/scherzo-execplan-revision` reads `SCHERZO_PR_REMOTE`, `SCHERZO_PR_REPO`, and `LINEAR_API_KEY`.

- Observation: The current base `devenv.nix` includes Gleam, Erlang, Rebar3, Node.js, Git, and jq, but not GitHub CLI, Jujutsu, or curl.
  Evidence: `devenv.nix` has `pkgs.gleam`, `pkgs.erlang`, `pkgs.rebar3`, `pkgs.nodejs_22`, `pkgs.git`, and `pkgs.jq` in `packages`; it does not include `pkgs.gh`, `pkgs.jujutsu`, or `pkgs.curl`.

- Observation: `devenv shell -P <profile> --help` exits successfully even for a missing profile because it prints command help instead of evaluating the requested profile.
  Evidence: `direnv exec . devenv shell -P definitely-not-a-profile --help` exited `0`, while `direnv exec . devenv shell -P definitely-not-a-profile true` failed with `Profile 'definitely-not-a-profile' not found`.

- Observation: `JJ_CONFIG` replaces the default jj user config search path when set, can contain multiple paths separated by `:` on Unix-like systems, and later-loaded config files override earlier ones.
  Evidence: `jj help -k config` documents `JJ_CONFIG` as a path or path list and states that config settings loaded later override earlier settings.

- Observation: SSH remote URLs can use a host alias, so the repository remote can select a system SSH configuration entry such as `github-scherzo-agent` without naming a private key file in this repository.
  Evidence: SSH-style Git URLs use the host portion before the colon or slash, for example `git@github-scherzo-agent:bromanko/scherzo.git`; `ssh` resolves that host through the operator's SSH config, where 1Password or another agent can supply the key.

## Decision Log

- Decision: Use a manual devenv profile named `scherzo-agent`.
  Rationale: Profiles keep the normal development shell unchanged and make agent identity activation explicit with `devenv shell -P scherzo-agent`.
  Date: 2026-05-01

- Decision: Use `SCHERZO_AGENT_GITHUB_TOKEN` as the local GitHub secret source and export both `GH_TOKEN` and `GITHUB_TOKEN` from it inside profile scripts.
  Rationale: `gh` expects `GH_TOKEN`, while other GitHub-aware tools and agents often look for `GITHUB_TOKEN`. A Scherzo-specific source variable avoids confusing the agent token with the human token in the outer shell.
  Date: 2026-05-01

- Decision: Use `SCHERZO_AGENT_LINEAR_API_KEY` as the local Linear secret source and export `LINEAR_API_KEY` from it inside profile scripts.
  Rationale: `.scherzo/scherzo.yaml` and `scripts/scherzo-execplan-revision` already consume `LINEAR_API_KEY`. A Scherzo-specific source variable lets the profile prevent silent fallback to a personal Linear key while preserving the existing Scherzo config interface.
  Date: 2026-05-01

- Decision: Use an explicit `scherzo-agent` git remote instead of mutating `origin`.
  Rationale: The operator's normal remote should stay personal and familiar. The checked-in workflows already support `SCHERZO_PR_REMOTE`, so selecting a separate remote is a small and reversible change.
  Date: 2026-05-01

- Decision: Generate or refresh `.scherzo/jj-agent.toml` from environment values in the agent scripts rather than committing an identity file, and put that file last in `JJ_CONFIG`.
  Rationale: The agent commit email is account-specific and should not be guessed or hard-coded in a shared file. `.scherzo/jj-agent.toml` is ignored by the current `.gitignore`, making it a suitable local artifact. Putting it last ensures the agent identity overrides any pre-existing jj user config.
  Date: 2026-05-01

- Decision: Require `SCHERZO_AGENT_GITHUB_LOGIN` for `scherzo-agent-whoami` and `scherzo-agent-run`.
  Rationale: A live command that merely prints the current login still relies on a human noticing a mismatch. Requiring the expected login gives an automated guardrail against accidentally using a personal token or SSH host alias.
  Date: 2026-05-01

- Decision: Set `GH_CONFIG_DIR` to `.scherzo/gh-agent` inside the profile scripts and never run `gh auth login` from those scripts.
  Rationale: `GH_TOKEN` should be the only GitHub CLI credential source for these workflows. An ignored config directory prevents accidental reads from or writes to the operator's personal GitHub CLI config.
  Date: 2026-05-01

- Decision: Prefer a dedicated SSH host alias such as `github-scherzo-agent` and do not require `SCHERZO_AGENT_SSH_KEY` or a repository-local key path.
  Rationale: The operator sources SSH keys from 1Password, and SSH host aliases are the standard way to let system SSH configuration choose the right agent-backed key for a specific remote. The repo only needs to name the remote and expected host alias; private key material and 1Password socket paths stay outside the repository.
  Date: 2026-05-01

- Decision: Unset inherited `GIT_SSH_COMMAND` by default in profile scripts.
  Rationale: An inherited command could force a personal key and bypass the remote host alias. Leaving `GIT_SSH_COMMAND` unset lets `ssh` consult system configuration for `github-scherzo-agent`.
  Date: 2026-05-01

- Decision: Add `pkgs.jujutsu` and `pkgs.curl` to the optional profile packages.
  Rationale: The workflow scripts use `jj` for fetch, push, rebase, bookmark, and identity operations, and the live Linear identity check needs a stable HTTP client. A fresh agent profile must not depend on untracked system-level `jj` or `curl` binaries.
  Date: 2026-05-01

- Decision: Use `devenv shell -P scherzo-agent true` rather than `devenv shell -P scherzo-agent --help` to verify the profile evaluates.
  Rationale: `--help` can exit successfully without loading the requested profile, so it is not a valid profile evaluation test.
  Date: 2026-05-01

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam project. The base development environment is declared in `devenv.nix`. The repository uses direnv through `.envrc`; when a command is run as `direnv exec . <command>`, direnv activates devenv and loads ignored local environment files named `.env` and `.env.local` if they exist.

Scherzo dogfood workflows live under `.scherzo/`. The checked-in orchestrator config is `.scherzo/scherzo.yaml`, and checked-in workflow DAG files live in `.scherzo/workflows/`. The orchestrator config reads the Linear tracker secret from `$LINEAR_API_KEY`, and Scherzo uses that key when it claims issues, moves issue state, and posts handoff comments. The ExecPlan workflow eventually calls `scripts/scherzo-execplan create-pr`, which uses `jj git push` and `gh pr create` or `gh pr view`. The ExecPlan revision workflow calls `scripts/scherzo-execplan-revision`, which uses `LINEAR_API_KEY` to read the source Linear issue, then uses `jj git fetch`, `jj git push`, `gh pr view`, `gh api`, and `gh pr comment` for the GitHub side.

In this plan, `gh` means the GitHub CLI program. `jj` means Jujutsu, the version-control tool this repository uses over a Git repository. A git remote is a named repository URL such as `origin` or `scherzo-agent`. An SSH host alias is a host name in a Git remote URL, such as `github-scherzo-agent`, that `ssh` resolves through the operator's `~/.ssh/config`; this is where 1Password's SSH agent or any other system key source should be configured. A devenv profile is an optional overlay in `devenv.nix` that is activated manually with `devenv shell -P <profile-name>`.

## Preconditions and Verified Facts

The implementer should start from the repository root. Before editing, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

If `direnv exec .` fails because `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. Treat a blocked `.envrc` as environment setup, not a code failure.

The current tree has these relevant facts:

- `.envrc` loads `.env` and `.env.local` through direnv.
- `.gitignore` ignores `.env`, `.env.local`, `.env.*.local`, most runtime state under `.scherzo/`, `tmp/`, and `test/tmp/`.
- `devenv.nix` currently defines base packages and `scripts.check.exec`; it does not define any profiles, and the base package list does not include `pkgs.gh`, `pkgs.jujutsu`, or `pkgs.curl`.
- `.scherzo/README.md` currently documents `LINEAR_API_KEY`, `SCHERZO_RESEARCH_PI_MODEL`, `SCHERZO_EXECPLAN_PI_MODEL`, `SCHERZO_PR_REMOTE`, `SCHERZO_PR_BASE`, `SCHERZO_PR_REPO`, and `SCHERZO_REPO_ROOT`.
- `.scherzo/scherzo.yaml` uses `scripts/scherzo-pi` as the pi command wrapper and routes `execplan` and `execplan-revision` workflows.
- `scripts/scherzo-execplan` defaults `SCHERZO_PR_REMOTE` to `origin` and uses `gh` for PR creation.
- `scripts/scherzo-execplan-revision` defaults `SCHERZO_PR_REMOTE` to `origin` and uses `gh` for PR lookup, feedback collection, pushes, and acknowledgement comments.
- `devenv --help` in the current toolchain shows `-P, --profile <PROFILES>` as the way to activate one or more profiles.

The operator must provide these local values before the live agent run can succeed:

    SCHERZO_AGENT_GITHUB_TOKEN=github_pat_or_fine_grained_token
    SCHERZO_AGENT_GITHUB_LOGIN=scherzo-agent-login
    SCHERZO_AGENT_LINEAR_API_KEY=lin_api_agent_key
    SCHERZO_AGENT_GIT_NAME=Scherzo Agent
    SCHERZO_AGENT_GIT_EMAIL=agent-email@example.invalid
    SCHERZO_AGENT_SSH_HOST=github-scherzo-agent

The token values are secret. The GitHub login, email, and SSH host alias are account-specific or machine-specific. Put them in `.env.local` or export them in the shell; do not commit secrets. The SSH host alias itself is not secret; the key selection and any 1Password agent socket configuration belong in the operator's system SSH configuration, not in this repository.

The token should be a fine-grained GitHub token for `bromanko/scherzo`. Configure repository permissions with metadata read access, pull request read/write access, and issue read/write access. SSH, not the token, is responsible for git push access, so configure the `github-scherzo-agent` SSH host alias to authenticate as the GitHub agent account and grant that account write access to the repository.

`LINEAR_API_KEY` remains the existing Scherzo runtime variable name because `.scherzo/scherzo.yaml` already references `$LINEAR_API_KEY`. The profile should derive it from `SCHERZO_AGENT_LINEAR_API_KEY`, unset inherited `LINEAR_API_KEY` when the agent source key is missing, and require the agent source key before `scherzo-agent-run` starts Scherzo.

## Scope Boundaries

In scope: an optional `scherzo-agent` profile in `devenv.nix`; profile-provided scripts for environment checking, live identity checking, and running Scherzo; profile-only packages for `gh`, OpenSSH, Jujutsu, and curl; documentation in `.scherzo/README.md`; use of ignored `.scherzo/jj-agent.toml` and `.scherzo/gh-agent/` local artifacts; guidance for adding a `scherzo-agent` remote; deriving `LINEAR_API_KEY` from `SCHERZO_AGENT_LINEAR_API_KEY` so Scherzo's Linear activity uses the agent key.

Out of scope: changing Scherzo's Gleam config schema; adding encrypted secret management; committing tokens, key paths, or real account email addresses; storing tokens through `gh auth login`; provisioning or administering the Linear agent account; changing pi model selection; rewriting the operator's existing `origin` remote; changing workflow DAG behavior; adding GitHub account provisioning automation.

## Milestones

Milestone 1 adds the profile skeleton, profile-only tools, and non-network local verification. At the end, the default devenv shell still works as before, `direnv exec . devenv shell -P scherzo-agent true` proves the requested profile evaluates, and `direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check` can prove that the profile maps local variables into the names Scherzo and its scripts will consume without printing secrets or contacting GitHub or Linear.

Milestone 2 adds strict live identity verification. At the end, an operator with real credentials can run `direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami` and see the expected GitHub login from `gh`, read access to the configured repository, a read-only Linear API check using the agent key, the effective git author identity, the effective jj identity, the configured `scherzo-agent` remote, and an SSH authentication result for the same expected login through the expected host alias. This milestone proves the profile is not accidentally using the operator's personal GitHub token, personal Linear key, personal SSH host, or default jj identity.

Milestone 3 adds the guarded run command and documentation. At the end, `.scherzo/README.md` explains the one-time setup, local `.env.local` variables, GitHub token permissions, Linear agent key expectations, remote setup, verification commands, and the final Scherzo startup command. `scherzo-agent-run` starts the daemon only after required variables are present, the ignored jj config has been generated, the target remote is SSH-based, the GitHub token login matches `SCHERZO_AGENT_GITHUB_LOGIN`, and `LINEAR_API_KEY` has been derived from `SCHERZO_AGENT_LINEAR_API_KEY`.

Milestone 4 validates and records outcomes. At the end, normal tests pass, the profile scripts have been exercised with safe dummy values where possible, live checks pass when credentials are available, and this plan's Progress and Outcomes sections record what was proven.

## Plan of Work

Edit `devenv.nix`. Keep the existing base `packages` list and `scripts.check.exec`. Add `profiles.scherzo-agent.module` at the same top level as `packages` and `scripts`. Use the function form shown in the devenv profile documentation, `profiles.scherzo-agent.module = { pkgs, ... }: { ... };`, so the profile can refer to profile-local packages without relying on outer lexical scope. The module should add `pkgs.gh`, `pkgs.openssh`, `pkgs.jujutsu`, and `pkgs.curl` to the profile packages. It should define three scripts: `scherzo-agent-env-check`, `scherzo-agent-whoami`, and `scherzo-agent-run`. Do not add secret values to the devenv `env` option.

Inside Nix indented strings, shell parameter expansions that use braces must escape the Nix interpolation marker. For example, write shell `${VAR:-}` expressions as escaped shell syntax in the Nix string, or use `$VAR` when a simple variable expansion is enough. This matters because an unescaped shell `${...}` inside `devenv.nix` is parsed as Nix interpolation, not shell code.

Each script should duplicate a small POSIX shell prelude rather than introducing a new checked-in helper file. The prelude should define `fail`, `require_command`, `status_configured`, `require_var`, `toml_escape`, `write_jj_config`, `prepare_agent_env`, `remote_url`, `require_agent_remote`, `show_identities`, and `require_live_identity`. Keeping the prelude duplicated is acceptable here because there are only three small scripts and avoiding a separate generated helper keeps the change contained to `devenv.nix`.

`prepare_agent_env` should set these defaults before exporting anything:

    SCHERZO_AGENT_PR_REMOTE defaults to scherzo-agent
    SCHERZO_AGENT_PR_REPO defaults to bromanko/scherzo
    SCHERZO_AGENT_GIT_NAME defaults to Scherzo Agent
    SCHERZO_AGENT_SSH_HOST defaults to github-scherzo-agent
    SCHERZO_REPO_ROOT defaults to $PWD
    GH_CONFIG_DIR defaults to $PWD/.scherzo/gh-agent

There is intentionally no default for `SCHERZO_AGENT_LINEAR_API_KEY`; the operator must provide the real Linear agent key for live checks and runs.

`prepare_agent_env` should create `.scherzo/` and `.scherzo/gh-agent/` if they do not exist. It should derive `GH_TOKEN` and `GITHUB_TOKEN` only from `SCHERZO_AGENT_GITHUB_TOKEN`; if `SCHERZO_AGENT_GITHUB_TOKEN` is empty, it must unset `GH_TOKEN` and `GITHUB_TOKEN` so inherited personal values are not used. It should derive `LINEAR_API_KEY` only from `SCHERZO_AGENT_LINEAR_API_KEY`; if `SCHERZO_AGENT_LINEAR_API_KEY` is empty, it must unset `LINEAR_API_KEY` so an inherited personal Linear key is not used. It should export:

    GH_CONFIG_DIR=$PWD/.scherzo/gh-agent
    GH_TOKEN=$SCHERZO_AGENT_GITHUB_TOKEN, only when the agent token is set
    GITHUB_TOKEN=$SCHERZO_AGENT_GITHUB_TOKEN, only when the agent token is set
    LINEAR_API_KEY=$SCHERZO_AGENT_LINEAR_API_KEY, only when the agent Linear key is set
    SCHERZO_PR_REMOTE=$SCHERZO_AGENT_PR_REMOTE
    SCHERZO_PR_REPO=$SCHERZO_AGENT_PR_REPO
    GIT_AUTHOR_NAME=$SCHERZO_AGENT_GIT_NAME
    GIT_AUTHOR_EMAIL=$SCHERZO_AGENT_GIT_EMAIL
    GIT_COMMITTER_NAME=$SCHERZO_AGENT_GIT_NAME
    GIT_COMMITTER_EMAIL=$SCHERZO_AGENT_GIT_EMAIL
    JJ_CONFIG=<any existing JJ_CONFIG paths first, then $PWD/.scherzo/jj-agent.toml>
    SCHERZO_REPO_ROOT=$PWD unless already set

`prepare_agent_env` should unset inherited `GIT_SSH_COMMAND` by default so `ssh` uses the system configuration for `SCHERZO_AGENT_SSH_HOST`. If a future operator explicitly needs to override SSH command behavior, that should be a separate opt-in variable such as `SCHERZO_AGENT_GIT_SSH_COMMAND`; it should not be required by this plan.

`SCHERZO_AGENT_GIT_EMAIL` must be required before writing `.scherzo/jj-agent.toml`, because git and jj identity checks are meaningless without an email. `SCHERZO_AGENT_GITHUB_TOKEN`, `SCHERZO_AGENT_GITHUB_LOGIN`, `SCHERZO_AGENT_LINEAR_API_KEY`, an SSH-based `SCHERZO_AGENT_PR_REMOTE`, and a remote host matching `SCHERZO_AGENT_SSH_HOST` must be required for `scherzo-agent-whoami` and `scherzo-agent-run`. `scherzo-agent-run` must verify that `LINEAR_API_KEY` was derived from `SCHERZO_AGENT_LINEAR_API_KEY` because starting the daemon with a human Linear key would defeat the purpose of the profile.

When generating `.scherzo/jj-agent.toml`, reject agent names or emails containing newlines, then quote the remaining values safely for TOML. A shell helper that escapes backslashes and double quotes is enough for the expected names and emails. The generated file should contain only:

    [user]
    name = "Scherzo Agent"
    email = "agent-email@example.invalid"

When setting `JJ_CONFIG`, preserve any pre-existing `JJ_CONFIG` path list first and append `$PWD/.scherzo/jj-agent.toml` last using the Unix path separator `:`. This ensures the agent identity overrides earlier user config while preserving intentionally supplied non-identity jj settings.

`remote_url` should prefer `jj git remote list --color=never` and fall back to `git remote get-url` only if needed. `require_agent_remote` should fail unless the selected remote exists, is SSH-based, and uses the expected SSH host alias. For the default `SCHERZO_AGENT_SSH_HOST=github-scherzo-agent`, accepted URL shapes are `git@github-scherzo-agent:bromanko/scherzo.git` and `ssh://git@github-scherzo-agent/bromanko/scherzo.git`. It should reject `https://github.com/...` and `git@github.com:...` with a remediation message that tells the operator to add or update the `scherzo-agent` remote rather than changing `origin`.

`scherzo-agent-env-check` should not contact GitHub or Linear. It should call `prepare_agent_env`, generate or verify `.scherzo/jj-agent.toml` when `SCHERZO_AGENT_GIT_EMAIL` is present, print non-secret effective values, print whether the GitHub token, expected GitHub login, Linear key, and SSH host alias are configured, print the resolved `gh`, `jj`, and `curl` commands, run `git var GIT_AUTHOR_IDENT`, run `git var GIT_COMMITTER_IDENT`, and run `jj config get user.name` and `jj config get user.email`. It must not print token values, token prefixes, or token suffixes. If the email is missing, it should print a clear remediation and exit nonzero after reporting other non-secret status.

`scherzo-agent-whoami` should require a real GitHub token, expected GitHub login, Linear agent API key, git email, and an SSH-based remote whose host matches `SCHERZO_AGENT_SSH_HOST`. It should call `gh api user --jq .login` and fail unless the result equals `SCHERZO_AGENT_GITHUB_LOGIN`. It should call `gh repo view "$SCHERZO_PR_REPO" --json nameWithOwner --jq .nameWithOwner` or an equivalent read-only `gh api repos/$SCHERZO_PR_REPO --jq .full_name` check and fail unless the repository name matches `SCHERZO_PR_REPO`. It should run a read-only Linear GraphQL `viewer { id name email }` query with `curl` using the derived `LINEAR_API_KEY`, print the returned actor name or id without printing the key, and fail if the query is unauthorized. It should show the remote URL, show the git and jj identities, and run an SSH authentication check against `git@$SCHERZO_AGENT_SSH_HOST`. Because GitHub's SSH test may return a nonzero status even when authentication succeeds, capture output and treat text containing both `successfully authenticated` and `SCHERZO_AGENT_GITHUB_LOGIN` as success.

`scherzo-agent-run` should perform the same strict local and live identity checks as `scherzo-agent-whoami`, require `SCHERZO_AGENT_LINEAR_API_KEY`, generate `.scherzo/jj-agent.toml`, export the effective environment with `LINEAR_API_KEY` derived from the agent key, and then run:

    gleam run -- .scherzo/scherzo.yaml

Edit `.scherzo/README.md`. In the required environment section, add a short subsection for the Scherzo agent profile. Explain that `.env.local` should contain the GitHub agent token, expected GitHub login, Linear agent API key, git name, git email, and optional SSH host alias if the default `github-scherzo-agent` is not used. Explain the required GitHub token permissions, the expectation that the Linear key belongs to the desired agent actor, and that the scripts use tokens only through environment variables, not through `gh auth login`. Explain the one-time SSH config and remote setup. The SSH config example should keep 1Password details outside the repository, for example:

    Host github-scherzo-agent
      HostName github.com
      User git
      # Configure IdentityAgent / IdentityFile here if your system needs it.
      # With 1Password, this belongs in ~/.ssh/config, not in .env.local.

Then explain the one-time remote command:

    jj git remote add scherzo-agent git@github-scherzo-agent:bromanko/scherzo.git

Also explain the verification and run commands:

    direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check
    direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami
    direnv exec . devenv shell -P scherzo-agent scherzo-agent-run

Do not edit `.gitignore` unless implementation discovers that `.scherzo/jj-agent.toml`, `.scherzo/gh-agent/`, or another generated local file would be tracked. The current ignore rules should already keep those paths out of version control.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Expect all tests to pass. If `.envrc` is blocked, inspect it, run `direnv allow .`, and retry.

2. Edit `devenv.nix` and add only the `profiles.scherzo-agent.module = { pkgs, ... }: { ... };` skeleton with profile packages `pkgs.gh`, `pkgs.openssh`, `pkgs.jujutsu`, and `pkgs.curl`. Keep the existing base packages and `scripts.check.exec` unchanged.

3. Run `direnv exec . devenv shell -P scherzo-agent true`. Expect exit code `0` and no `Profile 'scherzo-agent' not found` message. This proves the requested profile evaluates; do not use `--help` for this check because help output does not prove profile evaluation.

4. Extend `devenv.nix` with `scripts.scherzo-agent-env-check.exec`. Add the shared prelude functions needed for local setup, token redaction, TOML escaping, `.scherzo/jj-agent.toml` generation, `GH_CONFIG_DIR`, unsetting inherited `GIT_SSH_COMMAND`, checking `SCHERZO_AGENT_SSH_HOST`, and `JJ_CONFIG` ordering. Keep the script non-networked.

5. Run a local profile check with dummy non-secret values:

    SCHERZO_AGENT_GITHUB_TOKEN=dummy-token \
    SCHERZO_AGENT_GITHUB_LOGIN=dummy-agent-login \
    SCHERZO_AGENT_LINEAR_API_KEY=dummy-linear-token \
    SCHERZO_AGENT_GIT_EMAIL=agent@example.invalid \
    direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check

Expect output that says the GitHub token and Linear key are configured without printing `dummy-token` or `dummy-linear-token`, shows `SCHERZO_PR_REMOTE=scherzo-agent`, shows `SCHERZO_PR_REPO=bromanko/scherzo`, shows `GH_CONFIG_DIR=.scherzo/gh-agent` or an equivalent repo-root-resolved value, shows the jj config file as the last `JJ_CONFIG` entry, and shows git and jj identity values containing `Scherzo Agent` and `agent@example.invalid`.

6. Extend `devenv.nix` with `scripts.scherzo-agent-whoami.exec`. Reuse the same prelude, require `SCHERZO_AGENT_GITHUB_TOKEN`, `SCHERZO_AGENT_GITHUB_LOGIN`, `SCHERZO_AGENT_LINEAR_API_KEY`, `SCHERZO_AGENT_GIT_EMAIL`, an SSH-based `SCHERZO_AGENT_PR_REMOTE`, and a remote host matching `SCHERZO_AGENT_SSH_HOST`, then add the `gh api user`, GitHub repository access, Linear viewer, remote, git identity, jj identity, and SSH authentication checks described in the Plan of Work.

7. Extend `devenv.nix` with `scripts.scherzo-agent-run.exec`. Reuse the same prelude, perform the same strict checks as `scherzo-agent-whoami`, require `SCHERZO_AGENT_LINEAR_API_KEY`, verify that `LINEAR_API_KEY` was derived from it, and run `gleam run -- .scherzo/scherzo.yaml` only after those checks pass.

8. Run `direnv exec . devenv shell -P scherzo-agent true` again. Expect exit code `0`. Then rerun the dummy `scherzo-agent-env-check` command from step 5 and expect the same non-secret identity output.

9. Edit `.scherzo/README.md` and add the Scherzo agent profile documentation described in the Plan of Work, including required GitHub token permissions, `SCHERZO_AGENT_GITHUB_LOGIN`, `SCHERZO_AGENT_LINEAR_API_KEY`, the SSH remote setup, and the fact that the scripts do not use `gh auth login`.

10. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Expect the same pass result as the baseline; the profile should not affect normal tests.

11. If real agent credentials are available, add them to `.env.local` without committing the file. Include `SCHERZO_AGENT_GITHUB_TOKEN`, `SCHERZO_AGENT_GITHUB_LOGIN`, `SCHERZO_AGENT_LINEAR_API_KEY`, `SCHERZO_AGENT_GIT_EMAIL`, and, if the default is not correct, `SCHERZO_AGENT_SSH_HOST`. Ensure the `SCHERZO_AGENT_SSH_HOST` host alias in the operator's SSH config authenticates as the GitHub agent account and that the account has repository write access. Ensure the Linear API key belongs to the Linear actor that should appear in Scherzo issue activity.

12. If the remote is not present, run `jj git remote add scherzo-agent git@github-scherzo-agent:bromanko/scherzo.git` from the repository root. If it already exists, inspect it with `jj git remote list --color=never` and ensure the `scherzo-agent` URL is SSH-based and uses the host configured by `SCHERZO_AGENT_SSH_HOST`.

13. Run `direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami`. Expect the GitHub CLI login and SSH authentication output to match `SCHERZO_AGENT_GITHUB_LOGIN`, the target remote to be `scherzo-agent`, the repository access check to print `bromanko/scherzo`, the read-only Linear viewer check to print the agent Linear actor without printing the key, and git/jj identity output to match the agent name and email.

14. Do not start a live daemon unless the operator is ready for Scherzo to poll Linear and dispatch work. When ready, run `direnv exec . devenv shell -P scherzo-agent scherzo-agent-run`.

Commit point: after steps 1 through 10 pass, commit `devenv.nix`, `.scherzo/README.md`, and this plan. Do not commit `.env.local`, `.scherzo/jj-agent.toml`, or `.scherzo/gh-agent/`.

## Testing and Falsifiability

There are no new Gleam modules in this plan, so the main regression test is that existing formatting and tests still pass:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

The profile evaluation check is:

    direnv exec . devenv shell -P scherzo-agent true

That command must exit `0`. A help command is not an acceptable substitute, because `devenv shell -P <profile> --help` can succeed without evaluating the profile.

The profile itself is falsified if any of these checks fail:

- `direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check` with dummy values prints the literal dummy GitHub token or dummy Linear key. It must not print either secret or any secret prefix or suffix.
- The local check uses inherited `GH_TOKEN` or `GITHUB_TOKEN` when `SCHERZO_AGENT_GITHUB_TOKEN` is absent. It must unset those names instead.
- The local check uses inherited `LINEAR_API_KEY` when `SCHERZO_AGENT_LINEAR_API_KEY` is absent. It must unset that name instead.
- The local check does not set `GH_CONFIG_DIR` to an ignored `.scherzo/gh-agent` location before invoking any `gh` command.
- The local check does not make `pkgs.jujutsu` and `pkgs.curl` available, or `command -v jj` or `command -v curl` fails inside the profile.
- The local check builds `JJ_CONFIG` with `.scherzo/jj-agent.toml` before an existing `JJ_CONFIG` value. The generated file must be the last path so the agent identity wins.
- `git var GIT_AUTHOR_IDENT` or `git var GIT_COMMITTER_IDENT` shows a human identity rather than the agent identity when the profile script is active.
- `jj config get user.email` shows a human email rather than `SCHERZO_AGENT_GIT_EMAIL` when the profile script is active.
- `scherzo-agent-whoami` succeeds when `SCHERZO_AGENT_GITHUB_LOGIN` is missing, or when `gh api user --jq .login` returns a different login.
- `scherzo-agent-whoami` succeeds when `SCHERZO_AGENT_LINEAR_API_KEY` is missing, inherited `LINEAR_API_KEY` is present, or the read-only Linear viewer query is unauthorized.
- `scherzo-agent-whoami` succeeds when the `scherzo-agent` remote is missing, is an HTTPS URL, or is an SSH URL whose host does not equal `SCHERZO_AGENT_SSH_HOST`. It should fail with a remediation message, because HTTPS remotes may use a personal credential helper and generic SSH hosts may use a personal key.
- `scherzo-agent-whoami` succeeds when inherited `GIT_SSH_COMMAND` is set and the script does not unset it or explicitly reject it.
- `scherzo-agent-whoami` succeeds when the SSH authentication output from `ssh -T git@$SCHERZO_AGENT_SSH_HOST` does not contain both `successfully authenticated` and `SCHERZO_AGENT_GITHUB_LOGIN`.
- `scherzo-agent-run` starts Scherzo when `SCHERZO_AGENT_LINEAR_API_KEY` is missing, or when `LINEAR_API_KEY` was inherited instead of derived from the agent source key. It should fail before `gleam run`.

The live identity check requires real credentials and network access. With real credentials, `gh api user --jq .login` must print the configured agent login, not the human login. The repository access check must identify `bromanko/scherzo`. The Linear viewer query must return the intended Linear agent actor, not the human operator. The SSH check must show GitHub accepted the configured host alias for the same login. These observations prove the profile is using the agent account for GitHub API, Linear API, and SSH git operations.

## Validation and Acceptance

Accept the repository change when these behaviors are true:

From a normal shell without `-P scherzo-agent`, `direnv exec . gleam test` still passes. This proves the optional profile did not break normal development.

The profile evaluates with:

    direnv exec . devenv shell -P scherzo-agent true

The expected result is exit code `0` with no missing-profile error.

With dummy values, this command succeeds without leaking either token:

    SCHERZO_AGENT_GITHUB_TOKEN=dummy-token \
    SCHERZO_AGENT_GITHUB_LOGIN=dummy-agent-login \
    SCHERZO_AGENT_LINEAR_API_KEY=dummy-linear-token \
    SCHERZO_AGENT_GIT_EMAIL=agent@example.invalid \
    direnv exec . devenv shell -P scherzo-agent scherzo-agent-env-check

The expected output includes non-secret lines like:

    SCHERZO_PR_REMOTE=scherzo-agent
    SCHERZO_PR_REPO=bromanko/scherzo
    SCHERZO_AGENT_SSH_HOST=github-scherzo-agent
    GH_TOKEN=configured
    GITHUB_TOKEN=configured
    LINEAR_API_KEY=configured
    GIT_SSH_COMMAND=unset
    GH_CONFIG_DIR=.scherzo/gh-agent
    JJ_CONFIG=.../.scherzo/jj-agent.toml

The expected output must not include `dummy-token` or `dummy-linear-token`. The jj config entry shown for `.scherzo/jj-agent.toml` must be last.

With real values and an SSH remote, this command identifies the GitHub agent account:

    direnv exec . devenv shell -P scherzo-agent scherzo-agent-whoami

The expected output includes the agent GitHub login from `gh`, the configured repository name `bromanko/scherzo`, the Linear viewer actor for the agent key, the agent git author and committer identity, the agent jj identity, the `scherzo-agent` remote URL using `github-scherzo-agent`, and an SSH authentication success for `SCHERZO_AGENT_GITHUB_LOGIN`. It must not include a personal GitHub login, personal Linear actor, or personal email.

When the operator starts Scherzo with:

    direnv exec . devenv shell -P scherzo-agent scherzo-agent-run

The command must first pass the same strict identity checks as `scherzo-agent-whoami` and confirm `LINEAR_API_KEY` was derived from `SCHERZO_AGENT_LINEAR_API_KEY` without printing it. Scherzo should then behave as before from the workflow perspective, except Linear issue claims, state transitions, and comments use the Linear agent key; GitHub PR creation and PR comments use the GitHub agent token; jj git pushes/fetches through `SCHERZO_PR_REMOTE` use the agent SSH host alias and system SSH configuration; and commit metadata uses the agent identity.

## Rollout, Recovery, and Idempotence

The profile is additive and inactive unless the operator passes `-P scherzo-agent`, so rollout is low risk. If it misbehaves, stop using the profile and run the existing commands as before. Reverting the change only requires removing the `profiles.scherzo-agent.module` block and the README section.

Generating `.scherzo/jj-agent.toml` is idempotent. Re-running the scripts should refresh it from the current `SCHERZO_AGENT_GIT_NAME` and `SCHERZO_AGENT_GIT_EMAIL`. Creating `.scherzo/gh-agent/` is also idempotent. Because both paths are ignored by the existing `.gitignore` rules under `.scherzo/`, they should not affect commits.

Adding the `scherzo-agent` remote and the matching SSH host alias is a one-time local operation. If the remote URL or host alias is wrong, fix it with the normal jj or git remote command and the operator's SSH config before running the live daemon. Do not change `origin` as part of this plan. If the operator wants to stop using the agent profile locally, remove the `scherzo-agent` remote or leave it unused; it is not referenced unless `SCHERZO_PR_REMOTE` points to it.

If the profile accidentally uses the wrong GitHub account or Linear actor, immediately stop the daemon, revoke or rotate the mistaken token if needed, fix `.env.local`, rerun `scherzo-agent-whoami`, and only then restart Scherzo. If a PR, GitHub comment, Linear comment, or Linear state transition was already created under the wrong identity, record that manually in the PR or Linear issue and continue under the corrected profile. If the ignored GitHub CLI config directory ever contains an unwanted login, remove `.scherzo/gh-agent/` and rerun the checks; do not run `gh auth login` in this profile.

## Artifacts and Notes

A safe `.env.local` example uses variable names but not real secrets:

    SCHERZO_AGENT_GITHUB_TOKEN=github_pat_redacted
    SCHERZO_AGENT_GITHUB_LOGIN=scherzo-agent-login
    SCHERZO_AGENT_LINEAR_API_KEY=lin_api_redacted
    SCHERZO_AGENT_GIT_NAME=Scherzo Agent
    SCHERZO_AGENT_GIT_EMAIL=agent-email@example.invalid
    SCHERZO_AGENT_SSH_HOST=github-scherzo-agent

The fine-grained GitHub token should be limited to `bromanko/scherzo` and allow metadata read, pull request read/write, and issue read/write. The SSH host alias should authenticate as the same GitHub agent account and have repository write access for git pushes. The Linear API key should belong to the Linear agent actor that should appear on Scherzo issue comments, claim transitions, success transitions, and failure transitions.

A local jj identity file generated by the scripts should look like this:

    [user]
    name = "Scherzo Agent"
    email = "agent-email@example.invalid"

The ignored GitHub CLI config directory should be created at `.scherzo/gh-agent/`. The scripts should not write tokens there; it exists to prevent reads from the operator's normal GitHub CLI config.

The one-time SSH host alias can live in the operator's `~/.ssh/config`, where 1Password or another key source is configured outside the repository:

    Host github-scherzo-agent
      HostName github.com
      User git
      # IdentityAgent and IdentityFile settings, if needed, stay here.

The one-time remote setup should look like this when listed:

    scherzo-agent git@github-scherzo-agent:bromanko/scherzo.git

The profile scripts should leave `GIT_SSH_COMMAND` unset by default so the host alias decides which key or agent is used.

The scripts must redact by omission, not by partial masking. They should print `configured` or `missing` for token variables, never prefixes or suffixes of any token or API key.

## Interfaces and Dependencies

`devenv.nix` will expose one new manual profile:

    profiles.scherzo-agent.module

The profile will add these packages only when activated:

    pkgs.gh
    pkgs.openssh
    pkgs.jujutsu
    pkgs.curl

The profile will expose these scripts:

    scherzo-agent-env-check
    scherzo-agent-whoami
    scherzo-agent-run

The local environment interface is:

    SCHERZO_AGENT_GITHUB_TOKEN      required for live checks and runs
    SCHERZO_AGENT_GITHUB_LOGIN      required for live checks and runs
    SCHERZO_AGENT_LINEAR_API_KEY    required for live checks and runs
    SCHERZO_AGENT_GIT_NAME          optional, defaults to Scherzo Agent
    SCHERZO_AGENT_GIT_EMAIL         required for env-check, live checks, and runs
    SCHERZO_AGENT_SSH_HOST          optional, defaults to github-scherzo-agent
    SCHERZO_AGENT_PR_REMOTE         optional, defaults to scherzo-agent
    SCHERZO_AGENT_PR_REPO           optional, defaults to bromanko/scherzo

The exported Scherzo and tool interface is:

    GH_CONFIG_DIR
    GH_TOKEN
    GITHUB_TOKEN
    LINEAR_API_KEY
    SCHERZO_PR_REMOTE
    SCHERZO_PR_REPO
    GIT_AUTHOR_NAME
    GIT_AUTHOR_EMAIL
    GIT_COMMITTER_NAME
    GIT_COMMITTER_EMAIL
    JJ_CONFIG
    SCHERZO_REPO_ROOT

The repository file `.scherzo/jj-agent.toml` and directory `.scherzo/gh-agent/` are generated, ignored local artifacts. The checked-in workflow scripts `scripts/scherzo-execplan` and `scripts/scherzo-execplan-revision` consume `SCHERZO_PR_REMOTE`, `SCHERZO_PR_REPO`, `LINEAR_API_KEY`, `gh`, and jj behavior from this environment without needing code changes.

## Open Questions and Clarifications Needed

[CLARIFY] Decide the exact agent account email to use in `.env.local`. The plan intentionally does not commit it. If the GitHub agent account uses a noreply address, use that value as `SCHERZO_AGENT_GIT_EMAIL`.

[CLARIFY] Decide the exact Linear actor and API key to use as `SCHERZO_AGENT_LINEAR_API_KEY`. The plan assumes the key belongs to the account whose name should appear on Linear activity, but the account provisioning and permission model are outside this repository.
