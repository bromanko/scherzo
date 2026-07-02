---
name: scherzo-workflow-author
description: Create, edit, and validate checked-in Scherzo workflow bundles and configuration from pi. Use when asked to author or modify workflow YAML, prompts, schemas, helper scripts, workflow routes, labels, schedules, contracts, structured output, publications, or workspace-driver requirements. Not for operating a live daemon; use scherzo-operator for sessions, retries, retained runs, and control commands.
---

# Scherzo Workflow Author

Use this skill when the user wants to create or change Scherzo workflow definitions or the configuration that routes work into them. This is a repository-authoring mode, not a live-operator mode. For daemon inspection, `scherzoctl`, retrying workers, UI responses, retained-run recovery, or Linear operations, use `scherzo-operator` instead.

See [the workflow authoring reference](references/workflow-authoring.md) for schema details, examples, portability rules, and validation commands.

## Start with the checked-in sources

Before editing, inspect the current contract and the files being changed:

- `.scherzo/README.md` for repository conventions and portability requirements.
- `.scherzo/scherzo.yaml` for workflow routes, schedules, workspace drivers, artifact repositories, and task update policy.
- The target workflow YAML under `.scherzo/workflows/...` or the symlink target `workflows/dogfood/...`.
- Related bundled files under the same workflow bundle: `prompts/`, `schemas/`, `guidance/`, and `scripts/`.
- `schemas/scherzo.workflow.v1.schema.json` and `schemas/scherzo.config.v1.schema.json` when adding unfamiliar keys.

In this repository, `.scherzo/workflows` is a tracked symlink to `../workflows/dogfood`. Edit the checked-in bundle target when practical, but keep references portable as if the bundle is reached through `.scherzo/workflows`.

## Gather the authoring intent

Clarify any missing pieces before making workflow changes:

- What should trigger the workflow: a `workflow:*` label, a schedule, or a follow-up phase action?
- What artifact or user-visible outcome proves success?
- Which workspace driver and capabilities are required (`noop`, `dogfood-jj`, custom driver; `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-commit-stack`)?
- Which steps must mutate the primary workspace, and which can run in branch/review workspaces?
- What should happen on command failure: fail, continue, or recover with a bounded recovery prompt?
- Does the workflow need structured output, JSON Schema validation, publications, or task-state overrides?

## Authoring policy

- Keep workflow YAML schema comments such as `# yaml-language-server: $schema=...` on new files.
- Keep workflow ids, config route keys, labels, and schedule ids consistent. Config `workflows:` paths are relative to `.scherzo/scherzo.yaml`.
- Prefer explicit `kind: agent` / `kind: command` on new steps even though the schema can infer kind.
- Use `run_in: main` for the step that owns the primary change. Use named workspaces with `from: main` for parallel reviews, validation, or analysis.
- Keep prompts, schemas, scripts, and guidance bundled with the workflow. Do not require consuming repositories to have personal Pi skills installed.
- Do not put secrets, tokens, machine-specific absolute paths, or local-only usernames in workflow/config files. Use environment variables and ignored `*.local.yaml` overrides for machine-specific data.
- Do not edit runtime state while authoring workflows: avoid `.scherzo/workspaces/`, `.scherzo/.scherzo-state/`, control files, retained artifacts, and command-step diagnostics unless the user explicitly asks for runtime recovery and the operator skill is active.

## Validate proportionally

For workflow/config-only edits, at minimum run the workflow config doctor through direnv:

```sh
LINEAR_API_KEY=dummy direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
```

For workflow bundle portability changes, also run the packaged portability check when feasible:

```sh
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability --print-build-logs
```

For structured-output workflow changes, run the contract checker for the changed workflow or all workflows:

```sh
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflow --workflow .scherzo/workflows/<workflow>.yaml
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflows
```

If production Gleam code, helpers, schemas used by tests, or linter-sensitive files are changed, also run the relevant repository gates from `AGENTS.md`, including `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` as appropriate.

If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry through direnv.

## Report clearly

When done, summarize:

- Workflow/config files changed.
- Triggering/routing behavior.
- New or changed steps and artifacts.
- Validation commands run and their results.
- Any validation not run and why.
