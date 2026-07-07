---
name: scherzo-workflow-author
description: Create, edit, and validate checked-in Scherzo workflow bundles and configuration from pi. Use when asked to author or modify workflow YAML, prompts, schemas, helper scripts, workflow routes, labels, schedules, contracts, structured output, publications, or workspace-driver requirements. Not for operating a live daemon; use scherzo-operator for sessions, retries, retained runs, and control commands.
---

# Scherzo Workflow Author

Use this skill when the user wants to create or change Scherzo workflow definitions or the configuration that routes work into them. This is a repository-authoring mode, not a live-operator mode. For daemon inspection, `scherzoctl`, retrying workers, UI responses, retained-run recovery, or tracker operations, use `scherzo-operator` instead.

See [the workflow authoring reference](references/workflow-authoring.md) for schema details, examples, portability rules, and validation commands.

## Start with the checked-in sources

Before editing, inspect the current contract and the files being changed:

- Repository Scherzo notes such as `.scherzo/README.md` when present.
- The Scherzo config file being used, commonly `.scherzo/scherzo.yaml`, for workflow routes, schedules, workspace drivers, artifact repositories, and task update policy.
- The target workflow YAML under the configured bundle path, commonly `.scherzo/workflows/...`.
- Related bundled files under the same workflow bundle: `prompts/`, `schemas/`, `guidance/`, and `scripts/`.
- `schemas/scherzo.workflow.v1.schema.json` and `schemas/scherzo.config.v1.schema.json` when they are available and you are adding unfamiliar keys.

Resolve symlinks or vendored bundle targets only as an editing convenience. Keep workflow references portable from the configured bundle entry point rather than baking in repository-specific physical paths.

## Gather the authoring intent

Clarify any missing pieces before making workflow changes:

- What should trigger the workflow: a `workflow:*` label, a schedule, a manual/operator action, or a follow-up phase action?
- What artifact or user-visible outcome proves success?
- Which workspace driver and capabilities are required (`noop`, repository-specific custom drivers; `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-commit-stack`)?
- Which steps must mutate the primary workspace, and which can run in branch/review workspaces?
- What should happen on command failure: fail, continue, or recover with a bounded recovery prompt?
- Does the workflow need structured output, JSON Schema validation, publications, or task-state overrides?

## Authoring policy

- Keep workflow YAML schema comments such as `# yaml-language-server: $schema=...` on new files.
- Keep workflow ids, config route keys, labels, and schedule ids consistent. Config `workflows:` paths are relative to the Scherzo config file that contains them.
- Prefer explicit `kind: agent` / `kind: command` on new steps even though the schema can infer kind.
- Use `run_in: main` for the step that owns the primary change. Use named workspaces with `from: main` for parallel reviews, validation, or analysis.
- Keep duplicated twin-pipeline blocks synchronized when a bundle intentionally shares steps across workflows, such as Scherzo's `implementation.yaml` and `execplan-implementation.yaml` review pipeline.
- Keep shared prompt-family policy fragments synchronized across every prompt that carries the same policy block; do not hand-edit one copy without checking the family.
- Keep prompts, schemas, scripts, and guidance bundled with the workflow. Do not require consuming repositories to have personal Pi skills installed.
- Prefer prompt fragments for shared prompt policy. Put shared fragments under the bundle-local `prompts/fragments/` directory and include them with `{% include "fragments/name.md" %}` from host prompts.
- Include paths are relative to the file containing the include, must remain relative, must not contain `..` or control characters, and must stay inside the workflow bundle. Keep host-specific instructions outside shared fragments.
- Keep the skill repository-agnostic: do not assume a particular bundled workflow set, tracker, VCS, or workspace driver unless the checked-in config or user request says so.
- Do not put secrets, tokens, machine-specific absolute paths, or local-only usernames in workflow/config files. Use environment variables and ignored `*.local.yaml` overrides for machine-specific data.
- Do not edit runtime state while authoring workflows: avoid `.scherzo/workspaces/`, `.scherzo/.scherzo-state/`, control files, retained artifacts, and command-step diagnostics unless the user explicitly asks for runtime recovery and the operator skill is active.

## Validate proportionally

For workflow/config-only edits, at minimum run the workflow config doctor for the config under edit. Use the packaged `scherzo` CLI when available; in a Scherzo source checkout, run it through direnv:

```sh
# Packaged CLI:
scherzo doctor --check workflow-config .scherzo/scherzo.yaml

# Scherzo source checkout:
direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
```

If the config requires tracker credentials even for non-mutating checks, set the appropriate dummy environment variable for that tracker.

For Scherzo dogfood workflow bundle edits, run the bundle hygiene lint when it is present; it catches orphaned prompts, twin-pipeline drift, shared prompt-fragment drift, and dead `repo_root=` assignments in workflow `run:` strings:

```sh
.scherzo/workflows/scripts/scherzo-workflow-lint check --repo-root . --bundle-root .scherzo/workflows --config .scherzo/scherzo.yaml
```

For workflow bundle portability changes, also run the repository's portability check when one is provided. After fragment edits, rerun workflow-config doctor and the portability check so missing or escaping includes fail before dispatch:

```sh
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability --print-build-logs
```

For structured-output workflow changes, run the contract checker for the changed workflow or all workflows when the checker is available:

```sh
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflow --workflow .scherzo/workflows/<workflow>.yaml
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflows
```

If production code, helpers, schemas used by tests, or linter-sensitive files are changed, also run the relevant repository gates from local agent instructions. In the Scherzo source checkout, that includes `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` as appropriate.

If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry through direnv.

## Report clearly

When done, summarize:

- Workflow/config files changed.
- Triggering/routing behavior.
- New or changed steps and artifacts.
- Validation commands run and their results.
- Any validation not run and why.
