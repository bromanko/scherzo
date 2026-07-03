# Scherzo Workflow Authoring Reference

Use this reference after loading `scherzo-workflow-author` when creating or editing checked-in Scherzo workflows.

## Important paths

- `.scherzo/scherzo.yaml` — conventional repository runtime config. Workflow paths in `workflows:` are relative to this file. If the user supplies another config path, use that path instead.
- `.scherzo/workflows/` — common checked-in workflow bundle entry point. It may be a directory, symlink, vendored bundle, or generated checkout; resolve the actual edited file without hard-coding the physical target into workflow references.
- `<workflow-bundle>/*.yaml` — workflow definitions selected by the config `workflows:` map.
- `<workflow-bundle>/prompts/` — bundled prompt templates.
- `<workflow-bundle>/schemas/` — canonical and provider-facing schemas for workflow artifacts and structured output.
- `<workflow-bundle>/scripts/` — workflow-local helper scripts.
- `schemas/scherzo.workflow.v1.schema.json` — public workflow YAML schema when present in a Scherzo source checkout.
- `schemas/scherzo.config.v1.schema.json` — public runtime config schema when present in a Scherzo source checkout.
- `examples/workflows/` and `examples/scherzo*.yaml` — smaller reusable examples when present.

Avoid authoring changes in runtime paths such as `.scherzo/workspaces/`, `.scherzo/.scherzo-state/`, `.scherzo/command-step-diagnostics/`, or retained run artifact directories.

## Workflow YAML quick reference

A workflow file is a v1 DAG:

```yaml
# yaml-language-server: $schema=../../schemas/scherzo.workflow.v1.schema.json
version: 1
id: example-workflow
description: Short operator-facing description.
workspace:
  driver: noop
  requires: [assert-only]
concurrency: 1
steps:
  - id: plan
    kind: agent
    prompt: prompts/plan.md
    run_in: main

  - id: validate
    kind: command
    depends_on: [plan]
    run: |
      set -eu
      test -f result.md
    run_in: main
```

Root fields commonly used:

- `version: 1` — required.
- `id` — required; lowercase letter/digit start; lowercase letters, digits, `_`, and `-`.
- `description` — recommended for operator clarity.
- `model` / `thinking` — optional workflow defaults for agent steps.
- `workspace.driver` — optional override from config default.
- `workspace.requires` — declared workspace driver capabilities: `status`, `diff`, `changed-files`, `assert-only`, `baseline`, `refresh-base`, `publish-commit-stack`.
- `concurrency` — maximum parallel ready steps; integer >= 1.
- `recovery` — workflow-level recovery defaults.
- `contract` — typed inputs/context/outputs for artifacts, publications, workstream handoff, and repair compatibility.
- `workstream_phase` — optional phase metadata and next actions.
- `artifacts.publications` — file or commit-stack publication routes.
- `steps` — required non-empty DAG step list.

Removed/legacy keys are intentionally rejected by the schema: `workspace_profile`, `workspace_capabilities`, `max_parallel_steps`, `recover`, step-level `workspace`, and `timeout_ms`.

## Step patterns

Step ids must start with a lowercase letter and may contain lowercase letters, digits, and `_`.

Agent step:

```yaml
- id: implement
  kind: agent
  prompt: prompts/implement.md
  thinking: high
  run_in: main
```

Command step:

```yaml
- id: final_validation
  kind: command
  depends_on: [implement]
  timeout: 20m
  run: |
    set -eu
    test -s result.md
  run_in: main
```

Parallel review workspace:

```yaml
- id: review
  kind: agent
  depends_on: [implement]
  prompt: prompts/review.md
  run_in:
    name: review
    from: main
```

Failure/recovery:

```yaml
- id: collect_artifact
  kind: command
  depends_on: [draft]
  on_failure: fail
  recovery:
    attempts: 1
    prompt: prompts/recover-failed-step.md
  run: |
    set -eu
    test -f artifact.json
    cat artifact.json
  run_in: main
```

Use `on_failure: continue` only when downstream steps explicitly know how to interpret a failed/skipped prerequisite. Prefer bounded `recovery.attempts` with a workflow-bundled recovery prompt for repairable command failures.

## Structured output pattern

Structured output belongs on `kind: agent` steps. Provider-facing schemas should accept only model-owned fields; runner-owned metadata should be injected by deterministic materialization scripts when needed.

```yaml
- id: draft_pack
  kind: agent
  prompt: prompts/draft-pack.md
  structured_output:
    format: json
    artifact_name: implementation_pack
    required: true
    source:
      type: pi_tool_call
      tool_name: submit_implementation_pack
      parameters_schema_path: .scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json
    validators:
      - type: json_schema
        name: implementation-pack
        path: .scherzo/workflows/schemas/implementation-pack.v2.schema.json
    validation_retries: 1
  run_in: main
```

Check structured-output changes with:

```sh
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflow --workflow .scherzo/workflows/<workflow>.yaml
```

## Contract and artifact outputs

Use `contract` when a workflow has durable inputs/outputs or when downstream recovery, publication, or phase handoff needs a stable artifact identity.

Common source forms:

- `issue_context`, `scheduled_context`, `workspace_driver_base`, `mapped_output`.
- Literal/url/git ref objects.
- `{ step: <step_id>, field: stdout | final_response }`.
- `{ step: <step_id>, path: <repository-relative-path> }`.
- `{ step: <step_id>, structured_output: <artifact_name> }`.
- `{ step: <step_id>, inline_json: <artifact_name> }`.

Example:

```yaml
contract:
  version: 1
  inputs:
    prompt:
      type: text
      required: true
      source: issue_context
  outputs:
    findings:
      kind: file
      media_type: text/markdown
      artifact_type: scherzo.research_findings.v1
      required: true
      source:
        step: collect_findings
        field: stdout
```

## Publication routes

Publication routes live under workflow `artifacts.publications` and refer to repositories configured in the Scherzo config under `artifacts.repositories.github`.

File publication sketch:

```yaml
artifacts:
  publications:
    - id: publish_doc
      repository: github.code
      mode: files
      required: true
      files:
        - select:
            output: review_doc
          path: docs/plans/example.md
      target:
        kind: stable_branch
      pull_request:
        title: "Publish review document"
        body_template: .scherzo/workflows/prompts/example-pr-body.md
```

Commit-stack publication sketch:

```yaml
artifacts:
  publications:
    - id: publish_change
      repository: github.code
      mode: commit_stack
      required: true
      commit_stack:
        select:
          output: code_change_bundle
      target:
        kind: stable_branch
      pull_request:
        body_template: .scherzo/workflows/prompts/implementation-publication-pr-body.md
```

If a publication uses a workspace driver, ensure the workflow declares the needed capability, for example `publish-commit-stack`.

## Config routing and schedules

In the Scherzo config, commonly `.scherzo/scherzo.yaml`:

```yaml
workflows:
  research: workflows/research.yaml
  implementation: workflows/implementation.yaml
```

The keys are workflow ids. Paths are relative to the config file that contains them, so in the conventional layout `workflows/research.yaml` resolves through `.scherzo/workflows`.

Label-based routing usually uses `workflow:<id>` labels through the tracker label policy. After adding a new workflow route, make sure the tracker/project contract or documentation includes the matching label when required.

Scheduled workflows use config-level `schedules:`:

```yaml
schedules:
  - id: workspace-cleanup
    workflow: workspace-cleanup
    enabled: true
    every: 1h
    overlap: skip
    catch_up: false
    on_failure:
      task:
        enabled: true
        state: Triage
        dedupe: open_task_per_schedule
```

Enabled schedules require `every`. Use `overlap: skip` unless there is a deliberate reason to support overlap later.

Workflow-specific task update overrides live under `task_updates.workflows.<workflow_id>`. Use them when success/failure state semantics differ from the global policy.

## Portability rules

- Keep the workflow bundle self-contained: prompts, schemas, scripts, and guidance needed by the workflow should live under the configured bundle directory, commonly `.scherzo/workflows/`.
- Do not rely on personal or repository-local Pi skills in workflow prompts. Embed required guidance in bundled prompt/guidance files.
- Do not commit secrets or real tokens. Use environment variable names and local ignored overrides.
- Avoid absolute paths, `~`, drive-letter paths, and parent traversal in schema-checked paths.
- In workflow command scripts, prefer bundle/config environment variables:

```sh
bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}
if [ -z "$bundle_dir" ]; then
  bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"
fi
repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}
"$bundle_dir/scripts/helper" subcommand
```

- In workflow YAML, follow existing path conventions: prompt paths are bundle-relative (`prompts/name.md`), while config-level templates and schema paths that need repository-relative addressing use `.scherzo/workflows/...` or config-relative `workflows/...` as appropriate.
- New command steps should set `set -eu` and fail closed when required inputs/artifacts are missing.
- Prefer driver capabilities (`assert-only`, `changed-files`, `publish-commit-stack`, etc.) over ad hoc assumptions about VCS state.

## Validation checklist

Use the smallest set that proves the change, and explain any skipped command.

Basic workflow/config load. Use the packaged `scherzo` CLI when available; in a Scherzo source checkout, use the Gleam entry point through direnv. Pass the config path under edit:

```sh
# Packaged CLI:
scherzo doctor --check workflow-config .scherzo/scherzo.yaml

# Scherzo source checkout:
direnv exec . gleam run -- doctor --check workflow-config .scherzo/scherzo.yaml
```

If the config requires tracker credentials even for non-mutating checks, set the appropriate dummy environment variable for that tracker.

Tracker label/state contract when workflow labels, states, or project assumptions changed and tracker credentials are available:

```sh
# Packaged CLI:
scherzo doctor --check tracker-contract .scherzo/scherzo.yaml

# Scherzo source checkout:
direnv exec . gleam run -- doctor --check tracker-contract .scherzo/scherzo.yaml
```

Structured-output contracts:

```sh
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflow --workflow .scherzo/workflows/<workflow>.yaml
direnv exec . gleam run -m scherzo_structured_output_contract -- check-workflows
```

Workflow bundle portability when the repository provides a check, for example in the Scherzo source checkout:

```sh
nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability --print-build-logs
```

Manual portability harness when investigating failures in the Scherzo source checkout:

```sh
nix develop .#workflow-portability
python3 scripts/scherzo-workflow-portability check --repo-root . --scherzo scherzo --output-dir tmp/scherzo-workflow-portability/manual
```

General repository gates when production code, tests, helper scripts, or schemas changed: use the repository's own required checks. In the Scherzo source checkout, that commonly includes:

```sh
direnv exec . gleam test
direnv exec . gleam format --check src test
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```

If `.envrc` is blocked, inspect it, run `direnv allow .`, then retry validation through `direnv exec .`.
