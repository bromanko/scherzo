# Package dogfood workflows as a reusable Nix bundle

This review document is the human-facing proposal for LIV-409. The implementation details, exact edit sequence, tests, and interface notes are retained in the structured implementation pack submitted with this workflow run.

## Purpose / Big Picture

Scherzo's dogfood workflows should be consumable by another repository without copying a scatter of repository-root helper scripts and checked-in `.scherzo/workflows` files by hand. After this change, this repository will keep the dogfood workflows in one canonical bundle at `workflows/dogfood`, the local `.scherzo/workflows` path will be a symlink to that bundle, and Nix users will be able to build the bundle and symlink the build output as their own `.scherzo/workflows` directory.

The intended consumer command is:

```sh
mkdir -p .scherzo
bundle=$(nix build --no-link --print-out-paths github:scherzo-systems/scherzo#scherzo-dogfood-workflows)
ln -sfn "$bundle" .scherzo/workflows
```

## Problem Framing and Constraints

The current layout mixes portable workflow assets under `.scherzo/workflows` with workflow-private helpers under repository-root `scripts/`. That works inside this checkout because workflows can compute `SCHERZO_REPO_ROOT` and call `scripts/scherzo-review`, `scripts/scherzo-execplan`, and related helpers, but it is awkward for another Scherzo user who wants one packaged workflow bundle.

The migration must keep dogfood runs working in this repository, avoid moving general Scherzo runtime, test, CI, operator, and workspace-driver scripts into the workflow bundle, and avoid suggesting that the bundle is universally portable. The bundle still depends on Scherzo, Pi, a project/runtime-loaded generic structured-output Pi extension, jj, gh, Linear and GitHub credentials, and repository conventions used by the dogfood helpers. The workflow bundle cannot make Pi load an extension by merely containing it; Pi extension discovery happens at Pi startup through project/global `.pi` locations, Pi settings packages, or explicit `pi -e`/`--extension` flags supplied by the configured Scherzo Pi launcher.

## Strategy Overview

Use `workflows/dogfood` as the canonical source tree for dogfood workflow YAML, prompts, schemas, and workflow-private helper scripts. Keep `.scherzo/workflows` as a tracked symlink to `../workflows/dogfood` so existing config routes and schema paths can remain shaped like `.scherzo/workflows/...` while the source tree is packageable.

The target bundle is:

```text
workflows/dogfood/
  implementation.yaml
  execplan.yaml
  execplan-revision.yaml
  execplan-implementation.yaml
  research.yaml
  merge-conflict-resolution.yaml
  github-pr-conflict-scout.yaml
  origin-sync.yaml
  prompts/
  schemas/
  scripts/
    scherzo-implementation
    scherzo-execplan
    scherzo-execplan-html
    scherzo-review
    scherzo-review-lane-contract
    scherzo-merge-conflict
    scherzo-github-pr-conflict-scout
    scherzo-jj-origin-sync
    scherzo_review/
```

Scherzo should expose `SCHERZO_WORKFLOW_BUNDLE_DIR` as the absolute directory containing the loaded workflow YAML to command steps, agent steps, command validators, and workspace lifecycle hooks. Workflow commands should prefer that variable and fall back to `$SCHERZO_CONFIG_DIR/workflows` so the symlinked layout still works with older runners during the migration window. Root-level copies or wrappers for moved workflow-private helpers should not remain; local developer commands and docs must move to the bundle-local script paths in the same implementation.

Keep the generic Scherzo structured-output Pi extension outside `workflows/dogfood`. Workflow YAML can declare `structured_output.source.type: pi_tool_call`, and Scherzo can pass `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` into the agent step, but the extension that consumes that variable must already be loaded before the prompt runs. Consumers should satisfy that prerequisite through one of the runtime-owned loading shapes: a project-local `.pi/extensions/scherzo-structured-output`, a project `.pi/settings.json` package/extension entry, or a Scherzo `pi.command` wrapper that adds `-e <structured-output-extension-path>` from a pinned Nix output. The last option is acceptable as a runtime launcher detail, but workflow scripts should not spawn nested `pi -e` calls to make their own agent steps work.

## Alternatives Considered

The smallest alternative is to leave files where they are and add a Nix package that copies from both `.scherzo/workflows` and `scripts/`. That would produce an artifact, but it would leave source ownership split and let new root-script assumptions creep back into workflows.

A second alternative is to route `.scherzo/scherzo.yaml` directly to `../workflows/dogfood/*.yaml`. Current routing paths are config-relative and reject parent-directory traversal, so the symlink bridge is a smaller and safer compatibility shape.

A third alternative is to put every Scherzo-related helper, including core runtime wrappers and test scripts, into the bundle. That would blur the boundary between reusable workflow code and Scherzo development tooling, increase package size, and make consumers think the bundle is a complete Scherzo distribution.

A related alternative is to leave root-level compatibility wrappers for moved dogfood helpers. This plan rejects that approach because the goal is to make `workflows/dogfood` the single owner of workflow-private code and to catch stale root-script assumptions immediately during validation.

A fourth alternative is to move the generic structured-output Pi extension under `workflows/dogfood` and have workflow code load it with `pi -e`. That does not fit Scherzo's execution model: Scherzo, not a workflow command, launches Pi for agent steps, and the structured-output tool must be registered before the agent prompt starts. If `-e` is used, it belongs in the configured runtime Pi launcher or wrapper, not inside the workflow bundle itself.

## Risks and Countermeasures

The main risk is breaking live dogfood workflows by changing many paths at once. Counter this by keeping `.scherzo/workflows` as the compatibility path, using `SCHERZO_WORKFLOW_BUNDLE_DIR` only as a preferred locator, and validating through both source-tree and symlinked packaged-bundle consumption.

Another risk is schema drift. Counter this by preserving the existing `.scherzo/workflows/schemas/...` schema references in workflow YAML for structured-output validation, while teaching moved helper scripts to find their sibling `schemas/` directory through the bundle.

A third risk is overclaiming portability. Counter this in docs and package metadata by saying the artifact is reusable dogfood workflow code, not a standalone product workflow: consumers still need compatible Scherzo runtime configuration, Pi structured-output support, workspace drivers, credentials, and host tools.

A fourth risk is making the structured-output extension dependency implicit. Counter this by documenting it as a runtime prerequisite, validating packaged-bundle consumption with a Pi launcher that loads the extension from outside `workflows/dogfood`, and adding preflight diagnostics that distinguish an extension-not-loaded case from a loaded-but-misconfigured structured-output tool, for example by treating an unknown `/structured-output-tool-info` command as a missing extension and that command's own error output as a loaded-extension configuration failure.

## Scope Boundaries

In scope are dogfood workflow DAGs, prompts, schemas, workflow-private helper scripts, removal of the old root locations for moved workflow-private helpers, helper path resolution, schema path handling, portable jj workspace symlink behavior, Nix package output, portability checks, and docs that explain symlink-style consumption and the runtime-owned structured-output extension prerequisite.

Out of scope are redesigning the workflow semantics, making the dogfood workflows credential-free, moving Scherzo core/runtime scripts such as `scherzoctl`, `scherzo-start`, `scherzo-pi`, workspace drivers, test runners, CI helpers, or the generic structured-output Pi extension into the workflow bundle, and inventing bundle refs or hashes in this review document.

## Milestones

Milestone 1 establishes the bundle boundary and compatibility symlink. Reviewers should see one canonical `workflows/dogfood` tree, `.scherzo/workflows` pointing at it, and no retained root-level copies or wrappers for the workflow-private helpers that moved into the bundle.

Milestone 2 updates runtime and helper path resolution. Reviewers should see `SCHERZO_WORKFLOW_BUNDLE_DIR` in generated workflow environments and workflow commands using bundle-local helpers instead of `SCHERZO_REPO_ROOT/scripts` for moved dogfood helpers.

Milestone 3 packages and validates the bundle. Reviewers should see a `scherzo-dogfood-workflows` Nix package whose output can be symlinked directly as `.scherzo/workflows`, plus portability checks that exercise that shape.

Milestone 4 updates documentation and rollout guidance. Reviewers should see clear consumer instructions, dependency caveats, runtime structured-output extension loading guidance, local developer commands updated to bundle-local helper paths, and recovery notes.

## Progress

- [x] (2026-05-19) Drafted this human-facing review document for LIV-409.
- [x] (2026-05-19) Submitted the mechanical implementation pack through Scherzo structured output.
- [x] (2026-05-19) Resolved the structured-output extension packaging question: keep it outside `workflows/dogfood` as a runtime/project-loaded Pi extension, with `pi -e` only allowed in the configured launcher or wrapper.
- [x] (2026-05-19) Resolved the root-wrapper question: remove moved workflow-private helpers from the repository root in this plan instead of keeping compatibility wrappers.
- [ ] Implementation follow-up has not started; bundle refs and hashes will be created by Scherzo after this workflow run.

## Decision Log

- Decision: Use `workflows/dogfood` as the canonical checked-in bundle path and `.scherzo/workflows -> ../workflows/dogfood` as the repository compatibility bridge.
  Rationale: This keeps existing config, schema references, and workflow labels stable while making the source tree packageable.
  Date: 2026-05-19.

- Decision: Add `SCHERZO_WORKFLOW_BUNDLE_DIR` instead of continuing to infer helpers from `SCHERZO_REPO_ROOT/scripts`.
  Rationale: The workflow bundle, not the consumer repository root, should own private workflow helpers.
  Date: 2026-05-19.

- Decision: Name the Nix package output `scherzo-dogfood-workflows` and make the output root the workflow bundle directory itself.
  Rationale: Direct-output packaging keeps the consumer symlink command simple and matches the desired `.scherzo/workflows` shape.
  Date: 2026-05-19.

- Decision: Keep the generic Scherzo structured-output Pi extension as a runtime/project dependency outside `workflows/dogfood`.
  Rationale: Pi loads extensions before workflow prompts run and only discovers them through Pi's project/global extension locations, settings packages, or explicit launcher flags. A workflow bundle can require the extension and document how the Scherzo Pi launcher loads it, but putting it under `workflows/dogfood` would not make it active. If a Nix package exposes the extension, consumers should load that sibling output from project Pi settings or from the configured Scherzo `pi.command`/wrapper, not from workflow command scripts.
  Date: 2026-05-19.

- Decision: Do not keep root compatibility wrappers for moved workflow-private helpers.
  Rationale: The migration should make `workflows/dogfood` the only checked-in location for dogfood-private helper code. Keeping root wrappers would preserve the old split ownership and let stale `$SCHERZO_REPO_ROOT/scripts` assumptions survive instead of forcing workflows, local commands, docs, and validation to converge on the bundle boundary.
  Date: 2026-05-19.

## Validation and Acceptance

Acceptance requires reviewers to be able to identify the exact bundle layout, the root-script split, the new bundle-dir environment contract, the runtime structured-output extension prerequisite, and the package output name from this document and the retained implementation pack. Implementation acceptance requires source dogfood workflows to keep passing their normal lint, unit, workflow-config, and portability gates, and requires a Nix-built bundle to validate when consumed through a `.scherzo/workflows` symlink.

A successful package validation should prove that workflow YAML loads from the symlinked bundle, prompts and schemas resolve, moved scripts are executable with their dependencies, the consumer Pi launcher loads the structured-output extension from outside `workflows/dogfood`, no moved workflow command still depends on `SCHERZO_REPO_ROOT/scripts` for dogfood-private helpers, and no root compatibility wrapper remains for those moved helpers.

## Rollout, Recovery, and Idempotence

Roll out as a single boundary-setting migration: create the canonical bundle, keep only the `.scherzo/workflows` compatibility symlink, update workflows, local developer commands, tests, and docs to use bundle-local helpers, and remove the old root copies of moved workflow-private helpers in the same change. There should be no temporary root wrappers for those moved helpers.

Recovery is straightforward because the source files are only moved, not semantically rewritten. If a packaged-bundle validation fails, restore `.scherzo/workflows` as the canonical directory and restore the previous root helper files from version control, then keep the Nix package disabled until the failing path assumption is fixed. Re-running the symlink setup is idempotent when it replaces only `.scherzo/workflows` with the intended symlink.

## Resolved Clarifications

The generic Scherzo structured-output Pi extension remains outside `workflows/dogfood`. It may be packaged as a separate Scherzo runtime/Pi-extension artifact or Nix output, but the dogfood workflow bundle only declares the need for that runtime capability. Consumers must load the extension through Pi's normal project/global discovery, a project Pi settings entry, or the configured Scherzo Pi launcher adding `-e`/`--extension` for a pinned extension path. Workflows themselves should not call `pi -e` to compensate, because Scherzo has already launched the agent Pi process by the time workflow instructions run.

Root compatibility wrappers for moved workflow-private helpers should not be kept. Local developer commands and docs should move to the bundle path as part of this plan, and validation should fail if a moved dogfood-private helper still has a root wrapper or root copy.
