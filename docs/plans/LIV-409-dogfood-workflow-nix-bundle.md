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

The migration must keep dogfood runs working in this repository, avoid moving general Scherzo runtime, test, CI, operator, and workspace-driver scripts into the workflow bundle, and avoid suggesting that the bundle is universally portable. The bundle still depends on Scherzo, Pi, the generic structured-output Pi extension path or equivalent packaging, jj, gh, Linear and GitHub credentials, and repository conventions used by the dogfood helpers.

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

Scherzo should expose `SCHERZO_WORKFLOW_BUNDLE_DIR` as the absolute directory containing the loaded workflow YAML to command steps, agent steps, command validators, and workspace lifecycle hooks. Workflow commands should prefer that variable and fall back to `$SCHERZO_CONFIG_DIR/workflows` so the symlinked layout still works with older runners during the migration window.

## Alternatives Considered

The smallest alternative is to leave files where they are and add a Nix package that copies from both `.scherzo/workflows` and `scripts/`. That would produce an artifact, but it would leave source ownership split and let new root-script assumptions creep back into workflows.

A second alternative is to route `.scherzo/scherzo.yaml` directly to `../workflows/dogfood/*.yaml`. Current routing paths are config-relative and reject parent-directory traversal, so the symlink bridge is a smaller and safer compatibility shape.

A third alternative is to put every Scherzo-related helper, including core runtime wrappers and test scripts, into the bundle. That would blur the boundary between reusable workflow code and Scherzo development tooling, increase package size, and make consumers think the bundle is a complete Scherzo distribution.

## Risks and Countermeasures

The main risk is breaking live dogfood workflows by changing many paths at once. Counter this by keeping `.scherzo/workflows` as the compatibility path, using `SCHERZO_WORKFLOW_BUNDLE_DIR` only as a preferred locator, and validating through both source-tree and symlinked packaged-bundle consumption.

Another risk is schema drift. Counter this by preserving the existing `.scherzo/workflows/schemas/...` schema references in workflow YAML for structured-output validation, while teaching moved helper scripts to find their sibling `schemas/` directory through the bundle.

A third risk is overclaiming portability. Counter this in docs and package metadata by saying the artifact is reusable dogfood workflow code, not a standalone product workflow: consumers still need compatible Scherzo runtime configuration, Pi structured-output support, workspace drivers, credentials, and host tools.

## Scope Boundaries

In scope are dogfood workflow DAGs, prompts, schemas, workflow-private helper scripts, helper path resolution, schema path handling, portable jj workspace symlink behavior, Nix package output, portability checks, and docs that explain symlink-style consumption.

Out of scope are redesigning the workflow semantics, making the dogfood workflows credential-free, moving Scherzo core/runtime scripts such as `scherzoctl`, `scherzo-start`, `scherzo-pi`, workspace drivers, test runners, or CI helpers into the bundle, and inventing bundle refs or hashes in this review document.

## Milestones

Milestone 1 establishes the bundle boundary and compatibility symlink. Reviewers should see one canonical `workflows/dogfood` tree and `.scherzo/workflows` pointing at it.

Milestone 2 updates runtime and helper path resolution. Reviewers should see `SCHERZO_WORKFLOW_BUNDLE_DIR` in generated workflow environments and workflow commands using bundle-local helpers instead of `SCHERZO_REPO_ROOT/scripts` for moved dogfood helpers.

Milestone 3 packages and validates the bundle. Reviewers should see a `scherzo-dogfood-workflows` Nix package whose output can be symlinked directly as `.scherzo/workflows`, plus portability checks that exercise that shape.

Milestone 4 updates documentation and rollout guidance. Reviewers should see clear consumer instructions, dependency caveats, and recovery notes.

## Progress

- [x] (2026-05-19) Drafted this human-facing review document for LIV-409.
- [x] (2026-05-19) Submitted the mechanical implementation pack through Scherzo structured output.
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

## Validation and Acceptance

Acceptance requires reviewers to be able to identify the exact bundle layout, the root-script split, the new bundle-dir environment contract, and the package output name from this document and the retained implementation pack. Implementation acceptance requires source dogfood workflows to keep passing their normal lint, unit, workflow-config, and portability gates, and requires a Nix-built bundle to validate when consumed through a `.scherzo/workflows` symlink.

A successful package validation should prove that workflow YAML loads from the symlinked bundle, prompts and schemas resolve, moved scripts are executable with their dependencies, and no moved workflow command still depends on `SCHERZO_REPO_ROOT/scripts` for dogfood-private helpers.

## Rollout, Recovery, and Idempotence

Roll out as an additive migration first: create the canonical bundle, keep the `.scherzo/workflows` compatibility symlink, and keep any temporary root wrappers only as deliberate compatibility shims if tests or docs still need them. Once workflows and docs use bundle-local helpers, root wrappers can be retired separately.

Recovery is straightforward because the source files are only moved, not semantically rewritten. If a packaged-bundle validation fails, restore `.scherzo/workflows` as the canonical directory, point workflow commands back to the previous root helper paths, and keep the Nix package disabled until the failing path assumption is fixed. Re-running the symlink setup is idempotent when it replaces only `.scherzo/workflows` with the intended symlink.

## Open Questions and Clarifications Needed

The main open packaging question is whether the generic Scherzo structured-output Pi extension should remain a Scherzo runtime/project dependency or be packaged alongside the dogfood workflow bundle. The bundle plan assumes it remains outside `workflows/dogfood` unless a broader runtime-packaging decision says otherwise.

A second question is how long to keep root compatibility wrappers for moved workflow-private helpers. The proposed answer is only as long as needed for local developer commands and docs to move to the bundle path.
