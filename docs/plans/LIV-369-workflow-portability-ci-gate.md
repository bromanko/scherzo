# Plan workflow portability CI gate

## Purpose / Big Picture

This plan protects the checked-in `.scherzo/workflows` package from drifting back into repository-development assumptions. After implementation, CI will prove that the workflows can be loaded and exercised through the packaged `scherzo` CLI in a minimal runtime environment with fake pi, tracker, GitHub, and workspace-driver surfaces, rather than relying on the repo's `devenv` shell.

## Problem Framing and Constraints

The immediate risk is a regression where a workflow path works only because `gleam run` is available in this repository, even though copied or packaged workflows must not require the Gleam development toolchain. The gate must be positive rather than blacklist-driven: it should supply only the accepted runtime tools, avoid real Linear or GitHub mutations, and fail naturally when a workflow command depends on unstaged tools, missing helper paths, missing prompts, invalid schema paths, absent structured-output artifacts, or broken workspace-driver contracts.

## Strategy Overview

Add a dedicated Nix portability check and a matching local debug shell built around the packaged Scherzo CLI, not the source checkout runner. The check stages the workflow package into a temporary runtime root, installs fake but contract-shaped pi, tracker/provider, GitHub, and workspace-driver commands, runs config and workflow exercises against every `.scherzo/workflows/*.yaml`, and writes a machine-readable coverage report. This is the right size because it validates real workflow command wiring without contacting external services or teaching CI an ever-growing list of forbidden commands.

## Alternatives Considered

A static grep for `gleam`, `erl`, or other tool names would catch the last bug but would be brittle, noisy, and incomplete. Running the normal development test suite is also insufficient because `direnv exec .` intentionally provides Gleam and other repo-only tooling. Running only `doctor --check workflow-config` is useful but too shallow because it does not execute command steps, fake agent structured-output capture, or helper resolution.

## Risks and Countermeasures

The main risk is building a fake harness so permissive that workflows can pass while production behavior is broken. Countermeasure: make the harness validate contracts and artifacts, not business outcomes, and require a per-workflow coverage manifest that lists exercised steps, resolved helpers, prompts, schemas, structured-output tools, and workspace-driver operations. Another risk is flakiness from VCS or remote commands. Countermeasure: use isolated temporary repositories and fake `gh`/provider commands that record intended calls and reject mutation attempts unless the workflow contract explicitly says the call is simulated.

## Scope Boundaries

In scope are Nix outputs, a portability harness script, fake runtime commands, tests for the harness and Nix wiring, and the minimal workflow/helper changes needed so packaged execution is used. Out of scope are real Linear or GitHub API calls, semantic review quality, model quality, publishing real pull requests, and replacing the full Scherzo integration suite. If full workflow execution is too large for the first implementation, the accepted fallback is a staged subset with an explicit manifest entry explaining which workflows remain load-only and what fixture must be added to promote each one to full fake execution.

## Milestones

First, establish the minimal runtime package and prove that `scherzo doctor --check workflow-config` loads all checked-in workflows with fake driver profiles. Next, add fake pi structured-output responses and fake helper/provider scripts so representative workflows run without external effects. Then expand coverage to every checked-in workflow or record an explicit staged-subset gap with a promotion path. Finally, wire the check into flake checks, expose a matching debug shell, and update CI documentation so maintainers know how to reproduce failures locally.

## Progress

- [x] (2026-05-17) Drafted this human-reviewable plan surface for LIV-369.
- [x] (2026-05-18) Materialized the implementation pack and follow-up implementation task through the workflow bundle process.
- [x] (2026-05-18) Retired the active `gleam run -- workflow run` helper path in `scripts/scherzo-review` and added a portability regression test for packaged `scherzo workflow run`.
- [x] (2026-05-18) Added `scripts/scherzo-workflow-portability`, `nix/workflow-portability.nix`, flake wiring, and documentation for a separate packaged-CLI portability check that writes a per-workflow coverage report.
- [x] (2026-05-18) Validated the harness with a fake packaged `scherzo` test double and recorded every checked-in workflow as explicit staged `load-only` coverage with expansion paths.
- [x] (2026-05-18) Review pass tightened the staged config rewrite to preserve the checked-in `pi` launch fields while redirecting only command targets to fake runtime commands, corrected debug-shell docs to use PATH-resolved packaged `scherzo`, and constrained the harness/runtime build inputs so local `path:$PWD` Nix validation no longer absorbs `test/tmp` artifacts.

## Surprises & Discoveries

- Observation: Full fake execution for every checked-in workflow is larger than this repair window because the current workflow set mixes agent structured-output lanes, retained bundle materialization, and GitHub/Linear-mutating helpers.
  Evidence: The new portability harness can stage the packaged workflow bundle and prove packaged `doctor --check workflow-config` succeeds in a positive runtime, but the generated report still needs explicit `load-only` entries and helper-specific expansion paths for all eight workflows.
- Observation: A portability gate can become too synthetic if it rewrites away the checked-in `pi` launch shape.
  Evidence: The review update now preserves `argv`, `argv_env`, and `session_persistence` in the staged config and adds test assertions over the staged `.scherzo/scherzo.yaml`.
- Observation: Local `path:$PWD` Nix validation will happily ingest generated `test/tmp` symlink fixtures unless the flake source is filtered.
  Evidence: After `direnv exec . gleam test`, `nix build path:$PWD#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability --print-build-logs` failed in package build canonicalization until the flake source excluded `test/tmp` and the affected schema tests cleaned up their temporary fixtures.

## Decision Log

- Decision: Use a positive minimal Nix runtime instead of a forbidden-tool blacklist.
  Rationale: The failure mode is reliance on the repo development shell; absence of that shell is a stronger, less brittle test than enumerating banned executables.
  Date: 2026-05-17.
- Decision: Exercise workflows through packaged `scherzo workflow run`.
  Rationale: The portability claim is about copied or packaged workflows, so the source checkout `gleam run` path must not be part of the proof.
  Date: 2026-05-17.
- Decision: Let helper-driven nested workflow execution honor `SCHERZO_WORKFLOW_RUNNER` but default to `scherzo`.
  Rationale: The portability gate needs packaged-CLI execution by default, while the explicit override keeps local debugging flexible without reintroducing a repo-shell dependency into the active path.
  Date: 2026-05-18.
- Decision: Ship the first CI repair as config-backed packaged validation plus explicit `load-only` coverage reporting for every checked-in workflow.
  Rationale: The verifier required an observable portability harness, separate flake check, and report coverage for every workflow. Delivering the packaged-CLI stage, fake runtime commands, and explicit expansion paths retires the missing gate without broadening this repair into a full fake execution framework for all workflows.
  Date: 2026-05-18.
- Decision: Build the harness child PATH from resolved runtime tool directories and filter `test/tmp` out of the flake source used for `path:$PWD` validation.
  Rationale: The gate should prove a positive runtime rather than inherit the developer shell, and local Nix validation must stay reproducible even after the Gleam suite materializes temporary fixtures under `test/tmp`.
  Date: 2026-05-18.

## Outcomes & Retrospective

The repository now has a dedicated `workflow-portability` flake check and matching debug shell that run `scripts/scherzo-workflow-portability` against the packaged `scherzo` CLI. The generated report covers every checked-in workflow, records `remote_mutations: none`, and makes the current staged boundary explicit by listing `load-only` workflows with concrete expansion paths instead of silently omitting them. The harness now resolves its own positive PATH instead of inheriting the ambient shell, and `path:$PWD` Nix builds stay green after the Gleam suite because the flake source excludes generated `test/tmp` artifacts.

## Validation and Acceptance

Acceptance is observable when `nix flake check` includes a separate workflow portability check that passes without the Gleam/Rebar development tools in its supplied runtime path. The report must show every checked-in workflow either fully exercised through fake providers or explicitly listed as staged load-only with a clear expansion path, and it must show no real Linear, GitHub, or remote mutation attempts.

## Rollout, Recovery, and Idempotence

The change is additive: new Nix outputs and harness files can be reverted independently if they block unrelated work. The check must create all state under temporary directories, use deterministic fake providers, leave no repository changes behind, and be safe to rerun locally. During rollout, keep the check separate from existing package checks so failures identify portability regressions rather than ordinary build or test failures.

## Open Questions and Clarifications Needed

The implementation should confirm whether all current workflows can be fully fake-executed within acceptable CI time. If not, the initial subset should prioritize `implementation`, `execplan`, `execplan-implementation`, and one scheduled command-only workflow, then record the remaining workflows and required fixtures in the generated coverage report.
