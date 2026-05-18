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
- [ ] Materialize the implementation pack and follow-up implementation task through the workflow bundle process.
- [ ] Implement and validate the portability CI gate.

## Decision Log

- Decision: Use a positive minimal Nix runtime instead of a forbidden-tool blacklist.
  Rationale: The failure mode is reliance on the repo development shell; absence of that shell is a stronger, less brittle test than enumerating banned executables.
  Date: 2026-05-17.
- Decision: Exercise workflows through packaged `scherzo workflow run`.
  Rationale: The portability claim is about copied or packaged workflows, so the source checkout `gleam run` path must not be part of the proof.
  Date: 2026-05-17.

## Validation and Acceptance

Acceptance is observable when `nix flake check` includes a separate workflow portability check that passes without the Gleam/Rebar development tools in its supplied runtime path. The report must show every checked-in workflow either fully exercised through fake providers or explicitly listed as staged load-only with a clear expansion path, and it must show no real Linear, GitHub, or remote mutation attempts.

## Rollout, Recovery, and Idempotence

The change is additive: new Nix outputs and harness files can be reverted independently if they block unrelated work. The check must create all state under temporary directories, use deterministic fake providers, leave no repository changes behind, and be safe to rerun locally. During rollout, keep the check separate from existing package checks so failures identify portability regressions rather than ordinary build or test failures.

## Open Questions and Clarifications Needed

The implementation should confirm whether all current workflows can be fully fake-executed within acceptable CI time. If not, the initial subset should prioritize `implementation`, `execplan`, `execplan-implementation`, and one scheduled command-only workflow, then record the remaining workflows and required fixtures in the generated coverage report.
