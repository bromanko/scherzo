# Validate ExecPlan implementation workflow with a tiny Gleam test

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

This is a deliberately tiny validation plan for Scherzo's `workflow:execplan-implementation` dogfood workflow. After implementation, the repository will have one additional focused Gleam test proving that workflow DAG descriptions are parsed from YAML.

## Problem Framing and Constraints

The real goal is to exercise the new dogfood workflow end to end with a safe, low-risk code change. The plan must touch a `.gleam` file so the workflow's changed-file analysis detects `LANGUAGES=gleam` and runs the vendored Gleam review path. This validation plan is not meant to add a product feature beyond one small regression test.

## Strategy Overview

Add a single test to `test/workflow_dag_test.gleam`. The existing `scherzo/workflow_dag.parse/1` parser already supports the optional `description` field, so this plan should require only a test change.

## Alternatives Considered

A docs-only plan would be safer but would not exercise the Gleam language-review path. A source-code behavior change would be more realistic but would add unnecessary risk for an E2E validation run.

## Risks and Countermeasures

The main risk is changing production behavior during a workflow validation. Avoid that by adding only one test and no source-code changes unless the test reveals an actual parser bug. The workflow publish step, not the implementer, creates the final jj commit and PR.

## Progress

- [x] (2026-05-02 02:28Z) Added `parses_optional_description_test` to `test/workflow_dag_test.gleam`.
- [x] (2026-05-02 02:28Z) Ran validation with `direnv exec . gleam test`; it passed.
- [x] (2026-05-02 02:28Z) Ran `direnv exec . gleam format --check src test`; it passed.
- [x] (2026-05-02 02:28Z) Recorded the result in this plan's Outcomes & Retrospective section.

## Surprises & Discoveries

- Observation: The first attempt to run `direnv exec . gleam test` failed because the workspace-local `.envrc` had not yet been approved.
  Evidence: `direnv` reported that `.envrc` was blocked. After reading `.envrc` and running `direnv allow .`, `direnv exec . gleam test` completed successfully with `513 passed, no failures`.

## Decision Log

- Decision: Use a test-only Gleam change for this E2E validation.
  Rationale: It exercises changed-file language detection and Gleam review without changing runtime behavior.
  Date: 2026-05-01.
- Decision: Leave `src/scherzo/workflow_dag.gleam` unchanged.
  Rationale: The existing parser already reads `description` with `optional_string(root, "description")`, and the new regression test passes without production changes.
  Date: 2026-05-02.

## Outcomes & Retrospective

Implementation is complete. `test/workflow_dag_test.gleam` now includes `parses_optional_description_test`, which parses YAML containing `description: Test description` and asserts that `WorkflowDag.description` is `Some("Test description")`. Validation passed with `direnv exec . gleam test`, reporting `513 passed, no failures`; `direnv exec . gleam format --check src test` also passed. The change stayed within the intended tiny, test-only scope and should provide a `.gleam` diff for the workflow's language detection.

## Context and Orientation

`src/scherzo/workflow_dag.gleam` defines the YAML workflow DAG parser. A workflow DAG is the YAML file that tells Scherzo which agent and command steps to run for a Linear workflow. `test/workflow_dag_test.gleam` contains parser tests and already imports `gleam/option.{None, Some}`, so the new test can assert `dag.description == Some("...")` directly.

## Preconditions and Verified Facts

`test/workflow_dag_test.gleam` has a helper function named `parse_ok(source: String) -> workflow_dag.WorkflowDag`. It also already imports `Some`, so no import change should be required.

## Scope Boundaries

In scope: add one test to `test/workflow_dag_test.gleam`. Out of scope: changing `src/scherzo/workflow_dag.gleam`, changing workflow YAML files, changing Scherzo runtime behavior, or broad cleanup.

## Milestones

The only milestone is to add and validate the parser test. At the end, `gleam test` should still pass and the workflow should have a `.gleam` diff for review.

## Plan of Work

In `test/workflow_dag_test.gleam`, add a public test function named `parses_optional_description_test`. In the test, call `parse_ok` with YAML containing `version: 1`, `id: research`, `description: Test description`, and a minimal command step so the DAG has one terminal step. Assert that `dag.description == Some("Test description")`.

## Concrete Steps

From the repository root, edit `test/workflow_dag_test.gleam` and add the new test near `parses_minimal_workflow_dag_test`. If running targeted validation, run `direnv exec . gleam test` and expect all tests to pass. Do not create jj or git commits; the Scherzo workflow publish step owns the final commit.

## Testing and Falsifiability

The new test must fail if the parser stops preserving the YAML `description` field. It should pass today because `workflow_dag.parse_root` reads `optional_string(root, "description")` into `WorkflowDag.description`.

## Validation and Acceptance

Acceptance is met when `test/workflow_dag_test.gleam` contains `parses_optional_description_test`, `direnv exec . gleam test` passes, and the workflow's changed-file analysis reports `LANGUAGES=gleam`.

## Rollout, Recovery, and Idempotence

This test-only change is safe to abandon if the E2E validation fails. Re-running the plan should be idempotent: if the test already exists, do not add a duplicate.

## Artifacts and Notes

None yet.

## Interfaces and Dependencies

Use existing `workflow_dag.parse/1`, existing `parse_ok` test helper, and `gleam/option.Some` already imported by `test/workflow_dag_test.gleam`. No new dependencies are required.

## Open Questions and Clarifications Needed

None.
