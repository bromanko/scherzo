# Artifact publication configuration model

This review document frames the first implementation slice for artifact publication configuration. Runtime publication, GitHub mutation, and durable publication state are intentionally deferred.

## Purpose / Big Picture

After this change, Scherzo operators and workflow authors can declare where publishable workflow artifacts should go without coupling that declaration to workspace-driver `publish-change`. The observable result is configuration that can be parsed, defaulted, and rejected early: root config can define a named GitHub artifact repository such as `github.docs`, workflow YAML can define `artifacts.publications` routes that target it, and invalid references or malformed selectors fail before a run can begin.

## Problem Framing and Constraints

The PRD in `docs/specs/ARTIFACT_PUBLICATION_PRD.md` separates canonical Scherzo artifacts from derived external copies. Today the execplan workflow still publishes a review document through an explicit command step and a workspace publishing capability, which is the wrong seam for durable workflow artifacts. This first slice must build only the configuration model: it must parse repository targets, parse workflow-level publication routes, apply branch and pull-request defaults, and validate syntax and references. It must not create branches, open pull requests, record publication attempts, retry publication, or migrate existing workflows away from their current publishing steps.

## Strategy Overview

The right-sized approach is additive parsing and validation. Introduce explicit typed configuration for `artifacts.repositories.github.<name>` in orchestrator config and `artifacts.publications` in workflow YAML, store those typed values on the existing orchestrator and workflow DAG structures, and add bundle-load validation that checks publication repository references against configured repository targets. Validation should be strict enough to catch unsupported backends, misspelled repository ids, invalid branch and pull-request defaults, and malformed file selectors, while leaving runtime selection of retained artifact bytes for a later publication adapter slice.

## Alternatives Considered

One alternative is to implement GitHub publication end to end now, including checkout management and pull-request creation. That is too large because it mixes config shape decisions with remote mutation, durable state, and retry semantics. Another alternative is to leave publication settings as untyped maps until runtime. That would defer simple configuration mistakes until after workflow dispatch and would make later publication code guess at defaults. The selected approach gives future runtime work a stable model while keeping this slice reversible and testable.

## Risks and Countermeasures

The main risk is over-validating templates or artifact selectors before the generic artifact descriptor runtime exists. The countermeasure is to validate only static syntax, allowed backend ids, known template variables, repository-relative paths, and workflow contract output names where available; entry existence inside artifact sets remains runtime work. A second risk is silently accepting legacy or misspelled GitHub options such as repository-level `draft_pr`; the countermeasure is explicit negative tests that require an error pointing to `pull_request.draft`. A third risk is accidentally wiring runtime publication into workflow completion; the countermeasure is a scope boundary plus pre-publish diff evidence showing no branch, PR, ledger, retry, or workflow-run execution path was added.

## Scope Boundaries

In scope: typed parsing of GitHub artifact repositories under root `artifacts.repositories.github`, typed parsing of workflow `artifacts.publications`, defaulting and validation for `branch.strategy`, `branch.template`, `pull_request.enabled`, `pull_request.strategy`, and `pull_request.draft`, validation of route repository references such as `github.docs`, validation of publication ids, required flags, selectors, and repository-relative destination path templates, plus documentation and tests. Out of scope: materializing artifacts, reading artifact-store bytes, local publication checkouts, `git` or `gh` commands, GitHub API calls, PR body rendering, publication ledger records, operator retry commands, and migration of the checked-in `workflows/dogfood/execplan.yaml` source for the runtime execplan workflow away from its current `publish_review_doc` command.

## Milestones

Milestone 1 delivers the root repository model. At the end, a root config containing `artifacts.repositories.github.docs` parses into typed config with repo, base branch, managed checkout, branch defaults, and pull-request defaults, and malformed GitHub repository settings fail with targeted config errors.

Milestone 2 delivers workflow publication route parsing. At the end, workflow YAML can declare one or more `artifacts.publications` routes with ids, `github.docs` repository references, required/defaulted behavior, optional pull-request overrides, and file selectors of the form `select.output`, optional `select.entry`, and `path`.

Milestone 3 delivers cross-file validation. At the end, runtime bundle loading rejects a workflow publication that references an unconfigured repository such as `github.missing` or an unsupported backend such as `gitlab.docs`, while accepting a configured `github.docs` route.

Milestone 4 delivers validation hardening, backward-compatibility proof, and documentation. At the end, tests cover branch and pull-request defaults including `pull_request.draft`, selector and path syntax errors, duplicate ids, omitted publication config on existing root and workflow YAML, and runtime non-publication evidence. The simplified YAML spec at `docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md` documents the new configuration shape as config-only, and pre-publish diff evidence proves `workflows/dogfood/execplan.yaml` still uses its existing `publish_review_doc` publishing step and `publish-change` workspace requirement.

## Progress

- [x] (2026-05-29) Reviewed the artifact publication PRD, workflow artifact taxonomy, current root config parser, workflow DAG parser, and execplan workflow publishing shape.
- [x] (2026-05-29) Drafted this review document and separated mechanical implementation detail into the structured implementation pack.
- [x] (2026-05-29) Incorporated review feedback by adding explicit backward-compatibility acceptance, documentation acceptance, detailed negative-path cases, and pre-publish evidence that the checked-in execplan workflow publishing step is not migrated in this slice.

## Decision Log

- Decision: Treat this as a config-model slice only and validate it during parsing or runtime bundle loading, not during workflow execution.
  Rationale: The PRD requires an opt-in publication layer, but the task explicitly keeps runtime publication out of scope for the first slice.
  Date: 2026-05-29.
- Decision: Default GitHub branch strategy to `stable_per_work`, pull-request strategy to `update_existing`, pull-request enabled to `true`, and `pull_request.draft` to `false` unless explicitly set.
  Rationale: These defaults match the PRD's MVP branch and PR strategy while keeping draft PR creation an explicit operator choice.
  Date: 2026-05-29.
- Decision: Treat backward compatibility, documentation updates, detailed negative-path tests, and unchanged execplan workflow publishing as acceptance obligations for the first implementation slice.
  Rationale: Review feedback identified these as observable proof that the config model is additive, documented, strictly validated, and not accidentally coupled to runtime publication or migration work.
  Date: 2026-05-29.

## Validation and Acceptance

Acceptance requires concrete automated and pre-publish manual evidence. Repository parsing is accepted when `direnv exec . gleam test test/orchestrator_config_test.gleam` includes tests that assert a valid `artifacts.repositories.github.docs` config resolves, omitted `artifacts.repositories` parses to an empty publication registry, and invalid repo, checkout, branch, `draft_pr`, unsafe `pull_request.body_template`, and non-boolean `pull_request.draft` settings fail with targeted messages. Workflow route parsing is accepted when `direnv exec . gleam test test/workflow_dag_test.gleam` includes tests for valid `artifacts.publications`, omitted `artifacts.publications` parsing to no publication routes, defaulted `required: true`, duplicate route ids, invalid ids, non-string repository refs, empty `files`, missing `select.output`, unsupported selector keys, unknown template variables, unknown contract outputs, illegal `select.entry` on non-aggregate outputs, and unsafe destination paths. Repository-reference validation is accepted when `direnv exec . gleam test test/runtime_bundle_test.gleam` proves `github.docs` succeeds only when configured, `github.missing` or unsupported backends fail before dispatch, and representative existing workflows with no publication config still load before dispatch. Documentation is accepted when `docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md` shows the new root `artifacts.repositories.github.<name>` shape, workflow `artifacts.publications` shape, default values, selector syntax, repository-reference syntax, and a config-only note that runtime artifact publication, GitHub mutation, publication state, retry commands, and migration from `publish-change` are deferred. Full validation is accepted only after `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` pass. Runtime non-publication and non-migration are accepted by pre-publish manual evidence from `git diff --stat` and focused diffs showing no GitHub adapter, ledger publication events, retry command, workflow-run publication execution path, or changes to `workflows/dogfood/execplan.yaml` publishing steps and `publish-change` workspace requirement were added in this slice.

## Rollout, Recovery, and Idempotence

The rollout is additive: existing configs without `artifacts.repositories` and existing workflows without `artifacts.publications` continue to parse with empty publication config, and existing workflows must still load before dispatch. Those compatibility claims are acceptance requirements, not assumptions. Failed validation leaves no external side effects because this slice does not mutate repositories or durable publication state. Re-running the implementation or its tests is idempotent because defaults are deterministic, duplicate publication ids are rejected, and no runtime publication attempts are recorded. Recovery from a bad implementation is a straightforward revert of parser, type, test, and documentation changes; if pre-publish diff review shows `workflows/dogfood/execplan.yaml` was migrated away from its current publishing step or `publish-change` requirement, that migration must be reverted or split into a later task before publishing this slice.

## Open Questions and Clarifications Needed

No open questions. The plan assumes `pull_request.draft` defaults to `false`, `pull_request.enabled` defaults to `true` for an opted-in route, and artifact-set entry existence is validated later when runtime publication can inspect retained manifests.
