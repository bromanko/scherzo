# LIV-1366 prompt template includes for shared prompt fragments

This ExecPlan review document is paired with a structured implementation pack. The review document stays concise and human-reviewable; the implementation pack contains the mechanical file-by-file steps, interfaces, tests, and command details.

## Purpose / Big Picture

After this change, Scherzo workflow authors can place shared prompt policy in one Markdown fragment and inline it with `{% include "fragments/name.md" %}` from workflow prompt templates. Operators should see the same rendered prompts as before for the migrated dogfood workflows, while future edits to the ExecPlan identity model or verification contract happen once and affect every consuming step through normal workflow fingerprint drift.

## Problem Framing and Constraints

The current template engine in `src/scherzo/template.gleam` supports variables, `if`, and `for`, but it has no partial mechanism. The dogfood workflow bundle therefore repeats policy blocks such as the ExecPlan identity model and plan-completion verification contract across many prompt files, making mid-pipeline policy drift likely when only some copies are edited.

The solution must preserve strict prompt rendering, keep include paths portable and inside the workflow bundle, make missing or unsafe includes fail during config or doctor load, and keep fragment edits visible to workflow fingerprints. It must remain a small v1 feature: no include parameters, no remote files, no dynamic include paths, and no broader template language expansion.

## Strategy Overview

Add a load-time include expansion pass owned by the template module and used by runtime bundle prompt loading. The pass recognizes only `{% include "relative/path.md" %}`, resolves paths relative to the file that contains the include, rejects absolute or parent-segment paths, enforces workflow-bundle containment, tracks cycles, and stops at a depth limit of three include edges. Expansion happens before variable and block rendering, so fragments use the same context as their host prompt.

Runtime bundle loading should inline root prompt files and all resolved fragments into the prompt text stored in the workflow DAG. That keeps existing agent rendering simple and lets the current fingerprint path hash effective prompt content without inventing a separate fingerprint subsystem. The dogfood migration should then create prompt fragments under `.scherzo/workflows/prompts/fragments/` and replace duplicated policy blocks in the surviving ExecPlan implementation prompts after confirming the current post-LIV-1350 prompt inventory.

## Alternatives Considered

Leaving duplicated policy text in place is lowest risk for the template engine but preserves the drift problem this task is meant to solve. A preprocessor script or lint-only shared-fragment check would reduce some duplication but would not give workflow authors a native, documented authoring mechanism and would still require every renderer and doctor path to agree on script behavior. Parameterized includes or a larger Liquid-compatible engine are rejected for v1 because the host step context is already sufficient and a bigger language would increase safety and portability risk.

Resolving include paths from the workflow bundle root was considered, but relative-to-including-file resolution is easier for authors to understand when fragments include subfragments and keeps prompt-local organization self-contained. Bundle containment and parent-segment rejection retain the existing prompt-path safety model.

## Risks and Countermeasures

A missing or unsafe fragment could otherwise fail only when an agent step renders. The countermeasure is to expand includes while loading workflow config so `scherzo doctor --check workflow-config` and the workflow-portability gate fail before dispatch.

A fragment edit might not invalidate downstream runs if fingerprints hash only the root prompt path. The countermeasure is to store expanded prompt content in the loaded DAG and add fingerprint tests proving a fragment content change changes the workflow fingerprint for every consumer.

Cycles and deep include chains could hang or make errors hard to diagnose. The countermeasure is stack-based cycle detection, a fixed depth limit of three include edges, and clear `TemplateRenderError` messages that name the include chain or offending path.

Dogfood migration could accidentally change effective prompts while deduplicating text. The countermeasure is to compare rendered effective prompts before and after extraction for representative issue context, and to rely on retained prompt artifacts from a routed run or equivalent dry-run evidence before publishing.

## Scope Boundaries

In scope are the `{% include "path.md" %}` syntax, template-level include expansion and referenced-variable traversal, runtime bundle integration for prompt and recovery prompt files, doctor/load-time failure on missing or invalid fragments, workflow fingerprint coverage for fragment content, workflow-portability coverage through config loading, documentation updates for workflow authors, and dogfood extraction of the ExecPlan identity model plus the shared verification-contract block from the surviving implementation workflow prompts.

The likely code and test surface is `src/scherzo/template.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/workflow_fingerprint.gleam` only if the existing expanded-prompt fingerprint path is insufficient, `test/template_test.gleam`, `test/runtime_bundle_test.gleam`, `test/workflow_fingerprint_test.gleam`, `test/workflow_portability_test.gleam`, `docs/GETTING_STARTED.md`, `docs/SYMPHONY_SPEC.md`, and `.pi/skills/scherzo-workflow-author/SKILL.md`.

Out of scope are parameterized includes, includes from command-step shell bodies, remote includes, filesystem watching beyond the existing config reload model, a general fragment registry, and unrelated prompt rewrites. If LIV-1362's prompt-fragment drift lint exists by implementation time, only shrink it to an orphan-fragment check; if it is not present, do not create a new drift lint as part of this task.

## Milestones

Milestone 1 proves the template feature in isolation. At the end, template tests cover a simple include, nested includes, variables and blocks inside fragments, malformed include syntax, missing fragments, cycle detection, depth-limit failure, and referenced-variable traversal through included content.

Milestone 2 wires include expansion into workflow loading. At the end, root prompt files and recovery prompt files load as expanded inline prompts, missing or unsafe fragments fail config loading with actionable diagnostics, scheduled workflow validation sees variables referenced only from fragments, and workflow fingerprints change when fragment content changes.

Milestone 3 validates portability and documentation. At the end, the workflow-portability gate exercises a bundle containing a fragment include, workflow authoring docs explain syntax and resolution rules, and the repository-local workflow-author skill tells authors when to prefer fragments and how to avoid unsafe include paths.

Milestone 4 migrates dogfood prompts without changing behavior. At the end, `.scherzo/workflows/prompts/fragments/execplan-identity-model.md` and a verification-contract fragment exist once, all surviving prompts that need them include them, the old duplicated blocks are gone from those hosts, and rendered effective prompt evidence shows parity with the pre-extraction text except for intentional whitespace normalization if any is explicitly recorded.

## Progress

- [x] (2026-07-05) Read the repo-local ExecPlan workflow guidance and prepared this concise review document with mechanical implementation detail reserved for the structured pack.
- [x] (2026-07-05) Inspected the current template engine, prompt loading, fingerprinting, doctor/load path, portability harness, workflow authoring docs, and duplicated dogfood prompt blocks.
- [ ] Implement template include expansion and focused tests.
- [ ] Wire include expansion into runtime bundle loading, doctor validation, scheduled reference validation, and fingerprint coverage.
- [ ] Update documentation and the workflow-author skill.
- [ ] Extract dogfood prompt fragments and prove rendered prompt parity.

## Surprises & Discoveries

The runtime bundle already resolves prompt files into `PromptInline` content before validation and fingerprinting, so include expansion can piggyback on that load-time path instead of changing the agent execution loop. Current `referenced_variables` scans a string without file context, which means include traversal needs either a resolver-aware function or guaranteed pre-expansion before callers such as scheduled workflow validation.

The current tree still contains the five verify-family prompt files, and the ExecPlan identity model appears in twelve prompt files under `.scherzo/workflows/prompts/`. The implementation should re-check the prompt inventory at start because the task description expects LIV-1350 to remove some verify prompts before dogfood extraction.

## Decision Log

- Decision: Resolve include paths relative to the including prompt or fragment file, not the workflow YAML file or process working directory.
  Rationale: Authors can move a fragment directory as a unit and nested fragments are easier to reason about, while bundle containment still preserves portability.
  Date: 2026-07-05.

- Decision: Expand includes at workflow config load time and store expanded prompt text in the loaded DAG.
  Rationale: This makes doctor, portability, scheduled validation, rendering, and fingerprinting observe the same effective prompt.
  Date: 2026-07-05.

- Decision: Keep v1 include syntax literal-only with no parameters and a three-edge depth limit.
  Rationale: The existing step context supplies variables to fragments, and a small strict feature solves the duplication problem without broadening the template language.
  Date: 2026-07-05.

## Outcomes & Retrospective

Not started. Implementation should update this section after each milestone with the actual behavior delivered, validation evidence collected, any prompt parity differences, and any deferred human/operator dogfood checks.

## Validation and Acceptance

Acceptance requires automated unit coverage for include parsing and expansion, negative coverage for malformed, missing, unsafe, cyclic, and too-deep includes, and traversal coverage showing variables referenced only inside fragments are discovered. Runtime validation must show doctor/config loading fails before dispatch for missing or escaping fragments, scheduled workflow validation rejects `issue.*` references that appear only inside fragments, and workflow fingerprints change when a consumed fragment changes.

Dogfood acceptance requires the shared identity-model fragment to exist once and be included everywhere it is needed in the surviving bundle, plus a shared verification-contract fragment used by the remaining verify prompts. Rendered effective prompts for migrated routes must match the previous policy text, and retained prompt artifacts from a routed run or an explicitly documented equivalent dry-run must provide the evidence. Final validation must include the Gleam test suite, formatting, glinter, Scherzo lint, doctor workflow-config, workflow portability, and review-doc validation.

## Rollout, Recovery, and Idempotence

Rollout is additive for syntax: existing prompts without includes keep rendering as before. If include expansion causes a config-load failure in production dogfood, operators can revert the prompt migration by replacing include tags with the fragment contents, or revert the engine change; no stored run data migration is required.

Repeated implementation steps are safe when they preserve the fragment files and replace duplicated host blocks deterministically. Re-running doctor, portability, and fingerprint tests should produce the same pass/fail result for the same tree. If a fragment edit invalidates active work through workflow fingerprint drift, that is expected and should be handled through the existing workflow-drift safeguards rather than bypassed.

## Open Questions and Clarifications Needed

No blocking open questions. At implementation start, confirm whether LIV-1350 has landed and extract fragments only from the prompt set that exists in that current tree; do not preserve prompts that another landed change has intentionally removed.
