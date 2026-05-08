# Stage remaining Scherzo public API style lint rules

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo maintainers need a safe way to evaluate additional public API style lint rules without repeating the churn risk found during PR #82. After this plan is implemented, an operator can run an explicit production inventory for high-arity public functions, duplicate primitive public parameters, and broader unlabelled public `Bool` parameters; review the findings by module, subsystem, API shape, and likely exception class; and decide whether each candidate remains off, becomes inventory-only, becomes a warning, becomes module-scoped blocking, or becomes globally blocking.

The visible outcome is not a broad relabeling refactor. The visible outcome is a repeatable inventory and rollout decision process that prevents a rule from becoming CI-blocking until its production blast radius is understood and below an agreed churn threshold.

## Problem Framing and Constraints

PR #82 introduced the first repository-local Scherzo custom Glinter gate. The broader rule inventory originally found 247 production errors before rollout was narrowed to a lower-churn two-parameter public `Bool` rule. That history is the central constraint for this plan: a rule that sounds narrow in prose can still be too broad for a production API without a measured inventory.

The operator pain today is uncertainty. Maintainers know that high-arity public functions, duplicate primitive public parameters, and broader unlabelled `Bool` parameters may reduce API clarity, but they do not yet know which findings are genuine improvements, which are conventional exceptions, and which would force noisy public signature and call-site churn. This plan therefore treats inventory as a rollout gate. It does not assume any candidate should become PR-blocking immediately.

The plan must preserve the existing PR #82 behavior. The current two-parameter public `Bool` rule remains compatible with any future candidates and is not broadened as part of the first inventory slice. Future blocking is allowed only after the inventory is recorded, exceptions are classified, and thresholds in this plan are met.

## Strategy Overview

The approach is to stage the work in four increments. First, add an inventory-only analyzer and tests for public API shapes under the existing custom lint test harness. This analyzer reports facts and candidate findings but does not change `gleam.toml`, normal `glinter`, or the existing `scherzo_lint` gate. Second, run the analyzer on `src/` and record a concise production inventory grouped by module/subsystem and API shape. Third, use predefined thresholds to decide each candidate rule state. Fourth, only if a candidate is below threshold, add the smallest safe lint surface, preferably warning/inventory-only or module-scoped blocking rather than global blocking.

This is proportionate because the riskiest unknown is not whether a rule can be coded; the risky unknown is how many production APIs it would touch and whether those touches are worthwhile. A standalone inventory runner proves or disproves that assumption before any CI or SelfCI gate can block a PR.

## Alternatives Considered

The simplest plausible alternative is to enable Glinter's closest existing rules, or new Scherzo rules, as warnings or errors and let normal developer feedback discover the blast radius. That is insufficient because PR #82 already showed that prose-level confidence can hide a 247-error production inventory. Even warnings can become noisy enough that developers ignore the custom lint output.

Another option is to do a large production API relabeling refactor first and then enable all rules. That is too risky for this follow-up. It mixes style policy with broad signature churn, creates many call-site changes, and makes it hard to separate real lint value from mechanical cleanup.

A third option is to leave all remaining candidates off permanently. That is safe but loses the opportunity to improve public API readability where the inventory proves the change is small and high-signal. The chosen approach keeps the default safe state while still making progress through measured, reviewable slices.

## Risks and Countermeasures

The main risk is accidental gate expansion. A new candidate rule could start failing `direnv exec . gleam run -m scherzo_lint` before maintainers have agreed to the inventory. The countermeasure is that the first implementation milestone adds an explicit inventory runner and tests but does not register candidate rules in `test/scherzo_lint.gleam` and does not add candidate severities to `gleam.toml`.

Another risk is false positives in conventional APIs. Callbacks, comparators, FFI wrappers, decode helpers, builder-style helpers, and generated or test-adjacent code can look suspicious to a simple signature rule while still being conventional. The countermeasure is to classify likely exceptions in the inventory before deciding severity. External FFI functions marked with `@external` are excluded from diagnostics, matching the current two-parameter public `Bool` rule.

A third risk is broad public churn. Relabeling public Gleam parameters changes call sites and may force updates across many modules. The countermeasure is to define hard churn thresholds before any CI or SelfCI wiring. If a candidate exceeds a threshold, it stays off or inventory-only.

A fourth risk is duplicated diagnostics with the existing PR #82 rule. The countermeasure is to keep `scherzo_public_function_labels` unchanged and treat two-parameter public `Bool` findings as already covered. Any future broader `Bool` candidate must either exclude the covered two-parameter shape from normal diagnostics or explicitly replace the old rule in a dedicated follow-up with an equivalent compatibility test.

## Progress

- [x] (2026-05-08 00:00Z) Drafted this ExecPlan proposal from Linear issue LIV-140 and verified the current custom lint entry points and configuration.
- [ ] Add inventory-only analysis code and tests for the three public API style candidates.
- [ ] Run the production inventory against `src/` and record a concise findings summary.
- [ ] Apply the thresholds in this plan to choose off, warning/inventory-only, module-scoped blocking, or globally blocking for each candidate.
- [ ] If and only if a candidate is below threshold, implement the smallest safe lint configuration change and document suppression guidance.

## Surprises & Discoveries

- Observation: The existing custom Scherzo lint rule lives under `test/`, not `src/`, and is invoked through a test-module entry point.
  Evidence: `test/scherzo_lint.gleam` calls `glinter.run(extra_rules: [public_function_labels.rule()])`.
- Observation: The current PR #82 rule intentionally defers high-arity, duplicate primitive, and three-parameter `Bool` cases.
  Evidence: `test/scherzo_lint/public_function_labels_test.gleam` includes tests named `three_parameter_bool_is_deferred_by_rollout_test`, `duplicate_string_parameters_are_deferred_by_rollout_test`, and `high_arity_without_bool_is_deferred_by_rollout_test`.

## Decision Log

- Decision: Start with an inventory-only runner rather than registering new candidate rules in the normal custom lint gate.
  Rationale: PR #82 found 247 production errors before narrowing the rollout, so inventory must precede any blocking or noisy normal-lint behavior.
  Date: 2026-05-08
- Decision: Keep `scherzo_public_function_labels` unchanged in the first slice.
  Rationale: It is already configured as an error in `gleam.toml` and has compatibility tests that document deferred cases.
  Date: 2026-05-08
- Decision: Use explicit numeric churn thresholds before any CI or SelfCI wiring.
  Rationale: This makes the rollout falsifiable and prevents subjective "this seems narrow" decisions from expanding into broad API churn.
  Date: 2026-05-08

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

This repository is a Gleam project. Production code lives under `src/`. Tests and custom development tooling live under `test/`. The project uses Glinter, a Gleam lint runner, for production safety and style checks. The standard production Glinter command is:

    direnv exec . gleam run -m glinter

The Scherzo-specific custom lint command is:

    direnv exec . gleam run -m scherzo_lint

The current custom lint entry point is `test/scherzo_lint.gleam`. It registers the rule defined in `test/scherzo_lint/rules/public_function_labels.gleam`. That rule is named `scherzo_public_function_labels` and currently reports public, non-`@external` functions with exactly two parameters when at least one unlabelled parameter has primitive type `Bool`.

In this plan, "public API style candidate" means a lintable shape in a public Gleam function signature that may make call sites ambiguous. "High arity" means a public function with many parameters. The inventory must report arity buckets `4`, `5`, and `6+`; only arity `5+` is eligible for a first blocking proposal. "Duplicate primitive public parameters" means a public function with at least two unlabelled parameters of the same primitive type among `Bool`, `String`, `Int`, and `Float`. "Broader unlabelled Bool" means an unlabelled public `Bool` parameter outside the existing two-parameter shape, especially arity `1` and arity `3+` functions.

The inventory must group modules into subsystems by repository-relative path prefix:

- `src/scherzo/control/` and `src/scherzo/ctl.gleam` are control and CLI surfaces.
- `src/scherzo/orchestrator/` is orchestration.
- `src/scherzo/agent/` and `src/scherzo/pi/` are agent and pi integration.
- `src/scherzo/session/` is session tracking.
- `src/scherzo/state/` is persisted state and projection.
- `src/scherzo/config.gleam`, `src/scherzo/config/`, and `src/scherzo/model_config.gleam` are configuration.
- `src/scherzo/linear*.gleam` and `src/scherzo/linear_*` modules are Linear integration.
- `src/scherzo/workspace*.gleam` and `src/scherzo/workspace/` are workspace management.
- Other `src/scherzo/*.gleam` files are top-level utilities unless the inventory author adds a more precise subsystem label.

## Preconditions and Verified Facts

The repository must be used from its root directory. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through `direnv exec .`.

The current `gleam.toml` configures Glinter to include `src/` and exclude `test/`. It sets `scherzo_public_function_labels = "error"`. It also has dev dependencies on `glinter` and `glance`, which are the relevant lint runner and Gleam source parser used by the existing custom rule tests.

The current `test/scherzo_lint/rules/public_function_labels.gleam` excludes public functions with `@external` attributes, ignores discarded or unannotated parameters, and uses suppression guidance in the form:

    // nolint: scherzo_public_function_labels -- reason

The current `test/scherzo_lint/public_function_labels_test.gleam` verifies that labelled public `Bool` parameters do not report, private functions do not report, external public functions do not report, discarded `Bool` parameters do not report, and high-arity, duplicate primitive, and three-parameter `Bool` shapes are deferred.

A `.config/selfci` path exists in the repository. [CLARIFY] The exact SelfCI invocation was not inspected during this plan-authoring pass. If a future implementation changes CI or SelfCI wiring, verify the repository's current SelfCI command from `.config/selfci` or the project runbook before relying on the placeholder command in this plan.

No existing `docs/plans/LIV-140-*.md` file was found before this plan file was created.

## Scope Boundaries

In scope for the future implementation is an inventory-only analysis path for high-arity public functions, duplicate primitive public parameters, and broader unlabelled public `Bool` parameters. Also in scope is a recorded inventory summary and a recommendation for each candidate rule state.

Out of scope for the first implementation slice is enabling high-arity or duplicate-primitive linting as a blocking CI gate. Also out of scope is a broad public API relabeling refactor. The implementation may fix a tiny number of local examples only if the inventory proves they are below the thresholds in this plan and the fixes do not create broad call-site churn.

The existing `scherzo_public_function_labels` rule stays compatible. Do not broaden it in place during the inventory slice. Do not remove its existing tests. If a later milestone introduces a broader `Bool` candidate, avoid duplicate diagnostics for the two-parameter shape that the existing rule already owns.

Generated code and test-adjacent code are not expected in `src/`, but if the inventory finds generated-looking files or production fixtures, classify them as likely exceptions rather than forcing immediate relabeling.

## Candidate Rule States and Thresholds

The default state for all three candidates is inventory-only. A candidate may move to warning-only only when the recorded production inventory has fewer than 50 total findings, fewer than 20 percent likely false positives, and no single subsystem with more than 20 findings. A warning-only rule must not fail `direnv exec . gleam run -m scherzo_lint`.

A candidate may move to module-scoped blocking only when the selected module or subsystem has no more than 5 unsuppressed findings, no more than 10 estimated call-site edits, and fewer than 10 percent likely false positives. Module-scoped blocking is appropriate for a subsystem with clear API conventions and low churn, not as a way to hide a broad global problem.

A candidate may move to globally blocking only when the entire `src/` inventory has no more than 15 unsuppressed findings, no more than 5 affected modules, no more than 30 estimated call-site edits, fewer than 5 percent likely false positives, and zero unresolved conventional exception classes. Any number near the PR #82 247-error inventory is automatically too high for global blocking.

If a candidate exceeds these thresholds, it remains off or inventory-only. If the candidate is high-signal but too broad, prefer fixing one narrow subsystem and re-running the inventory rather than enabling a blocking rule.

Initial state decisions are:

- High-arity public functions: inventory-only. Consider warning-only after inventory if total arity `5+` findings are below the warning threshold. Consider module-scoped blocking only for a subsystem with at most 5 arity `5+` findings and no callback-heavy conventional exceptions. Do not globally block arity `4` findings in the first rollout.
- Duplicate primitive public parameters: inventory-only, likely the safest future warning candidate if duplicate unlabelled primitives are rare and mostly ordinary data arguments. Do not block until duplicate primitives are broken down by primitive type and by whether labels already exist.
- Broader unlabelled `Bool` cases: off or inventory-only beyond the existing two-parameter error rule. This candidate overlaps the current gate and is likely to produce API churn in higher-arity functions, so it should not become blocking until after the inventory proves that arity `3+` findings are rare and not conventional builder or callback APIs.

## Milestones

Milestone 1 adds the inventory analyzer without changing any gate. At the end of this milestone, developers can parse sample Gleam modules and receive structured findings for the three candidate shapes. Normal `direnv exec . gleam run -m scherzo_lint` output is unchanged except for any unrelated baseline drift.

Milestone 2 runs the analyzer on production code and records the findings. At the end of this milestone, the repository has a concise inventory summary that shows counts by subsystem, module, candidate, API shape, and likely exception class. This is the rollout gate.

Milestone 3 applies the thresholds and records a recommendation. At the end of this milestone, each candidate is explicitly categorized as off, warning/inventory-only, module-scoped blocking, or globally blocking. The recommendation explains why any safe first slice is below the churn threshold.

Milestone 4 is optional and only happens if Milestone 3 identifies a below-threshold slice. It adds the smallest safe lint behavior, such as warning-only diagnostics or module-scoped blocking, plus suppression documentation and validation. If no candidate is below threshold, the correct outcome is to stop after recording the inventory and leave the rules off or inventory-only.

## Plan of Work

Add a new inventory namespace under `test/scherzo_lint/public_api_style/`. Keep it under `test/` because it depends on development lint tooling and is not production Scherzo runtime code. Define a small data model for candidates, findings, subsystem names, API shape classifications, and likely exception classes. Reuse `glance.module(source)` and the same public and `@external` filtering ideas already present in `test/scherzo_lint/rules/public_function_labels.gleam`.

Create `test/scherzo_lint/public_api_style/inventory.gleam` with pure functions that analyze source strings and return findings. The analyzer should inspect public functions only. It should skip functions with `@external`. It should record but not diagnose parameters that are already labelled, so the report can distinguish "already clear" from "ambiguous" APIs. It should report arity buckets `4`, `5`, and `6+`; duplicate unlabelled primitive types among `Bool`, `String`, `Int`, and `Float`; and unlabelled `Bool` outside the current two-parameter covered shape.

Create `test/scherzo_lint/public_api_style/report.gleam` to render a stable Markdown or CSV report. The report must use repository-relative paths only. Each row must include candidate, subsystem, module path, function name, arity, API shape, duplicate primitive type if any, whether the existing two-parameter `Bool` rule already covers the shape, likely exception class, and a short churn estimate.

Create `test/scherzo_lint_inventory.gleam` as an explicit command-line runner. It should default to scanning `src/`, should accept an output path, and should fail only on parse or I/O errors, not because findings exist. The runner exists to produce inventory; it is not a lint gate.

Add tests in `test/scherzo_lint/public_api_style_inventory_test.gleam`. Tests should exercise pure analysis with inline source strings. Include cases for public high arity, private high arity, external high arity, duplicate unlabelled `String` parameters, duplicate labelled `String` parameters, broader three-parameter unlabelled `Bool`, single-parameter unlabelled `Bool`, existing two-parameter `Bool` marked as covered by `scherzo_public_function_labels`, discarded parameters, unannotated parameters, callback-like parameters, comparator-like shapes, and decode-helper-like names.

After the inventory exists, run it on `src/` and save a generated report outside committed source first, for example under `build/public-api-style-inventory.md`. Review it manually and then create or update a concise committed summary, preferably `docs/lint/public-api-style-inventory.md` if that directory exists or is appropriate. The committed summary should not be a noisy dump; it should contain counts, examples, exception categories, and the rule-state recommendation.

Only after the summary is reviewed should the implementer consider registering candidate rules in `test/scherzo_lint.gleam` or changing `gleam.toml`. If candidate rules are added, name them distinctly from the existing rule, for example `scherzo_public_high_arity`, `scherzo_duplicate_primitive_parameters`, and `scherzo_public_bool_parameters`. Defaults should be warning or off unless the thresholds permit module-scoped or global blocking.

## Concrete Steps

1. From the repo root, inspect source control state:

       jj status --color=never

   Expect either a clean tree or only the changes from the current implementation task. Do not manage jj workspaces from this plan.

2. If `direnv exec . gleam --version` fails because `.envrc` is blocked, inspect `.envrc`, run:

       direnv allow .

   Then retry all validation commands through `direnv exec .`.

3. Add `test/scherzo_lint/public_api_style/inventory.gleam`. Define these public types and functions, adjusting only for exact Gleam syntax required by the compiler:

       pub type Candidate {
         HighArity
         DuplicatePrimitiveParameters
         BroaderBoolParameters
       }

       pub type Finding {
         Finding(
           candidate: Candidate,
           path: String,
           module_name: String,
           subsystem: String,
           function_name: String,
           arity: Int,
           api_shape: String,
           primitive_type: option.Option(String),
           covered_by_existing_rule: Bool,
           likely_exception: option.Option(String),
           estimated_callsite_churn: Int,
         )
       }

       pub fn findings_for_source(path: String, source: String) -> Result(List(Finding), InventoryError)

   Keep the analysis pure so tests can pass inline source strings.

4. Add `test/scherzo_lint/public_api_style/report.gleam`. Implement deterministic sorting by candidate, subsystem, path, and function name. Render paths exactly as repository-relative paths.

5. Add `test/scherzo_lint_inventory.gleam`. Implement a runner that scans `src/`, writes a report to the path passed by `--output`, and exits successfully when findings exist. Findings are data, not failures.

6. Add `test/scherzo_lint/public_api_style_inventory_test.gleam`. Include tests for the exact scenarios listed in the Plan of Work. Reuse the inline-source testing style from `test/scherzo_lint/public_function_labels_test.gleam`.

7. Run:

       direnv exec . gleam format --check src test

   Expect formatting to pass. If it fails, run `direnv exec . gleam format src test`, review the diff, and repeat the check.

8. Run:

       direnv exec . gleam test

   Expect all tests to pass, including the new inventory tests.

9. Run the existing gates:

       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect no new production errors from the inventory-only analyzer. The Scherzo custom lint output should still be governed by `scherzo_public_function_labels` only unless a later optional milestone registers candidates.

10. Produce the production inventory:

       mkdir -p build
       direnv exec . gleam run -m scherzo_lint_inventory -- --path src --format markdown --output build/public-api-style-inventory.md

    Expect the command to exit successfully even if findings exist. The output file should contain grouped counts and example rows with repository-relative paths.

11. Review `build/public-api-style-inventory.md`. Classify every finding into one of these categories: ordinary actionable API ambiguity, callback or comparator convention, FFI wrapper, decode helper, builder-style helper, generated or test-adjacent production artifact, already covered by `scherzo_public_function_labels`, or unclear.

12. Create a concise committed inventory summary, such as `docs/lint/public-api-style-inventory.md`, with counts by candidate, subsystem, and API shape. Include representative examples and likely exceptions, but avoid committing a huge raw dump unless reviewers ask for it.

13. Apply the thresholds from `## Candidate Rule States and Thresholds`. Record the chosen state for each candidate in the inventory summary and in the Decision Log of this ExecPlan if this plan is being updated during implementation.

14. Commit the inventory-only slice after `gleam format --check src test`, `gleam test`, `gleam run -m glinter`, and `gleam run -m scherzo_lint` all pass through `direnv exec .`. Suggested commit message: `Add public API style lint inventory`.

15. Optional: if a candidate is below threshold, add the smallest safe Glinter rule module under `test/scherzo_lint/rules/`, register it in `test/scherzo_lint.gleam`, and set the chosen severity in `gleam.toml`. Prefer warning-only before blocking unless the module-scoped or global blocking thresholds are clearly met.

16. Optional: if any rule becomes blocking, add suppression examples to the committed inventory summary or lint runbook. Use one suppression line directly above the function:

       // nolint: scherzo_duplicate_primitive_parameters -- comparator-style API uses conventional left/right argument order
       pub fn compare(left: String, right: String) -> Order {
         // existing body
       }

    Suppressions are acceptable only for documented conventional exceptions, external API compatibility, generated code that cannot be shaped locally, or a false positive with an explanation. Suppressions are not acceptable merely to avoid a straightforward local label.

17. Optional: if CI or SelfCI wiring changes, run normal validation plus the repository's SelfCI command. The placeholder command is:

       direnv exec . selfci run

    [CLARIFY] Replace that placeholder with the exact command documented by `.config/selfci` or the current project runbook before implementation relies on it.

## Testing and Falsifiability

The first falsifiable claim is that inventory can be produced without changing normal lint gates. This is false if `direnv exec . gleam run -m scherzo_lint` starts reporting high-arity, duplicate primitive, or broader `Bool` findings before a severity decision is made. The test is to run the existing Scherzo custom lint command after adding the inventory analyzer and confirm only the existing registered rule participates.

The second falsifiable claim is that the analyzer correctly distinguishes candidate shapes. Add tests in `test/scherzo_lint/public_api_style_inventory_test.gleam` with inline source strings:

- A public function with four parameters produces a `HighArity` inventory row in the arity `4` bucket but is not eligible for first blocking.
- A public function with five parameters produces a `HighArity` row eligible for threshold evaluation.
- A private function with five parameters produces no finding.
- An `@external` public function with five parameters produces no diagnostic finding and is either omitted or classified as an FFI exception in inventory.
- A public function `copy(source: String, destination: String)` produces a duplicate `String` primitive finding when both parameters are unlabelled.
- A public function with labelled duplicate strings does not produce an actionable duplicate primitive finding, but may be counted as already-labelled context if the report includes that category.
- A public function with three parameters including an unlabelled `Bool` produces a broader `Bool` finding.
- A public function with exactly two parameters including an unlabelled `Bool` is marked `covered_by_existing_rule: True` and does not become a duplicate diagnostic from the broader rule.
- A discarded `Bool` parameter and an unannotated parameter do not produce actionable findings.
- Comparator-like, callback-like, decode-helper-like, and builder-style examples receive likely exception hints so reviewers can validate false-positive classification.

The third falsifiable claim is that a candidate is safe to promote. This claim is false if the recorded inventory exceeds the thresholds in this plan, if false positives exceed the threshold, or if estimated call-site churn exceeds the threshold. In that case, the implementation must leave the candidate off or inventory-only.

## Validation and Acceptance

For the inventory-only slice, acceptance is:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint
    direnv exec . gleam run -m scherzo_lint_inventory -- --path src --format markdown --output build/public-api-style-inventory.md

The expected result is that format, tests, normal Glinter, and existing Scherzo custom lint all pass, and the inventory command exits successfully while writing a report. Finding rows in the inventory report are not failures.

For any optional warning or blocking slice, acceptance additionally requires that the inventory summary names the chosen severity and shows the threshold math. If a blocking rule is proposed, reviewers must be able to see the exact un-suppressed count, affected modules, false-positive count, and estimated call-site churn. A blocking proposal fails acceptance if it lacks suppression syntax and examples.

If CI or SelfCI wiring is proposed, acceptance additionally requires running the repository's SelfCI command and recording the short result transcript in the PR description or implementation notes. Until the exact SelfCI command is verified, do not claim SelfCI coverage.

## Rollout, Recovery, and Idempotence

The inventory-only milestone is safe and reversible. It adds development tooling and tests but does not change production runtime behavior and does not block PRs. Re-running the inventory is idempotent: it reads `src/` and rewrites the requested report output.

If a warning-only rule proves too noisy, recover by setting the candidate rule to `off` in `gleam.toml` or by unregistering it from `test/scherzo_lint.gleam`. If a blocking rule unexpectedly blocks too much, recover by reverting the severity change first while keeping the inventory code and summary for future analysis.

If implementation stops after Milestone 1 or Milestone 2, the repository remains safe because no candidate gate has been enabled. The remaining work is policy review, not partial runtime behavior.

## Artifacts and Notes

The following facts were verified while drafting this plan:

    jj status --color=never
    The working copy has no changes.

    gleam.toml
    [tools.glinter] includes src/ and excludes test/.
    [tools.glinter.rules] sets scherzo_public_function_labels = "error".

    test/scherzo_lint.gleam
    glinter.run(extra_rules: [public_function_labels.rule()])

The current custom rule detail text already documents this suppression form:

    // nolint: scherzo_public_function_labels -- reason

Future candidate suppressions must use the same style with the specific rule name and a reason after `--`.

## Interfaces and Dependencies

Use existing dependencies already present in `gleam.toml`: `glance` for parsing Gleam source, `glinter` for optional future rule integration, `gleam_stdlib` for data processing, and `simplifile` if the inventory runner needs file traversal and writes. Do not add a new package just to scan `src/` or render Markdown.

The inventory analyzer should expose pure functions from `test/scherzo_lint/public_api_style/inventory.gleam` so tests can avoid filesystem setup. The command-line runner in `test/scherzo_lint_inventory.gleam` should be thin: parse arguments, read files, call the pure analyzer, render the report, write the output path, and return a nonzero status only for invalid arguments, parse errors, or file I/O errors.

If future Glinter rules are added, place them under `test/scherzo_lint/rules/` beside `public_function_labels.gleam`. Use names that match the configured rule keys exactly. Candidate rule names should be stable and specific:

- `scherzo_public_high_arity`
- `scherzo_duplicate_primitive_parameters`
- `scherzo_public_bool_parameters`

Do not make candidate rules depend on absolute local paths. All diagnostics, report rows, and documentation examples must use repository-relative paths.

## Open Questions and Clarifications Needed

- [CLARIFY] Verify the exact SelfCI command before any implementation changes `.config/selfci` or CI wiring. This plan uses `direnv exec . selfci run` only as a placeholder.
