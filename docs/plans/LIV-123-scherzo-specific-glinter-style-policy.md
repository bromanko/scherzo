# Codify Scherzo-Specific Glinter Style Policy

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo already has a checked production Glinter gate, but the gate deliberately keeps broad style-only rules such as `label_possible` turned off because they are too noisy for this repository. The result is that agents and humans must remember Scherzo's house style for labelled arguments by convention. After this plan is implemented, contributors will have an explicit lint command that enforces a narrow Scherzo-specific labelled-argument policy for production Gleam source, and SelfCI will run that command so new code cannot silently drift from the policy.

The visible outcome is operational rather than runtime behavior: from the repository root, a developer can run `direnv exec . gleam run -m scherzo_lint` and see the same Glinter-style diagnostics as the normal production gate, plus Scherzo-specific style errors. A public production function such as `pub fn configure(path: String, enabled: Bool)` will fail the custom lint because the boolean parameter is ambiguous at call sites, while labelled or exempt forms will pass.

## Problem Framing and Constraints

The concrete problem is review and maintenance friction. Scherzo is changed by both humans and coding agents, and reviewers should not have to repeatedly explain that some public functions need labels while blanket labelling of every possible argument is not wanted. Built-in Glinter rules do not exactly express that distinction: `label_possible` reports many cases Scherzo intentionally tolerates, while leaving it off provides no automated guardrail.

This plan must not mass-enable broad style rules, must not fix unrelated `discarded_result`, `thrown_away_error`, or `stringly_typed_error` warnings, and must not change runtime behavior except for adding development-time lint tooling. The production lint policy remains distinct from the test policy. Production source under `src/` receives the checked safety and style gates. Test source under `test/` remains formatted and unit-tested, but it does not inherit production-only rules such as `assert_ok_pattern`, `unused_exports`, or public API labelling unless a future targeted test profile justifies that extra noise.

A lint failure is allowed to require signature and call-site edits, because Gleam labels are compile-time call-site clarity features. The implementation must keep this targeted: it may adjust existing production functions that violate the new narrow rule, or add a narrowly justified `// nolint: scherzo_public_function_labels -- reason` directly above a function when the rule cannot recognize a conventional callback, comparator, or FFI shape. It must not use this issue as a pretext to convert the whole codebase to satisfy `label_possible`.

## Strategy Overview

Use Glinter's documented custom rule path rather than a separate parser or a fragile text wrapper. Add an explicit development dependency on `glance` before the custom rule imports `glance`, because the rule code type-checks against `glance` AST values directly and should not rely on Glinter's transitive dependency graph. Add a dev-only runner module at `test/scherzo_lint.gleam` whose `main` function calls `glinter.run(extra_rules: [...])`. Put the custom rule implementation under `test/scherzo_lint/rules/public_function_labels.gleam`, also dev-only, so production modules under `src/` do not import Glinter or Glance. The runner still lints production source because `gleam.toml` configures `[tools.glinter] include = ["src/"]`, and `glinter.run` uses that configuration when no explicit paths are passed.

The first Scherzo-specific rule is named `scherzo_public_function_labels`. It is an error. During implementation, the originally planned broader policy was tested first and produced 247 production errors, which exceeded the plan's churn threshold. The active version 1 rollout is therefore deliberately narrower: it inspects public production function definitions and reports one diagnostic when a function has exactly two parameters and an unlabelled named parameter annotated as the primitive type `Bool`. The rule ignores private functions, `@external` functions, labelled parameters, discarded parameters, unannotated parameters, and Bool parameters in functions with any arity other than two. High-arity public APIs and duplicate primitive parameters remain documented as future candidates, but they are intentionally deferred from this first blocking rollout.

This smaller rule still catches the motivating ambiguity represented by `pub fn configure(path: String, enabled: Bool)`, while keeping the first SelfCI gate low-churn and reversible. Existing production violations were fixed by adding labels and updating compiler-reported call sites rather than adding suppressions.

## Alternatives Considered

The simplest alternative is documentation only: update `AGENTS.md` or `README.md` to say that public booleans and high-arity APIs should be labelled. That is insufficient because the current pain is exactly that informal convention is weak when multiple agents and humans edit the codebase.

Enabling built-in `label_possible` is also rejected. The current `gleam.toml` deliberately sets both `label_possible = "off"` and `missing_labels = "off"`, and Glinter's built-in `label_possible` reports any unlabelled parameter in most functions with two or more parameters, except for some broad suppressions. That is still too blunt for Scherzo's desired distinction between public API ambiguity and pipeline-friendly private helper code.

A shell wrapper around normal Glinter output is rejected as the primary solution. It would need to parse Gleam syntax or Glinter text output, and it would not naturally support Glinter's rule configuration, severities, source spans, JSON/text output modes, or `// nolint:` handling.

Upstreaming a more configurable Glinter rule is a good long-term possibility but not the right first step. This repository can already use `glinter.run(extra_rules: ...)`, so waiting for upstream changes would leave Scherzo's style policy unenforced.

A repository-local guardrail outside Glinter, such as a bespoke script scanning for `Bool`, is a fallback only if the custom runner proves impossible. The verified Glinter API makes that fallback unnecessary for this work.

## Risks and Countermeasures

The main risk is style churn. The countermeasure is a narrow rule, one diagnostic per function, explicit exemptions, and a mandatory production-violation inventory checkpoint before any CI wiring. After the unit-tested runner exists, the implementer must run the custom lint command over `src/`, copy the exact diagnostics into the Production Violation Inventory section of this plan, and record a per-function disposition: add labels, add a narrow suppression with a reason, adjust the rule because the diagnostic is outside the intended policy, or stop and revise the rollout. If the inventory reports more than 10 production functions, or if applying labels would touch more than 15 distinct call-site files, do not continue to SelfCI wiring. Revise this plan with a smaller rollout, additional exemptions, or stakeholder-approved churn before proceeding.

Another risk is writing suppressions that look correct but do not suppress the custom rule. The only accepted suppression syntax is `// nolint: scherzo_public_function_labels -- reason`, with the `--` reason separator and a real reason after it. The suppression must be on its own leading line for the function, not as a trailing inline comment, and it must be validated by rerunning `direnv exec . gleam run -m scherzo_lint`. The runner-level smoke test in this plan proves that the custom rule's diagnostic span is suppressible before SelfCI is changed.

A third risk is accidentally adding Glinter or Glance as production dependencies. The countermeasure is to keep the runner and rule modules under `test/`, invoke them with `gleam run -m scherzo_lint`, and add `glance` only under `[dev_dependencies]`. A repository fact checked during planning is that `gleam run -m main_test` can resolve modules from `test/`; it failed only because `main_test` has no `main` function. This allows a dev-only runner without placing lint imports in `src/`.

A fourth risk is that the custom rule API lacks file paths inside module rules. The rule does not need file paths to decide whether a function declaration is ambiguous. Glinter's runner attaches the file path to each `LintResult`, and its normal annotation pass applies `// nolint:` filtering for module rules. The temporary-file smoke test exercises that runner behavior rather than relying only on unit tests.

A fifth risk is making test code noisy. The countermeasure is to keep `[tools.glinter] include = ["src/"]` for both the normal and custom production lint commands. Test code gets fixture coverage for the lint rule itself, but test source is not added to the production Glinter profile in this issue.

## Progress

- [x] (2026-05-07) Drafted this ExecPlan from the Linear issue and current repository facts.
- [x] (2026-05-07) Incorporated adversarial review feedback: direct `glance` dependency, production-violation inventory, runner-level smoke validation, exact `nolint` syntax, and a narrower callback exemption.
- [x] (2026-05-08) Added explicit direct development dependency on `glance` and verified `manifest.toml` records it as a direct requirement.
- [x] (2026-05-08) Implemented the dev-only custom Glinter runner and the `scherzo_public_function_labels` rule under `test/`.
- [x] (2026-05-08) Added unit tests for the active two-parameter public Bool policy and for deferred high-arity and duplicate-primitive cases.
- [x] (2026-05-08) Ran the first broad custom runner inventory, observed 247 production errors, and revised the rollout to the smaller two-parameter public Bool rule required by the churn threshold.
- [x] (2026-05-08) Ran the revised custom runner inventory, observed 5 production violations, and fixed them by adding labels and updating call sites.
- [x] (2026-05-08) Updated the production lint configuration, `docs/LINTING.md`, `README.md`, `AGENTS.md`, and the SelfCI validation path.
- [x] (2026-05-08) Ran final formatting, unit tests, normal Glinter, custom Glinter, runner smoke validation, and SelfCI validation.

## Surprises & Discoveries

- Observation: Glinter 2.16.0 already exposes `glinter.run(extra_rules: List(rule.Rule))`, and its module rule builder supports function visitors suitable for a project-specific label rule.
  Evidence: `build/packages/glinter/src/glinter.gleam` documents `run(extra_rules:)`; `build/packages/glinter/src/glinter/rule.gleam` exposes `rule.new`, `rule.with_simple_function_visitor`, `rule.with_default_severity`, and `rule.to_module_rule`.
- Observation: A runnable dev-only lint module can live under `test/` instead of `src/`.
  Evidence: `direnv exec . gleam run -m main_test` compiled the project and found `test/main_test.gleam`; it failed only because that existing test module has no public `main` function.
- Observation: The existing checked Glinter production gate allows warnings but fails on configured errors.
  Evidence: `direnv exec . gleam run -m glinter -- --stats` reported `Found 359 issues (0 errors, 359 warnings)` and exited successfully.
- Observation: The originally planned broader rule was too noisy for the current tree.
  Evidence: `direnv exec . gleam run -m scherzo_lint -- --format text` with high-arity and duplicate-primitive checks active reported `Found 611 issues (247 errors, 283 warnings)`, exceeding the 10-function churn threshold before any SelfCI wiring. Disabling only high-arity still left 134 custom errors, and a Bool-only check still left 20 custom errors.
- Observation: The revised two-parameter public Bool rollout is under the churn threshold and compiles cleanly after targeted label fixes.
  Evidence: `direnv exec . gleam run -m scherzo_lint -- --format text` after adding labels to five existing production functions reported `Found 364 issues (0 errors, 283 warnings)`.
- Observation: A long label at one call site can trip the source module line-count guardrail after formatting.
  Evidence: `direnv exec . gleam test` initially reported `src/scherzo/workflow_run.gleam grew beyond its line baseline: 2675 > 2674`; changing the `step_outcome` label from `on_failure_continue:` to the shorter `on_failure:` restored the baseline and the next test run passed.

## Decision Log

- Decision: Use a custom Glinter runner with `glinter.run(extra_rules: ...)` rather than a wrapper script or upstream-first work.
  Rationale: Glinter already exposes the needed extension point, preserves normal config and `// nolint:` behavior, and avoids building a second parser.
  Date: 2026-05-07.
- Decision: Place the runner and custom rule modules under `test/`.
  Rationale: Glinter and Glance are development tooling dependencies, and the custom lint implementation is development tooling. Keeping it under `test/` avoids importing lint packages from production `src/` modules while remaining runnable with `gleam run -m scherzo_lint`.
  Date: 2026-05-07.
- Decision: Add `glance` as an explicit direct development dependency before importing it from the custom rule.
  Rationale: The rule code will type-check against `glance` constructors and AST types directly. Relying on Glinter's transitive dependency would make the custom lint command fragile if Glinter changes its dependency graph.
  Date: 2026-05-07.
- Decision: Enforce one initial custom style rule, `scherzo_public_function_labels`, as an error for production source only.
  Rationale: This directly addresses the labelled-argument motivation while staying narrower than `label_possible` and leaving test-specific linting out of the production gate.
  Date: 2026-05-07.
- Decision: Exempt callback-shaped public helpers from the high-arity check only, not from duplicate primitive ambiguity.
  Rationale: A final function-typed callback can make three public parameters conventional, but two unlabelled primitive values such as `source: String` and `destination: String` remain ambiguous at call sites even when followed by a callback.
  Date: 2026-05-07.
- Decision: Require a production-violation inventory and churn threshold before SelfCI wiring.
  Rationale: The rule can force public signature and call-site edits. Capturing per-function dispositions before CI changes prevents a narrow style policy from becoming an unreviewed broad API refactor.
  Date: 2026-05-07.
- Decision: Standardize suppressions as `// nolint: scherzo_public_function_labels -- reason` and validate suppressibility with the real runner.
  Rationale: Glinter treats text after `--` as the reason. Text placed directly after the rule name can be parsed as part of the rule list and fail to suppress the intended rule.
  Date: 2026-05-07.
- Decision: Keep tests excluded from production Glinter and custom style profiles for this issue.
  Rationale: The ticket states that `assert_ok_pattern`, `unused_exports`, and `missing_type_annotation` are noisy in tests. A useful test-specific safety profile should be designed separately around targeted async/process/filesystem result handling, not bundled into this style-rule rollout.
  Date: 2026-05-07.
- Decision: Narrow the first blocking custom rule to exactly two-parameter public functions with an unlabelled primitive `Bool` parameter.
  Rationale: The first real inventory proved that the broader high-arity and duplicate-primitive design was not a low-churn rollout for the current tree: 247 production functions failed with the full policy, 134 still failed without high-arity, and 20 still failed with all unlabelled public Bool parameters. The two-parameter Bool subset produced 5 existing violations, which could be fixed with labels and a small call-site update set while preserving the motivating call-site clarity policy. High-arity and duplicate-primitive checks are deferred for a future inventory-backed rollout.
  Date: 2026-05-08.
- Decision: Fix the revised inventory with labels rather than suppressions.
  Rationale: The five remaining diagnostics were ordinary Scherzo-owned APIs where labels improved call-site clarity. No callback, comparator, or FFI exception was needed.
  Date: 2026-05-08.
- Decision: Use the shorter public call-site label `on_failure:` for `workflow_checkpoint.step_outcome` while keeping the internal parameter name `on_failure_continue`.
  Rationale: The more literal label `on_failure_continue:` made `src/scherzo/workflow_run.gleam` wrap one extra line and fail the checked source-size baseline. `on_failure:` still documents the Boolean decision at the call site and keeps the existing module line baseline intact.
  Date: 2026-05-08.

## Outcomes & Retrospective

As of 2026-05-08, the custom Glinter runner exists, the first blocking Scherzo-specific rule is implemented, and SelfCI is wired to run it as a separate production lint step. The completed rollout is narrower than the original design because the production inventory falsified the low-churn assumption for high-arity and duplicate-primitive public API checks. The shipped rule still improves call-site clarity for the highest-signal small API shape: public two-parameter functions with an unlabelled `Bool` parameter.

The main lesson is that repository-local style rules need a production inventory before CI wiring even when the rule sounds narrow in prose. The inventory checkpoint prevented a 247-function API churn rollout and turned the issue into a safe incremental gate. High-arity and duplicate-primitive checks remain useful candidates, but they need a separate plan that either starts as non-blocking inventory or defines narrower module/API boundaries before becoming PR-blocking.

## Context and Orientation

Scherzo is a Gleam/Erlang service. Production Gleam modules live under `src/`. Test and development-only Gleam modules live under `test/`. The repository uses `direnv` and `devenv`; commands in this plan should be run from the repository root, normally prefixed with `direnv exec .`.

The normal production Glinter gate is configured in `gleam.toml`. Glinter is listed under `[dev_dependencies]` as `glinter = ">= 2.16.0 and < 3.0.0"`. The current `[tools.glinter]` section has `include = ["src/"]`, so Glinter scans production source and excludes tests by default. The current `[tools.glinter.rules]` section sets `assert_ok_pattern`, `avoid_panic`, `avoid_todo`, and `division_by_zero` to `"error"`; sets `discarded_result`, `error_context_lost`, `thrown_away_error`, and `stringly_typed_error` to `"warning"`; and explicitly leaves style-heavy rules such as `label_possible`, `missing_labels`, `missing_type_annotation`, and `unused_exports` off.

The custom rule imports `glance` directly to inspect Gleam syntax trees. `manifest.toml` contains `glance` version `6.0.0`, and `gleam.toml` now declares `glance = ">= 6.0.0 and < 7.0.0"` as a direct development dependency so the custom lint code does not rely on Glinter's transitive dependency graph.

SelfCI is the repository-local final validation path. The script `.config/selfci/ci.sh` now runs these steps: `direnv allow`, `gleam format --check src test`, `gleam run -m glinter`, `gleam run -m scherzo_lint`, `rm -rf test/tmp`, `scherzo-test-unit`, and `nix flake check --print-build-logs`. The README describes SelfCI as the canonical final validation gate and says it runs the checked-in `.config/selfci/ci.sh`.

Glinter is both a command and a Gleam library. The package module `glinter` exposes `pub fn run(extra_rules extra_rules: List(rule.Rule)) -> Nil`. The `glinter/rule` module exposes a rule-building API. A module rule can visit `glance.Definition(glance.Function)` values. A `glance.Function` has `name`, `publicity`, `parameters`, `return`, and `body` fields. Each `glance.FunctionParameter` has `label`, `name`, and `type_` fields. Types such as `Bool`, `String`, `Int`, and `Float` appear as `glance.NamedType` values with no module qualifier.

## Preconditions and Verified Facts

The implementation assumes these facts, all verified against the current tree during planning:

- `gleam.toml` contains `glinter` as a dev dependency and configures `[tools.glinter] include = ["src/"]`.
- `gleam.toml` contains `glance = ">= 6.0.0 and < 7.0.0"` as a direct dev dependency, and `manifest.toml` contains the direct `glance` requirement.
- `gleam.toml` currently keeps `label_possible = "off"` and `missing_labels = "off"`.
- `manifest.toml` pins `glinter` at version `2.16.0` and currently contains transitive `glance` package version `6.0.0`.
- `build/packages/glinter/src/glinter.gleam` exposes `glinter.run(extra_rules:)` and says extra rules are configured like built-in rules in `gleam.toml` and through file-level ignores.
- `build/packages/glinter/src/glinter/rule.gleam` exposes module-rule builders and error constructors suitable for a function-declaration rule.
- `build/packages/glance/src/glance.gleam` exposes the AST fields needed to inspect function publicity, parameters, labels, and type annotations.
- `.config/selfci/ci.sh` contains the current SelfCI sequence and has separate `run_step "glinter" direnv exec . gleam run -m glinter` and `run_step "scherzo custom lint" direnv exec . gleam run -m scherzo_lint` production lint steps.
- `README.md` documents `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and the SelfCI command `direnv exec . selfci check --base main@origin --candidate @ --print-output`.
- `docs/plans/` has no existing `LIV-123-*` plan file other than this plan.

If any of these facts drift before implementation starts, normalize the plan before coding. For example, if adding the direct `glance` dependency resolves a version outside the 6.x API described here, or if Glinter moves or renames `run(extra_rules:)`, first update the plan's interface section with the new API and only then implement the rule.

## Scope Boundaries

In scope:

- Add `glance = ">= 6.0.0 and < 7.0.0"` as a direct dev dependency in `gleam.toml` and update `manifest.toml` so direct imports are intentional.
- Add a dev-only custom lint runner at `test/scherzo_lint.gleam`.
- Add one custom Glinter rule at `test/scherzo_lint/rules/public_function_labels.gleam`.
- Add focused unit tests for that rule under `test/scherzo_lint/public_function_labels_test.gleam`.
- Add the new rule name and severity to `gleam.toml` without enabling broad built-in label rules.
- Run the custom runner before CI wiring, update the Production Violation Inventory section with per-function dispositions, and stop for plan revision if the churn threshold is exceeded.
- Add runner-level smoke validation that creates a temporary production violation, observes the expected diagnostic, proves `// nolint: scherzo_public_function_labels -- reason` suppresses it, deletes the temporary file, and verifies the clean tree remains clean.
- Update documentation in `docs/LINTING.md`, `README.md`, and `AGENTS.md` so humans and agents know how the custom policy relates to the existing production Glinter gate, PR #60 / LIV-101, and the LIV-102 warning-ratchet work.
- Add the custom lint command to `.config/selfci/ci.sh` after the normal Glinter step.
- Fix or narrowly suppress existing production-source violations of the new custom rule so the custom lint command exits successfully.

Out of scope:

- Do not enable built-in `label_possible` or `missing_labels`.
- Do not ratchet existing warning rules such as `discarded_result`, `thrown_away_error`, or `stringly_typed_error` to errors; that belongs to LIV-102 or later subsystem work.
- Do not add a broad test-source Glinter profile in this issue.
- Do not change Scherzo runtime behavior.
- Do not introduce a separate package, code generator, or non-Gleam parser unless the verified Glinter custom-rule path breaks and the plan is revised.

## Production Violation Inventory

The first implementation of the originally planned broad policy was run against `src/` before any SelfCI wiring. It reported `Found 611 issues (247 errors, 283 warnings)`, where all 247 errors were `scherzo_public_function_labels` diagnostics. This exceeded the 10-function churn threshold, so implementation paused production signature edits and revised the rollout. Removing only the high-arity reason still left 134 custom errors; checking every unlabelled public `Bool` parameter still left 20 custom errors. The accepted revised rollout checks only exactly two-parameter public functions with an unlabelled primitive `Bool` parameter.

The revised inventory from `direnv exec . gleam run -m scherzo_lint -- --format text` on 2026-05-08 contained five production functions:

- Path/function: `src/scherzo/agent/pi_rpc.gleam` / `encode_set_auto_retry`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `enabled: Bool` in a two-parameter public wrapper.
  Disposition: add label `enabled enabled: Bool`.
  Call-site estimate: 0 direct external call-site files found by repository search; the wrapper body was updated to call the labelled protocol function.
  Rationale: this is a Scherzo-owned wrapper and the label preserves clear Boolean call sites without broad churn.
- Path/function: `src/scherzo/pi/protocol.gleam` / `encode_set_auto_retry`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `enabled: Bool` in a two-parameter public encoder.
  Disposition: add label `enabled enabled: Bool`.
  Call-site estimate: 3 files: `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/pi/client.gleam`, and `test/pi_protocol_test.gleam`.
  Rationale: this is the motivating public Bool shape and labelled calls read clearly as `enabled: ...`.
- Path/function: `src/scherzo/state/local_artifacts.gleam` / `reinitialize_state`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `yes: Bool`.
  Disposition: add label `yes yes: Bool`.
  Call-site estimate: 2 files: `src/scherzo/ctl.gleam` and `test/state_local_artifacts_test.gleam`.
  Rationale: the destructive confirmation flag benefits from an explicit `yes:` call-site label.
- Path/function: `src/scherzo/step_artifact.gleam` / `status_from_exit`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `timed_out: Bool`.
  Disposition: add label `timed_out timed_out: Bool`.
  Call-site estimate: 2 files: `src/scherzo/command_step.gleam` and the internal call in `src/scherzo/step_artifact.gleam`.
  Rationale: the status calculation is clearer when call sites distinguish the exit code from timeout state.
- Path/function: `src/scherzo/workflow_checkpoint.gleam` / `step_outcome`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `on_failure_continue: Bool`.
  Disposition: add label `on_failure on_failure_continue: Bool`.
  Call-site estimate: 1 file: `src/scherzo/workflow_run.gleam`.
  Rationale: the call-site label documents how failed steps are classified.

After those label fixes, `direnv exec . gleam run -m scherzo_lint -- --format text` reported `Found 364 issues (0 errors, 283 warnings)`. The warnings are the existing Glinter warning inventory, not custom-rule failures.

## Milestones

Milestone 1 proves the dependency and custom-rule extension point with tests before any documentation or CI wiring changes. At the end of this milestone, `gleam.toml` and `manifest.toml` intentionally record `glance` as a direct development dependency, `test/scherzo_lint/rules/public_function_labels.gleam` exists, and `test/scherzo_lint/public_function_labels_test.gleam` proves that the active two-parameter public Bool rule reports failures while private, labelled, discarded, unannotated, external, high-arity-only, and duplicate-primitive deferred cases pass. The observable proof is `direnv exec . gleam test` compiling and running the new tests.

Milestone 2 adds the runnable custom lint command and bounds the rollout blast radius. At the end of this milestone, `direnv exec . gleam run -m scherzo_lint` runs normal Glinter with the extra `scherzo_public_function_labels` rule over `src/`, and the Production Violation Inventory section records both the rejected broad inventory and the accepted revised five-function inventory.

Milestone 3 resolves accepted current violations and validates the runner behavior that unit tests cannot cover. At the end of this milestone, existing production violations have been labelled and compiler-reported call sites updated. The custom lint command exits 0 on the clean tree. A temporary file under `src/` is created, observed to fail with the expected custom diagnostic, suppressed with the exact supported `nolint` syntax, deleted, and followed by a clean rerun.

Milestone 4 documents and wires the policy. At the end of this milestone, contributors can find the policy in `docs/LINTING.md`, short command references in `README.md` and `AGENTS.md`, and SelfCI runs the custom lint step. The observable proof is that `.config/selfci/ci.sh` prints a separate `scherzo custom lint` step and the full SelfCI command succeeds.

## Plan of Work

First, make the direct parser dependency explicit. Add `glance = ">= 6.0.0 and < 7.0.0"` under `[dev_dependencies]` in `gleam.toml` and update `manifest.toml` so `[requirements]` contains a direct `glance` requirement. Do this before any new test module imports `glance`. If the resolved version is not a 6.x version compatible with the `build/packages/glance/src/glance.gleam` API described in this plan, stop and revise the plan rather than guessing at new AST shapes.

Second, create the rule module `test/scherzo_lint/rules/public_function_labels.gleam`. Define `pub fn rule() -> glinter/rule.Rule`. Build the rule with `rule.new(name: "scherzo_public_function_labels")`, `rule.with_default_severity(rule.Error)`, `rule.with_simple_function_visitor(visitor: check_function)`, and `rule.to_module_rule()`. The `check_function` helper receives `glance.Definition(glance.Function)` and the function span. It returns an empty list for private functions, `@external` functions, functions whose arity is not exactly two, and functions without an unlabelled named parameter annotated as unqualified primitive `Bool`. Otherwise it returns one `rule.error` at the function span.

In that rule module, keep the policy helpers small. `is_external_attribute` checks whether any attribute name is `"external"`. `is_public` checks `function.publicity == glance.Public`. `ambiguous_bool_parameter_names` returns only named parameters with `label == None` and `type_ == Some(glance.NamedType(name: "Bool", module: None, ..))`. Discarded parameters, labelled parameters, unannotated parameters, and module-qualified/custom Bool-like types are ignored. The error message should be stable and include the function name, for example `Public function 'configure' should use labels for ambiguous parameters`. The details should list the Bool parameter names and should say to add labels using the `label name: Type` form or add a preceding `// nolint: scherzo_public_function_labels -- reason` only for a genuine exception.

Third, add tests in `test/scherzo_lint/public_function_labels_test.gleam`. These are normal Gleeunit tests. They parse source snippets with `glance.module`, run the rule with `glinter/rule.run_on_module`, and inspect `glinter/rule.error_message` and `glinter/rule.error_details`. The tests cover a failing two-parameter public Bool fixture, a labelled equivalent, a private equivalent, an `@external` equivalent, discarded and unannotated parameters, and deferred shapes for three-parameter Bool, duplicate String parameters, and high-arity non-Bool parameters.

Fourth, add `test/scherzo_lint.gleam` with a `pub fn main() { glinter.run(extra_rules: [public_function_labels.rule()]) }`. Import the rule module from `test/scherzo_lint/rules/public_function_labels`. Add `scherzo_public_function_labels = "error"` to `[tools.glinter.rules]` in `gleam.toml` while leaving `label_possible = "off"` and `missing_labels = "off"`. Run `direnv exec . gleam run -m glinter` once after this config edit; normal Glinter should exit 0 while continuing to print the existing warning inventory.

Fifth, run `direnv exec . gleam run -m scherzo_lint -- --format text` from the repository root and use its output as the production-violation inventory. The broad pre-revision inventory exceeded the churn threshold and is recorded in this plan. For the active revised inventory, fix accepted style violations by adding labels to function declarations and updating compiler-reported or search-identified call sites. For a true exception, put the suppression on its own leading line for the function using exactly this syntax:

    // nolint: scherzo_public_function_labels -- conventional callback shape required by API parity
    pub fn example(...) {
      ...
    }

The `--` separator and reason text are required. Do not write `// nolint: scherzo_public_function_labels reason`, because Glinter can parse the extra text as part of the rule list. Do not use trailing inline `nolint` comments, and do not add blanket file-level ignores for this rule. For functions with doc comments or attributes, put the suppression in the leading annotation block for that function and rerun the custom lint command immediately to prove it is associated with the intended diagnostic.

Sixth, after the clean tree has no `scherzo_public_function_labels` diagnostics, run the temporary-file runner smoke test. Create `src/tmp_scherzo_lint_smoke.gleam` with the violating two-parameter Bool fixture from the Concrete Steps. Run the custom lint command and expect a non-zero exit with both the rule name and `tmp_scherzo_lint_smoke` in the output. Replace the file with the exact `// nolint: scherzo_public_function_labels -- smoke-test suppression, delete before commit` form, rerun the custom lint command, and expect it to exit 0 with no diagnostic for the temporary file. Delete `src/tmp_scherzo_lint_smoke.gleam`, verify `jj diff --name-only --color=never` no longer lists it, and rerun the clean custom lint command.

Seventh, document the policy. Create `docs/LINTING.md` with a short explanation of the two production lint commands. The document says that `direnv exec . gleam run -m glinter` is the PR #60 / LIV-101 production safety gate configured by `gleam.toml`, that LIV-102 owns ratcheting existing warning rules to errors, and that `direnv exec . gleam run -m scherzo_lint` is Scherzo's repository-specific style gate. It lists the enforced custom rule, currently warning-only built-in rules, intentionally off broad rules, the exact suppression syntax, and the test-source policy. Update `README.md` to mention the custom lint command in the SelfCI paragraph and the quick validation area. Update `AGENTS.md` so coding agents know to run the custom lint command along with the normal Glinter gate when touching production APIs.

Eighth, update `.config/selfci/ci.sh` by adding a new step immediately after the existing Glinter step:

    run_step "scherzo custom lint" direnv exec . gleam run -m scherzo_lint

Keep the existing normal Glinter step. The normal step continues to enforce the broad production safety policy; the new step enforces Scherzo-specific style rules that are not built into Glinter.

## Concrete Steps

1. From the repository root, ensure the development environment is allowed:

       direnv allow .

   If the environment was already allowed, this is a no-op.

2. In `gleam.toml`, add this direct development dependency under `[dev_dependencies]`:

       glance = ">= 6.0.0 and < 7.0.0"

3. From the repository root, update dependency metadata:

       direnv exec . gleam deps download

   Expect the command to exit 0. Then inspect `manifest.toml` and verify `[requirements]` contains a direct `glance` requirement.

4. Create `test/scherzo_lint/rules/public_function_labels.gleam` and implement `pub fn rule() -> rule.Rule` using Glinter's simple function visitor API. The rule reports exactly one error for a public non-`@external` function with exactly two parameters when any unlabelled named parameter is annotated as unqualified primitive `Bool`.

5. Create `test/scherzo_lint/public_function_labels_test.gleam` with the `errors_for` helper and fixtures for the active two-parameter Bool policy, private and external exemptions, labelled passing cases, discarded and unannotated ignored parameters, and deferred high-arity and duplicate-primitive shapes. Run:

       direnv exec . gleam test

   Expect the new rule tests to pass. If unrelated flaky tests fail, rerun targeted validation after verifying the lint rule tests compiled and ran.

6. Create `test/scherzo_lint.gleam` with `pub fn main()` that calls `glinter.run(extra_rules: [public_function_labels.rule()])`.

7. Add `scherzo_public_function_labels = "error"` to `[tools.glinter.rules]` in `gleam.toml`. Do not change the existing settings for `label_possible` or `missing_labels`.

8. Run:

       direnv exec . gleam run -m glinter

   Expect normal Glinter to exit 0 while possibly still printing existing warnings.

9. Run the custom runner to inventory existing production diagnostics:

       direnv exec . gleam run -m scherzo_lint -- --format text

   Record the inventory in this plan before SelfCI wiring. If the active rule reports more than 10 production functions or would touch more than 15 call-site files, stop and revise this plan again.

10. Fix accepted existing production violations by adding labels to function declarations and updating call sites. The implementation fixed these signatures:

       src/scherzo/agent/pi_rpc.gleam: encode_set_auto_retry(id: String, enabled enabled: Bool)
       src/scherzo/pi/protocol.gleam: encode_set_auto_retry(id: String, enabled enabled: Bool)
       src/scherzo/state/local_artifacts.gleam: reinitialize_state(workspace_root: String, yes yes: Bool)
       src/scherzo/step_artifact.gleam: status_from_exit(exit_code: Int, timed_out timed_out: Bool)
       src/scherzo/workflow_checkpoint.gleam: step_outcome(artifact: step_artifact.StepArtifact, on_failure on_failure_continue: Bool)

11. Run:

       direnv exec . gleam format --check src test
       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect formatting to pass and the custom lint command to exit 0 with no `scherzo_public_function_labels` errors.

12. Run the temporary-violation smoke test. Create `src/tmp_scherzo_lint_smoke.gleam` with:

       pub fn tmp_scherzo_lint_smoke(path: String, enabled: Bool) -> Nil {
         Nil
       }

13. Run:

       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect a non-zero exit. The output must include `scherzo_public_function_labels` and `tmp_scherzo_lint_smoke`.

14. Replace `src/tmp_scherzo_lint_smoke.gleam` with the suppressed version:

       // nolint: scherzo_public_function_labels -- smoke-test suppression, delete before commit
       pub fn tmp_scherzo_lint_smoke(path: String, enabled: Bool) -> Nil {
         Nil
       }

15. Run:

       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect exit 0 and no diagnostic for `tmp_scherzo_lint_smoke`.

16. Delete the temporary smoke file:

       rm src/tmp_scherzo_lint_smoke.gleam
       jj diff --name-only --color=never

   Expect the diff output not to list `src/tmp_scherzo_lint_smoke.gleam`.

17. Create `docs/LINTING.md` and update `README.md` and `AGENTS.md` with the policy, exact suppression syntax, and commands described above.

18. Update `.config/selfci/ci.sh` with the `scherzo custom lint` step immediately after `run_step "glinter" direnv exec . gleam run -m glinter`.

19. Run final validation:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint
       direnv exec . selfci check --base main@origin --candidate @ --print-output

   Expect all commands to exit 0. The normal Glinter command may continue to print warnings that are owned by LIV-102 or later work.

## Testing and Falsifiability

The custom rule must be tested before it is wired into SelfCI. The tests in `test/scherzo_lint/public_function_labels_test.gleam` are the falsifiability boundary for the active policy. If a public two-parameter function with an unlabelled `Bool` parameter does not produce one error, the rule is too weak. If a labelled equivalent, private helper, `@external` function, discarded parameter, unannotated parameter, high-arity-only function, or duplicate-primitive-only function produces an error, the first rollout is broader than the accepted revised policy and must be fixed before continuing.

A representative failing fixture is:

    pub fn configure(path: String, enabled: Bool) -> Nil {
      Nil
    }

The test asserts that `errors_for(source)` has length 1, that `rule.error_message` includes `configure`, and that `rule.error_details` includes both `enabled` and `Bool`.

A representative passing labelled fixture is:

    pub fn configure(path: String, enabled enabled: Bool) -> Nil {
      Nil
    }

The test asserts that `errors_for(source)` returns an empty list.

A representative deferred high-arity fixture is:

    pub fn launch(command: String, cwd: String, auto_retry: Bool) -> Nil {
      Nil
    }

The test asserts no errors because the first rollout deliberately avoids blocking every public Bool in higher-arity APIs. A representative deferred duplicate-primitive fixture is:

    pub fn copy(source: String, destination: String) -> Nil {
      Nil
    }

The test asserts no errors because duplicate primitive checks exceeded the churn budget and are deferred.

The unit tests deliberately bypass Glinter runner configuration and annotation filtering so they can exercise the rule visitor quickly. Runner-level behavior is tested separately. After `test/scherzo_lint.gleam` and the `gleam.toml` rule entry exist, the temporary-file smoke test must prove all of these claims: the runner scans `src/`, the custom rule name is configured as an error, the expected diagnostic text appears for an unsuppressed production violation, the exact `// nolint: scherzo_public_function_labels -- reason` syntax suppresses the diagnostic, deleting the temporary file cleans the tree, and the clean custom lint command exits 0.

The production-violation inventory is also a falsifiability checkpoint. The original broad inventory exceeded the threshold and forced this plan revision. If this narrower active rule grows to more than 10 production functions or accepted label fixes would touch more than 15 call-site files in a future tree, the claim that this is a low-churn first rollout is false again and the plan should be revised before CI wiring.

## Validation and Acceptance

Acceptance is met when all of the following are true:

- `gleam.toml` and `manifest.toml` record `glance` as a direct development dependency compatible with the 6.x API used by the custom rule.
- `test/scherzo_lint.gleam` runs `glinter.run(extra_rules: [...])` with `scherzo_public_function_labels` included.
- `test/scherzo_lint/rules/public_function_labels.gleam` implements the active two-parameter public Bool label policy described in this revised plan.
- `test/scherzo_lint/public_function_labels_test.gleam` contains passing and failing fixtures for the active two-parameter public Bool rule, private functions, external functions, labelled parameters, discarded parameters, unannotated parameters, and deferred high-arity and duplicate-primitive shapes.
- The Production Violation Inventory section records the broad-policy threshold failure, the revised five-function inventory, and the disposition for each current diagnostic.
- Existing diagnostics have been fixed with labels and updated call sites rather than broad suppressions.
- The temporary-file smoke test has proven an unsuppressed `src/tmp_scherzo_lint_smoke.gleam` violation fails, the exact `// nolint: scherzo_public_function_labels -- reason` syntax suppresses it, and deleting the file restores a clean custom lint run.
- `gleam.toml` declares `scherzo_public_function_labels = "error"` while keeping `label_possible = "off"` and `missing_labels = "off"`.
- `docs/LINTING.md`, `README.md`, and `AGENTS.md` explain the normal Glinter gate, the custom Scherzo lint gate, the relationship to PR #60 / LIV-101 and LIV-102, the intentionally off broad rules, the exact suppression syntax, and the test-source policy.
- `.config/selfci/ci.sh` runs a separate `scherzo custom lint` step.
- From the repository root, these commands exit 0:

      direnv exec . gleam format --check src test
      direnv exec . gleam test
      direnv exec . gleam run -m glinter
      direnv exec . gleam run -m scherzo_lint
      direnv exec . selfci check --base main@origin --candidate @ --print-output

The normal Glinter command may still print existing warnings. That does not fail acceptance for this issue because ratcheting those warnings is LIV-102's scope.

## Rollout, Recovery, and Idempotence

This is a development-time gate, so rollout is additive and reversible. The normal Glinter gate remains in place while the custom gate is added as a separate command and a separate SelfCI step. SelfCI must not be edited until the custom runner exists, the Production Violation Inventory has been filled from real output, the active revised inventory is under the churn threshold, existing accepted diagnostics have been resolved, and the temporary-file smoke test has proven diagnostic and suppression behavior.

If the custom rule is wrong after landing, recovery is to remove or temporarily disable only the `scherzo custom lint` SelfCI step and set `scherzo_public_function_labels = "off"` in `gleam.toml` while preserving the normal production safety gate. If the direct `glance` dependency proves incompatible with the Glinter API in this repository, recovery is to remove the new custom lint modules and dependency entry before any SelfCI wiring is added.

Running the commands in this plan is idempotent except for the temporary smoke file, which has an explicit cleanup step. `direnv allow .` may be repeated. The lint and test commands do not modify source files. The SelfCI script removes `test/tmp` as it already does today. Signature fixes for labels should be made once and then protected by the compiler and custom lint command.

If implementation stops halfway, keep the tree safe by stopping at a commit point where `direnv exec . gleam test` and `direnv exec . gleam run -m glinter` pass. Do not leave `.config/selfci/ci.sh` calling `scherzo_lint` until the runner exists, the temporary smoke file has been deleted, and the custom command exits 0 on the clean tree.

## Artifacts and Notes

Planning inspected the current normal Glinter gate and observed this summary from `direnv exec . gleam run -m glinter -- --stats`:

    Found 359 issues (0 errors, 359 warnings)
    Linted 100 files (49,450 lines) in 1418ms

This means the existing production Glinter gate is already a warning-tolerant gate for known warning categories. The new custom style rule should be introduced as a separate error-producing style gate, not by changing the existing warning categories.

Planning also verified that a module under `test/` can be addressed by `gleam run -m`:

    direnv exec . gleam run -m main_test
    error: Module does not have a main function
    `main_test` does not have a main function so the module can not be run.

That error is useful evidence: Gleam found the test module and only rejected it because the module lacks `pub fn main()`. The new `test/scherzo_lint.gleam` module has `pub fn main()`.

Implementation validation observed these final summaries:

    direnv exec . gleam test
    869 passed, no failures

    direnv exec . gleam run -m scherzo_lint -- --format text
    Found 364 issues (0 errors, 283 warnings)

    direnv exec . selfci check --base main@origin --candidate @ --print-output
    all checks passed!

## Interfaces and Dependencies

The custom runner module must have this shape in `test/scherzo_lint.gleam`:

    import glinter
    import scherzo_lint/rules/public_function_labels

    pub fn main() {
      glinter.run(extra_rules: [
        public_function_labels.rule(),
      ])
    }

The custom rule module must expose this function in `test/scherzo_lint/rules/public_function_labels.gleam`:

    pub fn rule() -> rule.Rule

It must import `glance`, `gleam/list`, `gleam/option`, `gleam/string`, and `glinter/rule`. It must use Glinter's module rule API, not a bespoke file scanner.

The `gleam.toml` development dependencies must include both Glinter and Glance:

    [dev_dependencies]
    gleeunit = ">= 1.0.0 and < 2.0.0"
    glinter = ">= 2.16.0 and < 3.0.0"
    glance = ">= 6.0.0 and < 7.0.0"

The `gleam.toml` rule configuration must include this entry under `[tools.glinter.rules]`:

    scherzo_public_function_labels = "error"

The exact local suppression syntax is:

    // nolint: scherzo_public_function_labels -- function-specific reason
    pub fn example(...) {
      ...
    }

The `--` separator is required. The comment must be a leading comment for the function, not a trailing inline comment, and every suppression must be validated with `direnv exec . gleam run -m scherzo_lint`.

The SelfCI script `.config/selfci/ci.sh` must keep the existing normal Glinter step and add this immediately after it:

    run_step "scherzo custom lint" direnv exec . gleam run -m scherzo_lint

No new external services, environment variables, Nix packages, or non-Gleam dependencies are required. The implementation relies on the existing dev dependencies `glinter` and `gleeunit`, plus the newly explicit dev dependency `glance`.

## Open Questions and Clarifications Needed

None.
