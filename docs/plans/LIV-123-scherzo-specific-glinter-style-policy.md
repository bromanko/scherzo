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

Use Glinter's documented custom rule path rather than a separate parser or a fragile text wrapper. Add an explicit development dependency on `glance` before the custom rule imports `glance`, because the rule code type-checks against `glance` AST values directly and should not rely on Glinter's transitive dependency graph. Add a dev-only runner module at `test/scherzo_lint.gleam` whose `main` function calls `glinter.run(extra_rules: [...])`. Put the custom rule implementation under `test/scherzo_lint/rules/public_function_labels.gleam`, also dev-only, so production modules under `src/` do not import Glinter or Glance. The runner still lints production source because `gleam.toml` already configures `[tools.glinter] include = ["src/"]`, and `glinter.run` uses that configuration when no explicit paths are passed.

The first Scherzo-specific rule is named `scherzo_public_function_labels`. It is an error. It inspects public production function definitions and reports one diagnostic per function when unlabelled parameters make the public API ambiguous. In version 1, ambiguity means any of these conditions are true:

- The function is public, has three or more parameters, has at least one unlabelled named parameter, and is not a callback-shaped helper.
- The function is public and has an unlabelled parameter whose annotation is the primitive type `Bool`.
- The function is public and has two or more unlabelled parameters with the same primitive type among `String`, `Int`, `Float`, or `Bool`, unless the function is a conventional two-argument comparator.

The rule intentionally ignores private functions, `@external` functions, discarded parameters, and unannotated parameters for primitive-type checks. A callback-shaped helper is exempt only from the high-arity check, because a trailing callback can make a three-argument public helper conventional without making two unlabelled `String`, `Int`, `Float`, or `Bool` parameters safe at call sites. Conventional two-argument comparators are exempt from the duplicate-primitive check. This is the smallest useful policy that catches the motivating style problem without adopting blanket `label_possible` behavior.

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
- [ ] Add explicit direct development dependency on `glance` and verify the manifest records it as a direct requirement.
- [ ] Implement the dev-only custom Glinter runner and custom labelled-argument rule.
- [ ] Add unit tests with passing and failing fixture snippets for the rule.
- [ ] Run the custom runner against production source, update the Production Violation Inventory section, and stop for plan revision if the churn threshold is exceeded.
- [ ] Fix or narrowly suppress accepted production violations, then prove temporary violation and suppression behavior through the runner-level smoke test.
- [ ] Update the production lint configuration, documentation, and SelfCI validation path.
- [ ] Run formatting, unit tests, normal Glinter, custom Glinter, and SelfCI validation.

## Surprises & Discoveries

- Observation: Glinter 2.16.0 already exposes `glinter.run(extra_rules: List(rule.Rule))`, and its module rule builder supports function visitors suitable for a project-specific label rule.
  Evidence: `build/packages/glinter/src/glinter.gleam` documents `run(extra_rules:)`; `build/packages/glinter/src/glinter/rule.gleam` exposes `rule.new`, `rule.with_simple_function_visitor`, `rule.with_default_severity`, and `rule.to_module_rule`.
- Observation: A runnable dev-only lint module can live under `test/` instead of `src/`.
  Evidence: `direnv exec . gleam run -m main_test` compiled the project and found `test/main_test.gleam`; it failed only because that existing test module has no public `main` function.
- Observation: The existing checked Glinter production gate allows warnings but fails on configured errors.
  Evidence: `direnv exec . gleam run -m glinter -- --stats` reported `Found 359 issues (0 errors, 359 warnings)` and exited successfully.

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

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam/Erlang service. Production Gleam modules live under `src/`. Test and development-only Gleam modules live under `test/`. The repository uses `direnv` and `devenv`; commands in this plan should be run from the repository root, normally prefixed with `direnv exec .`.

The normal production Glinter gate is configured in `gleam.toml`. Glinter is listed under `[dev_dependencies]` as `glinter = ">= 2.16.0 and < 3.0.0"`. The current `[tools.glinter]` section has `include = ["src/"]`, so Glinter scans production source and excludes tests by default. The current `[tools.glinter.rules]` section sets `assert_ok_pattern`, `avoid_panic`, `avoid_todo`, and `division_by_zero` to `"error"`; sets `discarded_result`, `error_context_lost`, `thrown_away_error`, and `stringly_typed_error` to `"warning"`; and explicitly leaves style-heavy rules such as `label_possible`, `missing_labels`, `missing_type_annotation`, and `unused_exports` off.

The custom rule will import `glance` directly to inspect Gleam syntax trees. `manifest.toml` currently contains `glance` version `6.0.0` because Glinter depends on it, but `gleam.toml` does not yet declare `glance` as a direct development dependency. This plan makes that dependency explicit by adding `glance = ">= 6.0.0 and < 7.0.0"` under `[dev_dependencies]` before adding custom rule code.

SelfCI is the repository-local final validation path. The script `.config/selfci/ci.sh` currently runs these steps: `direnv allow`, `gleam format --check src test`, `gleam run -m glinter`, `rm -rf test/tmp`, `scherzo-test-unit`, and `nix flake check --print-build-logs`. The README describes SelfCI as the canonical final validation gate and says it runs the checked-in `.config/selfci/ci.sh`.

Glinter is both a command and a Gleam library. The package module `glinter` exposes `pub fn run(extra_rules extra_rules: List(rule.Rule)) -> Nil`. The `glinter/rule` module exposes a rule-building API. A module rule can visit `glance.Definition(glance.Function)` values. A `glance.Function` has `name`, `publicity`, `parameters`, `return`, and `body` fields. Each `glance.FunctionParameter` has `label`, `name`, and `type_` fields. Types such as `Bool`, `String`, `Int`, and `Float` appear as `glance.NamedType` values with no module qualifier.

## Preconditions and Verified Facts

The implementation assumes these facts, all verified against the current tree during planning:

- `gleam.toml` contains `glinter` as a dev dependency and configures `[tools.glinter] include = ["src/"]`.
- `gleam.toml` does not yet contain `glance` as a direct dev dependency; this plan adds `glance = ">= 6.0.0 and < 7.0.0"` before any new code imports `glance`.
- `gleam.toml` currently keeps `label_possible = "off"` and `missing_labels = "off"`.
- `manifest.toml` pins `glinter` at version `2.16.0` and currently contains transitive `glance` package version `6.0.0`.
- `build/packages/glinter/src/glinter.gleam` exposes `glinter.run(extra_rules:)` and says extra rules are configured like built-in rules in `gleam.toml` and through file-level ignores.
- `build/packages/glinter/src/glinter/rule.gleam` exposes module-rule builders and error constructors suitable for a function-declaration rule.
- `build/packages/glance/src/glance.gleam` exposes the AST fields needed to inspect function publicity, parameters, labels, and type annotations.
- `.config/selfci/ci.sh` contains the current SelfCI sequence and has a single `run_step "glinter" direnv exec . gleam run -m glinter` production lint step.
- `README.md` documents `direnv exec . gleam test`, `direnv exec . gleam run -m glinter` through SelfCI, and the SelfCI command `direnv exec . selfci check --base main@origin --candidate @ --print-output`.
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

This section is intentionally empty before implementation because the custom rule does not exist yet. Milestone 2 must update this section before any documentation or SelfCI wiring happens.

When the runner first reports against `src/`, replace this paragraph with either `None observed by direnv exec . gleam run -m scherzo_lint -- --format text on YYYY-MM-DD.` or one bullet per diagnostic in this form:

- Path/function: `src/example.gleam` / `configure`
  Diagnostic: `scherzo_public_function_labels` because of unlabelled `enabled: Bool`.
  Disposition: add labels | suppress | adjust rule | pause and revise.
  Call-site estimate: N files, based on compiler errors or a repository search for `configure(`.
  Rationale: why this disposition preserves Scherzo's public API style without broad churn.

The churn threshold is part of the rollout safety story. If the first inventory contains more than 10 production functions, stop before editing signatures or SelfCI and revise this plan. If applying accepted label fixes would touch more than 15 distinct call-site files, stop before continuing and revise this plan. Suppressions do not count as call-site churn, but every suppression must use the exact syntax `// nolint: scherzo_public_function_labels -- reason` and must have a function-specific reason.

## Milestones

Milestone 1 proves the dependency and custom-rule extension point with tests before any production source, documentation, or CI wiring changes. At the end of this milestone, `gleam.toml` and `manifest.toml` intentionally record `glance` as a direct development dependency, `test/scherzo_lint/rules/public_function_labels.gleam` exists, and `test/scherzo_lint/public_function_labels_test.gleam` proves that failing examples are reported and intended exceptions pass. The observable proof is `direnv exec . gleam test` passing after the tests first fail without the rule implementation.

Milestone 2 adds the runnable custom lint command and bounds the rollout blast radius. At the end of this milestone, `direnv exec . gleam run -m scherzo_lint` runs normal Glinter with the extra `scherzo_public_function_labels` rule over `src/`, and the Production Violation Inventory section has been updated with every current diagnostic and a disposition. If the inventory exceeds the churn threshold, this milestone ends by revising the plan instead of continuing.

Milestone 3 resolves accepted current violations and validates the runner behavior that unit tests cannot cover. At the end of this milestone, existing production violations have either been labelled, narrowly suppressed with `// nolint: scherzo_public_function_labels -- reason`, or used to refine the rule. The custom lint command exits 0 on the clean tree. A temporary file under `src/` has been created, observed to fail with the expected custom diagnostic, suppressed with the exact supported `nolint` syntax, deleted, and followed by a clean rerun.

Milestone 4 documents and wires the policy. At the end of this milestone, contributors can find the policy in `docs/LINTING.md`, short command references in `README.md` and `AGENTS.md`, and SelfCI runs the custom lint step. The observable proof is that `.config/selfci/ci.sh` prints a separate `scherzo custom lint` step and the full SelfCI command succeeds.

## Plan of Work

First, make the direct parser dependency explicit. Add `glance = ">= 6.0.0 and < 7.0.0"` under `[dev_dependencies]` in `gleam.toml` and update `manifest.toml` so `[requirements]` contains a direct `glance` requirement. Do this before any new test module imports `glance`. If the resolved version is not a 6.x version compatible with the `build/packages/glance/src/glance.gleam` API described in this plan, stop and revise the plan rather than guessing at new AST shapes.

Second, create the rule module `test/scherzo_lint/rules/public_function_labels.gleam`. Define `pub fn rule() -> glinter/rule.Rule`. Build the rule with `rule.new(name: "scherzo_public_function_labels")`, `rule.with_default_severity(rule.Error)`, `rule.with_simple_function_visitor(visitor: check_function)`, and `rule.to_module_rule()`. The `check_function` helper receives `glance.Definition(glance.Function)` and the function span. It returns an empty list for private functions, `@external` functions, and functions with no unlabelled named parameters that the policy cares about. Otherwise it returns one `rule.error` at the function span.

In that rule module, add small helpers so the policy remains readable. `is_external_attribute` checks whether any attribute name is `"external"`. `is_public` checks `function.publicity == glance.Public`. `is_named_unlabelled_parameter` returns true only for `glance.Named(_)` parameters with `label == None`. `primitive_type_name` returns `Ok("Bool")`, `Ok("String")`, `Ok("Int")`, or `Ok("Float")` for unqualified `glance.NamedType` annotations and `Error(Nil)` otherwise. `is_callback_shaped` returns true when the final parameter's annotation is `glance.FunctionType` and the function has at most three parameters. `is_conventional_comparator` returns true when the function name is exactly `"compare"` or starts with `"compare_"` and it has exactly two parameters.

The rule computes three reasons. The high-arity reason applies when the function has three or more parameters, is not callback-shaped, and has any unlabelled named parameter. The boolean reason applies to every unlabelled named parameter annotated as `Bool`. The duplicate-primitive reason groups unlabelled named parameters by primitive type and applies when any primitive group has two or more parameters; it is skipped for conventional comparators, but it is not skipped for callback-shaped functions. The error message should be stable and include the function name, for example `Public function 'configure' should use labels for ambiguous parameters`. The details should list the reasons and the parameter names, and should say to add labels using the `label name: Type` form or add a preceding `// nolint: scherzo_public_function_labels -- reason` only for a genuine exception.

Third, add tests in `test/scherzo_lint/public_function_labels_test.gleam`. These are normal Gleeunit tests. They should parse source snippets with `glance.module`, run the rule with `glinter/rule.run_on_module`, and inspect `glinter/rule.error_message` and `glinter/rule.error_details`. Add a helper `errors_for(source: String) -> List(rule.RuleError)` in the test module so each fixture is compact.

The tests must include these cases:

- A public function with three unlabelled `String` parameters reports one error whose message includes the function name.
- The same shape with labels on all parameters reports no errors.
- A private function with three unlabelled parameters reports no errors.
- A public function with an unlabelled `Bool` parameter reports one error and details include the boolean parameter name.
- The same public boolean parameter labelled as `enabled enabled: Bool` reports no errors.
- A public function with two unlabelled `String` parameters reports one duplicate-primitive error.
- A public two-argument comparator named `compare` with two unlabelled `Int` parameters reports no errors.
- A public callback-shaped helper with three parameters where the final parameter is a function type, and the preceding unlabelled parameters are not duplicate primitive types, reports no high-arity error.
- A public callback-shaped helper such as `pub fn with_paths(source: String, destination: String, then: fn(String) -> a) -> a` reports one duplicate-primitive error for `source` and `destination`.
- An `@external` public function reports no errors.

Fourth, add `test/scherzo_lint.gleam` with a `pub fn main() { glinter.run(extra_rules: [public_function_labels.rule()]) }`. Import the rule module from `test/scherzo_lint/rules/public_function_labels`. Add `scherzo_public_function_labels = "error"` to `[tools.glinter.rules]` in `gleam.toml` while leaving `label_possible = "off"` and `missing_labels = "off"`. Run `direnv exec . gleam run -m glinter` once after this config edit; if normal Glinter rejects an unknown custom rule in the config, stop and revise the plan before proceeding, because the config assumption would be false.

Fifth, run `direnv exec . gleam run -m scherzo_lint -- --format text` from the repository root and use its output as the production-violation inventory. Update the Production Violation Inventory section in this plan before changing function signatures or adding suppressions. If it reports existing production violations, decide each one in the inventory first. Fix accepted style violations by adding labels to function declarations and updating compiler-reported call sites. For a true exception, put the suppression on its own leading line for the function using exactly this syntax:

    // nolint: scherzo_public_function_labels -- conventional callback shape required by API parity
    pub fn example(...) {
      ...
    }

The `--` separator and reason text are required. Do not write `// nolint: scherzo_public_function_labels reason`, because Glinter can parse the extra text as part of the rule list. Do not use trailing inline `nolint` comments, and do not add blanket file-level ignores for this rule. For functions with doc comments or attributes, put the suppression in the leading annotation block for that function and rerun the custom lint command immediately to prove it is associated with the intended diagnostic.

Sixth, after the clean tree has no `scherzo_public_function_labels` diagnostics, run the temporary-file runner smoke test. Create `src/tmp_scherzo_lint_smoke.gleam` with the violating fixture from the Concrete Steps. Run the custom lint command and expect a non-zero exit with both the rule name and `tmp_scherzo_lint_smoke` in the output. Replace the file with the exact `// nolint: scherzo_public_function_labels -- smoke-test suppression, delete before commit` form, rerun the custom lint command, and expect it to exit 0 with no diagnostic for the temporary file. Delete `src/tmp_scherzo_lint_smoke.gleam`, verify `jj diff --name-only --color=never` no longer lists it, and rerun the clean custom lint command.

Seventh, document the policy. Create `docs/LINTING.md` with a short explanation of the two production lint commands. The document should say that `direnv exec . gleam run -m glinter` is the PR #60 / LIV-101 production safety gate configured by `gleam.toml`, that LIV-102 owns ratcheting existing warning rules to errors, and that `direnv exec . gleam run -m scherzo_lint` is Scherzo's repository-specific style gate. It should list enforced custom rules, currently warning-only built-in rules, intentionally off broad rules, the exact suppression syntax, and the test-source policy. Update `README.md` to mention the custom lint command in the SelfCI paragraph and the quick validation area. Update `AGENTS.md` so coding agents know to run the custom lint command along with the normal Glinter gate when touching production APIs.

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

   Expect the command to exit 0. Then inspect `manifest.toml` and verify `[requirements]` contains a direct `glance` requirement. If `gleam deps download` does not update the direct requirement, run `direnv exec . gleam update` and verify again before continuing.

4. Create `test/scherzo_lint/rules/public_function_labels.gleam` and add the `scherzo_public_function_labels` rule skeleton with `pub fn rule() -> rule.Rule` and helper stubs. At this point the helpers may return permissive values so the file compiles.

5. Create `test/scherzo_lint/public_function_labels_test.gleam` with the `errors_for` helper and the ten fixture cases listed in the Plan of Work. Run:

       direnv exec . gleam test

   Expect at least the new failing-example tests to fail because the rule skeleton does not yet report errors.

6. Implement the high-arity, boolean, duplicate-primitive, external, private, callback-shaped, and comparator logic in `test/scherzo_lint/rules/public_function_labels.gleam`. Remember that callback-shaped functions are exempt from the high-arity reason only; duplicate primitive parameters still report unless the function is a conventional comparator.

7. Run:

       direnv exec . gleam test

   Expect the new rule tests to pass along with the existing unit suite.

8. Commit the dependency and tested rule implementation as one logical commit. A suitable commit message is `Add Scherzo public function label lint rule`.

9. Create `test/scherzo_lint.gleam` with `pub fn main()` that calls `glinter.run(extra_rules: [public_function_labels.rule()])`.

10. Add `scherzo_public_function_labels = "error"` to `[tools.glinter.rules]` in `gleam.toml`. Do not change the existing settings for `label_possible` or `missing_labels`.

11. Run:

       direnv exec . gleam run -m glinter

   Expect normal Glinter to exit 0 while possibly still printing existing warnings. If it fails because `scherzo_public_function_labels` is unknown to the normal runner, stop and revise this plan before continuing.

12. Run the custom runner to inventory existing production diagnostics:

       direnv exec . gleam run -m scherzo_lint -- --format text

   If the command exits 0 with no custom diagnostics, update the Production Violation Inventory section with `None observed` and continue. If it exits non-zero, copy every `scherzo_public_function_labels` diagnostic into the Production Violation Inventory section with the path, function name, reason, disposition, and call-site estimate before editing any production signatures.

13. Apply the churn threshold. If the inventory contains more than 10 production functions, stop and revise this plan instead of editing signatures. If accepted label fixes would require edits in more than 15 distinct call-site files, stop and revise this plan before continuing. Do not add SelfCI wiring while this checkpoint is unresolved.

14. Fix or suppress accepted existing production violations. For labels, update the function declaration and every compiler-reported call site. For true exceptions, put this exact form on its own leading line for the function, with a function-specific reason:

       // nolint: scherzo_public_function_labels -- conventional callback shape required by API parity
       pub fn example(...) {
         ...
       }

15. Run:

       direnv exec . gleam test
       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect the compiler and custom lint command to exit 0. If a suppression does not suppress, fix the syntax or placement before continuing.

16. Run the temporary-violation smoke test. Create `src/tmp_scherzo_lint_smoke.gleam` with:

       pub fn tmp_scherzo_lint_smoke(path: String, enabled: Bool) -> Nil {
         Nil
       }

17. Run:

       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect a non-zero exit. The output must include `scherzo_public_function_labels` and `tmp_scherzo_lint_smoke`.

18. Replace `src/tmp_scherzo_lint_smoke.gleam` with the suppressed version:

       // nolint: scherzo_public_function_labels -- smoke-test suppression, delete before commit
       pub fn tmp_scherzo_lint_smoke(path: String, enabled: Bool) -> Nil {
         Nil
       }

19. Run:

       direnv exec . gleam run -m scherzo_lint -- --format text

   Expect exit 0 and no diagnostic for `tmp_scherzo_lint_smoke`.

20. Delete the temporary smoke file:

       rm src/tmp_scherzo_lint_smoke.gleam
       jj diff --name-only --color=never

   Expect the diff output not to list `src/tmp_scherzo_lint_smoke.gleam`.

21. Run the formatting and lint checks:

       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect formatting to exit 0, normal Glinter to exit 0 while possibly still printing existing warnings, and custom lint to exit 0 with no `scherzo_public_function_labels` errors.

22. Commit the runner, config, inventory update, smoke validation evidence if recorded, and any targeted production-label fixes as one logical commit. A suitable commit message is `Wire Scherzo custom lint runner`.

23. Create `docs/LINTING.md` and update `README.md` and `AGENTS.md` with the policy, exact suppression syntax, and commands described above.

24. Update `.config/selfci/ci.sh` with the `scherzo custom lint` step immediately after `run_step "glinter" direnv exec . gleam run -m glinter`.

25. Run:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect all commands to exit 0. The normal Glinter command may continue to print warnings that are owned by LIV-102 or later work.

26. Run final SelfCI validation:

       direnv exec . selfci check --base main@origin --candidate @ --print-output

   Expect the output to include both the existing `glinter` step and the new `scherzo custom lint` step, and expect the full command to succeed.

27. Commit the documentation and SelfCI wiring as one logical commit. A suitable commit message is `Document and validate Scherzo lint policy`.

## Testing and Falsifiability

The custom rule must be tested before it is wired into SelfCI. The tests in `test/scherzo_lint/public_function_labels_test.gleam` are the falsifiability boundary for the policy. If a public three-parameter unlabelled function does not produce one error, the high-arity claim is false. If a labelled equivalent produces an error, the rule is too broad. If a private helper, an `@external` function, or a conventional two-argument comparator produces an error in the specified fixture, the exception logic is too broad and must be fixed before continuing. If a callback-shaped helper with non-duplicate primitive parameters reports a high-arity error, the callback exemption is too narrow. If a callback-shaped helper with two unlabelled `String` parameters passes, the duplicate-primitive rule is too weak for the chosen policy.

Use source strings as fixtures rather than adding temporary files under `src/`. A representative failing fixture is:

    pub fn configure(path: String, mode: String, enabled: Bool) -> Nil {
      Nil
    }

The test should assert that `errors_for(source)` has length 1, that `rule.error_message` includes `configure`, and that `rule.error_details` includes both the high-arity reason and the boolean parameter name `enabled`.

A representative passing labelled fixture is:

    pub fn configure(path path: String, mode mode: String, enabled enabled: Bool) -> Nil {
      Nil
    }

The test should assert that `errors_for(source)` returns an empty list.

A representative callback-shaped passing fixture is:

    pub fn fold(items: List(a), initial: b, with combine: fn(b, a) -> b) -> b {
      initial
    }

The test should assert no errors because the final function-typed parameter makes the helper callback-shaped for this rule's high-arity check, and the preceding unlabelled parameters are not duplicate primitive types.

A representative callback-shaped failing fixture is:

    pub fn with_paths(source: String, destination: String, then: fn(String) -> a) -> a {
      then(source)
    }

The test should assert one duplicate-primitive error mentioning `source` and `destination`, because the final callback does not make two unlabelled strings unambiguous.

The unit tests deliberately bypass Glinter runner configuration and annotation filtering so they can exercise the rule visitor quickly. Runner-level behavior is tested separately. After `test/scherzo_lint.gleam` and the `gleam.toml` rule entry exist, the temporary-file smoke test must prove all of these claims: the runner scans `src/`, the custom rule name is configured as an error, the expected diagnostic text appears for an unsuppressed production violation, the exact `// nolint: scherzo_public_function_labels -- reason` syntax suppresses the diagnostic, deleting the temporary file cleans the tree, and the clean custom lint command exits 0.

The production-violation inventory is also a falsifiability checkpoint. If the first real run reports more than 10 production functions or if accepted label fixes would touch more than 15 call-site files, the plan's claim that this is a narrow, low-churn policy is false for the current tree. In that case, stop and revise the rollout before wiring SelfCI.

The integration behavior is falsified by the custom lint command itself. After the runner exists and existing accepted diagnostics are resolved, `direnv exec . gleam run -m scherzo_lint` must fail when a production file contains an unlabelled public boolean parameter and must pass once that parameter is labelled or narrowly suppressed. Do not commit a temporary violation; use the unit fixtures for permanent failing examples and delete `src/tmp_scherzo_lint_smoke.gleam` before committing.

## Validation and Acceptance

Acceptance is met when all of the following are true:

- `gleam.toml` and `manifest.toml` record `glance` as a direct development dependency compatible with the 6.x API used by the custom rule.
- `test/scherzo_lint.gleam` runs `glinter.run(extra_rules: [...])` with `scherzo_public_function_labels` included.
- `test/scherzo_lint/rules/public_function_labels.gleam` implements the public function label policy described in this plan.
- `test/scherzo_lint/public_function_labels_test.gleam` contains passing and failing fixtures for high-arity public functions, public boolean parameters, duplicate same-primitive parameters, private helpers, callback-shaped high-arity helpers, callback-shaped duplicate-primitive helpers, comparators, and external functions.
- The Production Violation Inventory section has been updated from the real custom runner output, and any existing diagnostics have dispositions that stay under the churn threshold or the plan has been revised before continuing.
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

This is a development-time gate, so rollout is additive and reversible. The normal Glinter gate remains in place while the custom gate is added as a separate command and a separate SelfCI step. SelfCI must not be edited until the custom runner exists, the Production Violation Inventory has been filled from real output, the churn threshold has not been exceeded, existing accepted diagnostics have been resolved, and the temporary-file smoke test has proven diagnostic and suppression behavior.

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

That error is useful evidence: Gleam found the test module and only rejected it because the module lacks `pub fn main()`. The new `test/scherzo_lint.gleam` module will have `pub fn main()`.

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
