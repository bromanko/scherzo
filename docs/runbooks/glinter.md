# Glinter baseline, rule tiers, and ratchet policy

This note records the LIV-101 rollout baseline, the LIV-102 ratchet decision, the LIV-131 `unwrap_used` audit, and how agents should treat the warning set. The checked gate is:

```sh
direnv exec . gleam run -m glinter
```

## Checked policy

- The checked glinter run is a production-source gate: `gleam.toml` includes `src/` and explicitly excludes `test/`.
- Tests are excluded intentionally. Test code uses `let assert`, fixture helpers, and deliberately crashing paths that are useful for concise deterministic tests but would swamp the production policy.
- `direnv exec . gleam run -m glinter` must continue to exit successfully. Errors are blocking. Warnings are tracked debt and should not be converted wholesale to `warnings_as_errors = true`; instead, promote individual high-signal rules after a clean baseline.
- SelfCI already runs the same glinter command via `.config/selfci/ci.sh`, so rule promotions in `gleam.toml` automatically become PR-blocking validation failures.

## Current rule tiers

### Error

These rules are release-blocking in production `src/` code:

| Rule | Why it blocks |
| --- | --- |
| `assert_ok_pattern` | LIV-95 safety gate: production `let assert` can turn recoverable conditions into process crashes. |
| `avoid_panic` | LIV-95 safety gate: production code should return or log structured errors instead of crashing intentionally. |
| `avoid_todo` | LIV-95 safety gate: unfinished production paths must not ship. |
| `division_by_zero` | LIV-95 safety gate: deterministic runtime failure with no acceptable production use. |
| `error_context_lost` | LIV-102 ratchet: discarding the original error in `map_error(fn(_) { ... })` hides boundary context. Preserve or wrap the error when it matters; use `result.replace_error` only when intentionally replacing it with a stable public error. |
| `missing_type_annotation` | LIV-102 ratchet: explicit production function signatures make API drift, durable boundary changes, and agent edits easier to review. The `src/` baseline is clean, so new omissions can fail fast without churn. |

### Warning

These rules remain visible but non-blocking because the baseline is still too broad for a safe one-shot cleanup. Note: glinter 2.16.0 renders explicitly enabled rules at their built-in default severity, so `unwrap_used` findings currently display as `[off]` even though `gleam.toml` configures the rule as warning inventory.

| Rule | Current count | Why it stays warning |
| --- | ---: | --- |
| `thrown_away_error` | 178 | Many findings are parser/decoder fallback chains or boundary checks that need case-by-case review before blocking. |
| `discarded_result` | 119 | Safety-relevant, but current findings include process sends, cleanup, and command-boundary best-effort work that must be triaged by subsystem. |
| `unwrap_used` | 79 | LIV-131 audit: mostly intentional defaults, path canonicalization fallbacks, and domain-map defaults. Keep visible as warning-equivalent inventory, but do not block until the remaining patterns have narrower helpers or suppressions. |
| `stringly_typed_error` | 60 | Mostly FFI and boundary modules. Prefer typed errors for durable/domain APIs, but migrate gradually to avoid large interface churn. |

### Off

These rules are deliberately disabled until focused follow-up work can reduce noise or define a narrower Scherzo-specific policy:

- Message/style rules: `echo`, `panic_without_message`, `todo_without_message`, `string_inspect`, `short_variable_name`, `unnecessary_variable`, `redundant_case`, `prefer_guard_clause`, `unnecessary_string_concatenation`, `trailing_underscore`, `label_possible`, `missing_labels`, `unqualified_import`, `duplicate_import`.
- Structural/churn-heavy rules: `unused_exports`, `deep_nesting`, `function_complexity`, `module_complexity`.
- Safety-relevant but not yet triaged broadly enough: `ffi_usage`.

## Baseline commands

The current production baseline can be refreshed with:

```sh
direnv exec . gleam run -m glinter -- --format json --stats > tmp/glinter-current.json
```

A broader exploratory baseline was captured during LIV-101 with a temporary project config that enabled every built-in glinter rule for measurement and pointed at this repository's `src/` tree. The same temporary config was also run with both `src/` and `test/` included to evaluate test policy.

## Production warning baseline

After the LIV-131 `unwrap_used` inventory promotion, the checked `src/` gate scans 101 files and reports 436 findings with no errors: 357 warning-severity findings plus 79 `unwrap_used` findings that glinter currently prints as `[off]` despite the `warning` config override.

| Rule | Count | Classification |
| --- | ---: | --- |
| `thrown_away_error` | 178 | Keep as warning only; many are parser/decoder fallback chains that need case-by-case review. |
| `discarded_result` | 119 | Keep as warning only; prioritize process sends, filesystem cleanup, and command boundary handling when touching related code. |
| `unwrap_used` | 79 | Keep as warning-equivalent inventory only; the audit found mostly intentional defaults and domain fallbacks, with a few high-signal cases fixed before enabling the rule. |
| `stringly_typed_error` | 60 | Keep as warning only; mostly FFI and boundary modules that should move to typed error variants gradually. |
| `error_context_lost` | 0 | Promoted to error in LIV-102 after fixing the only current finding. |
| `missing_type_annotation` | 0 | Promoted to error in LIV-102 after a clean `src/` baseline. |

No `// nolint:` suppressions were added for the LIV-101, LIV-102, or LIV-131 rollouts.

## Triage summary

- **Promoted to error:** `error_context_lost` and `missing_type_annotation`.
- **Promoted to visible warning-equivalent inventory:** `unwrap_used` after the LIV-131 audit. It has useful signal when touching nearby code, but the remaining baseline is not appropriate for blocking PRs. In glinter 2.16.0, these findings still display as `[off]` because the rule's built-in default severity is `Off`.
- **Fixed for promotion:** one new `error_context_lost` finding in schedule parsing was changed from `result.map_error(fn(_) { ... })` to `result.replace_error(...)`, making the intentional context replacement explicit. LIV-131 also fixed high-signal `unwrap_used` cases in command/hook diagnostics, prompt-file path resolution, and workflow fingerprint representation before enabling the warning.
- **Suppress:** none. Future suppressions must be narrow `// nolint:` comments with reasons, as described below.
- **Keep as warning only:** `discarded_result`, `thrown_away_error`, and `stringly_typed_error`; keep `unwrap_used` as visible warning-equivalent inventory.
- **Keep off:** style, complexity, broad export, and FFI rules until a separate rollout proves acceptable signal.

## LIV-131 `unwrap_used` audit

The initial LIV-131 inventory found 88 production `unwrap_used` findings across 19 files. After fixing high-signal cases, the enabled baseline is 79 findings across 17 files. The main categories are:

| Category | Examples | Classification | Policy |
| --- | --- | --- | --- |
| Explicit optional defaults | Config defaults in `config.gleam`, command status reasons, terminal/render empty message defaults, command timeout defaults, source-workspace env defaults. | Acceptable explicit defaults. | Leave visible for ratcheting; do not rewrite mechanically. Prefer small domain helpers when already touching the parser or renderer. |
| Domain-map defaults | Missing attempt indexes default to `1`; missing command receipts are `CommandReceiptUnseen`; missing recovery states are `StepUnattempted`; missing counters allocate a fresh counter. | Domain defaults / invariants. | Keep warning-equivalent only until these maps gain named lookup helpers or narrow suppressions that document the invariant. |
| Path canonicalization fallbacks | `path.absolute`/`path.dirname` fallbacks in workspace path construction, recovery validation, handoff/result formatting, and config path resolution. | Mostly intentional fallback, but higher-risk around filesystem boundaries. | Do not promote to error until path helpers distinguish safe fallback from recoverable path-resolution failure. LIV-131 changed prompt-file loading to return `BundleError` instead of falling back silently. |
| Best-effort diagnostics/artifacts | Command/hook diagnostic reads and captured stdout artifact reads. | Best-effort fallback. | Use explicit fallback only when preserving the primary failure is more important than failing on diagnostics. LIV-131 changed command and hook diagnostics to report a diagnostics-read failure instead of replacing it with an empty string. |
| Infallible/future-proofed `Result` APIs | Workflow fingerprint calculation used a `Result` wrapper that always succeeded in the attempt context path. | Invariant needing better representation. | Prefer an infallible API where the operation cannot currently fail; LIV-131 changed the attempt-context helper to return `String`. |

Recommended future policy: keep `unwrap_used = "warning"` as visible ratchet inventory. It should not become an error until the path-boundary and domain-map default clusters are reduced or documented with narrow suppressions. If glinter starts applying severity overrides to default-off module rules, the same baseline should be treated as ordinary warnings, not errors.

## Historical LIV-101 broad `src/` exploratory baseline

With all built-in rules enabled for measurement, `src/` produced 3,734 findings. The noisy or structural rules remain disabled in the checked config rather than being made blocking churn:

| Rule | Count | Triage |
| --- | ---: | --- |
| `label_possible` | 2,904 | Ignore/off for now; too style-oriented for a safety rollout. |
| `thrown_away_error` | 170 | Keep warning; review opportunistically. |
| `unused_exports` | 153 | Ignore/off for now; public API and tests need a separate export policy. |
| `prefer_guard_clause` | 136 | Ignore/off for now; style preference. |
| `discarded_result` | 121 | Keep warning; safety-relevant but too broad to block yet. |
| `unwrap_used` | 85 | Audited in LIV-131 and promoted to warning after high-signal fixes; do not promote to error until the remaining defaults and path fallbacks are reduced or documented. |
| `deep_nesting` | 75 | Ignore/off for now; mostly orchestration/state-machine structure. |
| `stringly_typed_error` | 60 | Keep warning; gradual typed-error migration. |
| `function_complexity` | 14 | Ignore/off; defer to planned module decomposition. |
| `redundant_case` | 9 | Ignore/off; some single-variant domain types are intentionally future-proofed. |
| `module_complexity` | 7 | Ignore/off; defer to planned module decomposition. |
| `error_context_lost` | 0 | Fixed in LIV-101; later promoted by LIV-102. |
| `missing_type_annotation` | 0 | Fixed in LIV-101 for `src/`; later promoted by LIV-102. |

## Test-file evaluation

Including `test/` in the broad exploratory run produced 3,805 test findings, dominated by rules that conflict with current test style:

| Test rule | Count |
| --- | ---: |
| `assert_ok_pattern` | 1,632 |
| `missing_type_annotation` | 774 |
| `unused_exports` | 772 |
| `label_possible` | 330 |
| `unnecessary_string_concatenation` | 132 |
| `discarded_result` | 79 |
| `thrown_away_error` | 37 |
| `avoid_panic` | 15 |

Decision: keep `test/` excluded from the checked glinter gate. If tests are linted later, add a separate test-specific config that allows `let assert`, fixtures, and expected-crash helpers while still considering targeted safety rules such as `discarded_result` in async tests.

## Agent guidance

- Treat glinter errors as release-blocking.
- Treat warnings as a ratchet inventory: avoid adding new warnings, and fix nearby warnings when you are already editing the same function or the fix is clearly mechanical.
- Do not do large unrelated refactors solely to reduce warning counts.
- Prefer a real fix over a suppression. For intentionally ignored `Result` values, make the boundary explicit by logging, returning the error, or binding to a named value with a clear comment when appropriate.
- For `error_context_lost`, prefer preserving or wrapping the original error. Use `result.replace_error(...)` when the original error is intentionally hidden behind a stable public error message or type.
- `// nolint:` is acceptable only when a warning or error is a false positive or when preserving the code is safer than the lint-driven rewrite. It must be narrow, sit on its own line directly above the target expression/function, name only the specific rule(s), and include a reason after `--`, for example:

  ```gleam
  // nolint: discarded_result -- best-effort cleanup; the primary error is returned below
  cleanup_tmp_dir(path)
  ```

- Do not use trailing inline `// nolint:` comments; glinter treats them as fragile and does not suppress the underlying finding.

## Follow-up candidates

1. Reduce `discarded_result` and `thrown_away_error` by subsystem (`orchestrator/daemon`, `orchestrator/service`, `workflow_run`, and `control/server` are large clusters) before considering promotion.
2. Gradually migrate durable/domain `Result(_, String)` APIs to typed errors before promoting `stringly_typed_error`.
3. Reduce the remaining `unwrap_used` path-boundary and domain-map default clusters before considering error promotion; many current uses provide explicit defaults and should not be churned without domain review.
4. If tests are linted, create a separate test policy instead of applying the production gate directly to `test/`.
