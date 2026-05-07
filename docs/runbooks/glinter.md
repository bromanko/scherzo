# Glinter warning baseline and ratchet policy

This note records the LIV-101 rollout baseline for `glinter` and how agents should treat the warning set. The checked gate remains:

```sh
direnv exec . gleam run -m glinter
```

## Checked policy

- The checked glinter run is a production-source gate: `gleam.toml` includes `src/` and explicitly excludes `test/`.
- Tests are excluded intentionally. Test code uses `let assert`, fixture helpers, and deliberately crashing paths that are useful for concise deterministic tests but would swamp the production policy.
- `direnv exec . gleam run -m glinter` must continue to exit successfully. Errors are blocking. Warnings are tracked debt and should not be converted to `warnings_as_errors = true` until the baseline below is reduced.

## Baseline commands

The production baseline was captured with:

```sh
direnv exec . gleam run -m glinter -- --format json --stats > tmp/glinter-current-final.json
```

A broader exploratory baseline was captured with a temporary project config that enabled every built-in glinter rule for measurement and pointed at this repository's `src/` tree. The same temporary config was also run with both `src/` and `test/` included to evaluate test policy.

## Production warning baseline

After the easy fixes listed below, the checked `src/` gate scans 100 files and reports 351 warnings with no errors:

| Rule | Count | Classification |
| --- | ---: | --- |
| `thrown_away_error` | 170 | Keep as warning only; many are parser/decoder fallback chains that need case-by-case review. |
| `discarded_result` | 121 | Keep as warning only; prioritize process sends, filesystem cleanup, and command boundary handling when touching related code. |
| `stringly_typed_error` | 60 | Keep as warning only; mostly FFI and boundary modules that should move to typed error variants gradually. |
| `error_context_lost` | 0 | Fixed now; first candidate to promote to an error in a follow-up. |
| `missing_type_annotation` | 0 | Fixed now for `src/` and enabled as a warning ratchet; candidate to promote after one clean cycle. |

No `// nolint:` suppressions were added for this rollout.

## Triage summary

- **Fix now:** 8 `error_context_lost` findings were converted from `result.map_error(fn(_) { ... })` to explicit `result.replace_error(...)`; 4 `missing_type_annotation` findings in `src/` were fixed with return or parameter annotations.
- **Suppress:** none. Future suppressions must be narrow `// nolint:` comments with reasons, as described below.
- **Ignore/off:** style or structural rules with high baseline counts, including `label_possible`, `prefer_guard_clause`, `unused_exports`, `deep_nesting`, `unwrap_used`, and complexity rules.
- **Keep as warning only:** `discarded_result`, `thrown_away_error`, and `stringly_typed_error`.
- **Promote in follow-up:** `error_context_lost` first, then `missing_type_annotation` for `src/` after a clean cycle.

## Broad `src/` exploratory baseline

With all built-in rules enabled for measurement, `src/` produced 3,734 findings. The noisy or structural rules remain disabled in the checked config rather than being made blocking churn:

| Rule | Count | Triage |
| --- | ---: | --- |
| `label_possible` | 2,904 | Ignore/off for now; too style-oriented for a safety rollout. |
| `thrown_away_error` | 170 | Keep warning; review opportunistically. |
| `unused_exports` | 153 | Ignore/off for now; public API and tests need a separate export policy. |
| `prefer_guard_clause` | 136 | Ignore/off for now; style preference. |
| `discarded_result` | 121 | Keep warning; safety-relevant but too broad to block yet. |
| `unwrap_used` | 85 | Ignore/off for now; safety-relevant but needs manual triage of defaults and path fallbacks. |
| `deep_nesting` | 75 | Ignore/off for now; mostly orchestration/state-machine structure. |
| `stringly_typed_error` | 60 | Keep warning; gradual typed-error migration. |
| `function_complexity` | 14 | Ignore/off; defer to planned module decomposition. |
| `redundant_case` | 9 | Ignore/off; some single-variant domain types are intentionally future-proofed. |
| `module_complexity` | 7 | Ignore/off; defer to planned module decomposition. |
| `error_context_lost` | 0 | Fixed now. |
| `missing_type_annotation` | 0 | Fixed now for `src/`. |

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
- `// nolint:` is acceptable only when a warning is a false positive or when preserving the code is safer than the lint-driven rewrite. It must be narrow, sit on its own line directly above the target expression/function, name only the specific rule(s), and include a reason after `--`, for example:

  ```gleam
  // nolint: discarded_result -- best-effort cleanup; the primary error is returned below
  cleanup_tmp_dir(path)
  ```

- Do not use trailing inline `// nolint:` comments; glinter treats them as fragile and does not suppress the underlying finding.

## Follow-up promotion candidates

1. Promote `error_context_lost` to an error after this zero baseline has baked for one cycle.
2. Promote `missing_type_annotation` for `src/` after one clean cycle; keep tests excluded or give them a test-specific policy first.
3. After reducing counts, split follow-up work for `discarded_result` and `thrown_away_error` by subsystem (`orchestrator/daemon`, `orchestrator/service`, `workflow_run`, and `control/server` are the largest clusters).
4. Audit `unwrap_used` separately before enabling it; many current uses provide explicit defaults and should not be churned without domain review.
