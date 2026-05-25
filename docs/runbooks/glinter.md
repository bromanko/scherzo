# Glinter baseline, rule tiers, and ratchet policy

This note records the LIV-101 rollout baseline, the LIV-102 ratchet decision, the LIV-131 `unwrap_used` audit, the LIV-533 subsystem triage, the LIV-543 orchestrator daemon/transition hardening update, the LIV-544 state projection/recovery/artifact hardening update, the LIV-545 workspace boundary hardening, the LIV-546 agent/Pi hardening update, and how agents should treat the warning set. The checked production lint gates are:

```sh
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```

## Checked policy

- The checked `glinter`/`scherzo_lint` runs are production-source gates: `gleam.toml` includes `src/` and explicitly excludes `test/`.
- Tests are excluded intentionally from those checked commands. Test code uses `let assert`, fixture helpers, helper processes, and deliberately crashing paths that are useful for concise deterministic tests but conflict with several production-only rules.
- The repository now has a documented test-source policy below, but it is not wired into `gleam.toml` or SelfCI. Do not add `test/` to the production gate; a future rollout should use a separate test lint command or project config so test severities can differ from production severities.
- `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint` must continue to exit successfully. Errors are blocking. Warnings are tracked debt and should not be converted wholesale to `warnings_as_errors = true`; instead, promote individual high-signal rules after a clean baseline.
- SelfCI already runs both production lint commands via `.config/selfci/ci.sh`, so rule promotions in `gleam.toml` and custom Scherzo lint rules automatically become PR-blocking validation failures for production source.

## Current rule tiers

### Error

These rules are configured or intended to be release-blocking in production `src/` code:

| Rule | Why it blocks |
| --- | --- |
| `assert_ok_pattern` | LIV-95 safety gate: production `let assert` can turn recoverable conditions into process crashes. |
| `avoid_panic` | LIV-95 safety gate: production code should return or log structured errors instead of crashing intentionally. |
| `avoid_todo` | LIV-95 safety gate: unfinished production paths must not ship. |
| `division_by_zero` | LIV-95 safety gate: deterministic runtime failure with no acceptable production use. |
| `error_context_lost` | LIV-102 ratchet intent: discarding the original error in `map_error(fn(_) { ... })` hides boundary context. Preserve or wrap the error when it matters; use `result.replace_error` only when intentionally replacing it with a stable public error. The current refresh found seven findings reported as warning severity; treat that as high-signal baseline drift to fix before relying on the promotion as clean. |
| `missing_type_annotation` | LIV-102 ratchet: explicit production function signatures make API drift, durable boundary changes, and agent edits easier to review. The `src/` baseline is clean, so new omissions can fail fast without churn. |
| `scherzo_public_function_labels` | LIV-123 Scherzo-specific style gate: public two-parameter production functions with unlabelled `Bool` parameters must use labels or document a narrow suppression. |

### Warning

These rules remain visible but non-blocking because the baseline is still too broad for a safe one-shot cleanup. Note: glinter 2.16.0 renders explicitly enabled rules at their built-in default severity, so `unwrap_used` findings currently display as `[off]` even though `gleam.toml` configures the rule as warning inventory. The current refresh found seven `error_context_lost` findings reported as warnings despite the intended error-tier policy; they are counted here as urgent drift, not as a new acceptable warning class.

| Rule | Current count | Drift from prior documented baseline | Why it stays warning/inventory |
| --- | ---: | ---: | --- |
| `thrown_away_error` | 59 | -119 | Many findings are parser/decoder fallback chains or boundary checks that need case-by-case review before blocking. |
| `discarded_result` | 14 | -105 | Safety-relevant, but current findings include process sends, cleanup, and command-boundary best-effort work that must be triaged by subsystem. |
| `unwrap_used` | 60 | -19 | LIV-131 audit categories still apply, with remaining inventory concentrated in workflow, workspace manifest, tracker, config, and one residual state record default. Keep visible as warning-equivalent inventory until domain-map and path-boundary helpers or narrow suppressions exist. |
| `stringly_typed_error` | 66 | +6 | Mostly FFI, tracker/control, workflow contract, and residual state boundary modules. Prefer typed errors for durable/domain APIs, but migrate gradually to avoid large interface churn. |
| `error_context_lost` | 7 | +7 | High-signal drift in workflow recovery/output manifest handling and workspace manifest paths. Fix or explicitly replace errors before treating the LIV-102 promotion as clean again. |

### Off

These rules are deliberately disabled until focused follow-up work can reduce noise or define a narrower Scherzo-specific policy:

- Message/style rules: `echo`, `panic_without_message`, `todo_without_message`, `string_inspect`, `short_variable_name`, `unnecessary_variable`, `redundant_case`, `prefer_guard_clause`, `unnecessary_string_concatenation`, `trailing_underscore`, `label_possible`, `missing_labels`, `unqualified_import`, `duplicate_import`.
- Structural/churn-heavy rules: `unused_exports`, `deep_nesting`, `function_complexity`, `module_complexity`.
- Safety-relevant but not yet triaged broadly enough: `ffi_usage`.

## Baseline commands

The current production baseline can be refreshed with:

```sh
direnv exec . gleam run -m glinter -- --format json --stats > tmp/glinter-current.json
direnv exec . gleam run -m scherzo_lint -- --format json --stats > tmp/scherzo-lint-current.json
```

To regenerate the subsystem triage tables, keep the raw JSON output with the ticket or workflow artifacts and derive file/rule counts from `tmp/glinter-current.json` (or from the `scherzo_lint` JSON when custom-rule findings appear):

```sh
jq -r '.results[] | [.file, .rule] | @tsv' tmp/glinter-current.json \
  | sort \
  | uniq -c \
  > tmp/glinter-file-rule-counts.tsv

jq '.summary, .stats' tmp/glinter-current.json
```

Assign each finding to exactly one subsystem using this file-owner map, then verify subsystem row totals sum to `.summary.total` and rule columns match the top-level rule counts. The `Primary files` column below is a hotspot summary, not the exhaustive ownership source.

| Subsystem | File ownership used for counts |
| --- | --- |
| Orchestrator / daemon / transition / effect runner | `src/scherzo/orchestrator/*`, `src/scherzo/session/hub.gleam`, `src/scherzo/hooks.gleam`, `src/scherzo/instance_lock.gleam`, `src/scherzo/signal.gleam` |
| Workflow execution | `src/scherzo/command_step.gleam`, `src/scherzo/local_workflow_run.gleam`, `src/scherzo/template.gleam`, `src/scherzo/workflow_*.gleam` |
| State ledger / projection / recovery / artifacts | `src/scherzo/state/*`, `src/scherzo/step_artifact.gleam`, `src/scherzo/handoff_format.gleam` |
| Tracker / Linear / control boundaries | `src/scherzo/control/*`, `src/scherzo/ctl/*`, `src/scherzo/linear*.gleam`, `src/scherzo/port.gleam`, `src/scherzo/task.gleam`, `src/scherzo/tracker/*` |
| Config / parsing / operator CLI | `src/scherzo/config.gleam`, `src/scherzo/config/*`, `src/scherzo/doctor.gleam`, `src/scherzo/model_config.gleam`, `src/scherzo/review_lane_preflight_policy.gleam`, `src/scherzo/terminal/*`, `src/scherzo/version.gleam` |
| Workspace / workspace drivers | `src/scherzo/workspace*.gleam` |
| Agent / pi execution | `src/scherzo/agent/*`, `src/scherzo/pi/*` |

A broader exploratory baseline was captured during LIV-101 with a temporary project config that enabled every built-in glinter rule for measurement and pointed at this repository's `src/` tree. The same temporary config was also run with both `src/` and `test/` included to evaluate test policy.

## Production warning baseline

LIV-533 refreshed the production baseline on 2026-05-23 with both JSON/stat commands above, and LIV-543, LIV-544, LIV-545, and LIV-546 refreshed it again on 2026-05-25 after orchestrator daemon/transition, state projection/recovery/artifact, workspace/workspace-driver filesystem-boundary, and agent/Pi attempt-lifecycle hardening. `glinter` reports the current built-in inventory over 209 files / 118,062 lines: 206 findings, 0 errors, 146 warning-severity findings, and 60 `unwrap_used` findings rendered as `[off]`. `scherzo_lint` adds the custom `scherzo_public_function_labels` rule and currently has no additional findings.

| Rule | Prior documented count | Current count | Drift | Classification |
| --- | ---: | ---: | ---: | --- |
| `thrown_away_error` | 178 | 59 | -119 | Keep as warning only; review fallback chains and runtime-boundary catches by subsystem. |
| `discarded_result` | 119 | 14 | -105 | Keep as warning only; prioritize process sends, filesystem cleanup, hooks, and command/control boundaries. |
| `unwrap_used` | 79 | 60 | -19 | Keep as warning-equivalent inventory; remaining findings are concentrated in workflow, workspace manifest, tracker, config path/default code, and one residual state record default. |
| `stringly_typed_error` | 60 | 66 | +6 | Keep as warning only while durable/domain APIs migrate gradually to typed errors. |
| `error_context_lost` | 0 | 7 | +7 | High-signal drift; narrow fix or explicit replacement should happen before relying on the LIV-102 ratchet as clean. |
| `missing_type_annotation` | 0 | 0 | 0 | Clean error-tier baseline. |
| `scherzo_public_function_labels` | 0 | 0 | 0 | Clean custom Scherzo lint baseline. |

LIV-544 added four narrow `// nolint: stringly_typed_error` suppressions on leaf Erlang artifact-store FFI declarations; their public wrappers immediately normalize tagged strings into `ArtifactWriteError` or `ArtifactError`. No suppressions were added for the LIV-101, LIV-102, LIV-131, LIV-533, LIV-543, LIV-545, or LIV-546 rollouts.

## LIV-533/LIV-543/LIV-544/LIV-545/LIV-546 subsystem hardening triage

Warning ownership should follow runtime boundaries rather than rule names. The table below assigns every current production finding to one subsystem; totals sum to the 206-finding baseline above. LIV-543 reduced the orchestrator-owned row from 112 findings (`thrown_away_error`: 79, `discarded_result`: 20, `stringly_typed_error`: 9, `unwrap_used`: 4) to 18 findings (`thrown_away_error`: 7, `discarded_result`: 2, `stringly_typed_error`: 9). LIV-544 reduced the state projection/recovery/artifact row from 66 findings (`thrown_away_error`: 26, `stringly_typed_error`: 16, `unwrap_used`: 24) to 3 residual ledger/record findings. LIV-545 removed the prior workspace/workspace-driver filesystem-boundary cluster from the driver and workspace-run files; the current workspace row is 12 `workspace_manifest.gleam` findings after rebasing. LIV-546 reduced the agent/Pi row from 34 findings (`thrown_away_error`: 9, `discarded_result`: 25) to 0 findings.

| Subsystem | Total | `thrown_away_error` | `discarded_result` | `stringly_typed_error` | `unwrap_used` | `error_context_lost` | Primary files |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| Orchestrator / daemon / transition / effect runner | 18 | 7 | 2 | 9 | 0 | 0 | `orchestrator/daemon.gleam`, `orchestrator/core.gleam`, `orchestrator/worker_registry.gleam`, `orchestrator/transition.gleam`, `session/hub.gleam` |
| Workflow execution | 71 | 20 | 4 | 23 | 22 | 2 | `workflow_run.gleam`, `workflow_repair.gleam`, `workflow_scheduler.gleam`, `command_step.gleam`, `template.gleam` |
| State ledger / projection / recovery / artifacts | 3 | 0 | 0 | 2 | 1 | 0 | residual `state/ledger.gleam` FFI declarations and one `state/record.gleam` compatibility default |
| Tracker / Linear / control boundaries | 59 | 22 | 5 | 27 | 5 | 0 | `linear_body_data.gleam`, `linear.gleam`, `linear_contract.gleam`, `control/*`, `port.gleam`, `task.gleam`, `tracker/*` |
| Config / parsing / operator CLI | 43 | 9 | 3 | 5 | 26 | 0 | `config.gleam`, `config/types.gleam`, `doctor.gleam`, `review_lane_preflight_policy.gleam`, `terminal/*`, `version.gleam` |
| Workspace / workspace drivers | 12 | 1 | 0 | 0 | 6 | 5 | `workspace_manifest.gleam` |
| Agent / pi execution | 0 | 0 | 0 | 0 | 0 | 0 | Clean after LIV-546 agent/Pi attempt-lifecycle hardening. |

LIV-543 orchestrator daemon/transition hardening reduced that subsystem to the residual stringly typed daemon/transition helpers plus a few fallback-chain findings in adjacent orchestrator support files. LIV-544 state hardening started from the checked-run subsystem count of 66 findings and reduced the scoped projection/recovery/artifact files to zero findings by adding typed decode/recovery errors, named default helpers, contextual artifact decode messages, and four documented leaf-FFI suppressions. LIV-545 workspace/workspace-driver hardening removed the targeted 36-finding filesystem-boundary cluster (`thrown_away_error`: 3, `discarded_result`: 9, `unwrap_used`: 24); the current workspace row is 12 `workspace_manifest.gleam` findings after rebasing. LIV-546 agent/Pi hardening started from the checked-run subsystem count of 34 findings (`discarded_result`: 25, `thrown_away_error`: 9) across the agent/Pi attempt lifecycle files above and reduced that subsystem to 0 findings. Workspace cleanup/setup and driver boundary failures now return or surface explicit errors, agent cleanup, abort, artifact-write, and protocol fallback paths now either return the primary error, emit a lifecycle update, or explicitly use parser fallback helpers, and state artifact FFI suppressions document immediate normalization into typed domain errors.

Classification by warning pattern:

| Pattern | Classification | Triage guidance |
| --- | --- | --- |
| `error_context_lost` in workflow recovery/output manifest and workspace manifest handling | Mechanical fix candidate / high-signal drift | Preserve or wrap the original projection/manifest error when useful. Use `result.replace_error` only if the stable public error intentionally hides details. |
| `discarded_result` around process sends, hooks, cleanup, and filesystem work | Needs design at runtime boundaries; mechanical only for clearly best-effort diagnostics | Decide per subsystem whether to propagate, log, retry, or explicitly document best-effort behavior. Avoid mass binding to `_` just to silence lint. |
| `thrown_away_error` in parsers, decoders, projections, and transition fallbacks | Mixed: acceptable fallback chains, but needs design for daemon/state/workflow boundaries | Parser fallback chains may be acceptable with explicit `result.or`-style intent; runtime recovery and projection paths should preserve enough context for repair/debugging. |
| `stringly_typed_error` at FFI/control/tracker/workflow/state APIs | Needs design | Migrate durable/domain APIs to typed errors by boundary. Leaf FFI shims can remain stringly only if they normalize immediately into typed domain errors. |
| `unwrap_used` for config defaults, domain-map defaults, and path fallbacks | Mostly acceptable with explicit reason; remaining recovery/path cases need design | Introduce named lookup/default helpers or narrow suppressions for true invariants. Do not promote until remaining path-boundary fallbacks distinguish recoverable failure from intentional default. |
| Custom `scherzo_public_function_labels` | Clean | No backlog needed unless new findings appear. |

High-signal clusters for derivative backlog:

| Priority | Proposed follow-up | Rationale |
| --- | --- | --- |
| P0 | Workflow recovery, workspace manifest, and contract-output error-context hardening | Current `error_context_lost` findings are in workflow repair/output manifest and workspace manifest paths, where lost provenance can make repair failures opaque. |
| P0/P1 | Residual orchestrator daemon/transition error-handling hardening | LIV-543 reduced this cluster to 18 findings. Remaining work is mostly stringly typed daemon/transition helpers plus a few fallback-chain findings in adjacent orchestrator support files. |
| Done (LIV-544) | State projection/recovery/artifact typed-error and default-helper hardening | The scoped projection/recovery/artifact files were reduced from 66 subsystem findings to zero; three residual state findings remain in ledger FFI declarations and one record compatibility default. |
| P1 | Workspace manifest warning triage | Current workspace subsystem findings are 12 `workspace_manifest.gleam` warnings (`thrown_away_error`: 1, `unwrap_used`: 6, `error_context_lost`: 5) after the LIV-545 driver-boundary cleanup. |
| Done (LIV-545) | Workspace driver filesystem-boundary hardening | The original workspace/workspace-driver boundary findings in the driver and workspace-run files were handled by making cleanup/setup failures explicit and naming stable defaults. |
| Done (LIV-546) | Agent/pi attempt lifecycle result-handling hardening | The checked-run subsystem count moved from 34 findings to 0 by making cleanup/artifact/protocol failures visible or explicit. |
| P2 | Tracker/Linear/control typed-boundary migration | 59 findings, led by stringly typed FFI/control/Linear errors. This should be a gradual boundary-by-boundary typed-error migration to avoid wide interface churn. |
| P2 | Config/default helper and suppression audit | 43 findings, mostly explicit config/operator defaults. Convert repeated defaults to named helpers and add narrow `// nolint:` reasons only for stable invariants. |

## Triage summary

- **Promoted/intended error tier:** `missing_type_annotation`, `error_context_lost`, and the custom `scherzo_public_function_labels` rule. `missing_type_annotation` and `scherzo_public_function_labels` remain clean; `error_context_lost` currently has seven high-signal workflow/workspace manifest findings reported as warnings by the current glinter run and should be fixed or made explicit before treating the ratchet as clean.
- **Promoted to visible warning-equivalent inventory:** `unwrap_used` after the LIV-131 audit. It has useful signal when touching nearby code, but the remaining baseline is not appropriate for blocking PRs. In glinter 2.16.0, these findings still display as `[off]` because the rule's built-in default severity is `Off`.
- **Fixed for earlier promotion:** one schedule parsing `error_context_lost` finding was changed from `result.map_error(fn(_) { ... })` to `result.replace_error(...)`, making the intentional context replacement explicit. LIV-131 also fixed high-signal `unwrap_used` cases in command/hook diagnostics, prompt-file path resolution, and workflow fingerprint representation before enabling the warning.
- **Suppress:** only the four LIV-544 artifact-store leaf FFI `stringly_typed_error` suppressions described above. Future suppressions must be narrow `// nolint:` comments with reasons, as described below. No other rollout in this baseline (LIV-101, LIV-102, LIV-131, LIV-533, LIV-543, LIV-545, or LIV-546) added suppressions.
- **Keep as warning only:** `discarded_result`, `thrown_away_error`, and `stringly_typed_error`; keep `unwrap_used` as visible warning-equivalent inventory. Triage and backlog them by subsystem rather than doing blanket cleanup.
- **Keep off:** style, complexity, broad export, and FFI rules until a separate rollout proves acceptable signal.

## LIV-131 `unwrap_used` audit

The initial LIV-131 inventory found 88 production `unwrap_used` findings across 19 files. After fixing high-signal cases, the enabled baseline at the time was 79 findings across 17 files; the current LIV-543/LIV-544/LIV-545/LIV-546 refresh records the drift above. The main categories are:

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

## Test-source lint policy

Current enforcement remains: `test/` is formatted and executed by `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, but it is not linted by the checked production lint gates. The policy in this section is the contract for a future separate test lint command; it should not be implemented by directly adding `test/` to the existing production-source gate.

### Test errors

These rules should be blocking for a test-specific glinter profile:

| Rule | Rationale in tests |
| --- | --- |
| `avoid_todo` | A committed `todo` is unfinished test behavior. Use an explicit fixture, a skipped/manual runbook note, or a tracked follow-up instead of a runtime placeholder. |
| `division_by_zero` | Literal zero division is deterministic invalid behavior, not a useful test idiom. A test that intentionally documents runtime/compiler behavior must use a narrow `// nolint:` with a reason. |
| `error_context_lost` | Test helpers should preserve error context so failures explain the bad fixture, decoder, process, or filesystem boundary. Intentional replacement should use `result.replace_error`, as in production. |
| `panic_without_message` | Tests may use controlled panics for forbidden callbacks or expected crash paths, but a bare panic makes failures opaque. Panic in tests must say what invariant was violated. |

### Test warnings

These rules should remain visible but non-blocking in a test-specific profile:

| Rule | Rationale in tests |
| --- | --- |
| `discarded_result` | Async probes, process sends, cleanup, and fixture setup sometimes intentionally ignore results, but an ignored `Result` can hide a broken barrier, failed cleanup, or missed message. Review warnings when touching nearby tests. |
| `thrown_away_error` | Negative-path assertions and parser fallback fixtures may only care that an error occurred, but discarded error details can make failed tests harder to diagnose. |
| `stringly_typed_error` | Local test helpers may use `Result(_, String)` for compact fixtures. Shared or durable test helper APIs should prefer typed errors when the type improves assertions. |

### Disabled in tests

These rules should be off for test source unless a later issue proves a narrower, low-noise variant:

| Rule(s) | Why disabled for tests |
| --- | --- |
| `assert_ok_pattern` | `let assert Ok(...)`, `let assert Error(...)`, and shape assertions are the standard way tests fail at the exact setup or assertion line. Applying the production crash-avoidance rule would obscure failures and generated most of the test findings. |
| `avoid_panic` | Test doubles intentionally panic for “should not be called” branches and crash-path coverage. Message quality is covered by `panic_without_message`; banning all panics would block clear sentinel failures. |
| `missing_type_annotation` | Test functions and local fixture helpers benefit from inference and concise setup. Add explicit types for shared helpers when they clarify a contract, not as a blanket lint requirement. |
| `unused_exports` | Gleeunit discovers public `_test` functions, and test modules expose fixtures/helpers differently from production APIs. Production export hygiene does not map cleanly to `test/`. |
| `label_possible`, `missing_labels` | Test fixtures and assertions prioritize compact data setup. Public API call-site clarity is a production and shared-library concern, not a blanket test-source rule. |
| `todo_without_message` | Covered by `avoid_todo`; a more specific message rule adds no value once all test `todo` usage is blocking. |
| `echo`, `string_inspect`, `short_variable_name`, `unnecessary_variable`, `redundant_case`, `prefer_guard_clause`, `unnecessary_string_concatenation`, `trailing_underscore`, `unqualified_import`, `duplicate_import` | Style-only cleanup in tests is review guidance, not a lint gate. Current tests use compact names, constructor-heavy assertions, and fixture strings where these rules are noisy. |
| `deep_nesting`, `function_complexity`, `module_complexity` | Test scenarios often encode setup, action, and assertions together for readability. Split tests when humans cannot follow them; do not use broad complexity rules as a gate. |
| `unwrap_used` | Prefer `let assert` for new tests, but current unwrap/default helper uses need a targeted audit before enabling even a warning. |
| `ffi_usage` | The production FFI policy is handled separately. Test source may exercise FFI boundaries without inheriting a broad FFI lint gate. |

### Production-only rule decisions

- Apply unchanged to tests: `avoid_todo`, `division_by_zero`, and `error_context_lost` are high-signal failure-quality rules in both production and tests.
- Change severity or disable for tests: `assert_ok_pattern`, `avoid_panic`, and `missing_type_annotation` are production safety/API rules that conflict with clear test idioms, fixture setup, and expected-crash helpers.
- Keep warning-only in tests: `discarded_result`, `thrown_away_error`, and `stringly_typed_error` can reveal real async or helper-quality issues, but the current suite needs case-by-case triage before any blocking rollout.

## LIV-132 test-file evaluation

A LIV-132 scratch recheck with all built-in rules enabled against current `test/` scanned 100 files / 32,412 lines and produced 4,023 findings. The largest groups are the rules intentionally disabled by the test policy:

| Test rule | Count |
| --- | ---: |
| `assert_ok_pattern` | 1,731 |
| `missing_type_annotation` | 820 |
| `unused_exports` | 820 |
| `label_possible` | 335 |
| `unnecessary_string_concatenation` | 136 |
| `discarded_result` | 87 |
| `thrown_away_error` | 37 |
| `prefer_guard_clause` | 26 |
| `avoid_panic` | 15 |
| `stringly_typed_error` | 8 |
| `function_complexity` | 3 |
| `short_variable_name` | 2 |
| `deep_nesting` | 1 |
| `module_complexity` | 1 |
| `unwrap_used` | 1 |

The documented test profile above currently reduces that to 132 non-blocking warnings (`discarded_result`: 87, `thrown_away_error`: 37, `stringly_typed_error`: 8) and no blocking findings. It is still not enforced because the repository does not yet have a separate checked test lint command and the warning inventory should be triaged by async/process/filesystem subsystem before becoming a PR signal.

## Agent guidance

- Treat `glinter` and `scherzo_lint` errors as release-blocking.
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

1. Create a P0 workflow recovery/workspace manifest/contract-output hardening ticket for the seven current `error_context_lost` findings and adjacent typed contract errors.
2. Create a P0/P1 residual orchestrator daemon/transition hardening ticket for the 18-finding post-LIV-543 daemon/transition cluster.
3. Create P1 subsystem tickets for residual state ledger/record findings and workspace manifest warning triage; scoped state projection/recovery/artifact hardening was completed by LIV-544, workspace driver filesystem boundaries were completed by LIV-545, and agent/pi attempt lifecycle result handling was completed by LIV-546.
4. Create P2 typed-boundary migration tickets for tracker/Linear/control APIs and a config/default helper audit for stable invariants and narrow suppressions.
5. If tests are linted, implement the documented test-source policy as a separate command/profile instead of applying the production gate directly to `test/`.
