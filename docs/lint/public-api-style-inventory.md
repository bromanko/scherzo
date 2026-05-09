# Public API style lint inventory

This summary records the first production inventory for the remaining staged Scherzo public API style lint candidates from LIV-140/LIV-141. The raw report was generated from the repository root with:

    mkdir -p build
    direnv exec . gleam run -m scherzo_lint_inventory -- --path src --format markdown --output build/public-api-style-inventory.md

The generated `build/public-api-style-inventory.md` file is intentionally not committed because it is a large raw dump under the ignored build directory. This document captures the reviewable counts, examples, exception classification, and rollout recommendation. Findings are inventory data, not lint failures.

## Inventory totals

The production scan found 300 candidate rows across `src/`.

| Candidate | Total findings | Rollout decision |
| --- | ---: | --- |
| High-arity public functions | 133 total, including 87 arity `5+` findings | Keep inventory-only. This exceeds the warning threshold and the global blocking threshold. |
| Duplicate primitive public parameters | 147 | Keep inventory-only. This exceeds the warning threshold and the global blocking threshold. |
| Broader unlabelled public `Bool` parameters | 20 | Keep inventory-only for this slice. It is small enough to consider a future warning-only review, but it exceeds the global blocking threshold and overlaps the existing two-parameter `Bool` policy. |

No candidate is promoted to a warning or blocking Glinter rule by this implementation. The existing `scherzo_public_function_labels` rule remains the only Scherzo-specific custom rule registered by `direnv exec . gleam run -m scherzo_lint`.

## Counts by subsystem

| Subsystem | Findings |
| --- | ---: |
| Orchestration | 82 |
| Top-level utilities | 69 |
| Agent and pi integration | 40 |
| Persisted state and projection | 32 |
| Workspace management | 26 |
| Linear integration | 25 |
| Control and CLI surfaces | 15 |
| Session tracking | 8 |
| Configuration | 3 |

The largest concentrations are orchestration and top-level utilities, which means any blocking rollout would create broad public API churn rather than a narrow cleanup.

## Candidate details

High-arity public functions account for 133 rows: 46 in arity `4`, 34 in arity `5`, and 53 in arity `6+`. The first blocking-eligible high-arity bucket is arity `5+`, and that bucket alone has 87 findings. Arity `5+` appears in orchestration 27 times, top-level utilities 23 times, agent and pi integration 13 times, workspace management 11 times, persisted state and projection 6 times, Linear integration 3 times, control and CLI surfaces 2 times, and session tracking 2 times. Representative examples include `src/scherzo/orchestrator/effects/interpreter.gleam` `new_production_shell_state` with arity 45, `src/scherzo/agent/run_attempt.gleam` `run_prompt_mode_in_workspace` with arity 10, and `src/scherzo/linear_attachment.gleam` `attach_markdown_to_comment` with arity 6. Thirteen high-arity rows have callback or comparator convention hints.

Duplicate primitive public parameters account for 147 rows: 130 duplicate `String` rows, 15 duplicate `Int` rows, and 2 duplicate `Bool` rows. The largest subsystem concentrations are orchestration with 33 rows, top-level utilities with 32, persisted state and projection with 19, Linear integration with 18, and agent and pi integration with 17. Representative examples include `src/scherzo/linear.gleam` `build_comment_create_request` with `issue_id` and `body`, `src/scherzo/orchestrator/core.gleam` APIs with repeated workflow and run identifiers, and `src/scherzo/state/*` APIs with repeated state and artifact identifiers. Three duplicate-primitive rows have callback or comparator convention hints.

Broader unlabelled public `Bool` parameters account for 20 rows. The findings are spread across agent and pi integration 5 times, persisted state and projection 5 times, orchestration 4 times, top-level utilities 4 times, and Linear integration 2 times. Representative examples include `src/scherzo/agent/pi_rpc.gleam` and `src/scherzo/pi/client.gleam` `launch`/`launch_spec` with `auto_retry`, `src/scherzo/state/ledger.gleam` `append_many` with `fsync`, and `src/scherzo/linear_body_data.gleam` `append_file_node*` with `dedupe_by_filename`. No production rows were marked as already covered by the existing two-parameter `Bool` rule; the current production tree has already labelled or otherwise avoided that shape.

## Exception classification

The inventory runner assigns lightweight likely-exception hints so reviewers can separate conventional APIs from likely actionable ambiguity. In this scan, 16 rows were classified as callback or comparator convention candidates and 284 rows had no exception hint. No rows were classified by the current heuristics as FFI wrappers, decode helpers, builder-style helpers, generated artifacts, or test-adjacent production artifacts.

For threshold math, treat the 16 callback/comparator rows as likely exception candidates and the 284 unhinted rows as ordinary or unclear API ambiguity until reviewed manually. This is intentionally conservative. A future promotion proposal should review the raw rows for the selected candidate or subsystem and should not rely only on these heuristic hints.

## Threshold application and recommendations

High-arity public functions stay inventory-only. The candidate exceeds the warning threshold of fewer than 50 total findings, exceeds the global blocking threshold of no more than 15 unsuppressed findings, and has more than 20 findings in orchestration alone. Arity `4` is explicitly not eligible for first blocking, and arity `5+` still has 87 findings.

Duplicate primitive public parameters stay inventory-only. The candidate exceeds the warning threshold of fewer than 50 total findings and the global blocking threshold of no more than 15 unsuppressed findings. It also has multiple subsystems above the 20-finding subsystem warning threshold. The repeated values are often identifiers, paths, command strings, or body strings, so any future slice should start with one subsystem and call-site search rather than a repository-wide rule.

Broader unlabelled public `Bool` parameters stay inventory-only for this implementation. The raw count is below the warning-only count threshold, and no subsystem has more than 5 findings, so this is the only candidate worth a future warning-only review. It is not promoted here because it still exceeds the global blocking threshold, touches five subsystems, overlaps the existing `scherzo_public_function_labels` policy, and needs a manual call-site churn estimate before even warning noise is introduced.

No module-scoped or globally blocking rule is recommended from this inventory. If maintainers want a follow-up, the safest next step is to manually review the 20 broader-`Bool` rows or one narrow duplicate-primitive subsystem and then re-run the inventory after any small local label cleanup.
