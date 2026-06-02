# Linting policy

Scherzo has two checked production lint commands. Both commands read `gleam.toml`, both scan production Gleam source under `src/`, and both intentionally exclude `test/` from the production lint profile.

Run the normal Glinter production gate with:

```sh
direnv exec . gleam run -m glinter
```

This is the PR #60 / LIV-101 production safety gate. It enforces the blocking safety and API rules configured in `[tools.glinter.rules]`, including `assert_ok_pattern`, `avoid_panic`, `avoid_todo`, `division_by_zero`, `error_context_lost`, and `missing_type_annotation`. It also keeps warning-only inventory rules visible for `discarded_result`, `thrown_away_error`, `stringly_typed_error`, and `unwrap_used`. LIV-102 and later subsystem work own ratcheting those warning inventories to errors after the baseline is clean enough.

The high-signal warning inventory has a checked subsystem/module baseline at `docs/lint/glinter-high-signal-baseline.md`. Refresh it intentionally with:

```sh
direnv exec . gleam run -m scherzo_lint_high_signal_inventory -- --output docs/lint/glinter-high-signal-baseline.md
```

The report groups `discarded_result`, `error_context_lost`, `stringly_typed_error`, `thrown_away_error`, and selected `unwrap_used` findings by subsystem and module so follow-up tickets can ratchet one owner at a time. `direnv exec . gleam run -m scherzo_lint` also runs the `scherzo_lint_agent_pi_high_signal_zero` guard before the normal Glinter pass; `src/scherzo/agent/` and `src/scherzo/pi/` must remain at zero tracked high-signal findings unless a follow-up explicitly changes that ratchet.

Run the Scherzo-specific style gate with:

```sh
direnv exec . gleam run -m scherzo_lint
```

This command is implemented by `test/scherzo_lint.gleam`. It first enforces the `scherzo_lint_agent_pi_high_signal_zero` ratchet for `src/scherzo/agent/` and `src/scherzo/pi/`, then adds repository-local Glinter rules to the normal production lint run. The current custom Glinter rule is `scherzo_public_function_labels`, which blocks a public production function with exactly two parameters when an unlabelled named parameter is annotated as primitive `Bool`. For example, `pub fn configure(path: String, enabled: Bool)` fails because `configure(path, True)` is unclear at the call site; `pub fn configure(path: String, enabled enabled: Bool)` passes and callers use `configure(path, enabled: True)`.

The first rollout is deliberately narrower than broad labelled-argument style linting. High-arity public functions and duplicate primitive arguments remain deferred because the first inventory showed that enforcing those checks immediately would create broad API churn. The built-in `label_possible` and `missing_labels` rules stay off for the same reason.

Use a suppression only for a genuine exception. The exact syntax is:

```gleam
// nolint: scherzo_public_function_labels -- function-specific reason
pub fn example(path: String, enabled: Bool) {
  Nil
}
```

The comment must be on its own line immediately above the function, must name only the narrow rule being suppressed, and must include a real reason after `--`. Do not use trailing inline `// nolint:` comments or blanket file-level ignores for this rule. Prefer adding a label and updating compiler-reported call sites when the API shape is under Scherzo's control.

Production test policy remains unchanged. `test/` is formatted and executed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, but it is not included in the production Glinter profile. A future test-specific lint command should use its own severities rather than adding `test/` to the production gate.

When intentionally updating `docs/lint/glinter-high-signal-baseline.md`, first run the normal lint gates, regenerate the inventory report, and review the subsystem/module diff. A baseline increase should be tied to the touched subsystem and called out in review; otherwise prefer fixing the warning or adding a narrow, reasoned `// nolint:`. Do not relax the agent/Pi zero guard as part of unrelated work.

SelfCI runs both production lint commands: first `direnv exec . gleam run -m glinter`, then `direnv exec . gleam run -m scherzo_lint`. A pull request should pass both commands before review.
