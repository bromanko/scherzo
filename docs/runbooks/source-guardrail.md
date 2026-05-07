# Source module size and import-count guardrail

Scherzo keeps a lightweight guardrail in `test/source_guardrail_test.gleam` so
agents notice when source modules become harder to fit into working context.
The guardrail scans checked-in `src/**/*.gleam` and `src/**/*.erl` files during
`gleam test`.

## Checked command

```sh
direnv exec . gleam test
```

The guardrail test enforces these defaults for source files that are not already
baselined:

- at most 1,000 physical lines; and
- at most 20 internal Gleam imports, counted as `import scherzo...` lines.

Existing oversized modules are recorded in `oversized_source_baseline()` in
`test/source_guardrail_test.gleam` with their current line and internal-import
counts. Baselined files may shrink freely, but they fail validation if either
count grows above the recorded value. When a change intentionally shrinks a
baselined file, lower the matching `SourceLimit` in the same change when doing
so is straightforward; that keeps the ratchet from drifting stale.

## Failure output

When the guardrail fails, the test prints:

- the exact file and metric that regressed;
- the largest source modules by line count; and
- the largest source modules by internal import count.

Use the report as an early warning, not as a mandate for a broad refactor in an
unrelated change.

## What to do when it fails

1. Start with `docs/ARCHITECTURE.md` to find the subsystem boundary and nearby
   modules that should own the new behavior.
2. Prefer moving pure policy, parsing, formatting, or data-shaping logic into a
   focused module instead of adding more branches to an already oversized file.
3. If the growth is intentional and cannot be split safely in the current
   change, update the matching `SourceLimit` entry in
   `test/source_guardrail_test.gleam` or add a new entry for the new oversized
   file.
4. In the review summary, call out why the baseline update is intentional and
   what future extraction would reduce it.

Do not update the baseline just to make an unrelated change pass. The goal is to
make module growth visible and reviewable while allowing known large modules to
exist until planned decomposition work lands.
