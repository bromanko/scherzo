# Split ledger state by domain

This ExecPlan is a living document. Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must remain current as implementation proceeds. Detailed edit instructions, interfaces, commands, and test cases are carried in the structured implementation pack submitted with this review.

## Purpose / Big Picture

Scherzo maintainers should be able to change one durable-state domain without navigating or reviewing two repository-wide switchboards. After this refactor, ledger records, folds, snapshot codecs, and queries for a domain such as scheduled jobs or outbox delivery live together under `src/scherzo/state/record/` and `src/scherzo/state/projection/`. Replaying an existing workspace must produce the same state, and writing any existing record must produce byte-for-byte compatible schema-version-2 JSON.

## Problem Framing and Constraints

`src/scherzo/state/projection.gleam` is 7,255 lines and `src/scherzo/state/record.gleam` is 4,648 lines. The first owns 24 projected fields, more than 30 public state types, one fold over every record body, and the complete projection-snapshot codec. The second owns the common ledger envelope, every record-body constructor, a wide optional-field decoder, encoding, decoding, kind selection, and redaction. Changes from unrelated subsystems therefore collide in the same files, and the type system does not stop one projection area from consuming another area's records.

This is a behavior-preserving extraction, not a ledger migration. `record.schema_version` remains 2; ledger JSON keys, values, omission/null behavior, object-field order, record ids, redaction, decode errors, and projection-snapshot JSON remain unchanged. `projection.Projection`, `projection.new`, `projection.fold`, `projection.fold_from`, and `projection.apply` remain the top-level API. The existing `src/scherzo/state/record/` and `src/scherzo/state/projection/` modules are the starting point and must be completed rather than replaced by a second architecture.

## Strategy Overview

Use ten matching bounded contexts: legacy runs, workflow runs, publications, steps and step recovery, issue recovery, command receipts and dispatch pause, control operations, scheduled jobs, outbox, and workstreams. Each context receives a typed record-body view, a projection-state slice, an `apply` function that accepts only that view, and its own ledger and snapshot codec logic. The top-level projection retains its current field names and composes those slices, while `projection.apply` classifies a ledger body once and dispatches to exactly one context.

Gleam cannot re-export another module's constructors through a type alias. To keep existing ledger fixture and round-trip tests literally unchanged, `record.RecordBody` therefore remains as a compatibility vocabulary in `src/scherzo/state/record.gleam`; it contains declarations and conversion only, not domain codec policy. Domain record modules own typed body views and codecs, and the top-level record module maps between the compatibility constructors and those views. This explicit compromise preserves current producer and test syntax while still making projection consumption type-restricted. A future public-API cleanup may remove that compatibility sum, but it is not required here.

The projection compatibility outcome is fixed rather than optional. `projection.Projection`, its flat field names, and every current public function in `src/scherzo/state/projection.gleam` remain source-compatible. Public domain types remain available through top-level type aliases, so existing type annotations remain valid. Their constructors cannot remain qualified through `projection`, however, and every in-repository constructor expression or pattern must move to the owning module qualifier, such as `legacy_runs_projection.RunRunning` or `steps_projection.StepAttemptRunning`. Before extraction, a checked-in symbol-and-path inventory will enumerate all such uses under `src/`, `test/`, `docs/`, and `scripts/`; the implementation must migrate every inventory entry and an architecture test must reject new uses of the old qualifiers. No other source break is permitted.

The extraction proceeds one domain at a time. Every domain milestone includes the matching record codec and projection slice, leaves all prior domains working, runs the full test suite and fixed replay-parity oracle through direnv, and lands as a green commit.

## Alternatives Considered

Adding headings and comments inside the two large files is the smallest change, but it does not reduce review collisions, context size, or accidental cross-domain consumption. Splitting only helper functions, which the current stub modules partially do, leaves types, fold policy, and codecs centralized and therefore does not finish the decomposition.

Replacing `record.RecordBody` immediately with a nested sum of domain body types would produce the cleanest model, but every constructor call and pattern match in production and tests would change, and existing round-trip tests could not remain unmodified because Gleam aliases do not re-export constructors. Keeping a duplicated compatibility sum plus canonical typed domain views is a deliberate, bounded exception. Nesting every domain state record inside `projection.Projection` was also rejected because it would force hundreds of unrelated field-access edits; retaining the current top-level field names provides the same dispatcher boundary with a much smaller migration surface.

## Risks and Countermeasures

The highest risk is a silent wire-format change caused by moving encoder field order, null handling, legacy variants, decoder requirements, or redaction. Before extraction, a checked-in golden JSONL corpus will cover every `RecordBody` constructor and compatibility variant, with byte-identical decode/re-encode for each canonical line. A second fixed variant matrix will supply the supported absent, explicit-null, and present forms for every optional field and record the old codec's exact expected result, including canonical omission when an accepted null does not re-encode as null. For every secret-bearing record family, that matrix will also prove secret replacement, excerpt or payload bounding, and preservation of non-secret fields. The existing legacy fixtures and `test/state_record_test.gleam` remain untouched.

A second risk is changing fold, query, or snapshot semantics while moving code. Before extraction, an immutable mixed-domain ledger corpus will be replayed by the old fold and its exact projection snapshot plus representative per-domain and cross-domain query outputs will be checked in as the baseline oracle. The same replay test must pass unchanged after every extraction milestone. Existing projection tests remain additional parity coverage, and each context gets focused tests proving that its records update only its state slice, unrelated records are unrepresentable at its `apply` boundary, legacy snapshots still decode, malformed partial task references still fail, and snapshot round trips are equal.

A third risk is replacing two monoliths with ten smaller but still tangled modules. Architecture guardrails will enforce the domain import matrix: a sub-projection may import its matching record module and approved neutral task-reference/codec modules, but not `scherzo/state/record`, another domain's record module, or another sub-projection. New modules remain under the source guardrail's 1,000-line limit; large snapshot helpers may use subordinate modules within the same context.

A final risk is stopping halfway. Because each milestone moves one complete context and keeps top-level facades intact, a failed slice can be reverted without touching the on-disk ledger or previously completed contexts.

## Scope Boundaries

In scope are the two top-level state facades, all existing files below `src/scherzo/state/record/` and `src/scherzo/state/projection/`, new matching `control_ops.gleam` modules and any same-context codec helpers, state record/projection tests, architecture and source guardrails, and the durable-state section of `docs/ARCHITECTURE.md`.

Out of scope are adding, removing, or renaming ledger kinds; schema-version changes; rewriting retained ledgers or snapshots; changing compaction, recovery, daemon, scheduler, workstream, command, or outbox behavior; changing control/query JSON; and broad consumer refactors unrelated to constructor qualification. No browser, provider-live, provider-cache, workflow YAML, helper-script, or operator dogfood change is required.

## Milestones

A foundation milestone freezes the current wire representation and records the allowed domain-import matrix. It adds exhaustive golden compatibility data and guardrail tests before moving behavior.

The next ten milestones extract, in order, legacy runs; workflow runs; publications; steps and step recovery; issue recovery; command receipts and dispatch pause; control operations; scheduled jobs; outbox; and workstreams. Each milestone moves that context's record-body view and codec, projection types and state transition, snapshot encode/decode support, and local queries while preserving top-level wrappers. The order starts with small compatibility-heavy contexts, separates control operations from the existing command stub before tackling larger contexts, and leaves cross-domain aggregate queries and facade cleanup until every slice exists.

A final integration milestone reduces `record.gleam` to the ledger envelope, compatibility vocabulary, classification/conversion, and public codec facade; reduces `projection.gleam` to composition, dispatch, cross-domain queries, and public snapshot facade; lowers or removes their source-guardrail baselines; updates architecture documentation; and runs all repository gates.

## Progress

- [x] (2026-07-12) Read `.scherzo/workflows/guidance/exec-plan.md` and confirmed the split review-document and implementation-pack contract.
- [x] (2026-07-12) Confirmed the prepared output target is `docs/plans` and no LIV-1446 plan already exists there.
- [x] (2026-07-12) Inspected the top-level record/projection modules, all existing domain stubs, fixture and projection tests, architecture guardrail, and source-size guardrail.
- [x] (2026-07-12) Ran the baseline `direnv exec . gleam test`; 2,339 tests passed with no failures.
- [x] (2026-07-12) Wrote this planning review document; no production implementation was performed.
- [x] (2026-07-12) Incorporated adversarial review by fixing the public compatibility contract, replay oracle, wire/redaction matrix, immutable-test comparison, rollback boundary, and manual-check disposition.
- [ ] Implement the foundation compatibility and boundary checks.
- [ ] Complete each domain milestone with the full suite green and a separate commit.
- [ ] Complete integration validation and retrospective evidence.

## Surprises & Discoveries

The repository has already begun this extraction: nine matching record/projection stub modules exist, and scheduled jobs already own substantial transition and snapshot logic. However, most stubs still expose generic helpers while the top-level fold and codecs remain authoritative. `projection/scheduled.gleam` is the only sub-projection that currently imports the top-level record module, which demonstrates both the intended direction and the cross-domain access problem this plan must remove.

The source guardrail already baselines `projection.gleam` at 7,255 lines and `record.gleam` at 4,648 lines while rejecting new modules over 1,000 lines. This gives the refactor an objective completion signal and prevents relocating a monolith unchanged.

## Decision Log

- Decision: Use the ten-context map stated in Strategy Overview, adding `control_ops` and keeping command receipts plus dispatch pause together in `commands`. Rationale: it follows the existing stub names while separating the independently projected control-operation lifecycle. Date: 2026-07-12.
- Decision: Keep top-level `Projection` field names and public facade functions. Rationale: direct field access is widespread, and nesting fields would add migration risk without improving record-consumption enforcement. Date: 2026-07-12.
- Decision: Retain the closed `record.RecordBody` constructors as a compatibility vocabulary and convert them to typed domain views. Rationale: Gleam does not re-export constructors through type aliases, and the required existing fixtures and round-trip tests must remain unmodified. Date: 2026-07-12.
- Decision: Treat byte-for-byte JSON equality, not merely semantic decode equality, as the wire-compatibility bar. Rationale: this is explicitly a code move and operators must not receive unexplained ledger churn. Date: 2026-07-12.
- Decision: Require `direnv exec . gleam test` after every domain milestone. Rationale: each extraction must be independently reviewable and reversible from a green commit. Date: 2026-07-12.
- Decision: Preserve top-level projection functions, fields, and type annotations, but require all in-repository uses of moved projection constructors to adopt their owning domain-module qualifier. Rationale: Gleam type aliases preserve type names but cannot preserve constructor qualification, so this is the smallest deterministic source break. Date: 2026-07-12.
- Decision: Freeze both wire bytes and mixed-domain replay results before extraction, and compare immutable witnesses against the recorded implementation baseline revision rather than only the final working-copy diff. Rationale: the review identified that round trips alone do not prove fold parity and an uncommitted diff cannot detect changes made by earlier commits. Date: 2026-07-12.

## Outcomes & Retrospective

The planning outcome is a bounded, reversible decomposition that preserves Scherzo's durable formats and top-level projection API while creating compiler-enforced record-consumption boundaries. Implementation outcomes, deviations, line-count reductions, and final validation evidence must be added here after each milestone and at completion.

## Validation and Acceptance

The planning ticket is accepted when this single review document exists directly under `docs/plans/` and Scherzo captures its structured implementation pack.

The implementation is accepted when all ten matching record/projection contexts own their domain behavior; `projection.apply` is a thin exhaustive dispatcher; no sub-projection can import or consume another domain's record bodies; existing ledger fixture data and `test/state_record_test.gleam` remain byte-for-byte unchanged from the recorded baseline revision and pass; the canonical golden corpus covers every constructor and re-encodes byte for byte; the fixed variant matrix reproduces the pre-extraction expected output for every applicable absent/null/present and redaction/bounding case; and the immutable mixed-domain ledger replays to its exact pre-extraction snapshot and query oracle.

The top-level public projection facade must preserve `Projection`, all flat field names, every current public function signature, and type-annotation compatibility. The only permitted source migration is replacing `projection.<MovedConstructor>` with the fixed owning domain qualifier. Every baseline inventory entry in production code, tests, documentation snippets, and helper scripts must be migrated; completion evidence is an empty old-qualifier scanner, a clean full compile/test run, and no unaccounted inventory entry.

Every domain commit must pass `direnv exec . gleam test`, including the unchanged replay oracle. Before publish, formatting, the full suite, both production lint gates, architecture guardrails, source guardrails, the old-qualifier scanner, and the baseline-revision comparison for immutable tests must pass. No manual browser, manual UI, provider-live, provider-cache, operator dogfood, or deferred human/operator check is required at any stage because this internal refactor changes none of those surfaces.

## Rollout, Recovery, and Idempotence

Roll out as one green commit per context, with the compatibility corpus and guardrails first. There is no runtime flag or data migration: deployments continue reading and writing schema-version-2 ledgers and snapshots. Detect regression through fixture, golden-wire, snapshot, architecture, and full-suite failures before merge.

Before a later milestone depends on a context and before final facade cleanup, a failing context may be reverted independently because its top-level compatibility path still exists. After dependent milestones or the integration commit land, independent context rollback is not assumed: revert the complete extraction stack back through the foundation boundary unless a dedicated full-suite and replay-oracle run first demonstrates that the narrower revert compiles and preserves behavior. In either case, the top-level facade and all stored data remain valid. Re-running tests or repeating an extraction after a clean revert is safe. Do not rewrite ledger files, regenerate retained snapshots, or bump schema versions as a recovery action.

## Open Questions and Clarifications Needed

No open question blocks implementation. If a natural context cannot stay below 1,000 lines, split private codec or snapshot helpers beneath the same context rather than increasing the source-size baseline or creating cross-domain dependencies.

Revision note (2026-07-12): This revision incorporates the plan review by making the compatibility migration deterministic, adding fixed replay and expanded wire/redaction oracles, requiring baseline-revision immutability evidence, narrowing independent rollback claims, and explicitly closing all manual-check obligations.
