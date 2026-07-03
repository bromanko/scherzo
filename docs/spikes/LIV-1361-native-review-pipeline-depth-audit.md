# LIV-1361 native review evidence pipeline depth audit

Date: 2026-07-03

## Sample

I audited retained Scherzo dogfood `implementation` and `execplan-implementation` artifacts available locally under the top-level checkout's `.scherzo/workspaces/` tree and durable metadata under `.scherzo/workspaces/.scherzo-state/artifacts/runs/`.

Method: lane-stage counts came from the run roots below that still retained full `artifacts/review/lanes/**` JSON. Finalizer counts came from a read-only scan of `.scherzo/workspaces/.scherzo-state/artifacts/runs/**/finalize_review_dispositions-*/attempt-*.json`, filtered to `step_id == "finalize_review_dispositions"` and `workflow_id` in `implementation` or `execplan-implementation`. The finalizer denominator is command attempts, not unique run roots, so retried runs contribute multiple rows.

Full lane JSON was still retained for 10 native-review run roots: 8 `implementation` runs and 2 `execplan-implementation` runs. Those runs provided 40 evidence ledgers and 37 normalized lane results; one recent run stopped after one normalization result, so per-stage denominators differ.

Run roots with full lane artifacts:

- `implementation`: `LIV-1259-1782874099422-22`, `LIV-1244-1782867858163-4`, `LIV-1245-1782705963708-20`, `LIV-1220-1782562950115-46`, `LIV-1220-1782465235677-500`, `LIV-585-1779655756773-1`, `LIV-584-1779645282735-165`, `LIV-563-1779639932841-146`
- `execplan-implementation`: `LIV-1262-1782704485067-11`, `LIV-574-1779645397331-166`

For the disposition publication gate, durable command-attempt metadata was available for 255 `finalize_review_dispositions` attempts across both workflows: 190 `implementation` and 65 `execplan-implementation`.

## Results

| Stage | Denominator | Outcome-changing evidence | Pass-through / identity | Recommendation |
| --- | ---: | ---: | ---: | --- |
| `verify-evidence` | 35 draft findings with ledgers | 23 findings (65.7%) received negative deterministic annotations (`rejected` or `not_reproduced`) | 12 findings (34.3%) had only context/no verdict | Keep. This stage is not rubber-stamping lane drafts. |
| `normalize-lane-result` | 37 lane results / 33 draft findings | 14 lanes (37.8%) downgraded blockers; 15 findings (45.5%) changed `proposed_blocking: true` to `blocking: false` | 21 lanes (56.8%) identity-transformed; 2 more only recorded retry diagnostics | Keep semantics. It materially changes publish-blocking outcomes. |
| `finalize-dispositions --require-publishable` | 255 durable attempts | 0 publishability blocks; 5 finalizer failures rejected invalid or incomplete disposition inputs before publishability evaluation | 250 attempts emitted `REVIEW_PUBLISH_READY=true` | Keep as the disposition coverage/materialization gate; do not use the zero publishability-block count alone to remove it. |

### `verify-evidence`

Across 40 ledgers, the stage produced 42 verdicts:

- `rejected`: 25 verdicts, mostly unallowlisted evidence keys from lane drafts.
- `not_reproduced`: 4 verdicts, including fixture/schema checks that did not reproduce the requested observation.
- `context_only`: 13 verdicts.
- `verified`: 0 verdicts in this sample.

Per draft finding, 23 of 35 findings had a negative annotation. This is an anti-hallucination signal: the verifier frequently rejects unsupported evidence claims instead of passing them through.

### `normalize-lane-result`

The audited lane results had no severity changes, dropped findings, added findings, or synthesized failed-lane rescue artifacts:

- Severity changes: 0 findings.
- Dropped findings: 0 findings.
- Failed-lane rescue results: 0 lanes.

However, normalization changed blocker semantics often:

- `downgraded_unverified_blocker`: 12 findings.
- `downgraded_unproven_correctness_claim`: 3 findings.
- Total blocker downgrades: 15 findings across 14 lanes.

That means the stage preserved finding text/counts but prevented unverified lane claims from becoming publish-blocking review output.

### `finalize-dispositions`

The durable finalizer sample had 255 attempts:

- 250 succeeded with `REVIEW_PUBLISH_READY=true`.
- 0 failed with `blocking finding disposition state is not publishable`.
- 5 failed before publishability evaluation:
  - 3 attempts failed because the disposition input omitted synthesized finding IDs: 1 `implementation` attempt and 2 `execplan-implementation` attempts. The retained durable errors did not include the missing findings' categories.
  - 2 `implementation` attempts failed because `entries[0].evidence_refs[0]` was a bare string instead of a required evidence-ref object.

The five full final-disposition artifacts still retained in run roots contained 20 disposition entries:

- `resolved`: 18 entries.
- `deferred`: 2 entries, both non-blocking `maintainability` findings.
- `rejected`: 0 entries.
- `obsolete`: 0 entries.
- Blocking deferred findings: 0.

## Recommendation

Do not collapse the live native review pipeline based on this sample.

`verify-evidence` and `normalize-lane-result` both earn their keep: together they rejected unsupported evidence and downgraded 15 unverified would-be blockers. Removing them would change review outcomes, not just remove ceremony.

The publishability subcheck in `finalize-dispositions --require-publishable` did not fire in retained attempts, but the finalizer still caught invalid or incomplete disposition inputs and materializes the final disposition bundle. Keep it unless a later design preserves those checks in an equivalent disposition gate.

No follow-up collapse ExecPlan was filed from this audit because the proposed 3-stage collapse is not warranted by the retained-run evidence.
