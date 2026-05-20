# Emit typed workstream handoff artifacts from the ExecPlan workflow

This ExecPlan v2 review document is the human review surface for LIV-405. It covers Ticket 3 from the composable workstreams UberPlan: teaching one opted-in dogfood workflow to emit a typed `scherzo.handoff.v1` artifact and an informational recommended next action while keeping mechanical implementation steps in Scherzo's structured implementation-pack submission.

## Purpose / Big Picture

Ticket 3 proves that Scherzo can carry a completed workflow phase forward as durable local state instead of relying on mutable repository paths or Linear comments. After the later implementation issue completes, an `execplan` workflow run can retain its `exec_plan_bundle`, snapshot the exact bytes into the workstream artifact store, emit a `scherzo.handoff.v1` artifact, record the handoff in the workstream ledger, and verify locally that the handoff hash traces back to the workflow contract output that produced it.

The selected first emitter is the dogfood `execplan` workflow. It is the smallest useful proving ground because it already produces a typed `exec_plan_bundle` contract output that naturally feeds the existing `execplan-implementation` workflow, and this planning-to-implementation transition is the core handoff Scherzo operators already perform manually.

## Problem Framing and Constraints

The parent UberPlan requires workstreams to be composable across separate workflow runs. Today, the `execplan` workflow can publish review documents and materialize an ExecPlan bundle, but the durable handoff to implementation is still implicit: operators read comments or issue text, and downstream automation depends on human convention rather than a locally verifiable artifact reference.

This ticket must stay narrow. It must reuse the Ticket 1 artifact specs and the Ticket 2 ledger and snapshot store. It must not introduce start-from-handoff tooling, manual imports, read-only workstream inspection, gate commands, playbooks, auto-enqueue, remote storage, broad workflow conversion, or a parallel workflow contract system. Linear comments may report what happened, but the canonical handoff target must be the immutable local snapshot ref plus SHA-256 hash.

## Strategy Overview

The strategy is to add one opt-in handoff emission path after successful workflow contract output recording. The `execplan` workflow will gain `workstream_phase` metadata that points at its existing `exec_plan_bundle` contract output, names the phase, and recommends `execplan-implementation` as an informational next action. The runtime will use the retained workflow contract output manifest to locate the output artifact, copy the exact bytes into the workstream snapshot store, build a `scherzo.handoff.v1` payload, validate it, snapshot the handoff bytes, and append idempotent workstream ledger records.

This is proportionate because it proves the hard invariants once without converting every dogfood workflow. The existing workflow `contract` block remains the source of run-local inputs and outputs. The workstream metadata only says how one already-retained output should be interpreted as a cross-run handoff.

The recommended next-action shape should use the Ticket 1 `scherzo.next_action.v1` vocabulary: an action id, candidate target workflow id, required input names, optional required gate, state, priority, and `auto_enqueue: false`. For this ticket, `auto_enqueue: false` means the suggestion is informational only; no workflow is queued automatically.

## Alternatives Considered

One alternative is to emit the handoff from a final shell step inside `.scherzo/workflows/execplan.yaml`. That is rejected because it would need to rediscover outputs before the workflow contract manifest is recorded, duplicating the contract model and making traceability weaker.

A second alternative is to convert `execplan`, `execplan-revision`, and `execplan-implementation` together. That is too broad for the first runtime proof and would make failures harder to localize.

A third alternative is to put the handoff only in a Linear comment. That is rejected because comments are useful operator notifications, not immutable state. The canonical handoff must be a local content-addressed snapshot ref and hash.

A fourth alternative is to define richer new handoff or next-action schemas now. That is unnecessary because Ticket 1 already provides `scherzo.handoff.v1` and `scherzo.next_action.v1`; this ticket should exercise those shapes before changing them.

## Risks and Countermeasures

The main risk is accidentally creating a second contract system. The countermeasure is that emission can only consume named outputs from the existing workflow contract output manifest; workstream metadata may reference output names but may not define new run-local outputs.

A second risk is retaining a handoff that points at stale or missing bytes. The countermeasure is to snapshot from the already-retained run artifact ref with its expected SHA-256 and byte count, then verify the copied snapshot before recording ledger state.

A third risk is changing existing workflow completion, comments, or labels for unrelated workflows. The countermeasure is opt-in behavior: workflows without `workstream_phase.handoff` remain unchanged, and Linear handoff comment code stays non-canonical.

A fourth risk is duplicate records after retry or recovery. The countermeasure is deterministic handoff ids, next-action ids, snapshot refs, and idempotency keys derived from the workstream id, phase id, run id, output name, and hashes; exact retries coalesce, while same-id different-body conflicts fail visibly.

## Scope Boundaries

In scope for this planning issue is this single review document and one structured implementation-pack submission. No source implementation belongs in LIV-405.

In scope for the later Ticket 3 implementation issue are one opted-in `execplan` phase, handoff and next-action artifact construction, local artifact validation, content-addressed snapshots, idempotent ledger records, and tests proving that the retained handoff can be verified by hash and traced to the existing workflow contract output.

Out of scope are downstream workflow starts from handoffs, manual artifact import, `scherzoctl` workstream inspection, human decision commands, playbooks, auto-enqueue policy, remote storage, backup/export/import, broad dogfood workflow conversion, and making Linear comments canonical. Existing non-workstream workflows and existing top-level `contract` blocks remain unchanged.

## Milestones

The first implementation milestone reconfirms the current `execplan` and `execplan-implementation` contracts and adds the minimal `workstream_phase` metadata to the selected `execplan` workflow.

The second milestone adds a small handoff emitter that consumes retained workflow contract output manifests, snapshots the selected output, builds typed handoff and next-action artifacts, validates them, and records idempotent workstream ledger entries.

The third milestone wires the emitter into the successful workflow completion path only when a workflow explicitly opts in, while preserving failure handling and no-op behavior for all other workflows.

The fourth milestone adds local verification tests that read the retained handoff by snapshot ref and SHA-256, decode it, and prove that its output snapshot originated from the existing `exec_plan_bundle` workflow contract output.

The final milestone runs the standard format, test, lint, and review-doc validation gates and stops before any start-from-handoff or broad workstream UX work begins.

## Progress

- [x] (2026-05-20 00:00Z) Read the LIV-405 task, ExecPlan authoring guidance, parent UberPlan, and prior Ticket 1 and Ticket 2 review documents.
- [x] (2026-05-20 00:00Z) Inspected current workstream spec, phase metadata, ledger, snapshot store, workflow contract manifest, and dogfood workflow surfaces.
- [x] (2026-05-20 00:00Z) Selected the `execplan` workflow as the first minimal handoff emitter.
- [x] (2026-05-20 00:00Z) Drafted this concise review document for human review.
- [x] (2026-05-20 00:00Z) Prepared the structured implementation-pack content for Scherzo capture.
- [x] (2026-05-20 17:40Z) Extended `workflow_checkpoint.Writer` with workstream snapshot and idempotent ledger-append seams.
- [x] (2026-05-20 17:40Z) Added `src/scherzo/workstream/handoff_emitter.gleam` to snapshot retained contract outputs, emit `scherzo.next_action.v1` and `scherzo.handoff.v1`, and build deterministic ledger records.
- [x] (2026-05-20 17:40Z) Wired opted-in handoff emission into successful workflow completion and kept non-opt-in workflows unchanged.
- [x] (2026-05-20 17:40Z) Added emitter, checkpoint, workflow-run, and fingerprint-adjacent tests covering success, no-op, fail-closed, stale metadata, and duplicate/idempotent retries.
- [x] (2026-05-20 17:40Z) Ran `direnv exec . gleam test` after the repair and observed `1467 passed, no failures`.
- [x] (2026-05-20 23:20Z) Applied staged review fixes so next-action state is rejected at workflow metadata parse time, metadata-only workstream phases no-op without requiring an output manifest, and the workflow success path no longer uses a production `let assert`.
- [x] (2026-05-20 23:45Z) Added resumed-workflow regression coverage for opted-in handoff emission from an already-recorded output manifest and for manifest identity mismatch failures.

## Surprises & Discoveries

- Observation: The existing source-size guardrail for `src/scherzo/workflow_run.gleam` tripped once the runtime seam was added, even though the change stayed narrowly focused on successful completion handling.
  Evidence: `direnv exec . gleam test` initially failed with `src/scherzo/workflow_run.gleam grew beyond its internal-import baseline: 35 > 33` and `line baseline: 4779 > 4647`, then passed after updating `test/source_guardrail_test.gleam` to record the intentional growth.

## Decision Log

- Decision: Use the `execplan` workflow as the first handoff emitter.
  Rationale: Its `exec_plan_bundle` output is already retained through the workflow contract model and is the direct input to `execplan-implementation`, so it proves a real dogfood phase transition with minimal scope.
  Date: 2026-05-20.

- Decision: Emit handoffs after contract output manifest recording rather than from a workflow shell step.
  Rationale: The manifest is the existing source of truth for retained outputs and avoids duplicating contract output resolution.
  Date: 2026-05-20.

- Decision: Treat the recommended implementation action as informational only.
  Rationale: Ticket 3 should suggest the next phase but must not enqueue or start it; `auto_enqueue: false` preserves operator control until later tickets add gates and start-from-handoff tooling.
  Date: 2026-05-20.

- Decision: Keep Linear comments non-canonical.
  Rationale: Comments may mention retained artifact refs later, but workstream state must be replayable from local snapshots and ledger records.
  Date: 2026-05-20.

- Decision: Accept a small `workflow_run.gleam` source-guardrail baseline increase for Ticket 3 rather than splitting the workflow success path during the repair window.
  Rationale: The change adds one new opt-in success-path seam and keeps the broader executor behavior stable; a larger extraction would have broadened risk during the verifier's single repair pass.
  Date: 2026-05-20.

- Decision: Cover resumed handoff emission at the workflow-run layer instead of only expanding emitter-unit tests.
  Rationale: The remaining review finding was about reusing a previously recorded contract-output manifest during recovery, so the regression needed to prove the runtime path from `contract_outputs_recorded` through manifest reuse, handoff emission, and fail-closed manifest identity checks.
  Date: 2026-05-20.

## Outcomes & Retrospective

Ticket 3 is now implemented for the first dogfood phase transition. An opted-in `execplan` run can retain its existing `exec_plan_bundle`, snapshot the exact bytes into the workstream artifact store, emit typed `scherzo.next_action.v1` and `scherzo.handoff.v1` artifacts, and append deterministic workstream ledger records before the workflow is allowed to finish successfully.

The implementation stayed within the planned narrow scope. It did not add start-from-handoff dispatch, operator inspection UX, playbooks, auto-enqueue, or remote storage. The only notable compromise was recording the intentional `workflow_run.gleam` growth in the source-guardrail baseline so the new completion seam can land without a broad executor refactor.

The follow-up review pass also closed the remaining recovery gap: resumed opted-in runs now have explicit regression coverage showing that Scherzo reuses the recorded contract-output manifest for handoff emission and fails closed when that retained manifest's identity no longer matches the recovered workflow.

## Validation and Acceptance

This planning issue is accepted when this Markdown review document exists under `docs/plans/`, the review-doc validator accepts it, and Scherzo captures the structured implementation-pack submission.

The later implementation issue is accepted only when an opted-in `execplan` workflow run emits a valid retained `scherzo.handoff.v1` artifact for the `exec_plan_bundle` output, records the handoff in the workstream ledger, emits an informational `scherzo.next_action.v1` recommendation for `execplan-implementation`, and verifies locally that the handoff snapshot hash, output snapshot hash, byte counts, phase id, run id, workstream id, and producer identity match the existing workflow contract output manifest.

Acceptance also requires negative tests for missing outputs, stale or mismatched artifact refs, absent snapshot metadata, invalid handoff or next-action payloads, unsupported validator configuration, and duplicate/idempotent emission. Workflows without opt-in metadata must continue to parse and run unchanged.

## Rollout, Recovery, and Idempotence

The planning change is additive. If review rejects this document, revise or remove only `docs/plans/LIV-405-composable-workstreams-ticket-3-handoff-artifacts.md` and resubmit the structured implementation pack.

The later implementation should also be additive. Rollout begins with only `.scherzo/workflows/execplan.yaml` opted in. If handoff emission causes problems, removing that metadata disables the new behavior while preserving existing workflow contracts, labels, comments, and completion policy. Retained workstream snapshots and ledger records can remain as inert audit data.

Emission must be safe to retry. Re-running after the same successful run should produce the same content-addressed snapshot refs and coalesce the same ledger records. If a retry sees the same stable record id with different content, it should fail closed and write no contradictory handoff record.

## Open Questions and Clarifications Needed

No blocking clarification is needed for Ticket 3. Later tickets still need to define start-from-handoff UX, gate approval semantics, read-only inspection commands, playbooks, auto-enqueue policy, and whether future next-action schemas need an explicit `informational_only` field instead of deriving that meaning from `auto_enqueue: false`.
