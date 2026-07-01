# Redesign `scherzoctl` recovery commands around resources

This ExecPlan review document is intentionally concise. Mechanical implementation detail is supplied in the structured implementation pack for LIV-1333, and both artifacts together are the self-contained handoff for follow-up implementation.

## Purpose / Big Picture

After this change, an operator can look at the first word after `scherzoctl` and know which durable Scherzo resource is being changed. Whole tracker-task retries use `scherzoctl task retry <issue>`, retained-run repairs use `scherzoctl run ... <run-id>`, and publication retries use `scherzoctl publication retry <run-id>`. The visible recovery outcome is less ambiguity during incidents: a fresh workflow restart, an in-place step repair, an output recollection, a manual run finalization, and a publication retry are separate commands with separate safety promises.

The canonical and only spelling for workflow/issue drift recovery is `--start-fresh`. Every JSON and human response for that mode must say that Scherzo starts a fresh run from the current issue payload and current workflow definition rather than repairing the previous run.

## Problem Framing and Constraints

The current control surface grew around incidents instead of a grammar. In the current tree, `scripts/scherzoctl` is a development wrapper that runs `gleam run -- ctl`; `src/scherzo/main.gleam` routes `ctl` to `src/scherzo/ctl.gleam`; `src/scherzo/ctl/command_registry.gleam` parses command paths such as top-level `retry`, top-level `retry-step`, top-level `recollect-outputs`, and nested `artifact publication retry`; `src/scherzo/control/command.gleam` and `src/scherzo/control/protocol.gleam` carry operator commands; `src/scherzo/orchestrator/transitions/operator.gleam` handles normal whole-issue retry; and `src/scherzo/orchestrator/daemon.gleam` owns queued retry-step, recollect-output, and publication-retry shell work.

That shape is hard to operate because command names do not reveal whether the operator is starting over, preserving a retained run, reconstructing outputs, or replaying publication. The solution must not weaken retry-step's fail-closed artifact checks, must not let fresh retry bypass active/running issue checks, missing issue checks, invalid current workflow config, or workspace setup failures, and must not delete retained workspaces or artifacts during run-level recovery. This issue is an ExecPlan authoring task only; implementation belongs to follow-up work.

## Strategy Overview

Use the existing parser, protocol, and daemon shell infrastructure, but make the public recovery grammar resource-first:

    scherzoctl task retry <issue|id:<id>> [--start-fresh --reason <text>] [--json]
    scherzoctl run retry-step <run-id> --step <step-id> [--json]
    scherzoctl run recollect-outputs <run-id> [--json]
    scherzoctl run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason <text> (--dry-run|--yes) [--json]
    scherzoctl publication retry <run-id> [--publication <publication-id>] [--json]

The daemon-backed `scherzoctl` commands do not take `--root`; they use the control file and let the running daemon own retained-state mutation. Offline retained-state commands stay under top-level `scherzo ... --root <workspace-root>` and keep the instance-lock guard, so operators do not accidentally race a live daemon. JSON responses keep the current control envelope with `version`, `id`, `ok`, daemon `target`, and `data`; each command's `data` must contain `command`, `status`, `target`, optional `operation_id`, optional `reason`, and a `message` that names the recovery mode. Fresh retry messages must include the phrase `starts a fresh run`.

## Alternatives Considered

The smallest alternative is to add a fresh-run option to the existing top-level `retry` command. That would solve LIV-1330 narrowly but preserve the confusing distinction between top-level retry, top-level retry-step, top-level recollect-outputs, and nested publication retry. Another alternative is a complete internal module move away from `src/scherzo/ctl/*`; that is larger than needed because the operator problem is the public grammar, not the source-directory name. A third alternative is making a terse imperative option canonical, but that wording can imply bypassing safety checks, so `--start-fresh` is safer and clearer.

## Risks and Countermeasures

The main risk is that resource-first aliases accidentally change recovery semantics while parser tests still pass. The countermeasure is daemon-level tests that prove normal retry is unchanged, fresh retry is the only path that clears drift-parked state, retry-step still preserves upstream artifacts, recollection still starts no workers, publication retry still uses already-materialized outputs, and run finalize never starts a new workflow.

A second risk is racing retained-state mutation while a daemon is running. The countermeasure is a hard CLI boundary: `scherzoctl` recovery commands are daemon-backed and rootless, while top-level offline `scherzo artifact publication ... --root` remains lock-protected break-glass behavior.

A third risk is making manual `run finalize` too permissive. The countermeasure is dry-run-first planning, `--yes` for mutation, explicit `--reason`, rejection for active or unknown runs, validation adoption before output/tracker effects, duplicate-operation detection, and idempotent completion records using existing ledger record families where possible.

## Scope Boundaries

In scope are parser and help changes for `task retry`, `run retry-step`, `run recollect-outputs`, `run finalize`, and `publication retry`; control-command/protocol additions for fresh retry and run finalize; daemon routing for every resource-first command; JSON and human output updates; documentation and runbook migration; and tests for both success and rejection paths. Existing `task list`, `task show`, daemon inspection commands, pause/resume/reload, park/unpark, abort, stop-after-turn, prompt, UI response, schedule controls, and offline top-level retained-state commands remain outside this grammar redesign except for help text that prevents confusion. The implementation must also inventory checked-in docs, scripts, wrapper helpers, and workflow helper prompts for old recovery spellings, update operator-facing matches, and record a no-change rationale for unrelated matches.

Workflow provider-live integrations, remote-provider-cache or TTL behavior, workflow schemas, and provider-facing structured-output materialization helpers are out of scope unless a current grep proves one of those helpers invokes or documents these recovery commands. If implementation unexpectedly touches one of those surfaces, the implementer must record the discovery in this plan, add targeted tests for the touched surface, and keep the provider-live/cache behavior unchanged unless a new reviewed plan says otherwise.

The old `scherzoctl retry`, `scherzoctl retry-step`, and `scherzoctl recollect-outputs` spellings should become hidden deprecated aliases for one release, print or return replacement guidance, and then be removed. The existing offline `scherzo artifact publication retry --root ...` remains a daemon-stopped break-glass command; the new daemon-backed operator command is `scherzoctl publication retry <run-id> ...`.

## Milestones

Milestone 1 establishes the resource-first parser and help surface. At the end, parser tests show the canonical command paths, target forms, option conflicts, required `--reason` for fresh retry and finalize, required `--step` for run retry-step, and hidden legacy alias guidance.

Milestone 2 updates the control protocol and daemon routing without changing recovery behavior. At the end, `task retry` normal mode maps to the existing guarded retry semantics, `run retry-step` and `run recollect-outputs` map to the existing queued shell paths, and `publication retry` reuses daemon-side publication retry instead of the offline `--root` handler.

Milestone 3 implements start-fresh retry for drift recovery. At the end, normal guarded retry still rejects drift-parked issues, while `task retry <issue> --start-fresh --reason "workflow drift"` clears only the recovery/park state needed to queue a new run from current tracker and workflow data, preserves retained artifacts, and rejects active, missing, terminal, invalid-config, or unavailable-workspace cases.

Milestone 4 adds `run finalize`. At the end, dry-run reports the validation, output, publication, tracker-update, and ledger effects that would be adopted; `--yes` records auditable validation adoption, output materialization or adoption through `--outputs auto`, publication attempts when requested, tracker success update when requested, operation provenance, and terminal run completion without starting a new workflow.

Milestone 5 completes docs, helper inventory, alias retirement guidance, full validation, and rollout evidence. At the end, runbooks and getting-started examples teach the resource-first grammar, grep evidence covers `docs/`, `scripts/`, `.scherzo/workflows/scripts/`, `workflows/dogfood/prompts/`, and `workflows/dogfood/scripts/` for old recovery spellings, old aliases are covered by deprecation or rejection tests, provider-live/cache behavior is proven unchanged by no-touch inventory or targeted tests, targeted tests and full repository gates pass, and any live-daemon dogfood evidence is either captured against a safe fixture daemon or explicitly deferred to a human/operator after implementation.

## Progress

- [x] (2026-07-01) Read the repository-local ExecPlan workflow guidance and the prepared review-doc target.
- [x] (2026-07-01) Inspected the current `scherzoctl` wrapper, main routing, control parser, command registry, command/protocol DTOs, daemon retry-step/recollect/publication handlers, offline publication retry lock guard, and relevant tests/docs.
- [x] (2026-07-01) Wrote this concise review document for `docs/plans/` and prepared the structured implementation pack for follow-up implementation.
- [x] (2026-07-01) Incorporated review feedback by making docs/helper inventory, provider-live/cache no-change evidence, full validation, lint gates, and manual dogfood timing explicit in the review document and updated implementation pack.
- [x] (2026-07-01) Revised the plan to make `--start-fresh` the only fresh-run retry spelling and removed the previously proposed hidden compatibility alias.

## Surprises & Discoveries

The current tree already has a split between daemon and offline publication retry. Evidence: `src/scherzo/orchestrator/daemon.gleam` has queued daemon-side artifact publication retry, while `src/scherzo/ctl.gleam` currently requires `artifact publication retry` to pass `--root` and acquire `src/scherzo/instance_lock.gleam` before running the offline retry handler.

The parser is already registry-driven, so resource-first paths can be added without replacing the whole CLI. Evidence: `src/scherzo/ctl/command_registry.gleam` declares multi-segment paths such as `task show` and `artifact publication retry`, and `src/scherzo/ctl/command_spec.gleam` selects the longest matching path.

Run-level queued recovery work already has the right durable shape for reuse. Evidence: `retry-step`, `recollect-outputs`, and artifact-publication retry append `ControlOperationQueued` records, return `status: queued` with `operation_id`, and are later driven by daemon queued-operation replay.

## Decision Log

- Decision: Make `--start-fresh` the only fresh-run retry option and drop the previously proposed hidden compatibility alias. Rationale: operators need a name that describes the effect, no released operator contract depends on the alias, and avoiding the alias keeps parser, help, and tests simpler. Date: 2026-07-01.
- Decision: Use bare run ids for new `run` and `publication` targets. Rationale: the resource word already says the target kind, so `run:<run-id>` is redundant in the canonical grammar. Date: 2026-07-01.
- Decision: Keep daemon-backed recovery under `scherzoctl` and offline retained-state mutation under top-level `scherzo ... --root`. Rationale: this avoids instance-lock surprises and matches the current post-LIV-1271 CLI split. Date: 2026-07-01.
- Decision: Prefer existing ledger record families for fresh retry and manual finalization audit trails. Rationale: avoiding new durable record variants keeps rollback safer while still allowing auditable `ControlOperation`, output, publication, tracker, supersession, diagnostic, provenance-repair, and finish records. Date: 2026-07-01.
- Decision: Treat docs/helper migration and provider-live/cache behavior as explicit acceptance evidence rather than implied non-goals. Rationale: the command grammar is operator-facing, so checked-in examples must not keep teaching retired spellings, while provider-live registration, cache, TTL, and structured-output helper behavior should remain unchanged and must be proven by inventory or tests. Date: 2026-07-01.

## Outcomes & Retrospective

No implementation has been performed in this ticket. The expected outcome of the follow-up implementation is a resource-first operator CLI with clearer recovery semantics, explicit fresh-run messaging, and a manual retained-run finalization path that can be reviewed and tested before operators rely on it during incidents.

## Validation and Acceptance

Pre-publish automated evidence must include targeted parser/help tests in `test/ctl_test.gleam` and `test/ctl_command_spec_test.gleam` proving every canonical command shape, option conflict, missing-option error, and legacy command alias warning or rejection.

Protocol and JSON evidence must include `test/control_command_test.gleam`, `test/control_protocol_test.gleam`, and CLI output assertions showing the existing JSON envelope, daemon target context, command result fields, queued `operation_id` where applicable, and fresh retry messages containing `starts a fresh run`.

Behavior evidence must include daemon tests proving normal retry behavior is unchanged, normal retry rejects a drift-parked issue, start-fresh retry from a drift-parked issue queues a new run from current issue/workflow state, absolute safety checks still reject, retry-step and recollect-output run-level guarantees are unchanged, publication retry uses already-materialized outputs, and `run finalize` dry-run, success, duplicate/idempotent, active-run, missing-run, validation-failure, publication-failure, and tracker-update-failure cases are covered.

Documentation and helper evidence must include updated `docs/GETTING_STARTED.md`, `docs/runbooks/workflow-recovery.md`, `docs/runbooks/artifact-store.md`, and any affected help/usage tests so checked-in examples no longer teach the old recovery grammar. It must also include a grep inventory for `docs/`, `scripts/`, `.scherzo/workflows/scripts/`, `workflows/dogfood/prompts/`, and `workflows/dogfood/scripts/` proving no Scherzo helper still emits or documents retired operator spellings, or a recorded no-change rationale for unrelated matches.

Provider-live/cache acceptance is intentionally a no-change check: because this plan changes local operator recovery commands, provider-live registration/probes, remote-provider-cache, TTL, and token-cache behavior must remain unchanged. The implementer must prove this with a no-touch inventory, or with targeted tests and a Decision Log entry if a touch becomes necessary. Full validation evidence before publish is: `direnv exec . gleam format --check src test`, relevant targeted `direnv exec . gleam test ...` commands, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. Live daemon dogfood against a real retained run is useful but deferred human/operator evidence unless a safe fixture daemon and retained run are available during implementation.

## Rollout, Recovery, and Idempotence

Rollout is a single operator-CLI release with one-release hidden aliases for `retry`, `retry-step`, and `recollect-outputs`. New commands are canonical immediately. No retained workspace, artifact, provider-live, remote-provider-cache, workflow-schema, or structured-output helper migration is required, and the implementation should avoid new ledger record variants unless a later discovery proves that existing records cannot express the audit trail.

Rollback is to revert parser, command/protocol, daemon routing, and documentation changes. Runs already started by `task retry --start-fresh` remain normal runs, and finalized runs remain represented by existing output, publication, tracker, diagnostic, operation, and finish records. Dry-run commands are repeatable. Mutating run-level commands must be idempotent: repeated queued operations return the existing `operation_id` or an already-finalized/no-op result, and repeated fresh retry is rejected once a new run is active or pending.

## Open Questions and Clarifications Needed

No open questions. The plan assumes `--start-fresh` is the sole LIV-1330 fresh-run retry spelling, and live daemon dogfood may be deferred when no safe retained run exists during implementation.
