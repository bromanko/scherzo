# Composable workstreams dogfood runbook

Composable workstreams are opt-in. Existing `workflow:*` labels and normal completion-state policy continue to route and report workflows as before; workstream artifacts add a durable phase ledger alongside the Linear comments.

## Dogfood phase path

The checked-in dogfood path is:

1. `workflow:execplan` emits an `exec_plan_bundle` handoff and a suggested `implement_exec_plan` next action.
2. Operators record the human review decision against the handoff input snapshot.
3. `workstream start-from-handoff` records an input bundle and queues the `execplan-implementation` phase without relying on an ad hoc Linear comment as the source of truth.
4. `workflow:execplan-implementation` consumes the ExecPlan bundle, runs implementation, native review, validation/proof, and publish, then emits a `code_change_bundle` handoff that contains review, validation, plan-completion, diff, and PR evidence.
5. `workflow:execplan-revision` can emit a superseding `exec_plan_bundle` handoff when review requests changes.

Auto-enqueue remains disabled in the checked-in playbooks. Queueing a phase records durable workstream intent and input bundles; it does not change the existing Linear state policy by itself.

## Inspect a workstream

List all workstreams, or filter by a Linear key:

```sh
direnv exec . gleam run -- workstream list
direnv exec . gleam run -- workstream list LIV-465
direnv exec . gleam run -- workstream list --json
```

Inspect one workstream by workstream id or Linear key:

```sh
direnv exec . gleam run -- workstream show linear:LIV-465
direnv exec . gleam run -- workstream show LIV-465 --json
```

Check `phases`, `handoffs`, `artifacts`, `queued_phase_runs`, `unresolved_next_actions`, `decisions`, and `warnings`. Treat snapshot warnings or hash mismatches as blockers; do not advance from a Linear comment when the retained snapshot is missing or stale.

## Record a review decision

Use the snapshot ref and sha shown on the handoff or next-action input. For an approved ExecPlan handoff:

```sh
direnv exec . gleam run -- workstream decision approve \
  linear:LIV-465 \
  implement_exec_plan \
  human_review \
  "$USER" \
  "ExecPlan approved for implementation" \
  exec_plan_bundle:<snapshot-ref>:<snapshot-sha256>
```

Use `request-changes`, `reject`, or `deviate` with the same shape when the operator chooses a different path. The decision artifact is retained and visible in `workstream show`; it does not delete the original recommendation.

## Manually advance a phase

After approval, queue implementation from the retained handoff:

```sh
direnv exec . gleam run -- workstream start-from-handoff \
  execplan-implementation \
  implement_exec_plan \
  <handoff-ref> \
  <handoff-sha256> \
  <decision-id>
```

The command writes a `scherzo.input_bundle.v1` artifact, records a queued phase run, and prints the idempotency key. Re-running the same command should report `duplicate`; a different input hash for the same action should be treated as a separate operator decision. When using the existing Linear-label dispatcher to run the downstream issue, prefer the retained `exec_plan_bundle` snapshot ref from `workstream show` as the `Bundle ref:` value; `workflow:execplan-implementation` and `workflow:execplan-revision` now resolve both `runs/...` and `workstream-artifacts/sha256/...` bundle refs.

To retry from an already retained input bundle:

```sh
direnv exec . gleam run -- workstream start-from-input-bundle \
  execplan-implementation \
  implement_exec_plan \
  <input-bundle-ref> \
  <input-bundle-sha256> \
  <decision-id>
```

## Recovery guidance

If a phase crashes before emitting a handoff, inspect the retained run first and do not invent output artifacts. If a handoff exists but a later append failed, re-run the idempotent start command from the handoff snapshot. If `.scherzo-state/` is lost, restore it from backup; otherwise start a new workstream from fresh retained artifacts and record fresh decisions. Do not infer approvals from old Linear or PR comments alone.
