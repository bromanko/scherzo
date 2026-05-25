# Tracker adapters operator runbook

This runbook is operator guidance for configuring and checking tracker adapters. The normative implementation contract lives in the [Tracker Adapter Specification](../specs/TRACKER_ADAPTER_SPEC.md); use that spec for required data model, capability, startup-validation, idempotency, and recovery semantics.

Scherzo uses **task** as the backend-neutral word for a unit of work from an external task system. A Linear issue is the production task type today. A **tracker adapter** is the Scherzo boundary that reads tasks and performs optional task-system side effects such as comments, state transitions, remote commands, and scheduled failure publication.

Linear remains the only production adapter in this repository. Jira and Trello are follow-up backends, not supported runtime choices. The `test-memory` adapter is a test fixture used to prove the adapter contract without importing Linear code.

## Operator-facing names

Prefer the backend-neutral names in new docs, prompts, and scripts:

- Use **task** for work selected by Scherzo.
- Use **tracker adapter** for the integration with Linear or a future task system.
- Use `tracker-smoke`, `tracker-contract`, `--tracker-smoke`, and `--tracker-contract-check` in operator instructions.

These Linear names remain compatibility aliases or Linear-only surfaces:

- `linear-smoke` and `--linear-smoke` are compatibility aliases for tracker smoke checks.
- `linear-contract` and `--linear-contract-check` are compatibility aliases for tracker contract checks.
- `linear_contract` and `linear_commands` are current config sections for Linear board validation and Linear comment commands.
- `issue.*` prompt variables, `SCHERZO_ISSUE_ID`, `SCHERZO_ISSUE_IDENTIFIER`, and issue-shaped ledger fields remain compatibility aliases until the runtime task context is fully migrated.
- `--linear-attach-comment-file`, `.scherzo/workflows/scripts/scherzo-execplan`, and `.scherzo/workflows/scripts/scherzo-merge-conflict` are Linear-only because they create, update, or inspect Linear tasks directly through Linear issues today.

## Preferred Linear tracker config

New examples should use the nested tracker shape. The old flat `tracker.endpoint`, `tracker.api_key`, and `tracker.project_slug` fields still parse for one migration window. When both are present, the nested fields win and Scherzo reports `legacy_tracker_field_ignored` warnings.

```yaml
tracker:
  kind: linear
  credentials:
    api_key_env: LINEAR_API_KEY
  linear:
    endpoint: https://api.linear.app/graphql
    project_slug: YOUR_LINEAR_PROJECT_SLUG
  active_states: [Todo, In Progress]
  dispatch_states: [Todo]
  terminal_states: [Done, Canceled, Cancelled, Duplicate]
```

Use the backend-neutral doctor aliases first:

```sh
LINEAR_API_KEY=lin_api_... scherzo doctor \
  --check workflow-config \
  --check tracker-contract \
  --check tracker-smoke \
  .scherzo/scherzo.yaml
```

## Capability matrix

The matrix summarizes current operator readiness. The normative capability definitions and operation contracts are in the [Tracker Adapter Specification](../specs/TRACKER_ADAPTER_SPEC.md); this runbook intentionally does not duplicate the whole contract.

| Adapter | Status | task_source | comments | remote_commands | state_transitions | routing_metadata | links | handoff | scheduled_failures | readiness | smoke | attachments | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Linear | Production | Yes | Yes | Yes | Yes | Yes | No adapter capability | Yes | Yes | Compatibility path | Yes | No adapter capability | Linear is the only production backend. Contract/readiness checks still run through `linear_contract`; attachment upload is still exposed through the Linear-only comment-file helper rather than generic `attachments`. |
| Jira follow-up | Future | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Requires a future production adapter plan and live backend design. Do not claim support from the current architecture alone. |
| Trello follow-up | Future | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Requires a future production adapter plan and live backend design. Do not claim support from the current architecture alone. |
| test-memory | Test fixture | Yes | Yes | No by default | Yes | Yes | No | No | Yes | No | No | No | Test-only fake adapter for adapter contract and non-Linear seam tests. Do not use it in production examples. |

Capability names used in code and config diagnostics include `task_source`, `comments`, `remote_commands`, `state_transitions`, `routing_metadata`, `links`, `handoff`, `scheduled_failures`, `readiness`, `smoke`, and `attachments`.

## Remaining Linear coupling and rationale

Linear-specific modules are expected inside Linear adapter internals, Linear compatibility tests, and Linear setup docs. The current tree also keeps a few generic-looking entrypoints on Linear compatibility paths:

- `src/scherzo/orchestrator/service.gleam` still imports Linear contract, smoke, attachment, and transport modules for doctor and CLI compatibility checks.
- `src/scherzo/template.gleam` still exposes `issue.*` variables only; prompts can describe the source as a task while rendering through the compatibility variables.
- `.scherzo/workflows/scripts/scherzo-implementation` currently fetches workflow source context from Linear, so its fetch errors and fixture helper remain Linear-specific even when its operator summaries say task.

These are compatibility surfaces, not new backend contracts. Future Jira or Trello implementation should add production adapters and then move any remaining generic service paths behind adapter capabilities before advertising support.

## Adapter readiness checklist

Before enabling a new production adapter, verify these facts with tests and operator docs:

1. Candidate task reads, task refresh, and operator lookup are implemented through `task_source`.
2. Every enabled feature has a startup capability validation error when the adapter does not support it.
3. Handoff, scheduled failure publication, and remote command acknowledgements are either implemented through capabilities or disabled in config.
4. Readiness and smoke checks have backend-neutral operator names and Linear aliases only where they are truly compatibility aliases.
5. Prompt examples use task language while explicitly documenting any remaining `issue.*` compatibility variables.

## Black-box conformance MVP

The repository now includes a black-box tracker adapter conformance runner for external adapters that expose the MVP CLI driver protocol described in `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`.

LIV-410 baseline note: the existing MVP already covers the CLI `task_source` path, `fixtures.task_file`, optional setup and cleanup hooks, optional probes, support-failure counters, fixture/probe/hook namespace rejection inside `profile.adapter_operations`, configured redaction, and truncated external diagnostics.

Run the local MVP suite from the repository root with:

```sh
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/task-source-pass.manifest.json --report test/tmp/tracker-conformance/task-source-pass.report.json
```

The command exits `0` only when the selected profile passes and setup, probe, and cleanup counters remain zero. Report JSON distinguishes adapter case failures from `setup_failed`, `probe_failed`, and `cleanup_failed` support-path failures, configured redaction strings are replaced with `[REDACTED]` before Scherzo writes the report or prints the CLI summary, and captured driver or hook diagnostics are truncated before reporting.

The enriched report contract is additive. Existing top-level counters remain, and a grouped `counts` object repeats them for consumers that prefer a nested shape. Each case result now includes `expected_summary`, `actual_summary`, bounded `request_transcript`, bounded `response_transcript`, and `recovery_guidance`. Hook and probe results add `recovery_guidance` so failures can be triaged as setup, backend-visibility, cleanup, or adapter defects instead of being lumped together.

Use explicit `fixtures.tasks` declarations when you already know the trusted pre-provisioned task identities a profile should exercise. Each declaration names the fixture, pins the durable task ref, records one or more operator refs, and states its purpose. The runner still loads expected payloads from `fixtures.task_file`, but refresh and known-lookup cases use the explicit declarations when present.

Deterministic fake-driver dogfood command:

```sh
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/task-source-pass.manifest.json --report test/tmp/tracker-conformance/task-source-pass.report.json
! grep -R "SECRET_TOKEN" test/tmp/tracker-conformance/task-source-pass.report.json
```

The grep check should produce no matches. That proves the fake-driver run wrote enriched evidence without leaking configured secrets into retained report artifacts.

Optional-pack fake-driver examples from the repository root:

```sh
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/comments-pass.manifest.json --report test/tmp/tracker-conformance/comments-pass.report.json
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/remote-commands-pass.manifest.json --report test/tmp/tracker-conformance/remote-commands-pass.report.json
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/state-transition-pass.manifest.json --report test/tmp/tracker-conformance/state-transition-pass.report.json
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/routing-metadata-pass.manifest.json --report test/tmp/tracker-conformance/routing-metadata-pass.report.json
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/handoff-pass.manifest.json --report test/tmp/tracker-conformance/handoff-pass.report.json
direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/scheduled-failures-pass.manifest.json --report test/tmp/tracker-conformance/scheduled-failures-pass.report.json
```

Request optional packs explicitly in `profile.requested_packs`, claim the matching granular capabilities in `profile.capabilities`, and keep `probe.*` names out of `profile.adapter_operations`. Claimed-but-unrequested optional capabilities do not run extra cases. Requested-but-unclaimed optional packs fail manifest validation before setup, probe, cleanup, or driver commands run. `remote_commands` also requires `comments.create`, and side-effect packs such as `remote_commands` and `handoff` must declare `profile.retry_behavior` so reports can classify same-event acknowledgement and same-run handoff retries as `idempotent_update_or_dedupe` or `duplicate_visible`. `handoff` manifests must also include at least one backend-visibility probe because retry classification is probe-backed. `scheduled_failures` must claim `scheduled_failures`, include `scheduled_failures.publish`, and configure at least one backend-visibility probe named with the `scheduled-failures` prefix because duplicate suppression and no-visible-task checks are probe-backed even though the public receipt remains the adapter evidence. Scheduled-failure reports retain created remote ids, retry classifications, duplicate counts, visible task counts, cleanup status, and probe status so operators can recover duplicate or cleanup-failure runs safely. Remote-command fetch events are bounded: `event_id`, `author_id`, `command_name`, `body`, and `excerpt` all fail conformance when they exceed the protocol limits. Side-effect manifests should keep setup and cleanup hooks idempotent so reruns do not leave duplicate marker data behind.

Optional live-backend checklist for operators who already have trusted manifests and pre-provisioned fixtures:

1. Keep privileged setup, probe, and cleanup hooks inside operator-reviewed manifests only.
2. Prefer explicit `fixtures.tasks` declarations over inferred fixture selection when task identities are known ahead of time.
3. Run the same `tracker-conformance run ... --report ...` command shape against the live manifest.
4. Inspect only redacted report excerpts before sharing them outside the trusted operator context.
5. Treat provider-live cache behavior as not applicable here unless a future conformance change introduces a cache layer.

Helper/cache inventory for LIV-565: this scheduled-failure conformance pack changes only tracker-conformance modules, fake-driver fixtures, and operator docs. `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, and provider-live/cache behavior remain unchanged for this ticket, so no helper migration or provider-live/cache validation was applicable.
