# Tracker adapters operator runbook

This runbook is operator guidance for configuring and checking tracker adapters. The normative implementation contract lives in the [Tracker Adapter Specification](../specs/TRACKER_ADAPTER_SPEC.md); use that spec for required data model, capability, startup-validation, idempotency, and recovery semantics.

Scherzo uses **task** as the backend-neutral word for a unit of work from an external task system. A Linear issue is the production task type today. A **tracker adapter** is the Scherzo boundary that reads tasks and performs optional task-system side effects such as comments, state transitions, and scheduled failure publication. The historical `remote_commands` adapter surface is not a production operator-control path.

Linear remains the only production adapter in this repository. Jira and Trello are follow-up backends, not supported runtime choices. The `test-memory` adapter is a test fixture used to prove the adapter contract without importing Linear code.

## Operator-facing names

Prefer the backend-neutral names in new docs, prompts, and scripts:

- Use **task** for work selected by Scherzo.
- Use **tracker adapter** for the integration with Linear or a future task system.
- Use `tracker-smoke`, `tracker-contract`, `--tracker-smoke`, and `--tracker-contract-check` in operator instructions.

The old Linear-named smoke and contract operator aliases are retired:

- `linear-smoke`, `--linear-smoke`, `linear-contract`, and `--linear-contract-check` are no longer accepted operator paths. Use `tracker-smoke`, `--tracker-smoke`, `tracker-contract`, and `--tracker-contract-check` instead.
- `tracker.linear.check_setup` is the current Linear board validation switch. The old `linear_contract` section is replaced by fields derived from `tracker`, `workflows`, `task_routing`, and `task_updates`; leaving it in config is a startup validation error with migration guidance. `linear_commands` and `remote_commands` are also removed command-transport settings; leaving either section in config is a startup validation error, and operators should use `scherzoctl` instead. Keep the removed-key diagnostics until supported configs no longer need migration guidance for those names.
- `issue.*` prompt variables, `SCHERZO_ISSUE_*`, `issue_id`, `issue_identifier`, issue-shaped ledger fields, and `linear_command_*` ledger/event/outbox records remain legacy-reader compatibility surfaces until the runtime task context and command history are fully migrated. Retirement gate: do not remove them before dual-read prompt, helper, retained-ledger, and legacy command-record tests prove task-native replacements preserve old artifacts.
- `--linear-attach-comment-file`, `.scherzo/workflows/scripts/scherzo-execplan`, and `.scherzo/workflows/scripts/scherzo-merge-conflict` are Linear-only because they create, update, or inspect Linear tasks directly through Linear issues today. Retirement gate: keep these names until adapter-backed task-context fetch/publish helpers exist and the old Linear-only flows have replacement tests.

## Preferred Linear tracker config

New examples should use the simplified tracker shape. Keep the API key in the environment, name the Linear project under `tracker.linear.project`, and use `tracker.states.ready` for initial dispatch states.

```yaml
version: 1

tracker:
  linear:
    project: YOUR_LINEAR_PROJECT_SLUG
    api_key_env: LINEAR_API_KEY
    endpoint: https://api.linear.app/graphql
    check_setup: true
  states:
    ready: [Todo]
    active: [Todo, In Progress]
    terminal: [Done, Canceled, Cancelled, Duplicate]
  polling:
    every: 30s

workflows:
  research: workflows/research.yaml
```

Older tracker fields such as `tracker.kind`, `tracker.credentials.api_key_env`, `tracker.linear.project_slug`, `tracker.dispatch_states`, and `polling.interval_ms` belong to the pre-simplified config shape. Migrate them with [simplified YAML migration](simplified-yaml-migration.md).

Use the backend-neutral doctor check names:

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
| Linear | Production | Yes | Yes | No | Yes | Yes | No adapter capability | Yes | Yes | Compatibility path | Yes | No adapter capability | Linear is the only production backend. Contract/readiness checks use `tracker.linear.check_setup`; inbound Linear command comments are removed; attachment upload is still exposed through the Linear-only comment-file helper rather than generic `attachments`. |
| Jira follow-up | Future | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Requires a future production adapter plan and live backend design. Do not claim support from the current architecture alone. |
| Trello follow-up | Future | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Unknown | Requires a future production adapter plan and live backend design. Do not claim support from the current architecture alone. |
| test-memory | Test fixture | Yes | Yes | No by default | Yes | Yes | No | No | Yes | No | No | No | Test-only fake adapter for adapter contract and non-Linear seam tests. Do not use it in production examples. |

Capability names used in code and config diagnostics include `task_source`, `comments`, `remote_commands`, `state_transitions`, `routing_metadata`, `links`, `handoff`, `scheduled_failures`, `readiness`, `smoke`, and `attachments`.

## Remaining Linear coupling and rationale

Linear-specific modules are expected inside Linear adapter internals, Linear compatibility tests, and Linear setup docs. The current tree also keeps a few generic-looking entrypoints on Linear compatibility paths:

- `src/scherzo/orchestrator/service.gleam` still imports Linear contract, smoke, attachment, and transport modules for the current Linear-backed doctor and CLI checks.
- `src/scherzo/template.gleam` still exposes `issue.*` variables only; prompts can describe the source as a task while rendering through the compatibility variables.
- `.scherzo/workflows/scripts/scherzo-implementation` currently fetches workflow source context from Linear, so its fetch errors and fixture helper remain Linear-specific even when its operator summaries say task.

These are compatibility surfaces, not new backend contracts. Future Jira or Trello implementation should add production adapters and then move any remaining generic service paths behind adapter capabilities before advertising support.

## Adapter readiness checklist

Before enabling a new production adapter, verify these facts with tests and operator docs:

1. Candidate task reads, task refresh, and operator lookup are implemented through `task_source`.
2. Every enabled feature has a startup capability validation error when the adapter does not support it.
3. Task updates and scheduled failure publication are either implemented through capabilities or disabled in config; remote command ingestion remains disabled and must not be advertised as an operator control path.
4. Readiness and smoke checks use backend-neutral operator names; do not add backend-specific aliases unless a separate compatibility plan accepts them.
5. Prompt examples use task language while explicitly documenting any remaining `issue.*` compatibility variables.

## Black-box conformance MVP

The repository now includes a black-box tracker adapter conformance runner for external adapters that expose the MVP CLI driver protocol described in `docs/specs/TRACKER_CONFORMANCE_PROTOCOL.md`. Adapter authors who need a task-oriented walkthrough should start with `docs/runbooks/tracker-adapter-author-guide.md` and then return here for operator context.

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

## Linear conformance dogfood preparation

The first Linear dogfood profile stays read-only and manual-first. Do not point tracker conformance at the normal Scherzo production project, do not use real operator issue text as fixtures, and do not use `LINEAR_API_KEY` for dogfood runs. Live Linear dogfood must use a dedicated fixture workspace or a restricted fixture-only project, synthetic fixture issues labeled and titled for conformance, and a dedicated `SCHERZO_LINEAR_CONFORMANCE_API_KEY` credential that is kept out of the repository and out of retained reports.

Use explicit `fixtures.tasks` declarations for Linear dogfood manifests so the operator-reviewed fixture issues are pinned by durable Linear id and identifier before any run starts. The repository now carries two preparation manifests:

- `test/fixtures/tracker_conformance/linear-task-source.template.manifest.json` documents the live `task_source` manifest shape an operator will fill with approved fixture ids, project slugs, and redaction markers.
- `test/fixtures/tracker_conformance/linear-task-source-offline.manifest.json` documents the deterministic offline manifest shape that later fake-transport tests and pre-publish dogfood evidence will use.

The offline manifest is now runnable without a live Linear credential through the dedicated wrapper and fake transport:

```sh
env -u LINEAR_API_KEY SCHERZO_LINEAR_CONFORMANCE_API_KEY=fake-linear-token direnv exec . gleam run -- tracker-conformance run test/fixtures/tracker_conformance/linear-task-source-offline.manifest.json --report test/tmp/tracker-conformance/linear-task-source-offline.report.json
```

The operator-facing wrapper keeps live execution behind explicit approval and preflight:

```sh
env -u LINEAR_API_KEY SCHERZO_LINEAR_CONFORMANCE_API_KEY=<fixture-bot-token> scripts/scherzo-linear-conformance run --manifest test/fixtures/tracker_conformance/linear-task-source.template.manifest.json --run-id <operator-approved-run-id>
```

Before sharing any Linear dogfood evidence, keep the report under `tmp/tracker-conformance/linear/<run-id>/`, run a redaction check for the configured token and fixture-secret markers, and confirm the summary names only synthetic fixture tasks. Live `task_source` evidence must record the manifest path, run id, driver command, report path, redaction-check output, fixture project identifier, and explicit confirmation that no non-fixture Linear issue was read or mutated.

Comments, state transitions, handoff, scheduled failures, and any other write-capable pack stay disabled for live Linear dogfood until fake cleanup and recovery tests exist and a human reviews the fixture project again. `remote_commands` remains disabled for Linear.

Helper/cache inventory for LIV-756: this dogfood path changes operator docs, tracker-conformance fixture manifests, the dedicated wrapper script, and offline/live Linear driver wiring only. `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, and provider-live/cache behavior remain unchanged, so no helper migration or provider-live/cache validation was applicable.

Helper/cache inventory for LIV-565: this scheduled-failure conformance pack changes only tracker-conformance modules, fake-driver fixtures, and operator docs. `.scherzo/workflows/scripts/*`, workflow schemas, provider-facing structured-output helpers, review-lane contract files, and provider-live/cache behavior remain unchanged for this ticket, so no helper migration or provider-live/cache validation was applicable.
