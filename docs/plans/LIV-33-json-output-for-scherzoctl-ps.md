# Add JSON output for `scherzoctl ps`

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Operators currently use `scripts/scherzoctl ps` to see daemon sessions in a tab-separated human table. That table is useful at a terminal, but it is awkward and brittle for scripts because automation has to scrape column positions and status strings from output intended for people. After this change, an operator or local automation can run `scripts/scherzoctl ps --json` and receive one deterministic JSON document describing the current daemon sessions, while `scripts/scherzoctl ps` without `--json` continues to print the exact same table it prints today.

LIV-33's visible Linear title is `ExecPlan workflow smoke test: final PR creation path`. This plan treats that issue as the workflow smoke-test container and uses this small `scherzoctl ps --json` improvement as the concrete implementation payload. The change is intentionally narrow enough to exercise the draft, review-incorporation, implementation, validation, and final PR path without changing Scherzo workflow orchestration code.

The observable outcome is simple: from the repository root, `scripts/scherzoctl ps` still starts with `SESSION\tISSUE\tSTATUS\tTURN\tLAST_EVENT`, and `scripts/scherzoctl ps --json` prints a single JSON line with a documented schema version and a `sessions` array.

## Problem Framing and Constraints

A Scherzo daemon is the local background process that owns session state and exposes a local control API. A session is one tracked run of Scherzo work for an issue, including the Linear issue identifiers, current status, turn count, workspace path, timestamps, and token totals. Today, the CLI can list sessions for a human, but scripts that need to inspect active sessions must either call lower-level protocol details or parse the table. That creates a fragile local automation surface: adding spacing, reordering columns, or adding a new human column can break scripts.

This plan solves only the `ps` listing output. It does not change daemon session storage, the local TCP control protocol, stream event JSON, `session --json`, mutating command behavior, or the `scripts/scherzoctl` shell wrapper except where help text needs to describe the new behavior. The change must be reversible and additive for `scripts/scherzoctl ps` without `--json`; the only intentional behavior change is the successful `ps --json` JSON shape. The default human-readable output must remain byte-for-byte compatible for the scenarios covered by tests.

The current tree already parses `ps --json` and routes it to a raw control protocol response. This plan deliberately changes successful `ps --json` output to a stable CLI automation document rather than documenting the raw protocol envelope as public CLI behavior. That decision is part of the smoke-test payload: it gives scripts a versioned command contract while keeping the daemon wire protocol unchanged.

## Strategy Overview

Keep the implementation in the CLI layer, primarily `src/scherzo/ctl.gleam`. The daemon already exposes a typed `list_sessions` client call through `src/scherzo/control/client.gleam`, and session summaries already have a JSON encoder in `src/scherzo/session/json.gleam`. The right-sized change is to have the `Ps(_, True)` branch call the typed session-listing path, then print a CLI-specific JSON document such as:

    {"schema_version":1,"kind":"scherzoctl_ps","sessions":[...]}

Each session object should reuse the existing summary JSON fields from `scherzo/session/json.gleam`: `session_id`, `issue_id`, `issue_identifier`, `issue_title`, `workspace_path`, `pi_session_id`, `status`, `exit_reason`, `current_turn`, `started_at_ms`, `last_event_at_ms`, and `tokens` with `input`, `output`, `cache_read`, `cache_write`, and `total`.

This approach avoids changing the control server or adding a second session data model. It gives local automation a stable CLI contract with an explicit `schema_version`, while leaving the lower-level protocol envelope available internally for other commands that already use it.

## Alternatives Considered

The smallest possible change is to keep the existing `ps --json` behavior, which prints the raw protocol response from `protocol.ListSessions`: a document with `version`, `id`, `ok`, and `data.sessions`. That is already machine-readable, but it leaks request/response protocol mechanics into the operator CLI and makes local automation depend on fields such as a request id that are irrelevant to listing sessions. It also does not clearly document a CLI-level schema, and it does not satisfy the smoke-test payload's goal of creating a stable command-level automation contract.

Another option is to move the JSON contract into `src/scherzo/control/protocol.gleam` by changing `list_sessions_data`. That would affect every protocol client, not just `scripts/scherzoctl ps --json`, and would increase compatibility risk for no extra operator value. This plan keeps the protocol unchanged.

A broader option is to migrate every non-streaming `--json` command to CLI-specific envelopes. That may be a good future cleanup, but it is larger than this smoke-test feature and risks breaking users of `ping --json`, `session --json`, or operator command JSON output. This plan limits the behavior change to `ps --json`.

## Risks and Countermeasures

The main compatibility risk is accidentally changing the default human table. Countermeasure: add a regression test that captures `ctl.run_with_deps(ctl.Ps(..., False), ...)` output for known sessions and compares the exact header and rows, including tab characters.

A second compatibility risk is existing local automation using the raw `ps --json` protocol envelope with `version`, `id`, `ok`, and `data.sessions`. This plan intentionally changes that successful output because the raw envelope is an implementation detail of the control protocol and the stable CLI contract should not expose request ids or protocol response fields. Countermeasures: before changing behavior, search the repository for `ps --json` callers and raw-envelope assumptions; if any source file or script shells out to `scripts/scherzoctl ps --json` and reads the old envelope, stop and update this plan with a coexistence or migration path. No compatibility window is planned for external users because `scripts/scherzoctl` is a repository-local development/operator tool for this smoke-test payload, not a released public API. Recovery is a small revert of the `Ps(_, True)` branch.

A third risk is making JSON output nondeterministic. Countermeasure: specify a fixed top-level field order, reuse the existing summary encoder's stable field order, and preserve the list order returned by the control client. This plan defines deterministic ordering as preserving the control client's session order. The session hub currently returns summaries in its `session_order`, not in dictionary iteration order; do not add sorting by session id, issue identifier, start time, or last event time.

A fourth risk is confusing `--json` error handling. The current CLI reports control-file and connection failures as command errors, not JSON documents. Countermeasure: keep that behavior for this change and document it in tests. JSON error envelopes are out of scope because they would affect every CLI command that accepts `--json`, not only `ps`.

A fifth risk is weakening testability by continuing to call the real client directly from `run_with_deps`. Countermeasure: extend `ctl.ControlClient` with a `list_sessions` function and make the table and JSON `ps` paths use that dependency. This is a small internal refactor with no user-visible change.

## Progress

- [x] (2026-05-01 19:30Z) Read the repository-local ExecPlan skill and drafted this plan from the workflow issue and repository context.
- [x] (2026-05-01 19:30Z) Inspected the directly relevant CLI, control, protocol, session JSON, hub ordering, and control test files named in this plan.
- [x] (2026-05-01 20:05Z) Incorporated review feedback by reconciling the LIV-33 smoke-test scope, deciding the `ps --json` CLI contract, adding compatibility and validation guidance, and naming the second affected `ControlClient` test file.
- [ ] Implement Milestone 1: make `ps` output testable through the injected control client while preserving the default table.
- [ ] Implement Milestone 2: add the stable `ps --json` CLI document and tests for populated and empty session lists.
- [ ] Implement Milestone 3: update help text, edge-case tests, and validation notes.

## Surprises & Discoveries

- Observation: `src/scherzo/ctl.gleam` already accepts `ps --json` and currently prints a raw protocol response by calling `print_raw_request` with `protocol.ListSessions("1", "")`.
  Evidence: the `Ps(control_path, json)` branch in `src/scherzo/ctl.gleam` sends `protocol.ListSessions` when `json` is `True`.

- Observation: the shell entrypoint `scripts/scherzoctl` is only a wrapper around `direnv exec "$ROOT" gleam run -- ctl "$@"`; the actual `ps` flag behavior lives in Gleam code.
  Evidence: `scripts/scherzoctl` computes `ROOT` and execs `gleam run -- ctl` with all original arguments.

- Observation: session summary JSON fields already exist in `src/scherzo/session/json.gleam`, so this feature does not need a new session serialization model.
  Evidence: `summary_to_json` emits the summary fields and nested `tokens` object listed in this plan.

## Decision Log

- Decision: Treat LIV-33's visible workflow-smoke-test title as the workflow container and this `scherzoctl ps --json` change as the bounded implementation payload.
  Rationale: The workflow needs a small real repository change that can travel through draft, review, implementation, validation, and final PR creation. A stable `ps --json` command is a narrow operator-facing improvement, and repository plans already describe `scripts/scherzoctl ps --json` as structured input for automation. This avoids changing workflow orchestration code while still exercising the final PR path.
  Date: 2026-05-01

- Decision: Replace successful `ps --json` raw protocol output with a CLI-specific versioned document in this plan.
  Rationale: The raw protocol envelope was an accidental CLI surface. The stable command contract should expose only command-relevant fields and a schema version, while the daemon protocol remains unchanged for internal clients.
  Date: 2026-05-01

- Decision: Treat repository unit tests as the required acceptance path and live-daemon checks as optional manual validation.
  Rationale: The plan can deterministically prove the CLI formatting and error behavior with fake control-client dependencies. Requiring a live daemon and session would make validation depend on local operator state and could disturb real work.
  Date: 2026-05-01

- Decision: Treat `ps --json` as a CLI automation contract with `schema_version`, `kind`, and `sessions`, rather than exposing the raw control protocol envelope.
  Rationale: Operators and scripts need a stable local command output, not protocol request metadata. This is also easier to document and version at the CLI boundary.
  Date: 2026-05-01

- Decision: Preserve the existing default `ps` table exactly and test it with exact string comparisons.
  Rationale: The smoke-test payload is only the JSON automation surface; changing the existing human-readable table would expand blast radius without adding value. Exact tests catch accidental spacing or column changes.
  Date: 2026-05-01

- Decision: Reuse `scherzo/session/json.gleam` for per-session objects.
  Rationale: The encoder is already used by the control protocol, includes the fields automation is likely to need, and avoids parallel definitions that could drift.
  Date: 2026-05-01

- Decision: Keep non-JSON error behavior unchanged for missing control files and connection failures.
  Rationale: JSON error envelopes across all CLI commands are a broader interface design. This feature can remain safe and reversible by only changing successful `ps --json` output.
  Date: 2026-05-01

- Decision: Preserve the session order returned by `client.list_sessions`.
  Rationale: This matches the current table behavior and the hub's `session_order`. Sorting would be a separate user-visible behavior change with no smoke-test value.
  Date: 2026-05-01

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

The repository is a Gleam project. The user-facing development command is `scripts/scherzoctl`, a POSIX shell wrapper that runs `gleam run -- ctl` inside the repository's direnv environment. The CLI implementation is in `src/scherzo/ctl.gleam`.

The control path works like this. `src/scherzo/control/file.gleam` discovers and reads `.scherzo/workspaces/.scherzo-state/control.json` by default, or a path from `--control-file`, or `SCHERZO_CONTROL_FILE`. That control file contains the local host, port, token, workspace root, and daemon start timestamp. `src/scherzo/control/client.gleam` uses the control file to connect to the local daemon, authenticate requests, and decode responses from `src/scherzo/control/protocol.gleam`. The control server in `src/scherzo/control/server.gleam` handles `ListSessions` by asking a session backend for summaries and encoding them with `protocol.list_sessions_data`.

A session summary is represented by `event.SessionSummary` from `src/scherzo/session/event.gleam`. The existing JSON representation is defined by `summary_to_json` in `src/scherzo/session/json.gleam`. The in-memory session hub in `src/scherzo/session/hub.gleam` keeps `session_order` and returns summaries through `summaries_in_order`, so the protocol response does not depend on dictionary iteration order.

Existing tests relevant to this feature live in `test/ctl_test.gleam`, `test/control_protocol_test.gleam`, and `test/control_server_test.gleam`. `test/ctl_test.gleam` currently covers parsing and help text for `ps --json`, but it does not yet assert `ps` runtime output.

## Preconditions and Verified Facts

The following repository facts were verified before writing this plan:

- `scripts/scherzoctl` does not parse `ps`; it delegates all arguments to `gleam run -- ctl`.
- `src/scherzo/ctl.gleam` defines `Command.Ps(control_file: Option(String), json: Bool)`.
- `src/scherzo/ctl.gleam` parses `ps --json` as `Ps(_, True)`.
- `src/scherzo/ctl.gleam` currently prints the default `ps` table with the header `SESSION\tISSUE\tSTATUS\tTURN\tLAST_EVENT` and rows containing `session_id`, `issue_identifier`, status string, `current_turn`, and `last_event_at_ms` separated by tab characters.
- `src/scherzo/ctl.gleam` currently handles `Ps(_, True)` by printing the raw control protocol response for `protocol.ListSessions`.
- `src/scherzo/ctl.gleam` defines `ControlClient` without a `list_sessions` dependency, so the current table path calls `client.list_sessions` directly.
- `test/ctl_attach_render_test.gleam` constructs `ctl.ControlClient` in its `deps` helper and in `events_pretty_uses_paginated_replay_helper_test`; both constructors must gain a harmless `list_sessions` function when the type is extended.
- `docs/plans/pi-operator-skill.md` already describes `scripts/scherzoctl ps --json` as a structured command that automation should prefer, but it does not require the raw protocol envelope shape.
- `src/scherzo/control/client.gleam` already exposes `list_sessions(control_file)` and decodes `protocol.decode_list_sessions_response`.
- `src/scherzo/control/protocol.gleam` encodes a successful protocol response as `{"version":1,"id":...,"ok":true,"data":...}` and encodes session lists under `data.sessions`.
- `src/scherzo/session/json.gleam` defines `summary_to_json`, which emits the per-session fields required by this plan.
- `src/scherzo/control/file.gleam` discovers the control file from an explicit path, then `SCHERZO_CONTROL_FILE`, then `.scherzo/workspaces/.scherzo-state/control.json`.
- This repository expects validation commands to run through direnv, for example `direnv exec . gleam test` and `direnv exec . gleam format --check src test`. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the command.

## Scope Boundaries

In scope:

- Changing `src/scherzo/ctl.gleam` so successful `ps --json` prints a documented CLI JSON document.
- Refactoring `src/scherzo/ctl.gleam` so `ps` uses a `list_sessions` function from `ControlClient`, allowing runtime output tests without a real daemon.
- Updating `test/ctl_test.gleam` with exact tests for default table compatibility, parsed populated JSON output, exact empty JSON output, and error behavior.
- Updating `test/ctl_attach_render_test.gleam` so its existing `ctl.ControlClient` constructors compile after `list_sessions` is added.
- Updating CLI usage text in `src/scherzo/ctl.gleam` to mention the `ps --json` automation schema at a practical level.

Out of scope:

- Changing `scripts/scherzoctl` behavior beyond the behavior it obtains from `src/scherzo/ctl.gleam`.
- Changing daemon session storage or the session hub.
- Changing `src/scherzo/control/server.gleam` or the wire protocol response shape in `src/scherzo/control/protocol.gleam`.
- Changing `ping --json`, `session --json`, `events --json`, `attach --json`, or mutating command JSON output.
- Adding a new repository-wide JSON error format.
- Adding filters, sorting flags, pretty-printing flags, or additional `ps` columns.
- Changing Scherzo workflow orchestration, final PR creation mechanics, jj workspace management, or the Linear workflow integration itself.

## Milestones

Milestone 1 makes the existing `ps` table behavior testable without changing user-visible output. At the end of this milestone, `ctl.ControlClient` has a `list_sessions` dependency, all existing constructors in `src/scherzo/ctl.gleam`, `test/ctl_test.gleam`, and `test/ctl_attach_render_test.gleam` compile, and `ctl.run_with_deps` can run the default `ps` command with a fake session list. The default table still prints exactly the same header and rows. This comes first because it proves the implementation can protect the compatibility requirement before changing JSON behavior.

Milestone 2 changes successful `ps --json` output from the raw protocol envelope to the new CLI document. At the end of this milestone, a populated fake session list prints one JSON line with `schema_version`, `kind`, and `sessions`, the line parses as JSON with the documented fields, the session order matches the fake client order, and an empty fake session list prints the same document with `sessions: []`. This milestone delivers the main operator value.

Milestone 3 documents and validates edge behavior. At the end of this milestone, help text mentions `ps --json`, tests show failed control access remains a command error rather than partial JSON, the repository has been checked for callers that depend on the old raw envelope, and the full test and format commands pass. This milestone keeps rollout safe and makes the behavior reviewable.

## Commit Map

After Milestone 1, commit the injectable `ps` session-listing refactor only after `direnv exec . gleam format --check src test` and `direnv exec . gleam test` succeed. A suitable message is `refactor(cli): inject ps session listing`.

After Milestone 2, commit the stable `ps --json` document and its tests after the same validation commands succeed. A suitable message is `feat(cli): add stable JSON output for scherzoctl ps`.

After Milestone 3, commit the help text, compatibility-search notes if any were added to the plan during implementation, and final validation updates after the same commands succeed. If Milestone 3 only validates with no additional source changes, record the validation in the ExecPlan's Progress section rather than creating an empty commit.

## Plan of Work

In `src/scherzo/ctl.gleam`, extend the `ControlClient` type with:

    list_sessions: fn(file.ControlFile) -> Result(List(event.SessionSummary), client.ControlError)

Update `real_control_client()` to set `list_sessions: client.list_sessions`. Update every test constructor for `ctl.ControlClient` to provide this new field. In `test/ctl_attach_render_test.gleam`, add `list_sessions: fn(_) { Ok([]) }` to both the `deps` helper and the explicit constructor inside `events_pretty_uses_paginated_replay_helper_test`; those tests do not exercise `ps`, so returning an empty list is a harmless compile-preserving fake.

For Milestone 1, change only the default-table path of the `Ps(control_path, json)` branch so `json == False` loads the control file once, calls `deps.list_sessions(control_file)`, maps client errors through `client_error`, and calls `print_sessions_table(sessions, output)` exactly as before. Leave `json == True` on the existing raw request until Milestone 2 so the table-preserving refactor can be committed independently.

For Milestone 2, add imports for `gleam/json` and `scherzo/session/json` aliased to avoid confusion with `gleam/json`, for example `import scherzo/session/json as session_json`. Then change the `json == True` branch so it also calls `deps.list_sessions(control_file)`, maps client errors through `client_error`, and calls a new helper such as `print_sessions_json(sessions, output)`.

Add a helper in `src/scherzo/ctl.gleam` with the exact top-level schema:

    fn ps_document_to_json(sessions: List(event.SessionSummary)) -> json.Json {
      json.object([
        #("schema_version", json.int(1)),
        #("kind", json.string("scherzoctl_ps")),
        #("sessions", json.array(sessions, of: session_json.summary_to_json)),
      ])
    }

Add `print_sessions_json` to convert that JSON document with `json.to_string` and send it through `output.line`. Do not pretty-print, do not emit multiple lines, and do not include control protocol fields such as `version`, `id`, `ok`, `data`, or `error` in successful output.

Update `usage()` in `src/scherzo/ctl.gleam`. Keep existing command wording intact where possible, but add a clear command line such as `ps --json                    List sessions as a stable JSON document.` and adjust the `--json` option text so it does not imply `ps --json` is raw protocol JSON. Keep the `attach --json` stream wording unchanged.

In `test/ctl_test.gleam`, add runtime tests for `ps`. Define helpers that create a valid temporary control file, construct one or two `event.SessionSummary` values, build a fake `ctl.ControlClient`, run `ctl.run_with_deps`, and capture `Output.line` calls. Use `process.new_subject()` or an equivalent local helper to collect output lines. The fake client's `list_sessions` function should return the supplied summaries; the fake client's other functions can return harmless errors if they are accidentally called so the test fails clearly.

Add JSON parsing assertions as well as string assertions. Import `gleam/json` and `gleam/dynamic/decode` in `test/ctl_test.gleam`, define a small decoder for the `ps --json` document, and parse the emitted line in the populated JSON test. The decoder should assert `schema_version`, `kind`, session order, representative session fields, status, exit reason, and nested token totals. Keep exact string assertions for the empty-list case and negative string checks that the successful output does not contain the old raw envelope fields.

The tests should cover the default table with two sessions, JSON output with the same sessions, JSON output with an empty list, and connection failure behavior. If the fake `list_sessions` returns `Error(client.ConnectionFailed("closed"))`, `ctl.run_with_deps(ctl.Ps(Some(path), True), fake_client, output)` should return `Error(ctl.Failed("connection_failed", "closed"))` and should not emit a JSON line.

Do not change `src/scherzo/control/protocol.gleam`, `src/scherzo/control/server.gleam`, `src/scherzo/session/json.gleam`, or `scripts/scherzoctl` unless implementation discovers a small compile-only adjustment is required by the `ControlClient` refactor. If a broader change appears necessary, stop and update this ExecPlan before implementing it.

## Concrete Steps

Work from the repository root.

1. Inspect the current working tree:

       jj status --color=never

   Expect either no changes or only changes intentionally made for this implementation. Do not continue over unrelated edits without first understanding them.

2. If direnv is blocked, inspect `.envrc`, run `direnv allow .`, and then continue to use `direnv exec .` for validation commands.

3. Before changing behavior, inspect repository usage of the existing flag and raw envelope:

       grep -R "ps --json" docs src test scripts .pi --exclude-dir=build --exclude-dir=.git
       grep -R -E --include='*.gleam' --include='*.md' --include='*.sh' 'data\.sessions|"ok":true|"id":"1"' src test scripts docs

   Matches in this plan, general protocol tests, or documentation that merely says to prefer `ps --json` are acceptable. If a source file or script shells out to `scripts/scherzoctl ps --json` and reads `version`, `id`, `ok`, or `data.sessions`, stop and update this plan with a compatibility path before continuing.

4. In `src/scherzo/ctl.gleam`, extend `ControlClient` with `list_sessions` as specified in the Plan of Work.

5. In `src/scherzo/ctl.gleam`, update `real_control_client()` to include `list_sessions: client.list_sessions`.

6. In `test/ctl_attach_render_test.gleam`, add `list_sessions: fn(_) { Ok([]) }` to the `ctl.ControlClient` returned by the `deps` helper.

7. In `test/ctl_attach_render_test.gleam`, add `list_sessions: fn(_) { Ok([]) }` to the `ctl.ControlClient` constructed inside `events_pretty_uses_paginated_replay_helper_test`.

8. In `src/scherzo/ctl.gleam`, change the `Ps(control_path, json)` branch so the `json == False` path loads the control file once, calls `deps.list_sessions(control_file)`, maps client errors through `client_error`, and calls `print_sessions_table(sessions, output)`. Keep the `json == True` path on the existing raw request for this milestone.

9. In `test/ctl_test.gleam`, add imports and helper functions for a valid control file, fake session summaries, a fake `ControlClient`, and output capture. The fake `ControlClient` must include the new `list_sessions` field. Its non-`ps` functions should return clear harmless errors if accidentally called.

10. In `test/ctl_test.gleam`, add a test named `ps_default_table_output_is_unchanged_test`. It should run `ctl.Ps(Some(path), False)` with two summaries and assert the exact lines:

       SESSION\tISSUE\tSTATUS\tTURN\tLAST_EVENT
       session-a\tABC-1\trunning\t2\t200
       session-b\tABC-2\texited\t3\t400

11. Run validation for Milestone 1:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   If formatting fails because files need formatting, run `direnv exec . gleam format src test` and rerun the check. The tests must pass before committing the refactor.

12. Commit Milestone 1 with a message like:

       refactor(cli): inject ps session listing

13. In `src/scherzo/ctl.gleam`, add the `gleam/json` and `scherzo/session/json as session_json` imports.

14. In `src/scherzo/ctl.gleam`, change the `Ps(control_path, json)` branch so the `json == True` path uses the same `deps.list_sessions(control_file)` result and client-error mapping as the table path.

15. In `src/scherzo/ctl.gleam`, add `ps_document_to_json` and `print_sessions_json` near `print_sessions_table`, keeping `print_sessions_table` unchanged. `print_sessions_json` must call `output.line(json.to_string(ps_document_to_json(sessions)))` exactly once.

16. In `test/ctl_test.gleam`, import `gleam/json` and `gleam/dynamic/decode`, then add small decoder types for the `ps --json` document and representative session fields. The document decoder should read `schema_version`, `kind`, and `sessions`; the session decoder should read at least `session_id`, `issue_identifier`, `status`, `exit_reason`, `current_turn`, and `tokens.total`.

17. In `test/ctl_test.gleam`, add `ps_json_outputs_stable_document_test`. It should run `ctl.Ps(Some(path), True)` with the same two summaries, assert exactly one output line, parse that line with the decoder from Step 16, and assert `schema_version == 1`, `kind == "scherzoctl_ps"`, session ids are `session-a` then `session-b`, the running session has `status == "running"` and `exit_reason == None`, the exited session has `status == "exited"` and `exit_reason == Some("completed")`, and representative token totals match the fake summaries. Also assert the line starts with:

       {"schema_version":1,"kind":"scherzoctl_ps","sessions":[

   and does not contain `"ok":true`, `"data":`, or `"id":"1"` from the raw protocol envelope.

18. In `test/ctl_test.gleam`, add `ps_json_empty_sessions_test`. It should run `ctl.Ps(Some(path), True)` with `[]` and assert the exact output line:

       {"schema_version":1,"kind":"scherzoctl_ps","sessions":[]}

19. In `test/ctl_test.gleam`, add `ps_json_connection_failure_returns_error_without_output_test`. It should run `ctl.Ps(Some(path), True)` with a fake `list_sessions` returning `client.ConnectionFailed("closed")`, assert the returned error is `ctl.Failed("connection_failed", "closed")`, and assert no output lines were emitted.

20. Run validation for Milestone 2:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   If formatting fails because files need formatting, run `direnv exec . gleam format src test` and rerun the check. The tests must pass before committing the JSON behavior.

21. Commit Milestone 2 with a message like:

       feat(cli): add stable JSON output for scherzoctl ps

22. In `src/scherzo/ctl.gleam`, update `usage()` so `ps --json` is discoverable and the option text accurately distinguishes the stable `ps` document from protocol JSON used by other non-streaming commands.

23. Update `usage_mentions_commands_and_options_test` in `test/ctl_test.gleam` so it checks for `ps --json` and still checks for `--json` generally.

24. Run final validation:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect both commands to succeed. The exact number of tests may drift as the repository changes; success is the test runner completing with no failures.

25. Inspect the final diff:

       jj status --color=never
       jj diff --color=never

   Confirm the implementation changes only `src/scherzo/ctl.gleam`, `test/ctl_test.gleam`, `test/ctl_attach_render_test.gleam`, and this plan file unless this ExecPlan was updated to justify any additional files.

26. Commit Milestone 3 if Step 22 or Step 23 created changes not already included in Milestone 2. A suitable message is:

       docs(cli): document ps json output

## Testing and Falsifiability

This plan is falsified if `scripts/scherzoctl ps` without `--json` changes its tested table output, if `ps --json` emits the old raw protocol envelope, if empty sessions do not produce an empty `sessions` array, if populated JSON cannot be parsed as a single JSON document, or if a failed control-client request emits a partial success JSON line.

The primary tests belong in `test/ctl_test.gleam` because the behavior is CLI-layer formatting, not daemon protocol behavior. The fake session summaries should include at least one running session and one exited session so the test covers `status` and `exit_reason`. Use deterministic timestamps and token totals. For the exited session, use `event.Exited("completed")` and assert that the parsed JSON has `status == "exited"` and `exit_reason == Some("completed")`; for the running session, assert `status == "running"` and `exit_reason == None`.

The default table compatibility test should assert exact output lines, including tab characters. This catches accidental conversion to spaces, reordered columns, renamed headers, or additional default columns.

The populated JSON test should both parse and inspect the emitted line. Use `json.parse(line, decoder)` with decoders from `gleam/dynamic/decode`. The decoder should read the top-level `schema_version`, `kind`, and `sessions`, and representative nested fields from each session including `session_id`, `issue_identifier`, `status`, `exit_reason`, `current_turn`, and `tokens.total`. Assert the parsed session id list is exactly `["session-a", "session-b"]` so order is covered by structured data, not only by substrings.

The JSON behavior tests should assert all of the following:

- The command returns `Ok(Nil)` for successful fake client responses.
- Exactly one line is emitted for `ps --json`.
- The parsed top-level JSON contains `schema_version: 1`, `kind: "scherzoctl_ps"`, and `sessions`.
- The `sessions` list preserves the order returned by the fake client.
- Each session contains `session_id`, `issue_id`, `issue_identifier`, `issue_title`, `workspace_path`, `pi_session_id`, `status`, `exit_reason`, `current_turn`, `started_at_ms`, `last_event_at_ms`, and `tokens`.
- The nested `tokens` object contains `input`, `output`, `cache_read`, `cache_write`, and `total`; at minimum, the decoder must assert `tokens.total` for both fake sessions.
- Successful JSON output does not contain raw protocol envelope fields `"ok":true`, `"data":`, or request `"id":"1"`.
- An empty session list produces the exact line `{"schema_version":1,"kind":"scherzoctl_ps","sessions":[]}`.

The error behavior test should assert that a client connection failure returns `ctl.Failed("connection_failed", "closed")` and emits no output. A missing explicit control file can be covered if the test helper can exercise it without depending on local daemon state; in that case, assert a `control_file_read_failed` or `control_file_not_found` error according to the path used and assert no output.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Both commands must succeed before the implementation is accepted.

## Validation and Acceptance

Acceptance is behavior-based and the required acceptance path is the repository test suite, not a live daemon. From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Both commands must succeed. The tests must prove that `ctl.Ps(Some(path), False)` still emits a tab-separated table whose first line is exactly:

    SESSION	ISSUE	STATUS	TURN	LAST_EVENT

The tests must also prove that `ctl.Ps(Some(path), True)` prints one JSON line shaped like:

    {"schema_version":1,"kind":"scherzoctl_ps","sessions":[{"session_id":"...","issue_id":"...","issue_identifier":"...","issue_title":"...","workspace_path":"...","pi_session_id":null,"status":"running","exit_reason":null,"current_turn":1,"started_at_ms":...,"last_event_at_ms":...,"tokens":{"input":0,"output":0,"cache_read":0,"cache_write":0,"total":0}}]}

The exact fake values are defined by `test/ctl_test.gleam`, but the field names, top-level shape, one-line output, parsed session order, and absence of protocol envelope fields must be stable. The tests must prove that an empty successful session list prints exactly:

    {"schema_version":1,"kind":"scherzoctl_ps","sessions":[]}

If no daemon is running or the control file cannot be read, the command exits through the existing CLI error path. It must not print a partial JSON success document.

Optional manual validation can be done only against a disposable or already-intended local daemon session. Do not start, stop, or mutate a real user daemon just to satisfy this plan. If a disposable daemon is available, run `scripts/scherzoctl ps` and `scripts/scherzoctl ps --json` from the repository root and compare the output to the shapes above. If no disposable daemon is available, record that the live check was skipped and rely on the required tests.

## Open Questions and Clarifications Needed

None.

## Rollout, Recovery, and Idempotence

This is a local CLI behavior change with no data migration. Rollout is additive for the default command because `scripts/scherzoctl ps` without `--json` keeps the existing table. Successful `ps --json` does change from a raw protocol response to a CLI-specific document. The rollout decision is that no compatibility window is needed unless the repository search in the Concrete Steps discovers an internal source file or script that depends on the old envelope. If such a dependency exists, stop and revise this plan before implementation; do not silently break it.

The new top-level object has `schema_version: 1` and `kind: "scherzoctl_ps"`, so future changes can add fields without changing the meaning of existing fields. The daemon control protocol remains unchanged, so internal clients that call `client.list_sessions` or decode protocol responses are unaffected.

Recovery is straightforward. If the new JSON shape breaks users who already depended on the raw protocol envelope, revert the `Ps(_, True)` branch to `print_raw_request(control_file, protocol.ListSessions("1", ""), deps, output)` and keep the default table tests. If stakeholders need both shapes, update this plan before implementation to add a separate flag or command; do not overload `--json` with multiple incompatible shapes.

The implementation steps are idempotent in the normal source-control sense: rerunning tests, grep checks, and format commands is safe. Writing the temporary control files in tests should use paths under `test/tmp/ctl` and should overwrite or recreate those files as needed. Do not leave a daemon running as part of automated tests.

## Artifacts and Notes

Current default table printer in `src/scherzo/ctl.gleam` emits this header and row shape:

    SESSION	ISSUE	STATUS	TURN	LAST_EVENT
    <session_id>	<issue_identifier>	<status>	<current_turn>	<last_event_at_ms>

Current raw protocol success for listing sessions is shaped like this and should no longer be the successful `ps --json` CLI output after this plan is implemented:

    {"version":1,"id":"1","ok":true,"data":{"sessions":[...]}}

The desired successful CLI JSON output is shaped like this:

    {"schema_version":1,"kind":"scherzoctl_ps","sessions":[...]}

The empty-list success case is exactly:

    {"schema_version":1,"kind":"scherzoctl_ps","sessions":[]}

## Interfaces and Dependencies

No new package dependency is required. Use existing Gleam standard library modules and existing Scherzo modules.

In `src/scherzo/ctl.gleam`, the `ControlClient` type must include:

    list_sessions: fn(file.ControlFile) -> Result(List(event.SessionSummary), client.ControlError)

In `src/scherzo/ctl.gleam`, define the JSON document helper with this contract:

    fn ps_document_to_json(sessions: List(event.SessionSummary)) -> json.Json

It returns a JSON object with exactly these top-level fields in this order:

    schema_version: 1
    kind: "scherzoctl_ps"
    sessions: array of session_json.summary_to_json(summary)

In `src/scherzo/ctl.gleam`, define:

    fn print_sessions_json(sessions: List(event.SessionSummary), output: Output) -> Nil

It prints exactly one line by calling `output.line(json.to_string(ps_document_to_json(sessions)))`.

Keep using these existing functions and modules:

- `file.discover` through the existing `load_control_file` path in `src/scherzo/ctl.gleam`.
- `client.list_sessions` from `src/scherzo/control/client.gleam` for the real control client.
- `event.status_to_string` through `session_json.summary_to_json` for status strings.
- `event.exit_reason` through `session_json.summary_to_json` for `exit_reason`.
- `domain.TokenTotals` through `session_json.summary_to_json` for token totals.

Do not add new daemon protocol request types, new control-file fields, new environment variables, or new runtime services for this feature.
