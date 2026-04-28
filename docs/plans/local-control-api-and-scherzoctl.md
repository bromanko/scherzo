# Add a local read-only control API and scherzoctl

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can inspect a running Scherzo daemon from another terminal without scraping stderr logs or attaching to the BEAM process. The visible proof is that, while daemon mode is running, another terminal can run `scripts/scherzoctl ps` to list active and recently exited sessions, `scripts/scherzoctl events <session-id>` to replay recent structured events, and `scripts/scherzoctl attach --raw <session-id>` to replay and then follow one session's event stream in a basic line-oriented form. The same session summaries and events are available as JSON for scripts and for a future pi operator skill.

This phase is read-only. It must not add worker abort, follow-up prompts, UI responses, pause/resume, retry, park/unpark, Linear command parsing, or any other mutating control. Those actions belong to `docs/plans/mutating-operator-controls.md` after the read-only protocol and CLI are proven.

## Problem Framing and Constraints

Scherzo now has an in-memory EventHub under `src/scherzo/session/`, and daemon mode publishes session lifecycle and pi events to it. That solves the internal data model problem, but the data is still trapped inside the running BEAM node. Operators still need a safe local command that can ask the daemon which sessions exist and what one session recently did.

The control interface must be local by default, authenticated, scriptable, and small enough to implement without a web framework. It must not expose secrets, bind to public network interfaces accidentally, or become a second scheduler. The daemon remains the owner of runtime and session state. The control server only queries the EventHub and streams read-only event lines to clients.

The current EventHub deliberately has query APIs but no subscriber fan-out API. This plan therefore implements raw attach by polling `hub.events_after` from a separate control connection process at a short interval. That is less elegant than a true subscription, but it is proportionate for the first local read-only interface, it avoids slow-subscriber mailbox growth in the EventHub, and it can be replaced by a subscriber API later without changing the user-facing `scherzoctl attach --raw` command.

## Strategy Overview

Implement a local line-delimited JSON protocol over loopback TCP. Each request is one JSON object followed by a newline. A request includes `version`, `id`, `type`, `token`, and command-specific fields. Each ordinary response is one JSON object line with `version`, `id`, `ok`, and either `data` or `error`. Streaming attach sends an initial ordinary success response, then one JSON object line per event until the client disconnects or the server closes the stream.

Bind the server to `127.0.0.1` only, choose an OS-assigned port by default, generate a random token at daemon startup, and write a private control file at `workspace.root/.scherzo-state/control.json`. The control file contains the host, chosen port, token, workspace root, and start timestamp. The daemon logs the control file path and port but never logs the token.

Add modules under `src/scherzo/control/` for protocol encoding/decoding, control-file read/write/discovery, server lifecycle, and client calls. Add one Erlang FFI module, `src/scherzo_control_ffi.erl`, for `gen_tcp` socket operations, strong random token generation, and best-effort private file permissions. Do not add a web server dependency.

Add a `ctl` client mode through the existing executable. In development, the command is `direnv exec . gleam run -- ctl ...`. Add `scripts/scherzoctl`, a small POSIX shell wrapper that invokes the same mode from the repository root.

## Alternatives Considered

One alternative is to add an HTTP server and REST endpoints immediately. That is familiar, but it likely requires another dependency or more boilerplate than a local-only control channel needs. It also invites accidental remote binding and dashboard expectations. A line-delimited JSON protocol is enough for `scherzoctl`, scripts, and a pi skill.

Another alternative is to expose the EventHub by writing session event files and having `scherzoctl` read them. The EventHub phase intentionally made event history in-memory and bounded. File polling would not provide authoritative current session status and would create a second persistence surface before its retention and secrecy rules are designed.

A third alternative is to add an EventHub `Subscribe` message before building attach. That may be useful later, especially for richer terminal rendering, but it is not necessary for the first read-only CLI. Polling `events_after` from the connection process uses the APIs that already exist, preserves EventHub simplicity, and keeps slow clients isolated from worker event publication.

A fourth alternative is to put control commands directly in Linear comments. That would blur local operator control with issue-tracker state and make authentication and intent ambiguous. Local control should be explicit, authenticated by the control file token, and independent of Linear.

## Risks and Countermeasures

The main security risk is exposing daemon state to an unintended local or network client. Countermeasure: bind only to `127.0.0.1`, reject any requested non-loopback host in this phase, require the generated token on every request, write the control file with best-effort owner-only permissions, never log the token, and test wrong-token rejection for every command family.

The main protocol risk is locking future phases into a shape that cannot support mutations or richer streams. Countermeasure: every request has a stable `type`, `id`, and `version`; every response has `id`, `ok`, and either `data` or `error`; stream messages include `stream: true`, `id`, `cursor`, `session_id`, and `event`; unknown commands return a stable `unknown_command` error instead of closing the connection.

The main reliability risk is a slow attached client blocking the daemon or EventHub. Countermeasure: each client connection runs in its own process. Streaming attach polls EventHub with request/reply timeouts, sends socket data only from the connection process, and uses socket send timeouts so a stalled client closes its own stream instead of blocking worker publication or daemon scheduling.

The main usability risk is `scherzoctl` not knowing where the daemon wrote its control file. Countermeasure: the daemon logs `control_server_started` with the control file path, `scherzoctl` accepts `--control-file <path>`, `scherzoctl` honors `SCHERZO_CONTROL_FILE`, and as a convenience it tries `.scherzo/workspaces/.scherzo-state/control.json` from the repository root if that file exists. That last guess is only a convenience for the example workflow; custom workspace roots require the explicit flag, the environment variable, or copying the logged path.

The main compatibility risk is adding a CLI mode that breaks existing `gleam run -- WORKFLOW.md` daemon startup or the existing `--once`, `--linear-smoke`, `--pi-probe`, and `--help` modes. Countermeasure: only arguments beginning with `ctl` enter the control client path. Existing mode parsing and usage must remain covered by `test/main_test.gleam`.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed this phase should follow the EventHub phase and remain read-only.
- [x] (2026-04-28 18:40Z) Read the old baseline `src/scherzo/main.gleam`, which only supported run/help before real-board-readiness CLI changes.
- [x] (2026-04-28 21:40Z) Re-reviewed the current tree after real-board-readiness and EventHub work. The tree now has daemon modes, long-lived daemon startup, `src/scherzo/session/event.gleam`, `src/scherzo/session/hub.gleam`, `src/scherzo/session/json.gleam`, and daemon EventHub publication.
- [x] (2026-04-28 21:40Z) Ran `direnv exec . gleam test`; the current baseline reports `122 passed, no failures`.
- [x] (2026-04-28 21:40Z) Revised this plan to remove stale prerequisite-normalization work, document the exact current EventHub APIs, and implement raw attach by polling `hub.events_after` rather than assuming an EventHub subscription API.
- [x] (2026-04-28 22:16Z) Added `src/scherzo/control/protocol.gleam` with request/response JSON encoding and decoders, including stable errors for malformed, unauthorized, and unknown commands.
- [x] (2026-04-28 22:16Z) Added `src/scherzo/control/file.gleam` and token/private-file helpers in `src/scherzo_control_ffi.erl` for writing, reading, discovering, and removing `control.json`.
- [x] (2026-04-28 22:16Z) Added the loopback TCP server, client, EventHub read adapter, and polling raw attach implementation in `src/scherzo/control/server.gleam` and `src/scherzo/control/client.gleam`.
- [x] (2026-04-28 22:16Z) Integrated control-server startup and shutdown into `src/scherzo/orchestrator/daemon.gleam`, including `NoControlServer` test seams, control-file write on startup, and best-effort control-file removal on programmatic shutdown.
- [x] (2026-04-28 22:16Z) Added `src/scherzo/ctl.gleam`, wired `ctl` mode through `src/scherzo/main.gleam`, and added the executable `scripts/scherzoctl` wrapper.
- [x] (2026-04-28 22:16Z) Added read-only tests in `test/control_protocol_test.gleam`, `test/control_file_test.gleam`, `test/control_server_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/ctl_test.gleam`, and `test/main_test.gleam` covering protocol errors, control files, auth, session listing, event pages, polling attach, daemon lifecycle, and CLI parsing.
- [x] (2026-04-28 22:16Z) Updated README and this plan's retrospective after final validation. `direnv exec . gleam format --check src test` passed, `direnv exec . gleam test` reported `138 passed, no failures`, and `direnv exec . gleam run -- ctl --help` printed the expected control-command usage.

## Surprises & Discoveries

- Observation: The old prerequisites are now present in the working tree.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` defines long-lived daemon startup and starts an EventHub through `RuntimeDependencies.start_event_hub`; `src/scherzo/session/` contains `event.gleam`, `hub.gleam`, `json.gleam`, and `redaction.gleam`; `README.md` documents the internal session event model.

- Observation: The EventHub has read APIs but no subscriber API.
  Evidence: `src/scherzo/session/hub.gleam` exposes `list_sessions`, `get_session`, and `events_after`, and its public `Message` type has `ListSessions`, `GetSession`, and `EventsAfter` but no `Subscribe` or stream fan-out message.

- Observation: The current CLI already has the real-board modes but no control-client mode.
  Evidence: `src/scherzo/main.gleam` defines `RunMode` variants `Daemon`, `Once`, `LinearSmoke`, and `PiProbe`; `parse_args` has no `ctl` branch.

- Observation: The repository still has no control package, no TCP control FFI, and no `scripts/` directory.
  Evidence: `find` shows no `src/scherzo/control/`, no `src/scherzo_control_ffi.erl`, and no `scripts/scherzoctl`.

- Observation: Default control-file discovery must not be the only way to find a daemon.
  Evidence: `src/scherzo/config.gleam` resolves `workspace.root` from the workflow and its built-in default uses the host temp directory, while the README example sets `workspace.root: .scherzo/workspaces`. The logged path, `--control-file`, and `SCHERZO_CONTROL_FILE` are the reliable discovery mechanisms.

- Observation: Relative `workspace.root` values in test workflows are resolved relative to the workflow file location, so tests and operators should trust the logged control-file path rather than reconstructing it from the literal YAML value.
  Evidence: The daemon-control integration test initially looked for `test/tmp/daemon-control-basic/workspaces/.scherzo-state/control.json`, but the daemon correctly wrote under the resolved workspace root and the test was changed to read the `control_file` field from the `control_server_started` log.

- Observation: Loopback `gen_tcp` with `{packet, line}` behaved consistently for request/response and stream tests.
  Evidence: `test/control_server_test.gleam` starts a server on port zero, rejects a bad token, lists a registered session, returns a two-event page, replays one event through `stream_events`, observes a second event published after stream start, and confirms `client.ping` still works after the stream client stops.

## Decision Log

- Decision: Use local loopback TCP with line-delimited JSON for the first control API.
  Rationale: It is small, scriptable, easy for `scherzoctl` and pi skills to call, and avoids introducing a web framework before a dashboard exists.
  Date: 2026-04-28

- Decision: Require a generated token from a control file for every request.
  Rationale: Binding to loopback is not enough on multi-user machines. A token file makes accidental access harder and gives future clients one discovery mechanism.
  Date: 2026-04-28

- Decision: Keep this phase read-only.
  Rationale: Read-only session visibility retires the largest observability gap without adding worker command races or destructive operator actions.
  Date: 2026-04-28

- Decision: Implement `attach --raw` with a polling loop over `hub.events_after` instead of adding EventHub subscriptions now.
  Rationale: The current EventHub intentionally owns bounded replay only. Polling is sufficient for a first local attach command, isolates slow clients, and keeps subscriber backpressure and cancellation semantics out of this phase.
  Date: 2026-04-28

- Decision: Add a daemon dependency seam for starting and stopping the control server, with a daemon-local `NoControlServer` test handle.
  Rationale: The production daemon should start the real TCP server, while most daemon tests should use a constructible no-op handle and only the new daemon-control integration tests should open sockets. Returning the opaque `control/server.Server` type directly from the dependency would make no-op tests impossible to write.
  Date: 2026-04-28

- Decision: When the daemon dependency returns `NoControlServer`, skip writing and logging a control file.
  Rationale: Existing daemon tests should remain focused on scheduler and EventHub behavior without creating stale-looking control files or opening sockets. Production dependencies return `RealControlServer`, so the operator-visible path still always writes the authenticated control file before daemon startup succeeds.
  Date: 2026-04-28

## Outcomes & Retrospective

Implemented the read-only local control phase. The daemon now starts a loopback TCP control server after the EventHub is available, generates a per-start token, writes `workspace.root/.scherzo-state/control.json` with `version`, `host`, `port`, `token`, `workspace_root`, and `started_at_ms`, logs `control_server_started` with `control_file`, `host`, and `port`, and removes the control file during programmatic shutdown. The token is not logged.

The final CLI entry points are `direnv exec . gleam run -- ctl ...` and `scripts/scherzoctl ...`. Supported commands are `ping`, `ps`, `session <session-id>`, `events <session-id>`, and `attach --raw <session-id>`, with `--control-file <path>` and `SCHERZO_CONTROL_FILE` discovery. Non-streaming commands support `--json`; raw attach prints compact event lines by default and can print one JSON stream object per event with `--json`.

Loopback TCP FFI behaved consistently in automated tests. The suite validates bad-token rejection, session listing, event page replay, polling attach replay/follow behavior, daemon control-file lifecycle, stale post-shutdown connection failure, and CLI parsing. Final validation on 2026-04-28 ran `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `direnv exec . gleam run -- ctl --help`; the test suite reported `138 passed, no failures`.

Remaining limitations are intentional: the API is read-only, local loopback TCP only, backed by bounded in-memory EventHub retention, and rendered as basic line-oriented CLI output. Worker mutation, richer terminal rendering, durable audit history, EventHub subscriber fan-out, and remote/public control belong to later plans if they are still needed.

## Context and Orientation

Scherzo is a Gleam Erlang-target project. Runtime source lives under `src/scherzo/`, tests live under `test/`, and validation is run from the repository root with `direnv exec . gleam test`.

The current daemon is implemented in `src/scherzo/orchestrator/daemon.gleam`. It owns scheduler state, worker handles, timers, workflow reload state, the EventHub subject, and the control-server handle when production dependencies are used. Its public `Message` type includes poll and retry ticks, worker updates and finishes, side-effect completion, `Shutdown`, and `GetSnapshot`. Its `RuntimeDependencies` includes tracker and handoff constructors, an `agent_runner`, cleanup, structured logger, `now_ms`, timer helpers, `start_event_hub`, `make_control_token`, `start_control_server`, and `stop_control_server`.

The EventHub is implemented in `src/scherzo/session/hub.gleam`. It stores `event.SessionSummary` values and bounded per-session `event.SessionEvent` buffers. Mutation calls such as `register_session`, `update_status`, `update_pi_session`, `update_tokens`, `publish`, and `finish_session` are fire-and-forget. Query calls use request/reply with a timeout: `list_sessions(subject, timeout_ms)`, `get_session(subject, session_id, timeout_ms)`, and `events_after(subject, session_id, cursor, limit, timeout_ms)`.

The event and summary data model is in `src/scherzo/session/event.gleam`. Session statuses are `Preparing`, `Probing`, `Running`, `WaitingUi`, `Stopping`, and `Exited(reason)`. Event payloads include `kind`, `name`, optional turn, optional pi type, optional assistant message, optional UI request fields, optional tool name, token totals, and optional redacted raw JSON. JSON serializers already exist in `src/scherzo/session/json.gleam`; this plan should reuse them instead of inventing a second event JSON shape.

The current CLI entry point is `src/scherzo/main.gleam`. With no mode flag it runs daemon mode. It also supports `--once`, `--linear-smoke`, `--pi-probe`, `ctl`, and `--help`. The `ctl` mode delegates to `src/scherzo/ctl.gleam` and preserves all existing daemon run modes.

A control file in this plan is a small JSON file containing enough connection information for a local client to authenticate to the daemon. It is not a durable session database, not a lock file, and not an audit log. If a daemon crashes, the file may be stale; `scherzoctl ping` must fail cleanly in that case.

## Preconditions and Verified Facts

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-28 before implementation, `direnv exec . gleam test` ended with `122 passed, no failures`. After implementation and final validation, `direnv exec . gleam test` ended with `138 passed, no failures`.

Current repository facts this plan depends on:

- `src/scherzo/orchestrator/daemon.gleam` starts an EventHub through `RuntimeDependencies.start_event_hub` and stores the subject in daemon state.
- `src/scherzo/session/hub.gleam` exports `default_max_events_per_session`, `default_max_sessions`, `start`, `start_with_limits`, `stop`, `register_session`, `update_status`, `update_pi_session`, `update_tokens`, `publish`, `finish_session`, `list_sessions`, `get_session`, and `events_after`.
- `src/scherzo/session/event.gleam` defines `SessionSummary`, `SessionEvent`, `EventPage`, `EventPayload`, `SessionStatus`, and helper functions `status_to_string`, `exit_reason`, and `kind_to_string`.
- `src/scherzo/session/json.gleam` exposes `summary_to_string`, `event_to_string`, `payload_to_string`, `page_to_string`, and JSON helpers for the same shapes.
- `src/scherzo/main.gleam` parses daemon, once, Linear smoke, pi probe, help, and `ctl` modes.
- `src/scherzo/control/` contains `protocol.gleam`, `file.gleam`, `server.gleam`, and `client.gleam`; `src/scherzo_control_ffi.erl` provides TCP, token, environment, and private-permission helpers; `scripts/scherzoctl` is an executable development wrapper.
- The project already depends on `gleam_json`, `gleam_erlang`, `gleam_otp`, and `simplifile`; this implementation did not add a new package dependency.

If these facts differ when implementation starts, first normalize this plan against the current tree. Do not add a second scheduler, a web framework, or a durable event store to compensate for drift.

## Scope Boundaries

In scope: an authenticated read-only control protocol; a loopback TCP server; token generation; private control-file writing and discovery; a client module; `ctl ping`, `ctl ps`, `ctl session <id>`, `ctl events <id>`, and `ctl attach --raw <id>`; JSON output for automation; daemon lifecycle integration; tests for auth failures, unknown commands, session listing, session lookup, event replay, polling attach, server shutdown, stale control files, and CLI parsing; README documentation.

Out of scope: mutating commands; worker abort; queued prompts; UI request responses; pause/resume; retry/park/unpark; Linear comment commands; a pretty terminal renderer; an HTTP dashboard; public network binding; Unix domain sockets; TLS; durable event storage; EventHub subscriber fan-out; multi-daemon discovery.

## Milestones

Milestone 1 defines the protocol and control-file format. At the end, pure tests can encode and decode `ping`, `list_sessions`, `get_session`, `get_events`, and `stream_events` requests and responses, reject malformed JSON and missing tokens, and read/write a `control.json` fixture.

Milestone 2 implements the local server and client against a fake or test EventHub. At the end, tests can start a loopback server on port zero, discover the chosen port, authenticate with a token, call read-only commands, poll a stream for new events, reject a wrong token, and shut the server down.

Milestone 3 integrates the server with daemon startup. At the end, daemon mode starts the control server after the EventHub is available, writes the control file under the resolved workspace root, logs `control_server_started`, and removes the control file and closes the listener during programmatic shutdown.

Milestone 4 adds `scherzoctl`. At the end, `direnv exec . gleam run -- ctl ps --control-file <file>` and `scripts/scherzoctl ps --control-file <file>` print session data, while `--json` prints machine-readable JSON.

Milestone 5 validates raw attach and documentation. At the end, `ctl attach --raw <session-id>` replays recent events and then follows new event lines using EventHub polling. README explains that the API is local-only, token-authenticated, read-only, and backed by in-memory event retention.

## Plan of Work

Create `src/scherzo/control/protocol.gleam`. Define request variants `Ping`, `ListSessions`, `GetSession(session_id)`, `GetEvents(session_id, after, limit)`, and `StreamEvents(session_id, after)`. Define response helpers for success, error, and stream event lines. Use the existing `gleam/json` and `gleam/dynamic/decode` style already used in `src/scherzo/agent/pi_rpc.gleam`. Keep error codes stable strings such as `bad_json`, `unauthorized`, `unknown_command`, `missing_session`, `invalid_request`, `invalid_limit`, `event_hub_unavailable`, and `connection_failed`.

Create `src/scherzo/control/file.gleam`. Define `ControlFile(host, port, token, workspace_root, started_at_ms)`. The serialized JSON must include `version: 1`, `host`, `port`, `token`, `workspace_root`, and `started_at_ms`. Implement read, write, remove, and discovery helpers. Discovery order is explicit `--control-file`, `SCHERZO_CONTROL_FILE`, then `.scherzo/workspaces/.scherzo-state/control.json` if that file exists. The write helper creates `workspace.root/.scherzo-state`, writes `control.json`, and asks the FFI to set owner-only permissions where the host supports it.

Create `src/scherzo/control/server.gleam` and `src/scherzo_control_ffi.erl`. The FFI owns `gen_tcp` listen, accept, send line, receive line with timeout, peer close, listener close, bound-port lookup, strong random token generation, and private-file permission helpers. The Gleam server spawns one accept loop and one process per connection. Each connection reads exactly one request for ordinary commands. `StreamEvents` keeps the connection open after the initial success response and polls the EventHub for events after the last cursor.

Represent the EventHub read dependency in the server as a small adapter so tests can use either a real `hub.Subject` or a fake store. The production adapter calls `hub.list_sessions`, `hub.get_session`, and `hub.events_after` with short timeouts. The server must never call EventHub mutation functions.

Create `src/scherzo/control/client.gleam`. It reads a control file, connects to host and port, sends one request line with the token, reads response lines, and decodes them. Expose a low-level `request` function that returns the decoded protocol response for `--json`, plus typed helpers for `ping`, `list_sessions`, `get_session`, `get_events`, and `stream_events`. `stream_events` should accept a callback that can return `Continue` or `Stop` so tests and future callers can end a stream without killing the process.

Modify `src/scherzo/orchestrator/daemon.gleam`. Extend `RuntimeDependencies` with control-server lifecycle seams so normal daemon tests can provide no-op fakes and the integration tests can use the real TCP server. Define a daemon-local handle type such as `ControlServerHandle` with variants `NoControlServer` and `RealControlServer(control_server.Server)`, or an equivalent constructible wrapper, so tests are not forced to construct the opaque `control/server.Server` type. A suitable dependency shape is `make_control_token`, `start_control_server`, and `stop_control_server`, but the implementation may choose equivalent names if the responsibilities stay clear. Extend daemon state with the control server handle and control file path. After `start_event_hub` succeeds and the workflow config has resolved the workspace root, generate a token, start the loopback server on port zero, write the control file, and log `control_server_started` with `control_file`, `host`, and `port` fields. Do not log the token. If any of those steps fails, stop any partially started control server, remove any partial control file, and fail daemon startup with a clear `control_server_start_failed` or `control_file_write_failed` code.

Modify daemon shutdown handling so programmatic `daemon.shutdown` stops the control server and removes the control file before replying. If cleanup fails, log a warning and continue shutdown; a stale control file is recoverable because the next `scherzoctl ping` will fail cleanly. Keep the existing scheduler and worker shutdown behavior unchanged.

Modify `src/scherzo/main.gleam` to recognize `ctl` as a client mode without changing daemon run modes. Add `src/scherzo/ctl.gleam` for subcommand parsing and output. Supported commands are `ping`, `ps`, `session <id>`, `events <id>`, and `attach --raw <id>`. Every command accepts `--control-file <path>`; non-streaming commands accept `--json`; attach may print one JSON object line per received stream line when `--json` is present.

Add `scripts/scherzoctl` as a POSIX shell wrapper with executable permissions. The script should find the repository root relative to itself and run `direnv exec "$ROOT" gleam run -- ctl "$@"`. Add a short comment that packaged releases may replace the wrapper with a compiled executable later.

Update `README.md` with a `Read-only control API` section. Show how to find the control file from daemon logs, set `SCHERZO_CONTROL_FILE`, run `scripts/scherzoctl ps`, run `scripts/scherzoctl events <session-id>`, and use `--json` for automation. Warn that the API is local-only, token-authenticated, read-only, in-memory, and not a durable audit log.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count. If the EventHub or daemon tests fail, stop and fix the current tree before adding the control server.

2. Create `test/control_protocol_test.gleam`. Add `decode_ping_request_requires_token_test`, asserting a JSON request with `version: 1`, `type: "ping"`, `id: "1"`, and a token decodes, while the same request without token returns `invalid_request`.

3. In `test/control_protocol_test.gleam`, add `unknown_command_returns_stable_error_test`, asserting that `type: "delete_everything"` is handled as an `unknown_command` error response rather than crashing.

4. In `test/control_protocol_test.gleam`, add `encode_events_response_contains_cursor_and_session_test`, constructing a fake `event.EventPage` and asserting the encoded response contains the request id, session id, next cursor, `truncated`, and `ok: true`.

5. Implement `src/scherzo/control/protocol.gleam` until the protocol tests pass. Run `direnv exec . gleam test`.

6. Create `test/control_file_test.gleam`. Add tests for writing and reading `control.json`, for `SCHERZO_CONTROL_FILE` discovery using an injected environment lookup, and for default discovery failing cleanly when no file exists.

7. Implement `src/scherzo/control/file.gleam` and the token/private-file helpers in `src/scherzo_control_ffi.erl`. Run `direnv exec . gleam test`.

8. Create `test/control_server_test.gleam`. Add `server_rejects_bad_token_test`: start a real test EventHub or fake store with one fixed session, start the server on port zero, connect with the wrong token, and assert the response error code is `unauthorized`.

9. In `test/control_server_test.gleam`, add `server_lists_sessions_with_good_token_test`: call `client.list_sessions` with the correct control file and assert the fake session identifier appears.

10. In `test/control_server_test.gleam`, add `server_returns_event_page_test`: publish two retained events to a test EventHub, call `client.get_events` with cursor `0` and limit `10`, and assert ordered cursors and `truncated == False`.

11. In `test/control_server_test.gleam`, add `server_streams_events_by_polling_after_cursor_test`: start `client.stream_events` with a callback that records two events and returns `Stop` after the second event. Publish one event before stream start and one after stream start. Assert both are received in cursor order and the server remains alive after the client stops.

12. Implement `src/scherzo/control/server.gleam` and `src/scherzo/control/client.gleam` until the server tests pass. Keep socket and EventHub timeouts short in tests and avoid hard-coded ports.

13. Modify `src/scherzo/orchestrator/daemon.gleam` to start the control server and write the control file. Add `test/orchestrator_daemon_control_test.gleam` with `daemon_writes_control_file_and_serves_session_list_test`. It should start a daemon with fake dependencies and the real control server, wait for `control_server_started`, read the control file, call `client.list_sessions`, and then shutdown.

14. Extend the daemon control test with `daemon_shutdown_closes_control_server_and_removes_control_file_test`. After `daemon.shutdown`, assert `client.ping` fails with `connection_failed` and the control file is absent or marked stale according to the implemented cleanup choice.

15. Update existing daemon test helpers in `test/orchestrator_daemon_test.gleam` and `test/orchestrator_daemon_session_event_test.gleam` to return the daemon-local no-op control-server handle so unrelated daemon tests do not open sockets.

16. Update parser tests in `test/main_test.gleam` so `ctl ps`, `ctl events ABC-123`, and `ctl attach --raw ABC-123` parse as control-client mode while existing daemon, once, smoke, probe, and help modes still parse as before.

17. Create `src/scherzo/ctl.gleam`. Implement pure subcommand parsing first and add `test/ctl_test.gleam` for `ping`, `ps`, `session`, `events`, `attach --raw`, `--control-file`, `--json`, and usage errors.

18. Wire `main.gleam` to call `ctl.main` for `ctl` mode. For `ps`, print a table by default with columns `SESSION`, `ISSUE`, `STATUS`, `TURN`, and `LAST_EVENT`. For `--json`, print the protocol JSON. For `events`, print one compact line per event. For `attach --raw`, replay and stream compact event lines.

19. Create `scripts/scherzoctl` with executable permissions. Verify it calls the same `ctl` mode from the repository root.

20. Run `direnv exec . gleam run -- ctl --help`. Expect usage text listing `ping`, `ps`, `session`, `events`, `attach --raw`, `--control-file`, and `--json`.

21. Update `README.md` with examples and safety notes. Include a warning that the API is local-only and read-only in this phase.

22. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record the final pass count in Progress.

23. Commit the phase with a message such as `Add read-only Scherzo control API`.

## Testing and Falsifiability

The protocol is falsified if malformed JSON crashes a connection, if missing or wrong tokens are accepted, if unknown commands do not produce stable errors, if response JSON cannot be decoded by the client, or if `--json` output differs from the protocol shape documented by tests.

The server is falsified if it binds to a non-loopback host in this phase, if a slow stream blocks EventHub publication or daemon scheduling, if shutdown leaves a listener accepting connections, if a stale control file causes an unhandled crash, or if `scherzoctl` cannot use an explicitly supplied control file from a second process.

The attach design is falsified if `attach --raw` cannot replay existing retained events and then observe a new event published after the stream starts. Because attach uses polling in this phase, acceptance allows a small configured delay, but it must not require daemon logs or direct EventHub access from the CLI.

Add deterministic tests in `test/control_protocol_test.gleam`, `test/control_file_test.gleam`, `test/control_server_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/ctl_test.gleam`, and `test/main_test.gleam`. No test may require real Linear, real pi, public network access, or fixed TCP ports. Use fake tracker clients, fake daemon dependencies, and test EventHubs.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, protocol and control modules will not compile because they do not exist. After implementation, the full suite must pass without hanging on open sockets or stream clients.

## Validation and Acceptance

Manual acceptance after tests uses a safe fake-pi workflow or a private harmless workflow:

1. Start daemon mode in one terminal:

       LINEAR_API_KEY=lin_api_... REPO_URL=git@github.com:example/repo.git direnv exec . gleam run -- ./examples/WORKFLOW.md

2. Observe a log line like:

       level=info service=scherzo event=control_server_started control_file=... host=127.0.0.1 port=...

3. In another terminal, run:

       SCHERZO_CONTROL_FILE=<logged-file> scripts/scherzoctl ping

   Expect a successful response and exit code 0.

4. Run:

       SCHERZO_CONTROL_FILE=<logged-file> scripts/scherzoctl ps

   Expect a table of sessions or an empty table with a successful exit.

5. Run:

       SCHERZO_CONTROL_FILE=<logged-file> scripts/scherzoctl ps --json

   Expect JSON with `ok: true` and session data.

6. For a known session, run:

       SCHERZO_CONTROL_FILE=<logged-file> scripts/scherzoctl events <session-id>

   Expect recent ordered events, including lifecycle and pi events retained by the EventHub.

7. For an active fake-pi session, run:

       SCHERZO_CONTROL_FILE=<logged-file> scripts/scherzoctl attach --raw <session-id>

   Expect recent events to print and new events to appear until the session exits or the command is interrupted.

Do not accept this phase if any mutating command exists, if the token appears in daemon logs, or if the server can be configured to bind publicly.

## Rollout, Recovery, and Idempotence

The control server is additive. If it fails to start in daemon mode, daemon startup should fail with a clear error because the daemon would otherwise advertise no supported operator visibility path. If writing the control file fails, startup should also fail; otherwise operators cannot discover the authenticated endpoint safely.

If a daemon crashes, a stale control file may remain. `scherzoctl ping` should fail cleanly with `connection_failed` or `unauthorized` rather than crashing. The README should tell operators to restart the daemon and use the newly logged control file path rather than editing the control file manually.

The server chooses an OS-assigned port by default, so repeated test runs should not collide. Shutdown must close the listener before returning from `daemon.shutdown` and should remove the control file best-effort. Re-running tests is idempotent because each test uses its own workspace root and control file path.

The API is local and read-only. If this phase causes problems, reverting the control modules and daemon lifecycle hook returns Scherzo to the previous internal-EventHub state without changing workspace contents or Linear issue state.

## Artifacts and Notes

Request example:

    {"version":1,"id":"1","token":"...","type":"list_sessions"}

Response example:

    {"version":1,"id":"1","ok":true,"data":{"sessions":[...]}}

Event page response data should reuse the EventHub JSON shape from `src/scherzo/session/json.gleam`:

    {"events":[...],"next_cursor":42,"truncated":false}

Stream event example:

    {"version":1,"id":"3","stream":true,"session_id":"ABC-123-42-1","cursor":42,"event":{...}}

Control file example:

    {"version":1,"host":"127.0.0.1","port":54321,"token":"...","workspace_root":".scherzo/workspaces","started_at_ms":42}

## Interfaces and Dependencies

In `src/scherzo/control/protocol.gleam`, define command and response types equivalent to:

    pub type Request {
      Ping(id: String, token: String)
      ListSessions(id: String, token: String)
      GetSession(id: String, token: String, session_id: String)
      GetEvents(id: String, token: String, session_id: String, after: Int, limit: Int)
      StreamEvents(id: String, token: String, session_id: String, after: Int)
    }

    pub type ErrorBody {
      ErrorBody(code: String, message: String)
    }

    pub type Response {
      Response(id: String, ok: Bool, data: Option(json.Json), error: Option(ErrorBody))
    }

In `src/scherzo/control/file.gleam`, expose helpers equivalent to:

    pub type ControlFile {
      ControlFile(host: String, port: Int, token: String, workspace_root: String, started_at_ms: Int)
    }

    pub fn path_for_workspace(workspace_root: String) -> String
    pub fn read(path: String) -> Result(ControlFile, ControlFileError)
    pub fn write(path: String, control_file: ControlFile) -> Result(Nil, ControlFileError)
    pub fn remove(path: String) -> Nil
    pub fn discover(explicit_path: Option(String), env: fn(String) -> Option(String)) -> Result(ControlFile, ControlFileError)

In `src/scherzo/control/client.gleam`, expose:

    pub type StreamAction {
      Continue
      Stop
    }

    pub fn request(control_file: file.ControlFile, request: protocol.Request) -> Result(protocol.Response, ControlError)
    pub fn ping(control_file: file.ControlFile) -> Result(Nil, ControlError)
    pub fn list_sessions(control_file: file.ControlFile) -> Result(List(event.SessionSummary), ControlError)
    pub fn get_session(control_file: file.ControlFile, session_id: String) -> Result(Option(event.SessionSummary), ControlError)
    pub fn get_events(control_file: file.ControlFile, session_id: String, after: Int, limit: Int) -> Result(event.EventPage, ControlError)
    pub fn stream_events(control_file: file.ControlFile, session_id: String, after: Int, on_event: fn(event.SessionEvent) -> StreamAction) -> Result(Nil, ControlError)

In `src/scherzo/control/server.gleam`, expose a server handle and settings equivalent to:

    pub type Settings {
      Settings(host: String, port: Int, token: String, event_timeout_ms: Int, stream_poll_ms: Int)
    }

    pub opaque type Server

    pub fn start(settings: Settings, store: EventStore) -> Result(Server, ServerError)
    pub fn bound_port(server: Server) -> Int
    pub fn stop(server: Server) -> Nil

In `src/scherzo/orchestrator/daemon.gleam`, add a constructible handle wrapper for the dependency seam, equivalent to:

    pub type ControlServerHandle {
      NoControlServer
      RealControlServer(control_server.Server)
    }

The default dependencies use `RealControlServer` and the real server; unrelated daemon tests use `NoControlServer` and a `stop_control_server` function that returns `Nil`.

No new package dependency should be required. Add only `src/scherzo_control_ffi.erl` for loopback TCP, token generation, and private-file helpers if existing Gleam packages do not provide the needed operations.

## Revision Notes

2026-04-28: This revision updates the plan after the real-board-readiness and session-eventhub work landed in the working tree. It removes the stale prerequisite-normalization step, records the current `122 passed, no failures` baseline, updates the context to the actual daemon and EventHub interfaces, changes raw attach from a planned subscription model to polling `hub.events_after`, and narrows control-file discovery so explicit paths and `SCHERZO_CONTROL_FILE` are the reliable mechanisms for custom workspace roots.
