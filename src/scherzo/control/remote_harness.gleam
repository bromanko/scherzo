import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/control/query/dto as query_dto
import scherzo/control/query/types as query_types
import scherzo/control/remote/client as remote_client
import scherzo/control/remote_envelope
import scherzo/control/remote_harness_hello
import scherzo/control/remote_liveness
import scherzo/hash
import simplifile

pub type Listener

pub type Socket

pub type Scenario {
  Scenario(
    expected_token: String,
    client_token: String,
    daemon_id: String,
    boot_id: String,
    hello_now_ms: Int,
    heartbeat_now_ms: Int,
    stale_after_ms: Int,
    offline_after_ms: Int,
    send_malformed_hello: Bool,
    heartbeat_envelope: Option(remote_envelope.Envelope),
    state_sessions: List(remote_envelope.RemoteSession),
    transcript_path: Option(String),
    run_nonce: String,
    use_real_client: Bool,
    command_demo: Bool,
    query_demo: Bool,
  )
}

pub type TranscriptEvent {
  TranscriptEvent(
    sequence: Int,
    connection_sequence: Int,
    direction: String,
    kind: String,
    digest: Option(String),
    redacted_line: Option(String),
    command_id: Option(String),
    command_status: Option(String),
    dispatch_paused: Option(Bool),
  )
}

pub type Report {
  Report(
    run_nonce: String,
    bound_port: Int,
    daemon_id: String,
    boot_id: String,
    registry: remote_liveness.Registry,
    observations: List(remote_liveness.View),
    events: List(TranscriptEvent),
    transcript_json: String,
  )
}

pub type HarnessError {
  HarnessError(code: String, message: String)
}

type ServerOutcome {
  ServerOutcome(
    registry: remote_liveness.Registry,
    observations: List(remote_liveness.View),
    events: List(TranscriptEvent),
  )
}

pub fn default_scenario(
  token: String,
  transcript_path: String,
) -> Result(Scenario, HarnessError) {
  use run_nonce <- result.try(generate_run_nonce())
  let daemon_id = prefixed_id("daemon_", run_nonce <> ":daemon")
  let boot_id = prefixed_id("boot_", run_nonce <> ":boot")
  let now_ms = monotonic_ms()
  Ok(Scenario(
    expected_token: token,
    client_token: token,
    daemon_id: daemon_id,
    boot_id: boot_id,
    hello_now_ms: now_ms,
    heartbeat_now_ms: now_ms + 1,
    stale_after_ms: 50,
    offline_after_ms: 100,
    send_malformed_hello: False,
    heartbeat_envelope: None,
    state_sessions: [
      remote_envelope.RemoteSession(
        session_id: "session-1",
        display_name: "Harness demo session",
        issue_identifier: "LIV-686",
        status: "running",
        current_turn: 1,
        last_event_at_ms: now_ms,
      ),
    ],
    transcript_path: Some(transcript_path),
    run_nonce: run_nonce,
    use_real_client: True,
    command_demo: False,
    query_demo: False,
  ))
}

pub fn run_demo(
  token: String,
  transcript_path: String,
) -> Result(Report, HarnessError) {
  use scenario <- result.try(default_scenario(token, transcript_path))
  run_scenario(scenario)
}

pub fn run_command_demo(
  token: String,
  transcript_path: String,
) -> Result(Report, HarnessError) {
  use scenario <- result.try(default_scenario(token, transcript_path))
  run_scenario(Scenario(..scenario, command_demo: True))
}

pub fn run_query_demo(
  token: String,
  transcript_path: String,
) -> Result(Report, HarnessError) {
  use scenario <- result.try(default_scenario(token, transcript_path))
  run_scenario(Scenario(..scenario, query_demo: True))
}

pub fn run_scenario(scenario: Scenario) -> Result(Report, HarnessError) {
  use registry <- result.try(
    remote_liveness.new(scenario.stale_after_ms, scenario.offline_after_ms)
    |> result.map_error(registry_error),
  )
  use listener <- result.try(
    listen("127.0.0.1", 0) |> result.map_error(socket_error("listen failed")),
  )
  let bound_port = bound_port(listener)
  let server_subject = process.new_subject()
  let server_process =
    process.spawn_unlinked(fn() {
      server_loop(listener, registry, scenario, server_subject)
    })
  case scenario.use_real_client {
    True ->
      run_real_client_scenario(
        listener,
        bound_port,
        server_subject,
        server_process,
        scenario,
      )
    False ->
      run_manual_client_scenario(
        listener,
        bound_port,
        server_subject,
        server_process,
        scenario,
      )
  }
}

fn run_manual_client_scenario(
  listener: Listener,
  bound_port: Int,
  server_subject: process.Subject(Result(ServerOutcome, HarnessError)),
  server_process: process.Pid,
  scenario: Scenario,
) -> Result(Report, HarnessError) {
  case
    connect("127.0.0.1", bound_port, 1000)
    |> result.map_error(socket_error("connect failed"))
  {
    Error(err) -> {
      close_listener(listener)
      process.kill(server_process)
      Error(err)
    }
    Ok(client) ->
      run_connected_manual_scenario(
        listener,
        bound_port,
        server_subject,
        server_process,
        client,
        scenario,
      )
  }
}

fn run_connected_manual_scenario(
  listener: Listener,
  bound_port: Int,
  server_subject: process.Subject(Result(ServerOutcome, HarnessError)),
  server_process: process.Pid,
  client: Socket,
  scenario: Scenario,
) -> Result(Report, HarnessError) {
  let client_events_result = client_exchange(client, scenario)
  close_socket(client)
  case client_events_result {
    Error(err) -> {
      close_listener(listener)
      process.kill(server_process)
      Error(err)
    }
    Ok(client_events) ->
      finish_server_outcome(
        listener,
        server_subject,
        server_process,
        scenario,
        bound_port,
        client_events,
      )
  }
}

fn run_real_client_scenario(
  listener: Listener,
  bound_port: Int,
  server_subject: process.Subject(Result(ServerOutcome, HarnessError)),
  server_process: process.Pid,
  scenario: Scenario,
) -> Result(Report, HarnessError) {
  let client_lines = process.new_subject()
  let settings = client_settings(bound_port, scenario)
  let dependencies = client_dependencies(bound_port, scenario, client_lines)
  case remote_client.start(settings, dependencies) {
    Error(remote_client.ClientError(code: code, message: message)) -> {
      close_listener(listener)
      process.kill(server_process)
      Error(HarnessError(code, message))
    }
    Ok(handle) -> {
      let outcome =
        finish_server_outcome(
          listener,
          server_subject,
          server_process,
          scenario,
          bound_port,
          client_events_from_subject(client_lines, scenario),
        )
      let _ = remote_client.stop(handle, 1000)
      outcome
    }
  }
}

fn finish_server_outcome(
  listener: Listener,
  server_subject: process.Subject(Result(ServerOutcome, HarnessError)),
  server_process: process.Pid,
  scenario: Scenario,
  bound_port: Int,
  client_events: List(TranscriptEvent),
) -> Result(Report, HarnessError) {
  let outcome_result = case process.receive(server_subject, within: 1000) {
    Ok(Ok(outcome)) -> Ok(outcome)
    Ok(Error(err)) -> Error(err)
    Error(Nil) ->
      Error(HarnessError("server_timeout", "server did not finish in time"))
  }
  close_listener(listener)
  case outcome_result {
    Error(err) -> {
      case err.code == "server_timeout" {
        True -> process.kill(server_process)
        False -> Nil
      }
      Error(err)
    }
    Ok(outcome) ->
      report_from_outcome(scenario, bound_port, client_events, outcome)
  }
}

fn report_from_outcome(
  scenario: Scenario,
  bound_port: Int,
  client_events: List(TranscriptEvent),
  outcome: ServerOutcome,
) -> Result(Report, HarnessError) {
  let events = list.append(client_events, outcome.events) |> sort_events
  let transcript_json =
    transcript_json_string(
      scenario.run_nonce,
      bound_port,
      events,
      outcome.observations,
    )
  use _ <- result.try(write_transcript_if_requested(
    scenario.transcript_path,
    transcript_json,
  ))
  Ok(Report(
    run_nonce: scenario.run_nonce,
    bound_port: bound_port,
    daemon_id: scenario.daemon_id,
    boot_id: scenario.boot_id,
    registry: outcome.registry,
    observations: outcome.observations,
    events: events,
    transcript_json: transcript_json,
  ))
}

fn client_exchange(
  client: Socket,
  scenario: Scenario,
) -> Result(List(TranscriptEvent), HarnessError) {
  let hello_line = case scenario.send_malformed_hello {
    True -> "{"
    False ->
      remote_harness_hello.encode(
        scenario.daemon_id,
        scenario.boot_id,
        scenario.client_token,
        ["control_commands", "session_snapshots", "read_queries"],
      )
  }
  use _ <- result.try(
    send_line(client, hello_line, 1000)
    |> result.map_error(socket_error("send hello failed")),
  )
  let hello_event = line_event(1, 1, "client_send", "hello", hello_line)
  case
    scenario.send_malformed_hello
    || scenario.client_token != scenario.expected_token
  {
    True -> Ok([hello_event])
    False -> {
      let heartbeat_envelope = case scenario.heartbeat_envelope {
        Some(envelope) -> envelope
        None -> remote_envelope.RemoteHeartbeat(scenario.heartbeat_now_ms)
      }
      let heartbeat_line = heartbeat_envelope |> remote_envelope.to_string
      use _ <- result.try(
        send_line(client, heartbeat_line, 1000)
        |> result.map_error(socket_error("send heartbeat failed")),
      )
      Ok([
        hello_event,
        line_event(3, 1, "client_send", "heartbeat", heartbeat_line),
      ])
    }
  }
}

fn client_settings(
  bound_port: Int,
  scenario: Scenario,
) -> remote_client.Settings {
  remote_client.Settings(
    endpoint: "https://127.0.0.1:" <> int.to_string(bound_port),
    daemon_id: scenario.daemon_id,
    boot_id: scenario.boot_id,
    enrollment_token: scenario.client_token,
    capabilities: ["control_commands", "session_snapshots", "read_queries"],
    heartbeat_interval_ms: case scenario.query_demo {
      True -> 20
      False -> 10_000
    },
    state_interval_ms: case scenario.query_demo {
      True -> 30
      False -> 10_000
    },
    retry_initial_ms: 50,
    retry_max_ms: 100,
    connect_timeout_ms: 1000,
    command_timeout_ms: 1000,
    redaction_secrets: [scenario.expected_token, scenario.client_token],
  )
}

fn client_dependencies(
  bound_port: Int,
  scenario: Scenario,
  client_lines: process.Subject(String),
) -> remote_client.Dependencies(Socket, process.Timer) {
  remote_client.Dependencies(
    now_ms: fn() { scenario.heartbeat_now_ms },
    connect: fn(_, timeout_ms) { connect("127.0.0.1", bound_port, timeout_ms) },
    send_line: fn(socket, line, timeout_ms) {
      use _ <- result.try(send_line(socket, line, timeout_ms))
      process.send(client_lines, line)
      Ok(Nil)
    },
    recv_line: recv_line,
    close: close_socket,
    send_after: process.send_after,
    cancel_timer: fn(timer) {
      let _ = process.cancel_timer(timer)
      Nil
    },
    list_sessions: fn() { Ok(scenario.state_sessions) },
    apply_command: fn(operator_command, _timeout_ms) {
      Ok(command.applied(operator_command, Some("applied")))
    },
    execute_query: fn(query) {
      case query {
        query_types.Status ->
          Ok(
            query_types.StatusResponse(query_types.StatusDto(
              daemon_id: scenario.daemon_id,
              boot_id: scenario.boot_id,
              dispatch_paused: False,
              ui_server_enabled: False,
              supported_queries: query_types.supported_queries(),
            )),
          )
        query_types.Metrics ->
          Ok(query_types.MetricsResponse(
            query_types.default_operational_metrics_source(
              daemon_id: scenario.daemon_id,
              boot_id: scenario.boot_id,
            )
            |> query_dto.operational_metrics_from_source,
          ))
        query_types.TaskList(_) ->
          Ok(
            query_types.TaskListResponse(query_types.TaskListDto(
              items: [],
              page: query_types.PageDto(next_cursor: None, has_more: False),
            )),
          )
        query_types.TaskShow(_) ->
          Error(query_types.QueryError(
            query_types.QueryNotFound,
            "task not found",
          ))
        query_types.OutboxList(_) ->
          Ok(
            query_types.OutboxListResponse(query_types.OutboxListDto(
              items: [],
              page: query_types.PageDto(next_cursor: None, has_more: False),
            )),
          )
        query_types.OutboxShow(_) ->
          Error(query_types.QueryError(
            query_types.QueryNotFound,
            "outbox record not found",
          ))
      }
    },
    dispatch_paused: fn(_timeout_ms) { Error("dispatch_state_unavailable") },
    logger: fn(_, _, _, _) { Ok(Nil) },
  )
}

fn client_events_from_subject(
  subject: process.Subject(String),
  scenario: Scenario,
) -> List(TranscriptEvent) {
  collect_client_lines(subject, expected_client_event_count(scenario), [])
  |> list.index_map(fn(line, index) {
    let sequence = index * 2 + 1
    line_event(sequence, 1, "client_send", event_kind(line), line)
  })
}

fn expected_client_event_count(scenario: Scenario) -> Int {
  case scenario.command_demo, scenario.query_demo {
    True, _ -> 9
    _, True -> 6
    _, False -> 3
  }
}

fn collect_client_lines(
  subject: process.Subject(String),
  remaining: Int,
  acc: List(String),
) -> List(String) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      case process.receive(subject, within: 1000) {
        Ok(line) -> collect_client_lines(subject, remaining - 1, [line, ..acc])
        Error(Nil) -> list.reverse(acc)
      }
  }
}

fn server_loop(
  listener: Listener,
  registry: remote_liveness.Registry,
  scenario: Scenario,
  subject: process.Subject(Result(ServerOutcome, HarnessError)),
) -> Nil {
  let result = case
    accept(listener) |> result.map_error(socket_error("accept failed"))
  {
    Error(err) -> Error(err)
    Ok(server) -> {
      let outcome = server_exchange(server, registry, scenario)
      close_socket(server)
      outcome
    }
  }
  process.send(subject, result)
  Nil
}

fn server_exchange(
  server: Socket,
  registry: remote_liveness.Registry,
  scenario: Scenario,
) -> Result(ServerOutcome, HarnessError) {
  use hello_line <- result.try(
    recv_line(server, 1000)
    |> result.map_error(socket_error("recv hello failed")),
  )
  let hello_event = line_event(2, 1, "server_recv", "hello", hello_line)
  use hello <- result.try(
    remote_harness_hello.decode(hello_line, scenario.expected_token)
    |> result.map_error(hello_error),
  )
  use registered <- result.try(
    remote_liveness.register_hello(
      registry,
      hello.daemon_id,
      hello.boot_id,
      scenario.hello_now_ms,
    )
    |> result.map_error(registry_error),
  )
  use hello_view <- result.try(
    remote_liveness.view(registered, hello.daemon_id, scenario.hello_now_ms)
    |> result.map_error(registry_error),
  )
  use heartbeat_line <- result.try(
    recv_line(server, 1000)
    |> result.map_error(socket_error("recv heartbeat failed")),
  )
  let heartbeat_event =
    line_event(4, 1, "server_recv", "heartbeat", heartbeat_line)
  use heartbeat <- result.try(
    remote_envelope.decode(heartbeat_line)
    |> result.map_error(fn(err) { HarnessError(err.code, err.message) }),
  )
  case heartbeat {
    remote_envelope.RemoteHeartbeat(_sent_at_ms) -> {
      use refreshed <- result.try(
        remote_liveness.heartbeat(
          registered,
          hello.daemon_id,
          hello.boot_id,
          scenario.heartbeat_now_ms,
        )
        |> result.map_error(registry_error),
      )
      use heartbeat_view <- result.try(
        remote_liveness.view(
          refreshed,
          hello.daemon_id,
          scenario.heartbeat_now_ms,
        )
        |> result.map_error(registry_error),
      )
      case scenario.use_real_client {
        False ->
          Ok(
            ServerOutcome(refreshed, [hello_view, heartbeat_view], [
              hello_event,
              heartbeat_event,
            ]),
          )
        True -> {
          use state_line <- result.try(
            recv_line(server, 1000)
            |> result.map_error(socket_error("recv state failed")),
          )
          let state_event =
            line_event(6, 1, "server_recv", "state_snapshot", state_line)
          use state <- result.try(
            remote_envelope.decode(state_line)
            |> result.map_error(fn(err) { HarnessError(err.code, err.message) }),
          )
          case state {
            remote_envelope.RemoteStateSnapshot(_, False, sessions)
              if sessions == scenario.state_sessions
            ->
              case scenario.command_demo, scenario.query_demo {
                True, _ ->
                  command_demo_exchange(
                    server,
                    refreshed,
                    [hello_view, heartbeat_view],
                    [hello_event, heartbeat_event, state_event],
                  )
                _, True ->
                  query_demo_exchange(
                    server,
                    refreshed,
                    [hello_view, heartbeat_view],
                    [hello_event, heartbeat_event, state_event],
                  )
                _, False ->
                  Ok(
                    ServerOutcome(refreshed, [hello_view, heartbeat_view], [
                      hello_event,
                      heartbeat_event,
                      state_event,
                    ]),
                  )
              }
            remote_envelope.RemoteStateSnapshot(_, _, _) ->
              Error(HarnessError("invalid_state", "unexpected state snapshot"))
            _ -> Error(HarnessError("invalid_state", "expected state envelope"))
          }
        }
      }
    }
    _ -> Error(HarnessError("invalid_heartbeat", "expected heartbeat envelope"))
  }
}

fn query_demo_exchange(
  server: Socket,
  registry: remote_liveness.Registry,
  observations: List(remote_liveness.View),
  base_events: List(TranscriptEvent),
) -> Result(ServerOutcome, HarnessError) {
  let query_line =
    remote_envelope.RemoteQueryRequest("query-1", query_types.Status)
    |> remote_envelope.to_string
  use _ <- result.try(
    send_line(server, query_line, 1000)
    |> result.map_error(socket_error("send query failed")),
  )
  use query_response <- result.try(recv_and_expect(server, 8, "query_response"))
  use heartbeat <- result.try(recv_and_expect(server, 10, "heartbeat"))
  use state <- result.try(recv_and_expect(server, 12, "state_snapshot"))

  Ok(ServerOutcome(
    registry,
    observations,
    list.append(base_events, [
      line_event(7, 1, "server_send", "query_request", query_line),
      query_response,
      heartbeat,
      state,
    ]),
  ))
}

fn command_demo_exchange(
  server: Socket,
  registry: remote_liveness.Registry,
  observations: List(remote_liveness.View),
  base_events: List(TranscriptEvent),
) -> Result(ServerOutcome, HarnessError) {
  let pause_line =
    remote_envelope.RemoteServerCommand("pause-1", command.PauseDispatch)
    |> remote_envelope.to_string
  use _ <- result.try(
    send_line(server, pause_line, 1000)
    |> result.map_error(socket_error("send pause failed")),
  )
  use pause_receipt <- result.try(recv_and_expect(server, 8, "command_receipt"))
  use pause_result <- result.try(recv_and_expect(server, 9, "command_result"))
  use pause_state <- result.try(recv_and_expect(server, 10, "state_snapshot"))

  let resume_line =
    remote_envelope.RemoteServerCommand("resume-1", command.ResumeDispatch)
    |> remote_envelope.to_string
  use _ <- result.try(
    send_line(server, resume_line, 1000)
    |> result.map_error(socket_error("send resume failed")),
  )
  use resume_receipt <- result.try(recv_and_expect(
    server,
    12,
    "command_receipt",
  ))
  use resume_result <- result.try(recv_and_expect(server, 13, "command_result"))
  use resume_state <- result.try(recv_and_expect(server, 14, "state_snapshot"))

  Ok(ServerOutcome(
    registry,
    observations,
    list.append(base_events, [
      line_event(7, 1, "server_send", "server_command", pause_line),
      pause_receipt,
      pause_result,
      pause_state,
      line_event(11, 1, "server_send", "server_command", resume_line),
      resume_receipt,
      resume_result,
      resume_state,
    ]),
  ))
}

fn recv_and_expect(
  server: Socket,
  sequence: Int,
  expected_kind: String,
) -> Result(TranscriptEvent, HarnessError) {
  use line <- result.try(
    recv_line(server, 1000)
    |> result.map_error(socket_error("recv command event failed")),
  )
  let event = line_event(sequence, 1, "server_recv", expected_kind, line)
  case event.kind == expected_kind {
    True -> Ok(event)
    False ->
      Error(HarnessError("invalid_command_event", "unexpected event order"))
  }
}

fn sort_events(events: List(TranscriptEvent)) -> List(TranscriptEvent) {
  list.sort(events, by: fn(left, right) {
    int.compare(left.sequence, right.sequence)
  })
}

fn line_event(
  sequence: Int,
  connection_sequence: Int,
  direction: String,
  fallback_kind: String,
  line: String,
) -> TranscriptEvent {
  let #(kind, command_id, command_status, dispatch_paused) =
    envelope_metadata(line, fallback_kind)
  TranscriptEvent(
    sequence: sequence,
    connection_sequence: connection_sequence,
    direction: direction,
    kind: kind,
    digest: Some(hash.short_sha256_hex(line, 12)),
    redacted_line: Some(remote_harness_hello.redact_auth(line)),
    command_id: command_id,
    command_status: command_status,
    dispatch_paused: dispatch_paused,
  )
}

fn event_kind(line: String) -> String {
  let #(kind, _, _, _) = envelope_metadata(line, "hello")
  kind
}

fn envelope_metadata(
  line: String,
  fallback_kind: String,
) -> #(String, Option(String), Option(String), Option(Bool)) {
  case remote_envelope.decode(line) {
    Ok(remote_envelope.RemoteHeartbeat(_)) -> #("heartbeat", None, None, None)
    Ok(remote_envelope.RemoteServerCommand(command_id, _)) -> #(
      "server_command",
      Some(command_id),
      None,
      None,
    )
    Ok(remote_envelope.RemoteQueryRequest(query_id, _)) -> #(
      "query_request",
      Some(query_id),
      None,
      None,
    )
    Ok(remote_envelope.RemoteCommandReceipt(command_id, _, _)) -> #(
      "command_receipt",
      Some(command_id),
      None,
      None,
    )
    Ok(remote_envelope.RemoteCommandResult(command_id, result)) -> #(
      "command_result",
      Some(command_id),
      Some(command.status_to_string(result.status)),
      None,
    )
    Ok(remote_envelope.RemoteQueryResponse(query_id, _)) -> #(
      "query_response",
      Some(query_id),
      None,
      None,
    )
    Ok(remote_envelope.RemoteStateSnapshot(_, dispatch_paused, _)) -> #(
      "state_snapshot",
      None,
      None,
      Some(dispatch_paused),
    )
    Ok(remote_envelope.RemoteHello(_)) -> #("hello", None, None, None)
    Error(_) -> #(fallback_kind, None, None, None)
  }
}

fn transcript_json_string(
  run_nonce: String,
  bound_port: Int,
  events: List(TranscriptEvent),
  observations: List(remote_liveness.View),
) -> String {
  json.object([
    #("schema_version", json.int(1)),
    #("run_nonce", json.string(run_nonce)),
    #("bound_port", json.int(bound_port)),
    #("events", json.array(events, of: transcript_event_to_json)),
    #("liveness_observations", json.array(observations, of: view_to_json)),
  ])
  |> json.to_string
}

fn transcript_event_to_json(event: TranscriptEvent) -> json.Json {
  json.object([
    #("sequence", json.int(event.sequence)),
    #("connection_sequence", json.int(event.connection_sequence)),
    #("direction", json.string(event.direction)),
    #("kind", json.string(event.kind)),
    #("digest", option_string_to_json(event.digest)),
    #("redacted_line", option_string_to_json(event.redacted_line)),
    #("command_id", option_string_to_json(event.command_id)),
    #("command_status", option_string_to_json(event.command_status)),
    #("dispatch_paused", option_bool_to_json(event.dispatch_paused)),
  ])
}

fn view_to_json(view: remote_liveness.View) -> json.Json {
  json.object([
    #("daemon_id", json.string(view.daemon_id)),
    #("boot_id", json.string(view.boot_id)),
    #("status", json.string(remote_liveness.status_to_string(view.status))),
    #("last_seen_at_ms", json.int(view.last_seen_at_ms)),
    #("observed_at_ms", json.int(view.observed_at_ms)),
  ])
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_bool_to_json(value: Option(Bool)) -> json.Json {
  case value {
    Some(value) -> json.bool(value)
    None -> json.null()
  }
}

fn write_transcript_if_requested(
  transcript_path: Option(String),
  transcript_json: String,
) -> Result(Nil, HarnessError) {
  case transcript_path {
    None -> Ok(Nil)
    Some(path) -> {
      use _ <- result.try(ensure_directory(directory_name(path), path))
      simplifile.write(path, transcript_json <> "\n")
      |> result.map_error(fn(err) {
        HarnessError(
          "transcript_write_failed",
          path <> ": " <> simplifile.describe_error(err),
        )
      })
    }
  }
}

fn ensure_directory(dir: String, path: String) -> Result(Nil, HarnessError) {
  case dir == "" {
    True -> Ok(Nil)
    False ->
      simplifile.create_directory_all(dir)
      |> result.map_error(fn(err) {
        HarnessError(
          "transcript_directory_failed",
          path <> ": " <> simplifile.describe_error(err),
        )
      })
  }
}

fn directory_name(path: String) -> String {
  case string.split(path, "/") |> list.reverse {
    [] -> ""
    [_] -> ""
    [_, ..reversed_dir] ->
      reversed_dir |> list.reverse |> string.join(with: "/")
  }
}

fn prefixed_id(prefix: String, seed: String) -> String {
  prefix <> hash.short_sha256_hex(seed, 32)
}

fn generate_run_nonce() -> Result(String, HarnessError) {
  case control_file.generate_token() {
    Ok(token) ->
      Ok(hash.short_sha256_hex(
        token <> ":" <> int.to_string(monotonic_ms()),
        32,
      ))
    Error(err) ->
      Error(HarnessError("run_nonce_failed", control_file_error_message(err)))
  }
}

fn control_file_error_message(error: control_file.ControlFileError) -> String {
  case error {
    control_file.ControlFileNotFound(path) -> "control file not found: " <> path
    control_file.ControlFileReadFailed(path, message) ->
      "control file read failed " <> path <> ": " <> message
    control_file.ControlFileWriteFailed(path, message) ->
      "control file write failed " <> path <> ": " <> message
    control_file.ControlFileInvalid(path, message) ->
      "control file invalid " <> path <> ": " <> message
    control_file.ControlFilePermissionFailed(path, message) ->
      "control file permission failed " <> path <> ": " <> message
    control_file.TokenGenerationFailed(message) -> message
  }
}

fn socket_error(prefix: String) -> fn(String) -> HarnessError {
  fn(message: String) {
    HarnessError("socket_error", prefix <> ": " <> message)
  }
}

fn registry_error(error: remote_liveness.RegistryError) -> HarnessError {
  HarnessError("registry_error", remote_liveness.error_message(error))
}

fn hello_error(error: remote_harness_hello.HelloError) -> HarnessError {
  let remote_harness_hello.HelloError(code: code, message: message) = error
  HarnessError(code, message)
}

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "listen")
fn listen(host: String, port: Int) -> Result(Listener, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "accept")
fn accept(listener: Listener) -> Result(Socket, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "connect")
fn connect(host: String, port: Int, timeout_ms: Int) -> Result(Socket, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "send_line")
fn send_line(
  socket: Socket,
  line: String,
  timeout_ms: Int,
) -> Result(Nil, String)

// nolint: stringly_typed_error -- erlang socket ffi returns raw transport errors.
@external(erlang, "scherzo_control_ffi", "recv_line")
fn recv_line(socket: Socket, timeout_ms: Int) -> Result(String, String)

@external(erlang, "scherzo_control_ffi", "close_socket")
fn close_socket(socket: Socket) -> Nil

@external(erlang, "scherzo_control_ffi", "close_listener")
fn close_listener(listener: Listener) -> Nil

@external(erlang, "scherzo_control_ffi", "bound_port")
fn bound_port(listener: Listener) -> Int

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int
