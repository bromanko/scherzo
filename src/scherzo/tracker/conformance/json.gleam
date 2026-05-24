import birl
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/task
import scherzo/tracker/conformance/manifest_support
import scherzo/tracker/conformance/manifest_validation
import scherzo/tracker/conformance/profile
import scherzo/tracker/conformance/types

pub fn manifest_to_string(manifest: types.Manifest) -> String {
  manifest_to_json(manifest) |> json.to_string
}

pub fn manifest_to_json(manifest: types.Manifest) -> json.Json {
  let types.Manifest(
    schema_version: manifest_schema_version,
    adapter_kind: adapter_kind,
    driver: driver,
    profile: manifest_profile,
    fixtures: fixtures,
    probes: probes,
    hooks: hooks,
    report: report,
  ) = manifest

  json.object([
    #("schema_version", json.int(manifest_schema_version)),
    #("adapter_kind", json.string(adapter_kind)),
    #("driver", driver_to_json(driver)),
    #("profile", profile_config_to_json(manifest_profile)),
    #("fixtures", fixtures_to_json(fixtures)),
    #("probes", json.array(probes, of: probe_to_json)),
    #("hooks", hooks_to_json(hooks)),
    #("report", report_to_json(report)),
  ])
}

pub fn decode_manifest(
  contents: String,
) -> Result(types.Manifest, types.ManifestError) {
  case json.parse(contents, manifest_decoder()) {
    Ok(manifest) -> validate_manifest(manifest)
    Error(parse_error) -> {
      let _parse_error = parse_error
      Error(types.ManifestError(
        "invalid_manifest_json",
        "manifest must be valid conformance JSON",
      ))
    }
  }
}

pub fn request_to_string(request: types.DriverRequest) -> String {
  request_to_json(request) |> json.to_string
}

pub fn request_to_json(request: types.DriverRequest) -> json.Json {
  let types.DriverRequest(
    schema_version: request_schema_version,
    request_id: request_id,
    operation: operation,
    payload: payload,
  ) = request

  json.object([
    #("schema_version", json.int(request_schema_version)),
    #("request_id", json.string(request_id)),
    #("operation", json.string(profile.operation_to_string(operation))),
    #("payload", request_payload_to_json(payload)),
  ])
}

pub fn decode_request(contents: String) -> Result(types.DriverRequest, Nil) {
  json.parse(contents, request_decoder()) |> result.replace_error(Nil)
}

pub fn response_to_string(response: types.DriverResponse) -> String {
  response_to_json(response) |> json.to_string
}

pub fn response_to_json(response: types.DriverResponse) -> json.Json {
  case response {
    types.DriverResponseSuccess(
      schema_version: response_schema_version,
      request_id: request_id,
      result: response_result,
    ) ->
      json.object([
        #("schema_version", json.int(response_schema_version)),
        #("request_id", json.string(request_id)),
        #("ok", json.bool(True)),
        #("result", response_result_to_json(response_result)),
      ])
    types.DriverResponseError(
      schema_version: response_schema_version,
      request_id: request_id,
      error: driver_error,
    ) ->
      json.object([
        #("schema_version", json.int(response_schema_version)),
        #("request_id", json.string(request_id)),
        #("ok", json.bool(False)),
        #("error", driver_error_to_json(driver_error)),
      ])
  }
}

pub fn decode_response(contents: String) -> Result(types.DriverResponse, Nil) {
  json.parse(contents, response_decoder()) |> result.replace_error(Nil)
}

fn validate_manifest(
  manifest: types.Manifest,
) -> Result(types.Manifest, types.ManifestError) {
  let types.Manifest(
    schema_version: manifest_schema_version,
    adapter_kind: adapter_kind,
    driver: driver,
    profile: manifest_profile,
    fixtures: fixtures,
    probes: probes,
    ..,
  ) = manifest
  let types.ProfileConfig(
    name: name,
    capabilities: capabilities,
    requested_packs: requested_packs,
    adapter_operations: operations,
    retry_behavior: retry_behavior,
  ) = manifest_profile
  let types.FixtureConfig(task_file: task_file, tasks: fixture_tasks) = fixtures

  use Nil <- result.try(case manifest_schema_version == types.schema_version {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "unsupported_schema_version",
        "schema_version must equal 1",
      ))
  })

  use Nil <- result.try(validate_driver(driver))
  use Nil <- result.try(manifest_validation.validate_profile(
    name,
    capabilities,
    requested_packs,
    operations,
    retry_behavior,
  ))
  use Nil <- result.try(manifest_validation.validate_probe_requirements(
    requested_packs,
    probes,
  ))
  use Nil <- result.try(case valid_repository_relative_path(task_file) {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "invalid_repository_relative_path",
        "fixtures.task_file must be repository-relative and confined to the repository",
      ))
  })
  use Nil <- result.try(manifest_support.validate_tasks(
    fixture_tasks,
    adapter_kind,
  ))

  Ok(manifest)
}

fn validate_driver(
  driver: types.DriverConfig,
) -> Result(Nil, types.ManifestError) {
  case driver {
    types.CliDriverConfig(timeout_ms: timeout_ms, ..) ->
      validate_driver_timeout(timeout_ms)
    types.HttpDriverConfig(endpoint: endpoint, timeout_ms: timeout_ms) -> {
      use Nil <- result.try(validate_driver_timeout(timeout_ms))
      validate_http_endpoint(endpoint)
    }
  }
}

fn validate_http_endpoint(
  endpoint: types.HttpEndpointConfig,
) -> Result(Nil, types.ManifestError) {
  manifest_support.validate_endpoint(endpoint)
}

fn validate_driver_timeout(
  timeout_ms: Int,
) -> Result(Nil, types.ManifestError) {
  case timeout_ms >= 1 && timeout_ms <= types.max_driver_timeout_ms {
    True -> Ok(Nil)
    False ->
      Error(types.ManifestError(
        "invalid_driver_timeout",
        "driver.timeout_ms must be between 1 and "
          <> int.to_string(types.max_driver_timeout_ms),
      ))
  }
}

fn manifest_decoder() -> decode.Decoder(types.Manifest) {
  use manifest_schema_version <- decode.field("schema_version", decode.int)
  use adapter_kind <- decode.field("adapter_kind", decode.string)
  use driver <- decode.field("driver", driver_decoder())
  use manifest_profile <- decode.field("profile", profile_config_decoder())
  use fixtures <- decode.field("fixtures", fixtures_decoder())
  use probes <- decode.optional_field(
    "probes",
    [],
    decode.list(probe_decoder()),
  )
  use hooks <- decode.optional_field("hooks", empty_hooks(), hooks_decoder())
  use report <- decode.optional_field(
    "report",
    empty_report(),
    report_decoder(),
  )
  decode.success(types.Manifest(
    schema_version: manifest_schema_version,
    adapter_kind: adapter_kind,
    driver: driver,
    profile: manifest_profile,
    fixtures: fixtures,
    probes: probes,
    hooks: hooks,
    report: report,
  ))
}

fn driver_to_json(driver: types.DriverConfig) -> json.Json {
  case driver {
    types.CliDriverConfig(command: command, timeout_ms: timeout_ms) ->
      json.object([
        #(
          "transport",
          json.string(driver_transport_to_string(types.CliTransport)),
        ),
        #("command", driver_command_to_json(command)),
        #("timeout_ms", json.int(timeout_ms)),
      ])
    types.HttpDriverConfig(endpoint: endpoint, timeout_ms: timeout_ms) ->
      json.object([
        #(
          "transport",
          json.string(driver_transport_to_string(types.HttpTransport)),
        ),
        #("endpoint", manifest_support.endpoint_to_json(endpoint)),
        #("timeout_ms", json.int(timeout_ms)),
      ])
  }
}

fn driver_decoder() -> decode.Decoder(types.DriverConfig) {
  use transport <- decode.field("transport", driver_transport_decoder())
  case transport {
    types.CliTransport -> {
      use command <- decode.field("command", driver_command_decoder())
      use timeout_ms <- decode.field("timeout_ms", decode.int)
      decode.success(types.CliDriverConfig(
        command: command,
        timeout_ms: timeout_ms,
      ))
    }
    types.HttpTransport -> {
      use endpoint <- decode.field(
        "endpoint",
        manifest_support.endpoint_decoder(),
      )
      use timeout_ms <- decode.field("timeout_ms", decode.int)
      decode.success(types.HttpDriverConfig(
        endpoint: endpoint,
        timeout_ms: timeout_ms,
      ))
    }
  }
}

fn driver_transport_to_string(transport: types.DriverTransport) -> String {
  case transport {
    types.CliTransport -> "cli"
    types.HttpTransport -> "http"
  }
}

fn driver_transport_decoder() -> decode.Decoder(types.DriverTransport) {
  use transport <- decode.then(decode.string)
  case string.trim(transport) {
    "cli" -> decode.success(types.CliTransport)
    "http" -> decode.success(types.HttpTransport)
    _ ->
      decode.failure(
        types.CliTransport,
        expected: "driver transport cli or http",
      )
  }
}

fn driver_command_to_json(command: types.DriverCommand) -> json.Json {
  let types.DriverCommand(
    executable: executable,
    args: args,
    cwd: cwd,
    env: env,
  ) = command
  json.object([
    #("executable", json.string(executable)),
    #("args", json.array(args, of: json.string)),
    #("cwd", json.string(cwd)),
    #("env", json.array(env, of: env_var_to_json)),
  ])
}

fn driver_command_decoder() -> decode.Decoder(types.DriverCommand) {
  use executable <- decode.field("executable", decode.string)
  use args <- decode.optional_field("args", [], decode.list(decode.string))
  use cwd <- decode.optional_field("cwd", ".", decode.string)
  use env <- decode.optional_field("env", [], decode.list(env_var_decoder()))
  decode.success(types.DriverCommand(
    executable: executable,
    args: args,
    cwd: cwd,
    env: env,
  ))
}

fn env_var_to_json(env: types.EnvVar) -> json.Json {
  let types.EnvVar(name: name, value: value) = env
  json.object([#("name", json.string(name)), #("value", json.string(value))])
}

fn env_var_decoder() -> decode.Decoder(types.EnvVar) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  decode.success(types.EnvVar(name: name, value: value))
}

fn profile_config_to_json(manifest_profile: types.ProfileConfig) -> json.Json {
  let types.ProfileConfig(
    name: name,
    capabilities: capabilities,
    requested_packs: requested_packs,
    adapter_operations: operations,
    retry_behavior: retry_behavior,
  ) = manifest_profile
  json.object([
    #("name", json.string(profile.profile_name_to_string(name))),
    #(
      "capabilities",
      json.array(capabilities, of: fn(capability) {
        capability |> profile.capability_to_string |> json.string
      }),
    ),
    #(
      "requested_packs",
      json.array(requested_packs, of: fn(requested_pack) {
        requested_pack |> profile.pack_name_to_string |> json.string
      }),
    ),
    #(
      "adapter_operations",
      json.array(operations, of: fn(operation) {
        operation |> profile.operation_to_string |> json.string
      }),
    ),
    #(
      "retry_behavior",
      option_json(retry_behavior, manifest_support.retry_behavior_to_json),
    ),
  ])
}

fn profile_config_decoder() -> decode.Decoder(types.ProfileConfig) {
  use name <- decode.field("name", profile_name_decoder())
  use capabilities <- decode.field(
    "capabilities",
    decode.list(capability_decoder()),
  )
  use requested_packs <- decode.optional_field(
    "requested_packs",
    profile.default_requested_packs(),
    decode.list(requested_pack_decoder()),
  )
  use operations <- decode.field(
    "adapter_operations",
    adapter_operations_decoder(),
  )
  use retry_behavior <- decode.optional_field(
    "retry_behavior",
    None,
    decode.optional(manifest_support.retry_behavior_decoder()),
  )
  decode.success(types.ProfileConfig(
    name: name,
    capabilities: capabilities,
    requested_packs: requested_packs,
    adapter_operations: operations,
    retry_behavior: retry_behavior,
  ))
}

fn profile_name_decoder() -> decode.Decoder(profile.ProfileName) {
  use value <- decode.then(decode.string)
  case profile.profile_name_from_string(value) {
    Ok(name) -> decode.success(name)
    Error(Nil) ->
      decode.failure(profile.TaskSourceProfile, expected: "profile name")
  }
}

fn capability_decoder() -> decode.Decoder(profile.Capability) {
  use value <- decode.then(decode.string)
  decode.success(profile.capability_from_string(value))
}

fn requested_pack_decoder() -> decode.Decoder(profile.PackName) {
  use value <- decode.then(decode.string)
  decode.success(profile.pack_name_from_string(value))
}

fn adapter_operations_decoder() -> decode.Decoder(
  List(profile.AdapterOperation),
) {
  decode.list(decode.string)
  |> decode.map(fn(values) { list_map(values, profile.operation_from_string) })
}

fn fixtures_to_json(fixtures: types.FixtureConfig) -> json.Json {
  let types.FixtureConfig(task_file: task_file, tasks: tasks) = fixtures
  json.object([
    #("task_file", json.string(task_file)),
    #("tasks", manifest_support.tasks_to_json(tasks)),
  ])
}

fn fixtures_decoder() -> decode.Decoder(types.FixtureConfig) {
  use task_file <- decode.field("task_file", decode.string)
  use tasks <- decode.optional_field(
    "tasks",
    [],
    manifest_support.tasks_decoder(),
  )
  decode.success(types.FixtureConfig(task_file: task_file, tasks: tasks))
}

fn probe_to_json(probe: types.ProbeConfig) -> json.Json {
  let types.ProbeConfig(name: name, command: command) = probe
  json.object([
    #("name", json.string(name)),
    #("command", hook_command_to_json(command)),
  ])
}

fn probe_decoder() -> decode.Decoder(types.ProbeConfig) {
  use name <- decode.field("name", decode.string)
  use command <- decode.field("command", hook_command_decoder())
  decode.success(types.ProbeConfig(name: name, command: command))
}

fn hooks_to_json(hooks: types.HooksConfig) -> json.Json {
  let types.HooksConfig(setup: setup, cleanup: cleanup) = hooks
  json.object([
    #("setup", option_json(setup, hook_command_to_json)),
    #("cleanup", option_json(cleanup, hook_command_to_json)),
  ])
}

fn hooks_decoder() -> decode.Decoder(types.HooksConfig) {
  use setup <- decode.optional_field(
    "setup",
    None,
    decode.optional(hook_command_decoder()),
  )
  use cleanup <- decode.optional_field(
    "cleanup",
    None,
    decode.optional(hook_command_decoder()),
  )
  decode.success(types.HooksConfig(setup: setup, cleanup: cleanup))
}

fn hook_command_to_json(command: types.HookCommand) -> json.Json {
  let types.HookCommand(executable: executable, args: args, cwd: cwd) = command
  json.object([
    #("executable", json.string(executable)),
    #("args", json.array(args, of: json.string)),
    #("cwd", json.string(cwd)),
  ])
}

fn hook_command_decoder() -> decode.Decoder(types.HookCommand) {
  use executable <- decode.field("executable", decode.string)
  use args <- decode.optional_field("args", [], decode.list(decode.string))
  use cwd <- decode.optional_field("cwd", ".", decode.string)
  decode.success(types.HookCommand(executable: executable, args: args, cwd: cwd))
}

fn report_to_json(report: types.ReportConfig) -> json.Json {
  let types.ReportConfig(redact: redact) = report
  json.object([#("redact", json.array(redact, of: json.string))])
}

fn report_decoder() -> decode.Decoder(types.ReportConfig) {
  use redact <- decode.optional_field("redact", [], decode.list(decode.string))
  decode.success(types.ReportConfig(redact: redact))
}

fn request_payload_to_json(payload: types.RequestPayload) -> json.Json {
  case payload {
    types.FetchCandidatesPayload(task_search: task_search) ->
      task_search_to_json(task_search)
    types.RefreshByRefsPayload(refs: refs) ->
      json.object([#("refs", json.array(refs, of: task_ref_to_json))])
    types.LookupByOperatorRefPayload(operator_ref: operator_ref) ->
      json.object([#("operator_ref", json.string(operator_ref))])
    types.CommentsPostOrUpdatePayload(comment: comment) ->
      manifest_support.comment_request_to_json(comment)
    types.RemoteCommandsFetchPayload(fetch: fetch) ->
      manifest_support.remote_command_fetch_to_json(fetch)
    types.RemoteCommandsPostAckPayload(ack: ack) ->
      manifest_support.remote_command_ack_to_json(ack)
    types.StateTransitionPayload(transition: transition) ->
      manifest_support.state_transition_request_to_json(transition)
    types.HandoffReportPayload(event: event) ->
      manifest_support.handoff_event_to_json(event)
  }
}

fn request_decoder() -> decode.Decoder(types.DriverRequest) {
  use request_schema_version <- decode.field("schema_version", decode.int)
  use request_id <- decode.field("request_id", decode.string)
  use operation <- decode.field("operation", operation_decoder())
  use payload <- decode.field("payload", request_payload_decoder(operation))
  decode.success(types.DriverRequest(
    schema_version: request_schema_version,
    request_id: request_id,
    operation: operation,
    payload: payload,
  ))
}

fn request_payload_decoder(
  operation: profile.AdapterOperation,
) -> decode.Decoder(types.RequestPayload) {
  case operation {
    profile.TaskSourceFetchCandidates ->
      task_search_decoder()
      |> decode.map(fn(task_search) {
        types.FetchCandidatesPayload(task_search: task_search)
      })
    profile.TaskSourceRefreshByRefs -> refresh_by_refs_payload_decoder()
    profile.TaskSourceLookupByOperatorRef ->
      lookup_by_operator_ref_payload_decoder()
    profile.CommentsPostOrUpdate ->
      manifest_support.comment_request_payload_decoder()
    profile.RemoteCommandsFetchEvents ->
      manifest_support.remote_command_fetch_payload_decoder()
    profile.RemoteCommandsPostAck ->
      manifest_support.remote_command_ack_payload_decoder()
    profile.StateTransitionsTransition ->
      manifest_support.state_transition_payload_decoder()
    profile.HandoffReport -> manifest_support.handoff_report_payload_decoder()
    _ ->
      decode.failure(
        types.FetchCandidatesPayload(task_search: types.TaskSearchPayload(
          [],
          [],
          [],
          [],
          0,
        )),
        expected: "known request payload operation",
      )
  }
}

fn refresh_by_refs_payload_decoder() -> decode.Decoder(types.RequestPayload) {
  use refs <- decode.field("refs", decode.list(task_ref_decoder()))
  decode.success(types.RefreshByRefsPayload(refs: refs))
}

fn lookup_by_operator_ref_payload_decoder() -> decode.Decoder(
  types.RequestPayload,
) {
  use operator_ref <- decode.field("operator_ref", decode.string)
  decode.success(types.LookupByOperatorRefPayload(operator_ref: operator_ref))
}

fn task_search_to_json(task_search: types.TaskSearchPayload) -> json.Json {
  let types.TaskSearchPayload(
    active_states: active_states,
    dispatch_states: dispatch_states,
    terminal_states: terminal_states,
    workflow_labels: workflow_labels,
    limit: limit,
  ) = task_search
  json.object([
    #("active_states", json.array(active_states, of: json.string)),
    #("dispatch_states", json.array(dispatch_states, of: json.string)),
    #("terminal_states", json.array(terminal_states, of: json.string)),
    #("workflow_labels", json.array(workflow_labels, of: json.string)),
    #("limit", json.int(limit)),
  ])
}

fn task_search_decoder() -> decode.Decoder(types.TaskSearchPayload) {
  use active_states <- decode.optional_field(
    "active_states",
    [],
    decode.list(decode.string),
  )
  use dispatch_states <- decode.optional_field(
    "dispatch_states",
    [],
    decode.list(decode.string),
  )
  use terminal_states <- decode.optional_field(
    "terminal_states",
    [],
    decode.list(decode.string),
  )
  use workflow_labels <- decode.optional_field(
    "workflow_labels",
    [],
    decode.list(decode.string),
  )
  use limit <- decode.field("limit", decode.int)
  decode.success(types.TaskSearchPayload(
    active_states: active_states,
    dispatch_states: dispatch_states,
    terminal_states: terminal_states,
    workflow_labels: workflow_labels,
    limit: limit,
  ))
}

fn response_result_to_json(result_value: types.ResponseResult) -> json.Json {
  case result_value {
    types.TaskListResult(tasks: tasks) ->
      json.object([#("tasks", json.array(tasks, of: task_to_json))])
    types.OptionalTaskResult(task: maybe_task) ->
      json.object([#("task", option_json(maybe_task, task_to_json))])
    types.CommentResult(comment: comment) ->
      json.object([
        #("comment", manifest_support.comment_receipt_to_json(comment)),
      ])
    types.RemoteCommandEventsResult(events: events) ->
      json.object([
        #(
          "events",
          json.array(events, of: manifest_support.remote_command_event_to_json),
        ),
      ])
    types.RemoteCommandAckResult(comment: comment) ->
      json.object([
        #("ack", manifest_support.comment_receipt_to_json(comment)),
      ])
    types.StateTransitionResult(transition: transition) ->
      json.object([
        #(
          "transition",
          manifest_support.state_transition_receipt_to_json(transition),
        ),
      ])
    types.HandoffReportResult(receipt: receipt) ->
      json.object([
        #("handoff", manifest_support.handoff_report_receipt_to_json(receipt)),
      ])
  }
}

fn response_decoder() -> decode.Decoder(types.DriverResponse) {
  use response_schema_version <- decode.field("schema_version", decode.int)
  use request_id <- decode.field("request_id", decode.string)
  use ok <- decode.field("ok", decode.bool)
  case ok {
    True -> {
      use response_result <- decode.field("result", response_result_decoder())
      decode.success(types.DriverResponseSuccess(
        schema_version: response_schema_version,
        request_id: request_id,
        result: response_result,
      ))
    }
    False -> {
      use driver_error <- decode.field("error", driver_error_decoder())
      decode.success(types.DriverResponseError(
        schema_version: response_schema_version,
        request_id: request_id,
        error: driver_error,
      ))
    }
  }
}

fn response_result_decoder() -> decode.Decoder(types.ResponseResult) {
  use tasks <- decode.optional_field(
    "tasks",
    None,
    decode.optional(decode.list(task_decoder())),
  )
  use maybe_task <- decode.optional_field(
    "task",
    None,
    decode.optional(task_decoder()),
  )
  use comment <- decode.optional_field(
    "comment",
    None,
    decode.optional(manifest_support.comment_receipt_decoder()),
  )
  use events <- decode.optional_field(
    "events",
    None,
    decode.optional(
      decode.list(manifest_support.remote_command_event_decoder()),
    ),
  )
  use ack <- decode.optional_field(
    "ack",
    None,
    decode.optional(manifest_support.comment_receipt_decoder()),
  )
  use transition <- decode.optional_field(
    "transition",
    None,
    decode.optional(manifest_support.state_transition_receipt_decoder()),
  )
  use handoff <- decode.optional_field(
    "handoff",
    None,
    decode.optional(manifest_support.handoff_report_receipt_decoder()),
  )
  case tasks, maybe_task, comment, events, ack, transition, handoff {
    Some(tasks), _, _, _, _, _, _ ->
      decode.success(types.TaskListResult(tasks: tasks))
    None, Some(task_value), _, _, _, _, _ ->
      decode.success(types.OptionalTaskResult(task: Some(task_value)))
    None, None, Some(comment_value), _, _, _, _ ->
      decode.success(types.CommentResult(comment: comment_value))
    None, None, None, Some(events_value), _, _, _ ->
      decode.success(types.RemoteCommandEventsResult(events: events_value))
    None, None, None, None, Some(comment_value), _, _ ->
      decode.success(types.RemoteCommandAckResult(comment: comment_value))
    None, None, None, None, None, Some(transition_value), _ ->
      decode.success(types.StateTransitionResult(transition: transition_value))
    None, None, None, None, None, None, Some(receipt) ->
      decode.success(types.HandoffReportResult(receipt: receipt))
    None, None, None, None, None, None, None ->
      decode.success(types.OptionalTaskResult(task: None))
  }
}

fn driver_error_to_json(driver_error: types.DriverError) -> json.Json {
  let types.DriverError(
    kind: kind,
    message: message,
    ref: ref,
    capability: capability,
  ) = driver_error
  json.object([
    #("kind", json.string(driver_error_kind_to_string(kind))),
    #("message", json.string(message)),
    #("ref", option_json(ref, task_ref_to_json)),
    #("capability", option_json(capability, json.string)),
  ])
}

fn driver_error_decoder() -> decode.Decoder(types.DriverError) {
  use kind <- decode.field("kind", driver_error_kind_decoder())
  use message <- decode.field("message", decode.string)
  use ref <- decode.optional_field(
    "ref",
    None,
    decode.optional(task_ref_decoder()),
  )
  use capability <- decode.optional_field(
    "capability",
    None,
    decode.optional(decode.string),
  )
  decode.success(types.DriverError(
    kind: kind,
    message: message,
    ref: ref,
    capability: capability,
  ))
}

fn driver_error_kind_to_string(kind: types.DriverErrorKind) -> String {
  case kind {
    types.UnauthorizedError -> "unauthorized"
    types.NotFoundError -> "not_found"
    types.TransientError -> "transient"
    types.PermanentError -> "permanent"
    types.UnsupportedCapabilityError -> "unsupported_capability"
    types.DecodeFailedError -> "decode_failed"
  }
}

fn driver_error_kind_decoder() -> decode.Decoder(types.DriverErrorKind) {
  use value <- decode.then(decode.string)
  case string.trim(value) {
    "unauthorized" -> decode.success(types.UnauthorizedError)
    "not_found" -> decode.success(types.NotFoundError)
    "transient" -> decode.success(types.TransientError)
    "permanent" -> decode.success(types.PermanentError)
    "unsupported_capability" -> decode.success(types.UnsupportedCapabilityError)
    "decode_failed" -> decode.success(types.DecodeFailedError)
    _ -> decode.failure(types.PermanentError, expected: "driver error kind")
  }
}

fn operation_decoder() -> decode.Decoder(profile.AdapterOperation) {
  use value <- decode.then(decode.string)
  let operation = profile.operation_from_string(value)
  case operation {
    profile.TaskSourceFetchCandidates -> decode.success(operation)
    profile.TaskSourceRefreshByRefs -> decode.success(operation)
    profile.TaskSourceLookupByOperatorRef -> decode.success(operation)
    profile.CommentsPostOrUpdate -> decode.success(operation)
    profile.RemoteCommandsFetchEvents -> decode.success(operation)
    profile.RemoteCommandsPostAck -> decode.success(operation)
    profile.StateTransitionsTransition -> decode.success(operation)
    profile.HandoffReport -> decode.success(operation)
    _ ->
      decode.failure(
        profile.TaskSourceFetchCandidates,
        expected: "known operation",
      )
  }
}

fn task_ref_to_json(ref: task.TaskRef) -> json.Json {
  let task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ) = ref
  json.object([
    #("backend_kind", json.string(backend_kind)),
    #("remote_id", json.string(remote_id)),
    #("key", option_json(key, json.string)),
    #("url", option_json(url, json.string)),
  ])
}

fn task_ref_decoder() -> decode.Decoder(task.TaskRef) {
  use backend_kind <- decode.field("backend_kind", decode.string)
  use remote_id <- decode.field("remote_id", decode.string)
  use key <- decode.optional_field("key", None, decode.optional(decode.string))
  use url <- decode.optional_field("url", None, decode.optional(decode.string))
  decode.success(task.TaskRef(
    backend_kind: backend_kind,
    remote_id: remote_id,
    key: key,
    url: url,
  ))
}

fn task_to_json(value: task.Task) -> json.Json {
  let task.Task(
    ref: ref,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_hint: branch_hint,
    labels: labels,
    blockers: blockers,
    blockers_complete: blockers_complete,
    created_at: created_at,
    updated_at: updated_at,
  ) = value
  json.object([
    #("ref", task_ref_to_json(ref)),
    #("title", json.string(title)),
    #("description", option_json(description, json.string)),
    #("priority", option_json(priority, json.int)),
    #("state", task_state_to_json(state)),
    #("branch_hint", option_json(branch_hint, json.string)),
    #("labels", json.array(labels, of: task_label_to_json)),
    #("blockers", json.array(blockers, of: task_ref_to_json)),
    #("blockers_complete", json.bool(blockers_complete)),
    #("created_at", option_json(created_at, time_to_json)),
    #("updated_at", option_json(updated_at, time_to_json)),
  ])
}

fn task_decoder() -> decode.Decoder(task.Task) {
  use ref <- decode.field("ref", task_ref_decoder())
  use title <- decode.field("title", decode.string)
  use description <- decode.optional_field(
    "description",
    None,
    decode.optional(decode.string),
  )
  use priority <- decode.optional_field(
    "priority",
    None,
    decode.optional(decode.int),
  )
  use state <- decode.field("state", task_state_decoder())
  use branch_hint <- decode.optional_field(
    "branch_hint",
    None,
    decode.optional(decode.string),
  )
  use labels <- decode.optional_field(
    "labels",
    [],
    decode.list(task_label_decoder()),
  )
  use blockers <- decode.optional_field(
    "blockers",
    [],
    decode.list(task_ref_decoder()),
  )
  use blockers_complete <- decode.field("blockers_complete", decode.bool)
  use created_at <- decode.optional_field(
    "created_at",
    None,
    optional_time_value_decoder("created_at"),
  )
  use updated_at <- decode.optional_field(
    "updated_at",
    None,
    optional_time_value_decoder("updated_at"),
  )
  decode.success(task.Task(
    ref: ref,
    title: title,
    description: description,
    priority: priority,
    state: state,
    branch_hint: branch_hint,
    labels: labels,
    blockers: blockers,
    blockers_complete: blockers_complete,
    created_at: created_at,
    updated_at: updated_at,
  ))
}

fn task_state_to_json(state: task.TaskState) -> json.Json {
  let task.TaskState(id: id, name: name, category: category) = state
  json.object([
    #("id", option_json(id, json.string)),
    #("name", json.string(name)),
    #("category", json.string(task_state_category_to_string(category))),
  ])
}

fn task_state_decoder() -> decode.Decoder(task.TaskState) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  use category <- decode.field("category", task_state_category_decoder())
  decode.success(task.TaskState(id: id, name: name, category: category))
}

fn task_state_category_to_string(category: task.TaskStateCategory) -> String {
  case category {
    task.Backlog -> "backlog"
    task.Ready -> "ready"
    task.Active -> "active"
    task.Done -> "done"
    task.Canceled -> "canceled"
    task.Duplicate -> "duplicate"
    task.Unknown -> "unknown"
  }
}

fn task_state_category_decoder() -> decode.Decoder(task.TaskStateCategory) {
  use value <- decode.then(decode.string)
  case string.trim(value) {
    "backlog" -> decode.success(task.Backlog)
    "ready" -> decode.success(task.Ready)
    "active" -> decode.success(task.Active)
    "done" -> decode.success(task.Done)
    "canceled" -> decode.success(task.Canceled)
    "duplicate" -> decode.success(task.Duplicate)
    "unknown" -> decode.success(task.Unknown)
    _ -> decode.failure(task.Unknown, expected: "task state category")
  }
}

fn task_label_to_json(label: task.TaskLabel) -> json.Json {
  let task.TaskLabel(id: id, name: name) = label
  json.object([
    #("id", option_json(id, json.string)),
    #("name", json.string(name)),
  ])
}

fn task_label_decoder() -> decode.Decoder(task.TaskLabel) {
  use id <- decode.optional_field("id", None, decode.optional(decode.string))
  use name <- decode.field("name", decode.string)
  decode.success(task.TaskLabel(id: id, name: name))
}

fn time_to_json(value: birl.Time) -> json.Json {
  value |> birl.to_iso8601 |> json.string
}

fn optional_time_value_decoder(
  name: String,
) -> decode.Decoder(Option(birl.Time)) {
  use value <- decode.then(decode.optional(decode.string))
  case value {
    Some(text) ->
      case birl.parse(text) {
        Ok(time) -> decode.success(Some(time))
        Error(parse_error) -> {
          let _parse_error = parse_error
          decode.failure(None, expected: name <> " ISO-8601 timestamp")
        }
      }
    None -> decode.success(None)
  }
}

fn option_json(value: Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case value {
    Some(inner) -> encoder(inner)
    None -> json.null()
  }
}

fn empty_hooks() -> types.HooksConfig {
  types.HooksConfig(setup: None, cleanup: None)
}

fn empty_report() -> types.ReportConfig {
  types.ReportConfig(redact: [])
}

fn valid_repository_relative_path(value: String) -> Bool {
  value != ""
  && !string.starts_with(value, "/")
  && !has_windows_absolute_prefix(value)
  && !has_parent_segment(value)
  && !has_backslash_parent_segment(value)
}

fn list_map(values: List(a), mapper: fn(a) -> b) -> List(b) {
  case values {
    [] -> []
    [value, ..rest] -> [mapper(value), ..list_map(rest, mapper)]
  }
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn has_backslash_parent_segment(value: String) -> Bool {
  value == "..\\"
  || string.starts_with(value, "..\\")
  || string.ends_with(value, "\\..")
  || string.contains(value, "\\..\\")
}

fn has_windows_absolute_prefix(value: String) -> Bool {
  let length = string.length(value)
  length >= 3
  && is_ascii_letter(string.slice(value, 0, 1))
  && string.slice(value, 1, 1) == ":"
  && windows_separator(string.slice(value, 2, 1))
}

fn windows_separator(value: String) -> Bool {
  value == "\\" || value == "/"
}

fn is_ascii_letter(value: String) -> Bool {
  case value {
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    _ -> False
  }
}
