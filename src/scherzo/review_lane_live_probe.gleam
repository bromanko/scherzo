import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/file as control_file
import scherzo/error
import scherzo/json_value
import scherzo/path
import scherzo/pi/client
import scherzo/pi/command as pi_command
import scherzo/pi/protocol
import scherzo/result_artifact
import scherzo/review_lane_tools
import scherzo/runtime_bundle
import scherzo/step_artifact
import scherzo/structured_output
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag
import scherzo/workflow_structured_retry
import simplifile

pub type ProbeError {
  ProbeError(code: String, message: String)
}

type Options {
  Options(
    workflow: String,
    output_dir: String,
    skip_if_missing_credentials: Bool,
  )
}

pub type PhaseResult {
  PhaseResult(
    step_id: String,
    phase: String,
    status: String,
    code: Option(String),
    message: Option(String),
  )
}

pub type ProbeReport {
  ProbeReport(
    workflow: String,
    output_dir: String,
    status: String,
    code: Option(String),
    message: Option(String),
    phases: List(PhaseResult),
  )
}

pub fn probe_workflow(
  workflow_path: String,
  output_dir: String,
  skip_if_missing_credentials: Bool,
) -> Result(ProbeReport, ProbeError) {
  use bundle <- result.try(load_bundle())
  use dag <- result.try(load_workflow(workflow_path))
  let repository_root =
    structured_output.validator_repo_root(bundle.orchestrator.config_dir, ".")
  probe_loaded_workflow(
    workflow_path,
    dag,
    repository_root,
    bundle.effective,
    output_dir,
    skip_if_missing_credentials,
  )
}

pub fn probe_loaded_workflow(
  workflow_label: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  effective: config_types.EffectiveConfig,
  output_dir: String,
  skip_if_missing_credentials: Bool,
) -> Result(ProbeReport, ProbeError) {
  use Nil <- result.try(create_output_dir(output_dir))
  case live_credentials_present() {
    False ->
      missing_credentials_report(
        workflow_label,
        output_dir,
        skip_if_missing_credentials,
      )
    True ->
      run_provider_probe(
        workflow_label,
        dag,
        repository_root,
        effective,
        output_dir,
      )
  }
}

fn missing_credentials_report(
  workflow_label: String,
  output_dir: String,
  skip_if_missing_credentials: Bool,
) -> Result(ProbeReport, ProbeError) {
  let report = case skip_if_missing_credentials {
    True ->
      ProbeReport(
        workflow: workflow_label,
        output_dir: output_dir,
        status: "skipped",
        code: Some("skipped_missing_credentials"),
        message: Some("no supported provider credentials are present"),
        phases: [],
      )
    False ->
      ProbeReport(
        workflow: workflow_label,
        output_dir: output_dir,
        status: "failed",
        code: Some("review_lane_live_credentials_missing"),
        message: Some("live provider probe credentials are missing"),
        phases: [],
      )
  }
  use Nil <- result.try(write_report(output_dir, report))
  Ok(report)
}

fn run_provider_probe(
  workflow_label: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  effective: config_types.EffectiveConfig,
  output_dir: String,
) -> Result(ProbeReport, ProbeError) {
  use tools <- result.try(
    review_lane_tools.for_workflow(workflow_dag.id(dag), dag, repository_root)
    |> result.map_error(fn(err) {
      ProbeError(code: err.code, message: err.message)
    }),
  )
  let phases = probe_tools(tools, effective, output_dir, [])
  let report = report_for_phases(workflow_label, output_dir, tools, phases)
  use Nil <- result.try(write_report(output_dir, report))
  Ok(report)
}

fn probe_tools(
  tools: List(review_lane_tools.ReviewLaneTool),
  effective: config_types.EffectiveConfig,
  output_dir: String,
  acc: List(PhaseResult),
) -> List(PhaseResult) {
  case tools {
    [] -> list.reverse(acc)
    [tool, ..rest] -> {
      let registration =
        run_phase(
          tool,
          effective,
          output_dir,
          "registration",
          registration_prompt(tool.tool_spec),
        )
      let acc = [registration, ..acc]
      case registration.status == "passed" {
        False -> probe_tools(rest, effective, output_dir, acc)
        True -> {
          let repair =
            run_phase(
              tool,
              effective,
              output_dir,
              "repair",
              repair_prompt(tool, output_dir),
            )
          probe_tools(rest, effective, output_dir, [repair, ..acc])
        }
      }
    }
  }
}

fn run_phase(
  tool: review_lane_tools.ReviewLaneTool,
  effective: config_types.EffectiveConfig,
  output_dir: String,
  phase: String,
  prompt: String,
) -> PhaseResult {
  case structured_output_tool_spec.write(tool.tool_spec, output_dir) {
    Error(err) ->
      phase_failed(
        tool.step_id,
        phase,
        "provider_tool_registration_failed",
        err.message,
      )
    Ok(written) -> {
      let effective =
        config_types.with_pi_env(effective, [
          structured_output_tool_spec.env_pair(written),
        ])
      case pi_command.build_launch(effective.pi, pi_command.FreshNoSession) {
        Error(err) ->
          phase_failed(
            tool.step_id,
            phase,
            "provider_tool_registration_failed",
            error.config_message(err),
          )
        Ok(launch) ->
          case
            client.launch_spec(
              launch,
              output_dir,
              "Scherzo review-lane live probe " <> tool.step_id,
              False,
              effective.pi.read_timeout_ms,
            )
          {
            Error(err) ->
              phase_failed(
                tool.step_id,
                phase,
                "provider_tool_registration_failed",
                error.pi_rpc_detail(err),
              )
            Ok(session) ->
              run_phase_prompt(tool, effective, session, phase, prompt)
          }
      }
    }
  }
}

fn run_phase_prompt(
  tool: review_lane_tools.ReviewLaneTool,
  effective: config_types.EffectiveConfig,
  session: client.Session,
  phase: String,
  prompt: String,
) -> PhaseResult {
  case client.send_prompt(session, prompt, effective.pi.read_timeout_ms) {
    Error(err) -> {
      terminate_session(session)
      phase_failed(
        tool.step_id,
        phase,
        "provider_tool_call_failed",
        error.pi_rpc_detail(err),
      )
    }
    Ok(#(session, command_records)) ->
      case read_turn_records(session, effective, command_records) {
        Error(err) -> {
          terminate_session(session)
          phase_failed(
            tool.step_id,
            phase,
            "provider_tool_call_failed",
            error.pi_rpc_detail(err),
          )
        }
        Ok(#(session, records)) -> {
          terminate_session(session)
          let artifact = result_artifact.from_records(records, [], 8000)
          validate_phase_payload(tool, phase, artifact)
        }
      }
  }
}

fn read_turn_records(
  session: client.Session,
  effective: config_types.EffectiveConfig,
  command_records: List(protocol.RpcRecord),
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  read_turn_records_loop(
    session,
    effective,
    command_records,
    [],
    monotonic_ms() + effective.pi.turn_timeout_ms,
    monotonic_ms() + effective.pi.stall_timeout_ms,
  )
}

fn read_turn_records_loop(
  session: client.Session,
  effective: config_types.EffectiveConfig,
  command_records: List(protocol.RpcRecord),
  acc: List(protocol.RpcRecord),
  turn_deadline_ms: Int,
  stall_deadline_ms: Int,
) -> Result(#(client.Session, List(protocol.RpcRecord)), error.PiRpcError) {
  case
    client.read_turn_record(
      session,
      effective.pi.read_timeout_ms,
      turn_deadline_ms,
      stall_deadline_ms,
    )
  {
    Error(err) -> Error(err)
    Ok(#(session, None)) ->
      read_turn_records_loop(
        session,
        effective,
        command_records,
        acc,
        turn_deadline_ms,
        stall_deadline_ms,
      )
    Ok(#(session, Some(record))) -> {
      let acc = [record, ..acc]
      case record.type_ == "agent_end" {
        True -> Ok(#(session, list.append(command_records, list.reverse(acc))))
        False ->
          read_turn_records_loop(
            session,
            effective,
            command_records,
            acc,
            turn_deadline_ms,
            monotonic_ms() + effective.pi.stall_timeout_ms,
          )
      }
    }
  }
}

fn terminate_session(session: client.Session) -> Nil {
  case client.terminate(session) {
    Ok(Nil) -> Nil
    Error(err) ->
      io.println_error(
        "review-lane live probe cleanup warning: " <> error.pi_rpc_detail(err),
      )
  }
}

fn validate_phase_payload(
  tool: review_lane_tools.ReviewLaneTool,
  phase: String,
  artifact: result_artifact.ResultArtifact,
) -> PhaseResult {
  validate_phase_tool_calls(
    tool.step_id,
    phase,
    tool.tool_spec.tool_name,
    artifact.tool_calls,
  )
}

pub fn validate_phase_tool_calls(
  step_id: String,
  phase: String,
  tool_name: String,
  tool_calls: List(result_artifact.ToolCallSubmission),
) -> PhaseResult {
  let matching = list.filter(tool_calls, fn(call) { call.name == tool_name })
  let successful = list.filter(matching, result_artifact.tool_call_succeeded)
  case matching, successful {
    [], _ ->
      phase_failed(
        step_id,
        phase,
        payload_failure_code(phase),
        "provider response did not include the required review-lane tool call",
      )
    _, [] ->
      phase_failed(
        step_id,
        phase,
        "provider_tool_call_failed",
        "review-lane tool calls did not report successful completion",
      )
    _, [call] -> validate_tool_call(step_id, phase, call)
    _, _ ->
      phase_failed(
        step_id,
        phase,
        payload_failure_code(phase),
        "provider response included multiple successful review-lane tool calls",
      )
  }
}

fn validate_tool_call(
  step_id: String,
  phase: String,
  call: result_artifact.ToolCallSubmission,
) -> PhaseResult {
  case call.arguments_json {
    None ->
      phase_failed(
        step_id,
        phase,
        payload_failure_code(phase),
        "review-lane tool call did not include JSON arguments",
      )
    Some(arguments_json) ->
      case json_value.parse(arguments_json) {
        Error(Nil) ->
          phase_failed(
            step_id,
            phase,
            payload_failure_code(phase),
            "review-lane tool call arguments were not valid JSON",
          )
        Ok(json_value.JObject(entries)) ->
          validate_submission_object(step_id, phase, entries)
        Ok(_) ->
          phase_failed(
            step_id,
            phase,
            payload_failure_code(phase),
            "review-lane tool call arguments must be a JSON object",
          )
      }
  }
}

fn validate_submission_object(
  step_id: String,
  phase: String,
  entries: List(#(String, json_value.JsonValue)),
) -> PhaseResult {
  let missing =
    missing_submission_fields(entries, required_submission_fields(), [])
  let metadata = present_runner_metadata(entries, [])
  case missing, metadata {
    [], [] ->
      PhaseResult(
        step_id: step_id,
        phase: phase,
        status: "passed",
        code: None,
        message: None,
      )
    _, [] ->
      phase_failed(
        step_id,
        phase,
        payload_failure_code(phase),
        "review-lane tool call arguments are missing fields: "
          <> string.join(missing, with: ", "),
      )
    _, _ ->
      phase_failed(
        step_id,
        phase,
        payload_failure_code(phase),
        "review-lane tool call included runner-owned metadata fields: "
          <> string.join(metadata, with: ", "),
      )
  }
}

fn phase_failed(
  step_id: String,
  phase: String,
  code: String,
  message: String,
) -> PhaseResult {
  PhaseResult(
    step_id: step_id,
    phase: phase,
    status: "failed",
    code: Some(code),
    message: Some(message),
  )
}

fn registration_prompt(
  tool_spec: structured_output_tool_spec.ToolSpec,
) -> String {
  "Scherzo live review-lane provider canary. Register and call the Pi tool `"
  <> tool_spec.tool_name
  <> "` exactly once with a valid minimal review-lane submission. Use only model-owned fields: `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`. Do not include `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, or `remote_mutations`. Use this minimal payload: {\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"summary\":\"live provider canary passed\"}}. Do not create Linear runs, workspaces, comments, or remote mutations."
}

fn repair_prompt(
  tool: review_lane_tools.ReviewLaneTool,
  output_dir: String,
) -> String {
  let diagnostic =
    step_artifact.StructuredOutputRetryDiagnostic(
      attempt: 1,
      status: "validation_failed",
      failure_code: Some("structured_output_schema_invalid"),
      message: "The canary deliberately supplied invalid captured arguments `{}`; repair by calling the review-lane tool with a valid minimal model-owned submission.",
    )
  workflow_structured_retry.retry_prompt(
    tool.step_id,
    output_dir,
    ".",
    tool.spec,
    diagnostic,
  )
}

fn report_for_phases(
  workflow_path: String,
  output_dir: String,
  tools: List(review_lane_tools.ReviewLaneTool),
  phases: List(PhaseResult),
) -> ProbeReport {
  case tools, first_failed(phases) {
    [], _ ->
      ProbeReport(
        workflow: workflow_path,
        output_dir: output_dir,
        status: "failed",
        code: Some("provider_tool_registration_failed"),
        message: Some("workflow does not contain review-lane provider tools"),
        phases: phases,
      )
    _, Some(failed) ->
      ProbeReport(
        workflow: workflow_path,
        output_dir: output_dir,
        status: "failed",
        code: failed.code,
        message: failed.message,
        phases: phases,
      )
    _, None ->
      ProbeReport(
        workflow: workflow_path,
        output_dir: output_dir,
        status: "passed",
        code: None,
        message: Some(
          "provider registered review-lane tools and returned valid registration and repair payloads",
        ),
        phases: phases,
      )
  }
}

fn first_failed(phases: List(PhaseResult)) -> Option(PhaseResult) {
  case phases {
    [] -> None
    [phase, ..rest] ->
      case phase.status == "failed" {
        True -> Some(phase)
        False -> first_failed(rest)
      }
  }
}

fn payload_failure_code(phase: String) -> String {
  case phase == "repair" {
    True -> "repair_loop_failed"
    False -> "model_payload_invalid"
  }
}

fn required_submission_fields() -> List(String) {
  ["draft_findings", "review_notes", "evidence_requests", "self_check"]
}

fn runner_metadata_fields() -> List(String) {
  [
    "schema_version",
    "artifact_type",
    "generated_at_utc",
    "producer",
    "lane",
    "input_refs",
    "remote_mutations",
  ]
}

fn missing_submission_fields(
  entries: List(#(String, json_value.JsonValue)),
  required: List(String),
  acc: List(String),
) -> List(String) {
  case required {
    [] -> list.reverse(acc)
    [field, ..rest] ->
      case json_value.object_has_key(entries, field) {
        True -> missing_submission_fields(entries, rest, acc)
        False -> missing_submission_fields(entries, rest, [field, ..acc])
      }
  }
}

fn present_runner_metadata(
  entries: List(#(String, json_value.JsonValue)),
  acc: List(String),
) -> List(String) {
  case runner_metadata_fields() {
    [] -> list.reverse(acc)
    fields -> present_fields(entries, fields, acc)
  }
}

fn present_fields(
  entries: List(#(String, json_value.JsonValue)),
  fields: List(String),
  acc: List(String),
) -> List(String) {
  case fields {
    [] -> list.reverse(acc)
    [field, ..rest] ->
      case json_value.object_has_key(entries, field) {
        True -> present_fields(entries, rest, [field, ..acc])
        False -> present_fields(entries, rest, acc)
      }
  }
}

fn live_credentials_present() -> Bool {
  control_file.get_env("ANTHROPIC_API_KEY") != None
  || control_file.get_env("OPENAI_API_KEY") != None
  || control_file.get_env("GEMINI_API_KEY") != None
  || control_file.get_env("GOOGLE_API_KEY") != None
}

fn load_bundle() -> Result(runtime_bundle.RuntimeBundle, ProbeError) {
  runtime_bundle.load(None)
  |> result.map_error(fn(err) {
    ProbeError(code: err.code, message: err.message)
  })
}

fn load_workflow(
  workflow_path: String,
) -> Result(workflow_dag.WorkflowDag, ProbeError) {
  runtime_bundle.load_workflow_file(workflow_path)
  |> result.map_error(fn(err) {
    ProbeError(code: err.code, message: err.message)
  })
}

fn create_output_dir(output_dir: String) -> Result(Nil, ProbeError) {
  simplifile.create_directory_all(output_dir)
  |> result.map_error(fn(err) {
    ProbeError(
      code: "review_lane_live_probe_write_failed",
      message: "could not create output directory: "
        <> simplifile.describe_error(err),
    )
  })
}

fn write_report(
  output_dir: String,
  report: ProbeReport,
) -> Result(Nil, ProbeError) {
  simplifile.write(
    path.join(output_dir, "live-probe-report.v1.json"),
    report_to_json(report) |> json.to_string |> append_newline,
  )
  |> result.map_error(fn(err) {
    ProbeError(
      code: "review_lane_live_probe_write_failed",
      message: "could not write live probe report: "
        <> simplifile.describe_error(err),
    )
  })
}

fn append_newline(value: String) -> String {
  value <> "\n"
}

fn report_to_json(report: ProbeReport) -> json.Json {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string("review_lane_live_probe_report")),
    #("workflow", json.string(report.workflow)),
    #("output_dir", json.string(report.output_dir)),
    #("status", json.string(report.status)),
    #("code", optional_string_to_json(report.code)),
    #("message", optional_string_to_json(report.message)),
    #("remote_mutations", json.string("none")),
    #("phases", json.array(report.phases, of: phase_to_json)),
  ])
}

fn phase_to_json(phase: PhaseResult) -> json.Json {
  json.object([
    #("step_id", json.string(phase.step_id)),
    #("phase", json.string(phase.phase)),
    #("status", json.string(phase.status)),
    #("code", optional_string_to_json(phase.code)),
    #("message", optional_string_to_json(phase.message)),
  ])
}

fn optional_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(text) -> json.string(text)
    None -> json.null()
  }
}

fn parse_args(raw_args: List(String)) -> Result(Options, ProbeError) {
  let args = case raw_args {
    ["--", ..rest] -> rest
    _ -> raw_args
  }
  parse_args_loop(args, None, None, False)
}

fn parse_args_loop(
  args: List(String),
  workflow: Option(String),
  output_dir: Option(String),
  skip_if_missing_credentials: Bool,
) -> Result(Options, ProbeError) {
  case args {
    [] ->
      case workflow, output_dir {
        Some(workflow), Some(output_dir) ->
          Ok(Options(
            workflow: workflow,
            output_dir: output_dir,
            skip_if_missing_credentials: skip_if_missing_credentials,
          ))
        _, _ -> usage_error("--workflow and --output-dir are required")
      }
    ["--workflow", value, ..rest] ->
      parse_args_loop(
        rest,
        Some(value),
        output_dir,
        skip_if_missing_credentials,
      )
    ["--output-dir", value, ..rest] ->
      parse_args_loop(rest, workflow, Some(value), skip_if_missing_credentials)
    ["--skip-if-missing-credentials", ..rest] ->
      parse_args_loop(rest, workflow, output_dir, True)
    [other, ..] -> usage_error("unknown argument: " <> other)
  }
}

fn usage_error(message: String) -> Result(Options, ProbeError) {
  Error(ProbeError(
    code: "usage",
    message: message
      <> "\nUsage: gleam run -m scherzo/review_lane_live_probe -- --workflow <workflow> --output-dir <dir> [--skip-if-missing-credentials]",
  ))
}

pub fn main() -> Nil {
  case parse_args(args()) {
    Error(err) -> {
      io.println_error(err.message)
      halt(2)
    }
    Ok(options) ->
      case
        probe_workflow(
          options.workflow,
          options.output_dir,
          options.skip_if_missing_credentials,
        )
      {
        Error(err) -> {
          io.println_error(err.code <> ": " <> err.message)
          halt(1)
        }
        Ok(report) -> {
          io.println("REVIEW_LANE_LIVE_PROBE=" <> report.status)
          io.println(
            "REVIEW_LANE_LIVE_REPORT="
            <> path.join(options.output_dir, "live-probe-report.v1.json"),
          )
          case report.status == "passed" || report.status == "skipped" {
            True -> Nil
            False -> halt(1)
          }
        }
      }
  }
}

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int

@external(erlang, "scherzo_main_ffi", "args")
fn args() -> List(String)

@external(erlang, "scherzo_main_ffi", "halt")
fn halt(code: Int) -> Nil
