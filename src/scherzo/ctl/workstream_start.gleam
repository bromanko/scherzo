import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{type Option, None}
import gleam/string
import scherzo/ctl/workstream_helpers.{
  load_schedule_projection, try_workstream, workspace_root,
}
import scherzo/runtime_bundle
import scherzo/terminal/sanitize as terminal_sanitize
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workstream/start

pub fn run_from_handoff(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  workflow_id: String,
  action_id: String,
  handoff_ref: String,
  handoff_sha256: String,
  gate_decision_ids: List(String),
  line: fn(String) -> Nil,
  inline: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use root <- try_workstream(workspace_root(control_path, explicit_root))
  use projected <- try_workstream(load_schedule_projection(root))
  use contract <- try_workstream(load_workflow_contract(workflow_id))
  let checkpoint = workflow_checkpoint.ledger_writer(root, wall_clock_ms)
  use start_result <- try_workstream(
    start.from_handoff(
      workflow_id,
      action_id,
      handoff_ref,
      handoff_sha256,
      gate_decision_ids,
      contract,
      projected,
      checkpoint,
    )
    |> map_start_error,
  )
  print_start_result(start_result, json_output, line, inline)
  Ok(Nil)
}

pub fn run_from_input_bundle(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  workflow_id: String,
  action_id: String,
  input_bundle_ref: String,
  input_bundle_sha256: String,
  gate_decision_ids: List(String),
  line: fn(String) -> Nil,
  inline: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  use root <- try_workstream(workspace_root(control_path, explicit_root))
  use projected <- try_workstream(load_schedule_projection(root))
  use _contract <- try_workstream(load_workflow_contract(workflow_id))
  let checkpoint = workflow_checkpoint.ledger_writer(root, wall_clock_ms)
  use start_result <- try_workstream(
    start.from_input_bundle(
      workflow_id,
      action_id,
      input_bundle_ref,
      input_bundle_sha256,
      gate_decision_ids,
      projected,
      checkpoint,
    )
    |> map_start_error,
  )
  print_start_result(start_result, json_output, line, inline)
  Ok(Nil)
}

fn map_start_error(
  result: Result(a, start.StartError),
) -> Result(a, #(String, String)) {
  case result {
    Ok(value) -> Ok(value)
    Error(start.StartError(code, message)) -> Error(#(code, message))
  }
}

fn print_start_result(
  start_result: start.StartResult,
  json_output: Bool,
  line: fn(String) -> Nil,
  inline: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True -> line(start_result_to_json(start_result) |> json.to_string)
    False ->
      print_start_result_human(
        start_result,
        fn(text) { line(terminal_sanitize.text(text)) },
        fn(text) { inline(terminal_sanitize.text(text)) },
      )
  }
}

fn start_result_to_json(start_result: start.StartResult) -> json.Json {
  let #(status, outcome) = start_result_parts(start_result)
  json.object([
    #("status", json.string(status)),
    #("workstream_id", json.string(outcome.workstream_id)),
    #("phase_run_id", json.string(outcome.phase_run_id)),
    #("workflow_id", json.string(outcome.workflow_id)),
    #("action_id", json.string(outcome.action_id)),
    #("input_bundle_ref", json.string(outcome.input_bundle_ref)),
    #("input_bundle_sha256", json.string(outcome.input_bundle_sha256)),
    #("input_bundle_bytes", json.int(outcome.input_bundle_bytes)),
    #("idempotency_key", json.string(outcome.idempotency_key)),
    #(
      "contract_inputs",
      json.array(contract_input_names(outcome), of: json.string),
    ),
  ])
}

fn print_start_result_human(
  start_result: start.StartResult,
  line: fn(String) -> Nil,
  _inline: fn(String) -> Nil,
) -> Nil {
  let #(status, outcome) = start_result_parts(start_result)
  line(
    "workstream start "
    <> status
    <> ": "
    <> outcome.phase_run_id
    <> " workflow="
    <> outcome.workflow_id
    <> " action="
    <> outcome.action_id
    <> " input_bundle="
    <> outcome.input_bundle_ref
    <> " sha256="
    <> outcome.input_bundle_sha256,
  )
  line("workstream: " <> outcome.workstream_id)
  line(
    "contract_inputs: " <> string.join(contract_input_names(outcome), with: ","),
  )
}

fn start_result_parts(
  start_result: start.StartResult,
) -> #(String, start.StartOutcome) {
  case start_result {
    start.Queued(outcome) -> #("queued", outcome)
    start.Duplicate(outcome) -> #("duplicate", outcome)
  }
}

fn contract_input_names(outcome: start.StartOutcome) -> List(String) {
  outcome.contract_values.inputs
  |> dict.keys
  |> list.sort(by: string.compare)
}

fn load_workflow_contract(
  workflow_id: String,
) -> Result(Option(workflow_contract.Contract), #(String, String)) {
  case runtime_bundle.load_workflow_by_id(None, workflow_id) {
    Ok(#(_, dag)) -> Ok(dag.contract)
    Error(runtime_bundle.BundleError(code, message)) ->
      case code == "unknown_workflow_label" {
        True -> Error(#("workflow_lookup_failed:" <> code, message))
        False -> Error(#("workflow_config_load_failed:" <> code, message))
      }
  }
}

@external(erlang, "scherzo_time_ffi", "wall_clock_ms")
fn wall_clock_ms() -> Int
