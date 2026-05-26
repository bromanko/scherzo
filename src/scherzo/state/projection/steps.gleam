import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

pub const context_name = "steps"

pub fn attempt_key(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  run_id <> "\u{001f}" <> step_id <> "\u{001f}" <> int.to_string(attempt_index)
}

pub fn recovery_key(
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
) -> String {
  run_id
  <> "\u{001f}"
  <> step_id
  <> "\u{001f}"
  <> int.to_string(failed_attempt_index)
  <> "\u{001f}"
  <> int.to_string(recovery_attempt_number)
}

pub fn session_fact_values(
  status_workflow_id: String,
  status_workspace_name: String,
  status_workspace_path: String,
  fact_workflow_id: String,
  fact_workspace_name: String,
  fact_workspace_path: String,
  session_id: String,
  session_file: String,
  current_count: Int,
) -> #(Option(String), Option(String), Int) {
  let fact_count = current_count + 1
  case
    status_workflow_id == fact_workflow_id
    && status_workspace_name == fact_workspace_name
    && status_workspace_path == fact_workspace_path
  {
    True -> #(Some(session_id), Some(session_file), fact_count)
    False -> #(None, None, fact_count)
  }
}

pub fn next_attempt_index(
  attempts: Dict(String, status),
  attempt_identity: fn(status) -> #(String, String, Int),
  run_id: String,
  step_id: String,
) -> Int {
  attempts
  |> dict.values
  |> list.fold(0, fn(max_index, status) {
    case attempt_identity(status) {
      #(status_run_id, status_step_id, attempt_index) ->
        case
          status_run_id == run_id
          && status_step_id == step_id
          && attempt_index > max_index
        {
          True -> attempt_index
          False -> max_index
        }
    }
  })
  |> fn(value) { value + 1 }
}
