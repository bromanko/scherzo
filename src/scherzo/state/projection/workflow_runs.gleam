import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, from_result}

pub const context_name = "workflow_runs"

pub fn preserve_or_insert_task_ref(
  refs: Dict(String, task_ref),
  run_id: String,
  fallback: task_ref,
) -> Dict(String, task_ref) {
  case dict.has_key(refs, run_id) {
    True -> refs
    False -> dict.insert(refs, run_id, fallback)
  }
}

pub fn workflow_input_manifest(
  manifests: Dict(String, manifest),
  run_id: String,
) -> Option(manifest) {
  dict.get(manifests, run_id) |> from_result
}

pub fn workflow_output_manifest(
  manifests: Dict(String, manifest),
  run_id: String,
) -> Option(manifest) {
  dict.get(manifests, run_id) |> from_result
}

pub fn workflow_interface_snapshot(
  snapshots: Dict(String, snapshot),
  run_id: String,
) -> Option(snapshot) {
  dict.get(snapshots, run_id) |> from_result
}

pub fn latest_workflow_repair(
  repairs: Dict(String, repair),
  run_id: String,
) -> Option(repair) {
  dict.get(repairs, run_id) |> from_result
}

pub fn active_entries(
  runs: Dict(String, run_status),
  is_active: fn(run_status) -> Bool,
) -> List(#(String, run_status)) {
  runs
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(_, status) = entry
    is_active(status)
  })
}

pub fn has_run(runs: Dict(String, run_status), run_id: String) -> Bool {
  dict.has_key(runs, run_id)
}

pub fn run_root(
  runs: Dict(String, run_status),
  run_id: String,
  root_of: fn(run_status) -> String,
) -> String {
  case dict.get(runs, run_id) {
    Ok(status) -> root_of(status)
    Error(Nil) -> ""
  }
}
