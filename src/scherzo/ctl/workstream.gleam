import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/file
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/ledger
import scherzo/state/projection as state_projection
import scherzo/state/record
import scherzo/terminal/sanitize as terminal_sanitize
import scherzo/workstream/projection as workstream_projection
import scherzo/workstream/projection_json as workstream_projection_json
import scherzo/workstream/projection_snapshot as workstream_projection_snapshot

pub type Command {
  List(
    control_path: Option(String),
    root: Option(String),
    json_output: Bool,
    task_ref: Option(String),
  )
  Show(
    control_path: Option(String),
    root: Option(String),
    json_output: Bool,
    workstream_ref: String,
  )
}

type Output {
  Output(line: fn(String) -> Nil, inline: fn(String) -> Nil)
}

pub fn parse(
  args: List(String),
  control_path: Option(String),
  root: Option(String),
  json_output: Bool,
) -> Result(Command, String) {
  case args {
    ["list"] ->
      Ok(List(
        control_path: control_path,
        root: root,
        json_output: json_output,
        task_ref: None,
      ))
    ["list", task_ref] ->
      Ok(List(
        control_path: control_path,
        root: root,
        json_output: json_output,
        task_ref: Some(task_ref),
      ))
    ["show", workstream_ref] | ["inspect", workstream_ref] ->
      Ok(Show(
        control_path: control_path,
        root: root,
        json_output: json_output,
        workstream_ref: workstream_ref,
      ))
    _ -> Error(usage())
  }
}

pub fn usage() -> String {
  "workstream usage: workstream list [task-ref] | show <workstream-id-or-task-ref>"
}

pub fn run(
  command: Command,
  line: fn(String) -> Nil,
  inline: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let output = Output(line: line, inline: inline)
  case command {
    List(control_path, root, json_output, task_ref) ->
      run_list(control_path, root, json_output, task_ref, output)
    Show(control_path, root, json_output, workstream_ref) ->
      run_show(control_path, root, json_output, workstream_ref, output)
  }
}

fn run_list(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  task_ref: Option(String),
  output: Output,
) -> Result(Nil, #(String, String)) {
  use root <- try_workstream(workspace_root(control_path, explicit_root))
  use projected <- try_workstream(load_schedule_projection(root))
  let store = state_artifact_store.filesystem(root)
  let summaries = case task_ref {
    None -> workstream_projection.summaries(projected, store)
    Some(task_ref) ->
      workstream_projection.summaries_for_ref(projected, store, task_ref)
  }
  case json_output {
    True ->
      output.line(
        json.object([
          #(
            "workstreams",
            json.array(
              summaries,
              of: workstream_projection_json.summary_to_json,
            ),
          ),
        ])
        |> json.to_string,
      )
    False ->
      print_workstream_summaries(summaries, sanitized_text_output(output))
  }
  Ok(Nil)
}

fn run_show(
  control_path: Option(String),
  explicit_root: Option(String),
  json_output: Bool,
  workstream_ref: String,
  output: Output,
) -> Result(Nil, #(String, String)) {
  use root <- try_workstream(workspace_root(control_path, explicit_root))
  use projected <- try_workstream(load_schedule_projection(root))
  let store = state_artifact_store.filesystem(root)
  let inspections =
    workstream_projection.inspect_by_ref(projected, store, workstream_ref)
  case inspections {
    [] -> Error(#("workstream_not_found", "workstream not found"))
    _ -> {
      case json_output {
        True ->
          output.line(
            json.object([
              #(
                "workstreams",
                json.array(
                  inspections,
                  of: workstream_projection_json.inspection_to_json,
                ),
              ),
            ])
            |> json.to_string,
          )
        False ->
          print_workstream_inspections(
            inspections,
            sanitized_text_output(output),
          )
      }
      Ok(Nil)
    }
  }
}

fn sanitized_text_output(output: Output) -> Output {
  Output(
    line: fn(text) { output.line(terminal_sanitize.text(text)) },
    inline: fn(text) { output.inline(terminal_sanitize.text(text)) },
  )
}

fn workspace_root(
  control_path: Option(String),
  explicit_root: Option(String),
) -> Result(String, #(String, String)) {
  case explicit_root {
    Some(root) -> Ok(file.resolve_cli_path(root, file.get_env))
    None -> {
      use control_file <- try_workstream(load_control_file(control_path))
      Ok(control_file.workspace_root)
    }
  }
}

fn load_schedule_projection(
  root: String,
) -> Result(state_projection.Projection, #(String, String)) {
  case ledger.path_for_workspace_root(root) {
    Error(_) -> Error(#("ledger_path_failed", "could not resolve ledger path"))
    Ok(ledger_path) ->
      case ledger.load_projection(ledger_path) {
        Ok(projected) -> Ok(projected)
        Error(_) ->
          Error(#("ledger_load_failed", "could not load local ledger"))
      }
  }
}

fn load_control_file(
  explicit_path: Option(String),
) -> Result(file.ControlFile, #(String, String)) {
  file.discover(explicit_path, file.get_env) |> map_file_error
}

fn map_file_error(
  result: Result(a, file.ControlFileError),
) -> Result(a, #(String, String)) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(file_error(err))
  }
}

fn file_error(error: file.ControlFileError) -> #(String, String) {
  case error {
    file.ControlFileNotFound(path) -> #(
      "control_file_not_found",
      "control file not found: " <> path,
    )
    file.ControlFileReadFailed(_, message) -> #(
      "control_file_read_failed",
      message,
    )
    file.ControlFileWriteFailed(_, message) -> #(
      "control_file_write_failed",
      message,
    )
    file.ControlFileInvalid(_, message) -> #("control_file_invalid", message)
    file.ControlFilePermissionFailed(_, message) -> #(
      "control_file_permission_failed",
      message,
    )
    file.TokenGenerationFailed(message) -> #("token_generation_failed", message)
  }
}

fn print_workstream_summaries(
  summaries: List(workstream_projection.WorkstreamSummary),
  output: Output,
) -> Nil {
  case summaries {
    [] -> output.line("No workstreams found.")
    _ -> {
      output.line("WORKSTREAM  TASK  STATUS  ARTIFACTS  HANDOFFS  QUEUED")
      list.each(summaries, fn(summary) {
        output.line(
          summary.workstream_id
          <> "  "
          <> task_ref_label(summary.task_ref)
          <> "  "
          <> summary.status
          <> "  "
          <> int.to_string(summary.artifact_count)
          <> "  "
          <> int.to_string(summary.handoff_count)
          <> "  "
          <> int.to_string(summary.queued_phase_run_count),
        )
      })
    }
  }
}

fn print_workstream_inspections(
  inspections: List(workstream_projection.WorkstreamInspection),
  output: Output,
) -> Nil {
  list.each(inspections, fn(inspection) {
    print_workstream_inspection(inspection, output)
  })
}

fn print_workstream_inspection(
  inspection: workstream_projection.WorkstreamInspection,
  output: Output,
) -> Nil {
  output.line("workstream: " <> inspection.workstream_id)
  output.line("task: " <> task_ref_label(inspection.task_ref))
  output.line("status: " <> inspection.status)
  output.line("created_at_ms: " <> optional_ms(inspection.created_at_ms))
  output.line("assignment: " <> assignment_label(inspection.latest_assignment))
  print_workstream_phases(inspection.phases, output)
  print_workstream_artifacts(inspection.artifacts, output)
  print_workstream_handoffs(inspection.handoffs, output)
  print_workstream_phase_runs(inspection.queued_phase_runs, output)
  print_workstream_next_actions(inspection.unresolved_next_actions, output)
  print_workstream_decisions(inspection.decisions, output)
  print_workstream_warnings(inspection.warnings, output)
}

fn print_workstream_phases(
  phases: List(workstream_projection.PhaseInspection),
  output: Output,
) -> Nil {
  output.line("phases:")
  case phases {
    [] -> output.line("  -")
    _ ->
      list.each(phases, fn(phase) {
        output.line(
          "  "
          <> phase.phase_id
          <> " handoffs="
          <> int.to_string(phase.handoff_count)
          <> " latest="
          <> optional_string(phase.latest_handoff_ref),
        )
        case phase.latest_handoff_summary {
          Some(summary) -> output.line("    summary: " <> summary)
          None -> Nil
        }
      })
  }
}

fn print_workstream_artifacts(
  artifacts: List(workstream_projection.ArtifactInspection),
  output: Output,
) -> Nil {
  output.line("artifacts:")
  case artifacts {
    [] -> output.line("  -")
    _ ->
      list.each(artifacts, fn(artifact) {
        output.line(
          "  "
          <> artifact.artifact_id
          <> " "
          <> artifact.artifact_type
          <> " ref="
          <> artifact.snapshot_ref
          <> " sha256="
          <> artifact.snapshot_sha256,
        )
        output.line(
          "    path="
          <> artifact.original_path
          <> " bytes="
          <> int.to_string(artifact.snapshot_bytes)
          <> " producer="
          <> artifact.producer_workflow_id
          <> "/"
          <> artifact.producer_run_id
          <> "/"
          <> artifact.producer_step_id,
        )
        print_snapshot_status(artifact.snapshot_status, output)
        print_artifact_detail(artifact.detail, output)
      })
  }
}

fn print_workstream_handoffs(
  handoffs: List(workstream_projection.HandoffInspection),
  output: Output,
) -> Nil {
  output.line("handoffs:")
  case handoffs {
    [] -> output.line("  -")
    _ ->
      list.each(handoffs, fn(handoff) {
        output.line(
          "  "
          <> handoff.handoff_id
          <> " phase="
          <> optional_string(handoff.phase_id)
          <> " ref="
          <> handoff.handoff_ref
          <> " sha256="
          <> handoff.handoff_sha256,
        )
        output.line(
          "    source="
          <> handoff.source_workflow_id
          <> "/"
          <> handoff.source_run_id
          <> " bytes="
          <> int.to_string(handoff.handoff_bytes),
        )
        case handoff.summary {
          Some(summary) -> output.line("    summary: " <> summary)
          None -> Nil
        }
        print_snapshot_status(handoff.snapshot_status, output)
      })
  }
}

fn print_workstream_phase_runs(
  phase_runs: List(workstream_projection.PhaseRunInspection),
  output: Output,
) -> Nil {
  output.line("queued_phase_runs:")
  case phase_runs {
    [] -> output.line("  -")
    _ ->
      list.each(phase_runs, fn(run) {
        output.line(
          "  "
          <> run.phase_run_id
          <> " action="
          <> run.action_id
          <> " workflow="
          <> run.workflow_id
          <> " input="
          <> run.input_bundle_ref,
        )
      })
  }
}

fn print_workstream_next_actions(
  actions: List(workstream_projection.NextActionInspection),
  output: Output,
) -> Nil {
  output.line("unresolved_next_actions:")
  case actions {
    [] -> output.line("  -")
    _ ->
      list.each(actions, fn(action) {
        output.line(
          "  "
          <> action.action_id
          <> " workflow="
          <> action.workflow_id
          <> " state="
          <> action.state
          <> " priority="
          <> int.to_string(action.priority)
          <> " ref="
          <> action.snapshot_ref,
        )
      })
  }
}

fn print_workstream_decisions(
  decisions: List(workstream_projection.DecisionInspection),
  output: Output,
) -> Nil {
  output.line("decisions:")
  case decisions {
    [] -> output.line("  -")
    _ ->
      list.each(decisions, fn(decision) {
        output.line(
          "  "
          <> decision.artifact_id
          <> " kind="
          <> decision.kind
          <> " by="
          <> decision.decided_by
          <> " ref="
          <> decision.snapshot_ref,
        )
        output.line("    summary: " <> decision.summary)
      })
  }
}

fn print_workstream_warnings(
  warnings: List(workstream_projection_snapshot.ProjectionWarning),
  output: Output,
) -> Nil {
  output.line("warnings:")
  case warnings {
    [] -> output.line("  -")
    _ ->
      list.each(warnings, fn(warning) {
        output.line(
          "  "
          <> warning.code
          <> " ref="
          <> warning.ref
          <> " "
          <> warning.message,
        )
      })
  }
}

fn print_snapshot_status(
  status: workstream_projection_snapshot.SnapshotStatus,
  output: Output,
) -> Nil {
  case status {
    workstream_projection_snapshot.SnapshotOk(display_path, local_path) ->
      output.line(
        "    snapshot: ok path="
        <> display_path
        <> " local="
        <> optional_string(local_path),
      )
    workstream_projection_snapshot.SnapshotProblem(code, message) ->
      output.line("    snapshot: " <> code <> " " <> message)
  }
}

fn print_artifact_detail(
  detail: workstream_projection.ArtifactDetail,
  output: Output,
) -> Nil {
  case detail {
    workstream_projection.ArtifactUndecoded -> Nil
    workstream_projection.ArtifactDecodeFailed(code, message) ->
      output.line("    detail_error: " <> code <> " " <> message)
    workstream_projection.WorkstreamDetail(status, summary, next_actions) -> {
      output.line("    workstream_status: " <> status)
      output.line("    summary: " <> summary)
      output.line("    next_actions: " <> string.join(next_actions, with: ","))
    }
    workstream_projection.AssignmentDetail(workflow_id, playbook_id, reason) ->
      output.line(
        "    assignment: workflow="
        <> workflow_id
        <> " playbook="
        <> optional_string(playbook_id)
        <> " reason="
        <> reason,
      )
    workstream_projection.NextActionDetail(
      action_id,
      workflow_id,
      state,
      priority,
      inputs,
      requires_gate,
      auto_enqueue,
      resolved_by_phase_run_id,
    ) ->
      output.line(
        "    next_action: "
        <> action_id
        <> " workflow="
        <> workflow_id
        <> " state="
        <> state
        <> " priority="
        <> int.to_string(priority)
        <> " inputs="
        <> string.join(inputs, with: ",")
        <> " gate="
        <> optional_string(requires_gate)
        <> " auto_enqueue="
        <> bool_label(auto_enqueue)
        <> " resolved_by="
        <> optional_string(resolved_by_phase_run_id),
      )
    workstream_projection.DecisionDetail(kind, summary, decided_by) ->
      output.line(
        "    decision: kind="
        <> kind
        <> " by="
        <> decided_by
        <> " summary="
        <> summary,
      )
    workstream_projection.InputBundleDetail(
      workflow_id,
      source_handoff_ref,
      inputs,
      source_kind,
      source_reason,
    ) ->
      output.line(
        "    input_bundle: workflow="
        <> workflow_id
        <> " source_handoff="
        <> source_handoff_ref
        <> " inputs="
        <> string.join(inputs, with: ",")
        <> " source_kind="
        <> optional_string(source_kind)
        <> " source_reason="
        <> optional_string(source_reason),
      )
  }
}

fn task_ref_label(task_ref: Option(record.TaskRefFields)) -> String {
  case task_ref {
    None -> "-"
    Some(task_ref) ->
      case task_ref.task_key {
        Some(key) -> key
        None -> "id:" <> task_ref.task_remote_id
      }
  }
}

fn assignment_label(
  assignment: Option(workstream_projection.AssignmentInspection),
) -> String {
  case assignment {
    None -> "-"
    Some(assignment) ->
      assignment.assignment_id
      <> " workflow="
      <> assignment.workflow_id
      <> " playbook="
      <> optional_string(assignment.playbook_id)
      <> " reason="
      <> assignment.reason
  }
}

fn bool_label(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn optional_ms(value: Option(Int)) -> String {
  case value {
    Some(value) -> int.to_string(value)
    None -> "-"
  }
}

fn optional_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> "-"
  }
}

fn try_workstream(
  result: Result(a, #(String, String)),
  next: fn(a) -> Result(b, #(String, String)),
) -> Result(b, #(String, String)) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
