import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/state/ledger
import scherzo/state/local_artifacts
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_repair

type StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: String,
    run_id: String,
    repair_status: String,
    repair_mode: String,
    source_evidence: List(String),
    reason: Option(String),
    message: Option(String),
  )
}

pub fn run_status(
  root: String,
  json_output json_output: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let status = local_artifacts.inspect_state(root)
  case json_output {
    True ->
      output_line(
        status |> local_artifacts.state_status_to_json |> json.to_string,
      )
    False -> print_state_status(status, output_line)
  }
  Ok(Nil)
}

pub fn run_archive_old(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result =
    local_artifacts.archive_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_discard_old(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result =
    local_artifacts.discard_old_state(root, yes, local_artifacts.now_ms())
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_reinitialize(
  root: String,
  json_output json_output: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result = local_artifacts.reinitialize_state(root, yes: yes)
  print_state_mutation(result, json_output, output_line)
  Ok(Nil)
}

pub fn run_repair_run_provenance(
  root: String,
  run_id: String,
  json_output json_output: Bool,
  dry_run dry_run: Bool,
  yes yes: Bool,
  line output_line: fn(String) -> Nil,
) -> Result(Nil, #(String, String)) {
  let result = state_repair_run_provenance(root, run_id, dry_run, yes)
  print_state_repair_run_provenance(result, json_output, output_line)
  Ok(Nil)
}

fn state_repair_run_provenance(
  root: String,
  run_id: String,
  dry_run: Bool,
  yes: Bool,
) -> StateRunProvenanceRepairResult {
  case ledger.path_for_workspace_root(root) {
    Error(error) ->
      rejected_state_repair_result(
        run_id,
        "ledger_path_failed",
        ledger_error_message(error),
      )
    Ok(ledger_path) ->
      case ledger.read_records(ledger_path) {
        Error(error) ->
          rejected_state_repair_result(
            run_id,
            "ledger_read_failed",
            ledger_error_message(error),
          )
        Ok(read) -> {
          let projection_state = projection.fold(read.records)
          case
            workflow_repair.inspect_run_provenance_repair(
              projection_state,
              run_id,
              workflow_repair.state_repair_explicit_mode,
            )
          {
            Error(error) ->
              rejected_state_repair_result(
                run_id,
                workflow_repair.describe_error(error),
                repair_error_message_text(error),
              )
            Ok(workflow_repair.RunProvenanceRepairAlreadyPresent(..)) ->
              StateRunProvenanceRepairResult(
                status: "already_repaired",
                run_id: run_id,
                repair_status: "already_repaired",
                repair_mode: workflow_repair.state_repair_explicit_mode,
                source_evidence: [],
                reason: None,
                message: Some("workflow run provenance is already present"),
              )
            Ok(workflow_repair.RunProvenanceRepairRequired(plan)) ->
              case
                workflow_repair.validate_run_root_for_repair(
                  run_id,
                  plan.run_root,
                  root,
                )
              {
                Error(error) ->
                  rejected_state_repair_result(
                    run_id,
                    workflow_repair.describe_error(error),
                    repair_error_message_text(error),
                  )
                Ok(Nil) ->
                  case dry_run, yes {
                    True, _ ->
                      StateRunProvenanceRepairResult(
                        status: "dry_run",
                        run_id: run_id,
                        repair_status: "would_repair",
                        repair_mode: plan.repair_mode,
                        source_evidence: plan.source_evidence,
                        reason: None,
                        message: Some("workflow run provenance can be repaired"),
                      )
                    _, True ->
                      append_state_repair_run_provenance(ledger_path, plan)
                    _, _ ->
                      rejected_state_repair_result(
                        run_id,
                        "confirmation_required",
                        "pass --dry-run to inspect or --yes to repair",
                      )
                  }
              }
          }
        }
      }
  }
}

fn append_state_repair_run_provenance(
  ledger_path: ledger.LedgerPath,
  plan: workflow_repair.RunProvenanceRepairPlan,
) -> StateRunProvenanceRepairResult {
  let ledger_record =
    record.with_id(
      "workflow-run-provenance-repaired-" <> plan.run_id,
      local_artifacts.now_ms(),
      plan.record_body,
    )
  case ledger.append_idempotent(ledger_path, ledger_record, True) {
    Ok(ledger.Appended) ->
      StateRunProvenanceRepairResult(
        status: "repaired",
        run_id: plan.run_id,
        repair_status: "repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repaired"),
      )
    Ok(ledger.AlreadyRecorded(_)) ->
      StateRunProvenanceRepairResult(
        status: "already_repaired",
        run_id: plan.run_id,
        repair_status: "already_repaired",
        repair_mode: plan.repair_mode,
        source_evidence: plan.source_evidence,
        reason: None,
        message: Some("workflow run provenance repair was already recorded"),
      )
    Error(error) ->
      rejected_state_repair_result(
        plan.run_id,
        "ledger_append_failed",
        append_idempotent_error_message(error),
      )
  }
}

fn repair_error_message_text(error: workflow_repair.RepairError) -> String {
  case workflow_repair.error_message(error) {
    Some(message) -> message
    None -> workflow_repair.describe_error(error)
  }
}

fn rejected_state_repair_result(
  run_id: String,
  reason: String,
  message: String,
) -> StateRunProvenanceRepairResult {
  StateRunProvenanceRepairResult(
    status: "rejected",
    run_id: run_id,
    repair_status: "rejected",
    repair_mode: workflow_repair.state_repair_explicit_mode,
    source_evidence: [],
    reason: Some(reason),
    message: Some(message),
  )
}

fn print_state_repair_run_provenance(
  result: StateRunProvenanceRepairResult,
  json_output: Bool,
  output_line: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True ->
      output_line(
        result
        |> state_repair_run_provenance_to_json
        |> json.to_string,
      )
    False -> {
      output_line("state repair-run-provenance " <> result.status)
      output_line("run_id: " <> result.run_id)
      output_line("repair_status: " <> result.repair_status)
      case result.reason {
        Some(reason) -> output_line("reason: " <> reason)
        None -> Nil
      }
      case result.message {
        Some(message) -> output_line("message: " <> message)
        None -> Nil
      }
      case result.source_evidence {
        [] -> Nil
        _ -> {
          output_line("source_evidence:")
          list.each(result.source_evidence, fn(evidence) {
            output_line("  " <> evidence)
          })
        }
      }
    }
  }
}

fn state_repair_run_provenance_to_json(
  result: StateRunProvenanceRepairResult,
) -> json.Json {
  [
    #("command", json.string("state repair-run-provenance")),
    #("status", json.string(result.status)),
    #("run_id", json.string(result.run_id)),
    #("repair_status", json.string(result.repair_status)),
    #("repair_mode", json.string(result.repair_mode)),
    #("source_evidence", json.array(result.source_evidence, of: json.string)),
    #("reason", optional_string_json(result.reason)),
    #("message", optional_string_json(result.message)),
  ]
  |> json.object
}

fn ledger_error_message(error: ledger.LedgerError) -> String {
  ledger.ledger_error_to_string(error)
}

fn append_idempotent_error_message(
  error: ledger.AppendIdempotentError,
) -> String {
  case error {
    ledger.AppendLedgerError(error) -> ledger_error_message(error)
    ledger.RecordIdConflict(record_id) ->
      "ledger record id conflict: " <> record_id
  }
}

fn print_state_status(
  status: local_artifacts.StateStatusResult,
  output_line: fn(String) -> Nil,
) -> Nil {
  output_line("state: " <> state_status_name(status.status))
  output_line("message: " <> status.message)
  output_line("workspace_root: " <> status.workspace_root)
  output_line("ledger_dir: " <> status.ledger_dir)
  case status.warnings {
    [] -> Nil
    _ -> {
      output_line("warnings:")
      list.each(status.warnings, fn(warning) { output_line("  " <> warning) })
    }
  }
  case status.status {
    local_artifacts.StateUnsupported(_, _) -> {
      output_line("recovery: old_state_reset_required")
      output_line("safe actions: archive-old, discard-old, reinitialize")
    }
    _ -> output_line("recovery: -")
  }
}

fn state_status_name(status: local_artifacts.StateStatus) -> String {
  case status {
    local_artifacts.StateCurrent -> "current"
    local_artifacts.StateUnsupported(_, _) -> "unsupported"
    local_artifacts.StateCorrupt(_) -> "corrupt"
    local_artifacts.StateMissing -> "missing"
    local_artifacts.StateArchived -> "archived"
  }
}

fn print_state_mutation(
  result: local_artifacts.StateMutationResult,
  json_output: Bool,
  output_line: fn(String) -> Nil,
) -> Nil {
  case json_output {
    True ->
      output_line(
        result |> local_artifacts.state_mutation_to_json |> json.to_string,
      )
    False -> {
      output_line(result.action <> " " <> result.status)
      output_line("message: " <> result.message)
      case result.archive_path {
        Some(path) -> output_line("archive_path: " <> path)
        None -> Nil
      }
    }
  }
}

fn optional_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}
