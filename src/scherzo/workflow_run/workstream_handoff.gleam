import gleam/option.{None, Some}
import gleam/result
import scherzo/state/record
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_run/contract_io
import scherzo/workstream/handoff_emitter

pub fn emit_if_configured(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  outputs: contract_io.ContractOutputsResult,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, String) {
  case dag.workstream_phase {
    None -> Ok(Nil)
    Some(metadata) ->
      case metadata.handoff {
        None -> Ok(Nil)
        Some(_) ->
          case outputs.manifest {
            None -> Error("workflow_workstream_handoff_missing_output_manifest")
            Some(manifest) -> {
              use emitted <- result.try(
                handoff_emitter.emit(
                  dag.id,
                  run_id,
                  workflow_fingerprint,
                  issue.id,
                  issue.identifier,
                  issue.url,
                  metadata,
                  manifest,
                  checkpoint,
                )
                |> result.map_error(fn(error) {
                  handoff_emitter.error_code(error)
                  <> ":"
                  <> handoff_emitter.error_message(error)
                }),
              )
              case emitted {
                handoff_emitter.NoHandoff -> Ok(Nil)
                handoff_emitter.Emitted(records: records, ..) ->
                  append_workstream_records(records, checkpoint)
              }
            }
          }
      }
  }
}

fn append_workstream_records(
  records: List(record.LedgerRecord),
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, String) {
  case records {
    [] -> Ok(Nil)
    [ledger_record, ..rest] -> {
      use _ <- result.try(
        checkpoint.append_workstream_record_idempotent(ledger_record)
        |> result.map_error(workflow_checkpoint.describe_error),
      )
      append_workstream_records(rest, checkpoint)
    }
  }
}
