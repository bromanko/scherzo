import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/session/event
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/terminal/sanitize
import scherzo/workflow_outcome
import scherzo/workflow_step_recovery

const max_detail_chars = 120

pub opaque type History {
  History(entries: List(HistoryEntry))
}

type HistoryEntry {
  HistoryEntry(
    run_id: String,
    workflow_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
    recovery_session_id: String,
    entry_status: EntryStatus,
    failed_attempt_artifact_ref: String,
    recovery_result_artifact_ref: Option(String),
    decision: Option(String),
    summary: Option(String),
    reason: Option(String),
    recheck_attempt_index: Option(Int),
    recheck_attempt_artifact_ref: Option(String),
    recheck_result: Option(String),
    final_workflow_outcome: Option(String),
  )
}

type EntryStatus {
  Finished
  Incomplete
}

pub opaque type LoadError {
  LoadLedgerFailed(ledger.LedgerError)
}

type SessionTarget {
  FailedAttemptTarget(run_id: String, step_id: String, attempt_index: Int)
  RecheckAttemptTarget(run_id: String, step_id: String, attempt_index: Int)
  RecoveryTarget(
    run_id: String,
    step_id: String,
    failed_attempt_index: Int,
    recovery_attempt_number: Int,
  )
}

type RecoveryFinishData {
  RecoveryFinishData(
    result: String,
    summary: String,
    reason: String,
    recheck_attempt_index: Option(Int),
  )
}

pub fn load(
  workspace_root: String,
  summary: event.SessionSummary,
) -> Result(History, LoadError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(workspace_root)
    |> result.map_error(LoadLedgerFailed),
  )
  use replayed <- result.try(
    ledger.replay(ledger_path) |> result.map_error(LoadLedgerFailed),
  )
  Ok(from_replay(summary, replayed.records, replayed.projection))
}

pub fn from_replay(
  summary: event.SessionSummary,
  records: List(record.LedgerRecord),
  folded: projection.Projection,
) -> History {
  let targets = session_targets(summary.session_id, records)
  let finishes = recovery_finish_index(records)

  records
  |> list.fold([], fn(acc, ledger_record) {
    case ledger_record {
      record.LedgerRecord(
        _,
        _,
        record.WorkflowStepRecoveryStarted(
          run_id,
          workflow_id,
          step_id,
          failed_attempt_index,
          recovery_attempt_number,
          recovery_session_id,
          _,
          _,
        ),
      ) -> {
        let finish =
          dict.get(
            finishes,
            projection.step_recovery_key(
              run_id,
              step_id,
              failed_attempt_index,
              recovery_attempt_number,
            ),
          )
          |> result_to_option

        case
          matches_any_target(
            targets,
            run_id,
            step_id,
            failed_attempt_index,
            recovery_attempt_number,
            finish,
          )
        {
          True -> [
            history_entry(
              run_id,
              workflow_id,
              step_id,
              failed_attempt_index,
              recovery_attempt_number,
              recovery_session_id,
              finish,
              folded,
            ),
            ..acc
          ]
          False -> acc
        }
      }
      _ -> acc
    }
  })
  |> list.reverse
  |> History
}

pub fn render(history: History) -> List(String) {
  case history {
    History(entries: []) -> ["workflow_step_recovery_history: -"]
    History(entries: entries) -> [
      "workflow_step_recovery_history:",
      ..render_entries(entries, [])
    ]
  }
}

pub fn describe_load_error(error: LoadError) -> String {
  case error {
    LoadLedgerFailed(ledger.Io(message)) -> sanitize.text(message)
    LoadLedgerFailed(ledger.LedgerFfiFailed(error)) ->
      ledger.ledger_ffi_error_to_string(error) |> sanitize.text
    LoadLedgerFailed(ledger.UnsupportedVersion(version)) ->
      "unsupported ledger schema version " <> int.to_string(version)
    LoadLedgerFailed(ledger.CorruptRecord(line, reason)) ->
      "corrupt ledger record at line "
      <> int.to_string(line)
      <> ": "
      <> sanitize.text(reason)
    LoadLedgerFailed(ledger.AggregateInvariantViolation(reason, run_id)) ->
      reason
      <> ": workflow run "
      <> run_id
      <> " is not present in local state"
      |> sanitize.text
  }
}

fn history_entry(
  run_id: String,
  workflow_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  recovery_session_id: String,
  finish: Option(RecoveryFinishData),
  folded: projection.Projection,
) -> HistoryEntry {
  case finish {
    Some(RecoveryFinishData(result, summary, reason, recheck_attempt_index)) ->
      HistoryEntry(
        run_id: run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        failed_attempt_index: failed_attempt_index,
        recovery_attempt_number: recovery_attempt_number,
        recovery_session_id: recovery_session_id,
        entry_status: Finished,
        failed_attempt_artifact_ref: artifact_store.artifact_ref(
          run_id,
          step_id,
          failed_attempt_index,
        ),
        recovery_result_artifact_ref: recovery_result_artifact_ref(
          run_id,
          step_id,
          failed_attempt_index,
          recovery_attempt_number,
          result,
        ),
        decision: Some(result),
        summary: optional_text(summary),
        reason: optional_text(reason),
        recheck_attempt_index: recheck_attempt_index,
        recheck_attempt_artifact_ref: recheck_attempt_artifact_ref(
          run_id,
          step_id,
          recheck_attempt_index,
        ),
        recheck_result: recheck_result(
          folded,
          run_id,
          step_id,
          recheck_attempt_index,
        ),
        final_workflow_outcome: final_workflow_outcome(folded, run_id),
      )
    None ->
      HistoryEntry(
        run_id: run_id,
        workflow_id: workflow_id,
        step_id: step_id,
        failed_attempt_index: failed_attempt_index,
        recovery_attempt_number: recovery_attempt_number,
        recovery_session_id: recovery_session_id,
        entry_status: Incomplete,
        failed_attempt_artifact_ref: artifact_store.artifact_ref(
          run_id,
          step_id,
          failed_attempt_index,
        ),
        recovery_result_artifact_ref: None,
        decision: None,
        summary: None,
        reason: None,
        recheck_attempt_index: None,
        recheck_attempt_artifact_ref: None,
        recheck_result: None,
        final_workflow_outcome: final_workflow_outcome(folded, run_id),
      )
  }
}

fn session_targets(
  session_id: String,
  records: List(record.LedgerRecord),
) -> List(SessionTarget) {
  records
  |> list.fold([], fn(acc, ledger_record) {
    case ledger_record {
      record.LedgerRecord(
        _,
        _,
        record.StepAttemptStarted(
          run_id,
          _,
          step_id,
          attempt_index,
          operator_session_id,
          _,
          _,
        ),
      ) ->
        case operator_session_id == session_id {
          True ->
            unique_target_insert(
              RecheckAttemptTarget(run_id, step_id, attempt_index),
              unique_target_insert(
                FailedAttemptTarget(run_id, step_id, attempt_index),
                acc,
              ),
            )
          False -> acc
        }
      record.LedgerRecord(
        _,
        _,
        record.StepAttemptContinuationStarted(
          run_id,
          _,
          step_id,
          attempt_index,
          continuation_session_id,
        ),
      ) ->
        case continuation_session_id == session_id {
          True ->
            unique_target_insert(
              RecheckAttemptTarget(run_id, step_id, attempt_index),
              unique_target_insert(
                FailedAttemptTarget(run_id, step_id, attempt_index),
                acc,
              ),
            )
          False -> acc
        }
      record.LedgerRecord(
        _,
        _,
        record.WorkflowStepRecoveryStarted(
          run_id,
          _,
          step_id,
          failed_attempt_index,
          recovery_attempt_number,
          recovery_session_id,
          _,
          _,
        ),
      ) ->
        case recovery_session_id == session_id {
          True ->
            unique_target_insert(
              RecoveryTarget(
                run_id,
                step_id,
                failed_attempt_index,
                recovery_attempt_number,
              ),
              acc,
            )
          False -> acc
        }
      _ -> acc
    }
  })
  |> list.reverse
}

fn unique_target_insert(
  target: SessionTarget,
  targets: List(SessionTarget),
) -> List(SessionTarget) {
  case list.any(targets, fn(existing) { existing == target }) {
    True -> targets
    False -> [target, ..targets]
  }
}

fn recovery_finish_index(
  records: List(record.LedgerRecord),
) -> dict.Dict(String, RecoveryFinishData) {
  records
  |> list.fold(dict.new(), fn(acc, ledger_record) {
    case ledger_record {
      record.LedgerRecord(
        _,
        _,
        record.WorkflowStepRecoveryFinished(
          run_id,
          _,
          step_id,
          failed_attempt_index,
          recovery_attempt_number,
          _,
          result,
          summary,
          reason,
          recheck_attempt_index,
        ),
      ) -> {
        let key =
          projection.step_recovery_key(
            run_id,
            step_id,
            failed_attempt_index,
            recovery_attempt_number,
          )

        case dict.get(acc, key) {
          Ok(_) -> acc
          Error(Nil) ->
            dict.insert(
              acc,
              key,
              RecoveryFinishData(
                result: result,
                summary: summary,
                reason: reason,
                recheck_attempt_index: recheck_attempt_index,
              ),
            )
        }
      }
      _ -> acc
    }
  })
}

fn matches_any_target(
  targets: List(SessionTarget),
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  finish: Option(RecoveryFinishData),
) -> Bool {
  list.any(targets, fn(target) {
    case target {
      FailedAttemptTarget(target_run_id, target_step_id, attempt_index) ->
        target_run_id == run_id
        && target_step_id == step_id
        && attempt_index == failed_attempt_index
      RecheckAttemptTarget(target_run_id, target_step_id, attempt_index) ->
        target_run_id == run_id
        && target_step_id == step_id
        && recheck_attempt_matches(finish, attempt_index)
      RecoveryTarget(
        target_run_id,
        target_step_id,
        target_failed_attempt_index,
        target_recovery_attempt_number,
      ) ->
        target_run_id == run_id
        && target_step_id == step_id
        && target_failed_attempt_index == failed_attempt_index
        && target_recovery_attempt_number == recovery_attempt_number
    }
  })
}

fn recheck_attempt_matches(
  finish: Option(RecoveryFinishData),
  attempt_index: Int,
) -> Bool {
  case finish {
    Some(RecoveryFinishData(
      recheck_attempt_index: Some(recheck_attempt_index),
      ..,
    )) -> recheck_attempt_index == attempt_index
    _ -> False
  }
}

fn recheck_result(
  folded: projection.Projection,
  run_id: String,
  step_id: String,
  recheck_attempt_index: Option(Int),
) -> Option(String) {
  case recheck_attempt_index {
    None -> None
    Some(recheck_attempt_index) ->
      case
        dict.get(
          folded.step_attempts,
          projection.step_attempt_key(run_id, step_id, recheck_attempt_index),
        )
      {
        Ok(projection.StepAttemptPending(..)) -> Some("pending")
        Ok(projection.StepAttemptRunning(..)) -> Some("running")
        Ok(projection.StepAttemptFinishedStatus(outcome: outcome, ..)) ->
          Some(outcome)
        Ok(projection.StepAttemptInterruptedStatus(..)) -> Some("interrupted")
        Ok(projection.StepAttemptSupersededStatus(..)) -> Some("superseded")
        Error(Nil) -> None
      }
  }
}

fn recheck_attempt_artifact_ref(
  run_id: String,
  step_id: String,
  recheck_attempt_index: Option(Int),
) -> Option(String) {
  case recheck_attempt_index {
    Some(recheck_attempt_index) ->
      Some(artifact_store.artifact_ref(run_id, step_id, recheck_attempt_index))
    None -> None
  }
}

fn recovery_result_artifact_ref(
  run_id: String,
  step_id: String,
  failed_attempt_index: Int,
  recovery_attempt_number: Int,
  result: String,
) -> Option(String) {
  case result == "recheck" || result == "gave_up" {
    True ->
      Some(artifact_store.recovery_artifact_ref(
        run_id,
        step_id,
        failed_attempt_index,
        recovery_attempt_number,
        workflow_step_recovery.artifact_name,
      ))
    False -> None
  }
}

fn final_workflow_outcome(
  folded: projection.Projection,
  run_id: String,
) -> Option(String) {
  case dict.get(folded.workflow_runs, run_id) {
    Ok(projection.WorkflowRunFinished(outcome: outcome, ..)) ->
      case
        outcome == workflow_outcome.succeeded_after_recovery
        || outcome == workflow_outcome.failed_after_recovery
      {
        True -> Some(outcome)
        False -> None
      }
    _ -> None
  }
}

fn optional_text(value: String) -> Option(String) {
  case string.trim(value) == "" {
    True -> None
    False -> Some(inline(value))
  }
}

fn inline(value: String) -> String {
  let compact =
    value
    |> string.replace(each: "\r\n", with: "\n")
    |> string.replace(each: "\n", with: " ⏎ ")
    |> sanitize.text
  case string.length(compact) > max_detail_chars {
    True -> string.slice(compact, 0, max_detail_chars) <> "…"
    False -> compact
  }
}

fn render_entries(
  entries: List(HistoryEntry),
  acc: List(String),
) -> List(String) {
  case entries {
    [] -> acc
    [entry, ..rest] ->
      render_entries(rest, list.append(acc, render_entry(entry)))
  }
}

fn render_entry(entry: HistoryEntry) -> List(String) {
  let base = [
    "  - run_id: " <> sanitize.text(entry.run_id),
    "    workflow_id: " <> sanitize.text(entry.workflow_id),
    "    step_id: " <> sanitize.text(entry.step_id),
    "    failed_attempt_index: " <> int.to_string(entry.failed_attempt_index),
    "    recovery_attempt_number: "
      <> int.to_string(entry.recovery_attempt_number),
    "    recovery_session_id: " <> sanitize.text(entry.recovery_session_id),
    "    status: " <> entry_status_to_string(entry.entry_status),
    "    failed_attempt_artifact_ref: "
      <> sanitize.text(entry.failed_attempt_artifact_ref),
  ]

  base
  |> append_optional(
    "    recovery_result_artifact_ref: ",
    entry.recovery_result_artifact_ref,
  )
  |> append_optional("    decision: ", entry.decision)
  |> append_optional("    summary: ", entry.summary)
  |> append_optional("    reason: ", entry.reason)
  |> append_optional_int(
    "    recheck_attempt_index: ",
    entry.recheck_attempt_index,
  )
  |> append_optional(
    "    recheck_attempt_artifact_ref: ",
    entry.recheck_attempt_artifact_ref,
  )
  |> append_optional("    recheck_result: ", entry.recheck_result)
  |> append_optional(
    "    final_workflow_outcome: ",
    entry.final_workflow_outcome,
  )
}

fn append_optional(
  lines: List(String),
  prefix: String,
  value: Option(String),
) -> List(String) {
  case value {
    Some(value) -> list.append(lines, [prefix <> sanitize.text(value)])
    None -> lines
  }
}

fn append_optional_int(
  lines: List(String),
  prefix: String,
  value: Option(Int),
) -> List(String) {
  case value {
    Some(value) -> list.append(lines, [prefix <> int.to_string(value)])
    None -> lines
  }
}

fn result_to_option(value: Result(a, Nil)) -> Option(a) {
  case value {
    Ok(value) -> Some(value)
    Error(Nil) -> None
  }
}

fn entry_status_to_string(status: EntryStatus) -> String {
  case status {
    Finished -> "finished"
    Incomplete -> "incomplete"
  }
}
