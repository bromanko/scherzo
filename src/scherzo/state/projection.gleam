import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/result
import gleam/string
import scherzo/orchestrator/state as orchestrator_state
import scherzo/state/record

pub type Projection {
  Projection(
    runs: Dict(String, RunStatus),
    retries: Dict(String, RetryStatus),
    parked_issues: Dict(String, ParkedIssue),
    commands: Dict(String, CommandStatus),
    command_receipts: Dict(String, CommandReceiptState),
    outbox: Dict(String, OutboxStatus),
    issue_counters: Dict(String, IssueCounterStatus),
    known_workspaces: Dict(String, KnownWorkspace),
  )
}

pub type RunStatus {
  RunRunning(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
    started_at_ms: Int,
  )
  RunFinished(
    issue_id: String,
    classification: String,
    token_total: Int,
    turns: Int,
    finished_at_ms: Int,
  )
  RunInterrupted(issue_id: String, reason: String, interrupted_at_ms: Int)
}

pub type RetryStatus {
  RetryScheduled(
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
    scheduled_at_ms: Int,
  )
  RetryCancelled(generation: Int, reason: String, cancelled_at_ms: Int)
}

pub type ParkedIssue {
  ParkedIssue(
    issue_identifier: String,
    reason: String,
    observed_updated_at_ms: Int,
    parked_at_ms: Int,
    release_policy: String,
    issue_fingerprint: String,
  )
}

pub type IssueCounterStatus {
  IssueCounterStatus(
    issue_identifier: String,
    failure_attempts: Int,
    worker_sessions: Int,
    observed_updated_at_ms: Int,
    source_run_ids: List(String),
    updated_at_ms: Int,
  )
}

pub type KnownWorkspace {
  KnownWorkspace(
    issue_identifier: String,
    workspace_path: String,
    recorded_at_ms: Int,
  )
}

pub type CommandStatus {
  CommandSeen(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
  )
  CommandStarted(issue_id: String, command_name: String, started_at_ms: Int)
  CommandCompleted(
    issue_id: String,
    status: String,
    message_excerpt: String,
    completed_at_ms: Int,
  )
  CommandAcked(issue_id: String, acked_at_ms: Int)
}

pub type CommandReceiptState {
  CommandReceiptUnseen
  CommandReceiptSeen(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
  )
  CommandReceiptStarted(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    seen_at_ms: Int,
    started_at_ms: Int,
  )
  CommandReceiptCompleted(
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
    result_status: String,
    message_excerpt: String,
    seen_at_ms: Int,
    started_at_ms: Int,
    completed_at_ms: Int,
    acked_at_ms: Option(Int),
  )
  CommandReceiptAcked(issue_id: String, acked_at_ms: Int)
}

pub type OutboxStatus {
  OutboxPending(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    pending_at_ms: Int,
  )
  OutboxPendingV2(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
    pending_at_ms: Int,
  )
  OutboxCompleted(issue_id: String, outbox_kind: String, completed_at_ms: Int)
  OutboxFailed(
    issue_id: String,
    outbox_kind: String,
    error_code: String,
    failed_at_ms: Int,
  )
}

pub type OutboxReplay {
  OutboxReplay(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
}

pub type PendingOutboxError {
  OutboxPayloadMissing(outbox_id: String)
}

type RunSnapshot {
  RunSnapshot(run_id: String, status: RunStatus)
}

type RetrySnapshot {
  RetrySnapshot(issue_id: String, status: RetryStatus)
}

type ParkedSnapshot {
  ParkedSnapshot(issue_id: String, parked: ParkedIssue)
}

type CommandSnapshot {
  CommandSnapshot(comment_id: String, status: CommandStatus)
}

type CommandReceiptSnapshot {
  CommandReceiptSnapshot(comment_id: String, receipt: CommandReceiptState)
}

type OutboxSnapshot {
  OutboxSnapshot(outbox_id: String, status: OutboxStatus)
}

type IssueCounterSnapshot {
  IssueCounterSnapshot(issue_id: String, status: IssueCounterStatus)
}

type KnownWorkspaceSnapshot {
  KnownWorkspaceSnapshot(issue_id: String, workspace: KnownWorkspace)
}

type SnapshotFields {
  SnapshotFields(
    runs: List(RunSnapshot),
    retries: List(RetrySnapshot),
    parked_issues: List(ParkedSnapshot),
    commands: List(CommandSnapshot),
    command_receipts: List(CommandReceiptSnapshot),
    outbox: List(OutboxSnapshot),
    issue_counters: List(IssueCounterSnapshot),
    known_workspaces: List(KnownWorkspaceSnapshot),
  )
}

pub fn new() -> Projection {
  Projection(
    runs: dict.new(),
    retries: dict.new(),
    parked_issues: dict.new(),
    commands: dict.new(),
    command_receipts: dict.new(),
    outbox: dict.new(),
    issue_counters: dict.new(),
    known_workspaces: dict.new(),
  )
}

pub fn fold(records: List(record.LedgerRecord)) -> Projection {
  fold_from(new(), records)
}

pub fn fold_from(
  projection: Projection,
  records: List(record.LedgerRecord),
) -> Projection {
  list.fold(records, projection, fn(acc, ledger_record) {
    apply(acc, ledger_record)
  })
}

pub fn apply(
  projection: Projection,
  ledger_record: record.LedgerRecord,
) -> Projection {
  let at_ms = ledger_record.at_ms
  case ledger_record.body {
    record.RunStarted(run_id, issue_id, issue_identifier, workspace_path) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunRunning(issue_id, issue_identifier, workspace_path, at_ms),
        ),
      )
    record.RunFinished(run_id, issue_id, classification, token_total, turns) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunFinished(issue_id, classification, token_total, turns, at_ms),
        ),
      )
    record.RunInterrupted(run_id, issue_id, reason) ->
      Projection(
        ..projection,
        runs: dict.insert(
          projection.runs,
          run_id,
          RunInterrupted(issue_id, reason, at_ms),
        ),
      )
    record.RetryScheduled(
      issue_id,
      issue_identifier,
      delay_ms,
      generation,
      reason,
    ) ->
      Projection(
        ..projection,
        retries: dict.insert(
          projection.retries,
          issue_id,
          RetryScheduled(issue_identifier, delay_ms, generation, reason, at_ms),
        ),
      )
    record.RetryCancelled(issue_id, generation, reason) ->
      Projection(
        ..projection,
        retries: dict.insert(
          projection.retries,
          issue_id,
          RetryCancelled(generation, reason, at_ms),
        ),
      )
    record.IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_id,
    ) -> {
      let source_run_ids = case dict.get(projection.issue_counters, issue_id) {
        Ok(existing) -> existing.source_run_ids
        Error(_) -> []
      }
      let source_run_ids = case source_run_id {
        Some(run_id) -> insert_unique_string(source_run_ids, run_id)
        None -> source_run_ids
      }
      Projection(
        ..projection,
        issue_counters: dict.insert(
          projection.issue_counters,
          issue_id,
          IssueCounterStatus(
            issue_identifier,
            failure_attempts,
            worker_sessions,
            observed_updated_at_ms,
            source_run_ids,
            at_ms,
          ),
        ),
      )
    }
    record.KnownWorkspace(issue_id, issue_identifier, workspace_path) ->
      Projection(
        ..projection,
        known_workspaces: dict.insert(
          projection.known_workspaces,
          issue_id,
          KnownWorkspace(issue_identifier, workspace_path, at_ms),
        ),
      )
    record.IssueParked(
      issue_id,
      issue_identifier,
      reason,
      observed_updated_at_ms,
    ) ->
      Projection(
        ..projection,
        parked_issues: dict.insert(
          projection.parked_issues,
          issue_id,
          ParkedIssue(
            issue_identifier,
            reason,
            observed_updated_at_ms,
            at_ms,
            "explicit_unpark_only",
            "",
          ),
        ),
      )
    record.IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ) ->
      Projection(
        ..projection,
        parked_issues: dict.insert(
          projection.parked_issues,
          issue_id,
          ParkedIssue(
            issue_identifier,
            reason,
            observed_updated_at_ms,
            at_ms,
            release_policy,
            issue_fingerprint,
          ),
        ),
      )
    record.IssueUnparked(issue_id, _, _) ->
      Projection(
        ..projection,
        parked_issues: dict.delete(projection.parked_issues, issue_id),
      )
    record.LinearCommandSeen(
      comment_id,
      issue_id,
      author_id,
      command_name,
      excerpt,
    ) -> {
      let receipt =
        seen_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          author_id,
          command_name,
          excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandSeen(issue_id, author_id, command_name, excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandStarted(comment_id, issue_id, command_name) -> {
      let receipt =
        started_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          command_name,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandStarted(issue_id, command_name, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) -> {
      let receipt =
        completed_receipt(
          projection.command_receipts,
          comment_id,
          issue_id,
          status,
          message_excerpt,
          at_ms,
        )
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandCompleted(issue_id, status, message_excerpt, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.LinearCommandAcked(comment_id, issue_id) -> {
      let receipt =
        acked_receipt(projection.command_receipts, comment_id, issue_id, at_ms)
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandAcked(issue_id, at_ms),
        ),
        command_receipts: dict.insert(
          projection.command_receipts,
          comment_id,
          receipt,
        ),
      )
    }
    record.OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxPending(issue_id, outbox_kind, dedupe_key, at_ms),
        ),
      )
    record.OutboxPendingV2(
      outbox_id,
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
    ) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxPendingV2(
            issue_id,
            outbox_kind,
            dedupe_key,
            payload_json,
            at_ms,
          ),
        ),
      )
    record.OutboxCompleted(outbox_id, issue_id, outbox_kind) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxCompleted(issue_id, outbox_kind, at_ms),
        ),
      )
    record.OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxFailed(issue_id, outbox_kind, error_code, at_ms),
        ),
      )
  }
}

fn seen_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  author_id: String,
  command_name: String,
  excerpt: String,
  seen_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptUnseen) | Error(_) ->
      CommandReceiptSeen(issue_id, author_id, command_name, excerpt, seen_at_ms)
    Ok(receipt) -> receipt
  }
}

fn started_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  command_name: String,
  started_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(receipt) ->
      case receipt {
        CommandReceiptSeen(_, author_id, _, excerpt, seen_at_ms) ->
          CommandReceiptStarted(
            issue_id,
            author_id,
            command_name,
            excerpt,
            seen_at_ms,
            started_at_ms,
          )
        CommandReceiptStarted(_, author_id, _, excerpt, seen_at_ms, _) ->
          CommandReceiptStarted(
            issue_id,
            author_id,
            command_name,
            excerpt,
            seen_at_ms,
            started_at_ms,
          )
        CommandReceiptCompleted(..) | CommandReceiptAcked(..) -> receipt
        CommandReceiptUnseen ->
          CommandReceiptStarted(
            issue_id,
            "",
            command_name,
            "",
            0,
            started_at_ms,
          )
      }
    Error(_) ->
      CommandReceiptStarted(issue_id, "", command_name, "", 0, started_at_ms)
  }
}

fn completed_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  result_status: String,
  message_excerpt: String,
  completed_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptStarted(
      _,
      author_id,
      command_name,
      excerpt,
      seen_at_ms,
      started_at_ms,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        None,
      )
    Ok(CommandReceiptSeen(_, author_id, command_name, excerpt, seen_at_ms)) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        0,
        completed_at_ms,
        None,
      )
    Ok(CommandReceiptCompleted(
      _,
      author_id,
      command_name,
      excerpt,
      _,
      _,
      seen_at_ms,
      started_at_ms,
      _,
      acked_at_ms,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        acked_at_ms,
      )
    Ok(CommandReceiptAcked(_, acked_at_ms)) ->
      CommandReceiptCompleted(
        issue_id,
        "",
        "unknown",
        "",
        result_status,
        message_excerpt,
        0,
        0,
        completed_at_ms,
        Some(acked_at_ms),
      )
    _ ->
      CommandReceiptCompleted(
        issue_id,
        "",
        "unknown",
        "",
        result_status,
        message_excerpt,
        0,
        0,
        completed_at_ms,
        None,
      )
  }
}

fn acked_receipt(
  receipts: Dict(String, CommandReceiptState),
  comment_id: String,
  issue_id: String,
  acked_at_ms: Int,
) -> CommandReceiptState {
  case dict.get(receipts, comment_id) {
    Ok(CommandReceiptCompleted(
      _,
      author_id,
      command_name,
      excerpt,
      result_status,
      message_excerpt,
      seen_at_ms,
      started_at_ms,
      completed_at_ms,
      _,
    )) ->
      CommandReceiptCompleted(
        issue_id,
        author_id,
        command_name,
        excerpt,
        result_status,
        message_excerpt,
        seen_at_ms,
        started_at_ms,
        completed_at_ms,
        Some(acked_at_ms),
      )
    _ -> CommandReceiptAcked(issue_id, acked_at_ms)
  }
}

pub fn known_issue_ids(projection: Projection) -> List(String) {
  []
  |> append_unique_strings(run_issue_ids(projection.runs))
  |> append_unique_strings(dict.keys(projection.retries))
  |> append_unique_strings(dict.keys(projection.parked_issues))
  |> append_unique_strings(command_issue_ids(projection.commands))
  |> append_unique_strings(outbox_issue_ids(projection.outbox))
  |> append_unique_strings(dict.keys(projection.issue_counters))
  |> append_unique_strings(dict.keys(projection.known_workspaces))
}

pub fn known_workspace_for_issue(
  projection: Projection,
  issue_id: String,
) -> Result(String, Nil) {
  case dict.get(projection.known_workspaces, issue_id) {
    Ok(workspace) -> Ok(workspace.workspace_path)
    Error(_) -> Error(Nil)
  }
}

pub fn latest_counter(
  projection: Projection,
  issue_id: String,
) -> orchestrator_state.IssueCounter {
  case dict.get(projection.issue_counters, issue_id) {
    Ok(counter) ->
      orchestrator_state.IssueCounter(
        counter.failure_attempts,
        counter.worker_sessions,
      )
    Error(_) -> orchestrator_state.new_issue_counter()
  }
}

pub fn counter_has_source_run(
  projection: Projection,
  issue_id: String,
  run_id: String,
) -> Bool {
  case dict.get(projection.issue_counters, issue_id) {
    Ok(counter) -> list.contains(counter.source_run_ids, run_id)
    Error(_) -> False
  }
}

pub fn command_receipt(
  projection: Projection,
  comment_id: String,
) -> CommandReceiptState {
  dict.get(projection.command_receipts, comment_id)
  |> result.unwrap(CommandReceiptUnseen)
}

pub fn retry_due_at_ms(status: RetryStatus) -> Result(Int, Nil) {
  case status {
    RetryScheduled(_, delay_ms, _, _, scheduled_at_ms) ->
      Ok(scheduled_at_ms + delay_ms)
    RetryCancelled(_, _, _) -> Error(Nil)
  }
}

pub fn pending_outbox_replays(
  projection: Projection,
) -> Result(List(OutboxReplay), PendingOutboxError) {
  let entries =
    projection.outbox
    |> dict.to_list
    |> list.sort(by: compare_outbox_entries_by_time)
  pending_outbox_replays_loop(entries, [])
}

pub fn to_json(projection: Projection) -> json.Json {
  json.object([
    #("schema_version", json.int(record.schema_version)),
    #("kind", json.string("projection_snapshot")),
    #("runs", json.array(dict.to_list(projection.runs), of: run_entry_to_json)),
    #(
      "retries",
      json.array(dict.to_list(projection.retries), of: retry_entry_to_json),
    ),
    #(
      "parked_issues",
      json.array(
        dict.to_list(projection.parked_issues),
        of: parked_entry_to_json,
      ),
    ),
    #(
      "commands",
      json.array(dict.to_list(projection.commands), of: command_entry_to_json),
    ),
    #(
      "command_receipts",
      json.array(
        dict.to_list(projection.command_receipts),
        of: command_receipt_entry_to_json,
      ),
    ),
    #(
      "outbox",
      json.array(dict.to_list(projection.outbox), of: outbox_entry_to_json),
    ),
    #(
      "issue_counters",
      json.array(
        dict.to_list(projection.issue_counters),
        of: issue_counter_entry_to_json,
      ),
    ),
    #(
      "known_workspaces",
      json.array(
        dict.to_list(projection.known_workspaces),
        of: known_workspace_entry_to_json,
      ),
    ),
  ])
}

pub fn to_string(projection: Projection) -> String {
  projection |> to_json |> json.to_string
}

pub fn decode_string(contents: String) -> Result(Projection, String) {
  case json.parse(contents, snapshot_decoder()) {
    Ok(fields) ->
      Ok(Projection(
        runs: fields.runs
          |> list.map(fn(entry) {
            let RunSnapshot(run_id, status) = entry
            #(run_id, status)
          })
          |> dict.from_list,
        retries: fields.retries
          |> list.map(fn(entry) {
            let RetrySnapshot(issue_id, status) = entry
            #(issue_id, status)
          })
          |> dict.from_list,
        parked_issues: fields.parked_issues
          |> list.map(fn(entry) {
            let ParkedSnapshot(issue_id, parked) = entry
            #(issue_id, parked)
          })
          |> dict.from_list,
        commands: fields.commands
          |> list.map(fn(entry) {
            let CommandSnapshot(comment_id, status) = entry
            #(comment_id, status)
          })
          |> dict.from_list,
        command_receipts: fields.command_receipts
          |> list.map(fn(entry) {
            let CommandReceiptSnapshot(comment_id, receipt) = entry
            #(comment_id, receipt)
          })
          |> dict.from_list,
        outbox: fields.outbox
          |> list.map(fn(entry) {
            let OutboxSnapshot(outbox_id, status) = entry
            #(outbox_id, status)
          })
          |> dict.from_list,
        issue_counters: fields.issue_counters
          |> list.map(fn(entry) {
            let IssueCounterSnapshot(issue_id, status) = entry
            #(issue_id, status)
          })
          |> dict.from_list,
        known_workspaces: fields.known_workspaces
          |> list.map(fn(entry) {
            let KnownWorkspaceSnapshot(issue_id, workspace) = entry
            #(issue_id, workspace)
          })
          |> dict.from_list,
      ))
    Error(_) -> Error("malformed projection snapshot")
  }
}

fn run_entry_to_json(entry: #(String, RunStatus)) -> json.Json {
  let #(run_id, status) = entry
  case status {
    RunRunning(issue_id, issue_identifier, workspace_path, started_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("running")),
        #("issue_id", json.string(issue_id)),
        #("issue_identifier", json.string(issue_identifier)),
        #("workspace_path", json.string(workspace_path)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    RunFinished(issue_id, classification, token_total, turns, finished_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("finished")),
        #("issue_id", json.string(issue_id)),
        #("classification", json.string(classification)),
        #("token_total", json.int(token_total)),
        #("turns", json.int(turns)),
        #("finished_at_ms", json.int(finished_at_ms)),
      ])
    RunInterrupted(issue_id, reason, interrupted_at_ms) ->
      json.object([
        #("run_id", json.string(run_id)),
        #("status", json.string("interrupted")),
        #("issue_id", json.string(issue_id)),
        #("reason", json.string(reason)),
        #("interrupted_at_ms", json.int(interrupted_at_ms)),
      ])
  }
}

fn retry_entry_to_json(entry: #(String, RetryStatus)) -> json.Json {
  let #(issue_id, status) = entry
  case status {
    RetryScheduled(
      issue_identifier,
      delay_ms,
      generation,
      reason,
      scheduled_at_ms,
    ) ->
      json.object([
        #("issue_id", json.string(issue_id)),
        #("status", json.string("scheduled")),
        #("issue_identifier", json.string(issue_identifier)),
        #("delay_ms", json.int(delay_ms)),
        #("generation", json.int(generation)),
        #("reason", json.string(reason)),
        #("scheduled_at_ms", json.int(scheduled_at_ms)),
      ])
    RetryCancelled(generation, reason, cancelled_at_ms) ->
      json.object([
        #("issue_id", json.string(issue_id)),
        #("status", json.string("cancelled")),
        #("generation", json.int(generation)),
        #("reason", json.string(reason)),
        #("cancelled_at_ms", json.int(cancelled_at_ms)),
      ])
  }
}

fn parked_entry_to_json(entry: #(String, ParkedIssue)) -> json.Json {
  let #(issue_id, parked) = entry
  let ParkedIssue(
    issue_identifier,
    reason,
    observed_updated_at_ms,
    parked_at_ms,
    release_policy,
    issue_fingerprint,
  ) = parked
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    #("parked_at_ms", json.int(parked_at_ms)),
    #("release_policy", json.string(release_policy)),
    #("issue_fingerprint", json.string(issue_fingerprint)),
  ])
}

fn command_entry_to_json(entry: #(String, CommandStatus)) -> json.Json {
  let #(comment_id, status) = entry
  case status {
    CommandSeen(issue_id, author_id, command_name, excerpt, seen_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("seen")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
      ])
    CommandStarted(issue_id, command_name, started_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("started")),
        #("issue_id", json.string(issue_id)),
        #("command_name", json.string(command_name)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    CommandCompleted(issue_id, result_status, message_excerpt, completed_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("result_status", json.string(result_status)),
        #("message_excerpt", json.string(message_excerpt)),
        #("completed_at_ms", json.int(completed_at_ms)),
      ])
    CommandAcked(issue_id, acked_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("acked")),
        #("issue_id", json.string(issue_id)),
        #("acked_at_ms", json.int(acked_at_ms)),
      ])
  }
}

fn option_int_to_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn command_receipt_entry_to_json(
  entry: #(String, CommandReceiptState),
) -> json.Json {
  let #(comment_id, receipt) = entry
  case receipt {
    CommandReceiptUnseen ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("unseen")),
      ])
    CommandReceiptSeen(issue_id, author_id, command_name, excerpt, seen_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("seen")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
      ])
    CommandReceiptStarted(
      issue_id,
      author_id,
      command_name,
      excerpt,
      seen_at_ms,
      started_at_ms,
    ) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("started")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
        #("started_at_ms", json.int(started_at_ms)),
      ])
    CommandReceiptCompleted(
      issue_id,
      author_id,
      command_name,
      excerpt,
      result_status,
      message_excerpt,
      seen_at_ms,
      started_at_ms,
      completed_at_ms,
      acked_at_ms,
    ) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("author_id", json.string(author_id)),
        #("command_name", json.string(command_name)),
        #("excerpt", json.string(excerpt)),
        #("result_status", json.string(result_status)),
        #("message_excerpt", json.string(message_excerpt)),
        #("seen_at_ms", json.int(seen_at_ms)),
        #("started_at_ms", json.int(started_at_ms)),
        #("completed_at_ms", json.int(completed_at_ms)),
        #("acked_at_ms", option_int_to_json(acked_at_ms)),
      ])
    CommandReceiptAcked(issue_id, acked_at_ms) ->
      json.object([
        #("comment_id", json.string(comment_id)),
        #("status", json.string("acked")),
        #("issue_id", json.string(issue_id)),
        #("acked_at_ms", json.int(acked_at_ms)),
      ])
  }
}

fn outbox_entry_to_json(entry: #(String, OutboxStatus)) -> json.Json {
  let #(outbox_id, status) = entry
  case status {
    OutboxPending(issue_id, outbox_kind, dedupe_key, pending_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("pending")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("pending_at_ms", json.int(pending_at_ms)),
      ])
    OutboxPendingV2(
      issue_id,
      outbox_kind,
      dedupe_key,
      payload_json,
      pending_at_ms,
    ) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("pending_v2")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("dedupe_key", json.string(dedupe_key)),
        #("payload_json", json.string(payload_json)),
        #("pending_at_ms", json.int(pending_at_ms)),
      ])
    OutboxCompleted(issue_id, outbox_kind, completed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("completed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("completed_at_ms", json.int(completed_at_ms)),
      ])
    OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms) ->
      json.object([
        #("outbox_id", json.string(outbox_id)),
        #("status", json.string("failed")),
        #("issue_id", json.string(issue_id)),
        #("outbox_kind", json.string(outbox_kind)),
        #("error_code", json.string(error_code)),
        #("failed_at_ms", json.int(failed_at_ms)),
      ])
  }
}

fn issue_counter_entry_to_json(
  entry: #(String, IssueCounterStatus),
) -> json.Json {
  let #(issue_id, status) = entry
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(status.issue_identifier)),
    #("failure_attempts", json.int(status.failure_attempts)),
    #("worker_sessions", json.int(status.worker_sessions)),
    #("observed_updated_at_ms", json.int(status.observed_updated_at_ms)),
    #("source_run_ids", json.array(status.source_run_ids, of: json.string)),
    #("updated_at_ms", json.int(status.updated_at_ms)),
  ])
}

fn known_workspace_entry_to_json(
  entry: #(String, KnownWorkspace),
) -> json.Json {
  let #(issue_id, workspace) = entry
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(workspace.issue_identifier)),
    #("workspace_path", json.string(workspace.workspace_path)),
    #("recorded_at_ms", json.int(workspace.recorded_at_ms)),
  ])
}

fn snapshot_decoder() -> decode.Decoder(SnapshotFields) {
  use schema_version <- decode.field("schema_version", decode.int)
  use kind <- decode.field("kind", decode.string)
  use runs <- decode.field("runs", decode.list(of: run_snapshot_decoder()))
  use retries <- decode.field(
    "retries",
    decode.list(of: retry_snapshot_decoder()),
  )
  use parked_issues <- decode.field(
    "parked_issues",
    decode.list(of: parked_snapshot_decoder()),
  )
  use commands <- decode.field(
    "commands",
    decode.list(of: command_snapshot_decoder()),
  )
  use command_receipts <- decode.optional_field(
    "command_receipts",
    [],
    decode.list(of: command_receipt_snapshot_decoder()),
  )
  use outbox <- decode.field(
    "outbox",
    decode.list(of: outbox_snapshot_decoder()),
  )
  use issue_counters <- decode.optional_field(
    "issue_counters",
    [],
    decode.list(of: issue_counter_snapshot_decoder()),
  )
  use known_workspaces <- decode.optional_field(
    "known_workspaces",
    [],
    decode.list(of: known_workspace_snapshot_decoder()),
  )
  case
    schema_version == record.schema_version && kind == "projection_snapshot"
  {
    True ->
      decode.success(SnapshotFields(
        runs,
        retries,
        parked_issues,
        commands,
        command_receipts,
        outbox,
        issue_counters,
        known_workspaces,
      ))
    False ->
      decode.failure(
        SnapshotFields([], [], [], [], [], [], [], []),
        expected: "SnapshotFields",
      )
  }
}

fn run_snapshot_decoder() -> decode.Decoder(RunSnapshot) {
  use run_id <- decode.field("run_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "running" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use workspace_path <- decode.field("workspace_path", decode.string)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunRunning(issue_id, issue_identifier, workspace_path, started_at_ms),
      ))
    }
    "finished" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use classification <- decode.field("classification", decode.string)
      use token_total <- decode.field("token_total", decode.int)
      use turns <- decode.field("turns", decode.int)
      use finished_at_ms <- decode.field("finished_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunFinished(
          issue_id,
          classification,
          token_total,
          turns,
          finished_at_ms,
        ),
      ))
    }
    "interrupted" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use reason <- decode.field("reason", decode.string)
      use interrupted_at_ms <- decode.field("interrupted_at_ms", decode.int)
      decode.success(RunSnapshot(
        run_id,
        RunInterrupted(issue_id, reason, interrupted_at_ms),
      ))
    }
    _ ->
      decode.failure(
        RunSnapshot("", RunInterrupted("", "", 0)),
        expected: "RunSnapshot",
      )
  }
}

fn retry_snapshot_decoder() -> decode.Decoder(RetrySnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "scheduled" -> {
      use issue_identifier <- decode.field("issue_identifier", decode.string)
      use delay_ms <- decode.field("delay_ms", decode.int)
      use generation <- decode.field("generation", decode.int)
      use reason <- decode.field("reason", decode.string)
      use scheduled_at_ms <- decode.field("scheduled_at_ms", decode.int)
      decode.success(RetrySnapshot(
        issue_id,
        RetryScheduled(
          issue_identifier,
          delay_ms,
          generation,
          reason,
          scheduled_at_ms,
        ),
      ))
    }
    "cancelled" -> {
      use generation <- decode.field("generation", decode.int)
      use reason <- decode.field("reason", decode.string)
      use cancelled_at_ms <- decode.field("cancelled_at_ms", decode.int)
      decode.success(RetrySnapshot(
        issue_id,
        RetryCancelled(generation, reason, cancelled_at_ms),
      ))
    }
    _ ->
      decode.failure(
        RetrySnapshot("", RetryCancelled(0, "", 0)),
        expected: "RetrySnapshot",
      )
  }
}

fn parked_snapshot_decoder() -> decode.Decoder(ParkedSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use reason <- decode.field("reason", decode.string)
  use observed_updated_at_ms <- decode.field(
    "observed_updated_at_ms",
    decode.int,
  )
  use parked_at_ms <- decode.field("parked_at_ms", decode.int)
  use release_policy <- decode.optional_field(
    "release_policy",
    "explicit_unpark_only",
    decode.string,
  )
  use issue_fingerprint <- decode.optional_field(
    "issue_fingerprint",
    "",
    decode.string,
  )
  decode.success(ParkedSnapshot(
    issue_id,
    ParkedIssue(
      issue_identifier,
      reason,
      observed_updated_at_ms,
      parked_at_ms,
      release_policy,
      issue_fingerprint,
    ),
  ))
}

fn command_snapshot_decoder() -> decode.Decoder(CommandSnapshot) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandSeen(issue_id, author_id, command_name, excerpt, seen_at_ms),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandStarted(issue_id, command_name, started_at_ms),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandCompleted(
          issue_id,
          result_status,
          message_excerpt,
          completed_at_ms,
        ),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(CommandSnapshot(
        comment_id,
        CommandAcked(issue_id, acked_at_ms),
      ))
    }
    _ ->
      decode.failure(
        CommandSnapshot("", CommandAcked("", 0)),
        expected: "CommandSnapshot",
      )
  }
}

fn command_receipt_snapshot_decoder() -> decode.Decoder(CommandReceiptSnapshot) {
  use comment_id <- decode.field("comment_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "unseen" ->
      decode.success(CommandReceiptSnapshot(comment_id, CommandReceiptUnseen))
    "seen" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptSeen(
          issue_id,
          author_id,
          command_name,
          excerpt,
          seen_at_ms,
        ),
      ))
    }
    "started" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptStarted(
          issue_id,
          author_id,
          command_name,
          excerpt,
          seen_at_ms,
          started_at_ms,
        ),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use author_id <- decode.field("author_id", decode.string)
      use command_name <- decode.field("command_name", decode.string)
      use excerpt <- decode.field("excerpt", decode.string)
      use result_status <- decode.field("result_status", decode.string)
      use message_excerpt <- decode.field("message_excerpt", decode.string)
      use seen_at_ms <- decode.field("seen_at_ms", decode.int)
      use started_at_ms <- decode.field("started_at_ms", decode.int)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      use acked_at_ms <- decode.optional_field(
        "acked_at_ms",
        None,
        decode.optional(decode.int),
      )
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptCompleted(
          issue_id,
          author_id,
          command_name,
          excerpt,
          result_status,
          message_excerpt,
          seen_at_ms,
          started_at_ms,
          completed_at_ms,
          acked_at_ms,
        ),
      ))
    }
    "acked" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use acked_at_ms <- decode.field("acked_at_ms", decode.int)
      decode.success(CommandReceiptSnapshot(
        comment_id,
        CommandReceiptAcked(issue_id, acked_at_ms),
      ))
    }
    _ ->
      decode.failure(
        CommandReceiptSnapshot("", CommandReceiptUnseen),
        expected: "CommandReceiptSnapshot",
      )
  }
}

fn outbox_snapshot_decoder() -> decode.Decoder(OutboxSnapshot) {
  use outbox_id <- decode.field("outbox_id", decode.string)
  use status <- decode.field("status", decode.string)
  case status {
    "pending" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use pending_at_ms <- decode.field("pending_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxPending(issue_id, outbox_kind, dedupe_key, pending_at_ms),
      ))
    }
    "pending_v2" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use dedupe_key <- decode.field("dedupe_key", decode.string)
      use payload_json <- decode.field("payload_json", decode.string)
      use pending_at_ms <- decode.field("pending_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxPendingV2(
          issue_id,
          outbox_kind,
          dedupe_key,
          payload_json,
          pending_at_ms,
        ),
      ))
    }
    "completed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use completed_at_ms <- decode.field("completed_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxCompleted(issue_id, outbox_kind, completed_at_ms),
      ))
    }
    "failed" -> {
      use issue_id <- decode.field("issue_id", decode.string)
      use outbox_kind <- decode.field("outbox_kind", decode.string)
      use error_code <- decode.field("error_code", decode.string)
      use failed_at_ms <- decode.field("failed_at_ms", decode.int)
      decode.success(OutboxSnapshot(
        outbox_id,
        OutboxFailed(issue_id, outbox_kind, error_code, failed_at_ms),
      ))
    }
    _ ->
      decode.failure(
        OutboxSnapshot("", OutboxFailed("", "", "", 0)),
        expected: "OutboxSnapshot",
      )
  }
}

fn issue_counter_snapshot_decoder() -> decode.Decoder(IssueCounterSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use failure_attempts <- decode.field("failure_attempts", decode.int)
  use worker_sessions <- decode.field("worker_sessions", decode.int)
  use observed_updated_at_ms <- decode.field(
    "observed_updated_at_ms",
    decode.int,
  )
  use source_run_ids <- decode.optional_field(
    "source_run_ids",
    [],
    decode.list(of: decode.string),
  )
  use updated_at_ms <- decode.field("updated_at_ms", decode.int)
  decode.success(IssueCounterSnapshot(
    issue_id,
    IssueCounterStatus(
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_ids,
      updated_at_ms,
    ),
  ))
}

fn known_workspace_snapshot_decoder() -> decode.Decoder(KnownWorkspaceSnapshot) {
  use issue_id <- decode.field("issue_id", decode.string)
  use issue_identifier <- decode.field("issue_identifier", decode.string)
  use workspace_path <- decode.field("workspace_path", decode.string)
  use recorded_at_ms <- decode.field("recorded_at_ms", decode.int)
  decode.success(KnownWorkspaceSnapshot(
    issue_id,
    KnownWorkspace(issue_identifier, workspace_path, recorded_at_ms),
  ))
}

fn run_issue_ids(runs: Dict(String, RunStatus)) -> List(String) {
  runs
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      RunRunning(issue_id, _, _, _) -> issue_id
      RunFinished(issue_id, _, _, _, _) -> issue_id
      RunInterrupted(issue_id, _, _) -> issue_id
    }
  })
}

fn command_issue_ids(commands: Dict(String, CommandStatus)) -> List(String) {
  commands
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      CommandSeen(issue_id, _, _, _, _) -> issue_id
      CommandStarted(issue_id, _, _) -> issue_id
      CommandCompleted(issue_id, _, _, _) -> issue_id
      CommandAcked(issue_id, _) -> issue_id
    }
  })
}

fn outbox_issue_ids(outbox: Dict(String, OutboxStatus)) -> List(String) {
  outbox
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(_, status) = entry
    case status {
      OutboxPending(issue_id, _, _, _) -> issue_id
      OutboxPendingV2(issue_id, _, _, _, _) -> issue_id
      OutboxCompleted(issue_id, _, _) -> issue_id
      OutboxFailed(issue_id, _, _, _) -> issue_id
    }
  })
}

fn append_unique_strings(
  values: List(String),
  more: List(String),
) -> List(String) {
  list.fold(more, values, insert_unique_string)
}

fn insert_unique_string(values: List(String), value: String) -> List(String) {
  case list.contains(values, value) {
    True -> values
    False -> [value, ..values]
  }
}

fn pending_outbox_replays_loop(
  entries: List(#(String, OutboxStatus)),
  acc: List(OutboxReplay),
) -> Result(List(OutboxReplay), PendingOutboxError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [entry, ..rest] -> {
      let #(outbox_id, status) = entry
      case status {
        OutboxPending(_, _, _, _) -> pending_outbox_replays_loop(rest, acc)
        OutboxPendingV2(issue_id, outbox_kind, dedupe_key, payload_json, _) ->
          pending_outbox_replays_loop(rest, [
            OutboxReplay(
              outbox_id,
              issue_id,
              outbox_kind,
              dedupe_key,
              payload_json,
            ),
            ..acc
          ])
        OutboxCompleted(_, _, _) | OutboxFailed(_, _, _, _) ->
          pending_outbox_replays_loop(rest, acc)
      }
    }
  }
}

fn compare_outbox_entries_by_time(
  a: #(String, OutboxStatus),
  b: #(String, OutboxStatus),
) -> Order {
  let #(a_id, a_status) = a
  let #(b_id, b_status) = b
  case int.compare(outbox_status_time(a_status), outbox_status_time(b_status)) {
    Eq -> string.compare(a_id, b_id)
    order -> order
  }
}

fn outbox_status_time(status: OutboxStatus) -> Int {
  case status {
    OutboxPending(_, _, _, pending_at_ms) -> pending_at_ms
    OutboxPendingV2(_, _, _, _, pending_at_ms) -> pending_at_ms
    OutboxCompleted(_, _, completed_at_ms) -> completed_at_ms
    OutboxFailed(_, _, _, failed_at_ms) -> failed_at_ms
  }
}

pub fn describe_pending_outbox_error(error: PendingOutboxError) -> String {
  case error {
    OutboxPayloadMissing(outbox_id) -> "outbox_payload_missing:" <> outbox_id
  }
}

pub fn retry_status_to_string(status: RetryStatus) -> String {
  case status {
    RetryScheduled(_, delay_ms, generation, reason, scheduled_at_ms) ->
      "scheduled delay_ms="
      <> int.to_string(delay_ms)
      <> " generation="
      <> int.to_string(generation)
      <> " reason="
      <> reason
      <> " scheduled_at_ms="
      <> int.to_string(scheduled_at_ms)
    RetryCancelled(generation, reason, cancelled_at_ms) ->
      "cancelled generation="
      <> int.to_string(generation)
      <> " reason="
      <> reason
      <> " cancelled_at_ms="
      <> int.to_string(cancelled_at_ms)
  }
}
