import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import scherzo/state/record

pub type Projection {
  Projection(
    runs: Dict(String, RunStatus),
    retries: Dict(String, RetryStatus),
    parked_issues: Dict(String, ParkedIssue),
    commands: Dict(String, CommandStatus),
    outbox: Dict(String, OutboxStatus),
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

pub type OutboxStatus {
  OutboxPending(
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
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

type OutboxSnapshot {
  OutboxSnapshot(outbox_id: String, status: OutboxStatus)
}

type SnapshotFields {
  SnapshotFields(
    runs: List(RunSnapshot),
    retries: List(RetrySnapshot),
    parked_issues: List(ParkedSnapshot),
    commands: List(CommandSnapshot),
    outbox: List(OutboxSnapshot),
  )
}

pub fn new() -> Projection {
  Projection(
    runs: dict.new(),
    retries: dict.new(),
    parked_issues: dict.new(),
    commands: dict.new(),
    outbox: dict.new(),
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
          ParkedIssue(issue_identifier, reason, observed_updated_at_ms, at_ms),
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
    ) ->
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandSeen(issue_id, author_id, command_name, excerpt, at_ms),
        ),
      )
    record.LinearCommandStarted(comment_id, issue_id, command_name) ->
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandStarted(issue_id, command_name, at_ms),
        ),
      )
    record.LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) ->
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandCompleted(issue_id, status, message_excerpt, at_ms),
        ),
      )
    record.LinearCommandAcked(comment_id, issue_id) ->
      Projection(
        ..projection,
        commands: dict.insert(
          projection.commands,
          comment_id,
          CommandAcked(issue_id, at_ms),
        ),
      )
    record.OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) ->
      Projection(
        ..projection,
        outbox: dict.insert(
          projection.outbox,
          outbox_id,
          OutboxPending(issue_id, outbox_kind, dedupe_key, at_ms),
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
      "outbox",
      json.array(dict.to_list(projection.outbox), of: outbox_entry_to_json),
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
        outbox: fields.outbox
          |> list.map(fn(entry) {
            let OutboxSnapshot(outbox_id, status) = entry
            #(outbox_id, status)
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
  ) = parked
  json.object([
    #("issue_id", json.string(issue_id)),
    #("issue_identifier", json.string(issue_identifier)),
    #("reason", json.string(reason)),
    #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    #("parked_at_ms", json.int(parked_at_ms)),
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
  use outbox <- decode.field(
    "outbox",
    decode.list(of: outbox_snapshot_decoder()),
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
        outbox,
      ))
    False ->
      decode.failure(
        SnapshotFields([], [], [], [], []),
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
  decode.success(ParkedSnapshot(
    issue_id,
    ParkedIssue(issue_identifier, reason, observed_updated_at_ms, parked_at_ms),
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
