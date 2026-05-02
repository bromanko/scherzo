import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/log

pub const schema_version = 1

pub const max_excerpt_chars = 500

pub type LedgerRecord {
  LedgerRecord(record_id: String, at_ms: Int, body: RecordBody)
}

pub type RecordBody {
  RunStarted(
    run_id: String,
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
  RunFinished(
    run_id: String,
    issue_id: String,
    classification: String,
    token_total: Int,
    turns: Int,
  )
  RunInterrupted(run_id: String, issue_id: String, reason: String)
  RetryScheduled(
    issue_id: String,
    issue_identifier: String,
    delay_ms: Int,
    generation: Int,
    reason: String,
  )
  RetryCancelled(issue_id: String, generation: Int, reason: String)
  IssueCounterUpdated(
    issue_id: String,
    issue_identifier: String,
    failure_attempts: Int,
    worker_sessions: Int,
    observed_updated_at_ms: Int,
    source_run_id: Option(String),
  )
  KnownWorkspace(
    issue_id: String,
    issue_identifier: String,
    workspace_path: String,
  )
  IssueParked(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    observed_updated_at_ms: Int,
  )
  IssueParkedV2(
    issue_id: String,
    issue_identifier: String,
    reason: String,
    release_policy: String,
    issue_fingerprint: String,
    observed_updated_at_ms: Int,
  )
  IssueUnparked(issue_id: String, issue_identifier: String, reason: String)
  LinearCommandSeen(
    comment_id: String,
    issue_id: String,
    author_id: String,
    command_name: String,
    excerpt: String,
  )
  LinearCommandStarted(
    comment_id: String,
    issue_id: String,
    command_name: String,
  )
  LinearCommandCompleted(
    comment_id: String,
    issue_id: String,
    status: String,
    message_excerpt: String,
  )
  LinearCommandAcked(comment_id: String, issue_id: String)
  OutboxPending(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
  )
  OutboxPendingV2(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    dedupe_key: String,
    payload_json: String,
  )
  OutboxCompleted(outbox_id: String, issue_id: String, outbox_kind: String)
  OutboxFailed(
    outbox_id: String,
    issue_id: String,
    outbox_kind: String,
    error_code: String,
  )
}

pub type DecodeError {
  MalformedJson(String)
  UnsupportedVersion(Int)
  InvalidRecord(String)
  UnknownKind(String)
}

type RecordFields {
  RecordFields(
    schema_version: Int,
    record_id: String,
    at_ms: Int,
    kind: String,
    run_id: Option(String),
    issue_id: Option(String),
    issue_identifier: Option(String),
    workspace_path: Option(String),
    classification: Option(String),
    token_total: Option(Int),
    turns: Option(Int),
    reason: Option(String),
    delay_ms: Option(Int),
    generation: Option(Int),
    failure_attempts: Option(Int),
    worker_sessions: Option(Int),
    observed_updated_at_ms: Option(Int),
    source_run_id: Option(String),
    release_policy: Option(String),
    issue_fingerprint: Option(String),
    comment_id: Option(String),
    author_id: Option(String),
    command_name: Option(String),
    excerpt: Option(String),
    status: Option(String),
    message_excerpt: Option(String),
    outbox_id: Option(String),
    outbox_kind: Option(String),
    dedupe_key: Option(String),
    payload_json: Option(String),
    error_code: Option(String),
  )
}

pub fn new(at_ms: Int, sequence: Int, body: RecordBody) -> LedgerRecord {
  LedgerRecord(
    record_id: int.to_string(at_ms)
      <> "-"
      <> int.to_string(sequence)
      <> "-"
      <> kind(body),
    at_ms: at_ms,
    body: body,
  )
}

pub fn with_id(
  record_id: String,
  at_ms: Int,
  body: RecordBody,
) -> LedgerRecord {
  LedgerRecord(record_id: record_id, at_ms: at_ms, body: body)
}

pub fn kind(body: RecordBody) -> String {
  case body {
    RunStarted(..) -> "run_started"
    RunFinished(..) -> "run_finished"
    RunInterrupted(..) -> "run_interrupted"
    RetryScheduled(..) -> "retry_scheduled"
    RetryCancelled(..) -> "retry_cancelled"
    IssueCounterUpdated(..) -> "issue_counter_updated"
    KnownWorkspace(..) -> "known_workspace"
    IssueParked(..) -> "issue_parked"
    IssueParkedV2(..) -> "issue_parked_v2"
    IssueUnparked(..) -> "issue_unparked"
    LinearCommandSeen(..) -> "linear_command_seen"
    LinearCommandStarted(..) -> "linear_command_started"
    LinearCommandCompleted(..) -> "linear_command_completed"
    LinearCommandAcked(..) -> "linear_command_acked"
    OutboxPending(..) -> "outbox_pending"
    OutboxPendingV2(..) -> "outbox_pending_v2"
    OutboxCompleted(..) -> "outbox_completed"
    OutboxFailed(..) -> "outbox_failed"
  }
}

pub fn to_json(ledger_record: LedgerRecord) -> json.Json {
  [
    #("schema_version", json.int(schema_version)),
    #("record_id", json.string(ledger_record.record_id)),
    #("at_ms", json.int(ledger_record.at_ms)),
    #("kind", json.string(kind(ledger_record.body))),
    ..body_entries(ledger_record.body)
  ]
  |> json.object
}

pub fn to_string(ledger_record: LedgerRecord) -> String {
  ledger_record |> to_json |> json.to_string
}

pub fn decode_string(line: String) -> Result(LedgerRecord, DecodeError) {
  case json.parse(line, fields_decoder()) {
    Error(json.UnexpectedEndOfInput) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnexpectedByte(_)) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnexpectedSequence(_)) -> Error(MalformedJson("malformed JSON"))
    Error(json.UnableToDecode(_)) ->
      Error(InvalidRecord("invalid ledger record shape"))
    Ok(fields) -> fields_to_record(fields)
  }
}

pub fn redact_excerpts(
  ledger_record: LedgerRecord,
  secrets: List(String),
) -> LedgerRecord {
  LedgerRecord(..ledger_record, body: redact_body(ledger_record.body, secrets))
}

pub fn describe_error(error: DecodeError) -> String {
  case error {
    MalformedJson(reason) -> reason
    UnsupportedVersion(version) ->
      "unsupported schema version " <> int.to_string(version)
    InvalidRecord(reason) -> reason
    UnknownKind(kind) -> "unknown ledger record kind " <> kind
  }
}

fn body_entries(body: RecordBody) -> List(#(String, json.Json)) {
  case body {
    RunStarted(run_id, issue_id, issue_identifier, workspace_path) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("workspace_path", json.string(workspace_path)),
    ]
    RunFinished(run_id, issue_id, classification, token_total, turns) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("classification", json.string(classification)),
      #("token_total", json.int(token_total)),
      #("turns", json.int(turns)),
    ]
    RunInterrupted(run_id, issue_id, reason) -> [
      #("run_id", json.string(run_id)),
      #("issue_id", json.string(issue_id)),
      #("reason", json.string(reason)),
    ]
    RetryScheduled(issue_id, issue_identifier, delay_ms, generation, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("delay_ms", json.int(delay_ms)),
      #("generation", json.int(generation)),
      #("reason", json.string(reason)),
    ]
    RetryCancelled(issue_id, generation, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("generation", json.int(generation)),
      #("reason", json.string(reason)),
    ]
    IssueCounterUpdated(
      issue_id,
      issue_identifier,
      failure_attempts,
      worker_sessions,
      observed_updated_at_ms,
      source_run_id,
    ) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("failure_attempts", json.int(failure_attempts)),
      #("worker_sessions", json.int(worker_sessions)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
      #("source_run_id", option_string_to_json(source_run_id)),
    ]
    KnownWorkspace(issue_id, issue_identifier, workspace_path) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("workspace_path", json.string(workspace_path)),
    ]
    IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    ]
    IssueParkedV2(
      issue_id,
      issue_identifier,
      reason,
      release_policy,
      issue_fingerprint,
      observed_updated_at_ms,
    ) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
      #("release_policy", json.string(release_policy)),
      #("issue_fingerprint", json.string(issue_fingerprint)),
      #("observed_updated_at_ms", json.int(observed_updated_at_ms)),
    ]
    IssueUnparked(issue_id, issue_identifier, reason) -> [
      #("issue_id", json.string(issue_id)),
      #("issue_identifier", json.string(issue_identifier)),
      #("reason", json.string(reason)),
    ]
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("author_id", json.string(author_id)),
      #("command_name", json.string(command_name)),
      #("excerpt", json.string(excerpt)),
    ]
    LinearCommandStarted(comment_id, issue_id, command_name) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("command_name", json.string(command_name)),
    ]
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
      #("status", json.string(status)),
      #("message_excerpt", json.string(message_excerpt)),
    ]
    LinearCommandAcked(comment_id, issue_id) -> [
      #("comment_id", json.string(comment_id)),
      #("issue_id", json.string(issue_id)),
    ]
    OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
    ]
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("dedupe_key", json.string(dedupe_key)),
      #("payload_json", json.string(payload_json)),
    ]
    OutboxCompleted(outbox_id, issue_id, outbox_kind) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
    ]
    OutboxFailed(outbox_id, issue_id, outbox_kind, error_code) -> [
      #("outbox_id", json.string(outbox_id)),
      #("issue_id", json.string(issue_id)),
      #("outbox_kind", json.string(outbox_kind)),
      #("error_code", json.string(error_code)),
    ]
  }
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn fields_to_record(fields: RecordFields) -> Result(LedgerRecord, DecodeError) {
  case fields.schema_version != schema_version {
    True -> Error(UnsupportedVersion(fields.schema_version))
    False -> {
      use body <- result.try(body_from_fields(fields))
      Ok(LedgerRecord(
        record_id: fields.record_id,
        at_ms: fields.at_ms,
        body: body,
      ))
    }
  }
}

fn body_from_fields(fields: RecordFields) -> Result(RecordBody, DecodeError) {
  case fields.kind {
    "run_started" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      Ok(RunStarted(run_id, issue_id, issue_identifier, workspace_path))
    }
    "run_finished" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use classification <- result.try(required_string(
        fields.classification,
        "classification",
      ))
      use token_total <- result.try(required_int(
        fields.token_total,
        "token_total",
      ))
      use turns <- result.try(required_int(fields.turns, "turns"))
      Ok(RunFinished(run_id, issue_id, classification, token_total, turns))
    }
    "run_interrupted" -> {
      use run_id <- result.try(required_string(fields.run_id, "run_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RunInterrupted(run_id, issue_id, reason))
    }
    "retry_scheduled" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use delay_ms <- result.try(required_int(fields.delay_ms, "delay_ms"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RetryScheduled(
        issue_id,
        issue_identifier,
        delay_ms,
        generation,
        reason,
      ))
    }
    "retry_cancelled" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use generation <- result.try(required_int(fields.generation, "generation"))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(RetryCancelled(issue_id, generation, reason))
    }
    "issue_counter_updated" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use failure_attempts <- result.try(required_int(
        fields.failure_attempts,
        "failure_attempts",
      ))
      use worker_sessions <- result.try(required_int(
        fields.worker_sessions,
        "worker_sessions",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueCounterUpdated(
        issue_id,
        issue_identifier,
        failure_attempts,
        worker_sessions,
        observed_updated_at_ms,
        fields.source_run_id,
      ))
    }
    "known_workspace" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use workspace_path <- result.try(required_string(
        fields.workspace_path,
        "workspace_path",
      ))
      Ok(KnownWorkspace(issue_id, issue_identifier, workspace_path))
    }
    "issue_parked" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueParked(issue_id, issue_identifier, reason, observed_updated_at_ms))
    }
    "issue_parked_v2" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      use release_policy <- result.try(required_string(
        fields.release_policy,
        "release_policy",
      ))
      use issue_fingerprint <- result.try(required_string(
        fields.issue_fingerprint,
        "issue_fingerprint",
      ))
      use observed_updated_at_ms <- result.try(required_int(
        fields.observed_updated_at_ms,
        "observed_updated_at_ms",
      ))
      Ok(IssueParkedV2(
        issue_id,
        issue_identifier,
        reason,
        release_policy,
        issue_fingerprint,
        observed_updated_at_ms,
      ))
    }
    "issue_unparked" -> {
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use issue_identifier <- result.try(required_string(
        fields.issue_identifier,
        "issue_identifier",
      ))
      use reason <- result.try(required_string(fields.reason, "reason"))
      Ok(IssueUnparked(issue_id, issue_identifier, reason))
    }
    "linear_command_seen" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use author_id <- result.try(required_string(fields.author_id, "author_id"))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      use excerpt <- result.try(required_string(fields.excerpt, "excerpt"))
      Ok(LinearCommandSeen(
        comment_id,
        issue_id,
        author_id,
        command_name,
        excerpt,
      ))
    }
    "linear_command_started" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use command_name <- result.try(required_string(
        fields.command_name,
        "command_name",
      ))
      Ok(LinearCommandStarted(comment_id, issue_id, command_name))
    }
    "linear_command_completed" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use status <- result.try(required_string(fields.status, "status"))
      use message_excerpt <- result.try(required_string(
        fields.message_excerpt,
        "message_excerpt",
      ))
      Ok(LinearCommandCompleted(comment_id, issue_id, status, message_excerpt))
    }
    "linear_command_acked" -> {
      use comment_id <- result.try(required_string(
        fields.comment_id,
        "comment_id",
      ))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      Ok(LinearCommandAcked(comment_id, issue_id))
    }
    "outbox_pending" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      Ok(OutboxPending(outbox_id, issue_id, outbox_kind, dedupe_key))
    }
    "outbox_pending_v2" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use dedupe_key <- result.try(required_string(
        fields.dedupe_key,
        "dedupe_key",
      ))
      use payload_json <- result.try(required_string(
        fields.payload_json,
        "payload_json",
      ))
      Ok(OutboxPendingV2(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
        payload_json,
      ))
    }
    "outbox_completed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      Ok(OutboxCompleted(outbox_id, issue_id, outbox_kind))
    }
    "outbox_failed" -> {
      use outbox_id <- result.try(required_string(fields.outbox_id, "outbox_id"))
      use issue_id <- result.try(required_string(fields.issue_id, "issue_id"))
      use outbox_kind <- result.try(required_string(
        fields.outbox_kind,
        "outbox_kind",
      ))
      use error_code <- result.try(required_string(
        fields.error_code,
        "error_code",
      ))
      Ok(OutboxFailed(outbox_id, issue_id, outbox_kind, error_code))
    }
    other -> Error(UnknownKind(other))
  }
}

fn fields_decoder() -> decode.Decoder(RecordFields) {
  use schema_version <- decode.field("schema_version", decode.int)
  use record_id <- decode.field("record_id", decode.string)
  use at_ms <- decode.field("at_ms", decode.int)
  use kind <- decode.field("kind", decode.string)
  use run_id <- decode.optional_field(
    "run_id",
    None,
    decode.optional(decode.string),
  )
  use issue_id <- decode.optional_field(
    "issue_id",
    None,
    decode.optional(decode.string),
  )
  use issue_identifier <- decode.optional_field(
    "issue_identifier",
    None,
    decode.optional(decode.string),
  )
  use workspace_path <- decode.optional_field(
    "workspace_path",
    None,
    decode.optional(decode.string),
  )
  use classification <- decode.optional_field(
    "classification",
    None,
    decode.optional(decode.string),
  )
  use token_total <- decode.optional_field(
    "token_total",
    None,
    decode.optional(decode.int),
  )
  use turns <- decode.optional_field("turns", None, decode.optional(decode.int))
  use reason <- decode.optional_field(
    "reason",
    None,
    decode.optional(decode.string),
  )
  use delay_ms <- decode.optional_field(
    "delay_ms",
    None,
    decode.optional(decode.int),
  )
  use generation <- decode.optional_field(
    "generation",
    None,
    decode.optional(decode.int),
  )
  use failure_attempts <- decode.optional_field(
    "failure_attempts",
    None,
    decode.optional(decode.int),
  )
  use worker_sessions <- decode.optional_field(
    "worker_sessions",
    None,
    decode.optional(decode.int),
  )
  use observed_updated_at_ms <- decode.optional_field(
    "observed_updated_at_ms",
    None,
    decode.optional(decode.int),
  )
  use source_run_id <- decode.optional_field(
    "source_run_id",
    None,
    decode.optional(decode.string),
  )
  use release_policy <- decode.optional_field(
    "release_policy",
    None,
    decode.optional(decode.string),
  )
  use issue_fingerprint <- decode.optional_field(
    "issue_fingerprint",
    None,
    decode.optional(decode.string),
  )
  use comment_id <- decode.optional_field(
    "comment_id",
    None,
    decode.optional(decode.string),
  )
  use author_id <- decode.optional_field(
    "author_id",
    None,
    decode.optional(decode.string),
  )
  use command_name <- decode.optional_field(
    "command_name",
    None,
    decode.optional(decode.string),
  )
  use excerpt <- decode.optional_field(
    "excerpt",
    None,
    decode.optional(decode.string),
  )
  use status <- decode.optional_field(
    "status",
    None,
    decode.optional(decode.string),
  )
  use message_excerpt <- decode.optional_field(
    "message_excerpt",
    None,
    decode.optional(decode.string),
  )
  use outbox_id <- decode.optional_field(
    "outbox_id",
    None,
    decode.optional(decode.string),
  )
  use outbox_kind <- decode.optional_field(
    "outbox_kind",
    None,
    decode.optional(decode.string),
  )
  use dedupe_key <- decode.optional_field(
    "dedupe_key",
    None,
    decode.optional(decode.string),
  )
  use payload_json <- decode.optional_field(
    "payload_json",
    None,
    decode.optional(decode.string),
  )
  use error_code <- decode.optional_field(
    "error_code",
    None,
    decode.optional(decode.string),
  )
  decode.success(RecordFields(
    schema_version: schema_version,
    record_id: record_id,
    at_ms: at_ms,
    kind: kind,
    run_id: run_id,
    issue_id: issue_id,
    issue_identifier: issue_identifier,
    workspace_path: workspace_path,
    classification: classification,
    token_total: token_total,
    turns: turns,
    reason: reason,
    delay_ms: delay_ms,
    generation: generation,
    failure_attempts: failure_attempts,
    worker_sessions: worker_sessions,
    observed_updated_at_ms: observed_updated_at_ms,
    source_run_id: source_run_id,
    release_policy: release_policy,
    issue_fingerprint: issue_fingerprint,
    comment_id: comment_id,
    author_id: author_id,
    command_name: command_name,
    excerpt: excerpt,
    status: status,
    message_excerpt: message_excerpt,
    outbox_id: outbox_id,
    outbox_kind: outbox_kind,
    dedupe_key: dedupe_key,
    payload_json: payload_json,
    error_code: error_code,
  ))
}

fn required_string(
  value: Option(String),
  field: String,
) -> Result(String, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(InvalidRecord("missing " <> field))
  }
}

fn required_int(value: Option(Int), field: String) -> Result(Int, DecodeError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(InvalidRecord("missing " <> field))
  }
}

fn redact_body(body: RecordBody, secrets: List(String)) -> RecordBody {
  case body {
    LinearCommandSeen(comment_id, issue_id, author_id, command_name, excerpt) ->
      LinearCommandSeen(
        comment_id,
        issue_id,
        author_id,
        command_name,
        safe_excerpt(excerpt, secrets),
      )
    LinearCommandCompleted(comment_id, issue_id, status, message_excerpt) ->
      LinearCommandCompleted(
        comment_id,
        issue_id,
        status,
        safe_excerpt(message_excerpt, secrets),
      )
    OutboxPendingV2(outbox_id, issue_id, outbox_kind, dedupe_key, payload_json) ->
      OutboxPendingV2(
        outbox_id,
        issue_id,
        outbox_kind,
        dedupe_key,
        safe_payload(payload_json, secrets),
      )
    other -> other
  }
}

fn safe_payload(value: String, secrets: List(String)) -> String {
  log.redact("outbox_payload", value, secrets)
  |> log.truncate(max_excerpt_chars)
}

fn safe_excerpt(value: String, secrets: List(String)) -> String {
  log.redact("ledger_excerpt", value, secrets)
  |> log.truncate(max_excerpt_chars)
}
