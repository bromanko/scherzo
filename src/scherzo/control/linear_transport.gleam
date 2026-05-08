import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/linear_parser
import scherzo/linear
import scherzo/linear_comment_format as comment_format
import scherzo/log
import scherzo/state/projection

pub type TransportState {
  TransportState(
    daemon_started_at_ms: Int,
    processed_comment_ids: Dict(String, Bool),
    command_receipts: Dict(String, projection.CommandReceiptState),
  )
}

pub type TransportAction {
  SubmitCommand(
    comment: linear.LinearComment,
    parsed: linear_parser.ParsedLinearCommand,
  )
  PostAck(issue_id: String, source_comment_id: String, body: String)
  LogIgnored(reason: String, comment_id: String)
}

type ReceiptHandling {
  ReceiptSkip
  ReceiptAck(TransportState, TransportAction)
  ReceiptProcessNormally
}

pub fn new_state(daemon_started_at_ms: Int) -> TransportState {
  TransportState(
    daemon_started_at_ms: daemon_started_at_ms,
    processed_comment_ids: dict.new(),
    command_receipts: dict.new(),
  )
}

pub fn new_state_with_receipts(
  daemon_started_at_ms: Int,
  command_receipts: Dict(String, projection.CommandReceiptState),
) -> TransportState {
  TransportState(
    daemon_started_at_ms: daemon_started_at_ms,
    processed_comment_ids: dict.new(),
    command_receipts: command_receipts,
  )
}

pub fn has_processed(state: TransportState, comment_id: String) -> Bool {
  let TransportState(processed_comment_ids: processed, ..) = state
  dict.has_key(processed, comment_id)
}

pub fn mark_processed(
  state: TransportState,
  comment_id: String,
) -> TransportState {
  let TransportState(processed_comment_ids: processed, ..) = state
  TransportState(
    ..state,
    processed_comment_ids: dict.insert(processed, comment_id, True),
  )
}

pub fn process_comments(
  state: TransportState,
  config: config_types.LinearCommandConfig,
  comments: List(linear.LinearComment),
  issue_sessions: Dict(String, String),
) -> #(TransportState, List(TransportAction)) {
  case config.enabled {
    False -> #(state, [])
    True -> process_loop(state, config, comments, issue_sessions, 0, [])
  }
}

fn process_loop(
  state: TransportState,
  config: config_types.LinearCommandConfig,
  comments: List(linear.LinearComment),
  issue_sessions: Dict(String, String),
  processed_this_tick: Int,
  actions: List(TransportAction),
) -> #(TransportState, List(TransportAction)) {
  case comments {
    [] -> #(state, list.reverse(actions))
    [comment, ..rest] ->
      case has_processed(state, comment.id) {
        True ->
          process_loop(
            state,
            config,
            rest,
            issue_sessions,
            processed_this_tick,
            actions,
          )
        False ->
          case durable_receipt_handling(state, config, comment) {
            ReceiptSkip ->
              process_loop(
                state,
                config,
                rest,
                issue_sessions,
                processed_this_tick,
                actions,
              )
            ReceiptAck(next_state, action) ->
              case processed_this_tick >= config.max_comments_per_tick {
                True ->
                  process_loop(
                    state,
                    config,
                    rest,
                    issue_sessions,
                    processed_this_tick,
                    [LogIgnored("deferred_over_limit", comment.id), ..actions],
                  )
                False ->
                  process_loop(
                    next_state,
                    config,
                    rest,
                    issue_sessions,
                    processed_this_tick + 1,
                    [action, ..actions],
                  )
              }
            ReceiptProcessNormally ->
              case
                !linear_parser.contains_command_line(
                  config.prefix,
                  comment.body,
                )
              {
                True ->
                  process_loop(
                    state,
                    config,
                    rest,
                    issue_sessions,
                    processed_this_tick,
                    actions,
                  )
                False ->
                  case processed_this_tick >= config.max_comments_per_tick {
                    True ->
                      process_loop(
                        state,
                        config,
                        rest,
                        issue_sessions,
                        processed_this_tick,
                        [
                          LogIgnored("deferred_over_limit", comment.id),
                          ..actions
                        ],
                      )
                    False ->
                      process_command_like_comment(
                        state,
                        config,
                        comment,
                        issue_sessions,
                        rest,
                        processed_this_tick,
                        actions,
                      )
                  }
              }
          }
      }
  }
}

fn durable_receipt_handling(
  state: TransportState,
  config: config_types.LinearCommandConfig,
  comment: linear.LinearComment,
) -> ReceiptHandling {
  let TransportState(command_receipts: receipts, ..) = state
  case dict.get(receipts, comment.id) {
    Ok(projection.CommandReceiptAcked(_, _)) -> ReceiptSkip
    Ok(projection.CommandReceiptCompleted(_, _, _, _, _, _, _, _, _, Some(_))) ->
      ReceiptSkip
    Ok(projection.CommandReceiptCompleted(
      issue_id,
      _,
      command_name,
      _,
      result_status,
      message_excerpt,
      _,
      _,
      _,
      None,
    )) ->
      case should_ack_receipt_status(config, result_status) {
        True ->
          ReceiptAck(
            mark_processed(state, comment.id),
            PostAck(
              issue_id,
              comment.id,
              completed_receipt_ack_body(
                comment.id,
                command_name,
                result_status,
                message_excerpt,
              ),
            ),
          )
        False ->
          ReceiptAck(
            mark_processed(state, comment.id),
            LogIgnored("ack_disabled", comment.id),
          )
      }
    Ok(projection.CommandReceiptStarted(issue_id, _, command_name, _, _, _)) ->
      case config.acknowledge_rejection {
        True ->
          ReceiptAck(
            mark_processed(state, comment.id),
            PostAck(
              issue_id,
              comment.id,
              unknown_after_restart_ack_body(comment.id, command_name),
            ),
          )
        False ->
          ReceiptAck(
            mark_processed(state, comment.id),
            LogIgnored("ack_disabled", comment.id),
          )
      }
    Ok(projection.CommandReceiptSeen(_, _, _, _, _))
    | Ok(projection.CommandReceiptUnseen)
    | Error(_) -> ReceiptProcessNormally
  }
}

fn should_ack_receipt_status(
  config: config_types.LinearCommandConfig,
  status: String,
) -> Bool {
  case status {
    "applied" | "queued" -> config.acknowledge_success
    _ -> config.acknowledge_rejection
  }
}

fn process_command_like_comment(
  state: TransportState,
  config: config_types.LinearCommandConfig,
  comment: linear.LinearComment,
  issue_sessions: Dict(String, String),
  rest: List(linear.LinearComment),
  processed_this_tick: Int,
  actions: List(TransportAction),
) -> #(TransportState, List(TransportAction)) {
  case list.contains(config.authorized_user_ids, comment.author.id) {
    False -> {
      let state = mark_processed(state, comment.id)
      let actions = [LogIgnored("unauthorized", comment.id), ..actions]
      let actions =
        maybe_rejection_ack(
          config,
          comment.issue_id,
          comment.id,
          unauthorized_ack_body(comment.id, comment.author.id),
          actions,
        )
      process_loop(
        state,
        config,
        rest,
        issue_sessions,
        processed_this_tick + 1,
        actions,
      )
    }
    True -> {
      let current_session_id =
        dict.get(issue_sessions, comment.issue_id) |> option.from_result
      case
        linear_parser.parse_comment(
          config.prefix,
          comment.issue_id,
          comment.issue_id,
          current_session_id,
          comment.id,
          comment.body,
        )
      {
        Ok(Some(parsed)) -> {
          let state = mark_processed(state, comment.id)
          process_loop(
            state,
            config,
            rest,
            issue_sessions,
            processed_this_tick + 1,
            [SubmitCommand(comment, parsed), ..actions],
          )
        }
        Ok(None) ->
          process_loop(
            state,
            config,
            rest,
            issue_sessions,
            processed_this_tick,
            actions,
          )
        Error(err) -> {
          let state = mark_processed(state, comment.id)
          let actions = [
            LogIgnored(parse_error_reason(err), comment.id),
            ..actions
          ]
          let actions =
            maybe_rejection_ack(
              config,
              comment.issue_id,
              comment.id,
              parse_error_ack_body(comment.id, err),
              actions,
            )
          process_loop(
            state,
            config,
            rest,
            issue_sessions,
            processed_this_tick + 1,
            actions,
          )
        }
      }
    }
  }
}

fn maybe_rejection_ack(
  config: config_types.LinearCommandConfig,
  issue_id: String,
  source_comment_id: String,
  body: String,
  actions: List(TransportAction),
) -> List(TransportAction) {
  case config.acknowledge_rejection {
    True -> [PostAck(issue_id, source_comment_id, body), ..actions]
    False -> actions
  }
}

fn unauthorized_ack_body(comment_id: String, author_id: String) -> String {
  let body =
    [
      comment_format.title("🔒", "Scherzo command not allowed"),
      ack_table("unknown", "not_allowed", None, comment_id),
      comment_format.section(
        "Summary",
        "Scherzo ignored this command because the Linear user is not authorized.",
      ),
      comment_format.section(
        "Run details",
        "- Linear user id: " <> comment_format.code_span(author_id, "unknown"),
      ),
    ]
    |> join_blocks
  comment_format.finalize_body("linear_command_ack", body, [])
}

fn parse_error_ack_body(
  comment_id: String,
  err: linear_parser.ParseError,
) -> String {
  case err {
    linear_parser.NoCurrentSession(name) ->
      common_ack_body(
        comment_id,
        bounded_no_secrets(name, 160),
        "not_found",
        None,
        None,
        None,
        [],
      )
    _ ->
      common_ack_body(
        comment_id,
        parse_error_command_name(err),
        "rejected",
        None,
        Some(parse_error_message(err)),
        None,
        [],
      )
  }
}

pub fn completed_receipt_ack_body(
  comment_id: String,
  command_name: String,
  status: String,
  message_excerpt: String,
) -> String {
  common_ack_body(
    comment_id,
    command_name,
    status,
    None,
    optional_non_empty(redacted_truncated(message_excerpt, [], 160)),
    None,
    [],
  )
}

pub fn unknown_after_restart_ack_body(
  comment_id: String,
  command_name: String,
) -> String {
  common_ack_body(
    comment_id,
    command_name,
    "unknown_after_restart",
    None,
    None,
    None,
    [],
  )
}

pub fn result_ack_body(
  source_comment_id: String,
  parsed: linear_parser.ParsedLinearCommand,
  result: command.CommandResult,
  secrets: List(String),
) -> String {
  let message = case result.message {
    Some(message) -> Some(redacted_truncated(message, secrets, 160))
    None -> None
  }
  let excerpt = case string.trim(parsed.excerpt) == "" {
    True -> None
    False -> Some(redacted_truncated(parsed.excerpt, secrets, 80))
  }
  common_ack_body(
    source_comment_id,
    result.command,
    command.status_to_string(result.status),
    result.target,
    message,
    excerpt,
    secrets,
  )
}

pub fn should_ack_result(
  config: config_types.LinearCommandConfig,
  result: command.CommandResult,
) -> Bool {
  case result.status {
    command.Applied | command.Queued -> config.acknowledge_success
    command.Rejected(_) | command.NotFound | command.NotAllowed(_) ->
      config.acknowledge_rejection
  }
}

fn common_ack_body(
  comment_id: String,
  command_name: String,
  status: String,
  target: Option(String),
  message: Option(String),
  excerpt: Option(String),
  secrets: List(String),
) -> String {
  let command_name = redacted_truncated(command_name, secrets, 160)
  let status = redacted_truncated(status, secrets, 80)
  let target = case target {
    Some(target) -> Some(redacted_truncated(target, secrets, 160))
    None -> None
  }
  let blocks = [
    status_title(status),
    ack_table(command_name, status, target, comment_id),
    comment_format.section("Summary", summary_text(status, message)),
  ]
  let blocks = case next_action_text(status) {
    None -> blocks
    Some(next_action) ->
      list.append(blocks, [comment_format.section("Next action", next_action)])
  }
  let blocks = case excerpt {
    None -> blocks
    Some(excerpt) ->
      list.append(blocks, [
        comment_format.section(
          "Command excerpt",
          comment_format.code_span(excerpt, "command excerpt"),
        ),
      ])
  }
  let body = blocks |> join_blocks
  comment_format.finalize_body("linear_command_ack", body, secrets)
}

fn ack_table(
  command_name: String,
  status: String,
  target: Option(String),
  comment_id: String,
) -> String {
  let rows = [
    comment_format.SummaryRow(
      "Command",
      comment_format.table_code(command_name, "unknown"),
    ),
    comment_format.SummaryRow(
      "Status",
      comment_format.table_code(status, "unknown"),
    ),
  ]
  let rows = list.append(rows, comment_format.optional_row("Target", target))
  let rows =
    list.append(rows, [
      comment_format.SummaryRow(
        "Source comment",
        comment_format.table_code(comment_id, "unknown"),
      ),
    ])
  comment_format.summary_table(rows)
}

fn status_title(status: String) -> String {
  case status {
    "applied" -> comment_format.title("✅", "Scherzo command applied")
    "queued" -> comment_format.title("⏳", "Scherzo command queued")
    "rejected" -> comment_format.title("🚫", "Scherzo command rejected")
    "not_found" -> comment_format.title("🔎", "Scherzo command target not found")
    "not_allowed" -> comment_format.title("🔒", "Scherzo command not allowed")
    "unknown_after_restart" ->
      comment_format.title("❓", "Scherzo command status is unknown")
    _ -> comment_format.title("ℹ️", "Scherzo command result recorded")
  }
}

fn summary_text(status: String, message: Option(String)) -> String {
  case message {
    Some(message) -> message
    None ->
      case status {
        "applied" -> "Scherzo applied the command."
        "queued" -> "Scherzo queued the command."
        "rejected" -> "Scherzo rejected the command."
        "not_found" -> "No current Scherzo session is running for this issue."
        "not_allowed" ->
          "Scherzo ignored this command because it is not allowed."
        "unknown_after_restart" ->
          "Scherzo restarted while this command was in progress, so it cannot safely confirm the outcome from memory."
        _ -> "Scherzo recorded the command result."
      }
  }
}

fn next_action_text(status: String) -> Option(String) {
  case status {
    "rejected" ->
      Some("Edit or post one `/scherzo` command with the required arguments.")
    "not_found" ->
      Some("Start or retry a run before sending this session command.")
    "unknown_after_restart" ->
      Some(
        "Inspect the current issue or session state. Post a new command only if action is still needed.",
      )
    _ -> None
  }
}

fn parse_error_command_name(err: linear_parser.ParseError) -> String {
  case err {
    linear_parser.UnknownCommand(name) -> bounded_no_secrets(name, 160)
    linear_parser.MissingArgument(name) -> bounded_no_secrets(name, 160)
    linear_parser.InvalidArgument(_) -> "malformed"
    linear_parser.MultipleCommands -> "multiple"
    linear_parser.NoCurrentSession(name) -> bounded_no_secrets(name, 160)
  }
}

fn parse_error_message(err: linear_parser.ParseError) -> String {
  case err {
    linear_parser.UnknownCommand(name) ->
      bounded_no_secrets("Unknown command: " <> name, 160)
    linear_parser.MissingArgument(name) ->
      bounded_no_secrets("Missing argument: " <> name, 160)
    linear_parser.InvalidArgument(value) ->
      bounded_no_secrets("Invalid argument: " <> value, 160)
    linear_parser.MultipleCommands ->
      "Only one Scherzo command is allowed per Linear comment."
    linear_parser.NoCurrentSession(_) ->
      "No current Scherzo session is running for this issue."
  }
}

fn join_blocks(blocks: List(String)) -> String {
  blocks |> string.join(with: "\n\n")
}

fn bounded_no_secrets(value: String, max: Int) -> String {
  redacted_truncated(value, [], max)
}

fn parse_error_reason(err: linear_parser.ParseError) -> String {
  case err {
    linear_parser.UnknownCommand(_) -> "unknown_command"
    linear_parser.MissingArgument(_) -> "missing_argument"
    linear_parser.InvalidArgument(_) -> "invalid_argument"
    linear_parser.MultipleCommands -> "multiple_commands"
    linear_parser.NoCurrentSession(_) -> "missing_session"
  }
}

fn redacted_truncated(
  value: String,
  secrets: List(String),
  max: Int,
) -> String {
  log.redact("linear_command", value, secrets) |> log.truncate(max)
}

fn optional_non_empty(value: String) -> Option(String) {
  case string.trim(value) == "" {
    True -> None
    False -> Some(value)
  }
}
