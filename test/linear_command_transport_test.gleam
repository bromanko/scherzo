import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/linear_transport
import scherzo/linear
import scherzo/state/projection

fn config() -> config_types.LinearCommandConfig {
  config_types.LinearCommandConfig(
    enabled: True,
    prefix: "/scherzo",
    authorized_user_ids: ["user-1"],
    poll_limit_per_issue: 25,
    max_comments_per_tick: 50,
    acknowledge_success: True,
    acknowledge_rejection: True,
  )
}

fn config_with_cap(max_comments: Int) -> config_types.LinearCommandConfig {
  config_types.LinearCommandConfig(
    ..config(),
    max_comments_per_tick: max_comments,
  )
}

fn author(id: String, email: Option(String)) -> linear.LinearCommentAuthor {
  linear.LinearCommentAuthor(id: id, email: email, name: Some("User " <> id))
}

fn comment(
  id: String,
  body: String,
  user_id: String,
  email: Option(String),
  created_at_ms: Int,
) -> linear.LinearComment {
  linear.LinearComment(
    id: id,
    issue_id: "issue-1",
    body: body,
    created_at_ms: created_at_ms,
    updated_at_ms: created_at_ms,
    author: author(user_id, email),
  )
}

fn sessions() {
  dict.from_list([#("issue-1", "session-1")])
}

pub fn authorized_new_comment_submits_command_and_marks_processed_test() {
  let state = linear_transport.new_state(1000)
  let comments = [
    comment("c1", "/scherzo prompt continue", "user-1", None, 1000),
  ]
  let #(next, actions) =
    linear_transport.process_comments(state, config(), comments, sessions())
  assert linear_transport.has_processed(next, "c1")
  let assert [linear_transport.SubmitCommand(_, parsed)] = actions
  assert parsed.command == command.PromptSession("session-1", "continue")

  let ack =
    linear_transport.result_ack_body(
      "c1",
      parsed,
      command.queued(parsed.command, Some("queued for next turn")),
      [],
    )
  assert string.contains(ack, "comment c1")
  assert string.contains(ack, "Command: prompt")
  assert string.contains(ack, "Status: queued")
  assert string.contains(ack, "Target: session-1")
}

pub fn unauthorized_comment_is_rejected_without_command_test() {
  let state = linear_transport.new_state(1000)
  let unauthorized =
    comment(
      "c1",
      "/scherzo retry",
      "user-2",
      Some("operator@example.com"),
      1000,
    )
  let #(next, actions) =
    linear_transport.process_comments(
      state,
      config(),
      [unauthorized],
      sessions(),
    )
  assert linear_transport.has_processed(next, "c1")
  assert submit_count(actions) == 0
  assert ack_count(actions) == 1
  let assert Some(body) = first_ack(actions)
  assert string.contains(body, "Status: not_allowed")

  let #(again, again_actions) =
    linear_transport.process_comments(
      next,
      config(),
      [unauthorized],
      sessions(),
    )
  assert linear_transport.has_processed(again, "c1")
  assert again_actions == []
}

pub fn processed_comment_id_is_not_executed_twice_test() {
  let state = linear_transport.new_state(1000)
  let c1 = comment("c1", "/scherzo retry", "user-1", None, 1000)
  let #(next, first_actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  let #(_, second_actions) =
    linear_transport.process_comments(next, config(), [c1], sessions())
  assert submit_count(first_actions) == 1
  assert second_actions == []
}

pub fn edited_processed_comment_is_ignored_test() {
  let state = linear_transport.new_state(1000)
  let c1 = comment("c1", "/scherzo retry", "user-1", None, 1000)
  let #(next, first_actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  let edited =
    linear.LinearComment(..c1, body: "/scherzo abort", updated_at_ms: 2000)
  let #(_, edited_actions) =
    linear_transport.process_comments(next, config(), [edited], sessions())
  let assert [linear_transport.SubmitCommand(_, parsed)] = first_actions
  assert parsed.command == command.RetryIssue(command.IssueId("issue-1"))
  assert edited_actions == []
}

pub fn unseen_old_comment_can_submit_test() {
  let state = linear_transport.new_state(1000)
  let c1 = comment("c1", "/scherzo retry", "user-1", None, 900)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  assert linear_transport.has_processed(next, "c1")
  assert submit_count(actions) == 1
}

pub fn acked_receipt_skips_comment_test() {
  let state =
    linear_transport.new_state_with_receipts(
      1000,
      dict.from_list([
        #(
          "c1",
          projection.CommandReceiptCompleted(
            issue_id: "issue-1",
            author_id: "user-1",
            command_name: "park",
            excerpt: "hold",
            result_status: "applied",
            message_excerpt: "issue parked",
            seen_at_ms: 100,
            started_at_ms: 110,
            completed_at_ms: 120,
            acked_at_ms: Some(130),
          ),
        ),
      ]),
    )
  let c1 = comment("c1", "/scherzo park --reason hold", "user-1", None, 900)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  assert !linear_transport.has_processed(next, "c1")
  assert actions == []
}

pub fn completed_unacked_receipt_posts_ack_without_submit_test() {
  let state =
    linear_transport.new_state_with_receipts(
      1000,
      dict.from_list([
        #(
          "c1",
          projection.CommandReceiptCompleted(
            issue_id: "issue-1",
            author_id: "user-1",
            command_name: "park",
            excerpt: "hold",
            result_status: "applied",
            message_excerpt: "issue parked",
            seen_at_ms: 100,
            started_at_ms: 110,
            completed_at_ms: 120,
            acked_at_ms: None,
          ),
        ),
      ]),
    )
  let c1 = comment("c1", "/scherzo park --reason hold", "user-1", None, 900)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  assert linear_transport.has_processed(next, "c1")
  assert submit_count(actions) == 0
  assert ack_count(actions) == 1
  let assert Some(body) = first_ack(actions)
  assert string.contains(body, "Command: park")
  assert string.contains(body, "Status: applied")
  assert string.contains(body, "Message: issue parked")
}

pub fn started_uncompleted_receipt_posts_unknown_ack_test() {
  let state =
    linear_transport.new_state_with_receipts(
      1000,
      dict.from_list([
        #(
          "c1",
          projection.CommandReceiptStarted(
            issue_id: "issue-1",
            author_id: "user-1",
            command_name: "park",
            excerpt: "hold",
            seen_at_ms: 100,
            started_at_ms: 110,
          ),
        ),
      ]),
    )
  let c1 = comment("c1", "/scherzo park --reason hold", "user-1", None, 900)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  assert linear_transport.has_processed(next, "c1")
  assert submit_count(actions) == 0
  assert ack_count(actions) == 1
  let assert Some(body) = first_ack(actions)
  assert string.contains(body, "Status: unknown_after_restart")
  assert string.contains(body, "Command: park")
}

pub fn malformed_command_is_acknowledged_once_test() {
  let state = linear_transport.new_state(1000)
  let c1 = comment("c1", "/scherzo ui respond ui-1", "user-1", None, 1000)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  assert submit_count(actions) == 0
  assert ack_count(actions) == 1
  assert linear_transport.has_processed(next, "c1")
  let #(_, again_actions) =
    linear_transport.process_comments(next, config(), [c1], sessions())
  assert again_actions == []
}

pub fn max_comments_per_tick_defers_overflow_without_marking_processed_test() {
  let state = linear_transport.new_state(1000)
  let comments = [
    comment("c1", "/scherzo retry", "user-1", None, 1000),
    comment("c2", "/scherzo unpark", "user-1", None, 1001),
    comment("c3", "/scherzo prompt later", "user-1", None, 1002),
  ]
  let #(next, actions) =
    linear_transport.process_comments(
      state,
      config_with_cap(2),
      comments,
      sessions(),
    )
  assert submit_count(actions) == 2
  assert linear_transport.has_processed(next, "c1")
  assert linear_transport.has_processed(next, "c2")
  assert !linear_transport.has_processed(next, "c3")
}

pub fn session_command_without_current_session_acks_not_found_test() {
  let state = linear_transport.new_state(1000)
  let c1 = comment("c1", "/scherzo abort", "user-1", None, 1000)
  let #(next, actions) =
    linear_transport.process_comments(state, config(), [c1], dict.new())
  assert linear_transport.has_processed(next, "c1")
  assert submit_count(actions) == 0
  let assert Some(body) = first_ack(actions)
  assert string.contains(body, "Status: not_found")
}

pub fn result_ack_redacts_and_truncates_text_test() {
  let state = linear_transport.new_state(1000)
  let c1 =
    comment(
      "c1",
      "/scherzo prompt secret-token-with-a-very-long-tail-that-should-not-appear-in-full",
      "user-1",
      None,
      1000,
    )
  let #(_, actions) =
    linear_transport.process_comments(state, config(), [c1], sessions())
  let assert [linear_transport.SubmitCommand(_, parsed)] = actions
  let ack =
    linear_transport.result_ack_body(
      "c1",
      parsed,
      command.queued(parsed.command, Some("secret-token queued")),
      ["secret-token"],
    )
  assert string.contains(ack, "[REDACTED]")
  assert !string.contains(ack, "secret-token")
}

fn submit_count(actions: List(linear_transport.TransportAction)) -> Int {
  case actions {
    [] -> 0
    [linear_transport.SubmitCommand(_, _), ..rest] -> 1 + submit_count(rest)
    [_, ..rest] -> submit_count(rest)
  }
}

fn ack_count(actions: List(linear_transport.TransportAction)) -> Int {
  case actions {
    [] -> 0
    [linear_transport.PostAck(_, _, _), ..rest] -> 1 + ack_count(rest)
    [_, ..rest] -> ack_count(rest)
  }
}

fn first_ack(
  actions: List(linear_transport.TransportAction),
) -> Option(String) {
  case actions {
    [] -> None
    [linear_transport.PostAck(_, _, body), ..] -> Some(body)
    [_, ..rest] -> first_ack(rest)
  }
}
