import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/control/linear_parser

const prefix = "/scherzo"

const issue_id = "issue-1"

const issue_identifier = "ABC-1"

const comment_id = "comment-1"

const session_id = "ABC-1-42-1"

fn parse(body: String) {
  linear_parser.parse_comment(
    prefix,
    issue_id,
    issue_identifier,
    Some(session_id),
    comment_id,
    body,
  )
}

fn parse_without_session(body: String) {
  linear_parser.parse_comment(
    prefix,
    issue_id,
    issue_identifier,
    None,
    comment_id,
    body,
  )
}

pub fn parses_issue_targeted_commands_test() {
  let assert Ok(Some(retry)) = parse("/scherzo retry")
  assert retry.source_issue_id == issue_id
  assert retry.source_comment_id == comment_id
  assert retry.command == command.RetryIssue(command.IssueId(issue_id))

  let assert Ok(Some(retry_step)) = parse("/scherzo retry-step --step build")
  assert retry_step.command
    == command.RetryWorkflowStep(
      command.RetryWorkflowStepIssueRef(command.IssueId(issue_id)),
      Some("build"),
    )
  assert retry_step.excerpt == "build"

  let assert Ok(Some(park)) = parse("  /scherzo park --reason waiting")
  assert park.command == command.ParkIssue(command.IssueId(issue_id), "waiting")
  assert park.excerpt == "waiting"

  let assert Ok(Some(unpark)) = parse("/scherzo unpark")
  assert unpark.command == command.UnparkIssue(command.IssueId(issue_id))
}

pub fn parses_session_targeted_commands_test() {
  let assert Ok(Some(abort)) = parse("/scherzo abort")
  assert abort.command == command.AbortSession(session_id)

  let assert Ok(Some(stop)) = parse("/scherzo stop-after-turn")
  assert stop.command == command.StopAfterCurrentTurn(session_id)

  let assert Ok(Some(prompt)) = parse("/scherzo prompt please continue")
  assert prompt.command == command.PromptSession(session_id, "please continue")
  assert prompt.excerpt == "please continue"

  let assert Ok(Some(cancel)) = parse("/scherzo ui respond ui-1 --cancel")
  assert cancel.command
    == command.RespondUi(session_id, "ui-1", command.UiCancel)

  let assert Ok(Some(value)) =
    parse("/scherzo ui respond ui-1 --value approved")
  assert value.command
    == command.RespondUi(session_id, "ui-1", command.UiValue("approved"))
  assert value.excerpt == "approved"
}

pub fn ignores_non_commands_and_enforces_prefix_boundary_test() {
  assert parse("ordinary discussion about /scherzo retry") == Ok(None)
  assert parse("/not-scherzo retry") == Ok(None)
  assert parse("/scherzoed retry") == Ok(None)
  assert parse(
      "```
/scherzo retry
```",
    )
    == Ok(None)
  let assert Ok(Some(_)) =
    parse(
      "paragraph

/scherzo retry",
    )
}

pub fn rejects_malformed_explicit_commands_test() {
  let assert Error(linear_parser.UnknownCommand("dance")) =
    parse("/scherzo dance")
  let assert Error(linear_parser.UnknownCommand("stop")) =
    parse("/scherzo stop")
  let assert Error(linear_parser.UnknownCommand("continue")) =
    parse("/scherzo continue")
  let assert Error(linear_parser.MissingArgument("prompt")) =
    parse("/scherzo prompt")
  let assert Error(linear_parser.MissingArgument("step")) =
    parse("/scherzo retry-step --step")
  let assert Error(linear_parser.MissingArgument("reason")) =
    parse("/scherzo park --reason")
  let assert Error(linear_parser.MissingArgument("value")) =
    parse("/scherzo ui respond ui-1 --value")
  let assert Error(linear_parser.MissingArgument("--cancel or --value")) =
    parse("/scherzo ui respond ui-1")
  let assert Error(linear_parser.InvalidArgument("--bad")) =
    parse("/scherzo ui respond ui-1 --bad")
}

pub fn session_commands_without_current_session_are_errors_test() {
  let assert Error(linear_parser.NoCurrentSession("abort")) =
    parse_without_session("/scherzo abort")
  let assert Error(linear_parser.NoCurrentSession("stop-after-turn")) =
    parse_without_session("/scherzo stop-after-turn")
  let assert Error(linear_parser.NoCurrentSession("prompt")) =
    parse_without_session("/scherzo prompt continue")
  let assert Error(linear_parser.NoCurrentSession("ui respond")) =
    parse_without_session("/scherzo ui respond ui-1 --cancel")
}

pub fn rejects_multiple_explicit_commands_in_one_comment_test() {
  let assert Error(linear_parser.MultipleCommands) =
    parse(
      "/scherzo retry
Some text
/scherzo abort",
    )
}
