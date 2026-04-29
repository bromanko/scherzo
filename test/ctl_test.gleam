import gleam/option.{None, Some}
import gleam/string
import scherzo/control/command
import scherzo/ctl
import scherzo/terminal/style

pub fn parse_ping_ps_session_events_and_attach_test() {
  assert ctl.parse(["ping"]) == Ok(ctl.Ping(None, False))
  assert ctl.parse(["ps", "--json"]) == Ok(ctl.Ps(None, True))
  assert ctl.parse(["session", "ABC-1", "--control-file", "state/control.json"])
    == Ok(ctl.Session(Some("state/control.json"), False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Raw, style.ColorNever, 0, "ABC-1"))
  assert ctl.parse(["events", "ABC-1", "--json"])
    == Ok(ctl.Events(None, ctl.Json, style.ColorNever, 0, "ABC-1"))
  assert ctl.parse(["events", "--pretty", "ABC-1"])
    == Ok(ctl.Events(None, ctl.Pretty, style.ColorAuto, 0, "ABC-1"))
  assert ctl.parse(["attach", "ABC-1"])
    == Ok(ctl.Attach(None, ctl.Pretty, style.ColorAuto, ctl.Follow, 0, "ABC-1"))
  assert ctl.parse(["attach", "--raw", "ABC-1"])
    == Ok(ctl.Attach(None, ctl.Raw, style.ColorNever, ctl.Follow, 0, "ABC-1"))
  assert ctl.parse(["attach", "--json", "ABC-1"])
    == Ok(ctl.Attach(None, ctl.Json, style.ColorNever, ctl.Follow, 0, "ABC-1"))
  assert ctl.parse(["attach", "--raw", "ABC-1", "--json"])
    == Ok(ctl.Attach(None, ctl.Json, style.ColorNever, ctl.Follow, 0, "ABC-1"))
  assert ctl.parse(["attach", "--no-follow", "ABC-1"])
    == Ok(ctl.Attach(
      None,
      ctl.Pretty,
      style.ColorAuto,
      ctl.NoFollow,
      0,
      "ABC-1",
    ))
  assert ctl.parse(["attach", "--since-cursor", "40", "ABC-1"])
    == Ok(ctl.Attach(None, ctl.Pretty, style.ColorAuto, ctl.Follow, 40, "ABC-1"))
  assert ctl.parse(["attach", "--color=never", "ABC-1"])
    == Ok(ctl.Attach(None, ctl.Pretty, style.ColorNever, ctl.Follow, 0, "ABC-1"))
}

pub fn parse_operator_commands_test() {
  assert ctl.parse(["pause"])
    == Ok(ctl.Operator(None, False, command.PauseDispatch))
  assert ctl.parse(["resume"])
    == Ok(ctl.Operator(None, False, command.ResumeDispatch))
  assert ctl.parse(["reload"])
    == Ok(ctl.Operator(None, False, command.ReloadWorkflow))
  assert ctl.parse(["retry", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RetryIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["park", "ABC-123", "--reason", "manual", "--yes"])
    == Ok(ctl.Operator(
      None,
      False,
      command.ParkIssue(command.IssueIdentifier("ABC-123"), "manual"),
    ))
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["park", "ABC-123", "--reason", "manual"])
  assert ctl.parse(["unpark", "ABC-123"])
    == Ok(ctl.Operator(
      None,
      False,
      command.UnparkIssue(command.IssueIdentifier("ABC-123")),
    ))
  assert ctl.parse(["abort", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.AbortSession("session-1")))
  let assert Error(ctl.UsageError(_)) = ctl.parse(["abort", "session-1"])
  assert ctl.parse(["stop-after-turn", "session-1", "--yes"])
    == Ok(ctl.Operator(None, False, command.StopAfterCurrentTurn("session-1")))
  assert ctl.parse(["prompt", "session-1", "continue"])
    == Ok(ctl.Operator(
      None,
      False,
      command.PromptSession("session-1", "continue"),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--cancel"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiCancel),
    ))
  assert ctl.parse(["ui", "respond", "session-1", "ui-1", "--value", "ok"])
    == Ok(ctl.Operator(
      None,
      False,
      command.RespondUi("session-1", "ui-1", command.UiValue("ok")),
    ))
}

pub fn parse_rejects_usage_errors_test() {
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--raw", "--pretty", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "-1", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--since-cursor", "wat", "ABC-1"])
  let assert Error(ctl.UsageError(_)) =
    ctl.parse(["attach", "--color=bad", "ABC-1"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["ps", "--control-file"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["unknown"])
}

pub fn usage_mentions_commands_and_options_test() {
  let usage = ctl.usage()
  assert string.contains(usage, "ping")
  assert string.contains(usage, "ps")
  assert string.contains(usage, "session <session-id>")
  assert string.contains(usage, "events <session-id>")
  assert string.contains(usage, "events --pretty <session-id>")
  assert string.contains(usage, "attach <session-id>")
  assert string.contains(usage, "attach --raw <session-id>")
  assert string.contains(usage, "attach --raw --json <session-id>")
  assert string.contains(usage, "pause")
  assert string.contains(usage, "abort <session-id> --yes")
  assert string.contains(usage, "ui respond")
  assert string.contains(usage, "--control-file <path>")
  assert string.contains(usage, "--json")
  assert string.contains(usage, "--since-cursor <n>")
}
