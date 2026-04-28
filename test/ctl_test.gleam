import gleam/option.{None, Some}
import gleam/string
import scherzo/ctl

pub fn parse_ping_ps_session_events_and_attach_test() {
  assert ctl.parse(["ping"]) == Ok(ctl.Ping(None, False))
  assert ctl.parse(["ps", "--json"]) == Ok(ctl.Ps(None, True))
  assert ctl.parse(["session", "ABC-1", "--control-file", "state/control.json"])
    == Ok(ctl.Session(Some("state/control.json"), False, "ABC-1"))
  assert ctl.parse(["events", "ABC-1", "--json"])
    == Ok(ctl.Events(None, True, "ABC-1"))
  assert ctl.parse(["attach", "--raw", "ABC-1"])
    == Ok(ctl.AttachRaw(None, False, "ABC-1"))
  assert ctl.parse(["attach", "--raw", "ABC-1", "--json"])
    == Ok(ctl.AttachRaw(None, True, "ABC-1"))
}

pub fn parse_rejects_usage_errors_test() {
  let assert Error(ctl.UsageError(_)) = ctl.parse(["attach", "ABC-1"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["ps", "--control-file"])
  let assert Error(ctl.UsageError(_)) = ctl.parse(["unknown"])
}

pub fn usage_mentions_commands_and_options_test() {
  let usage = ctl.usage()
  assert string.contains(usage, "ping")
  assert string.contains(usage, "ps")
  assert string.contains(usage, "session <session-id>")
  assert string.contains(usage, "events <session-id>")
  assert string.contains(usage, "attach --raw <session-id>")
  assert string.contains(usage, "--control-file <path>")
  assert string.contains(usage, "--json")
}
