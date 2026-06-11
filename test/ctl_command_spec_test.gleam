import gleam/option.{Some}
import scherzo/ctl/command_spec

pub fn parse_supports_nested_paths_and_options_before_or_after_words_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "task_show",
      path: ["task", "show"],
      usage: "task show <task>",
      summary: "",
      positionals: [command_spec.Required("task")],
      options: [
        command_spec.flag_option("--json", ""),
        command_spec.value_option(
          "--color",
          "<mode>",
          "",
          False,
          color_validator,
        ),
      ],
      help_lines: [],
    ),
  ]

  let assert Ok(command_spec.Parsed(parsed)) =
    command_spec.parse(["task", "show", "LIV-1", "--json"], specs)
  assert parsed.handler == "task_show"
  assert parsed.positionals == ["LIV-1"]
  assert command_spec.has_flag(parsed, "--json")

  let assert Ok(command_spec.Parsed(parsed)) =
    command_spec.parse(["task", "--color=never", "show", "LIV-1"], specs)
  assert command_spec.option_value(parsed, "--color") == Some("never")
}

pub fn parse_supports_optional_and_rest_positionals_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "status",
      path: ["schedules", "status"],
      usage: "schedules status [job]",
      summary: "",
      positionals: [command_spec.Optional("job")],
      options: [],
      help_lines: [],
    ),
    command_spec.CommandSpec(
      handler: "decision",
      path: ["workstream", "decision"],
      usage: "workstream decision <kind> <args>...",
      summary: "",
      positionals: [
        command_spec.Required("kind"),
        command_spec.Rest("inputs"),
      ],
      options: [],
      help_lines: [],
    ),
  ]

  let assert Ok(command_spec.Parsed(status)) =
    command_spec.parse(["schedules", "status"], specs)
  assert status.positionals == []

  let assert Ok(command_spec.Parsed(decision)) =
    command_spec.parse(
      ["workstream", "decision", "approve", "input-1", "input-2"],
      specs,
    )
  assert decision.positionals == ["approve", "input-1", "input-2"]
}

pub fn parse_supports_repeatable_values_and_validation_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "task_list",
      path: ["task", "list"],
      usage: "task list",
      summary: "",
      positionals: [],
      options: [
        command_spec.value_option(
          "--state",
          "<state>",
          "",
          True,
          state_validator,
        ),
        command_spec.value_option(
          "--limit",
          "<n>",
          "",
          False,
          positive_int_validator,
        ),
      ],
      help_lines: [],
    ),
  ]

  let assert Ok(command_spec.Parsed(parsed)) =
    command_spec.parse(
      ["task", "list", "--state", "ready", "--state", "active", "--limit", "50"],
      specs,
    )
  assert command_spec.option_values(parsed, "--state") == ["ready", "active"]
  assert command_spec.option_value(parsed, "--limit") == Some("50")
}

pub fn parse_rejects_duplicate_single_value_options_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "task_list",
      path: ["task", "list"],
      usage: "task list",
      summary: "",
      positionals: [],
      options: [
        command_spec.value_option(
          "--state",
          "<state>",
          "",
          True,
          state_validator,
        ),
        command_spec.value_option(
          "--limit",
          "<n>",
          "",
          False,
          positive_int_validator,
        ),
      ],
      help_lines: [],
    ),
  ]

  let assert Error(error) =
    command_spec.parse(
      ["task", "list", "--limit", "50", "--limit", "50"],
      specs,
    )
  assert command_spec.error_message(error)
    == "option may only be supplied once: --limit"

  let assert Ok(command_spec.Parsed(parsed)) =
    command_spec.parse(
      ["task", "list", "--state", "ready", "--state", "active"],
      specs,
    )
  assert command_spec.option_values(parsed, "--state") == ["ready", "active"]
}

pub fn parse_rejects_unknown_options_and_missing_values_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "attach",
      path: ["attach"],
      usage: "attach <session>",
      summary: "",
      positionals: [command_spec.Required("session")],
      options: [
        command_spec.flag_option("--json", ""),
        command_spec.value_option(
          "--color",
          "auto|always|never",
          "",
          False,
          color_validator,
        ),
      ],
      help_lines: [],
    ),
  ]

  let assert Error(error) =
    command_spec.parse(["attach", "ABC-1", "--bogus"], specs)
  assert command_spec.error_message(error) == "unknown option: --bogus"

  let assert Error(error) =
    command_spec.parse(["attach", "ABC-1", "--color"], specs)
  assert command_spec.error_message(error)
    == "--color requires auto|always|never"
}

pub fn parse_rejects_invalid_values_test() {
  let specs = [
    command_spec.CommandSpec(
      handler: "events",
      path: ["events"],
      usage: "events <session>",
      summary: "",
      positionals: [command_spec.Required("session")],
      options: [
        command_spec.value_option(
          "--since-cursor",
          "<n>",
          "",
          False,
          non_negative_int_validator,
        ),
      ],
      help_lines: [],
    ),
  ]

  let assert Error(error) =
    command_spec.parse(["events", "ABC-1", "--since-cursor", "-1"], specs)
  assert command_spec.error_message(error)
    == "--since-cursor requires a non-negative integer"
}

pub fn render_help_lines_aligns_command_and_option_tables_test() {
  let rendered =
    command_spec.render_help_lines([
      command_spec.HelpLine("ping", "Check daemon reachability."),
      command_spec.HelpLine(
        "attach --json <session-ref>",
        "Follow JSON events.",
      ),
      command_spec.HelpLine("", "Continuation line."),
    ])

  assert rendered
    == [
      "  ping                        Check daemon reachability.",
      "  attach --json <session-ref> Follow JSON events.",
      "                              Continuation line.",
    ]
}

fn color_validator(value: String) -> Result(String, String) {
  case value {
    "auto" | "always" | "never" -> Ok(value)
    _ -> Error("--color must be auto, always, or never")
  }
}

fn state_validator(value: String) -> Result(String, String) {
  case value {
    "ready" | "active" -> Ok(value)
    _ -> Error("--state must be ready or active")
  }
}

fn positive_int_validator(value: String) -> Result(String, String) {
  case value {
    "50" -> Ok(value)
    _ -> Error("--limit requires a positive integer")
  }
}

fn non_negative_int_validator(value: String) -> Result(String, String) {
  case value {
    "0" | "1" | "40" -> Ok(value)
    _ -> Error("--since-cursor requires a non-negative integer")
  }
}
