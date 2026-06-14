import gleam/string
import scherzo/control/query/types as query_types
import scherzo/ctl/command_spec

pub fn command(
  handler: handler,
  control_file_option: command_spec.OptionSpec,
  json_option: command_spec.OptionSpec,
  limit_option: command_spec.OptionSpec,
  cursor_option: command_spec.OptionSpec,
) -> command_spec.CommandSpec(handler) {
  command_spec.CommandSpec(
    handler: handler,
    path: ["outbox"],
    usage: "outbox [filters] | outbox <outbox-id>",
    summary: "Inspect durable outbox records through the daemon query surface.",
    positionals: [command_spec.Optional("outbox_id")],
    options: [
      control_file_option,
      json_option,
      status_option(),
      kind_option(),
      limit_option,
      cursor_option,
    ],
    help_lines: [
      line("outbox", "List durable outbox records without payload bodies."),
      line("outbox <outbox-id>", "Inspect one outbox record safely."),
      line(
        "outbox --status retryable",
        "Filter list output by status: pending, in_flight, retryable, completed, failed, permanent.",
      ),
    ],
  )
}

pub fn status_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--status",
    "<status>",
    "Filter outbox by status; may be repeated: pending, in_flight, retryable, completed, failed, permanent.",
    True,
    fn(value) {
      case query_types.outbox_status_from_string(value) {
        Ok(_) -> Ok(value)
        Error(_) ->
          Error(
            "--status must be pending, in_flight, retryable, completed, failed, or permanent",
          )
      }
    },
  )
}

pub fn kind_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--kind",
    "<kind>",
    "Filter outbox by outbox kind; may be repeated.",
    True,
    fn(value) {
      case string.trim(value) == "" {
        True -> Error("--kind must not be empty")
        False -> Ok(value)
      }
    },
  )
}

fn line(left: String, right: String) -> command_spec.HelpLine {
  command_spec.HelpLine(left, right)
}
