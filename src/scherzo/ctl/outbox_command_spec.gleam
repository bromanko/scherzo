import gleam/int
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

pub fn cleanup_command(
  handler: handler,
  control_file_option: command_spec.OptionSpec,
  root_option: command_spec.OptionSpec,
  json_option: command_spec.OptionSpec,
  dry_run_option: command_spec.OptionSpec,
  yes_option: command_spec.OptionSpec,
  limit_option: command_spec.OptionSpec,
  cursor_option: command_spec.OptionSpec,
) -> command_spec.CommandSpec(handler) {
  command_spec.CommandSpec(
    handler: handler,
    path: ["cleanup"],
    usage: "cleanup [--yes] [--limit <n>] [--cursor <cursor>] [--max-runtime-ms <ms>]",
    summary: "Dry-run owned cleanup inventory.",
    positionals: [],
    options: [
      control_file_option,
      root_option,
      json_option,
      dry_run_option,
      yes_option,
      limit_option,
      cursor_option,
      cleanup_max_runtime_option(),
    ],
    help_lines: [
      line("cleanup", "Dry-run owned cleanup inventory."),
      line("cleanup --yes", "Apply eligible owned cleanup after safety checks."),
      line(
        "cleanup --limit 100 --max-runtime-ms 240000",
        "Request a bounded cleanup page and report resume metadata.",
      ),
    ],
  )
}

fn cleanup_max_runtime_option() -> command_spec.OptionSpec {
  command_spec.value_option(
    "--max-runtime-ms",
    "<ms>",
    "Maximum cleanup runtime budget in milliseconds.",
    False,
    fn(value) {
      case int.parse(value) {
        Ok(limit) if limit > 0 -> Ok(value)
        Ok(_) | Error(_) ->
          Error("--max-runtime-ms requires a positive integer")
      }
    },
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
