import gleam/list
import gleam/string
import scherzo/ctl/command_registry

pub fn text() -> String {
  let lines =
    [
      "Usage: scherzo ctl <command> [options]",
      "       scherzoctl <command> [options]",
      "",
      "Local Scherzo daemon inspection and operator controls. Commands:",
    ]
    |> list.append(command_registry.usage_lines())
    |> list.append(["", "Options:"])
    |> list.append(command_registry.option_usage_lines())

  string.join(lines, with: "\n")
}
