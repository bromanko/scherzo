import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import scherzo/ui/repl

// ---------------------------------------------------------------------------
// Command Parsing Tests
// ---------------------------------------------------------------------------

pub fn parse_empty_input_test() {
  repl.parse_command("")
  |> should.equal(None)
}

pub fn parse_whitespace_only_test() {
  repl.parse_command("   ")
  |> should.equal(None)
}

pub fn parse_simple_command_test() {
  repl.parse_command("help")
  |> should.equal(Some(repl.ParsedCommand(name: "help", args: [])))
}

pub fn parse_command_with_single_arg_test() {
  repl.parse_command("pause agent-1")
  |> should.equal(Some(repl.ParsedCommand(name: "pause", args: ["agent-1"])))
}

pub fn parse_command_with_multiple_args_test() {
  repl.parse_command("focus agent-1 agent-2")
  |> should.equal(
    Some(repl.ParsedCommand(name: "focus", args: ["agent-1", "agent-2"])),
  )
}

pub fn parse_command_with_leading_whitespace_test() {
  repl.parse_command("  help")
  |> should.equal(Some(repl.ParsedCommand(name: "help", args: [])))
}

pub fn parse_command_with_trailing_whitespace_test() {
  repl.parse_command("help  ")
  |> should.equal(Some(repl.ParsedCommand(name: "help", args: [])))
}

pub fn parse_command_with_extra_spaces_test() {
  repl.parse_command("pause   agent-1")
  |> should.equal(Some(repl.ParsedCommand(name: "pause", args: ["agent-1"])))
}

pub fn parse_command_lowercases_name_test() {
  repl.parse_command("HELP")
  |> should.equal(Some(repl.ParsedCommand(name: "help", args: [])))
}

pub fn parse_command_preserves_arg_case_test() {
  repl.parse_command("pause Agent-1")
  |> should.equal(Some(repl.ParsedCommand(name: "pause", args: ["Agent-1"])))
}

// ---------------------------------------------------------------------------
// Quoted String Parsing Tests
// ---------------------------------------------------------------------------

pub fn parse_double_quoted_arg_test() {
  repl.parse_command("run \"my task\"")
  |> should.equal(Some(repl.ParsedCommand(name: "run", args: ["my task"])))
}

pub fn parse_double_quoted_args_test() {
  repl.parse_command("run \"task title\" \"task description\"")
  |> should.equal(
    Some(
      repl.ParsedCommand(name: "run", args: ["task title", "task description"]),
    ),
  )
}

pub fn parse_single_quoted_arg_test() {
  repl.parse_command("run 'my task'")
  |> should.equal(Some(repl.ParsedCommand(name: "run", args: ["my task"])))
}

pub fn parse_mixed_quoted_args_test() {
  repl.parse_command("run \"task title\" 'task desc'")
  |> should.equal(
    Some(repl.ParsedCommand(name: "run", args: ["task title", "task desc"])),
  )
}

pub fn parse_quoted_with_unquoted_test() {
  repl.parse_command("run \"my task\" --verbose")
  |> should.equal(
    Some(repl.ParsedCommand(name: "run", args: ["my task", "--verbose"])),
  )
}

pub fn parse_empty_quoted_string_test() {
  repl.parse_command("run \"\"")
  |> should.equal(Some(repl.ParsedCommand(name: "run", args: [])))
}

pub fn parse_quoted_with_special_chars_test() {
  repl.parse_command("run \"fix bug #123\"")
  |> should.equal(Some(repl.ParsedCommand(name: "run", args: ["fix bug #123"])))
}

// ---------------------------------------------------------------------------
// Command Execution Tests
// ---------------------------------------------------------------------------

pub fn execute_unknown_command_test() {
  let registry = dict.new()
  let cmd = repl.ParsedCommand(name: "unknown", args: [])

  repl.execute_command(registry, cmd)
  |> should.equal(repl.UnknownCommand("unknown"))
}

pub fn execute_registered_command_test() {
  let handler = fn(_args) { repl.CommandOk }
  let registry = dict.new() |> dict.insert("test", handler)
  let cmd = repl.ParsedCommand(name: "test", args: [])

  repl.execute_command(registry, cmd)
  |> should.equal(repl.CommandOk)
}

pub fn execute_command_with_output_test() {
  let handler = fn(_args) { repl.CommandOutput("hello world") }
  let registry = dict.new() |> dict.insert("echo", handler)
  let cmd = repl.ParsedCommand(name: "echo", args: [])

  repl.execute_command(registry, cmd)
  |> should.equal(repl.CommandOutput("hello world"))
}

pub fn execute_command_receives_args_test() {
  let handler = fn(args) {
    case args {
      [arg] -> repl.CommandOutput("got: " <> arg)
      _ -> repl.CommandError("expected one arg")
    }
  }
  let registry = dict.new() |> dict.insert("say", handler)
  let cmd = repl.ParsedCommand(name: "say", args: ["hello"])

  repl.execute_command(registry, cmd)
  |> should.equal(repl.CommandOutput("got: hello"))
}

// ---------------------------------------------------------------------------
// Registry Management Tests
// ---------------------------------------------------------------------------

pub fn default_config_has_help_test() {
  let config = repl.default_config()

  repl.has_command(config, "help")
  |> should.be_true
}

pub fn default_config_has_quit_test() {
  let config = repl.default_config()

  repl.has_command(config, "quit")
  |> should.be_true
}

pub fn default_config_has_exit_test() {
  let config = repl.default_config()

  repl.has_command(config, "exit")
  |> should.be_true
}

pub fn default_config_has_q_test() {
  let config = repl.default_config()

  repl.has_command(config, "q")
  |> should.be_true
}

pub fn add_command_test() {
  let config = repl.default_config()
  let handler = fn(_args) { repl.CommandOk }

  let config = repl.add_command(config, "custom", handler)

  repl.has_command(config, "custom")
  |> should.be_true
}

pub fn remove_command_test() {
  let config = repl.default_config()
  let config = repl.remove_command(config, "help")

  repl.has_command(config, "help")
  |> should.be_false
}

pub fn list_commands_includes_defaults_test() {
  let config = repl.default_config()
  let commands = repl.list_commands(config)

  // Default config should have at least help, quit, exit, q
  { commands != [] }
  |> should.be_true
}

pub fn add_multiple_commands_test() {
  let config = repl.default_config()
  let handler = fn(_args) { repl.CommandOk }

  let config =
    repl.add_commands(config, [#("cmd1", handler), #("cmd2", handler)])

  repl.has_command(config, "cmd1")
  |> should.be_true

  repl.has_command(config, "cmd2")
  |> should.be_true
}
