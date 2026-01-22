/// Control REPL for scherzo tmux UI
///
/// Provides an interactive command-line interface for controlling
/// the orchestrator from within the tmux control pane.
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// A parsed command with its arguments
pub type ParsedCommand {
  ParsedCommand(name: String, args: List(String))
}

/// Result of executing a command
pub type CommandResult {
  /// Command executed successfully
  CommandOk
  /// Command executed with output to display
  CommandOutput(output: String)
  /// Command failed with error message
  CommandError(message: String)
  /// Unknown command
  UnknownCommand(name: String)
  /// Request to quit the REPL
  QuitRequested
}

/// A command handler function type
/// Takes arguments and returns a result
pub type CommandHandler =
  fn(List(String)) -> CommandResult

/// Command registry mapping names to handlers
pub type CommandRegistry =
  Dict(String, CommandHandler)

/// REPL configuration
pub type ReplConfig {
  ReplConfig(
    /// Prompt to display
    prompt: String,
    /// Command registry
    commands: CommandRegistry,
    /// Help text for unknown commands
    unknown_command_help: String,
  )
}

/// Create a default REPL config with basic commands
pub fn default_config() -> ReplConfig {
  ReplConfig(
    prompt: "scherzo> ",
    commands: default_commands(),
    unknown_command_help: "Type 'help' for available commands",
  )
}

/// Default command registry with built-in commands
fn default_commands() -> CommandRegistry {
  dict.new()
  |> dict.insert("help", help_command)
  |> dict.insert("quit", quit_command)
  |> dict.insert("exit", quit_command)
  |> dict.insert("q", quit_command)
}

/// Built-in help command
fn help_command(_args: List(String)) -> CommandResult {
  let help_text =
    "Available commands:
  help              - Show this help message
  run \"title\" [\"desc\"] - Run a single task
  run --from-tickets    - Run tasks from .tickets/
  status            - Show orchestrator status
  tasks             - List all tasks
  agents            - Show agent status
  pause <id>        - Pause an agent
  resume <id>       - Resume a paused agent
  retry <id>        - Retry a failed task
  kill <id>         - Kill a running agent
  focus <id>        - Focus on an agent pane
  quit              - Exit the REPL (graceful shutdown)
  abort             - Immediate shutdown"

  CommandOutput(help_text)
}

/// Built-in quit command
fn quit_command(_args: List(String)) -> CommandResult {
  QuitRequested
}

// ---------------------------------------------------------------------------
// Command Parsing
// ---------------------------------------------------------------------------

/// Parse a line of input into a command
pub fn parse_command(input: String) -> Option(ParsedCommand) {
  let trimmed = string.trim(input)

  case trimmed {
    "" -> None
    _ -> {
      let parts =
        trimmed
        |> string.split(" ")
        |> list.filter(fn(s) { s != "" })

      case parts {
        [] -> None
        [name, ..args] ->
          Some(ParsedCommand(name: string.lowercase(name), args: args))
      }
    }
  }
}

/// Execute a parsed command using the registry
pub fn execute_command(
  registry: CommandRegistry,
  command: ParsedCommand,
) -> CommandResult {
  case dict.get(registry, command.name) {
    Ok(handler) -> handler(command.args)
    Error(_) -> UnknownCommand(command.name)
  }
}

// ---------------------------------------------------------------------------
// REPL Loop
// ---------------------------------------------------------------------------

/// Run the REPL loop (blocking)
/// This reads from stdin and processes commands until quit
pub fn run(config: ReplConfig) -> Nil {
  repl_loop(config)
}

/// Internal REPL loop
fn repl_loop(config: ReplConfig) -> Nil {
  // Print prompt
  io.print(config.prompt)

  // Read line from stdin
  case read_line() {
    Error(_) -> {
      // EOF or error, exit gracefully
      io.println("")
      io.println("Goodbye!")
      Nil
    }
    Ok(line) -> {
      // Parse and execute command
      case parse_command(line) {
        None -> {
          // Empty input, continue
          repl_loop(config)
        }
        Some(cmd) -> {
          let result = execute_command(config.commands, cmd)
          handle_result(result, config)
        }
      }
    }
  }
}

/// Handle command result and continue or exit
fn handle_result(result: CommandResult, config: ReplConfig) -> Nil {
  case result {
    CommandOk -> repl_loop(config)

    CommandOutput(output) -> {
      io.println(output)
      repl_loop(config)
    }

    CommandError(message) -> {
      io.println("Error: " <> message)
      repl_loop(config)
    }

    UnknownCommand(name) -> {
      io.println("Unknown command: " <> name)
      io.println(config.unknown_command_help)
      repl_loop(config)
    }

    QuitRequested -> {
      io.println("Shutting down...")
      Nil
    }
  }
}

// ---------------------------------------------------------------------------
// Stdin Reading
// ---------------------------------------------------------------------------

/// Read a line from stdin
/// Uses custom FFI that wraps Erlang io:get_line/1 and returns Result
fn read_line() -> Result(String, Nil) {
  case do_read_line("") {
    Ok(line) -> Ok(string.trim_end(line))
    Error(_) -> Error(Nil)
  }
}

/// FFI wrapper for io:get_line that returns proper Result type
@external(erlang, "scherzo_io_ffi", "get_line")
fn do_read_line(prompt: String) -> Result(String, Nil)

// ---------------------------------------------------------------------------
// Registry Management
// ---------------------------------------------------------------------------

/// Add a command to the registry
pub fn add_command(
  config: ReplConfig,
  name: String,
  handler: CommandHandler,
) -> ReplConfig {
  ReplConfig(..config, commands: dict.insert(config.commands, name, handler))
}

/// Add multiple commands to the registry
pub fn add_commands(
  config: ReplConfig,
  commands: List(#(String, CommandHandler)),
) -> ReplConfig {
  list.fold(commands, config, fn(cfg, cmd) {
    let #(name, handler) = cmd
    add_command(cfg, name, handler)
  })
}

/// Remove a command from the registry
pub fn remove_command(config: ReplConfig, name: String) -> ReplConfig {
  ReplConfig(..config, commands: dict.delete(config.commands, name))
}

/// Check if a command exists in the registry
pub fn has_command(config: ReplConfig, name: String) -> Bool {
  dict.has_key(config.commands, name)
}

/// List all command names
pub fn list_commands(config: ReplConfig) -> List(String) {
  dict.keys(config.commands)
}
