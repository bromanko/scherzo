import argv
import gleam/io
import gleam/result
import gleam/string
import glint
import scherzo/orchestrator.{OrchestratorConfig}
import simplifile

pub fn main() {
  glint.new()
  |> glint.with_name("scherzo")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: root_command())
  |> glint.add(at: ["run"], do: run_command())
  |> glint.add(at: ["status"], do: status_command())
  |> glint.run(argv.load().arguments)
}

fn root_command() -> glint.Command(Nil) {
  use <- glint.command_help("Scherzo - AI Agent Orchestrator")
  use _, _, _ <- glint.command()
  io.println("Scherzo - AI Agent Orchestrator")
  io.println("Use 'scherzo --help' for usage information")
  Nil
}

/// Flag for working directory
fn workdir_flag() {
  glint.string_flag("workdir")
  |> glint.flag_default(".")
  |> glint.flag_help("Working directory for the agent")
}

fn run_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Run a task with an AI agent. Usage: scherzo run \"task title\" \"task description\"",
  )
  use workdir_getter <- glint.flag(workdir_flag())
  use _, args, flags <- glint.command()

  // Get the working directory from flags
  let working_dir =
    workdir_getter(flags)
    |> result.unwrap(".")
    |> resolve_path()

  // Parse arguments
  case args {
    [title, description] -> {
      io.println("Running task: " <> title)
      io.println("Working directory: " <> working_dir)
      io.println("")

      let config =
        OrchestratorConfig(working_dir: working_dir, max_retries: 3)

      case orchestrator.run_task(config, title, description) {
        orchestrator.RunSuccess(output, change_id) -> {
          io.println("Task completed successfully!")
          io.println("Change ID: " <> change_id)
          io.println("")
          io.println("Output:")
          io.println(output)
        }
        orchestrator.RunFailed(reason) -> {
          io.println("Task failed: " <> reason)
        }
      }
    }

    [title] -> {
      // If only title provided, use it as description too
      io.println("Running task: " <> title)
      io.println("Working directory: " <> working_dir)
      io.println("")

      let config =
        OrchestratorConfig(working_dir: working_dir, max_retries: 3)

      case orchestrator.run_task(config, title, title) {
        orchestrator.RunSuccess(output, change_id) -> {
          io.println("Task completed successfully!")
          io.println("Change ID: " <> change_id)
          io.println("")
          io.println("Output:")
          io.println(output)
        }
        orchestrator.RunFailed(reason) -> {
          io.println("Task failed: " <> reason)
        }
      }
    }

    _ -> {
      io.println("Usage: scherzo run \"task title\" [\"task description\"]")
      io.println("")
      io.println("Examples:")
      io.println("  scherzo run \"Fix the login bug\"")
      io.println(
        "  scherzo run \"Add feature\" \"Add user authentication to the API\"",
      )
    }
  }
  Nil
}

fn status_command() -> glint.Command(Nil) {
  use <- glint.command_help("Show orchestrator status")
  use _, _, _ <- glint.command()
  io.println("Status: Ready")
  io.println("Available agents: claude")
  Nil
}

/// Resolve a path (handle relative paths)
fn resolve_path(path: String) -> String {
  case string.starts_with(path, "/") {
    True -> path
    False -> {
      let cwd = get_cwd()
      case path {
        "." -> cwd
        _ -> cwd <> "/" <> path
      }
    }
  }
}

/// Get current working directory
fn get_cwd() -> String {
  case simplifile.current_directory() {
    Ok(dir) -> dir
    Error(_) -> "."
  }
}
