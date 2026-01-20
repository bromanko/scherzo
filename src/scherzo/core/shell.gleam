/// Shell command utilities with timeout support
import gleam/erlang/process.{type Subject}
import shellout

/// Default timeout for jj commands (60 seconds)
pub const jj_timeout_ms = 60_000

/// Default timeout for tk commands (30 seconds)
pub const tk_timeout_ms = 30_000

/// Default timeout for short utility commands like mkfifo, test (5 seconds)
pub const util_timeout_ms = 5000

/// Default timeout for tmux commands (10 seconds)
pub const tmux_timeout_ms = 10_000

/// Result of a shell command
pub type CommandResult {
  /// Command completed successfully
  Success(output: String)
  /// Command failed with exit code
  Failed(exit_code: Int, output: String)
  /// Command timed out
  TimedOut
}

/// Run a shell command with a timeout
/// Returns TimedOut if the command doesn't complete within timeout_ms
pub fn run_with_timeout(
  executable: String,
  args: List(String),
  working_dir: String,
  timeout_ms: Int,
) -> CommandResult {
  // Create a subject to receive the result
  let result_subject: Subject(CommandResult) = process.new_subject()

  // Spawn a process to run the command
  let pid =
    erlang_spawn(fn() {
      let result = case
        shellout.command(run: executable, with: args, in: working_dir, opt: [])
      {
        Ok(output) -> Success(output)
        Error(#(exit_code, output)) -> Failed(exit_code, output)
      }
      process.send(result_subject, result)
    })

  // Wait for result with timeout
  case process.receive(result_subject, timeout_ms) {
    Ok(result) -> result
    Error(Nil) -> {
      // Timeout occurred - kill the spawned process
      erlang_exit_kill(pid)
      TimedOut
    }
  }
}

/// Convert CommandResult to Result(String, String) for simple use cases
pub fn to_result(
  cmd_result: CommandResult,
  cmd_name: String,
) -> Result(String, String) {
  case cmd_result {
    Success(output) -> Ok(output)
    Failed(_, output) -> Error(cmd_name <> " error: " <> output)
    TimedOut -> Error(cmd_name <> " command timed out")
  }
}

/// Spawn an Erlang process (not linked)
@external(erlang, "erlang", "spawn")
fn erlang_spawn(func: fn() -> a) -> process.Pid

/// Kill an Erlang process with exit reason 'kill'
@external(erlang, "scherzo_process_ffi", "exit_kill")
fn erlang_exit_kill(pid: process.Pid) -> Bool
