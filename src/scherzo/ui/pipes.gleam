/// Named pipe (FIFO) management for agent output routing
///
/// Creates and manages named pipes for streaming agent output to tmux panes.
/// Each agent gets a dedicated pipe at .scherzo/pipes/<agent-id>
import gleam/list
import gleam/result
import gleam/string
import scherzo/core/shell
import simplifile

/// Default directory for pipes
pub const default_pipes_dir = ".scherzo/pipes"

/// Configuration for pipe management
pub type PipeConfig {
  PipeConfig(
    /// Base directory for pipes
    pipes_dir: String,
  )
}

/// Error during pipe operations
pub type PipeError {
  /// Failed to create pipe directory
  DirectoryError(String)
  /// Failed to create named pipe
  CreateError(String)
  /// Pipe does not exist
  PipeNotFound(agent_id: String)
  /// Failed to remove pipe
  RemoveError(String)
  /// Write failed
  WriteError(String)
}

/// Create default pipe config
pub fn default_config(working_dir: String) -> PipeConfig {
  PipeConfig(pipes_dir: working_dir <> "/" <> default_pipes_dir)
}

/// Get the path to an agent's pipe
pub fn pipe_path(config: PipeConfig, agent_id: String) -> String {
  config.pipes_dir <> "/" <> sanitize_agent_id(agent_id)
}

/// Sanitize agent ID for use as filename
fn sanitize_agent_id(agent_id: String) -> String {
  agent_id
  |> string.replace("/", "_")
  |> string.replace(" ", "_")
  |> string.replace("..", "_")
}

// ---------------------------------------------------------------------------
// Pipe Lifecycle
// ---------------------------------------------------------------------------

/// Ensure pipes directory exists
pub fn ensure_pipes_dir(config: PipeConfig) -> Result(Nil, PipeError) {
  case simplifile.create_directory_all(config.pipes_dir) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(DirectoryError(simplifile_error_to_string(err)))
  }
}

/// Create a named pipe (FIFO) for an agent
/// The pipe will be created at .scherzo/pipes/<agent-id>
pub fn create_pipe(
  config: PipeConfig,
  agent_id: String,
) -> Result(String, PipeError) {
  // Ensure directory exists
  use _ <- result.try(ensure_pipes_dir(config))

  let path = pipe_path(config, agent_id)

  // Remove existing pipe if present
  let _ = simplifile.delete(path)

  // Create named pipe using mkfifo (with 5s timeout)
  case shell.run_with_timeout("mkfifo", [path], ".", shell.util_timeout_ms) {
    shell.Success(_) -> Ok(path)
    shell.Failed(_, err) -> Error(CreateError("mkfifo failed: " <> err))
    shell.TimedOut -> Error(CreateError("mkfifo timed out"))
  }
}

/// Remove an agent's pipe
pub fn remove_pipe(
  config: PipeConfig,
  agent_id: String,
) -> Result(Nil, PipeError) {
  let path = pipe_path(config, agent_id)

  case simplifile.delete(path) {
    Ok(_) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    // Already gone, that's fine
    Error(err) -> Error(RemoveError(simplifile_error_to_string(err)))
  }
}

/// Check if a pipe exists for an agent
/// Note: Uses file_info instead of is_file because FIFOs are special files
pub fn pipe_exists(config: PipeConfig, agent_id: String) -> Bool {
  let path = pipe_path(config, agent_id)
  // Use test -p to check for named pipe (FIFO) with 5s timeout
  case
    shell.run_with_timeout("test", ["-p", path], ".", shell.util_timeout_ms)
  {
    shell.Success(_) -> True
    _ -> False
  }
}

/// List all existing pipes
pub fn list_pipes(config: PipeConfig) -> Result(List(String), PipeError) {
  case simplifile.read_directory(config.pipes_dir) {
    Ok(files) -> Ok(files)
    Error(simplifile.Enoent) -> Ok([])
    Error(err) -> Error(DirectoryError(simplifile_error_to_string(err)))
  }
}

/// Clean up all pipes in the directory
pub fn cleanup_all_pipes(config: PipeConfig) -> Result(Nil, PipeError) {
  case list_pipes(config) {
    Error(err) -> Error(err)
    Ok(files) -> {
      // Remove each file
      list.each(files, fn(file) {
        let path = config.pipes_dir <> "/" <> file
        let _ = simplifile.delete(path)
        Nil
      })
      Ok(Nil)
    }
  }
}

// ---------------------------------------------------------------------------
// tmux Integration
// ---------------------------------------------------------------------------

/// Get the tail command for a pipe
/// This command can be run in a tmux pane to display agent output
pub fn tail_command(config: PipeConfig, agent_id: String) -> String {
  let path = pipe_path(config, agent_id)
  "tail -f " <> path
}

/// Get a cat command for a pipe (non-blocking read)
pub fn cat_command(config: PipeConfig, agent_id: String) -> String {
  let path = pipe_path(config, agent_id)
  "cat " <> path
}

// ---------------------------------------------------------------------------
// Writing to Pipes
// ---------------------------------------------------------------------------

/// Write a message to an agent's pipe
/// Note: This will block if no reader is connected to the pipe
/// For non-blocking writes, use write_async or ensure a reader exists
pub fn write_to_pipe(
  config: PipeConfig,
  agent_id: String,
  message: String,
) -> Result(Nil, PipeError) {
  let path = pipe_path(config, agent_id)

  case pipe_exists(config, agent_id) {
    False -> Error(PipeNotFound(agent_id))
    True -> {
      // Use shell echo with output redirection to avoid blocking
      // The timeout ensures we don't hang forever if no reader (5s timeout)
      case
        shell.run_with_timeout(
          "sh",
          ["-c", "echo '" <> escape_for_shell(message) <> "' > " <> path],
          ".",
          shell.util_timeout_ms,
        )
      {
        shell.Success(_) -> Ok(Nil)
        shell.Failed(_, err) -> Error(WriteError(err))
        shell.TimedOut -> Error(WriteError("write to pipe timed out"))
      }
    }
  }
}

/// Escape a string for shell single quotes
fn escape_for_shell(s: String) -> String {
  // Replace single quotes with escaped version
  string.replace(s, "'", "'\\''")
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert simplifile error to string
fn simplifile_error_to_string(err: simplifile.FileError) -> String {
  case err {
    simplifile.Enoent -> "File not found"
    simplifile.Eacces -> "Permission denied"
    simplifile.Eexist -> "File exists"
    simplifile.Enotdir -> "Not a directory"
    simplifile.Eisdir -> "Is a directory"
    _ -> "File error"
  }
}
