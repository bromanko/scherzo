/// tmux session and pane management for scherzo UI
///
/// Provides functions to create, attach to, and destroy tmux sessions,
/// as well as manage panes for agent output.
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import scherzo/core/shell
import shellout

/// Default session name for scherzo
pub const default_session_name = "scherzo"

/// Result of a tmux operation
pub type TmuxError {
  /// Session already exists
  SessionExists(name: String)
  /// Session does not exist
  SessionNotFound(name: String)
  /// tmux command failed
  CommandFailed(message: String)
  /// tmux is not installed or not available
  TmuxNotAvailable
}

/// A tmux pane identifier
pub type PaneId =
  String

/// Information about a pane
pub type PaneInfo {
  PaneInfo(id: PaneId, index: Int, is_active: Bool)
}

// ---------------------------------------------------------------------------
// Session Lifecycle
// ---------------------------------------------------------------------------

/// Check if tmux is available on the system
pub fn is_available() -> Bool {
  case run_tmux(["list-sessions"]) {
    Ok(_) -> True
    // "no server running" is expected if tmux is installed but no sessions exist
    Error(CommandFailed(msg)) -> string.contains(msg, "no server running")
    Error(_) -> False
  }
}

/// Check if a session with the given name exists
pub fn session_exists(name: String) -> Bool {
  case run_tmux(["has-session", "-t", name]) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Check if we're currently running inside a specific tmux session
pub fn is_inside_session(target_session: String) -> Bool {
  // If TMUX env var is not set, we're not in any tmux session
  case get_env("TMUX") {
    Error(_) -> False
    Ok(_) -> {
      // Get current session name
      case run_tmux(["display-message", "-p", "#{session_name}"]) {
        Ok(current_session) -> string.trim(current_session) == target_session
        Error(_) -> False
      }
    }
  }
}

/// Create a new detached tmux session
/// Returns error if session already exists
pub fn create_session(name: String) -> Result(Nil, TmuxError) {
  case session_exists(name) {
    True -> Error(SessionExists(name))
    False -> {
      run_tmux(["new-session", "-d", "-s", name])
      |> result.map(fn(_) { Nil })
    }
  }
}

/// Create a session with a specific initial command running in it
pub fn create_session_with_command(
  name: String,
  command: String,
) -> Result(Nil, TmuxError) {
  case session_exists(name) {
    True -> Error(SessionExists(name))
    False -> {
      run_tmux(["new-session", "-d", "-s", name, command])
      |> result.map(fn(_) { Nil })
    }
  }
}

/// Attach to an existing session (blocks until detached)
/// This replaces the current terminal with the tmux session
pub fn attach_session(name: String) -> Result(Nil, TmuxError) {
  case session_exists(name) {
    False -> Error(SessionNotFound(name))
    True -> {
      // Use exec to replace current process with tmux attach
      run_tmux_exec(["attach-session", "-t", name])
      |> result.map(fn(_) { Nil })
    }
  }
}

/// Kill (destroy) a session
pub fn kill_session(name: String) -> Result(Nil, TmuxError) {
  case session_exists(name) {
    False -> Error(SessionNotFound(name))
    True -> {
      run_tmux(["kill-session", "-t", name])
      |> result.map(fn(_) { Nil })
    }
  }
}

/// List all session names
pub fn list_sessions() -> Result(List(String), TmuxError) {
  run_tmux(["list-sessions", "-F", "#{session_name}"])
  |> result.map(fn(output) {
    output
    |> string.trim
    |> string.split("\n")
    |> list.filter(fn(s) { s != "" })
  })
}

// ---------------------------------------------------------------------------
// Pane Management
// ---------------------------------------------------------------------------

/// Split the current pane horizontally (new pane below)
pub fn split_horizontal(session: String) -> Result(PaneId, TmuxError) {
  run_tmux([
    "split-window",
    "-t",
    session,
    "-v",
    "-P",
    "-F",
    "#{pane_id}",
  ])
  |> result.map(string.trim)
}

/// Split the current pane vertically (new pane to the right)
pub fn split_vertical(session: String) -> Result(PaneId, TmuxError) {
  run_tmux([
    "split-window",
    "-t",
    session,
    "-h",
    "-P",
    "-F",
    "#{pane_id}",
  ])
  |> result.map(string.trim)
}

/// Split and run a command in the new pane
pub fn split_with_command(
  session: String,
  command: String,
  horizontal: Bool,
) -> Result(PaneId, TmuxError) {
  let direction = case horizontal {
    True -> "-v"
    False -> "-h"
  }
  run_tmux([
    "split-window",
    "-t",
    session,
    direction,
    "-P",
    "-F",
    "#{pane_id}",
    command,
  ])
  |> result.map(string.trim)
}

/// Kill a specific pane
pub fn kill_pane(pane_id: PaneId) -> Result(Nil, TmuxError) {
  run_tmux(["kill-pane", "-t", pane_id])
  |> result.map(fn(_) { Nil })
}

/// Send keys to a pane (simulates typing)
pub fn send_keys(pane_id: PaneId, keys: String) -> Result(Nil, TmuxError) {
  run_tmux(["send-keys", "-t", pane_id, keys, "Enter"])
  |> result.map(fn(_) { Nil })
}

/// Send keys without pressing Enter
pub fn send_keys_raw(pane_id: PaneId, keys: String) -> Result(Nil, TmuxError) {
  run_tmux(["send-keys", "-t", pane_id, keys])
  |> result.map(fn(_) { Nil })
}

/// Apply tiled layout to session (evenly distribute panes)
pub fn apply_tiled_layout(session: String) -> Result(Nil, TmuxError) {
  run_tmux(["select-layout", "-t", session, "tiled"])
  |> result.map(fn(_) { Nil })
}

/// Apply main-horizontal layout (one large pane on top, others below)
pub fn apply_main_horizontal_layout(session: String) -> Result(Nil, TmuxError) {
  run_tmux(["select-layout", "-t", session, "main-horizontal"])
  |> result.map(fn(_) { Nil })
}

/// Set the height of the main pane (for main-horizontal layout)
pub fn set_main_pane_height(
  session: String,
  height: Int,
) -> Result(Nil, TmuxError) {
  run_tmux([
    "set-option",
    "-t",
    session,
    "main-pane-height",
    int.to_string(height),
  ])
  |> result.map(fn(_) { Nil })
}

/// List all panes in a session
pub fn list_panes(session: String) -> Result(List(PaneInfo), TmuxError) {
  run_tmux([
    "list-panes",
    "-t",
    session,
    "-F",
    "#{pane_id}:#{pane_index}:#{pane_active}",
  ])
  |> result.map(fn(output) {
    output
    |> string.trim
    |> string.split("\n")
    |> list.filter(fn(s) { s != "" })
    |> list.filter_map(parse_pane_info)
  })
}

/// List panes with their running commands
/// Returns list of (pane_id, command) tuples
pub fn list_pane_commands(
  session: String,
) -> Result(List(#(PaneId, String)), TmuxError) {
  run_tmux([
    "list-panes",
    "-t",
    session,
    "-F",
    "#{pane_id}:#{pane_current_command}",
  ])
  |> result.map(fn(output) {
    output
    |> string.trim
    |> string.split("\n")
    |> list.filter(fn(s) { s != "" })
    |> list.filter_map(fn(line) {
      case string.split_once(line, ":") {
        Ok(#(pane_id, cmd)) -> Ok(#(pane_id, cmd))
        Error(_) -> Error(Nil)
      }
    })
  })
}

/// Select (focus) a specific pane
pub fn select_pane(pane_id: PaneId) -> Result(Nil, TmuxError) {
  run_tmux(["select-pane", "-t", pane_id])
  |> result.map(fn(_) { Nil })
}

/// Zoom a pane to full screen (toggle)
pub fn zoom_pane(pane_id: PaneId) -> Result(Nil, TmuxError) {
  run_tmux(["resize-pane", "-t", pane_id, "-Z"])
  |> result.map(fn(_) { Nil })
}

/// Resize a pane to a specific height (in rows)
pub fn resize_pane_height(pane_id: PaneId, height: Int) -> Result(Nil, TmuxError) {
  run_tmux(["resize-pane", "-t", pane_id, "-y", int.to_string(height)])
  |> result.map(fn(_) { Nil })
}

/// Get the ID of the first pane in a session
pub fn get_first_pane(session: String) -> Result(PaneId, TmuxError) {
  run_tmux(["list-panes", "-t", session, "-F", "#{pane_id}"])
  |> result.try(fn(output) {
    output
    |> string.trim
    |> string.split("\n")
    |> list.first
    |> result.map_error(fn(_) { CommandFailed("No panes found in session") })
  })
}

// ---------------------------------------------------------------------------
// Window Management (for future use)
// ---------------------------------------------------------------------------

/// Rename the current window
pub fn rename_window(session: String, name: String) -> Result(Nil, TmuxError) {
  run_tmux(["rename-window", "-t", session, name])
  |> result.map(fn(_) { Nil })
}

// ---------------------------------------------------------------------------
// Key Bindings
// ---------------------------------------------------------------------------

/// Bind a key to run a shell command (no prefix required)
/// The binding is registered in the root key table, so it works without
/// the tmux prefix key. Use this for hotkeys like F1.
pub fn bind_key(key: String, shell_command: String) -> Result(Nil, TmuxError) {
  run_tmux(["bind-key", "-n", key, "run-shell", shell_command])
  |> result.map(fn(_) { Nil })
}

/// Unbind a key from the root table
pub fn unbind_key(key: String) -> Result(Nil, TmuxError) {
  run_tmux(["unbind-key", "-n", key])
  |> result.map(fn(_) { Nil })
}

// ---------------------------------------------------------------------------
// Internal Helpers
// ---------------------------------------------------------------------------

/// Parse pane info from tmux list-panes output line
fn parse_pane_info(line: String) -> Result(PaneInfo, Nil) {
  case string.split(line, ":") {
    [id, index_str, active_str] -> {
      case int.parse(index_str) {
        Ok(index) -> {
          let is_active = active_str == "1"
          Ok(PaneInfo(id: id, index: index, is_active: is_active))
        }
        Error(_) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Run a tmux command and return the output (with 10s timeout)
fn run_tmux(args: List(String)) -> Result(String, TmuxError) {
  case shell.run_with_timeout("tmux", args, ".", shell.tmux_timeout_ms) {
    shell.Success(output) -> Ok(output)
    shell.Failed(127, _) -> Error(TmuxNotAvailable)
    shell.Failed(_, err_output) -> Error(CommandFailed(err_output))
    shell.TimedOut -> Error(CommandFailed("tmux command timed out"))
  }
}

/// Run tmux attach (which needs special handling as it takes over the terminal)
/// Uses shellout with LetBeStdout to allow terminal passthrough
fn run_tmux_exec(args: List(String)) -> Result(String, TmuxError) {
  // Use LetBeStdout so the terminal is passed through to tmux
  // This allows interactive commands like attach to work properly
  case shellout.command("tmux", args, ".", [shellout.LetBeStdout]) {
    Ok(output) -> Ok(output)
    Error(#(127, _)) -> Error(TmuxNotAvailable)
    Error(#(_, err_output)) -> Error(CommandFailed(err_output))
  }
}

/// Get an environment variable
/// Returns Ok(value) if set, Error(Nil) if not set
fn get_env(name: String) -> Result(String, Nil) {
  case os_getenv_raw(name) {
    "" -> Error(Nil)
    value -> Ok(value)
  }
}

/// Raw Erlang os:getenv - returns empty string if not set
@external(erlang, "scherzo_env_ffi", "getenv")
fn os_getenv_raw(name: String) -> String
