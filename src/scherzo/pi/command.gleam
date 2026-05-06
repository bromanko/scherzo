import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error

pub type LaunchSpec {
  ShellLaunch(command: String)
  ArgvLaunch(
    executable: String,
    args: List(String),
    env: List(#(String, String)),
  )
}

pub type LaunchMode {
  FreshNoSession
  FreshPersistent
  ContinueSession(session_file: String)
}

pub fn build_launch(
  pi: config_types.PiConfig,
  mode: LaunchMode,
) -> Result(LaunchSpec, error.ConfigError) {
  case mode {
    FreshNoSession -> Ok(ShellLaunch(pi.command))
    FreshPersistent -> argv_launch(pi.argv_command)
    ContinueSession(session_file) -> {
      let session_file = string.trim(session_file)
      case session_file == "" {
        True ->
          Error(error.InvalidConfig(
            "pi.session_persistence requires a non-empty session file",
          ))
        False -> {
          use launch <- result.try(argv_launch(pi.argv_command))
          case launch {
            ShellLaunch(_) ->
              Error(error.InvalidConfig(
                "pi.session_persistence continuation requires pi.argv",
              ))
            ArgvLaunch(executable, args, env) ->
              Ok(ArgvLaunch(
                executable,
                list_append(args, ["--session", session_file]),
                env,
              ))
          }
        }
      }
    }
  }
}

fn argv_launch(
  argv_command: Option(config_types.PiArgvCommand),
) -> Result(LaunchSpec, error.ConfigError) {
  case argv_command {
    None ->
      Error(error.InvalidConfig("pi.session_persistence requires pi.argv"))
    Some(argv) ->
      case string.trim(argv.executable) == "" {
        True ->
          Error(error.InvalidConfig(
            "pi.session_persistence requires pi.argv executable to be non-empty",
          ))
        False ->
          case has_forbidden_session_flag(argv.args) {
            True ->
              Error(error.InvalidConfig(
                "pi.session_persistence requires pi.argv without --session or --no-session",
              ))
            False -> Ok(ArgvLaunch(argv.executable, argv.args, argv.env))
          }
      }
  }
}

fn has_forbidden_session_flag(args: List(String)) -> Bool {
  case args {
    [] -> False
    [arg, ..rest] ->
      arg == "--session"
      || arg == "--no-session"
      || has_forbidden_session_flag(rest)
  }
}

fn list_append(left: List(a), right: List(a)) -> List(a) {
  case left {
    [] -> right
    [item, ..rest] -> [item, ..list_append(rest, right)]
  }
}
