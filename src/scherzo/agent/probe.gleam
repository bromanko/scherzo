import scherzo/config/types as config_types
import scherzo/error
import scherzo/pi/client
import scherzo/pi/command as pi_command

pub fn probe(
  command: String,
  cwd: String,
  read_timeout_ms: Int,
) -> Result(Nil, error.PiRpcError) {
  probe_launch(pi_command.ShellLaunch(command), cwd, read_timeout_ms)
}

pub fn probe_config(
  pi: config_types.PiConfig,
  cwd: String,
  read_timeout_ms: Int,
) -> Result(Nil, error.PiRpcError) {
  case pi_command.build_launch(pi, pi_command.FreshNoSession) {
    Error(err) -> Error(error.PiProtocolError(error.config_message(err)))
    Ok(launch) -> probe_launch(launch, cwd, read_timeout_ms)
  }
}

pub fn probe_launch(
  launch: pi_command.LaunchSpec,
  cwd: String,
  read_timeout_ms: Int,
) -> Result(Nil, error.PiRpcError) {
  case
    client.launch_spec(
      launch,
      cwd,
      "scherzo compatibility probe",
      False,
      read_timeout_ms,
    )
  {
    Ok(session) -> {
      case client.get_session_stats(session, read_timeout_ms) {
        Ok(#(session, _)) -> client.terminate(session)
        Error(err) -> Error(client.terminate_after_failure(session, err))
      }
    }
    Error(err) -> Error(err)
  }
}
