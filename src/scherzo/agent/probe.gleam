import scherzo/agent/pi_rpc
import scherzo/error

pub fn probe(
  command: String,
  cwd: String,
  read_timeout_ms: Int,
) -> Result(Nil, error.PiRpcError) {
  case
    pi_rpc.launch(
      command,
      cwd,
      "scherzo compatibility probe",
      False,
      read_timeout_ms,
    )
  {
    Ok(session) -> {
      case pi_rpc.get_session_stats(session, read_timeout_ms) {
        Ok(#(session, _)) -> {
          let _ = pi_rpc.terminate(session)
          Ok(Nil)
        }
        Error(err) -> {
          let _ = pi_rpc.terminate(session)
          Error(err)
        }
      }
    }
    Error(err) -> Error(err)
  }
}
