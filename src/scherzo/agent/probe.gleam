import scherzo/error
import scherzo/pi/client

pub fn probe(
  command: String,
  cwd: String,
  read_timeout_ms: Int,
) -> Result(Nil, error.PiRpcError) {
  case
    client.launch(
      command,
      cwd,
      "scherzo compatibility probe",
      False,
      read_timeout_ms,
    )
  {
    Ok(session) -> {
      case client.get_session_stats(session, read_timeout_ms) {
        Ok(#(session, _)) -> {
          let _ = client.terminate(session)
          Ok(Nil)
        }
        Error(err) -> {
          let _ = client.terminate(session)
          Error(err)
        }
      }
    }
    Error(err) -> Error(err)
  }
}
