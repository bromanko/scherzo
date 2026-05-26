import gleam/option.{Some}
import gleam/string
import scherzo/error
import scherzo/path
import scherzo/pi/client
import scherzo/pi/command as pi_command
import support/test_helpers

pub fn real_pi_session_file_can_be_reopened_from_recorded_workspace_test() {
  let cwd = "test/tmp/real-pi-session-validation"
  test_helpers.reset_dir(cwd)
  let assert Ok(abs_cwd) = path.absolute(cwd)
  let fresh_spec = pi_command.ArgvLaunch("pi", ["--mode", "rpc"], [])
  let assert Ok(session) =
    client.launch_spec(
      fresh_spec,
      abs_cwd,
      "Scherzo real pi session validation",
      False,
      120_000,
    )
  let assert Some(session_file) = session.session_file
  assert string.trim(session_file) != ""

  let assert Ok(#(session, _)) =
    client.send_prompt(
      session,
      "Scherzo validation: confirm this pi RPC session can receive one message. Reply briefly.",
      120_000,
    )
  let assert Ok(session) = collect_agent_end(session, 200)
  let _ = client.terminate(session)

  let continue_spec =
    pi_command.ArgvLaunch(
      "pi",
      ["--mode", "rpc", "--session", session_file],
      [],
    )
  let assert Ok(reopened) =
    client.reopen_session_for_continuation(
      continue_spec,
      abs_cwd,
      session_file,
      120_000,
    )
  let assert Ok(#(reopened, _)) =
    client.send_prompt(
      reopened,
      "Scherzo validation recovery prompt: continue from the reopened session and reply briefly.",
      120_000,
    )
  let assert Ok(reopened) = collect_agent_end(reopened, 200)
  let _ = client.terminate(reopened)
  Nil
}

fn collect_agent_end(
  session: client.Session,
  remaining: Int,
) -> Result(client.Session, error.PiRpcError) {
  case remaining <= 0 {
    True -> Error(error.PiProtocolError("real pi validation agent_end missing"))
    False -> {
      use pair <- try_pi(client.read_turn_record(
        session,
        120_000,
        9_999_999_999,
        9_999_999_999,
      ))
      let #(session, maybe_record) = pair
      case maybe_record {
        Some(record) ->
          case record.type_ == "agent_end" {
            True -> Ok(session)
            False -> collect_agent_end(session, remaining - 1)
          }
        _ -> collect_agent_end(session, remaining - 1)
      }
    }
  }
}

fn try_pi(
  result: Result(a, error.PiRpcError),
  next: fn(a) -> Result(b, error.PiRpcError),
) -> Result(b, error.PiRpcError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}
