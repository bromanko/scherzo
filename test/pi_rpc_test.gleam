import gleam/option.{Some}
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/path
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

pub fn pi_rpc_facade_forwards_protocol_and_client_helpers_test() {
  assert string.contains(pi_rpc.encode_prompt("1", "hello"), "prompt")
  let assert Ok(record) =
    pi_rpc.decode_record(
      "{\"id\":\"1\",\"type\":\"response\",\"command\":\"get_state\",\"success\":true,\"data\":{\"sessionId\":\"fake\"}}",
    )
  assert record.session_id == Some("fake")

  let cwd = "test/tmp/pi-rpc-facade"
  reset_dir(cwd)
  let assert Ok(session) = pi_rpc.launch(fake_pi(), cwd, "name", False, 1000)
  let assert Ok(#(session, totals)) = pi_rpc.get_session_stats(session, 1000)
  assert totals.total == 3
  let _ = pi_rpc.terminate(session)
}
