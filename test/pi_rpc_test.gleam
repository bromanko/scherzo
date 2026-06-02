import gleam/option.{Some}
import gleam/string
import scherzo/path
import scherzo/pi/client
import scherzo/pi/protocol
import support/test_helpers

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

pub fn pi_protocol_and_client_helpers_work_together_test() {
  assert string.contains(protocol.encode_prompt("1", "hello"), "prompt")
  let assert Ok(record) =
    protocol.decode_record(
      "{\"id\":\"1\",\"type\":\"response\",\"command\":\"get_state\",\"success\":true,\"data\":{\"sessionId\":\"fake\"}}",
    )
  assert record.session_id == Some("fake")

  let cwd = "test/tmp/pi-rpc-facade"
  test_helpers.reset_dir(cwd)
  let assert Ok(session) = client.launch(fake_pi(), cwd, "name", False, 1000)
  let assert Ok(#(session, totals)) = client.get_session_stats(session, 1000)
  assert totals.total == 3
  let _ = client.terminate(session)
}
