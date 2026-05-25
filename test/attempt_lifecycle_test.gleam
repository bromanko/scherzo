import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/attempt_lifecycle
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/pi/client
import scherzo/port
import scherzo/state/artifact_store
import simplifile

pub fn artifact_write_failure_emits_context_recovery_update_test() {
  let updates = process.new_subject()

  attempt_lifecycle.artifact_write(
    "ABC-123",
    "attempt-2-result.json",
    Error(artifact_store.ArtifactIo("disk full")),
    fn(issue_id, update) { process.send(updates, #(issue_id, update)) },
  )

  let assert Ok(#("ABC-123", agent_types.RunnerPiUpdate(update))) =
    process.receive(updates, within: 1000)
  assert update.event == pi_event.ContextRecoveryArtifactWriteFailed
  let assert Some(message) = update.message
  assert string.contains(message, "attempt-2-result.json")
  assert string.contains(message, "artifact_io: disk full")
}

pub fn terminate_failure_emits_pi_terminate_failed_update_test() {
  let cwd = "test/tmp/attempt-lifecycle-terminate-failed"
  let _ = simplifile.delete(cwd)
  let assert Ok(Nil) = simplifile.create_directory_all(cwd)
  let assert Ok(port_process) = port.start("sleep 60", cwd)
  let assert Ok(temp_dir) = port.temp_dir_for_test(port_process)
  let assert Ok(Nil) = simplifile.write(temp_dir <> "/sticky", "x")
  chmod_dir("a-w", temp_dir)

  let updates = process.new_subject()
  let session =
    client.Session(
      process: port_process,
      command: "sleep 60",
      cwd: cwd,
      session_id: None,
      session_file: None,
      reported_cwd: None,
      next_id: 1,
    )
  attempt_lifecycle.terminate("ABC-123", session, fn(issue_id, update) {
    process.send(updates, #(issue_id, update))
  })

  chmod_dir("u+w", temp_dir)
  let _ = client.terminate(session)
  let _ = simplifile.delete(temp_dir)

  let assert Ok(#("ABC-123", agent_types.RunnerPiUpdate(update))) =
    process.receive(updates, within: 1000)
  assert update.event == pi_event.PiTerminateFailed
  let assert Some(message) = update.message
  assert string.contains(message, "cleanup failed")
}

fn chmod_dir(mode: String, path: String) -> Nil {
  let assert Ok(chmod) = port.start_argv("chmod", [mode, path], ".", [])
  let assert Ok(0) = port.await_exit(chmod, 1000)
  Nil
}
