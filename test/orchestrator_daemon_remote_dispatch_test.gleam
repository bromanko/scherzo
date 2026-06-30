import gleam/erlang/process
import scherzo/orchestrator/daemon
import test_async

pub fn get_remote_dispatch_paused_uses_worker_owned_reply_subject_test() {
  let daemon_subject_ready = process.new_subject()
  let caller = process.self()
  let observed_reply_owned_by_caller = process.new_subject()
  let late_reply_barrier = test_async.new_barrier()

  let _ =
    process.spawn_unlinked(fn() {
      let daemon_subject = process.new_subject()
      process.send(daemon_subject_ready, daemon_subject)
      let assert Ok(daemon.GetRemoteDispatchPaused(reply)) =
        process.receive(daemon_subject, within: 1000)
      let assert Ok(owner) = process.subject_owner(reply)
      process.send(observed_reply_owned_by_caller, owner == caller)
      test_async.block_until_released(late_reply_barrier)
      process.send(reply, False)
    })

  let assert Ok(daemon_subject) =
    process.receive(daemon_subject_ready, within: 1000)
  let assert Error(Nil) = daemon.get_remote_dispatch_paused(daemon_subject, 1)
  let assert Ok(False) =
    process.receive(observed_reply_owned_by_caller, within: 1000)
  test_async.release_barrier(late_reply_barrier)
}
