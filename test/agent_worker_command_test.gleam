import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/worker_command
import scherzo/control/command

pub fn worker_command_variants_carry_reply_subjects_test() {
  let reply = process.new_subject()

  let abort = worker_command.Abort(reply)
  let stop = worker_command.StopAfterCurrentTurn(reply)
  let prompt = worker_command.QueuePrompt("continue", reply)
  let ui = worker_command.RespondToUi("ui-1", command.UiCancel, reply)

  assert abort == worker_command.Abort(reply)
  assert stop == worker_command.StopAfterCurrentTurn(reply)
  assert prompt == worker_command.QueuePrompt("continue", reply)
  assert ui == worker_command.RespondToUi("ui-1", command.UiCancel, reply)
}

pub fn worker_replies_are_separate_from_public_command_results_test() {
  let reply = process.new_subject()
  process.send(reply, worker_command.Applied(Some("ok")))
  process.send(reply, worker_command.Queued(None))
  process.send(reply, worker_command.Rejected("reason", None))
  process.send(reply, worker_command.NotFound(None))
  process.send(reply, worker_command.NotAllowed("reason", None))

  assert process.receive(reply, within: 100)
    == Ok(worker_command.Applied(Some("ok")))
  assert process.receive(reply, within: 100) == Ok(worker_command.Queued(None))
  assert process.receive(reply, within: 100)
    == Ok(worker_command.Rejected("reason", None))
  assert process.receive(reply, within: 100)
    == Ok(worker_command.NotFound(None))
  assert process.receive(reply, within: 100)
    == Ok(worker_command.NotAllowed("reason", None))
}
