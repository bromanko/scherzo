import gleam/option.{Some}
import scherzo/control/command
import scherzo/control/remote_command_router

pub fn remote_command_router_registers_pause_before_apply_test() {
  let router = remote_command_router.new()
  let #(router, decision) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)

  assert decision == remote_command_router.StartApply

  let #(_, duplicate) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  assert duplicate == remote_command_router.DuplicateInFlight
}

pub fn remote_command_router_replays_completed_duplicate_result_test() {
  let router = remote_command_router.new()
  let #(router, _) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  let applied = command.applied(command.PauseDispatch, Some("paused"))
  let router = remote_command_router.complete(router, "cmd-1", applied)

  let #(_, replay) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  assert replay == remote_command_router.ReplayCompleted(applied)
}

pub fn remote_command_router_tracks_in_flight_duplicate_without_second_apply_test() {
  let router = remote_command_router.new()
  let #(router, first) =
    remote_command_router.register(router, "cmd-1", command.ResumeDispatch)
  let #(_, duplicate) =
    remote_command_router.register(router, "cmd-1", command.ResumeDispatch)

  assert first == remote_command_router.StartApply
  assert duplicate == remote_command_router.DuplicateInFlight
}

pub fn remote_command_router_rejects_conflicting_duplicate_before_apply_test() {
  let router = remote_command_router.new()
  let #(router, _) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  let #(_, decision) =
    remote_command_router.register(router, "cmd-1", command.ResumeDispatch)

  let assert remote_command_router.Reject(result) = decision
  assert result.command == "resume"
  assert result.status == command.Rejected("remote_command_id_conflict")
}

pub fn remote_command_router_rejects_unsupported_operator_commands_test() {
  let router = remote_command_router.new()
  let #(_, decision) =
    remote_command_router.register(router, "cmd-1", command.ReloadWorkflow)

  let assert remote_command_router.Reject(result) = decision
  assert result.command == "reload"
  assert result.status == command.Rejected("unsupported_remote_command")
}
