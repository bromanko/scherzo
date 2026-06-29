import gleam/int
import gleam/option.{None, Some}
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

pub fn remote_command_router_accepts_decoded_operator_commands_test() {
  let router = remote_command_router.new()
  let #(router, decision) =
    remote_command_router.register(router, "cmd-1", command.ReloadWorkflow)
  assert decision == remote_command_router.StartApply

  let applied = command.applied(command.ReloadWorkflow, Some("reloaded"))
  let router = remote_command_router.complete(router, "cmd-1", applied)
  let #(_, replay) =
    remote_command_router.register(router, "cmd-1", command.ReloadWorkflow)
  assert replay == remote_command_router.ReplayCompleted(applied)
}

pub fn remote_command_router_replays_existing_result_for_malformed_duplicate_test() {
  let router = remote_command_router.new()
  let #(router, _) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  let applied = command.applied(command.PauseDispatch, Some("paused"))
  let router = remote_command_router.complete(router, "cmd-1", applied)
  let rejected = malformed_result("mystery")

  let #(_, replay) =
    remote_command_router.register_rejection(router, "cmd-1", rejected)

  assert replay == remote_command_router.ReplayCompleted(applied)
}

pub fn remote_command_router_replays_malformed_rejection_duplicate_test() {
  let rejected = malformed_result("mystery")
  let #(router, first) =
    remote_command_router.register_rejection(
      remote_command_router.new(),
      "cmd-1",
      rejected,
    )
  let #(_, replay) =
    remote_command_router.register_rejection(router, "cmd-1", rejected)

  assert first == remote_command_router.Reject(rejected)
  assert replay == remote_command_router.ReplayCompleted(rejected)
}

pub fn remote_command_router_rejects_new_command_when_in_flight_limit_full_test() {
  let #(router, first) =
    remote_command_router.register_limited(
      remote_command_router.new(),
      "cmd-1",
      command.PauseDispatch,
      1,
    )
  let #(_, duplicate) =
    remote_command_router.register_limited(
      router,
      "cmd-1",
      command.PauseDispatch,
      1,
    )
  let #(router, overloaded) =
    remote_command_router.register_limited(
      router,
      "cmd-2",
      command.ResumeDispatch,
      1,
    )
  let #(_, overloaded_replay) =
    remote_command_router.register_limited(
      router,
      "cmd-2",
      command.ResumeDispatch,
      1,
    )

  assert first == remote_command_router.StartApply
  assert duplicate == remote_command_router.DuplicateInFlight
  let assert remote_command_router.Reject(result) = overloaded
  assert result.status == command.Rejected("remote_command_overloaded")
  assert overloaded_replay == remote_command_router.ReplayCompleted(result)
}

pub fn remote_command_router_bounds_completed_replay_entries_test() {
  let applied = command.applied(command.PauseDispatch, Some("completed"))
  let count = remote_command_router.completed_replay_limit + 1
  let router = complete_commands(remote_command_router.new(), 1, count, applied)

  let #(_, evicted) =
    remote_command_router.register(router, "cmd-1", command.PauseDispatch)
  let #(_, retained) =
    remote_command_router.register(
      router,
      "cmd-" <> int.to_string(count),
      command.PauseDispatch,
    )

  assert evicted == remote_command_router.StartApply
  assert retained == remote_command_router.ReplayCompleted(applied)
}

fn malformed_result(command_name: String) -> command.CommandResult {
  command.CommandResult(
    command: command_name,
    status: command.Rejected("unknown_command"),
    target: None,
    message: Some("unknown command"),
    operation_id: None,
  )
}

fn complete_commands(
  router: remote_command_router.State,
  current: Int,
  total: Int,
  result: command.CommandResult,
) -> remote_command_router.State {
  case current > total {
    True -> router
    False -> {
      let command_id = "cmd-" <> int.to_string(current)
      let #(router, _) =
        remote_command_router.register(
          router,
          command_id,
          command.PauseDispatch,
        )
      let router = remote_command_router.complete(router, command_id, result)
      complete_commands(router, current + 1, total, result)
    }
  }
}
