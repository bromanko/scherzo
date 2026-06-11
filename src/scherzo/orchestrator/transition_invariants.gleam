import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state

pub type InvariantError {
  InvariantError(code: String, identity: String, message: String)
}

pub fn check(
  state: transition_types.State,
) -> Result(Nil, List(InvariantError)) {
  errors(state)
  |> errors_to_result
}

pub fn check_runtime(
  runtime: orchestrator_state.RuntimeState,
) -> Result(Nil, List(InvariantError)) {
  runtime_errors(runtime)
  |> errors_to_result
}

fn errors(state: transition_types.State) -> List(InvariantError) {
  list.flatten([
    runtime_errors(state.runtime),
    claimed_state_errors(state),
    worker_directory_errors(state),
    pending_claim_errors(state),
    pending_dispatch_validation_errors(state),
    pending_review_lane_preflight_errors(state),
    dispatch_slot_errors(state),
  ])
}

fn runtime_errors(
  runtime: orchestrator_state.RuntimeState,
) -> List(InvariantError) {
  list.flatten([
    running_runtime_errors(runtime),
    retry_runtime_errors(runtime),
    claimed_runtime_errors(runtime),
    parked_runtime_errors(runtime),
  ])
}

pub fn error_code(error: InvariantError) -> String {
  let InvariantError(code: code, ..) = error
  code
}

fn format_error(error: InvariantError) -> String {
  let InvariantError(code: code, identity: identity, message: message) = error
  code <> " [" <> identity <> "]: " <> message
}

pub fn format_errors(errors: List(InvariantError)) -> String {
  errors
  |> list.map(format_error)
  |> string.join(with: "\n")
}

fn errors_to_result(
  errors: List(InvariantError),
) -> Result(Nil, List(InvariantError)) {
  case errors {
    [] -> Ok(Nil)
    _ -> Error(errors)
  }
}

fn running_runtime_errors(
  runtime: orchestrator_state.RuntimeState,
) -> List(InvariantError) {
  runtime.running
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, entry) = pair
    let expected_identity = orchestrator_state.task_identity(entry.task)
    // runtime.completed is a historical/latest-completed cache, so a
    // continuation worker may run for an identity that has completed before.
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "running_key_mismatch",
          task_identity,
          "runtime.running key does not match the running task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        !dict.has_key(runtime.claimed, task_identity),
        invariant(
          "running_claim_missing",
          task_identity,
          "runtime.running entry is missing the corresponding runtime.claimed entry",
        ),
      ),
      when(
        dict.has_key(runtime.parked, task_identity),
        invariant(
          "running_parked_conflict",
          task_identity,
          "task is present in both runtime.running and runtime.parked",
        ),
      ),
      when(
        dict.has_key(runtime.retry_attempts, task_identity),
        invariant(
          "running_retry_conflict",
          task_identity,
          "task is present in both runtime.running and runtime.retry_attempts",
        ),
      ),
    ])
  })
}

fn retry_runtime_errors(
  runtime: orchestrator_state.RuntimeState,
) -> List(InvariantError) {
  runtime.retry_attempts
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, entry) = pair
    let expected_identity = orchestrator_state.task_ref_identity(entry.task_ref)
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "retry_key_mismatch",
          task_identity,
          "runtime.retry_attempts key does not match retry task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        !dict.has_key(runtime.claimed, task_identity),
        invariant(
          "retry_claim_missing",
          task_identity,
          "runtime.retry_attempts entry is missing the corresponding runtime.claimed entry",
        ),
      ),
      when(
        dict.has_key(runtime.running, task_identity),
        invariant(
          "retry_running_conflict",
          task_identity,
          "task is present in both runtime.retry_attempts and runtime.running",
        ),
      ),
      when(
        dict.has_key(runtime.parked, task_identity),
        invariant(
          "retry_parked_conflict",
          task_identity,
          "task is present in both runtime.retry_attempts and runtime.parked",
        ),
      ),
    ])
  })
}

fn claimed_runtime_errors(
  runtime: orchestrator_state.RuntimeState,
) -> List(InvariantError) {
  runtime.claimed
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, identifier) = pair
    list.flatten([
      when(
        string.trim(identifier) == "",
        invariant(
          "claimed_identifier_empty",
          task_identity,
          "runtime.claimed identifier is empty",
        ),
      ),
      when(
        dict.has_key(runtime.parked, task_identity),
        invariant(
          "claimed_parked_conflict",
          task_identity,
          "task is present in both runtime.claimed and runtime.parked",
        ),
      ),
    ])
  })
}

fn parked_runtime_errors(
  runtime: orchestrator_state.RuntimeState,
) -> List(InvariantError) {
  runtime.parked
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, entry) = pair
    let expected_identity = orchestrator_state.task_ref_identity(entry.task_ref)
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "parked_key_mismatch",
          task_identity,
          "runtime.parked key does not match parked task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        dict.has_key(runtime.running, task_identity),
        invariant(
          "parked_running_conflict",
          task_identity,
          "task is present in both runtime.parked and runtime.running",
        ),
      ),
      when(
        dict.has_key(runtime.retry_attempts, task_identity),
        invariant(
          "parked_retry_conflict",
          task_identity,
          "task is present in both runtime.parked and runtime.retry_attempts",
        ),
      ),
    ])
  })
}

fn claimed_state_errors(state: transition_types.State) -> List(InvariantError) {
  state.runtime.claimed
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, _) = pair
    let has_lifecycle_entry =
      dict.has_key(state.runtime.running, task_identity)
      || dict.has_key(state.runtime.retry_attempts, task_identity)
      || dict.has_key(state.pending_claims, task_identity)
      || dict.has_key(state.pending_review_lane_preflights, task_identity)
    when(
      !has_lifecycle_entry,
      invariant(
        "claimed_lifecycle_missing",
        task_identity,
        "runtime.claimed entry is not backed by runtime.running, runtime.retry_attempts, pending_claims, or pending_review_lane_preflights",
      ),
    )
  })
}

fn worker_directory_errors(
  state: transition_types.State,
) -> List(InvariantError) {
  list.flatten([
    running_worker_index_errors(state),
    worker_by_issue_errors(state),
    worker_by_session_errors(state.workers),
    worker_route_errors(state.workers),
    yaml_step_run_errors(state.workers),
    stopped_yaml_run_errors(state.workers),
  ])
}

fn running_worker_index_errors(
  state: transition_types.State,
) -> List(InvariantError) {
  state.runtime.running
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, _) = pair
    case dict.get(state.workers.by_issue, task_identity) {
      Ok(_) -> []
      Error(Nil) -> [
        invariant(
          "running_worker_missing",
          task_identity,
          "runtime.running entry is missing the corresponding workers.by_issue entry",
        ),
      ]
    }
  })
}

fn worker_by_issue_errors(
  state: transition_types.State,
) -> List(InvariantError) {
  state.workers.by_issue
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, entry) = pair
    let expected_identity = worker_entry_identity(entry)
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "worker_issue_key_mismatch",
          task_identity,
          "workers.by_issue key does not match worker task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        entry.issue.id != entry.issue_id,
        invariant(
          "worker_issue_id_mismatch",
          task_identity,
          "worker entry issue_id "
            <> entry.issue_id
            <> " does not match embedded issue id "
            <> entry.issue.id,
        ),
      ),
      worker_runtime_errors(state.runtime, task_identity, entry),
      worker_session_index_errors(state.workers, task_identity, entry),
      worker_route_index_errors(state.workers, task_identity, entry),
    ])
  })
}

fn worker_runtime_errors(
  runtime: orchestrator_state.RuntimeState,
  task_identity: identity.TaskIdentity,
  entry: transition_types.WorkerEntry,
) -> List(InvariantError) {
  case dict.get(runtime.running, task_identity) {
    Error(Nil) -> [
      invariant(
        "worker_running_missing",
        task_identity,
        "workers.by_issue entry is missing the corresponding runtime.running entry",
      ),
    ]
    Ok(running) ->
      list.flatten([
        when(
          running.issue.id != entry.issue_id,
          invariant(
            "worker_running_issue_mismatch",
            task_identity,
            "runtime.running issue id "
              <> running.issue.id
              <> " does not match worker issue id "
              <> entry.issue_id,
          ),
        ),
        when(
          running.workspace_path != entry.workspace_path,
          invariant(
            "worker_running_workspace_mismatch",
            task_identity,
            "runtime.running workspace path does not match worker directory entry",
          ),
        ),
      ])
  }
}

fn worker_session_index_errors(
  workers: transition_types.WorkerDirectory,
  task_identity: identity.TaskIdentity,
  entry: transition_types.WorkerEntry,
) -> List(InvariantError) {
  case dict.get(workers.by_session, entry.session_id) {
    Error(Nil) -> [
      invariant(
        "worker_session_index_missing",
        task_identity,
        "workers.by_session is missing session " <> entry.session_id,
      ),
    ]
    Ok(indexed_identity) ->
      when(
        indexed_identity != task_identity,
        invariant_text(
          "worker_session_index_drift",
          entry.session_id,
          "workers.by_session points at "
            <> identity.to_string(indexed_identity)
            <> " but workers.by_issue has "
            <> identity.to_string(task_identity),
        ),
      )
  }
}

fn worker_route_index_errors(
  workers: transition_types.WorkerDirectory,
  task_identity: identity.TaskIdentity,
  entry: transition_types.WorkerEntry,
) -> List(InvariantError) {
  case dict.get(workers.route_to_session, entry.command_route_id) {
    Error(Nil) -> [
      invariant(
        "worker_route_index_missing",
        task_identity,
        "workers.route_to_session is missing command route "
          <> entry.command_route_id,
      ),
    ]
    Ok(indexed_session_id) ->
      when(
        indexed_session_id != entry.session_id,
        invariant_text(
          "worker_route_index_drift",
          entry.command_route_id,
          "workers.route_to_session points at session "
            <> indexed_session_id
            <> " but worker entry uses session "
            <> entry.session_id,
        ),
      )
  }
}

fn worker_by_session_errors(
  workers: transition_types.WorkerDirectory,
) -> List(InvariantError) {
  workers.by_session
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(session_id, task_identity) = pair
    case dict.get(workers.by_issue, task_identity) {
      Error(Nil) -> [
        invariant_text(
          "worker_session_index_dangling",
          session_id,
          "workers.by_session points at missing worker "
            <> identity.to_string(task_identity),
        ),
      ]
      Ok(entry) ->
        when(
          entry.session_id != session_id,
          invariant_text(
            "worker_session_index_drift",
            session_id,
            "workers.by_session key does not match worker session "
              <> entry.session_id,
          ),
        )
    }
  })
}

fn worker_route_errors(
  workers: transition_types.WorkerDirectory,
) -> List(InvariantError) {
  workers.route_to_session
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(route_id, session_id) = pair
    case dict.get(workers.by_session, session_id) {
      Error(Nil) -> [
        invariant_text(
          "worker_route_index_dangling",
          route_id,
          "workers.route_to_session points at missing session " <> session_id,
        ),
      ]
      Ok(task_identity) ->
        case dict.get(workers.by_issue, task_identity) {
          Error(Nil) -> [
            invariant_text(
              "worker_route_index_dangling",
              route_id,
              "workers.route_to_session points at missing worker "
                <> identity.to_string(task_identity),
            ),
          ]
          Ok(entry) ->
            when(
              entry.command_route_id != route_id,
              invariant_text(
                "worker_route_index_drift",
                route_id,
                "workers.route_to_session key does not match worker command route "
                  <> entry.command_route_id,
              ),
            )
        }
    }
  })
}

fn yaml_step_run_errors(
  workers: transition_types.WorkerDirectory,
) -> List(InvariantError) {
  workers.yaml_step_runs
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(session_id, run_id) = pair
    list.flatten([
      when(
        string.trim(session_id) == "",
        invariant_text(
          "yaml_step_session_empty",
          "workers.yaml_step_runs",
          "workers.yaml_step_runs contains an empty session id",
        ),
      ),
      when(
        string.trim(run_id) == "",
        invariant_text(
          "yaml_step_run_id_empty",
          session_id,
          "workers.yaml_step_runs contains an empty run id",
        ),
      ),
      yaml_step_run_lifecycle_errors(workers, session_id, run_id),
    ])
  })
}

fn yaml_step_run_lifecycle_errors(
  workers: transition_types.WorkerDirectory,
  session_id: String,
  run_id: String,
) -> List(InvariantError) {
  case string.trim(run_id) == "" {
    True -> []
    False ->
      case
        worker_run_exists(workers, run_id)
        || dict.has_key(workers.stopped_yaml_runs, run_id)
      {
        True -> []
        False -> [
          invariant_text(
            "yaml_step_run_dangling",
            session_id,
            "workers.yaml_step_runs points at missing run " <> run_id,
          ),
        ]
      }
  }
}

fn stopped_yaml_run_errors(
  workers: transition_types.WorkerDirectory,
) -> List(InvariantError) {
  workers.stopped_yaml_runs
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(run_id, _) = pair
    list.flatten([
      when(
        string.trim(run_id) == "",
        invariant_text(
          "stopped_yaml_run_id_empty",
          "workers.stopped_yaml_runs",
          "workers.stopped_yaml_runs contains an empty run id",
        ),
      ),
      when(
        string.trim(run_id) != "" && worker_run_exists(workers, run_id),
        invariant_text(
          "stopped_yaml_run_active_worker_conflict",
          run_id,
          "workers.stopped_yaml_runs contains active worker run " <> run_id,
        ),
      ),
    ])
  })
}

fn worker_run_exists(
  workers: transition_types.WorkerDirectory,
  run_id: String,
) -> Bool {
  workers.by_issue
  |> dict.values
  |> list.any(fn(entry) { entry.run_id == run_id })
}

fn pending_claim_errors(state: transition_types.State) -> List(InvariantError) {
  state.pending_claims
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, pending) = pair
    let expected_identity =
      orchestrator_state.task_ref_identity(pending.task_ref)
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "pending_claim_key_mismatch",
          task_identity,
          "pending_claims key does not match pending task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        pending.issue.id != pending.issue_id,
        invariant(
          "pending_claim_issue_mismatch",
          task_identity,
          "pending claim issue_id "
            <> pending.issue_id
            <> " does not match embedded issue id "
            <> pending.issue.id,
        ),
      ),
      when(
        dict.has_key(state.runtime.running, task_identity),
        invariant(
          "pending_claim_running_conflict",
          task_identity,
          "task is present in both pending_claims and runtime.running",
        ),
      ),
      when(
        dict.has_key(state.workers.by_issue, task_identity),
        invariant(
          "pending_claim_worker_conflict",
          task_identity,
          "task is present in both pending_claims and workers.by_issue",
        ),
      ),
      when(
        dict.has_key(state.runtime.retry_attempts, task_identity),
        invariant(
          "pending_claim_retry_conflict",
          task_identity,
          "task is present in both pending_claims and runtime.retry_attempts",
        ),
      ),
      when(
        dict.has_key(state.runtime.parked, task_identity),
        invariant(
          "pending_claim_parked_conflict",
          task_identity,
          "task is present in both pending_claims and runtime.parked",
        ),
      ),
      when(
        dict.has_key(state.workers.by_session, pending.session_id),
        invariant_text(
          "pending_claim_session_conflict",
          pending.session_id,
          "pending claim session id is already indexed in workers.by_session",
        ),
      ),
      when(
        dict.has_key(state.workers.route_to_session, pending.command_route_id),
        invariant_text(
          "pending_claim_route_conflict",
          pending.command_route_id,
          "pending claim command route is already indexed in workers.route_to_session",
        ),
      ),
    ])
  })
}

fn pending_dispatch_validation_errors(
  state: transition_types.State,
) -> List(InvariantError) {
  state.pending_dispatch_validations
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, pending) = pair
    let expected_identity =
      orchestrator_state.task_ref_identity(pending.task_ref)
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "pending_dispatch_validation_key_mismatch",
          task_identity,
          "pending_dispatch_validations key does not match pending task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        pending.issue.id != pending.task_ref.remote_id,
        invariant(
          "pending_dispatch_validation_issue_mismatch",
          task_identity,
          "pending dispatch validation issue id "
            <> pending.issue.id
            <> " does not match task ref remote id "
            <> pending.task_ref.remote_id,
        ),
      ),
      when(
        pending.generation <= 0,
        invariant(
          "pending_dispatch_validation_generation_invalid",
          task_identity,
          "pending dispatch validation generation must be positive",
        ),
      ),
      when(
        pending.generation >= state.next_dispatch_validation_generation,
        invariant(
          "pending_dispatch_validation_generation_unreserved",
          task_identity,
          "pending dispatch validation generation "
            <> int.to_string(pending.generation)
            <> " is not below next_dispatch_validation_generation "
            <> int.to_string(state.next_dispatch_validation_generation),
        ),
      ),
      when(
        dict.has_key(state.runtime.running, task_identity),
        invariant(
          "pending_dispatch_validation_running_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and runtime.running",
        ),
      ),
      when(
        dict.has_key(state.runtime.claimed, task_identity),
        invariant(
          "pending_dispatch_validation_claimed_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and runtime.claimed",
        ),
      ),
      when(
        dict.has_key(state.pending_claims, task_identity),
        invariant(
          "pending_dispatch_validation_pending_claim_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and pending_claims",
        ),
      ),
      when(
        dict.has_key(state.runtime.retry_attempts, task_identity),
        invariant(
          "pending_dispatch_validation_retry_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and runtime.retry_attempts",
        ),
      ),
      when(
        dict.has_key(state.runtime.parked, task_identity),
        invariant(
          "pending_dispatch_validation_parked_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and runtime.parked",
        ),
      ),
      when(
        dict.has_key(state.workers.by_issue, task_identity),
        invariant(
          "pending_dispatch_validation_worker_conflict",
          task_identity,
          "task is present in both pending_dispatch_validations and workers.by_issue",
        ),
      ),
    ])
  })
}

fn pending_review_lane_preflight_errors(
  state: transition_types.State,
) -> List(InvariantError) {
  state.pending_review_lane_preflights
  |> dict.to_list
  |> list.flat_map(fn(pair) {
    let #(task_identity, pending) = pair
    let expected_identity =
      orchestrator_state.task_ref_identity(pending.task_ref)
    // Pending review-lane preflights can belong to a claimed retry refresh, so
    // runtime.claimed is intentionally not treated as a conflict here.
    list.flatten([
      when(
        expected_identity != task_identity,
        invariant(
          "pending_review_lane_preflight_key_mismatch",
          task_identity,
          "pending_review_lane_preflights key does not match pending task ref "
            <> identity.to_string(expected_identity),
        ),
      ),
      when(
        pending.issue.id != pending.task_ref.remote_id,
        invariant(
          "pending_review_lane_preflight_issue_mismatch",
          task_identity,
          "pending review-lane preflight issue id "
            <> pending.issue.id
            <> " does not match task ref remote id "
            <> pending.task_ref.remote_id,
        ),
      ),
      when(
        pending.generation <= 0,
        invariant(
          "pending_review_lane_preflight_generation_invalid",
          task_identity,
          "pending review-lane preflight generation must be positive",
        ),
      ),
      when(
        pending.generation >= state.next_dispatch_validation_generation,
        invariant(
          "pending_review_lane_preflight_generation_unreserved",
          task_identity,
          "pending review-lane preflight generation "
            <> int.to_string(pending.generation)
            <> " is not below next_dispatch_validation_generation "
            <> int.to_string(state.next_dispatch_validation_generation),
        ),
      ),
      when(
        dict.has_key(state.runtime.running, task_identity),
        invariant(
          "pending_review_lane_preflight_running_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and runtime.running",
        ),
      ),
      when(
        dict.has_key(state.pending_claims, task_identity),
        invariant(
          "pending_review_lane_preflight_pending_claim_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and pending_claims",
        ),
      ),
      when(
        dict.has_key(state.pending_dispatch_validations, task_identity),
        invariant(
          "pending_review_lane_preflight_dispatch_validation_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and pending_dispatch_validations",
        ),
      ),
      when(
        dict.has_key(state.runtime.retry_attempts, task_identity),
        invariant(
          "pending_review_lane_preflight_retry_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and runtime.retry_attempts",
        ),
      ),
      when(
        dict.has_key(state.runtime.parked, task_identity),
        invariant(
          "pending_review_lane_preflight_parked_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and runtime.parked",
        ),
      ),
      when(
        dict.has_key(state.workers.by_issue, task_identity),
        invariant(
          "pending_review_lane_preflight_worker_conflict",
          task_identity,
          "task is present in both pending_review_lane_preflights and workers.by_issue",
        ),
      ),
    ])
  })
}

fn dispatch_slot_errors(state: transition_types.State) -> List(InvariantError) {
  let pending_slots =
    dict.size(state.pending_claims)
    + dict.size(state.pending_dispatch_validations)
    + dict.size(state.pending_review_lane_preflights)
  let running_slots = dict.size(state.runtime.running)
  let max_slots = state.runtime.max_concurrent_agents
  case max_slots <= 0 {
    True ->
      list.flatten([
        when(
          pending_slots > 0,
          invariant_text(
            "pending_slot_overcommit",
            "dispatch_slots",
            "pending dispatch reservations exist while max_concurrent_agents is "
              <> int.to_string(max_slots),
          ),
        ),
        when(
          running_slots > 0,
          invariant_text(
            "dispatch_slot_overcommit",
            "dispatch_slots",
            "running workers exist while max_concurrent_agents is "
              <> int.to_string(max_slots),
          ),
        ),
      ])
    False ->
      list.flatten([
        when(
          pending_slots > max_slots,
          invariant_text(
            "pending_slot_overcommit",
            "dispatch_slots",
            "pending dispatch reservations "
              <> int.to_string(pending_slots)
              <> " exceed max_concurrent_agents "
              <> int.to_string(max_slots),
          ),
        ),
        when(
          pending_slots + running_slots > max_slots,
          invariant_text(
            "dispatch_slot_overcommit",
            "dispatch_slots",
            "running plus pending dispatch slots "
              <> int.to_string(pending_slots + running_slots)
              <> " exceed max_concurrent_agents "
              <> int.to_string(max_slots),
          ),
        ),
      ])
  }
}

fn worker_entry_identity(
  entry: transition_types.WorkerEntry,
) -> identity.TaskIdentity {
  orchestrator_state.task_ref_identity(entry.task_ref)
}

fn invariant(
  code: String,
  task_identity: identity.TaskIdentity,
  message: String,
) -> InvariantError {
  InvariantError(
    code: code,
    identity: identity.to_string(task_identity),
    message: message,
  )
}

fn invariant_text(
  code: String,
  identity_text: String,
  message: String,
) -> InvariantError {
  InvariantError(code: code, identity: identity_text, message: message)
}

fn when(condition: Bool, error: InvariantError) -> List(InvariantError) {
  case condition {
    True -> [error]
    False -> []
  }
}
