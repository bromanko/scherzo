import gleam/dict
import gleam/int
import gleam/list
import scherzo/log
import scherzo/orchestrator/retry_scheduler
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition_types
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type LifecycleError =
  task_lifecycle.LifecycleError

pub type IssueStateKey =
  issue_state.IssueStateKey

const fail_closed_pending_count = 1_000_000_000

pub fn error_code(error: task_lifecycle.LifecycleError) -> String {
  task_lifecycle.error_code(error)
}

pub fn error_fields(error: task_lifecycle.LifecycleError) -> List(log.Field) {
  let base = [
    #("error_code", error_code(error)),
    #("error_variant", error_code(error)),
  ]
  case error {
    task_lifecycle.ConflictingLifecycleSources(
      task_identity,
      existing,
      incoming,
    ) -> [
      #(
        "task_identity",
        orchestrator_state.task_identity_to_string(task_identity),
      ),
      #("existing_lifecycle", lifecycle_label(existing)),
      #("incoming_lifecycle", lifecycle_label(incoming)),
      ..base
    ]
    task_lifecycle.MissingClaimedLifecycle(task_identity) -> [
      #(
        "task_identity",
        orchestrator_state.task_identity_to_string(task_identity),
      ),
      ..base
    ]
    task_lifecycle.MissingRetryWaitingForRefresh(task_identity, generation) -> [
      #(
        "task_identity",
        orchestrator_state.task_identity_to_string(task_identity),
      ),
      #("generation", int.to_string(generation)),
      ..base
    ]
    task_lifecycle.RunningWorkerMismatch(task_identity) -> [
      #(
        "task_identity",
        orchestrator_state.task_identity_to_string(task_identity),
      ),
      ..base
    ]
  }
}

pub fn has_dispatch_blocker(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case projected_lifecycle(state) {
    Ok(directory) ->
      task_lifecycle.has_dispatch_blocker(directory, task_identity)
    Error(error) -> fail_closed_bool(error)
  }
}

pub fn has_tracker_claim(
  state: transition_types.State,
  task_identity: identity.TaskIdentity,
) -> Bool {
  case projected_lifecycle(state) {
    Ok(directory) -> task_lifecycle.has_tracker_claim(directory, task_identity)
    Error(error) -> fail_closed_bool(error)
  }
}

pub fn pending_count_for_state(
  state: transition_types.State,
  normalized_state: issue_state.IssueStateKey,
) -> Int {
  case projected_lifecycle(state) {
    Error(error) -> fail_closed_pending_count_for_error(error)
    Ok(directory) ->
      directory
      |> task_lifecycle.entries
      |> list.filter(fn(entry) {
        let #(_, lifecycle) = entry
        case lifecycle {
          task_lifecycle.Claiming(issue: issue, ..)
          | task_lifecycle.Validating(issue: issue, ..)
          | task_lifecycle.Starting(issue: issue, ..)
          | task_lifecycle.Running(issue: issue, ..)
          | task_lifecycle.Stopping(issue: issue, ..) ->
            issue_state.key(issue.state) == normalized_state
          _ -> False
        }
      })
      |> list.length
  }
}

pub fn keep_state_after_projection_error(
  state: transition_types.State,
  error: task_lifecycle.LifecycleError,
) -> transition_types.State {
  case error {
    task_lifecycle.ConflictingLifecycleSources(_, _, _)
    | task_lifecycle.MissingClaimedLifecycle(_)
    | task_lifecycle.MissingRetryWaitingForRefresh(_, _)
    | task_lifecycle.RunningWorkerMismatch(_) -> state
  }
}

fn fail_closed_bool(error: task_lifecycle.LifecycleError) -> Bool {
  case error {
    task_lifecycle.ConflictingLifecycleSources(_, _, _)
    | task_lifecycle.MissingClaimedLifecycle(_)
    | task_lifecycle.MissingRetryWaitingForRefresh(_, _)
    | task_lifecycle.RunningWorkerMismatch(_) -> True
  }
}

fn fail_closed_pending_count_for_error(
  error: task_lifecycle.LifecycleError,
) -> Int {
  case error {
    task_lifecycle.ConflictingLifecycleSources(_, _, _)
    | task_lifecycle.MissingClaimedLifecycle(_)
    | task_lifecycle.MissingRetryWaitingForRefresh(_, _)
    | task_lifecycle.RunningWorkerMismatch(_) -> fail_closed_pending_count
  }
}

pub fn from_legacy_state(
  state: transition_types.State,
  retries: retry_scheduler.State(timer),
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  from_transition_sources(state, retry_scheduler.refresh_generations(retries))
}

pub fn from_transition_state(
  state: transition_types.State,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  from_transition_sources(state, dict.to_list(state.retry_refresh_generations))
}

fn from_transition_sources(
  state: transition_types.State,
  refresh_generations: List(#(identity.TaskIdentity, Int)),
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case ensure_running_matches_workers(state) {
    Error(error) -> Error(error)
    Ok(Nil) -> {
      let retry_entries = dict.to_list(state.runtime.retry_attempts)
      let refresh_generations =
        normalize_refresh_generations(refresh_generations, retry_entries)
      case
        insert_worker_entries(
          dict.to_list(state.workers.by_issue),
          task_lifecycle.new(),
        )
      {
        Error(error) -> Error(error)
        Ok(directory) ->
          case
            insert_pending_dispatch_validation_entries(
              dict.to_list(state.pending_dispatch_validations),
              directory,
            )
          {
            Error(error) -> Error(error)
            Ok(directory) ->
              case
                insert_pending_claim_entries(
                  dict.to_list(state.pending_claims),
                  directory,
                )
              {
                Error(error) -> Error(error)
                Ok(directory) ->
                  case
                    insert_retry_entries(
                      retry_entries,
                      directory,
                      refresh_generations,
                    )
                  {
                    Error(error) -> Error(error)
                    Ok(directory) ->
                      case
                        insert_parked_entries(
                          dict.to_list(state.runtime.parked),
                          directory,
                        )
                      {
                        Error(error) -> Error(error)
                        Ok(directory) ->
                          case
                            insert_completed_entries(
                              dict.to_list(state.runtime.completed),
                              directory,
                            )
                          {
                            Error(error) -> Error(error)
                            Ok(directory) ->
                              case
                                ensure_refresh_entries(
                                  refresh_generations,
                                  directory,
                                )
                              {
                                Error(error) -> Error(error)
                                Ok(directory) ->
                                  ensure_claimed_entries(
                                    dict.keys(state.runtime.claimed),
                                    directory,
                                  )
                              }
                          }
                      }
                  }
              }
          }
      }
    }
  }
}

fn normalize_refresh_generations(
  refresh_generations: List(#(identity.TaskIdentity, Int)),
  retry_entries: List(#(identity.TaskIdentity, orchestrator_state.RetryEntry)),
) -> List(#(identity.TaskIdentity, Int)) {
  let retry_identity_index = retry_refresh_identity_index(retry_entries)
  list.map(refresh_generations, fn(refresh_entry) {
    let #(refresh_identity, generation) = refresh_entry
    case dict.get(retry_identity_index, refresh_identity) {
      Ok(retry_identity) -> #(retry_identity, generation)
      Error(Nil) -> refresh_entry
    }
  })
}

fn retry_refresh_identity_index(
  retry_entries: List(#(identity.TaskIdentity, orchestrator_state.RetryEntry)),
) -> dict.Dict(identity.TaskIdentity, identity.TaskIdentity) {
  list.fold(retry_entries, dict.new(), fn(index, retry_entry) {
    let #(retry_identity, retry) = retry_entry
    index
    |> dict.insert(retry_identity, retry_identity)
    |> dict.insert(
      orchestrator_state.linear_issue_id_identity(retry.issue_id),
      retry_identity,
    )
  })
}

fn ensure_running_matches_workers(
  state: transition_types.State,
) -> Result(Nil, task_lifecycle.LifecycleError) {
  let runtime_pairs = dict.to_list(state.runtime.running)
  let worker_pairs = dict.to_list(state.workers.by_issue)
  case
    list.all(runtime_pairs, fn(pair) {
      let #(task_identity, _) = pair
      dict.has_key(state.workers.by_issue, task_identity)
    })
    && list.all(worker_pairs, fn(pair) {
      let #(task_identity, _) = pair
      dict.has_key(state.runtime.running, task_identity)
    })
  {
    True -> Ok(Nil)
    False ->
      case list.first(runtime_pairs) {
        Ok(#(task_identity, _)) ->
          Error(task_lifecycle.RunningWorkerMismatch(task_identity))
        Error(Nil) ->
          case list.first(worker_pairs) {
            Ok(#(task_identity, _)) ->
              Error(task_lifecycle.RunningWorkerMismatch(task_identity))
            Error(Nil) -> Ok(Nil)
          }
      }
  }
}

fn insert_worker_entries(
  entries: List(#(identity.TaskIdentity, transition_types.WorkerEntry)),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, worker), ..rest] -> {
      let lifecycle = case worker.status {
        transition_types.WorkerStarting ->
          task_lifecycle.Starting(
            task_ref: worker.task_ref,
            issue: worker.issue,
            run_id: worker.run_id,
            session_id: worker.session_id,
            workspace_path: worker.workspace_path,
          )
        transition_types.WorkerRunning ->
          task_lifecycle.Running(
            task_ref: worker.task_ref,
            issue: worker.issue,
            run_id: worker.run_id,
            session_id: worker.session_id,
            workspace_path: worker.workspace_path,
          )
        transition_types.WorkerStopping(reason) ->
          task_lifecycle.Stopping(
            task_ref: worker.task_ref,
            issue: worker.issue,
            run_id: worker.run_id,
            session_id: worker.session_id,
            workspace_path: worker.workspace_path,
            reason: reason,
          )
        transition_types.WorkerFinishedStatus ->
          task_lifecycle.Completed(
            task_ref: worker.task_ref,
            issue: worker.issue,
          )
      }
      case task_lifecycle.put(directory, lifecycle) {
        Error(error) -> Error(error)
        Ok(next) -> insert_worker_entries(rest, next)
      }
    }
  }
}

fn insert_pending_dispatch_validation_entries(
  entries: List(
    #(identity.TaskIdentity, transition_types.PendingDispatchValidation),
  ),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, pending), ..rest] ->
      case
        task_lifecycle.put(
          directory,
          task_lifecycle.Validating(
            task_ref: pending.task_ref,
            issue: pending.issue,
            generation: pending.generation,
          ),
        )
      {
        Error(error) -> Error(error)
        Ok(next) -> insert_pending_dispatch_validation_entries(rest, next)
      }
  }
}

fn insert_pending_claim_entries(
  entries: List(#(identity.TaskIdentity, transition_types.PendingClaim)),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, pending), ..rest] ->
      case
        task_lifecycle.put(
          directory,
          task_lifecycle.Claiming(
            task_ref: pending.task_ref,
            issue: pending.issue,
            run_id: pending.run_id,
            session_id: pending.session_id,
          ),
        )
      {
        Error(error) -> Error(error)
        Ok(next) -> insert_pending_claim_entries(rest, next)
      }
  }
}

fn insert_retry_entries(
  entries: List(#(identity.TaskIdentity, orchestrator_state.RetryEntry)),
  directory: task_lifecycle.TaskDirectory,
  refresh_generations: List(#(identity.TaskIdentity, Int)),
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, retry), ..rest] -> {
      let task_identity = orchestrator_state.task_ref_identity(retry.task_ref)
      let lifecycle = case
        list.find(refresh_generations, fn(entry) {
          let #(refresh_task_identity, _) = entry
          refresh_task_identity == task_identity
        })
      {
        Ok(_) ->
          task_lifecycle.RetryRefreshing(
            task_ref: retry.task_ref,
            issue_id: retry.issue_id,
            generation: retry.timer_generation,
            delay_ms: retry.delay_ms,
          )
        Error(Nil) ->
          task_lifecycle.RetryWaiting(
            task_ref: retry.task_ref,
            issue_id: retry.issue_id,
            generation: retry.timer_generation,
            delay_ms: retry.delay_ms,
          )
      }
      case task_lifecycle.put(directory, lifecycle) {
        Error(error) -> Error(error)
        Ok(next) -> insert_retry_entries(rest, next, refresh_generations)
      }
    }
  }
}

fn insert_parked_entries(
  entries: List(#(identity.TaskIdentity, orchestrator_state.ParkedEntry)),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, parked), ..rest] ->
      case
        task_lifecycle.put(
          directory,
          task_lifecycle.Parked(
            task_ref: parked.task_ref,
            issue_id: parked.issue_id,
            identifier: parked.identifier,
          ),
        )
      {
        Error(error) -> Error(error)
        Ok(next) -> insert_parked_entries(rest, next)
      }
  }
}

fn insert_completed_entries(
  entries: List(#(identity.TaskIdentity, tracker_issue.Issue)),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(_, issue), ..rest] ->
      case
        task_lifecycle.put(
          directory,
          task_lifecycle.Completed(
            task_ref: orchestrator_state.issue_ref(issue),
            issue: issue,
          ),
        )
      {
        Error(error) -> Error(error)
        Ok(next) -> insert_completed_entries(rest, next)
      }
  }
}

fn ensure_refresh_entries(
  entries: List(#(identity.TaskIdentity, Int)),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case entries {
    [] -> Ok(directory)
    [#(task_identity, generation), ..rest] ->
      case task_lifecycle.get(directory, task_identity) {
        Ok(lifecycle) ->
          case task_lifecycle.is_retry_refreshing(lifecycle) {
            True -> ensure_refresh_entries(rest, directory)
            False ->
              Error(task_lifecycle.MissingRetryWaitingForRefresh(
                task_identity,
                generation,
              ))
          }
        Error(Nil) ->
          Error(task_lifecycle.MissingRetryWaitingForRefresh(
            task_identity,
            generation,
          ))
      }
  }
}

fn projected_lifecycle(
  state: transition_types.State,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  from_transition_state(state)
}

fn lifecycle_label(lifecycle: task_lifecycle.TaskLifecycle) -> String {
  case lifecycle {
    task_lifecycle.Idle(_) -> "idle"
    task_lifecycle.Validating(_, _, _) -> "validating"
    task_lifecycle.Claiming(_, _, _, _) -> "claiming"
    task_lifecycle.Starting(_, _, _, _, _) -> "starting"
    task_lifecycle.Running(_, _, _, _, _) -> "running"
    task_lifecycle.RetryWaiting(_, _, _, _) -> "retry_waiting"
    task_lifecycle.RetryRefreshing(_, _, _, _) -> "retry_refreshing"
    task_lifecycle.Stopping(_, _, _, _, _, _) -> "stopping"
    task_lifecycle.Parked(_, _, _) -> "parked"
    task_lifecycle.Completed(_, _) -> "completed"
  }
}

fn ensure_claimed_entries(
  claimed: List(identity.TaskIdentity),
  directory: task_lifecycle.TaskDirectory,
) -> Result(task_lifecycle.TaskDirectory, task_lifecycle.LifecycleError) {
  case claimed {
    [] -> Ok(directory)
    [task_identity, ..rest] ->
      case task_lifecycle.get(directory, task_identity) {
        Ok(lifecycle) ->
          case task_lifecycle.holds_tracker_claim(lifecycle) {
            True -> ensure_claimed_entries(rest, directory)
            False ->
              Error(task_lifecycle.MissingClaimedLifecycle(task_identity))
          }
        Error(Nil) ->
          Error(task_lifecycle.MissingClaimedLifecycle(task_identity))
      }
  }
}
