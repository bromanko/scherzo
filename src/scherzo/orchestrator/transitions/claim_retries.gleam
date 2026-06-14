import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/task_lifecycle_legacy
import scherzo/orchestrator/transition_types.{type PendingClaim, type State}
import scherzo/runtime/identity.{type TaskIdentity}
import scherzo/runtime/state as orchestrator_state
import scherzo/tracker/issue.{type Issue}

const paused_preflight_defer_ms = 60_000

pub fn reconcile_unstarted_pending_claim(
  state: State,
  task_identity: TaskIdentity,
  pending: PendingClaim,
) -> #(State, List(effects_types.Effect)) {
  let state = clear_pending_claim(state, task_identity)
  case pending.retry_cancellation {
    None -> #(state, [])
    Some(retry_cancellation) ->
      restore_retry_cancellation(
        state,
        task_identity,
        pending.issue,
        retry_cancellation,
        None,
      )
  }
}

pub fn restore_after_pre_claim_failure(
  state: State,
  issue: Issue,
  retry_cancellation: Option(transition_types.RetryCancellation),
) -> #(State, List(effects_types.Effect)) {
  case retry_cancellation {
    None -> #(state, [])
    Some(
      transition_types.RetryCancellation(previous_retry: previous_retry, ..) as retry_cancellation,
    ) ->
      restore_retry_cancellation(
        state,
        orchestrator_state.task_ref_identity(previous_retry.task_ref),
        issue,
        retry_cancellation,
        None,
      )
  }
}

pub fn restore_after_paused_preflight(
  state: State,
  issue: Issue,
  retry_cancellation: Option(transition_types.RetryCancellation),
) -> #(State, List(effects_types.Effect)) {
  case retry_cancellation {
    None -> #(state, [])
    Some(
      transition_types.RetryCancellation(previous_retry: previous_retry, ..) as retry_cancellation,
    ) ->
      restore_retry_cancellation(
        state,
        orchestrator_state.task_ref_identity(previous_retry.task_ref),
        issue,
        retry_cancellation,
        Some(paused_preflight_defer_ms),
      )
  }
}

fn restore_retry_cancellation(
  state: State,
  task_identity: TaskIdentity,
  issue: Issue,
  retry_cancellation: transition_types.RetryCancellation,
  defer_delay_ms: Option(Int),
) -> #(State, List(effects_types.Effect)) {
  let transition_types.RetryCancellation(
    issue_id: retry_issue_id,
    generation: generation,
    previous_retry: previous_retry,
    ..,
  ) = retry_cancellation
  let delay_ms = case defer_delay_ms {
    Some(delay_ms) -> delay_ms
    None -> previous_retry.delay_ms
  }
  let runtime =
    orchestrator_state.RuntimeState(
      ..state.runtime,
      retry_attempts: dict.insert(
        state.runtime.retry_attempts,
        task_identity,
        previous_retry,
      ),
      claimed: dict.insert(
        state.runtime.claimed,
        task_identity,
        issue.identifier,
      ),
    )
  let state = transition_types.State(..state, runtime: runtime) |> sync_state
  #(state, [
    effects_types.DeferRetryTimer(retry_issue_id, generation, delay_ms),
  ])
}

fn clear_pending_claim(state: State, task_identity: TaskIdentity) -> State {
  transition_types.State(
    ..state,
    pending_claims: dict.delete(state.pending_claims, task_identity),
  )
  |> sync_state
}

fn sync_state(state: State) -> State {
  case task_lifecycle_legacy.from_transition_state(state) {
    Ok(directory) -> transition_types.State(..state, lifecycle: directory)
    Error(error) ->
      task_lifecycle_legacy.keep_state_after_projection_error(state, error)
  }
}
