import gleam/dict
import scherzo/control/query/types as query_types
import scherzo/orchestrator/query_snapshot_cache
import scherzo/orchestrator/read_model
import scherzo/orchestrator/workflow_reloader
import scherzo/state/projection

pub type State {
  State(
    query_cache: query_snapshot_cache.Handle,
    read_model: read_model.ReadModel,
    ledger_projection: projection.Projection,
  )
}

pub fn initial_snapshot(
  model: read_model.ReadModel,
  projection_state: projection.Projection,
  workflow: workflow_reloader.State,
  sampled_at_ms: Int,
) -> query_snapshot_cache.Snapshot {
  query_snapshot_cache.Snapshot(
    read_model: read_model.snapshot(model, sampled_at_ms: sampled_at_ms),
    projection: projection_state,
    outbox: dict.to_list(projection_state.outbox),
    workflow: workflow,
    dispatch_paused: projection_state.dispatch_paused,
    claims: query_types.ClaimListDto(sampled_at_ms: sampled_at_ms, items: []),
  )
}

pub fn new(
  query_cache: query_snapshot_cache.Handle,
  read_model: read_model.ReadModel,
  ledger_projection: projection.Projection,
) -> State {
  State(
    query_cache: query_cache,
    read_model: read_model,
    ledger_projection: ledger_projection,
  )
}

pub fn query_cache(state: State) -> query_snapshot_cache.Handle {
  state.query_cache
}

pub fn publish(state: State, snapshot: query_snapshot_cache.Snapshot) -> State {
  query_snapshot_cache.update(state.query_cache, snapshot)
  state
}

pub fn stop_cache_best_effort(handle: query_snapshot_cache.Handle) -> Nil {
  case query_snapshot_cache.stop(handle, 1000) {
    Ok(Nil) -> Nil
    Error(Nil) -> Nil
  }
}

pub fn read_model(state: State) -> read_model.ReadModel {
  state.read_model
}

pub fn ledger_projection(state: State) -> projection.Projection {
  state.ledger_projection
}

pub fn set_read_model(state: State, read_model: read_model.ReadModel) -> State {
  State(..state, read_model: read_model)
}

pub fn update_read_model(
  state: State,
  update: fn(read_model.ReadModel) -> read_model.ReadModel,
) -> State {
  set_read_model(state, update(state.read_model))
}

pub fn set_ledger_projection(
  state: State,
  ledger_projection: projection.Projection,
) -> State {
  State(..state, ledger_projection: ledger_projection)
}
