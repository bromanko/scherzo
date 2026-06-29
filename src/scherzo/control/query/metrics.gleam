import scherzo/control/query/dto
import scherzo/control/query/types
import scherzo/orchestrator/read_model

const snapshot_timeout_ms = 1000

pub fn execute_status(
  get_snapshot get_snapshot: fn(Int) -> Result(read_model.Snapshot, Nil),
) -> Result(types.QueryResponse, types.QueryError) {
  case get_snapshot(snapshot_timeout_ms) {
    Ok(snapshot) ->
      Ok(types.StatusResponse(
        snapshot |> read_model.status_source |> dto.status_from_source,
      ))
    Error(Nil) -> timeout_error("daemon status query timed out")
  }
}

pub fn execute_metrics(
  get_snapshot get_snapshot: fn(Int) -> Result(read_model.Snapshot, Nil),
) -> Result(types.QueryResponse, types.QueryError) {
  case get_snapshot(snapshot_timeout_ms) {
    Ok(snapshot) ->
      Ok(types.MetricsResponse(
        snapshot
        |> read_model.metrics_source
        |> dto.operational_metrics_from_source,
      ))
    Error(Nil) -> timeout_error("daemon metrics query timed out")
  }
}

fn timeout_error(
  message: String,
) -> Result(types.QueryResponse, types.QueryError) {
  Error(types.QueryError(types.QueryTimeout, message))
}
