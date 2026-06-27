import scherzo/config/types as config_types
import scherzo/control/query/backend as query_backend
import scherzo/control/query/metrics as query_metrics
import scherzo/control/query/outbox as query_outbox
import scherzo/control/query/service as query_service
import scherzo/control/query/types as query_types
import scherzo/control/query/workflow as query_workflow
import scherzo/daemon_identity
import scherzo/orchestrator/read_model
import scherzo/orchestrator/workflow_reloader
import scherzo/state/projection
import scherzo/tracker/adapter

pub fn start(
  effective: config_types.EffectiveConfig,
  identity: daemon_identity.DaemonIdentity,
  tracker_adapter: adapter.TrackerAdapter,
  get_dispatch_paused get_dispatch_paused: fn(Int) -> Result(Bool, Nil),
  get_read_model_snapshot get_read_model_snapshot: fn(Int) ->
    Result(read_model.Snapshot, Nil),
  get_projection_snapshot get_projection_snapshot: fn(Int) ->
    Result(projection.Projection, Nil),
  get_outbox_snapshot get_outbox_snapshot: fn(Int) ->
    Result(List(#(String, projection.OutboxStatus)), Nil),
  get_workflow_snapshot get_workflow_snapshot: fn(Int) ->
    Result(workflow_reloader.State, Nil),
) -> Result(query_service.Handle, query_service.StartError) {
  query_service.start(
    query_service.default_settings(),
    query_service.Backend(run: fn(query) {
      case query {
        query_types.Status ->
          query_metrics.execute_status(get_snapshot: get_read_model_snapshot)
        query_types.Metrics ->
          query_metrics.execute_metrics(get_snapshot: get_read_model_snapshot)
        query_types.TaskList(_)
        | query_types.TaskShow(_)
        | query_types.WorkItemList(_)
        | query_types.WorkItemShow(_) ->
          query_backend.run_with_projection(
            effective,
            identity,
            tracker_adapter,
            get_dispatch_paused,
            get_projection_snapshot,
            query,
          )
        query_types.OutboxList(outbox_query) ->
          query_outbox.execute_list(
            get_outbox: get_outbox_snapshot,
            query: outbox_query,
          )
        query_types.OutboxShow(outbox_query) ->
          query_outbox.execute_show(
            get_outbox: get_outbox_snapshot,
            query: outbox_query,
          )
        query_types.WorkflowList ->
          execute_workflow_query(
            get_snapshot: get_workflow_snapshot,
            run: query_workflow.execute_list,
          )
        query_types.WorkflowDetail(workflow_query) ->
          execute_workflow_query(
            get_snapshot: get_workflow_snapshot,
            run: fn(snapshot) {
              query_workflow.execute_detail(snapshot, workflow_query)
            },
          )
      }
    }),
  )
}

fn execute_workflow_query(
  get_snapshot get_snapshot: fn(Int) -> Result(workflow_reloader.State, Nil),
  run run: fn(workflow_reloader.State) ->
    Result(query_types.QueryResponse, query_types.QueryError),
) -> Result(query_types.QueryResponse, query_types.QueryError) {
  case get_snapshot(100) {
    Ok(snapshot) -> run(snapshot)
    Error(Nil) ->
      Error(query_types.QueryError(
        query_types.QueryTimeout,
        "workflow query timed out",
      ))
  }
}
