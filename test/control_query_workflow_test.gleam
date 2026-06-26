import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/query/service as query_service
import scherzo/control/query/types
import scherzo/control/query/workflow as query_workflow
import scherzo/daemon_identity
import scherzo/orchestrator/query_runtime
import scherzo/orchestrator/read_model
import scherzo/orchestrator/workflow_reloader
import scherzo/runtime_bundle
import scherzo/state/projection
import scherzo/tracker/adapter
import simplifile
import support/test_helpers

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
}

fn write_project(dir: String) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: build\n    kind: command\n    run: echo build\n  - id: ship\n    kind: command\n    depends_on: [build]\n    run: echo ship\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/review.yaml",
      "version: 1\nid: review\nsteps:\n  - id: review\n    kind: command\n    run: echo review\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n  implementation: workflows/implementation.yaml\n  review: workflows/review.yaml\n",
    )
  Nil
}

fn state_for(dir: String) -> workflow_reloader.State {
  write_project(dir)
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  workflow_reloader.from_bundle(Some(dir <> "/scherzo.yaml"), bundle)
}

pub fn workflow_inventory_lists_multiple_loaded_workflows_test() {
  let state = state_for("test/tmp/control-query-workflow-inventory")

  let inventory = query_workflow.workflow_list_from_state(state)

  assert inventory.schema_version == types.workflow_query_schema_version
  assert inventory.freshness.reload_status == "valid"
  assert string.length(inventory.freshness.source_hash) == 64
  assert inventory.diagnostics == []
  assert list.map(inventory.workflows, fn(workflow) { workflow.id })
    == ["implementation", "review"]

  let assert [implementation, review] = inventory.workflows
  assert implementation.label == Some("workflow:implementation")
  assert implementation.yaml_paths
    == ["scherzo.yaml", "workflows/implementation.yaml"]
  assert implementation.step_count == 2
  assert review.yaml_paths == ["scherzo.yaml", "workflows/review.yaml"]
}

pub fn workflow_detail_includes_yaml_sources_and_graph_test() {
  let state = state_for("test/tmp/control-query-workflow-detail")

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "implementation"),
    )

  assert detail.summary.id == "implementation"
  assert detail.summary.name == "implementation"
  assert detail.summary.route == Some("implementation")
  assert detail.summary.label == Some("workflow:implementation")
  assert detail.freshness.reload_status == "valid"

  let assert [config_source, workflow_source] = detail.yaml_sources
  assert config_source.path == "scherzo.yaml"
  assert string.contains(config_source.contents, "workflows:")
  assert config_source.contents_truncated == False
  assert string.length(config_source.contents_sha256) == 64
  assert workflow_source.path == "workflows/implementation.yaml"
  assert string.contains(workflow_source.contents, "depends_on: [build]")
  assert workflow_source.contents_truncated == False

  assert list.map(detail.graph.nodes, fn(node) { node.id }) == ["build", "ship"]
  assert list.map(detail.graph.nodes, fn(node) { node.kind })
    == ["command", "command"]
  let assert [edge] = detail.graph.edges
  assert edge.from == "build"
  assert edge.to == "ship"
}

pub fn workflow_detail_redacts_sensitive_yaml_keys_and_caps_contents_test() {
  let state = state_for("test/tmp/control-query-workflow-safe-contents")
  let redacted_config =
    "version: 1\ntracker:\n  api_key: literal-secret\n"
    <> string.repeat("x", times: query_workflow.max_yaml_source_contents_chars)
    <> "overflow"
  let dependencies =
    state.bundle.dependencies
    |> list.map(fn(dependency) {
      case dependency.path == state.bundle.config_path {
        True ->
          runtime_bundle.BundleDependency(
            path: dependency.path,
            contents: redacted_config,
          )
        False -> dependency
      }
    })
  let bundle =
    runtime_bundle.RuntimeBundle(..state.bundle, dependencies: dependencies)
  let state = workflow_reloader.State(..state, bundle: bundle)

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "implementation"),
    )

  let assert [config_source, _] = detail.yaml_sources
  assert string.contains(config_source.contents, "api_key: [REDACTED]")
  assert !string.contains(config_source.contents, "literal-secret")
  assert config_source.contents_truncated == True
  assert string.length(config_source.contents)
    == query_workflow.max_yaml_source_contents_chars
  assert string.length(config_source.contents_sha256) == 64
}

pub fn workflow_queries_execute_through_query_runtime_test() {
  let state = state_for("test/tmp/control-query-workflow-runtime")
  let handle = start_query_runtime(state, fn(_) { Ok(state) })

  let assert Ok(types.WorkflowListResponse(inventory)) =
    query_service.query(handle, types.WorkflowList)
  assert list.map(inventory.workflows, fn(workflow) { workflow.id })
    == ["implementation", "review"]

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_service.query(
      handle,
      types.WorkflowDetail(types.WorkflowDetailQuery(
        workflow_id: "implementation",
      )),
    )
  assert detail.summary.id == "implementation"
  let assert [_, workflow_source] = detail.yaml_sources
  assert string.contains(workflow_source.contents, "depends_on: [build]")

  assert query_service.stop(handle, 1000) == Ok(Nil)
}

pub fn workflow_query_runtime_maps_snapshot_timeout_test() {
  let state = state_for("test/tmp/control-query-workflow-timeout")
  let handle = start_query_runtime(state, fn(_) { Error(Nil) })

  let assert Error(types.QueryError(code: code, message: message)) =
    query_service.query(handle, types.WorkflowList)
  assert code == types.QueryTimeout
  assert message == "workflow query timed out"

  assert query_service.stop(handle, 1000) == Ok(Nil)
}

pub fn workflow_detail_unknown_id_returns_not_found_test() {
  let state = state_for("test/tmp/control-query-workflow-not-found")

  let assert Error(types.QueryError(code: code, message: message)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "missing"),
    )

  assert code == types.QueryNotFound
  assert message == "workflow not found: missing"
}

pub fn workflow_paths_are_relativized_and_sanitized_test() {
  let root = "test/tmp/control-query-workflow-paths"
  test_helpers.reset_dir(root)

  assert query_workflow.safe_relative_path(
      root <> "/workflows/implementation.yaml",
      [root],
    )
    == "workflows/implementation.yaml"

  let outside =
    query_workflow.safe_relative_path(root <> "/../secret.yaml", [root])
  assert string.starts_with(outside, "external/")
  assert string.ends_with(outside, "-secret.yaml")
  assert !string.contains(outside, "..")
  assert !string.contains(outside, root)
}

pub fn workflow_reload_error_inventory_includes_diagnostics_test() {
  let state = state_for("test/tmp/control-query-workflow-diagnostics")
  let invalid_state =
    workflow_reloader.State(
      ..workflow_reloader.mark_invalid(state, "missing_workflow_file"),
      last_invalid_dependency_snapshot: Some([
        workflow_reloader.DependencyRead(
          state.bundle.orchestrator.config_dir <> "/workflows/missing.yaml",
          workflow_reloader.ReadFailed,
        ),
      ]),
    )

  let inventory = query_workflow.workflow_list_from_state(invalid_state)

  assert inventory.freshness.reload_status == "reload_error"
  let assert [reload_diagnostic, dependency_diagnostic] = inventory.diagnostics
  assert reload_diagnostic.code == "missing_workflow_file"
  assert reload_diagnostic.path == None
  assert dependency_diagnostic.code == "dependency_read_failed"
  assert dependency_diagnostic.path == Some("workflows/missing.yaml")

  let assert [first, ..] = inventory.workflows
  assert first.status == "reload_error"
}

fn start_query_runtime(
  state: workflow_reloader.State,
  get_workflow_snapshot: fn(Int) -> Result(workflow_reloader.State, Nil),
) -> query_service.Handle {
  let assert Ok(handle) =
    query_runtime.start(
      state.bundle.effective,
      daemon_identity.DaemonIdentity(
        daemon_id: "daemon-1",
        boot_id: "boot-1",
        path: "test/tmp/control-query-workflow-runtime/identity.json",
      ),
      empty_tracker_adapter(),
      get_dispatch_paused: fn(_) { Ok(False) },
      get_read_model_snapshot: fn(_) { Ok(empty_read_model_snapshot()) },
      get_projection_snapshot: fn(_) { Ok(projection.new()) },
      get_outbox_snapshot: fn(_) { Ok([]) },
      get_workflow_snapshot: get_workflow_snapshot,
    )
  handle
}

fn empty_read_model_snapshot() -> read_model.Snapshot {
  read_model.snapshot(
    read_model.new(
      daemon_id: "daemon-1",
      boot_id: "boot-1",
      ui_server_enabled: False,
    ),
    sampled_at_ms: 0,
  )
}

fn empty_tracker_adapter() -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    kind: "test-memory",
    display_name: "Test memory",
    task_source: adapter.TaskSourceCapability(
      fetch_candidates: fn(_) { Ok([]) },
      refresh_by_refs: fn(_) { Ok([]) },
      lookup_by_operator_ref: fn(_) { Ok(None) },
      list_tasks: fn(_) { Ok(adapter.TaskPage(items: [], has_more: False)) },
      lookup_task_detail: fn(_) { Ok(None) },
    ),
    work_items: None,
    comments: None,
    remote_commands: None,
    state_transitions: None,
    routing_metadata: None,
    links: None,
    handoff: None,
    scheduled_failures: None,
    readiness: None,
    smoke: None,
    attachments: None,
  )
}
