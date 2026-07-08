import gleam/erlang/process
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
import scherzo/state/record
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

fn write_structured_project(dir: String) -> Nil {
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/recover.md",
      "Recover this workflow step without starting over.",
    )
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/draft.md", "Draft output.")
  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/review.md", "Review output.")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/prompts/daemon.md",
      "Use daemon defaults.",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\n"
        <> "id: implementation\n"
        <> "workspace:\n"
        <> "  driver: jj\n"
        <> "  requires: [status, publish-commit-stack]\n"
        <> "concurrency: 2\n"
        <> "model: anthropic/claude-sonnet-4\n"
        <> "recovery:\n"
        <> "  attempts: 2\n"
        <> "  model: openai/gpt-5-mini\n"
        <> "  prompt: prompts/recover.md\n"
        <> "contract:\n"
        <> "  version: 1\n"
        <> "  inputs:\n"
        <> "    brief:\n"
        <> "      kind: file\n"
        <> "      media_type: text/plain\n"
        <> "      required: true\n"
        <> "      source: issue_context\n"
        <> "  context:\n"
        <> "    base:\n"
        <> "      kind: ref\n"
        <> "      ref_type: git_ref\n"
        <> "      required: false\n"
        <> "      source: workspace_driver_base\n"
        <> "  outputs:\n"
        <> "    report:\n"
        <> "      kind: file\n"
        <> "      media_type: text/markdown\n"
        <> "      source:\n"
        <> "        step: review\n"
        <> "        field: final_response\n"
        <> "    commit_stack:\n"
        <> "      kind: commit_stack\n"
        <> "      media_type: application/vnd.scherzo.git-commit-stack+json\n"
        <> "      artifact_type: scherzo.git_commit_stack.v1\n"
        <> "      source:\n"
        <> "        step: prepare\n"
        <> "        path: state/commit-stack.json\n"
        <> "artifacts:\n"
        <> "  publications:\n"
        <> "    - id: implementation_pr\n"
        <> "      repository: github.code\n"
        <> "      required: true\n"
        <> "      mode: commit_stack\n"
        <> "      commit_stack:\n"
        <> "        select:\n"
        <> "          output: commit_stack\n"
        <> "      target:\n"
        <> "        kind: stable_branch\n"
        <> "workstream_phase:\n"
        <> "  phase_id: implementation\n"
        <> "  display_name: Implementation\n"
        <> "  next_actions:\n"
        <> "    - action_id: review\n"
        <> "      workflow_id: review\n"
        <> "      inputs: [report]\n"
        <> "      requires_gate: human_review\n"
        <> "      auto_enqueue: true\n"
        <> "steps:\n"
        <> "  - id: prepare\n"
        <> "    kind: command\n"
        <> "    run: echo prepare\n"
        <> "    timeout: 30s\n"
        <> "  - id: draft\n"
        <> "    kind: agent\n"
        <> "    depends_on: [prepare]\n"
        <> "    prompt: prompts/draft.md\n"
        <> "    structured_output:\n"
        <> "      format: json\n"
        <> "      artifact_name: implementation_pack\n"
        <> "      source:\n"
        <> "        type: pi_tool_call\n"
        <> "        tool_name: submit_implementation_pack\n"
        <> "        parameters_schema_path: schemas/implementation-pack.schema.json\n"
        <> "      required: true\n"
        <> "      validators:\n"
        <> "        - name: implementation_pack_shape\n"
        <> "          type: json_schema\n"
        <> "          path: schemas/implementation-pack.schema.json\n"
        <> "          draft: \"2020-12\"\n"
        <> "      validation_retries: 0\n"
        <> "  - id: review\n"
        <> "    kind: agent\n"
        <> "    depends_on: [draft]\n"
        <> "    prompt: prompts/review.md\n"
        <> "    model: openai/gpt-5\n"
        <> "    on_failure: continue\n"
        <> "    recovery:\n"
        <> "      attempts: 3\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/scheduled.yaml",
      "version: 1\nid: scheduled\nworkspace:\n  driver: noop\nsteps:\n  - id: scan\n    kind: command\n    run: echo scan\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/daemon-default.yaml",
      "version: 1\nid: daemon-default\nsteps:\n  - id: ask\n    kind: agent\n    prompt: prompts/daemon.md\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/disabled-scheduled.yaml",
      "version: 1\nid: disabled-scheduled\nworkspace:\n  driver: noop\nsteps:\n  - id: scan\n    kind: command\n    run: echo disabled\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    api_key_env: LINEAR_API_KEY\n"
        <> "    project: TEST\n"
        <> "  states:\n"
        <> "    ready: [Todo]\n"
        <> "workspace:\n"
        <> "  root: workspaces\n"
        <> "  driver: jj\n"
        <> "  drivers:\n"
        <> "    noop:\n"
        <> "      type: noop\n"
        <> "agents:\n"
        <> "  model: openai/gpt-5\n"
        <> "workflows:\n"
        <> "  implementation: workflows/implementation.yaml\n"
        <> "  scheduled: workflows/scheduled.yaml\n"
        <> "  daemon-default: workflows/daemon-default.yaml\n"
        <> "  disabled-scheduled: workflows/disabled-scheduled.yaml\n"
        <> "schedules:\n"
        <> "  - id: scheduled\n"
        <> "    workflow: scheduled\n"
        <> "    enabled: true\n"
        <> "    every: 15m\n"
        <> "    overlap: skip\n"
        <> "    catch_up: false\n"
        <> "    on_failure:\n"
        <> "      task:\n"
        <> "        enabled: true\n"
        <> "        state: Triage\n"
        <> "        labels: [schedule-failed]\n"
        <> "        dedupe: open_task_per_schedule\n"
        <> "  - id: disabled-scheduled\n"
        <> "    workflow: disabled-scheduled\n"
        <> "    enabled: false\n"
        <> "    every: 15m\n"
        <> "    overlap: skip\n"
        <> "    catch_up: false\n"
        <> "artifacts:\n"
        <> "  repositories:\n"
        <> "    github:\n"
        <> "      code:\n"
        <> "        repo: example/repo\n"
        <> "        base: main\n",
    )
  Nil
}

fn structured_state_for(dir: String) -> workflow_reloader.State {
  write_structured_project(dir)
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

pub fn workflow_detail_includes_structured_read_model_test() {
  let state = state_for_structured_detail()

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "implementation"),
    )

  assert detail.schema_version == types.workflow_query_schema_version
  let assert types.WorkflowRoutedTriggerDto(route, label) = detail.trigger
  assert route == "implementation"
  assert label == Some("workflow:implementation")
  assert detail.workspace.driver == "jj"
  assert detail.workspace.required_capabilities
    == [
      "status",
      "publish-commit-stack",
    ]
  assert detail.execution.model.model == Some("anthropic/claude-sonnet-4")
  assert detail.execution.max_parallel_steps == 2
  let assert Some(workflow_recovery) = detail.execution.recovery
  assert workflow_recovery.attempts == 2
  assert workflow_recovery.model == Some("openai/gpt-5-mini")
  assert workflow_recovery.prompt.kind == "file"
  assert workflow_recovery.prompt.ref == Some("prompts/recover.md")

  let assert Some(contract) = detail.contract
  let assert [brief] = contract.inputs
  assert brief.name == "brief"
  assert brief.type_ == "text"
  assert brief.source.required == True
  assert brief.source.kind == Some("issue_context")
  assert brief.descriptor_present == True
  let assert [context] = contract.context
  assert context.source.kind == Some("workspace_driver_base")
  let assert [report, commit_stack] = contract.outputs
  assert report.source.kind == Some("field")
  assert commit_stack.type_ == "commit_stack"

  let assert [prepare, draft, review] = detail.steps
  assert prepare.kind == "command"
  assert prepare.depends_on == []
  let assert Some(command) = prepare.command
  assert command.run == "echo prepare"
  assert command.timeout_ms == Some(30_000)
  assert draft.kind == "agent"
  assert draft.depends_on == ["prepare"]
  let assert Some(draft_model) = draft.model
  assert draft_model.model == Some("anthropic/claude-sonnet-4")
  let assert Some(draft_agent) = draft.agent
  assert draft_agent.prompt.kind == "file"
  assert draft_agent.prompt.ref == Some("prompts/draft.md")
  let assert Some(structured_output) = draft_agent.structured_output
  assert structured_output.artifact_name == "implementation_pack"
  assert structured_output.required == True
  assert structured_output.validation_retries == 0
  let assert [validator] = structured_output.validators
  assert validator.name == "implementation_pack_shape"
  assert validator.kind == "json_schema"
  assert review.on_failure == "continue"
  let assert Some(review_model) = review.model
  assert review_model.model == Some("openai/gpt-5")
  let assert Some(review_recovery) = review.recovery
  assert review_recovery.attempts == 3
  assert review_recovery.model == Some("openai/gpt-5-mini")
  assert review_recovery.prompt.kind == "file"
  assert review_recovery.prompt.ref == Some("prompts/recover.md")

  let assert [publication] = detail.publications
  assert publication.id == "implementation_pr"
  assert publication.repository == "github.code"
  assert publication.required == True
  assert publication.mode == "commit_stack"

  let assert [next_action] = detail.next_actions
  assert next_action.action_id == "review"
  assert next_action.workflow_id == "review"
  assert next_action.requires_gate == Some("human_review")
  assert next_action.auto_enqueue == True
}

pub fn workflow_detail_includes_scheduled_trigger_test() {
  let state = state_for_structured_detail()

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "scheduled"),
    )

  let assert types.WorkflowScheduledTriggerDto(
    schedule_id,
    every_ms,
    overlap,
    catch_up,
    on_failure,
  ) = detail.trigger
  assert schedule_id == "scheduled"
  assert every_ms == 900_000
  assert overlap == "skip"
  assert catch_up == False
  assert on_failure.task_enabled == True
  assert on_failure.task_state == Some("Triage")
  assert on_failure.task_labels == ["schedule-failed"]
  assert on_failure.task_dedupe == "open_task_per_schedule"
}

pub fn workflow_detail_ignores_disabled_schedule_trigger_test() {
  let state = state_for_structured_detail()

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "disabled-scheduled"),
    )

  let assert types.WorkflowRoutedTriggerDto(route, label) = detail.trigger
  assert route == "disabled-scheduled"
  assert label == Some("workflow:disabled-scheduled")
}

pub fn workflow_detail_resolves_daemon_default_model_test() {
  let state = state_for_structured_detail()

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "daemon-default"),
    )

  assert detail.execution.model.model == Some("openai/gpt-5")
  let assert [step] = detail.steps
  let assert Some(model) = step.model
  assert model.model == Some("openai/gpt-5")
}

fn state_for_structured_detail() -> workflow_reloader.State {
  structured_state_for("test/tmp/control-query-workflow-structured-detail")
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

pub fn workflow_detail_caps_large_structured_strings_test() {
  let dir = "test/tmp/control-query-workflow-structured-string-cap"
  let large =
    string.repeat("x", times: query_workflow.max_structured_field_chars + 1)
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/workflows")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/implementation.yaml",
      "version: 1\n"
        <> "id: implementation\n"
        <> "contract:\n"
        <> "  version: 1\n"
        <> "  inputs:\n"
        <> "    brief:\n"
        <> "      kind: file\n"
        <> "      media_type: text/plain\n"
        <> "      description: "
        <> large
        <> "\n"
        <> "      required: true\n"
        <> "      source: issue_context\n"
        <> "steps:\n"
        <> "  - id: oversized\n"
        <> "    kind: command\n"
        <> "    run: "
        <> large
        <> "\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  linear:\n    api_key_env: LINEAR_API_KEY\n    project: TEST\n  states:\n    ready: [Todo]\nworkspace:\n  root: workspaces\nworkflows:\n  implementation: workflows/implementation.yaml\n",
    )
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let state =
    workflow_reloader.from_bundle(Some(dir <> "/scherzo.yaml"), bundle)

  let assert Ok(types.WorkflowDetailResponse(detail)) =
    query_workflow.execute_detail(
      state,
      types.WorkflowDetailQuery(workflow_id: "implementation"),
    )

  let assert [step] = detail.steps
  let assert Some(command) = step.command
  assert string.length(command.run) == query_workflow.max_structured_field_chars
  let assert Some(contract) = detail.contract
  let assert [input] = contract.inputs
  let assert Some(description) = input.description
  assert string.length(description) == query_workflow.max_structured_field_chars
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
  assert message == "daemon actor query timed out while loading workflow state"

  assert query_service.stop(handle, 1000) == Ok(Nil)
}

pub fn operation_status_query_executes_through_query_runtime_test() {
  let state = state_for("test/tmp/control-query-workflow-operation-status")
  let projection =
    projection.fold([
      record.with_id(
        "op-queued",
        1000,
        record.ControlOperationQueued(
          operation_id: "op-123",
          operation_kind: "artifact_publication_retry",
          command_name: "retry_artifact_publication",
          target: "run:run-1:execplan_review_doc",
          run_id: Some("run-1"),
          issue_id: Some("issue-1"),
          issue_identifier: Some("LIV-1"),
          requested_step_id: None,
          publication_id: Some("execplan_review_doc"),
        ),
      ),
      record.with_id(
        "op-started",
        1001,
        record.ControlOperationStarted(operation_id: "op-123"),
      ),
    ])
  let handle =
    start_query_runtime_with_projection(state, fn(_) { Ok(state) }, fn(_) {
      Ok(projection)
    })

  let assert Ok(types.OperationStatusResponse(operation)) =
    query_service.query(
      handle,
      types.OperationStatus(types.OperationStatusQuery(operation_id: "op-123")),
    )
  assert operation.operation_id == "op-123"
  assert operation.status == "running"
  assert operation.started_at_ms == Some(1001)
  assert operation.publication_id == Some("execplan_review_doc")
  assert operation.finished_at_ms == None

  assert query_service.stop(handle, 1000) == Ok(Nil)
}

pub fn operation_status_query_uses_standard_snapshot_timeout_test() {
  let state =
    state_for("test/tmp/control-query-workflow-operation-status-timeout-budget")
  let timeout_subject = process.new_subject()
  let handle =
    start_query_runtime_with_projection(
      state,
      fn(_) { Ok(state) },
      fn(timeout_ms) {
        process.send(timeout_subject, timeout_ms)
        Ok(projection.new())
      },
    )

  let assert Error(types.QueryError(code: code, message: message)) =
    query_service.query(
      handle,
      types.OperationStatus(types.OperationStatusQuery(operation_id: "missing")),
    )
  assert code == types.QueryNotFound
  assert message == "operation not found: missing"

  let assert Ok(timeout_ms) = process.receive(timeout_subject, within: 1000)
  assert timeout_ms == state.effective.control.command_timeout_ms
  assert query_service.stop(handle, 1000) == Ok(Nil)
}

pub fn operation_status_query_maps_not_found_and_timeout_test() {
  let state =
    state_for("test/tmp/control-query-workflow-operation-status-errors")
  let missing_handle =
    start_query_runtime_with_projection(state, fn(_) { Ok(state) }, fn(_) {
      Ok(projection.new())
    })

  let assert Error(types.QueryError(
    code: missing_code,
    message: missing_message,
  )) =
    query_service.query(
      missing_handle,
      types.OperationStatus(types.OperationStatusQuery(operation_id: "missing")),
    )
  assert missing_code == types.QueryNotFound
  assert missing_message == "operation not found: missing"
  assert query_service.stop(missing_handle, 1000) == Ok(Nil)

  let timeout_handle =
    start_query_runtime_with_projection(state, fn(_) { Ok(state) }, fn(_) {
      Error(Nil)
    })

  let assert Error(types.QueryError(
    code: timeout_code,
    message: timeout_message,
  )) =
    query_service.query(
      timeout_handle,
      types.OperationStatus(types.OperationStatusQuery(operation_id: "op-123")),
    )
  assert timeout_code == types.QueryTimeout
  assert timeout_message
    == "daemon actor query timed out while loading operation status"
  assert query_service.stop(timeout_handle, 1000) == Ok(Nil)
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
  start_query_runtime_with_projection(state, get_workflow_snapshot, fn(_) {
    Ok(projection.new())
  })
}

fn start_query_runtime_with_projection(
  state: workflow_reloader.State,
  get_workflow_snapshot: fn(Int) -> Result(workflow_reloader.State, Nil),
  get_projection_snapshot: fn(Int) -> Result(projection.Projection, Nil),
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
      get_projection_snapshot: get_projection_snapshot,
      get_outbox_snapshot: fn(_) { Ok([]) },
      get_workflow_snapshot: get_workflow_snapshot,
      get_claims_snapshot: fn(_) {
        Ok(types.ClaimListDto(sampled_at_ms: 0, items: []))
      },
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
