import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/artifact_publication_config
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/path
import scherzo/runtime_bundle
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_run
import simplifile

pub type Options {
  Options(workflow_path: String, run_root: String, run_id: String)
}

pub type RunError {
  RunError(code: String, message: String)
}

pub fn run(options: Options) -> Result(Nil, RunError) {
  use dag <- result.try(
    runtime_bundle.load_workflow_file(options.workflow_path)
    |> result.map_error(map_bundle_error),
  )
  let orchestrator = local_orchestrator(options.run_root)
  let issue = local_issue(workflow_dag.id(dag))
  let tracker_client = local_tracker(issue)
  let dependencies = local_dependencies(options, issue)
  let outcome =
    workflow_run.execute(
      issue,
      dag,
      orchestrator,
      tracker_client,
      [],
      options.run_id,
      dependencies,
    )
  use Nil <- result.try(write_summary(options, dag, outcome))
  case outcome {
    Ok(_) -> {
      io.println("SCHERZO_WORKFLOW_RUN=ok")
      io.println("SCHERZO_WORKFLOW_ID=" <> workflow_dag.id(dag))
      io.println("SCHERZO_RUN_ID=" <> options.run_id)
      io.println("SCHERZO_RUN_ROOT=" <> options.run_root)
      io.println(
        "SCHERZO_WORKFLOW_RUN_SUMMARY="
        <> path.join(options.run_root, "native-runner-summary.v1.json"),
      )
      Ok(Nil)
    }
    Error(failure) ->
      Error(RunError(
        code: "workflow_run_failed",
        message: workflow_run.failure_report(failure),
      ))
  }
}

fn map_bundle_error(error: runtime_bundle.BundleError) -> RunError {
  let runtime_bundle.BundleError(code, message) = error
  RunError(code: code, message: message)
}

fn local_issue(workflow_id: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "local-workflow-run",
    identifier: "LOCAL-WORKFLOW-RUN",
    title: "Local workflow run",
    description: None,
    priority: None,
    state: issue_state.todo_state(),
    branch_name: None,
    url: None,
    labels: ["workflow:" <> workflow_id],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn local_tracker(issue: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([issue]) },
  )
}

fn local_orchestrator(run_root: String) -> config_types.OrchestratorConfig {
  let hooks = config_types.empty_dag_hooks()
  let profile =
    config_types.WorkspaceHookProfile(
      name: "dogfood-jj",
      driver: None,
      source: config_types.SyntheticDefaultWorkspace,
    )
  let tracker =
    config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "local://native-workflow-runner",
      api_key: Some("local-native-runner"),
      project_slug: Some("LOCAL"),
      task_scope: None,
      active_states: issue_state.list_from_strings(["Todo", "Done"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    )
  let workspace =
    config_types.WorkspaceConfig(root: path.join(run_root, "workspaces"))
  config_types.OrchestratorConfig(
    effective: config_types.EffectiveConfig(
      ..config.default_effective_config(tracker, workspace),
      agent: config_types.AgentConfig(
        ..config.default_agent_config(),
        max_turns: 1,
      ),
      pi: config_types.PiConfig(
        ..config.default_pi_config(),
        compatibility_probe: False,
      ),
    ),
    config_dir: ".scherzo",
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: False,
      default_workflow: Some("implementation"),
      workflows: dict.new(),
    ),
    dag_hooks: hooks,
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "dogfood-jj",
      profiles: dict.from_list([#("dogfood-jj", profile)]),
    ),
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 20_000,
      template_field_max_chars: 20_000,
      workflow_summary_max_chars: 40_000,
    ),
    artifact_repositories: artifact_publication_config.empty_repositories(),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn local_dependencies(
  options: Options,
  _issue: tracker_issue.Issue,
) -> workflow_run.Dependencies {
  let default_dependencies = workflow_run.default_dependencies()
  workflow_run.Dependencies(
    prepare_step: fn(
      _issue,
      workflow_id,
      run_id,
      _step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      known,
    ) {
      prepare_local_step(
        options.run_root,
        workflow_id,
        run_id,
        attempt_index,
        workspace_ref,
        profile,
        known,
      )
    },
    prepare_recovered_step: fn(
      _issue,
      workflow_id,
      run_id,
      _expected_run_root,
      _step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      known,
    ) {
      prepare_local_step(
        options.run_root,
        workflow_id,
        run_id,
        attempt_index,
        workspace_ref,
        profile,
        known,
      )
    },
    after_step: fn(_, _, _, _, _) { Nil },
    cleanup_run: fn(_, _, _) { Ok(Nil) },
    command_step: fn(context, command, timeout_ms, secrets, limits) {
      command_step.run_with_env(
        context.step_id,
        command,
        context.workspace_path,
        timeout_ms,
        local_step_env(context),
        secrets,
        limits,
      )
    },
    agent_step: fn(
      step_issue,
      context,
      prompt_mode,
      attempt_context,
      effective,
      tracker_client,
      emit_update,
      command_ready,
      record_pi_session,
    ) {
      let context =
        workflow_run.StepContext(
          ..context,
          run_artifact_dir: local_run_artifact_dir(
            context.run_root,
            context.run_id,
          ),
        )
      default_dependencies.agent_step(
        step_issue,
        context,
        prompt_mode,
        attempt_context,
        effective,
        tracker_client,
        emit_update,
        command_ready,
        record_pi_session,
      )
    },
    checkpoint: workflow_checkpoint.ledger_writer(options.run_root, fn() { 123 }),
  )
}

fn prepare_local_step(
  run_root: String,
  workflow_id: String,
  run_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  profile: config_types.WorkspaceHookProfile,
  known: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError) {
  case simplifile.create_directory_all(run_root) {
    Error(err) ->
      Error(
        workspace_run.WorkspaceFailure(
          error.WorkspaceIo(simplifile.describe_error(err)),
        ),
      )
    Ok(Nil) -> {
      let source = case workspace_ref.from {
        None -> workspace.FreshWorkspace
        Some(name) ->
          case dict.get(known, name) {
            Ok(prepared) -> workspace.DerivedWorkspace(name, prepared.path)
            Error(Nil) -> workspace.DerivedWorkspace(name, ".")
          }
      }
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        workflow_bundle_dir: workflow_identity.bundle_dir_for_path(
          ".scherzo",
          "workflows/" <> workflow_id <> ".yaml",
        ),
        attempt_index: attempt_index,
        workspace_name: workspace_ref.name,
        path: ".",
        source: source,
        workspace_profile: profile.name,
      ))
    }
  }
}

fn local_run_artifact_dir(run_root: String, run_id: String) -> String {
  path.join(
    path.join(path.join(run_root, ".scherzo-state"), "artifacts"),
    "runs/" <> run_id,
  )
}

fn local_step_env(
  context: workflow_run.StepContext,
) -> List(#(String, String)) {
  let base = [
    #("SCHERZO_REPO_ROOT", path.absolute(".") |> result.unwrap(".")),
    #("SCHERZO_CONFIG_DIR", context.config_dir),
    #("SCHERZO_WORKFLOW_ID", context.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", context.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", context.run_id),
    #("SCHERZO_RUN_ROOT", context.run_root),
    #(
      "SCHERZO_RUN_ARTIFACT_DIR",
      local_run_artifact_dir(context.run_root, context.run_id),
    ),
    #("SCHERZO_RUN_KIND", context.run_kind),
    #("SCHERZO_ISSUE_ID", context.issue_id),
    #("SCHERZO_ISSUE_IDENTIFIER", context.issue_identifier),
    #("SCHERZO_SCHEDULED_JOB_ID", context.scheduled_job_id),
    #("SCHERZO_SCHEDULE_DUE_AT", context.schedule_due_at),
    #("SCHERZO_SCHEDULE_STARTED_AT", context.schedule_started_at),
    #("SCHERZO_RUN_ATTEMPT", int.to_string(context.run_attempt)),
    #("SCHERZO_STEP_ID", context.step_id),
    #("SCHERZO_ATTEMPT_INDEX", int.to_string(context.attempt_index)),
    #(
      "SCHERZO_ATTEMPT_KEY",
      workflow_identity.attempt_key(
        context.run_id,
        context.step_id,
        context.attempt_index,
      ),
    ),
    #(
      "SCHERZO_HOOK_IDEMPOTENCY_KEY",
      workflow_identity.hook_idempotency_key(context.run_id, context.step_id),
    ),
    #("SCHERZO_WORKSPACE_NAME", context.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", context.workspace_path),
  ]
  list.append(base, context.extra_pi_env)
}

fn write_summary(
  options: Options,
  dag: workflow_dag.WorkflowDag,
  outcome: Result(
    workflow_run.WorkflowRunSuccess,
    workflow_run.WorkflowRunFailure,
  ),
) -> Result(Nil, RunError) {
  let #(status, artifacts, reason) = case outcome {
    Ok(success) -> #("succeeded", success.artifacts, None)
    Error(failure) -> #("failed", failure.artifacts, Some(failure.reason))
  }
  let summary =
    json.object([
      #("schema_version", json.int(1)),
      #("artifact_type", json.string("workflow_local_run_summary")),
      #("workflow_path", json.string(options.workflow_path)),
      #("workflow_id", json.string(workflow_dag.id(dag))),
      #("run_id", json.string(options.run_id)),
      #("run_root", json.string(options.run_root)),
      #("status", json.string(status)),
      #("failure_reason", option_string_json(reason)),
      #("remote_mutations", json.string("none")),
      #("steps", json.array(step_summaries(artifacts), of: identity_json)),
    ])
  let summary_path =
    path.join(options.run_root, "native-runner-summary.v1.json")
  use Nil <- result.try(
    simplifile.create_directory_all(options.run_root)
    |> result.map_error(fn(err) {
      RunError(
        code: "summary_write_failed",
        message: simplifile.describe_error(err),
      )
    }),
  )
  simplifile.write(summary_path, json.to_string(summary) <> "\n")
  |> result.map_error(fn(err) {
    RunError(
      code: "summary_write_failed",
      message: simplifile.describe_error(err),
    )
  })
}

fn identity_json(value: json.Json) -> json.Json {
  value
}

fn step_summaries(
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> List(json.Json) {
  artifacts
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(step_id, artifact) = entry
    step_summary(step_id, artifact)
  })
}

fn step_summary(
  step_id: String,
  artifact: step_artifact.StepArtifact,
) -> json.Json {
  json.object([
    #("step_id", json.string(step_id)),
    #("status", json.string(step_artifact.status_to_string(artifact.status))),
    #("failure_code", option_string_json(artifact.failure_code)),
    #("exit_code", option_int_json(artifact.exit_code)),
    #("summary", json.string(artifact.summary_text)),
    #("stdout", json.string(artifact.stdout)),
    #("stderr", json.string(artifact.stderr)),
    #("structured_output", structured_output_json(artifact.structured_output)),
  ])
}

fn structured_output_json(
  outcome: Option(step_artifact.StructuredOutputOutcome),
) -> json.Json {
  case outcome {
    Some(step_artifact.StructuredOutputValid(metadata)) ->
      json.object([
        #("status", json.string("valid")),
        #("artifact_name", json.string(metadata.artifact_name)),
        #("format", json.string(metadata.format)),
        #("ref", json.string(metadata.ref)),
        #("path", json.string(metadata.path)),
        #("uri", json.string(metadata.uri)),
        #("display_path", json.string(metadata.display_path)),
        #("local_path", option_string_json(metadata.local_path)),
        #("sha256", json.string(metadata.sha256)),
        #("bytes", json.int(metadata.bytes)),
        #("schema_status", json.string(metadata.schema_status)),
        #("source_type", json.string(metadata.source_type)),
        #("source_tool_name", option_string_json(metadata.source_tool_name)),
        #("retry", structured_output_retry_json(metadata.retry)),
      ])
    Some(step_artifact.StructuredOutputAbsent(
      artifact_name,
      format,
      schema_status,
    )) ->
      json.object([
        #("status", json.string("absent")),
        #("artifact_name", json.string(artifact_name)),
        #("format", json.string(format)),
        #("schema_status", json.string(schema_status)),
      ])
    Some(step_artifact.StructuredOutputError(
      artifact_name,
      format,
      message,
      details,
      retry,
    )) ->
      json.object([
        #("status", json.string("error")),
        #("artifact_name", json.string(artifact_name)),
        #("format", json.string(format)),
        #("error", json.string(message)),
        #("failure", structured_output_error_details_json(details)),
        #("retry", structured_output_retry_json(retry)),
      ])
    None -> json.null()
  }
}

fn structured_output_retry_json(
  retry: Option(step_artifact.StructuredOutputRetryInfo),
) -> json.Json {
  case retry {
    None -> json.null()
    Some(info) ->
      json.object([
        #("max_retries", json.int(info.max_retries)),
        #("attempts", json.int(info.attempts)),
        #("outcome", json.string(info.outcome)),
        #(
          "diagnostics",
          json.array(
            info.diagnostics,
            of: structured_output_retry_diagnostic_json,
          ),
        ),
      ])
  }
}

fn structured_output_error_details_json(
  details: Option(step_artifact.StructuredOutputErrorDetails),
) -> json.Json {
  case details {
    Some(details) ->
      json.object([
        #("code", json.string(details.code)),
        #("retryable", json.bool(details.retryable)),
        #("validator_name", option_string_json(details.validator_name)),
        #("validator_type", option_string_json(details.validator_type)),
        #("diagnostic_summary", json.string(details.diagnostic_summary)),
        #("stdout_truncated", json.bool(details.stdout_truncated)),
        #("stderr_truncated", json.bool(details.stderr_truncated)),
      ])
    None -> json.null()
  }
}

fn structured_output_retry_diagnostic_json(
  diagnostic: step_artifact.StructuredOutputRetryDiagnostic,
) -> json.Json {
  json.object([
    #("attempt", json.int(diagnostic.attempt)),
    #("status", json.string(diagnostic.status)),
    #("failure_code", option_string_json(diagnostic.failure_code)),
    #("message", json.string(diagnostic.message)),
  ])
}

fn option_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}
