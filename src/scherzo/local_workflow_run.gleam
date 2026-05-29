import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/agent/types as agent_types
import scherzo/artifact_publication_config
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/path
import scherzo/result_artifact
import scherzo/runtime_bundle
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workflow_run
import scherzo/workspace_run
import simplifile

pub type Options {
  Options(
    workflow_path: String,
    run_root: String,
    run_id: String,
    native_review_scenario: Option(String),
  )
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
  let issue = local_issue(dag.id)
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
      io.println("SCHERZO_WORKFLOW_ID=" <> dag.id)
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
  config_types.OrchestratorConfig(
    effective: config_types.EffectiveConfig(
      tracker: config_types.TrackerConfig(
        kind: tracker_kind.LinearTracker,
        endpoint: "local://native-workflow-runner",
        api_key: Some("local-native-runner"),
        project_slug: Some("LOCAL"),
        active_states: issue_state.list_from_strings(["Todo", "Done"]),
        dispatch_states: issue_state.list_from_strings(["Todo"]),
        terminal_states: issue_state.list_from_strings(["Done"]),
      ),
      polling: config.default_polling_config(),
      workspace: config_types.WorkspaceConfig(root: path.join(
        run_root,
        "workspaces",
      )),
      hooks: config.default_hooks_config(),
      agent: config_types.AgentConfig(
        ..config.default_agent_config(),
        max_turns: 1,
      ),
      pi: config_types.PiConfig(
        ..config.default_pi_config(),
        compatibility_probe: False,
      ),
      handoff: config.default_handoff_config(),
      linear_contract: config.default_linear_contract_config(),
      linear_commands: config.default_linear_command_config(),
      ui_server: config.default_ui_server_config(),
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
  issue: tracker_issue.Issue,
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
        local_step_env(context, options.native_review_scenario),
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
      case options.native_review_scenario {
        Some(_) ->
          native_fixture_agent_step(
            options.native_review_scenario,
            issue,
            context,
          )
        None ->
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
      }
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
      let source_path = case workspace_ref.from {
        None -> None
        Some(name) ->
          case dict.get(known, name) {
            Ok(prepared) -> Some(prepared.path)
            Error(Nil) -> Some(".")
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
        source_workspace_name: workspace_ref.from,
        source_workspace_path: source_path,
        workspace_profile: profile.name,
      ))
    }
  }
}

fn local_step_env(
  context: workflow_run.StepContext,
  scenario: Option(String),
) -> List(#(String, String)) {
  let base = [
    #("SCHERZO_REPO_ROOT", path.absolute(".") |> result.unwrap(".")),
    #("SCHERZO_CONFIG_DIR", context.config_dir),
    #("SCHERZO_WORKFLOW_ID", context.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", context.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", context.run_id),
    #("SCHERZO_RUN_ROOT", context.run_root),
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
  let base = case scenario {
    Some(value) -> [#("SCHERZO_NATIVE_REVIEW_SCENARIO", value), ..base]
    None -> base
  }
  list.append(base, context.extra_pi_env)
}

fn native_fixture_agent_step(
  scenario: Option(String),
  issue: tracker_issue.Issue,
  context: workflow_run.StepContext,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  let scenario_id = option.unwrap(scenario, "default")
  case should_fail_agent_step(scenario_id, context.step_id) {
    True ->
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError(
          "native fixture agent failed before producing structured output",
        )),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    False -> {
      Ok(agent_types.WorkerSuccess(
        final_issue: Some(issue),
        final_classification: agent_types.FinalTerminal,
        workspace_path: context.workspace_path,
        tokens: session_tokens.zero_token_totals(),
        turns: 1,
        result: fixture_result(scenario_id, context),
      ))
    }
  }
}

fn should_fail_agent_step(scenario_id: String, step_id: String) -> Bool {
  scenario_id == "lane-failure" && step_id == "lane_correctness"
}

fn fixture_result(
  scenario_id: String,
  context: workflow_run.StepContext,
) -> result_artifact.ResultArtifact {
  let draft_json =
    review_lane_draft_json_with_artifact_type(
      lane_id_for_step(context.step_id),
      scenario_id,
      context.run_root,
      artifact_type_for_step(context.step_id),
    )
  case should_emit_missing_tool_call(scenario_id, context.step_id) {
    True ->
      result_artifact.from_final_response(
        Some(draft_json),
        False,
        "native_review_fixture_agent_final_response_only",
      )
    False -> {
      let arguments_json = case
        should_emit_malformed_json(scenario_id, context.step_id)
      {
        True -> "{ this is not valid JSON from the native fixture agent\n"
        False -> draft_json
      }
      result_artifact.from_final_response_with_tool_calls(
        None,
        False,
        "native_review_fixture_agent_tool_call",
        [
          result_artifact.ToolCallSubmission(
            name: "submit_structured_output",
            arguments_json: Some(arguments_json),
            status: Some("success"),
            sibling_count: 1,
            receipt_json: Some(
              "{\"artifact_type\":\"scherzo_structured_output_tool_receipt\",\"tool_name\":\"submit_structured_output\",\"remote_mutations\":\"none\"}",
            ),
          ),
        ],
      )
    }
  }
}

fn should_emit_missing_tool_call(
  _scenario_id: String,
  step_id: String,
) -> Bool {
  step_id == "failed_lane"
}

fn should_emit_malformed_json(scenario_id: String, step_id: String) -> Bool {
  scenario_id == "malformed-agent-output" && step_id == "lane_correctness"
}

fn artifact_type_for_step(step_id: String) -> String {
  case step_id {
    "malformed_lane" -> "not_review_lane_draft"
    _ -> "review_lane_draft"
  }
}

fn lane_id_for_step(step_id: String) -> String {
  case step_id {
    "lane_correctness" | "valid_lane" | "malformed_lane" | "failed_lane" ->
      "correctness"
    "lane_test_quality" -> "test-quality"
    "lane_idioms_maintainability" -> "idioms-maintainability"
    "lane_security_performance" -> "security-performance"
    _ -> "correctness"
  }
}

fn review_lane_draft_json_with_artifact_type(
  lane_id: String,
  scenario_id: String,
  run_root: String,
  artifact_type: String,
) -> String {
  let #(findings, notes, requests) = draft_parts(lane_id, scenario_id)
  json.object([
    #(
      "$schema",
      json.string(".scherzo/workflows/schemas/review-artifacts.v1.schema.json"),
    ),
    #("schema_version", json.int(1)),
    #("artifact_type", json.string(artifact_type)),
    #("generated_at_utc", json.string("2026-05-09T00:00:00Z")),
    #(
      "producer",
      json.object([
        #("name", json.string("native-scherzo-agent-fixture")),
        #("version", json.string("1")),
        #("mode", json.string("native")),
      ]),
    ),
    #("lane", lane_json(lane_id)),
    #("input_refs", json.array(input_refs(run_root), of: identity_json)),
    #("draft_findings", json.array(findings, of: identity_json)),
    #("review_notes", json.array(notes, of: identity_json)),
    #("evidence_requests", json.array(requests, of: identity_json)),
    #(
      "self_check",
      json.object([
        #("inspected_diff", json.bool(True)),
        #("used_repository_relative_paths", json.bool(True)),
      ]),
    ),
    #("remote_mutations", json.string("none")),
  ])
  |> json.to_string
}

fn identity_json(value: json.Json) -> json.Json {
  value
}

fn input_refs(_run_root: String) -> List(json.Json) {
  let prepare_dir = "artifacts/review/prepare_review"
  [
    artifact_ref_json(
      "review_brief",
      path.join(prepare_dir, "review-brief.v1.json"),
    ),
    artifact_ref_json("diff", path.join(prepare_dir, "diff.patch")),
    artifact_ref_json(
      "source_metadata",
      path.join(prepare_dir, "source-metadata.v1.json"),
    ),
    artifact_ref_json(
      "changed_files",
      path.join(prepare_dir, "changed-files.v1.json"),
    ),
    artifact_ref_json(
      "validation_status",
      path.join(prepare_dir, "validation-status.v1.json"),
    ),
    artifact_ref_json(
      "context_manifest",
      path.join(prepare_dir, "context-manifest.v1.json"),
    ),
  ]
}

fn artifact_ref_json(artifact_type: String, ref_path: String) -> json.Json {
  json.object([
    #("artifact_type", json.string(artifact_type)),
    #("path", json.string(ref_path)),
  ])
}

fn lane_json(lane_id: String) -> json.Json {
  case lane_id {
    "test-quality" ->
      json.object([
        #("id", json.string("test-quality")),
        #("name", json.string("Test-quality reviewer")),
        #("category", json.string("testing")),
        #("version", json.string("1")),
      ])
    "idioms-maintainability" ->
      json.object([
        #("id", json.string("idioms-maintainability")),
        #("name", json.string("Idioms / maintainability reviewer")),
        #("category", json.string("maintainability")),
        #("version", json.string("1")),
      ])
    "security-performance" ->
      json.object([
        #("id", json.string("security-performance")),
        #("name", json.string("Security / performance risk reviewer")),
        #("category", json.string("security-performance")),
        #("version", json.string("1")),
      ])
    _ ->
      json.object([
        #("id", json.string("correctness")),
        #("name", json.string("Correctness reviewer")),
        #("category", json.string("correctness")),
        #("version", json.string("1")),
      ])
  }
}

fn draft_parts(
  lane_id: String,
  scenario_id: String,
) -> #(List(json.Json), List(json.Json), List(json.Json)) {
  case scenario_id, lane_id {
    "inverted-auth-control-condition", "correctness" -> #(
      [inverted_auth_finding()],
      [],
      [inverted_auth_request()],
    )
    "static-suspicion", "correctness" -> #([static_suspicion_finding()], [], [
      static_suspicion_request(),
    ])
    "pr-80", "idioms-maintainability" -> #([], [pr80_review_note()], [])
    _, _ -> #([], [], [])
  }
}

fn inverted_auth_finding() -> json.Json {
  finding_json(
    id: "F1",
    title: "User role can delete a project after the control-condition change.",
    claim: "The changed authorization branch returns Ok for User instead of Error.",
    category: "correctness",
    severity: "high",
    proposed_blocking: True,
    location_path: "src/liv_152_fixture/project_authorization.gleam",
    request_id: "E1",
    suggested_fix: "Restore the User branch to Error(\"forbidden\") and keep the reproduction fixture passing.",
  )
}

fn static_suspicion_finding() -> json.Json {
  finding_json(
    id: "F1",
    title: "Paused workflow branch appears to dispatch without executable proof.",
    claim: "The changed branch returns True while paused, but the fixture does not include a reproduction.",
    category: "correctness",
    severity: "high",
    proposed_blocking: True,
    location_path: "src/liv_152_fixture/workflow_gate.gleam",
    request_id: "E1",
    suggested_fix: "Add an executable reproduction before treating this as a blocker.",
  )
}

fn finding_json(
  id id: String,
  title title: String,
  claim claim: String,
  category category: String,
  severity severity: String,
  proposed_blocking proposed_blocking: Bool,
  location_path location_path: String,
  request_id request_id: String,
  suggested_fix suggested_fix: String,
) -> json.Json {
  json.object([
    #("draft_finding_id", json.string(id)),
    #("title", json.string(title)),
    #("claim", json.string(claim)),
    #("category", json.string(category)),
    #("severity", json.string(severity)),
    #("proposed_blocking", json.bool(proposed_blocking)),
    #(
      "locations",
      json.array(
        [
          json.object([
            #("path", json.string(location_path)),
            #("start_line", json.int(1)),
          ]),
        ],
        of: identity_json,
      ),
    ),
    #("evidence_request_ids", json.array([request_id], of: json.string)),
    #("suggested_fix", json.string(suggested_fix)),
  ])
}

fn inverted_auth_request() -> json.Json {
  request_json(
    id: "E1",
    finding_id: "F1",
    evidence_key: "fixture_reproduction",
    claim: "The changed control condition allows unauthorized project deletion.",
    expected: "REPRODUCED: unauthorized User received Ok(\"deleted\")",
    target: json.object([
      #("fixture_id", json.string("inverted-auth-control-condition")),
    ]),
  )
}

fn static_suspicion_request() -> json.Json {
  request_json(
    id: "E1",
    finding_id: "F1",
    evidence_key: "gleam_test",
    claim: "Paused workflows dispatch without approval.",
    expected: "targeted paused-workflow reproduction fails before fix",
    target: json.object([]),
  )
}

fn request_json(
  id id: String,
  finding_id finding_id: String,
  evidence_key evidence_key: String,
  claim claim: String,
  expected expected: String,
  target target: json.Json,
) -> json.Json {
  json.object([
    #("request_id", json.string(id)),
    #("draft_finding_id", json.string(finding_id)),
    #("evidence_key", json.string(evidence_key)),
    #("claim", json.string(claim)),
    #("expected_observation", json.string(expected)),
    #("target", target),
  ])
}

fn pr80_review_note() -> json.Json {
  json.object([
    #("id", json.string("N1")),
    #("kind", json.string("review_note")),
    #("category", json.string("maintainability")),
    #("severity", json.string("info")),
    #(
      "locations",
      json.array(
        [
          json.object([
            #("path", json.string(".scherzo/workflows/scripts/scherzo-review")),
          ]),
        ],
        of: identity_json,
      ),
    ),
    #(
      "summary",
      json.string(
        "Native review retained a PR #80-inspired context note without turning examples into blockers.",
      ),
    ),
    #(
      "details",
      json.string(
        "The fixture exercises staged review precision on documentation, workflow examples, helper scripts, source, and tests.",
      ),
    ),
    #(
      "suggested_action",
      json.string(
        "Inspect the final review artifact if this precision regresses.",
      ),
    ),
  ])
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
      #("workflow_id", json.string(dag.id)),
      #("run_id", json.string(options.run_id)),
      #("run_root", json.string(options.run_root)),
      #(
        "native_review_scenario",
        option_string_json(options.native_review_scenario),
      ),
      #(
        "agent_lane_mode",
        json.string(agent_lane_mode(options.native_review_scenario)),
      ),
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

fn agent_lane_mode(scenario: Option(String)) -> String {
  case scenario {
    Some(_) -> "native_fixture"
    None -> "real_agent"
  }
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
