import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/pi_rpc
import scherzo/agent/types as agent_types
import scherzo/command_step
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hash
import scherzo/json_value
import scherzo/model_config
import scherzo/orchestrator/schedule_core
import scherzo/path
import scherzo/result_artifact
import scherzo/session/event as session_event
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/record
import scherzo/step_artifact
import scherzo/structured_output_tool_spec
import scherzo/template
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_outcome
import scherzo/workflow_run
import scherzo/workflow_scheduler
import scherzo/workspace_driver_context
import scherzo/workspace_driver_discovery
import scherzo/workspace_run
import scherzo/workstream/artifacts as workstream_artifacts
import simplifile
import support/expected_crash
import test_async

type CommandStart {
  CommandStart(step_id: String, release: process.Subject(String))
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Implement DAGs",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["workflow:implementation"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn effective() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "https://api.linear.app/graphql",
      api_key: Some("test-key"),
      project_slug: Some("TEST"),
      active_states: issue_state.list_from_strings(["Todo"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(
      root: "test/tmp/workflow-run/workspaces",
    ),
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
  )
}

fn dag_hooks() -> config_types.DagHooksConfig {
  dag_hooks_with_timeout(1000)
}

fn dag_hooks_with_timeout(timeout_ms: Int) -> config_types.DagHooksConfig {
  config_types.DagHooksConfig(
    create: None,
    before_step: None,
    after_step: None,
    remove: None,
    timeout_ms: timeout_ms,
  )
}

fn workspace_profile(
  name: String,
  _hooks: config_types.DagHooksConfig,
  source: config_types.WorkspaceProfileSource,
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(name: name, driver: None, source: source)
}

fn workspace_profile_with_driver(
  name: String,
  hooks: config_types.DagHooksConfig,
  source: config_types.WorkspaceProfileSource,
  command: String,
  capabilities: List(config_types.WorkspaceCapability),
) -> config_types.WorkspaceHookProfile {
  workspace_profile_with_driver_env(
    name,
    hooks,
    source,
    command,
    capabilities,
    [],
  )
}

fn workspace_profile_with_driver_env(
  name: String,
  hooks: config_types.DagHooksConfig,
  source: config_types.WorkspaceProfileSource,
  command: String,
  capabilities: List(config_types.WorkspaceCapability),
  env: List(#(String, String)),
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(
    name: name,
    driver: Some(config_types.WorkspaceDriverConfig(
      command: command,
      lifecycle: [],
      capabilities: capabilities,
      timeout_ms: hooks.timeout_ms,
      env: env,
    )),
    source: source,
  )
}

fn legacy_workspace_profiles(
  hooks: config_types.DagHooksConfig,
) -> config_types.WorkspaceHookProfiles {
  config_types.WorkspaceHookProfiles(
    default_profile: "default",
    profiles: dict.from_list([
      #(
        "default",
        workspace_profile(
          "default",
          hooks,
          config_types.SyntheticDefaultWorkspace,
        ),
      ),
    ]),
  )
}

fn orchestrator() -> config_types.OrchestratorConfig {
  config_types.OrchestratorConfig(
    effective: effective(),
    config_dir: "test/tmp/workflow-run",
    routing: config_types.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.from_list([#("implementation", "implementation.yaml")]),
    ),
    dag_hooks: dag_hooks(),
    workspace_profiles: legacy_workspace_profiles(dag_hooks()),
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 1000,
      template_field_max_chars: 1000,
      workflow_summary_max_chars: 4000,
    ),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.StepRecoveryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn success_agent(prompt: String) -> agent_types.WorkerSuccess {
  success_agent_with_response(Some("response:" <> prompt), False)
}

fn success_agent_with_response(
  final_response: Option(String),
  truncated: Bool,
) -> agent_types.WorkerSuccess {
  success_agent_with_result(result_artifact.from_final_response(
    final_response,
    truncated,
    "test",
  ))
}

fn success_agent_with_result(
  result: result_artifact.ResultArtifact,
) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue()),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn artifact_root(root: String) -> String {
  root <> "/.scherzo-state/artifacts"
}

fn hidden_local_path_store(root: String) -> artifact_store.Store {
  let store_root = artifact_root(root)
  artifact_store.custom(
    "hidden-local-path",
    artifact_store.StoreCallbacks(
      write: fn(ref, contents) {
        let final_path = store_root <> "/" <> ref
        let assert Ok(parent) = path.dirname(final_path)
        use Nil <- result.try(
          simplifile.create_directory_all(parent)
          |> result.map_error(fn(error) {
            artifact_store.ArtifactIo(simplifile.describe_error(error))
          }),
        )
        artifact_store.write_atomic(final_path, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read: fn(ref) {
        simplifile.read(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            simplifile.Enoent -> artifact_store.MissingStepArtifact(ref)
            _ -> artifact_store.ArtifactIo(simplifile.describe_error(error))
          }
        })
      },
      write_immutable_bytes: fn(ref, contents) {
        artifact_store.write_immutable(store_root <> "/" <> ref, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read_bytes: fn(ref) {
        artifact_store.read_file_bytes(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            artifact_store.MissingStepArtifact(_) ->
              artifact_store.MissingStepArtifact(ref)
            _ -> error
          }
        })
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://hidden-local-path/" <> ref,
          display_path: "artifacts://" <> ref,
          local_path: None,
        ))
      },
    ),
  )
}

fn hidden_local_path_checkpoint(root: String) -> workflow_checkpoint.Writer {
  workflow_checkpoint.ledger_writer_with_artifact_store(
    root,
    fn() { 123 },
    hidden_local_path_store(root),
  )
}

fn ledger_records(root: String) -> List(record.LedgerRecord) {
  let assert Ok(contents) =
    simplifile.read(root <> "/.scherzo-state/ledger/current.jsonl")
  contents
  |> string.split(on: "\n")
  |> list.filter(fn(line) { string.trim(line) != "" })
  |> list.map(fn(line) {
    let assert Ok(decoded) = record.decode_string(line)
    decoded
  })
}

fn workflow_finished_outcome(root: String) -> String {
  let records = ledger_records(root)
  let assert Ok(ledger_record) =
    list.find(records, fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_run_finished"
    })
  case ledger_record.body {
    record.WorkflowRunFinished(outcome: outcome, ..)
    | record.WorkflowRunFinishedWithTask(outcome: outcome, ..) -> outcome
    _ -> panic as "expected workflow_run_finished"
  }
}

fn step_finished_outcome(root: String, step_id: String) -> String {
  let records = ledger_records(root)
  let assert Ok(ledger_record) =
    list.find(records, fn(ledger_record) {
      case ledger_record.body {
        record.StepAttemptFinished(step_id: finished_step_id, ..) ->
          finished_step_id == step_id
        _ -> False
      }
    })
  case ledger_record.body {
    record.StepAttemptFinished(outcome: outcome, ..) -> outcome
    _ -> panic as "expected step_attempt_finished"
  }
}

fn has_step_interrupted_before_workflow_finished(
  root: String,
  step_id: String,
  reason: String,
) -> Bool {
  has_step_interrupted_before_workflow_finished_loop(
    ledger_records(root),
    step_id,
    reason,
    False,
  )
}

fn has_step_interrupted_before_workflow_finished_loop(
  records: List(record.LedgerRecord),
  step_id: String,
  reason: String,
  interrupted_seen: Bool,
) -> Bool {
  case records {
    [] -> False
    [ledger_record, ..rest] ->
      case ledger_record.body {
        record.StepAttemptInterrupted(
          step_id: interrupted_step_id,
          reason: interrupted_reason,
          ..,
        ) ->
          has_step_interrupted_before_workflow_finished_loop(
            rest,
            step_id,
            reason,
            interrupted_seen
              || {
              interrupted_step_id == step_id && interrupted_reason == reason
            },
          )
        record.WorkflowRunFinished(..)
        | record.WorkflowRunFinishedWithTask(..) -> interrupted_seen
        _ ->
          has_step_interrupted_before_workflow_finished_loop(
            rest,
            step_id,
            reason,
            interrupted_seen,
          )
      }
  }
}

fn recording_checkpoint(
  root: String,
  subject: process.Subject(String),
) -> workflow_checkpoint.Writer {
  let base = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  workflow_checkpoint.Writer(
    ..base,
    workflow_finished: fn(finished: workflow_checkpoint.WorkflowFinished) {
      process.send(subject, "workflow_finished:" <> finished.outcome)
      base.workflow_finished(finished)
    },
  )
}

fn fake_pi() -> String {
  let assert Ok(abs) = path.absolute("test/fixtures/fake_pi_rpc.sh")
  abs
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn test_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn absolute_path(value: String) -> String {
  let assert Ok(abs) = path.absolute(value)
  abs
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_executable",
      "chmod +x " <> shell_quote(path),
      ".",
      5000,
      [],
      test_limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn write_noop_path_shim(root: String, log_path: String) -> String {
  let bin = root <> "/bin"
  let target = bin <> "/scherzo-workspace-noop"
  let source = absolute_path("scripts/scherzo-workspace-noop")
  let log = absolute_path(log_path)
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let assert Ok(Nil) =
    simplifile.write(
      target,
      "#!/bin/sh\n"
        <> "printf '%s\\n' \"$*\" >> "
        <> shell_quote(log)
        <> "\nexec "
        <> shell_quote(source)
        <> " \"$@\"\n",
    )
  chmod_executable(target)
  absolute_path(bin)
}

fn path_with_prefix(prefix: String, original: Option(String)) -> String {
  case original {
    Some(value) -> prefix <> ":" <> value
    None -> prefix
  }
}

fn restore_path(original: Option(String)) -> Nil {
  case original {
    Some(value) -> {
      let assert Ok(Nil) = setenv("PATH", value)
      Nil
    }
    None -> unsetenv("PATH")
  }
}

fn deps(
  subject: process.Subject(String),
  failing_command: Option(String),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    prepare_step: fn(
      _issue,
      workflow_id,
      run_id,
      step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      known,
    ) {
      prepare_fake_step(
        subject,
        workflow_id,
        run_id,
        step_id,
        attempt_index,
        workspace_ref,
        known,
        profile,
        "prepare",
      )
    },
    prepare_recovered_step: fn(
      _issue,
      workflow_id,
      run_id,
      _expected_run_root,
      step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      known,
    ) {
      prepare_fake_step(
        subject,
        workflow_id,
        run_id,
        step_id,
        attempt_index,
        workspace_ref,
        known,
        profile,
        "prepare_recovered",
      )
    },
    after_step: fn(_issue, step_id, _prepared, _orchestrator, _profile) {
      process.send(subject, "after:" <> step_id)
    },
    cleanup_run: fn(run_root, _orchestrator, _profile) {
      process.send(subject, "cleanup:" <> run_root)
      Ok(Nil)
    },
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(subject, "run:" <> context.step_id)
      let exit_code = case failing_command == Some(context.step_id) {
        True -> 1
        False -> 0
      }
      step_artifact.from_command_result(
        context.step_id,
        exit_code,
        "stdout:" <> context.step_id,
        "stderr:" <> context.step_id,
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      _issue,
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      let prompt = prompt_text(prompt_mode)
      process.send(subject, "agent:" <> context.workspace_path <> ":" <> prompt)
      Ok(success_agent(prompt))
    },
    checkpoint: workflow_checkpoint.noop_writer(),
  )
}

fn deps_with_structured_agent_response(
  subject: process.Subject(String),
  final_response: Option(String),
  truncated: Bool,
  checkpoint: workflow_checkpoint.Writer,
) -> workflow_run.Dependencies {
  deps_with_structured_agent_result(
    subject,
    result_artifact.from_final_response(final_response, truncated, "test"),
    checkpoint,
  )
}

fn deps_with_structured_agent_result(
  subject: process.Subject(String),
  result: result_artifact.ResultArtifact,
  checkpoint: workflow_checkpoint.Writer,
) -> workflow_run.Dependencies {
  let base = deps(subject, None)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      _issue,
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      process.send(
        subject,
        "agent:" <> context.step_id <> ":" <> prompt_text(prompt_mode),
      )
      Ok(success_agent_with_result(result))
    },
    checkpoint: checkpoint,
  )
}

fn prepare_fake_step(
  subject: process.Subject(String),
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  known: dict.Dict(String, workspace_run.PreparedStepWorkspace),
  profile: config_types.WorkspaceHookProfile,
  event_prefix: String,
) -> Result(workspace_run.PreparedStepWorkspace, workspace_run.PrepareError) {
  let source = case workspace_ref.from {
    None -> ""
    Some(name) ->
      case dict.get(known, name) {
        Ok(prepared) -> name <> "=" <> prepared.path
        Error(_) -> name <> "=missing"
      }
  }
  process.send(
    subject,
    event_prefix <> ":" <> step_id <> ":" <> workspace_ref.name <> ":" <> source,
  )
  Ok(workspace_run.PreparedStepWorkspace(
    workflow_id: workflow_id,
    run_id: run_id,
    run_root: "test/tmp/workflow-run/workspaces/implementation/ABC-123",
    workflow_bundle_dir: ".scherzo/workflows",
    attempt_index: attempt_index,
    workspace_name: workspace_ref.name,
    path: "test/tmp/workflow-run/workspaces/implementation/ABC-123/"
      <> workspace_ref.name,
    source_workspace_name: workspace_ref.from,
    source_workspace_path: case workspace_ref.from {
      None -> None
      Some(name) ->
        case dict.get(known, name) {
          Ok(prepared) -> Some(prepared.path)
          Error(_) -> None
        }
    },
    workspace_profile: profile.name,
  ))
}

pub fn default_command_step_receives_profile_driver_env_test() {
  let root = "test/tmp/workflow-run/profile-driver-env-command"
  reset_dir(root)
  let bin = root <> "/bin"
  let assert Ok(Nil) = simplifile.create_directory_all(bin)
  let helper = bin <> "/profile-helper"
  let assert Ok(Nil) =
    simplifile.write(helper, "#!/bin/sh\necho helper-found\n")
  chmod_executable(helper)
  let context =
    workflow_run.StepContext(
      workflow_id: "workflow",
      run_id: "run",
      run_root: root <> "/run",
      workflow_bundle_dir: ".scherzo/workflows",
      step_id: "step",
      attempt_index: 0,
      workspace_name: "main",
      workspace_path: root,
      workspace_context: workspace_driver_context.Context(
        profile: "isolated",
        driver: "driver-command",
        capabilities: [config_types.WorkspaceStatus],
        env: [
          #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
          #("PATH", absolute_path(bin)),
        ],
      ),
      config_dir: root,
      issue_id: "issue-id",
      issue_identifier: "ABC-123",
      run_kind: "issue",
      scheduled_job_id: "",
      schedule_due_at: "",
      schedule_started_at: "",
      run_attempt: 0,
      extra_pi_env: [],
    )
  let dependencies = workflow_run.default_dependencies()
  let artifact =
    dependencies.command_step(
      context,
      "printf '%s\\n' \"$SCHERZO_JJ_WORKSPACE_BASE\"; if command -v ls >/dev/null 2>&1; then echo unexpected-system-path; exit 1; fi; profile-helper",
      1000,
      [],
      test_limits(),
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert string.contains(artifact.stdout, "profile-base")
  assert string.contains(artifact.stdout, "helper-found")
  assert !string.contains(artifact.stdout, "unexpected-system-path")
}

pub fn workflow_command_redacts_sensitive_profile_driver_env_test() {
  let subject = process.new_subject()
  let redaction_probe = "redaction-probe-value-123"
  let profile =
    workspace_profile_with_driver_env(
      "isolated",
      dag_hooks(),
      config_types.ConfiguredWorkspaceDriver,
      "driver-command",
      [],
      [#("DRIVER_SECRET_TOKEN", redaction_probe)],
    )
  let base = orchestrator()
  let orchestrator =
    config_types.OrchestratorConfig(
      ..base,
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "isolated",
        profiles: dict.from_list([#("isolated", profile)]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: isolated\nsteps:\n  - id: leak\n    kind: command\n    run: leak\n    workspace: main\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout " <> redaction_probe,
          "stderr " <> redaction_probe,
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  let assert Some(step_id) = failure.failed_step_id
  let assert Ok(artifact) = dict.get(failure.artifacts, step_id)
  assert string.contains(artifact.stdout, "[REDACTED]")
  assert string.contains(artifact.stderr, "[REDACTED]")
  assert !string.contains(artifact.stdout, redaction_probe)
  assert !string.contains(artifact.stderr, redaction_probe)
  let report = workflow_run.failure_report(failure)
  assert string.contains(report, "[REDACTED]")
  assert !string.contains(report, redaction_probe)
}

pub fn context_recovery_exhausted_agent_failure_marks_artifact_summary_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: do it\n    workspace: main\n",
    )
  let context_ref =
    "runs/run-1/implement/attempt-1/context-recovery/context-window-exhausted.json"
  let result_ref =
    "runs/run-1/implement/attempt-1/context-recovery/attempt-2-result.json"
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Error(agent_types.WorkerFailure(
          reason: error.ContextRecoveryExhausted(
            recovery_method: "fresh_session",
            context_artifact_ref: Some(context_ref),
            result_artifact_ref: Some(result_ref),
            final_error: error.PiContextWindowExhausted(
              provider: Some("openai-codex"),
              provider_code: Some("context_length_exceeded"),
              detail: "too many input tokens",
            ),
          ),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  let assert Ok(artifact) = dict.get(failure.artifacts, "implement")
  assert string.contains(
    artifact.stderr,
    "context recovery attempted but exhausted",
  )
  assert string.contains(artifact.stderr, "recovery_method: fresh_session")
  assert string.contains(artifact.stderr, "provider: openai-codex")
  assert string.contains(artifact.summary_text, "context_recovery=failed")
  assert string.contains(artifact.summary_text, "recovery_exhausted=true")
  assert string.contains(artifact.summary_text, "recovery_method=fresh_session")
  assert string.contains(
    artifact.summary_text,
    "context_artifact=" <> context_ref,
  )
  assert string.contains(
    artifact.summary_text,
    "result_artifact=" <> result_ref,
  )
}

fn implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 3\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement prompt\n    workspace: main\n  - id: test_after_implement\n    kind: command\n    depends_on: [implement]\n    run: test command\n    workspace: main\n    on_failure: continue\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: code review prompt\n    workspace:\n      name: code-review\n      from: main\n  - id: apply_feedback\n    kind: agent\n    depends_on: [test_after_implement, code_review]\n    prompt: apply {{ steps.code_review.final_response }} {{ steps.test_after_implement.exit_code }}\n    workspace: main\n",
    )
  dag
}

fn structured_output_dag(required: Bool) -> workflow_dag.WorkflowDag {
  let required_text = case required {
    True -> "true"
    False -> "false"
  }
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: review prompt\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      required: "
      <> required_text
      <> "\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n",
    )
  dag
}

fn tool_call_structured_output_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: example prompt\n    workspace: main\n    structured_output:\n      artifact_name: example_artifact\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  dag
}

fn tool_call_result(
  final_response: Option(String),
  arguments_json: Option(String),
  status: Option(String),
  sibling_count: Int,
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(
    final_response,
    False,
    "test",
    [
      result_artifact.ToolCallSubmission(
        name: "submit_example_artifact",
        arguments_json: arguments_json,
        status: status,
        sibling_count: sibling_count,
        receipt_json: None,
      ),
    ],
  )
}

fn review_result_tool_call(
  arguments_json: Option(String),
  status: Option(String),
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(None, False, "test", [
    result_artifact.ToolCallSubmission(
      name: "submit_review_result",
      arguments_json: arguments_json,
      status: status,
      sibling_count: 1,
      receipt_json: None,
    ),
  ])
}

fn native_review_tool_call_result(
  arguments_json: Option(String),
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(None, False, "test", [
    result_artifact.ToolCallSubmission(
      name: "submit_review_lane_draft",
      arguments_json: arguments_json,
      status: Some("success"),
      sibling_count: 1,
      receipt_json: None,
    ),
  ])
}

fn native_review_lane_structured_output_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: lane_correctness\n    kind: agent\n    prompt: review prompt\n    workspace: main\n    on_failure: continue\n    structured_output:\n      artifact_name: correctness_draft\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n      validator: review_lane_draft\n      schema:\n        required: [schema_version, artifact_type, generated_at_utc, producer, lane, input_refs, draft_findings, review_notes, evidence_requests, self_check, remote_mutations]\n",
    )
  dag
}

fn native_review_lane_draft_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"workflow-run-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn native_review_lane_draft_missing_generated_at_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"producer\":{\"name\":\"workflow-run-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"category\":\"correctness\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn native_review_lane_draft_missing_lane_category_json() -> String {
  "{\"schema_version\":1,\"artifact_type\":\"review_lane_draft\",\"generated_at_utc\":\"2026-05-10T00:00:00Z\",\"producer\":{\"name\":\"workflow-run-test\",\"version\":\"1\",\"mode\":\"native\"},\"lane\":{\"id\":\"correctness\",\"name\":\"Correctness reviewer\",\"version\":\"1\"},\"input_refs\":[],\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"inspected_diff\":true,\"used_repository_relative_paths\":true},\"remote_mutations\":\"none\"}"
}

fn structured_output_downstream_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: review prompt\n    workspace: main\n    structured_output:\n      artifact_name: review_result\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n  - id: followup\n    kind: agent\n    depends_on: [review_json]\n    prompt: use {{ steps.review_json.structured_output.ref }} {{ steps.review_json.structured_output.path }}\n    workspace: main\n",
    )
  dag
}

fn final_message_record(content: String) -> pi_rpc.RpcRecord {
  let line =
    json.object([
      #("type", json.string("agent_end")),
      #(
        "messages",
        json.array(
          [
            json.object([
              #("role", json.string("assistant")),
              #("content", json.string(content)),
            ]),
          ],
          of: fn(value) { value },
        ),
      ),
    ])
    |> json.to_string
  let assert Ok(record) = pi_rpc.decode_record(line)
  record
}

fn over_display_limit_result(
  response: String,
) -> result_artifact.ResultArtifact {
  let display_result =
    result_artifact.from_records([final_message_record(response)], [], 40)
  let assert result_artifact.ResultArtifact(
    final_response: Some(display_response),
    truncated: True,
    source: "completed_assistant_messages",
    structured_response: Some(structured_response),
    structured_response_truncated: False,
    tool_calls: [],
  ) = display_result
  assert display_response == string.slice(response, 0, 40) <> "..."
  assert structured_response == response
  result_artifact.ResultArtifact(
    final_response: Some(display_response),
    truncated: True,
    source: "completed_assistant_messages",
    structured_response: Some(structured_response),
    structured_response_truncated: False,
    tool_calls: [
      result_artifact.ToolCallSubmission(
        name: "submit_review_result",
        arguments_json: Some(response),
        status: Some("success"),
        sibling_count: 1,
        receipt_json: None,
      ),
    ],
  )
}

fn command_dag_with_profile(profile: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: "
      <> profile
      <> "\nsteps:\n  - id: run\n    kind: command\n    run: echo ok\n    workspace: main\n",
    )
  dag
}

fn recovered_context(
  workflow_id: String,
  scheduler_statuses: dict.Dict(String, workflow_scheduler.StepRuntime),
  artifacts: dict.Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: dict.Dict(String, workspace_run.PreparedStepWorkspace),
  step_attempts: dict.Dict(String, Int),
) -> workflow_run.RecoveredRunContext {
  workflow_run.RecoveredRunContext(
    workflow_id: workflow_id,
    workflow_fingerprint: "wf-test",
    run_id: "run-1",
    run_root: "test/tmp/workflow-run/workspaces/implementation/ABC-123",
    recovery_evidence: workflow_outcome.NoStepRecovery,
    scheduler_statuses: scheduler_statuses,
    artifacts: artifacts,
    prepared_workspaces: prepared_workspaces,
    step_attempts: step_attempts,
    token_totals: session_tokens.zero_token_totals(),
    final_issue: None,
    turns: 0,
    warnings: [],
    pi_session_continuations: dict.new(),
    contract_inputs_recorded: None,
    contract_outputs_recorded: None,
  )
}

pub fn command_default_timeout_uses_builtin_default_test() {
  let subject = process.new_subject()
  let default_hooks = dag_hooks_with_timeout(1000)
  let noop_hooks = dag_hooks_with_timeout(42)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      dag_hooks: default_hooks,
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "default",
        profiles: dict.from_list([
          #(
            "default",
            workspace_profile(
              "default",
              default_hooks,
              config_types.SyntheticDefaultWorkspace,
            ),
          ),
          #(
            "noop",
            workspace_profile(
              "noop",
              noop_hooks,
              config_types.ConfiguredWorkspaceDriver,
            ),
          ),
        ]),
      ),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        timeout_ms,
        secrets,
        limits,
      ) {
        process.send(subject, "timeout:" <> int.to_string(timeout_ms))
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      command_dag_with_profile("noop"),
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(subject) == "prepare:run:main:"
  assert receive_event(subject) == "timeout:60000"
}

pub fn execute_rejects_missing_workspace_capabilities_before_prepare_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [config_types.WorkspaceStatus],
            ),
          ),
        ]),
      ),
    )

  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nworkspace_capabilities: [assert-only]\nsteps:\n  - id: run\n    kind: command\n    run: echo ok\n    workspace: main\n",
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      deps(subject, None),
    )

  assert string.starts_with(
    failure.reason,
    "workspace_capabilities_unavailable:",
  )
  assert string.contains(failure.reason, "missing: assert-only")
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn command_step_receives_workspace_driver_context_from_resolved_profile_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [
                config_types.WorkspaceAssertOnly,
                config_types.WorkspaceChangedFiles,
              ],
            ),
          ),
        ]),
      ),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout_ms,
        secrets,
        limits,
      ) {
        process.send(
          subject,
          "driver_env:"
            <> context.workspace_context.profile
            <> "|"
            <> context.workspace_context.driver
            <> "|"
            <> workspace_driver_context.serialize_capabilities(
            context.workspace_context.capabilities,
          ),
        )
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      command_dag_with_profile("dogfood-jj"),
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(subject) == "prepare:run:main:"
  assert receive_event(subject)
    == "driver_env:dogfood-jj|scripts/scherzo-workspace-jj|assert-only changed-files"
}

pub fn command_step_receives_discovered_workspace_driver_context_test() {
  let subject = process.new_subject()
  reset_dir("test/tmp/workflow-run")
  let hooks = dag_hooks_with_timeout(1000)
  let assert Ok(driver_path) = path.absolute("scripts/scherzo-workspace-noop")
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "noop",
        profiles: dict.from_list([
          #(
            "noop",
            workspace_profile_with_driver(
              "noop",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              driver_path,
              [],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(orchestrator) =
    workspace_driver_discovery.enrich_orchestrator(orchestrator)
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout_ms,
        secrets,
        limits,
      ) {
        process.send(
          subject,
          "driver_env:"
            <> context.workspace_context.profile
            <> "|"
            <> workspace_driver_context.serialize_capabilities(
            context.workspace_context.capabilities,
          ),
        )
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      command_dag_with_profile("noop"),
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(subject) == "prepare:run:main:"
  assert receive_event(subject)
    == "driver_env:noop|status changed-files assert-only"
}

pub fn packaged_noop_command_name_discovers_and_runs_driver_lifecycle_test() {
  let root = "test/tmp/workflow-run-packaged-noop-driver"
  reset_dir(root)
  let bin = write_noop_path_shim(root, root <> "/driver.log")
  let original_path = path.env("PATH")
  let assert Ok(Nil) = setenv("PATH", path_with_prefix(bin, original_path))

  let base = orchestrator()
  let profile =
    config_types.WorkspaceHookProfile(
      name: "noop",
      driver: Some(
        config_types.WorkspaceDriverConfig(
          command: "scherzo-workspace-noop",
          lifecycle: [
            config_types.LifecycleCreate,
            config_types.LifecycleBeforeStep,
            config_types.LifecycleAfterStep,
            config_types.LifecycleRemove,
          ],
          capabilities: [],
          timeout_ms: 5000,
          env: [],
        ),
      ),
      source: config_types.ConfiguredWorkspaceDriver,
    )
  let orchestrator =
    config_types.OrchestratorConfig(
      ..base,
      config_dir: root,
      effective: config_types.EffectiveConfig(
        ..base.effective,
        workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
      ),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "noop",
        profiles: dict.from_list([#("noop", profile)]),
      ),
    )
  let discovery_result =
    workspace_driver_discovery.enrich_orchestrator(orchestrator)
  let run_result = case discovery_result {
    Ok(orchestrator) -> {
      let assert Ok(dag) =
        workflow_dag.parse(
          "version: 1\nid: implementation\nworkspace_profile: noop\nworkspace_capabilities: [assert-only]\nsteps:\n  - id: collect_findings\n    kind: command\n    run: |\n      set -eu\n      printf 'findings\\n' > research-findings.md\n      driver=${SCHERZO_WORKSPACE_DRIVER:?SCHERZO_WORKSPACE_DRIVER is required}\n      \"$driver\" assert-only --path research-findings.md\n    workspace: main\n",
        )
      case
        workflow_run.execute(
          issue(),
          dag,
          orchestrator,
          empty_tracker(),
          [],
          "run-1",
          workflow_run.default_dependencies(),
        )
      {
        Ok(_) -> Ok(Nil)
        Error(failure) -> Error(failure.reason)
      }
    }
    Error(error) -> Error(workspace_driver_discovery.error_message(error))
  }
  restore_path(original_path)

  let assert Ok(enriched) = discovery_result
  let assert Ok(enriched_profile) =
    dict.get(enriched.workspace_profiles.profiles, "noop")
  let assert Some(driver) = enriched_profile.driver
  assert driver.command == "scherzo-workspace-noop"
  assert driver.capabilities
    == [
      config_types.WorkspaceStatus,
      config_types.WorkspaceChangedFiles,
      config_types.WorkspaceAssertOnly,
    ]
  let assert Ok(Nil) = run_result
  let assert Ok(log) = simplifile.read(root <> "/driver.log")
  assert string.contains(log, "describe --json")
  assert string.contains(log, "lifecycle create")
  assert string.contains(log, "lifecycle before-step")
  assert string.contains(log, "assert-only --path research-findings.md")
  assert string.contains(log, "lifecycle after-step")
  assert string.contains(log, "lifecycle remove")
  assert simplifile.is_directory(
      root <> "/workspaces/implementation/ABC-123/run-1",
    )
    == Ok(False)
}

pub fn default_agent_step_receives_workspace_driver_environment_test() {
  let root = "test/tmp/workflow-run-agent-driver-env"
  reset_dir(root)
  let assert Ok(env_log) = path.absolute(root <> "/pi-env.log")
  let profile_path =
    path_with_prefix(absolute_path(root <> "/profile-bin"), path.env("PATH"))
  let hooks = dag_hooks_with_timeout(1000)
  let base = orchestrator()
  let orchestrator =
    config_types.OrchestratorConfig(
      ..base,
      effective: config_types.EffectiveConfig(
        ..base.effective,
        workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
        pi: config_types.PiConfig(
          ..base.effective.pi,
          command: "FAKE_PI_ENV_LOG="
            <> shell_quote(env_log)
            <> " "
            <> shell_quote(fake_pi()),
          compatibility_probe: False,
        ),
      ),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver_env(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [config_types.WorkspaceAssertOnly],
              [
                #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
                #("PATH", profile_path),
              ],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nsteps:\n  - id: implement\n    kind: agent\n    prompt: check env\n    workspace: main\n",
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      workflow_run.default_dependencies(),
    )

  let assert Ok(log) = simplifile.read(env_log)
  assert string.contains(log, "SCHERZO_WORKSPACE_PROFILE=dogfood-jj")
  assert string.contains(
    log,
    "SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj",
  )
  assert string.contains(log, "SCHERZO_WORKSPACE_CAPABILITIES=assert-only")
  assert string.contains(log, "SCHERZO_JJ_WORKSPACE_BASE=profile-base")
  assert string.contains(log, "PATH=" <> profile_path)
  assert string.contains(log, "SCHERZO_WORKSPACE_NAME=main")
  assert string.contains(log, "SCHERZO_WORKSPACE_PATH=")
}

fn agent_driver_env_step_context(
  root: String,
  profile_env: List(#(String, String)),
) -> workflow_run.StepContext {
  workflow_run.StepContext(
    workflow_id: "workflow",
    run_id: "run-1",
    run_root: root <> "/run",
    workflow_bundle_dir: ".scherzo/workflows",
    step_id: "implement",
    attempt_index: 0,
    workspace_name: "main",
    workspace_path: root,
    workspace_context: workspace_driver_context.Context(
      profile: "dogfood-jj",
      driver: "scripts/scherzo-workspace-jj",
      capabilities: [config_types.WorkspaceAssertOnly],
      env: profile_env,
    ),
    config_dir: root,
    issue_id: "issue-id",
    issue_identifier: "ABC-123",
    run_kind: "issue",
    scheduled_job_id: "",
    schedule_due_at: "",
    schedule_started_at: "",
    run_attempt: 0,
    extra_pi_env: [],
  )
}

fn agent_attempt_context(root: String) -> workflow_attempt.StepAttemptContext {
  workflow_attempt.StepAttemptContext(
    run_id: "run-1",
    issue_id: "issue-id",
    issue_identifier: "ABC-123",
    workflow_id: "workflow",
    workflow_fingerprint: "fingerprint",
    step_id: "implement",
    workspace_name: "main",
    attempt_index: 0,
    workspace_path: root,
    continuation_capable: False,
    continuation_session_file: None,
  )
}

pub fn default_agent_step_argv_mode_receives_profile_driver_environment_test() {
  let root = "test/tmp/workflow-run-agent-driver-env-argv"
  reset_dir(root)
  let assert Ok(env_log) = path.absolute(root <> "/pi-env.log")
  let profile_path =
    path_with_prefix(absolute_path(root <> "/profile-bin"), path.env("PATH"))
  let base = orchestrator()
  let effective =
    config_types.EffectiveConfig(
      ..base.effective,
      workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
      pi: config_types.PiConfig(
        ..base.effective.pi,
        command: "unused-shell-pi-command",
        compatibility_probe: False,
        argv_command: Some(
          config_types.PiArgvCommand(executable: fake_pi(), args: [], env: [
            #("FAKE_PI_ENV_LOG", env_log),
            #("SCHERZO_JJ_WORKSPACE_BASE", "pi-base"),
            #("PATH", "pi-base-path"),
          ]),
        ),
        session_persistence: config_types.PiSessionPersistenceConfig(
          enabled: True,
          recovery_prompt: "recover",
        ),
      ),
    )
  let context =
    agent_driver_env_step_context(root, [
      #("SCHERZO_JJ_WORKSPACE_BASE", "profile-base"),
      #("PATH", profile_path),
    ])

  let assert Ok(_) =
    workflow_run.default_dependencies().agent_step(
      issue(),
      context,
      workflow_attempt.OriginalPrompt("check env"),
      agent_attempt_context(root),
      effective,
      empty_tracker(),
      fn(_) { Nil },
      fn(_) { Nil },
      fn(_) { Nil },
    )

  let assert Ok(log) = simplifile.read(env_log)
  assert string.contains(log, "SCHERZO_WORKSPACE_PROFILE=dogfood-jj")
  assert string.contains(
    log,
    "SCHERZO_WORKSPACE_DRIVER=scripts/scherzo-workspace-jj",
  )
  assert string.contains(log, "SCHERZO_WORKSPACE_CAPABILITIES=assert-only")
  assert string.contains(log, "SCHERZO_JJ_WORKSPACE_BASE=profile-base")
  assert string.contains(log, "PATH=" <> profile_path)
  assert !string.contains(log, "SCHERZO_JJ_WORKSPACE_BASE=pi-base")
  assert !string.contains(log, "PATH=pi-base-path")
}

pub fn default_agent_step_redacts_sensitive_profile_driver_env_updates_test() {
  let root = "test/tmp/workflow-run-agent-driver-env-redaction"
  reset_dir(root)
  let redaction_probe = "redaction-probe-value-123"
  let update_subject = process.new_subject()
  let base = orchestrator()
  let effective =
    config_types.EffectiveConfig(
      ..base.effective,
      workspace: config_types.WorkspaceConfig(root: root <> "/workspaces"),
      pi: config_types.PiConfig(
        ..base.effective.pi,
        command: "FAKE_PI_MESSAGE_SECRET="
          <> shell_quote(redaction_probe)
          <> " "
          <> shell_quote(fake_pi()),
        compatibility_probe: False,
      ),
    )

  let assert Ok(_) =
    workflow_run.default_dependencies().agent_step(
      issue(),
      agent_driver_env_step_context(root, [
        #("DRIVER_SECRET_TOKEN", redaction_probe),
      ]),
      workflow_attempt.OriginalPrompt("emit secret"),
      agent_attempt_context(root),
      effective,
      empty_tracker(),
      emit_update_text(update_subject),
      fn(_) { Nil },
      fn(_) { Nil },
    )

  let updates =
    string.join(test_async.drain_subject(update_subject), with: "\n")
  assert string.contains(updates, "[REDACTED]")
  assert !string.contains(updates, redaction_probe)
}

fn emit_update_text(
  subject: process.Subject(String),
) -> fn(agent_types.RunnerUpdate) -> Nil {
  fn(update) {
    case update {
      agent_types.RunnerTurnUpdate(_) -> Nil
      agent_types.RunnerPiUpdate(update) -> {
        emit_optional_text(subject, update.message)
        emit_optional_text(subject, update.tool_input)
        emit_optional_text(subject, update.tool_output)
        case update.raw_json {
          Some(raw) -> {
            let session_event.RedactedRawJson(value: value, truncated: _) = raw
            process.send(subject, value)
          }
          None -> Nil
        }
      }
    }
  }
}

fn emit_optional_text(
  subject: process.Subject(String),
  value: Option(String),
) -> Nil {
  case value {
    Some(value) -> process.send(subject, value)
    None -> Nil
  }
}

pub fn workspace_driver_context_resolves_repo_root_placeholder_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      config_dir: "test/tmp/workflow-run/.scherzo",
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "$SCHERZO_REPO_ROOT/scripts/scherzo-workspace-jj",
              [config_types.WorkspaceAssertOnly],
            ),
          ),
        ]),
      ),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout_ms,
        secrets,
        limits,
      ) {
        process.send(subject, "driver:" <> context.workspace_context.driver)
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      command_dag_with_profile("dogfood-jj"),
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(subject) == "prepare:run:main:"
  let driver = receive_event(subject)
  assert string.starts_with(driver, "driver:")
  assert !string.contains(driver, "$SCHERZO_REPO_ROOT")
  assert string.ends_with(driver, "/scripts/scherzo-workspace-jj")
}

pub fn workflow_yaml_cannot_override_workspace_driver_context_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [config_types.WorkspaceAssertOnly],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nworkspace_capabilities: [assert-only]\nworkspace_driver: scripts/malicious-driver\nsteps:\n  - id: run\n    kind: command\n    run: echo ok\n    workspace: main\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout_ms,
        secrets,
        limits,
      ) {
        process.send(subject, "driver:" <> context.workspace_context.driver)
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(subject) == "prepare:run:main:"
  assert receive_event(subject) == "driver:scripts/scherzo-workspace-jj"
}

pub fn agent_prompt_renders_workspace_driver_locals_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [
                config_types.WorkspaceAssertOnly,
                config_types.WorkspaceChangedFiles,
              ],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nsteps:\n  - id: implement\n    kind: agent\n    prompt: 'driver={{ workspace.driver }} profile={{ workspace.profile }} caps={% for capability in workspace.capabilities %}{{ capability }};{% endfor %}'\n    workspace: main\n",
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      deps(subject, None),
    )

  assert receive_event(subject) == "prepare:implement:main:"
  let agent_event = receive_event(subject)
  assert string.contains(agent_event, "driver=scripts/scherzo-workspace-jj")
  assert string.contains(agent_event, "profile=dogfood-jj")
  assert string.contains(agent_event, "assert-only;")
  assert string.contains(agent_event, "changed-files;")
}

pub fn agent_prompt_preserves_artifact_locals_with_workspace_driver_locals_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [config_types.WorkspaceAssertOnly],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n  - id: summarize\n    kind: agent\n    depends_on: [collect]\n    prompt: 'artifact={{ steps.collect.stdout }} driver={{ workspace.driver }}'\n    workspace: main\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout_ms,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        step_artifact.from_command_result(
          context.step_id,
          0,
          "artifact-value",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let agent_event = receive_event_with_prefix(subject, "agent:", 5)
  assert string.contains(agent_event, "artifact=artifact-value")
  assert string.contains(agent_event, "driver=scripts/scherzo-workspace-jj")
}

pub fn recovery_prompt_does_not_rerender_workspace_driver_locals_test() {
  let subject = process.new_subject()
  let hooks = dag_hooks_with_timeout(1000)
  let orchestrator =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      workspace_profiles: config_types.WorkspaceHookProfiles(
        default_profile: "dogfood-jj",
        profiles: dict.from_list([
          #(
            "dogfood-jj",
            workspace_profile_with_driver(
              "dogfood-jj",
              hooks,
              config_types.ConfiguredWorkspaceDriver,
              "scripts/scherzo-workspace-jj",
              [config_types.WorkspaceAssertOnly],
            ),
          ),
        ]),
      ),
    )
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nworkspace_profile: dogfood-jj\nsteps:\n  - id: resume\n    kind: agent\n    prompt: ORIGINAL {{ workspace.driver }}\n    workspace: main\n",
    )
  let context =
    workflow_run.RecoveredRunContext(
      ..recovered_context(
        dag.id,
        dict.new(),
        dict.new(),
        dict.new(),
        dict.from_list([#("resume", 1)]),
      ),
      pi_session_continuations: dict.from_list([
        #(
          "resume",
          workflow_attempt.PiContinuation(
            run_id: "run-1",
            issue_id: "issue-id",
            issue_identifier: "ABC-123",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-test",
            step_id: "resume",
            workspace_name: "main",
            attempt_index: 1,
            workspace_path: "test/tmp/workflow-run/workspaces/implementation/ABC-123/main",
            session_id: "pi-session",
            session_file: "test/tmp/session.json",
            recovery_prompt: "RECOVERY {{ workspace.driver }}",
          ),
        ),
      ]),
    )

  let assert Ok(_) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator,
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      deps(subject, None),
    )

  assert receive_event(subject) == "prepare_recovered:resume:main:"
  let agent_event = receive_event(subject)
  assert string.contains(agent_event, "RECOVERY {{ workspace.driver }}")
}

pub fn workflow_run_fans_out_fans_in_and_renders_artifacts_test() {
  let subject = process.new_subject()
  let result =
    workflow_run.execute(
      issue(),
      implementation_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps(subject, None),
    )
  let assert Ok(success) = result
  assert success.run_root
    == "test/tmp/workflow-run/workspaces/implementation/ABC-123"
  let assert Some(text) = success.worker_success.result.final_response
  assert string.contains(text, "response:apply response:code review prompt 0")

  assert receive_event(subject) == "prepare:implement:main:"
  assert receive_event(subject)
    == "agent:test/tmp/workflow-run/workspaces/implementation/ABC-123/main:implement prompt"
  assert receive_event(subject) == "after:implement"
  assert receive_event(subject) == "prepare:test_after_implement:main:"
  assert receive_event(subject)
    == "prepare:code_review:code-review:main=test/tmp/workflow-run/workspaces/implementation/ABC-123/main"
  let fanout_events = receive_events(subject, 4, [])
  assert list.contains(fanout_events, "run:test_after_implement")
  assert list.contains(fanout_events, "after:test_after_implement")
  assert list.contains(
    fanout_events,
    "agent:test/tmp/workflow-run/workspaces/implementation/ABC-123/code-review:code review prompt",
  )
  assert list.contains(fanout_events, "after:code_review")
  assert receive_event(subject) == "prepare:apply_feedback:main:"
  assert receive_event(subject)
    == "agent:test/tmp/workflow-run/workspaces/implementation/ABC-123/main:apply response:code review prompt 0"
  assert receive_event(subject) == "after:apply_feedback"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
}

pub fn valid_json_final_response_becomes_retained_structured_artifact_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/structured-valid"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      ["token-123"],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        review_result_tool_call(
          Some("{\"summary\":\"token-123\",\"findings\":[\"token-123\"]}"),
          Some("success"),
        ),
        checkpoint,
      ),
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "review_json")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  assert metadata.ref
    == "runs/run-1/review_json/attempt-1/structured/review_result.json"
  let assert Ok(contents) = simplifile.read(metadata.path)
  assert string.contains(contents, "\"artifact_type\":\"structured_output\"")
  assert string.contains(contents, "[REDACTED]")
  assert !string.contains(contents, "token-123")
}

pub fn pi_tool_call_source_persists_retained_structured_artifact_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/tool-source-valid"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let result =
    tool_call_result(
      Some(
        "{\"schema_version\":999,\"artifact_type\":\"final_json_should_be_ignored\"}",
      ),
      Some(
        "{\"schema_version\":1,\"artifact_type\":\"example\",\"secret\":\"token-123\"}",
      ),
      Some("success"),
      1,
    )
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      tool_call_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      ["token-123"],
      "run-1",
      deps_with_structured_agent_result(subject, result, checkpoint),
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "example_json")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  assert metadata.ref
    == "runs/run-1/example_json/attempt-1/structured/example_artifact.json"
  assert metadata.source_type == "pi_tool_call"
  assert metadata.source_tool_name == Some("submit_example_artifact")
  let assert Ok(contents) = simplifile.read(metadata.path)
  assert string.contains(contents, "\"artifact_type\":\"example\"")
  assert string.contains(contents, "[REDACTED]")
  assert !string.contains(contents, "final_json_should_be_ignored")
}

pub fn final_json_without_configured_tool_call_source_fails_test() {
  let subject = process.new_subject()
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      tool_call_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(
        subject,
        Some("{\"schema_version\":1,\"artifact_type\":\"example\"}"),
        False,
        workflow_checkpoint.noop_writer(),
      ),
    )

  assert failure.failed_step_id == Some("example_json")
  assert string.contains(failure.reason, "structured_output_tool_call_missing")
  let assert Ok(artifact) = dict.get(failure.artifacts, "example_json")
  assert artifact.failure_code == Some("structured_output_tool_call_missing")
}

pub fn native_review_missing_generated_at_retries_and_records_diagnostics_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/structured-retry"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        let prompt = prompt_text(prompt_mode)
        process.send(subject, "agent:" <> context.step_id <> ":" <> prompt)
        case prompt_mode {
          workflow_attempt.StructuredOutputRetryPrompt(_) ->
            Ok(
              success_agent_with_result(
                native_review_tool_call_result(
                  Some(native_review_lane_draft_json()),
                ),
              ),
            )
          _ ->
            Ok(
              success_agent_with_result(
                native_review_tool_call_result(
                  Some(native_review_lane_draft_missing_generated_at_json()),
                ),
              ),
            )
        }
      },
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      native_review_lane_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let _initial_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  let retry_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  assert string.contains(retry_agent, "Scherzo structured-output retry")
  assert string.contains(
    retry_agent,
    "Structured-output artifact name: correctness_draft",
  )
  assert string.contains(retry_agent, "generated_at_utc")
  assert !string.contains(
    retry_agent,
    native_review_lane_draft_missing_generated_at_json(),
  )
  let assert Ok(artifact) = dict.get(success.artifacts, "lane_correctness")
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.failure_code == None
  assert string.contains(
    artifact.summary_text,
    "structured_output_retry=succeeded",
  )
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  let assert Some(retry) = metadata.retry
  assert retry.max_retries == 1
  assert retry.attempts == 2
  assert retry.outcome == "succeeded"
  let assert [initial, retried] = retry.diagnostics
  assert initial.status == "error"
  assert initial.failure_code == Some("structured_output_schema_invalid")
  assert string.contains(initial.message, "generated_at_utc")
  assert retried.status == "valid"
  let assert Ok(_) = simplifile.read(metadata.path)
}

pub fn native_review_missing_nested_lane_metadata_retries_and_records_diagnostics_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/structured-nested-retry"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        let prompt = prompt_text(prompt_mode)
        process.send(subject, "agent:" <> context.step_id <> ":" <> prompt)
        case prompt_mode {
          workflow_attempt.StructuredOutputRetryPrompt(_) ->
            Ok(
              success_agent_with_result(
                native_review_tool_call_result(
                  Some(native_review_lane_draft_json()),
                ),
              ),
            )
          _ ->
            Ok(
              success_agent_with_result(
                native_review_tool_call_result(
                  Some(native_review_lane_draft_missing_lane_category_json()),
                ),
              ),
            )
        }
      },
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      native_review_lane_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let _initial_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  let retry_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  assert string.contains(retry_agent, "Scherzo structured-output retry")
  assert string.contains(
    retry_agent,
    "Structured-output artifact name: correctness_draft",
  )
  assert string.contains(retry_agent, "lane.category")
  let assert Ok(artifact) = dict.get(success.artifacts, "lane_correctness")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  let assert Some(retry) = metadata.retry
  assert retry.outcome == "succeeded"
  let assert [initial, retried] = retry.diagnostics
  assert initial.status == "error"
  assert initial.failure_code == Some("structured_output_command_rejected")
  assert string.contains(initial.message, "lane.category")
  assert retried.status == "valid"
  let assert Ok(_) = simplifile.read(metadata.path)
}

pub fn native_review_transient_pi_termination_retries_once_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/native-transient-retry"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        let prompt = prompt_text(prompt_mode)
        process.send(subject, "agent:" <> context.step_id <> ":" <> prompt)
        case prompt_mode {
          workflow_attempt.StructuredOutputRetryPrompt(_) ->
            Ok(
              success_agent_with_result(
                native_review_tool_call_result(
                  Some(native_review_lane_draft_json()),
                ),
              ),
            )
          _ ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError(
                "pi message_end reported stopReason=error: terminated",
              )),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: Some(issue()),
            ))
        }
      },
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      native_review_lane_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let _initial_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  let retry_agent =
    receive_event_with_prefix(subject, "agent:lane_correctness:", 10)
  assert string.contains(retry_agent, "Scherzo structured-output retry")
  assert string.contains(retry_agent, "agent_pi_failed")
  assert string.contains(
    retry_agent,
    "Structured-output artifact name: correctness_draft",
  )
  let assert Ok(artifact) = dict.get(success.artifacts, "lane_correctness")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  let assert Some(retry) = metadata.retry
  assert retry.max_retries == 1
  assert retry.attempts == 2
  assert retry.outcome == "succeeded"
  let assert [initial, retried] = retry.diagnostics
  assert initial.status == "agent_failure"
  assert initial.failure_code == Some("agent_pi_failed")
  assert string.contains(initial.message, "terminated")
  assert retried.status == "valid"
  let assert Ok(_) = simplifile.read(metadata.path)
}

pub fn over_display_limit_valid_json_still_retains_structured_artifact_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/structured-over-limit"
  reset_dir(root)
  let large_summary = string.repeat("x", times: 180)
  let response = "{\"summary\":\"" <> large_summary <> "\",\"findings\":[]}"
  let result = over_display_limit_result(response)

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        result,
        workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      ),
    )

  let assert Ok(step_artifact.StepArtifact(
    status: step_artifact.StepSucceeded,
    final_response_truncated: True,
    structured_output: Some(step_artifact.StructuredOutputValid(metadata)),
    ..,
  )) = dict.get(success.artifacts, "review_json")
  let assert Ok(contents) = simplifile.read(metadata.path)
  assert string.contains(contents, large_summary)
}

pub fn over_display_limit_malformed_json_reports_invalid_not_truncated_test() {
  let subject = process.new_subject()
  let response = "{\"summary\":\"" <> string.repeat("x", times: 180)
  let result = over_display_limit_result(response)

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        result,
        workflow_checkpoint.noop_writer(),
      ),
    )

  assert failure.failed_step_id == Some("review_json")
  assert string.contains(
    failure.reason,
    "structured_output_tool_call_arguments_invalid",
  )
  let assert Ok(step_artifact.StepArtifact(
    status: step_artifact.StepFailed,
    final_response_truncated: True,
    failure_code: Some("structured_output_tool_call_arguments_invalid"),
    structured_output: Some(step_artifact.StructuredOutputError(
      _,
      _,
      message,
      _,
      _,
    )),
    ..,
  )) = dict.get(failure.artifacts, "review_json")
  assert string.contains(message, "arguments were not valid JSON")
}

pub fn invalid_json_structured_output_fails_agent_step_clearly_test() {
  let subject = process.new_subject()
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        review_result_tool_call(Some("not json"), Some("success")),
        workflow_checkpoint.noop_writer(),
      ),
    )

  assert string.contains(
    failure.reason,
    "structured_output_tool_call_arguments_invalid",
  )
  assert failure.failed_step_id == Some("review_json")
  let assert Ok(artifact) = dict.get(failure.artifacts, "review_json")
  assert artifact.status == step_artifact.StepFailed
  assert artifact.failure_code
    == Some("structured_output_tool_call_arguments_invalid")
  let assert Some(step_artifact.StructuredOutputError(
    _,
    _,
    message,
    _,
    Some(retry),
  )) = artifact.structured_output
  assert string.contains(message, "review_json")
  assert retry.outcome == "failed"
  assert retry.attempts == 2
  let assert [initial, retried] = retry.diagnostics
  assert initial.failure_code
    == Some("structured_output_tool_call_arguments_invalid")
  assert retried.failure_code
    == Some("structured_output_tool_call_arguments_invalid")
}

pub fn missing_required_structured_output_fails_agent_step_clearly_test() {
  let subject = process.new_subject()
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(
        subject,
        None,
        False,
        workflow_checkpoint.noop_writer(),
      ),
    )

  assert string.contains(failure.reason, "structured_output_tool_call_missing")
  let assert Ok(artifact) = dict.get(failure.artifacts, "review_json")
  assert artifact.failure_code == Some("structured_output_tool_call_missing")
}

pub fn optional_missing_structured_output_succeeds_without_artifact_test() {
  let subject = process.new_subject()
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      structured_output_dag(False),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(
        subject,
        None,
        False,
        workflow_checkpoint.noop_writer(),
      ),
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "review_json")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputAbsent(_, _, _)) =
    artifact.structured_output
  let locals =
    step_artifact.to_template_locals(
      dict.from_list([#("review_json", artifact)]),
    )
  let assert Ok(status) =
    list.key_find(locals, "steps.review_json.structured_output.status")
  assert status == template.VString("absent")
  let assert Ok(path) =
    list.key_find(locals, "steps.review_json.structured_output.path")
  assert path == template.VNil
}

pub fn structured_artifact_write_failure_fails_step_without_metadata_test() {
  let subject = process.new_subject()
  let checkpoint =
    workflow_checkpoint.Writer(
      ..workflow_checkpoint.noop_writer(),
      write_structured_output_artifact: fn(_) {
        Error(workflow_checkpoint.CheckpointArtifactFailed(
          "structured write failed",
        ))
      },
    )
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      structured_output_dag(True),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        review_result_tool_call(
          Some("{\"summary\":\"ok\",\"findings\":[]}"),
          Some("success"),
        ),
        checkpoint,
      ),
    )

  assert string.contains(
    failure.reason,
    "structured_output_artifact_write_failed",
  )
  let assert Ok(artifact) = dict.get(failure.artifacts, "review_json")
  assert artifact.failure_code
    == Some("structured_output_artifact_write_failed")
  let assert Some(step_artifact.StructuredOutputError(_, _, message, _, _)) =
    artifact.structured_output
  assert string.contains(message, "structured write failed")
}

pub fn structured_artifact_metadata_available_to_downstream_template_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/structured-downstream"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        let prompt = prompt_text(prompt_mode)
        case context.step_id {
          "review_json" ->
            Ok(
              success_agent_with_result(review_result_tool_call(
                Some("{\"summary\":\"ok\",\"findings\":[]}"),
                Some("success"),
              )),
            )
          _ -> {
            process.send(subject, "rendered:" <> prompt)
            Ok(success_agent(prompt))
          }
        }
      },
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      structured_output_downstream_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let rendered = receive_event_with_prefix(subject, "rendered:", 20)
  assert string.contains(
    rendered,
    "runs/run-1/review_json/attempt-1/structured/review_result.json",
  )
  assert string.contains(rendered, ".scherzo-state/artifacts/")
  let assert Ok(artifact) = dict.get(success.artifacts, "review_json")
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  let assert Ok(_) = simplifile.read(metadata.path)
}

pub fn workflow_without_structured_output_behaves_unchanged_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: plain\n    kind: agent\n    prompt: plain prompt\n    workspace: main\n",
    )
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps(subject, None),
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "plain")
  assert artifact.final_response == Some("response:plain prompt")
  assert artifact.structured_output == None
  let locals =
    step_artifact.to_template_locals(dict.from_list([#("plain", artifact)]))
  let assert Ok(status) =
    list.key_find(locals, "steps.plain.structured_output.status")
  assert status == template.VString("not_configured")
}

pub fn workflow_run_completed_cleanup_failure_marks_failed_terminal_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      cleanup_run: fn(run_root, _orchestrator, _profile) {
        process.send(subject, "cleanup:" <> run_root)
        Error(error.WorkspaceIo("delete failed"))
      },
      checkpoint: workflow_checkpoint.Writer(
        ..workflow_checkpoint.noop_writer(),
        workflow_finished: fn(finished: workflow_checkpoint.WorkflowFinished) {
          process.send(subject, "workflow_finished:" <> finished.outcome)
          Ok(Nil)
        },
      ),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "cleanup_failed:workspace_io"
  assert receive_event(subject) == "prepare:collect:main:"
  assert receive_event(subject) == "run:collect"
  assert receive_event(subject) == "after:collect"
  assert receive_event(subject) == "workflow_finished:completed"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  assert receive_event(subject) == "workflow_finished:failed_fatal"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_on_failure_continue_makes_artifact_available_test() {
  let subject = process.new_subject()
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      implementation_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps(subject, Some("test_after_implement")),
    )
  let assert Some(text) = success.worker_success.result.final_response
  assert string.contains(text, "response:apply response:code review prompt 1")
}

pub fn failed_recovery_finalizer_blocks_downstream_steps_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: gate\n    kind: command\n    run: gate\n    workspace: main\n    on_failure: continue\n  - id: classify\n    kind: command\n    depends_on: [gate]\n    run: classify\n    workspace: main\n  - id: finalize\n    kind: command\n    depends_on: [classify]\n    run: finalize\n    workspace: main\n  - id: review\n    kind: command\n    depends_on: [finalize]\n    run: review\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        let exit_code = case
          context.step_id == "gate" || context.step_id == "finalize"
        {
          True -> 1
          False -> 0
        }
        step_artifact.from_command_result(
          context.step_id,
          exit_code,
          "stdout:" <> context.step_id,
          "stderr:" <> context.step_id,
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.failed_step_id == Some("finalize")
  assert receive_event(subject) == "prepare:gate:main:"
  assert receive_event(subject) == "run:gate"
  assert receive_event(subject) == "after:gate"
  assert receive_event(subject) == "prepare:classify:main:"
  assert receive_event(subject) == "run:classify"
  assert receive_event(subject) == "after:classify"
  assert receive_event(subject) == "prepare:finalize:main:"
  assert receive_event(subject) == "run:finalize"
  assert receive_event(subject) == "after:finalize"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn recovered_completed_upstream_step_is_not_rerun_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n  - id: summarize\n    kind: command\n    depends_on: [collect]\n    run: summarize\n    workspace: main\n",
    )
  let collect_artifact =
    step_artifact.from_command_result(
      "collect",
      0,
      "collected",
      "",
      False,
      [],
      orchestrator().artifact_limits,
    )
  let subject = process.new_subject()
  let context =
    recovered_context(
      dag.id,
      dict.from_list([#("collect", workflow_scheduler.Succeeded)]),
      dict.from_list([#("collect", collect_artifact)]),
      dict.new(),
      dict.new(),
    )
  let assert Ok(success) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      deps(subject, None),
    )

  assert dict.has_key(success.artifacts, "collect")
  assert dict.has_key(success.artifacts, "summarize")
  assert receive_event(subject) == "prepare_recovered:summarize:main:"
  assert receive_event(subject) == "run:summarize"
  assert receive_event(subject) == "after:summarize"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn recovered_failed_continued_artifact_feeds_downstream_prompt_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: lint\n    kind: command\n    run: lint\n    workspace: main\n    on_failure: continue\n  - id: repair\n    kind: agent\n    depends_on: [lint]\n    prompt: fix {{ steps.lint.stderr }} {{ steps.lint.exit_code }}\n    workspace: main\n",
    )
  let lint_artifact =
    step_artifact.from_command_result(
      "lint",
      1,
      "",
      "lint failed",
      False,
      [],
      orchestrator().artifact_limits,
    )
  let subject = process.new_subject()
  let context =
    recovered_context(
      dag.id,
      dict.from_list([#("lint", workflow_scheduler.FailedContinued)]),
      dict.from_list([#("lint", lint_artifact)]),
      dict.new(),
      dict.new(),
    )
  let assert Ok(success) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      deps(subject, None),
    )

  let assert Ok(restored_lint) = dict.get(success.artifacts, "lint")
  assert restored_lint == lint_artifact
  assert receive_event(subject) == "prepare_recovered:repair:main:"
  let agent_event = receive_event(subject)
  assert string.contains(agent_event, "lint failed")
  assert string.contains(agent_event, "1")
}

pub fn recovered_pi_resume_validation_failure_is_fatal_even_with_continue_policy_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: resume\n    kind: agent\n    prompt: ORIGINAL_PROMPT_SHOULD_NOT_APPEAR\n    workspace: main\n    on_failure: continue\n  - id: downstream\n    kind: command\n    depends_on: [resume]\n    run: downstream\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(
          subject,
          "agent_prompt:" <> context.step_id <> ":" <> prompt_text(prompt_mode),
        )
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError(
            workflow_attempt.recovery_pi_resume_validation_failed,
          )),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: Some(issue()),
        ))
      },
    )
  let context =
    workflow_run.RecoveredRunContext(
      ..recovered_context(
        dag.id,
        dict.new(),
        dict.new(),
        dict.new(),
        dict.from_list([#("resume", 1)]),
      ),
      pi_session_continuations: dict.from_list([
        #(
          "resume",
          workflow_attempt.PiContinuation(
            run_id: "run-1",
            issue_id: "issue-id",
            issue_identifier: "ABC-123",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-test",
            step_id: "resume",
            workspace_name: "main",
            attempt_index: 1,
            workspace_path: "test/tmp/workflow-run/workspaces/implementation/ABC-123/main",
            session_id: "pi-session",
            session_file: "test/tmp/session.json",
            recovery_prompt: "RECOVERY_PROMPT_MARKER",
          ),
        ),
      ]),
    )

  let assert Error(failure) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      dependencies,
    )

  assert failure.agent_reason
    == Some(
      error.PiFailed(error.PiProtocolError(
        workflow_attempt.recovery_pi_resume_validation_failed,
      )),
    )
  assert receive_event(subject) == "prepare_recovered:resume:main:"
  assert receive_event(subject) == "agent_prompt:resume:RECOVERY_PROMPT_MARKER"
  assert receive_event(subject) == "after:resume"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn recovered_start_checkpoint_failure_does_not_cleanup_before_attempt_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: build\n    kind: command\n    run: build\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      checkpoint: workflow_checkpoint.Writer(
        ..workflow_checkpoint.noop_writer(),
        step_started: fn(_, _, _, _, _, _, _) {
          Error(workflow_checkpoint.CheckpointAppendFailed("start failed"))
        },
      ),
    )
  let context =
    recovered_context(dag.id, dict.new(), dict.new(), dict.new(), dict.new())
  let assert Error(failure) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      dependencies,
    )

  assert failure.reason == "checkpoint_failed:start failed"
  assert failure.run_root
    == Some("test/tmp/workflow-run/workspaces/implementation/ABC-123")
  assert receive_event(subject) == "prepare_recovered:build:main:"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn recovered_prepare_failure_interrupts_stale_prepared_attempt_before_terminal_failure_test() {
  let root =
    "test/tmp/workflow-run/recovered-prepare-failure-interrupts-stale-attempt"
  let subject = process.new_subject()
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: docs\n    kind: command\n    run: docs\n    workspace: docs\n  - id: tests\n    kind: command\n    run: tests\n    workspace: tests\n  - id: finish\n    kind: command\n    depends_on: [docs, tests]\n    run: finish\n    workspace: main\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps_with_prepare_recovered_failure(subject, "tests"),
      checkpoint: recording_checkpoint(root, subject),
    )
  let context =
    recovered_context(dag.id, dict.new(), dict.new(), dict.new(), dict.new())

  let assert Error(failure) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      dependencies,
    )

  assert failure.reason == "workspace_failed:workspace_io"
  assert receive_event(subject) == "prepare_recovered:docs:docs:"
  assert receive_event(subject) == "prepare_recovered_failed:tests"
  assert receive_event_with_prefix(subject, "workflow_finished:", 20)
    == "workflow_finished:failed_fatal"
  assert has_step_interrupted_before_workflow_finished(
    root,
    "docs",
    "terminal_failure",
  )
}

pub fn parallel_recovery_runs_only_interrupted_branch_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: docs\n    kind: command\n    run: docs\n    workspace: docs\n  - id: tests\n    kind: command\n    run: tests\n    workspace: tests\n  - id: final\n    kind: command\n    depends_on: [docs, tests]\n    run: final\n    workspace: final\n",
    )
  let docs_artifact =
    step_artifact.from_command_result(
      "docs",
      0,
      "docs done",
      "",
      False,
      [],
      orchestrator().artifact_limits,
    )
  let subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(
          subject,
          "run_attempt:"
            <> context.step_id
            <> ":"
            <> int.to_string(context.attempt_index),
        )
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout:" <> context.step_id,
          "",
          False,
          secrets,
          limits,
        )
      },
    )
  let context =
    recovered_context(
      dag.id,
      dict.from_list([#("docs", workflow_scheduler.Succeeded)]),
      dict.from_list([#("docs", docs_artifact)]),
      dict.new(),
      dict.from_list([#("tests", 2)]),
    )
  let assert Ok(success) =
    workflow_run.execute_with_context(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      workflow_run.RecoveredRun(context),
      dependencies,
    )

  assert dict.has_key(success.artifacts, "docs")
  assert receive_event(subject) == "prepare_recovered:tests:tests:"
  assert receive_event(subject) == "run_attempt:tests:2"
  assert receive_event(subject) == "after:tests"
  assert receive_event(subject) == "prepare_recovered:final:final:"
  assert receive_event(subject) == "run_attempt:final:1"
}

pub fn workflow_run_resolves_default_and_step_model_settings_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: default_step\n    kind: agent\n    prompt: default prompt\n    workspace: main\n  - id: full_override\n    kind: agent\n    depends_on: [default_step]\n    prompt: full prompt\n    workspace: main\n    model: github-copilot/gpt-5.1-codex\n    thinking: high\n  - id: partial_thinking\n    kind: agent\n    depends_on: [full_override]\n    prompt: thinking prompt\n    workspace: main\n    thinking: xhigh\n  - id: partial_model\n    kind: agent\n    depends_on: [partial_thinking]\n    prompt: model prompt\n    workspace: main\n    model: openai/gpt-5.1\n",
    )
  let event_subject = process.new_subject()
  let command_subject = process.new_subject()
  let base = deps(event_subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        effective: config_types.EffectiveConfig,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(
          command_subject,
          context.step_id <> ":" <> effective.pi.command,
        )
        Ok(success_agent(prompt_text(prompt_mode)))
      },
    )
  let orch =
    config_types.OrchestratorConfig(
      ..orchestrator(),
      model_settings: model_config.Settings(
        model: Some("google/gemini-2.5-flash"),
        thinking: Some(model_config.ThinkingLow),
      ),
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orch,
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert receive_event(command_subject)
    == "default_step:pi --mode rpc --no-session --rpc-message-updates off --model 'google/gemini-2.5-flash' --thinking 'low'"
  assert receive_event(command_subject)
    == "full_override:pi --mode rpc --no-session --rpc-message-updates off --model 'github-copilot/gpt-5.1-codex' --thinking 'high'"
  assert receive_event(command_subject)
    == "partial_thinking:pi --mode rpc --no-session --rpc-message-updates off --model 'google/gemini-2.5-flash' --thinking 'xhigh'"
  assert receive_event(command_subject)
    == "partial_model:pi --mode rpc --no-session --rpc-message-updates off --model 'openai/gpt-5.1' --thinking 'low'"
}

pub fn workflow_run_prepare_failure_cleans_partial_ready_batch_test() {
  let subject = process.new_subject()
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      implementation_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_prepare_failure(subject, "code_review"),
    )
  assert failure.reason == "workspace_failed:workspace_io"
  assert failure.run_root
    == Some("test/tmp/workflow-run/workspaces/implementation/ABC-123")

  assert receive_event(subject) == "prepare:implement:main:"
  assert receive_event(subject)
    == "agent:test/tmp/workflow-run/workspaces/implementation/ABC-123/main:implement prompt"
  assert receive_event(subject) == "after:implement"
  assert receive_event(subject) == "prepare:test_after_implement:main:"
  assert receive_event(subject) == "prepare_failed:code_review"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_prepare_hook_failure_is_first_class_test() {
  let subject = process.new_subject()
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      implementation_dag(),
      orchestrator(),
      empty_tracker(),
      ["secret-token"],
      "run-1",
      deps_with_prepare_hook_failure(
        subject,
        "implement",
        "hook secret-token stderr",
      ),
    )

  assert string.contains(failure.reason, "hook_failed:hook_failed")
  assert string.contains(failure.reason, "before_step exited 17")
  assert string.contains(failure.reason, "hook [REDACTED] stderr")
  assert !string.contains(failure.reason, "secret-token")
  assert failure.agent_reason
    == Some(
      error.WorkflowHookFailed(error.HookFailed(
        "before_step",
        17,
        "hook secret-token stderr",
      )),
    )
}

pub fn workflow_run_ready_batch_runs_steps_concurrently_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: first\n    kind: command\n    run: first\n    workspace: main\n  - id: second\n    kind: command\n    run: second\n    workspace: review\n  - id: final\n    kind: command\n    depends_on: [first, second]\n    run: final\n    workspace: main\n",
    )
  let command_subject = process.new_subject()
  let result_subject = process.new_subject()
  let dummy_subject = process.new_subject()
  let base = deps(dummy_subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        let release_subject = process.new_subject()
        process.send(
          command_subject,
          CommandStart(context.step_id, release_subject),
        )
        case context.step_id == "final" {
          True -> Nil
          False -> {
            let _ = process.receive_forever(release_subject)
            Nil
          }
        }
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout:" <> context.step_id,
          "",
          False,
          secrets,
          limits,
        )
      },
    )

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        workflow_run.execute(
          issue(),
          dag,
          orchestrator(),
          empty_tracker(),
          [],
          "run-1",
          dependencies,
        )
      process.send(result_subject, result)
    })

  let starts = receive_command_starts(command_subject, 2, [])
  let start_ids = command_start_ids(starts, [])
  assert list.contains(start_ids, "first")
  assert list.contains(start_ids, "second")

  release_commands(starts)
  let assert Ok(Ok(_success)) = process.receive(result_subject, within: 1000)
}

pub fn workflow_run_ready_steps_sharing_workspace_are_serialized_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: first\n    kind: command\n    run: first\n    workspace: main\n  - id: second\n    kind: command\n    run: second\n    workspace: main\n  - id: final\n    kind: command\n    depends_on: [first, second]\n    run: final\n    workspace: main\n",
    )
  let command_subject = process.new_subject()
  let result_subject = process.new_subject()
  let dummy_subject = process.new_subject()
  let base = deps(dummy_subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        let release_subject = process.new_subject()
        process.send(
          command_subject,
          CommandStart(context.step_id, release_subject),
        )
        case context.step_id == "final" {
          True -> Nil
          False -> {
            let _ = process.receive_forever(release_subject)
            Nil
          }
        }
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout:" <> context.step_id,
          "",
          False,
          secrets,
          limits,
        )
      },
    )

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        workflow_run.execute(
          issue(),
          dag,
          orchestrator(),
          empty_tracker(),
          [],
          "run-1",
          dependencies,
        )
      process.send(result_subject, result)
    })

  let assert Ok(first_start) = process.receive(command_subject, within: 1000)
  let CommandStart(step_id: first_id, release: release_first) = first_start
  assert first_id == "first"
  test_async.assert_no_extra_message_within(command_subject, 50)

  process.send(release_first, "go")
  let assert Ok(second_start) = process.receive(command_subject, within: 1000)
  let CommandStart(step_id: second_id, release: release_second) = second_start
  assert second_id == "second"
  process.send(release_second, "go")
  let assert Ok(Ok(_success)) = process.receive(result_subject, within: 1000)
}

pub fn workflow_run_fatal_ready_step_cancels_active_siblings_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: fail\n    kind: command\n    run: fail\n    workspace: main\n  - id: slow\n    kind: command\n    run: slow\n    workspace: review\n  - id: final\n    kind: command\n    depends_on: [fail, slow]\n    run: final\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let command_subject = process.new_subject()
  let result_subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        let release_subject = process.new_subject()
        process.send(
          command_subject,
          CommandStart(context.step_id, release_subject),
        )
        let _ = process.receive_forever(release_subject)
        let exit_code = case context.step_id == "fail" {
          True -> 1
          False -> 0
        }
        step_artifact.from_command_result(
          context.step_id,
          exit_code,
          "stdout:" <> context.step_id,
          "",
          False,
          secrets,
          limits,
        )
      },
    )

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        workflow_run.execute(
          issue(),
          dag,
          orchestrator(),
          empty_tracker(),
          [],
          "run-1",
          dependencies,
        )
      process.send(result_subject, result)
    })

  assert receive_event(subject) == "prepare:fail:main:"
  assert receive_event(subject) == "prepare:slow:review:"
  let starts = receive_command_starts(command_subject, 2, [])
  let start_ids = command_start_ids(starts, [])
  assert list.contains(start_ids, "fail")
  assert list.contains(start_ids, "slow")

  release_command_by_id(starts, "fail")
  let assert Ok(Error(failure)) = process.receive(result_subject, within: 1000)
  assert failure.reason == "workflow_step_failed"
  assert receive_event(subject) == "after:fail"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_after_step_runs_in_dag_order_for_ready_batch_test() {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: first\n    kind: command\n    run: first\n    workspace: main\n  - id: second\n    kind: command\n    run: second\n    workspace: review\n  - id: final\n    kind: command\n    depends_on: [first, second]\n    run: final\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let result_subject = process.new_subject()
  let command_subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        case context.step_id == "first" {
          True -> {
            let release_subject = process.new_subject()
            process.send(
              command_subject,
              CommandStart(context.step_id, release_subject),
            )
            let _ = process.receive_forever(release_subject)
            Nil
          }
          False -> Nil
        }
        step_artifact.from_command_result(
          context.step_id,
          0,
          "stdout:" <> context.step_id,
          "",
          False,
          secrets,
          limits,
        )
      },
    )

  let _ =
    process.spawn_unlinked(fn() {
      let result =
        workflow_run.execute(
          issue(),
          dag,
          orchestrator(),
          empty_tracker(),
          [],
          "run-1",
          dependencies,
        )
      process.send(result_subject, result)
    })

  assert receive_event(subject) == "prepare:first:main:"
  assert receive_event(subject) == "prepare:second:review:"
  let run_events = receive_events(subject, 2, [])
  assert list.contains(run_events, "run:first")
  assert list.contains(run_events, "run:second")

  let assert Ok(first_start) = process.receive(command_subject, within: 1000)
  let CommandStart(step_id: first_id, release: release_first) = first_start
  assert first_id == "first"
  process.send(release_first, "go")
  assert receive_event(subject) == "after:first"
  assert receive_event(subject) == "after:second"
  assert receive_event(subject) == "prepare:final:main:"
  assert receive_event(subject) == "run:final"
  assert receive_event(subject) == "after:final"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  let assert Ok(Ok(_success)) = process.receive(result_subject, within: 1000)
}

pub fn workflow_run_step_worker_crash_returns_failure_test() {
  use <- expected_crash.suppressing([
    "test/workflow_run_test.gleam",
    "workflow_run_step_worker_crash_returns_failure_test",
    "command crashed",
  ])
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: crash\n    kind: command\n    run: crash\n    workspace: main\n",
    )
  let subject = process.new_subject()
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        _secrets,
        _limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        panic as "command crashed"
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  assert failure.reason == "step_worker_crashed:crash"
  assert receive_event(subject) == "prepare:crash:main:"
  assert receive_event(subject) == "run:crash"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_fatal_failure_stops_remaining_steps_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 2\nsteps:\n  - id: implement\n    kind: command\n    run: fail\n    workspace: main\n  - id: later\n    kind: command\n    depends_on: [implement]\n    run: later\n    workspace: main\n",
    )
  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps(subject, Some("implement")),
    )
  assert failure.reason == "workflow_step_failed"
  assert receive_event(subject) == "prepare:implement:main:"
  assert receive_event(subject) == "run:implement"
  assert receive_event(subject) == "after:implement"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_agent_pi_error_fails_step_artifact_and_report_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement prompt\n    workspace: main\n  - id: analyze_changes\n    kind: command\n    depends_on: [implement]\n    run: analyze\n    workspace: main\n",
    )
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(subject, "agent:" <> context.step_id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError(
            "pi turn_end reported stopReason=error: terminated",
          )),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: Some(issue()),
        ))
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert failure.failed_step_id == Some("implement")
  let assert Ok(artifact) = dict.get(failure.artifacts, "implement")
  assert artifact.status == step_artifact.StepFailed
  assert string.contains(artifact.stderr, "agent_pi_failed")
  assert string.contains(artifact.stderr, "terminated")
  let report = workflow_run.failure_report(failure)
  assert string.contains(report, "workflow_step_failed")
  assert string.contains(
    report,
    "pi protocol error: pi turn_end reported stopReason=error: terminated",
  )
  assert receive_event(subject) == "prepare:implement:main:"
  assert receive_event(subject) == "agent:implement"
  assert receive_event(subject) == "after:implement"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn workflow_run_failure_report_promotes_command_failure_code_test() {
  let subject = process.new_subject()
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: publish_pr\n    kind: command\n    run: publish\n    workspace: main\n",
    )
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        step_artifact.from_command_result(
          context.step_id,
          1,
          "",
          "SCHERZO_FAILURE_CODE=publish_rebase_conflict\nconflict output\n",
          False,
          secrets,
          limits,
        )
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert workflow_run.failed_command_failure(failure)
    == Some(#("publish_rebase_conflict", "publish_pr"))
  let report = workflow_run.failure_report(failure)
  assert string.contains(
    report,
    "workflow_command_failed:publish_rebase_conflict",
  )
  assert string.contains(report, "failure_code=publish_rebase_conflict")
  assert string.contains(report, "step=publish_pr")
}

fn deps_with_prepare_failure(
  subject: process.Subject(String),
  fail_step_id: String,
) -> workflow_run.Dependencies {
  let base = deps(subject, None)
  workflow_run.Dependencies(
    ..base,
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      step_id,
      attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      known,
    ) {
      case step_id == fail_step_id {
        True -> {
          process.send(subject, "prepare_failed:" <> step_id)
          Error(workspace_run.WorkspaceFailure(error.WorkspaceIo("boom")))
        }
        False ->
          base.prepare_step(
            issue,
            workflow_id,
            run_id,
            step_id,
            attempt_index,
            workspace_ref,
            orchestrator,
            profile,
            known,
          )
      }
    },
  )
}

fn deps_with_prepare_recovered_failure(
  subject: process.Subject(String),
  fail_step_id: String,
) -> workflow_run.Dependencies {
  let base = deps(subject, None)
  workflow_run.Dependencies(
    ..base,
    prepare_recovered_step: fn(
      issue,
      workflow_id,
      run_id,
      expected_run_root,
      step_id,
      attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      known,
    ) {
      case step_id == fail_step_id {
        True -> {
          process.send(subject, "prepare_recovered_failed:" <> step_id)
          Error(workspace_run.WorkspaceFailure(error.WorkspaceIo("boom")))
        }
        False ->
          base.prepare_recovered_step(
            issue,
            workflow_id,
            run_id,
            expected_run_root,
            step_id,
            attempt_index,
            workspace_ref,
            orchestrator,
            profile,
            known,
          )
      }
    },
  )
}

fn deps_with_prepare_hook_failure(
  subject: process.Subject(String),
  fail_step_id: String,
  diagnostics: String,
) -> workflow_run.Dependencies {
  let base = deps(subject, None)
  workflow_run.Dependencies(
    ..base,
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      step_id,
      attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      known,
    ) {
      case step_id == fail_step_id {
        True ->
          Error(
            workspace_run.HookFailure(error.HookFailed(
              "before_step",
              17,
              diagnostics,
            )),
          )
        False ->
          base.prepare_step(
            issue,
            workflow_id,
            run_id,
            step_id,
            attempt_index,
            workspace_ref,
            orchestrator,
            profile,
            known,
          )
      }
    },
  )
}

fn receive_event_with_prefix(
  subject: process.Subject(String),
  prefix: String,
  remaining: Int,
) -> String {
  case remaining <= 0 {
    True -> ""
    False -> {
      let event = receive_event(subject)
      case string.starts_with(event, prefix) {
        True -> event
        False -> receive_event_with_prefix(subject, prefix, remaining - 1)
      }
    }
  }
}

fn receive_event(subject: process.Subject(String)) -> String {
  let assert Ok(event) = process.receive(subject, within: 1000)
  event
}

fn receive_events(
  subject: process.Subject(String),
  count: Int,
  acc: List(String),
) -> List(String) {
  case count <= 0 {
    True -> acc
    False -> receive_events(subject, count - 1, [receive_event(subject), ..acc])
  }
}

fn receive_command_starts(
  subject: process.Subject(CommandStart),
  count: Int,
  acc: List(CommandStart),
) -> List(CommandStart) {
  case count <= 0 {
    True -> acc
    False -> {
      let assert Ok(start) = process.receive(subject, within: 1000)
      receive_command_starts(subject, count - 1, [start, ..acc])
    }
  }
}

fn command_start_ids(
  starts: List(CommandStart),
  acc: List(String),
) -> List(String) {
  case starts {
    [] -> acc
    [CommandStart(step_id: step_id, ..), ..rest] ->
      command_start_ids(rest, [step_id, ..acc])
  }
}

fn release_commands(starts: List(CommandStart)) -> Nil {
  case starts {
    [] -> Nil
    [CommandStart(release: release, ..), ..rest] -> {
      process.send(release, "go")
      release_commands(rest)
    }
  }
}

fn release_command_by_id(starts: List(CommandStart), step_id: String) -> Nil {
  case starts {
    [] -> Nil
    [CommandStart(step_id: current, release: release), ..rest] ->
      case current == step_id {
        True -> process.send(release, "go")
        False -> release_command_by_id(rest, step_id)
      }
  }
}

@external(erlang, "scherzo_test_ffi", "setenv")
fn setenv(name: String, value: String) -> Result(Nil, Nil)

@external(erlang, "scherzo_test_ffi", "unsetenv")
fn unsetenv(name: String) -> Nil

fn generic_tool_call_structured_output_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: example prompt\n    workspace: main\n    structured_output:\n      artifact_name: review_lane_submission\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n        parameters_schema_path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n        require_single: true\n        reject_sibling_tool_calls: true\n      validators:\n        - name: review_lane_submission_shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [draft_findings, review_notes, evidence_requests, self_check]\n",
    )
  dag
}

fn generic_tool_call_result() -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(
    Some("{\"schema_version\":999,\"artifact_type\":\"ignored\"}"),
    False,
    "test",
    [
      result_artifact.ToolCallSubmission(
        name: "submit_review_lane_draft",
        arguments_json: Some(
          "{\"draft_findings\":[],\"review_notes\":[],\"evidence_requests\":[],\"self_check\":{\"summary\":\"ok\"}}",
        ),
        status: Some("success"),
        sibling_count: 1,
        receipt_json: Some(
          "{\"artifact_type\":\"scherzo_structured_output_tool_receipt\",\"tool_name\":\"submit_review_lane_draft\",\"remote_mutations\":\"none\"}",
        ),
      ),
    ],
  )
}

fn env_value(env: List(#(String, String)), key: String) -> Option(String) {
  case env {
    [] -> None
    [#(candidate, value), ..rest] ->
      case candidate == key {
        True -> Some(value)
        False -> env_value(rest, key)
      }
  }
}

pub fn generic_pi_tool_call_step_generates_spec_env_and_metadata_test() {
  let subject = process.new_subject()
  let result = generic_tool_call_result()
  let base =
    deps_with_structured_agent_result(
      subject,
      result,
      workflow_checkpoint.noop_writer(),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        let assert Some(spec_path) =
          env_value(
            context.extra_pi_env,
            "SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH",
          )
        process.send(subject, "spec_env:" <> spec_path)
        Ok(success_agent_with_result(result))
      },
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      generic_tool_call_structured_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  let assert Ok(artifact) = dict.get(success.artifacts, "example_json")
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  assert metadata.source_type == "pi_tool_call"
  assert metadata.source_tool_name == Some("submit_review_lane_draft")
  assert metadata.source_parameters_schema_path
    == Some(
      ".scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    )
  let assert Some(schema_sha) = metadata.source_parameters_schema_sha256
  assert string.length(schema_sha) == 64
  let assert Some(receipt_json) = metadata.source_receipt_json
  assert string.contains(receipt_json, "remote_mutations")
}

pub fn contracted_command_run_records_inputs_before_steps_and_outputs_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-recording"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo findings\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert success.run_root != ""
  let records = ledger_records(root)
  let assert [first, second, ..] = records
  assert record.kind(first.body) == "workflow_run_inputs_recorded"
  assert record.kind(second.body) == "step_attempt_prepared"

  let assert Ok(input_manifest_text) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/inputs.v1.json",
    )
  let assert Ok(input_manifest) =
    workflow_contract_manifest.decode_input_manifest(input_manifest_text)
  assert input_manifest.workflow_id == "implementation"
  let assert [prompt] = input_manifest.inputs
  assert prompt.name == "prompt"

  let assert Ok(output_manifest_text) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/outputs.v1.json",
    )
  let assert Ok(output_manifest) =
    workflow_contract_manifest.decode_output_manifest(output_manifest_text)
  let assert [findings] = output_manifest.outputs
  assert findings.name == "findings"
  assert findings.value.status == workflow_contract_manifest.Present
  assert findings.value.ref == Some("runs/run-1/outputs/findings.md")
}

pub fn contracted_mapped_input_missing_fails_before_prepare_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-missing-input"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    exec_plan:\n      type: exec_plan\n      source: mapped_output\nsteps:\n  - id: implement\n    kind: command\n    run: echo should-not-run\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  assert failure.reason == "workflow_required_input_missing:exec_plan"
  assert process.receive(subject, within: 20) == Error(Nil)
}

pub fn contracted_existing_mismatched_input_manifest_fails_before_prepare_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-mismatched-input-manifest"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\nsteps:\n  - id: implement\n    kind: command\n    run: echo should-not-run\n",
    )
  let fingerprint = workflow_attempt.workflow_fingerprint(dag, orchestrator())
  let stale_manifest =
    workflow_contract_manifest.ContractInputManifest(
      run_id: "run-1",
      workflow_id: "implementation",
      workflow_fingerprint: fingerprint,
      inputs: [],
      context: [],
      diagnostics: ["stale-manifest"],
    )
  let artifact_dir = root <> "/.scherzo-state/artifacts/runs/run-1"
  let assert Ok(Nil) = simplifile.create_directory_all(artifact_dir)
  let assert Ok(Nil) =
    simplifile.write(
      artifact_dir <> "/inputs.v1.json",
      workflow_contract_manifest.input_manifest_to_string(stale_manifest),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )
  assert string.contains(
    failure.reason,
    "existing_input_manifest_mismatch:runs/run-1/inputs.v1.json",
  )
  assert process.receive(subject, within: 20) == Error(Nil)
}

pub fn opted_in_workstream_phase_emits_handoff_and_next_action_test() {
  let root = "test/tmp/workflow-run/workstream-phase-success"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      opted_in_workstream_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(process.new_subject(), None),
        command_step: fn(
          context: workflow_run.StepContext,
          _command,
          _timeout_ms,
          secrets,
          limits,
        ) {
          step_artifact.from_command_result(
            context.step_id,
            0,
            "{\"bundle_id\":\"bundle-1\"}\n",
            "",
            False,
            secrets,
            limits,
          )
        },
        checkpoint: checkpoint,
      ),
    )

  let assert Ok(_) = dict.get(success.artifacts, "materialize_bundle")
  let workstream_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      string.starts_with(record.kind(ledger_record.body), "workstream_")
    })
  assert list.length(workstream_records) == 5
  let handoff_ref = recorded_handoff_ref(workstream_records)
  let next_action_ref = recorded_next_action_ref(workstream_records)
  let assert Ok(handoff_contents) = checkpoint.read_artifact(handoff_ref)
  let assert Ok(handoff) = workstream_artifacts.decode_handoff(handoff_contents)
  assert handoff.workstream_id == "linear:ABC-123"
  let assert [recommended_next_action] = handoff.recommended_next_actions
  let assert Ok(next_action_contents) =
    checkpoint.read_artifact(next_action_ref)
  let assert Ok(next_action) =
    workstream_artifacts.decode_next_action(next_action_contents)
  assert recommended_next_action == next_action.artifact_id
  assert next_action.workflow_id == "execplan-implementation"
  assert next_action.auto_enqueue == False
}

pub fn workflow_without_workstream_phase_writes_no_workstream_records_test() {
  let root = "test/tmp/workflow-run/workstream-phase-noop"
  reset_dir(root)

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      non_opted_in_workstream_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(process.new_subject(), None),
        command_step: fn(
          context: workflow_run.StepContext,
          _command,
          _timeout_ms,
          secrets,
          limits,
        ) {
          step_artifact.from_command_result(
            context.step_id,
            0,
            "{\"bundle_id\":\"bundle-1\"}\n",
            "",
            False,
            secrets,
            limits,
          )
        },
        checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      ),
    )

  let workstream_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      string.starts_with(record.kind(ledger_record.body), "workstream_")
    })
  assert workstream_records == []
}

pub fn metadata_only_workstream_phase_without_contract_noops_test() {
  let root = "test/tmp/workflow-run/workstream-phase-metadata-only"
  reset_dir(root)

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      metadata_only_workstream_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(process.new_subject(), None),
        command_step: fn(
          context: workflow_run.StepContext,
          _command,
          _timeout_ms,
          secrets,
          limits,
        ) {
          step_artifact.from_command_result(
            context.step_id,
            0,
            "ok\n",
            "",
            False,
            secrets,
            limits,
          )
        },
        checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
      ),
    )

  let workstream_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      string.starts_with(record.kind(ledger_record.body), "workstream_")
    })
  assert workstream_records == []
}

pub fn opted_in_workstream_phase_fails_closed_when_output_is_absent_test() {
  let root = "test/tmp/workflow-run/workstream-phase-absent-output"
  let subject = process.new_subject()
  reset_dir(root)
  let base_checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let checkpoint =
    workflow_checkpoint.Writer(
      ..base_checkpoint,
      workflow_finished: fn(finished: workflow_checkpoint.WorkflowFinished) {
        process.send(subject, finished.outcome)
        base_checkpoint.workflow_finished(finished)
      },
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      opted_in_optional_output_dag(),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(subject, None),
        agent_step: fn(
          _issue,
          _context,
          _prompt_mode,
          _attempt_context,
          _effective,
          _tracker,
          _emit_update,
          _command_ready,
          _record_pi_session,
        ) {
          Ok(success_agent_with_response(None, False))
        },
        checkpoint: checkpoint,
      ),
    )

  assert string.starts_with(
    failure.reason,
    "workflow_workstream_handoff_failed:workstream_handoff_output_absent",
  )
  assert receive_event(subject) == "prepare:materialize_bundle:main:"
  assert receive_event(subject) == "after:materialize_bundle"
  assert receive_event(subject) == "failed_fatal"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
}

pub fn resumed_opted_in_workstream_phase_reuses_recorded_output_manifest_test() {
  let root = "test/tmp/workflow-run/workstream-phase-recovery-success"
  reset_dir(root)
  let dag = opted_in_workstream_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest = write_recorded_execplan_output_manifest(root, dag, "execplan")
  let resume =
    opted_in_workstream_resume_state(workflow_checkpoint.ArtifactWritten(
      ref: "runs/run-1/outputs.v1.json",
      sha256: hash.sha256_hex(
        workflow_contract_manifest.output_manifest_to_string(manifest),
      ),
      bytes: string.length(workflow_contract_manifest.output_manifest_to_string(
        manifest,
      )),
    ))

  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(process.new_subject(), None),
        checkpoint: checkpoint,
      ),
      resume,
    )

  let workstream_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      string.starts_with(record.kind(ledger_record.body), "workstream_")
    })
  assert list.length(workstream_records) == 5
  let handoff_ref = recorded_handoff_ref(workstream_records)
  let assert Ok(handoff_contents) = checkpoint.read_artifact(handoff_ref)
  let assert Ok(handoff) = workstream_artifacts.decode_handoff(handoff_contents)
  let assert [handoff_output] = handoff.outputs
  assert handoff_output.snapshot.producer.workflow_id == "execplan"
  assert handoff_output.snapshot.producer.run_id == "run-1"
  assert handoff_output.snapshot.producer.step_id == "materialize_bundle"
}

pub fn resumed_opted_in_workstream_phase_rejects_mismatched_output_manifest_identity_test() {
  let root = "test/tmp/workflow-run/workstream-phase-recovery-manifest-mismatch"
  reset_dir(root)
  let dag = opted_in_workstream_dag()
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest =
    write_recorded_execplan_output_manifest(root, dag, "unexpected-workflow")
  let resume =
    opted_in_workstream_resume_state(workflow_checkpoint.ArtifactWritten(
      ref: "runs/run-1/outputs.v1.json",
      sha256: hash.sha256_hex(
        workflow_contract_manifest.output_manifest_to_string(manifest),
      ),
      bytes: string.length(workflow_contract_manifest.output_manifest_to_string(
        manifest,
      )),
    ))

  let assert Error(failure) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(process.new_subject(), None),
        checkpoint: checkpoint,
      ),
      resume,
    )

  assert string.starts_with(
    failure.reason,
    "workflow_workstream_handoff_failed:manifest_workflow_id_mismatch",
  )
  let workstream_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      string.starts_with(record.kind(ledger_record.body), "workstream_")
    })
  assert workstream_records == []
}

fn opted_in_workstream_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nworkstream_phase:\n  phase_id: execplan\n  display_name: ExecPlan authored\n  handoff:\n    output: exec_plan_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\n  next_actions:\n    - action_id: implement_exec_plan\n      workflow_id: execplan-implementation\n      state: suggested\n      priority: 0\n      inputs: [exec_plan_bundle]\n      requires_gate: human_review\n      auto_enqueue: false\nsteps:\n  - id: materialize_bundle\n    kind: command\n    run: emit bundle\n    workspace: main\n",
    )
  dag
}

fn non_opted_in_workstream_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        field: stdout\nsteps:\n  - id: materialize_bundle\n    kind: command\n    run: emit bundle\n    workspace: main\n",
    )
  dag
}

fn metadata_only_workstream_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\nworkstream_phase:\n  phase_id: execplan\nsteps:\n  - id: materialize_bundle\n    kind: command\n    run: emit bundle\n    workspace: main\n",
    )
  dag
}

fn opted_in_optional_output_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      required: false\n      source:\n        step: materialize_bundle\n        field: final_response\nworkstream_phase:\n  phase_id: execplan\n  handoff:\n    output: exec_plan_bundle\n    artifact_type: scherzo.handoff.v1\n    snapshot: required\nsteps:\n  - id: materialize_bundle\n    kind: agent\n    prompt: emit bundle\n    workspace: main\n",
    )
  dag
}

fn opted_in_workstream_resume_state(
  recorded: workflow_checkpoint.ArtifactWritten,
) -> workflow_run.ResumeState {
  workflow_run.ResumeState(
    artifacts: dict.from_list([
      #(
        "materialize_bundle",
        step_artifact.from_command_result(
          "materialize_bundle",
          0,
          "{\"bundle_id\":\"bundle-1\"}\n",
          "",
          False,
          [],
          orchestrator().artifact_limits,
        ),
      ),
    ]),
    workspaces: dict.new(),
    next_attempt_indexes: dict.new(),
    run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
    recovery_evidence: workflow_outcome.NoStepRecovery,
    pi_session_continuations: dict.new(),
    contract_inputs_recorded: None,
    contract_outputs_recorded: Some(recorded),
  )
}

fn write_recorded_execplan_output_manifest(
  root: String,
  dag: workflow_dag.WorkflowDag,
  manifest_workflow_id: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  let store = artifact_store.new(root)
  let contents = "{\"bundle_id\":\"bundle-1\"}\n"
  let assert Ok(existing) =
    artifact_store.write_output_blob(
      store,
      "run-1",
      "exec_plan_bundle",
      ".json",
      contents,
    )
  let manifest =
    workflow_contract_manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: manifest_workflow_id,
      workflow_fingerprint: workflow_attempt.workflow_fingerprint(
        dag,
        orchestrator(),
      ),
      outputs: [
        workflow_contract_manifest.NamedManifestValue(
          name: "exec_plan_bundle",
          value: workflow_contract_manifest.present_run_artifact(
            workflow_contract.ExecPlanBundle,
            workflow_contract_manifest.ArtifactWritten(
              ref: existing.ref,
              sha256: existing.sha256,
              bytes: existing.bytes,
            ),
            "application/json",
            Some(source_json("materialize_bundle")),
          ),
        ),
      ],
      diagnostics: [],
    )
  let manifest_text =
    workflow_contract_manifest.output_manifest_to_string(manifest)
  let artifact_dir = root <> "/.scherzo-state/artifacts/runs/run-1"
  let assert Ok(Nil) = simplifile.create_directory_all(artifact_dir)
  let assert Ok(Nil) =
    simplifile.write(artifact_dir <> "/outputs.v1.json", manifest_text)
  manifest
}

fn source_json(step_id: String) -> json_value.JsonValue {
  json_value.JObject([#("step_id", json_value.JString(step_id))])
}

fn recorded_handoff_ref(records: List(record.LedgerRecord)) -> String {
  let assert Ok(ledger_record) =
    list.find(records, fn(ledger_record) {
      record.kind(ledger_record.body) == "workstream_handoff_recorded"
    })
  let assert record.WorkstreamHandoffRecorded(handoff_ref: handoff_ref, ..) =
    ledger_record.body
  handoff_ref
}

fn recorded_next_action_ref(records: List(record.LedgerRecord)) -> String {
  let assert Ok(ledger_record) =
    list.find(records, fn(ledger_record) {
      case ledger_record.body {
        record.WorkstreamArtifactRecorded(
          artifact_type: artifact_type,
          contract_type: contract_type,
          ..,
        ) ->
          artifact_type == "scherzo.next_action.v1" && contract_type == "text"
        _ -> False
      }
    })
  let assert record.WorkstreamArtifactRecorded(snapshot_ref: snapshot_ref, ..) =
    ledger_record.body
  snapshot_ref
}

fn read_input_manifest(
  root: String,
  run_id: String,
) -> workflow_contract_manifest.ContractInputManifest {
  let assert Ok(text) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/" <> run_id <> "/inputs.v1.json",
    )
  let assert Ok(manifest) =
    workflow_contract_manifest.decode_input_manifest(text)
  manifest
}

fn read_output_manifest(
  root: String,
  run_id: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  let assert Ok(text) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/" <> run_id <> "/outputs.v1.json",
    )
  let assert Ok(manifest) =
    workflow_contract_manifest.decode_output_manifest(text)
  manifest
}

fn output_named(
  outputs: List(workflow_contract_manifest.NamedManifestValue),
  name: String,
) -> workflow_contract_manifest.ManifestValue {
  let assert Ok(output) =
    outputs
    |> list.find(fn(output) { output.name == name })
  output.value
}

pub fn contracted_mapped_input_can_be_supplied_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-supplied-input"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    exec_plan:\n      type: exec_plan\n      source: mapped_output\nsteps:\n  - id: implement\n    kind: command\n    run: echo runs\n",
    )
  let supplied =
    workflow_run.ContractRunValues(
      inputs: dict.from_list([
        #(
          "exec_plan",
          workflow_contract_manifest.present_run_artifact(
            workflow_contract.ExecPlan,
            workflow_contract_manifest.ArtifactWritten(
              ref: "runs/upstream/outputs/exec_plan.md",
              sha256: "abc",
              bytes: 12,
            ),
            "text/markdown",
            None,
          ),
        ),
      ]),
      context: dict.new(),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute_with_contract_values(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      supplied,
      dependencies,
    )
  assert receive_event(subject) == "prepare:implement:main:"
}

pub fn handoff_derived_contract_values_are_recorded_in_input_manifest_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-handoff-derived-input"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    reviewed_plan:\n      type: exec_plan\n      source: mapped_output\nsteps:\n  - id: implement\n    kind: command\n    run: echo runs\n",
    )
  let assert Ok(source_json) =
    json_value.parse(
      "{\"source_handoff_ref\":\"runs/upstream/outputs/handoff.json\",\"workstream_id\":\"linear:LIV-244\"}",
    )
  let supplied =
    workflow_run.ContractRunValues(
      inputs: dict.from_list([
        #(
          "reviewed_plan",
          workflow_contract_manifest.present_run_artifact(
            workflow_contract.ExecPlan,
            workflow_contract_manifest.ArtifactWritten(
              ref: "runs/upstream/outputs/reviewed_plan.md",
              sha256: "abc",
              bytes: 12,
            ),
            "text/markdown",
            Some(source_json),
          ),
        ),
      ]),
      context: dict.new(),
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute_with_contract_values(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      supplied,
      dependencies,
    )

  let assert Ok(input_manifest_text) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/inputs.v1.json",
    )
  assert string.contains(
    input_manifest_text,
    "\"artifact_type\":\"workflow_contract_inputs\"",
  )

  let manifest = read_input_manifest(root, "run-1")
  let assert [reviewed_plan] = manifest.inputs
  assert reviewed_plan.name == "reviewed_plan"
  assert reviewed_plan.value.status == workflow_contract_manifest.Present
  assert reviewed_plan.value.ref
    == Some("runs/upstream/outputs/reviewed_plan.md")
  let assert Some(source) = reviewed_plan.value.source
  let source_text = json.to_string(json_value.to_json(source))
  assert string.contains(source_text, "runs/upstream/outputs/handoff.json")
  assert string.contains(source_text, "linear:LIV-244")
}

pub fn contracted_scheduled_context_records_metadata_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-scheduled-context"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: scheduled-repair\ncontract:\n  version: 1\n  inputs:\n    scheduled:\n      type: text\n      source: scheduled_context\nsteps:\n  - id: repair\n    kind: command\n    run: echo repair\n",
    )
  let scheduled =
    schedule_core.ScheduledRunContext(
      job_id: "nightly-repair",
      workflow_id: "scheduled-repair",
      due_at_ms: 1_800_000,
      started_at_ms: 1_860_000,
      run_id: "run-scheduled",
      attempt: 2,
      trigger: "automatic",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute_scheduled(
      scheduled,
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      dependencies,
    )

  let manifest = read_input_manifest(root, "run-scheduled")
  let assert [scheduled_input] = manifest.inputs
  assert scheduled_input.name == "scheduled"
  let assert Some(value) = scheduled_input.value.value
  let value_text = json.to_string(json_value.to_json(value))
  assert string.contains(value_text, "nightly-repair")
  assert string.contains(value_text, "run-scheduled")
  assert !string.contains(value_text, "ABC-123")
}

pub fn contracted_optional_workspace_driver_base_records_absent_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-optional-driver-base"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  context:\n    base_ref:\n      type: git_ref\n      required: false\n      source: workspace_driver_base\nsteps:\n  - id: implement\n    kind: command\n    run: echo implement\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let manifest = read_input_manifest(root, "run-1")
  let assert [base_ref] = manifest.context
  assert base_ref.name == "base_ref"
  assert base_ref.value.status == workflow_contract_manifest.Absent
}

pub fn contracted_required_final_response_output_missing_fails_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-missing-required-output"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: write\n        field: final_response\nsteps:\n  - id: write\n    kind: agent\n    prompt: write prompt\n    workspace: main\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(subject, None, False, checkpoint),
    )

  assert failure.reason == "workflow_required_output_missing:findings"
  let manifest = read_output_manifest(root, "run-1")
  let assert [findings] = manifest.outputs
  assert findings.value.status == workflow_contract_manifest.Absent
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "findings is required but absent",
  )
}

pub fn contracted_step_failure_keeps_terminal_reason_and_records_output_diagnostics_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-step-failure-output-diagnostics"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect\n        field: stdout\nsteps:\n  - id: fail\n    kind: command\n    run: exit 1\n  - id: collect\n    kind: command\n    depends_on: [fail]\n    run: echo findings\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, Some("fail")),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let manifest = read_output_manifest(root, "run-1")
  let assert [findings] = manifest.outputs
  assert findings.value.status == workflow_contract_manifest.Absent
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "workflow_output_source_step_missing:collect",
  )
}

pub fn contracted_failed_stdout_source_output_is_absent_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-failed-stdout-output"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    implementation_pack:\n      type: implementation_pack\n      source:\n        step: materialize\n        field: stdout\nsteps:\n  - id: materialize\n    kind: command\n    run: exit 1\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, Some("materialize")),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let manifest = read_output_manifest(root, "run-1")
  let assert [implementation_pack] = manifest.outputs
  assert implementation_pack.name == "implementation_pack"
  assert implementation_pack.value.status == workflow_contract_manifest.Absent
  assert implementation_pack.value.ref == None
  assert implementation_pack.value.bytes == None
  let assert Error(_) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/outputs/implementation_pack.json",
    )
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "workflow_output_source_step_failed:materialize",
  )
}

pub fn contracted_failed_agent_source_outputs_are_absent_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-failed-agent-source-outputs"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    final_plan:\n      type: exec_plan\n      source:\n        step: draft\n        field: final_response\n    structured_change:\n      type: code_change\n      source:\n        step: draft\n        structured_output: code_change\nsteps:\n  - id: draft\n    kind: agent\n    prompt: write prompt\n    workspace: main\n    structured_output:\n      artifact_name: code_change\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_code_change\n      schema:\n        required: [branch]\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        result_artifact.from_final_response_with_tool_calls(
          Some("# Draft plan\n"),
          False,
          "test",
          [
            result_artifact.ToolCallSubmission(
              name: "submit_code_change",
              arguments_json: Some("not json"),
              status: Some("success"),
              sibling_count: 1,
              receipt_json: None,
            ),
          ],
        ),
        checkpoint,
      ),
    )

  assert string.contains(
    failure.reason,
    "structured_output_tool_call_arguments_invalid",
  )
  let manifest = read_output_manifest(root, "run-1")
  assert output_named(manifest.outputs, "final_plan").status
    == workflow_contract_manifest.Absent
  assert output_named(manifest.outputs, "structured_change").status
    == workflow_contract_manifest.Absent
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "workflow_output_source_step_failed:draft",
  )
  let assert Error(_) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/outputs/final_plan.md",
    )
  let assert Error(_) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/outputs/structured_change.json",
    )
}

pub fn contracted_final_response_output_is_retained_as_markdown_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-final-response-output"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    exec_plan:\n      type: exec_plan\n      source:\n        step: write\n        field: final_response\nsteps:\n  - id: write\n    kind: agent\n    prompt: write prompt\n    workspace: main\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(
        subject,
        Some("# ExecPlan\n"),
        False,
        checkpoint,
      ),
    )

  let manifest = read_output_manifest(root, "run-1")
  let assert [exec_plan] = manifest.outputs
  assert exec_plan.value.ref == Some("runs/run-1/outputs/exec_plan.md")
  let assert Ok(blob) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/outputs/exec_plan.md",
    )
  assert blob == "# ExecPlan\n"
}

pub fn contracted_execplan_v2_step_field_outputs_are_retained_as_json_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-execplan-json-outputs"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize\n        field: stdout\n    implementation_pack:\n      type: implementation_pack\n      source:\n        step: materialize\n        field: stdout\n    code_change_bundle:\n      type: code_change_bundle\n      source:\n        step: materialize\n        field: stdout\nsteps:\n  - id: materialize\n    kind: command\n    run: echo '{\"schema_version\":2}'\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let base_deps = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base_deps,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(subject, "run:" <> context.step_id)
        step_artifact.from_command_result(
          context.step_id,
          0,
          "{\"schema_version\":2}\n",
          "",
          False,
          secrets,
          limits,
        )
      },
      checkpoint: checkpoint,
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let manifest = read_output_manifest(root, "run-1")
  assert output_named(manifest.outputs, "exec_plan_bundle").ref
    == Some("runs/run-1/outputs/exec_plan_bundle.json")
  assert output_named(manifest.outputs, "implementation_pack").ref
    == Some("runs/run-1/outputs/implementation_pack.json")
  assert output_named(manifest.outputs, "code_change_bundle").ref
    == Some("runs/run-1/outputs/code_change_bundle.json")
}

pub fn contracted_file_output_uses_output_path_not_truncated_stdout_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-file-output-json"
  reset_dir(root)
  reset_dir("test/tmp/workflow-run/workspaces/implementation/ABC-123")
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    implementation_pack:\n      type: implementation_pack\n      source:\n        step: materialize\n        path: tmp/execplan-implementation-pack.json\nsteps:\n  - id: materialize\n    kind: command\n    run: ignored\n",
    )
  let large_json =
    "{\"schema_version\":2,\"payload\":\""
    <> string.repeat("x", times: 1500)
    <> "\"}\n"
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let base_deps = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base_deps,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        let output_dir = path.join(context.workspace_path, "tmp")
        let assert Ok(Nil) = simplifile.create_directory_all(output_dir)
        let assert Ok(Nil) =
          simplifile.write(
            path.join(output_dir, "execplan-implementation-pack.json"),
            large_json,
          )
        step_artifact.from_command_result(
          context.step_id,
          0,
          "{not-json-from-stdout",
          "",
          False,
          secrets,
          limits,
        )
      },
      checkpoint: checkpoint,
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let manifest = read_output_manifest(root, "run-1")
  let pack = output_named(manifest.outputs, "implementation_pack")
  assert pack.ref == Some("runs/run-1/outputs/implementation_pack.json")
  assert pack.bytes == Some(string.length(large_json))
  let assert Ok(blob) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/outputs/implementation_pack.json",
    )
  assert blob == large_json
}

pub fn contracted_json_stdout_output_truncation_fails_publication_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-json-stdout-truncated"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    implementation_pack:\n      type: implementation_pack\n      source:\n        step: materialize\n        field: stdout\nsteps:\n  - id: materialize\n    kind: command\n    run: ignored\n",
    )
  let large_json =
    "{\"schema_version\":2,\"payload\":\""
    <> string.repeat("x", times: 1500)
    <> "\"}\n"
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let base_deps = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base_deps,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        step_artifact.from_command_result(
          context.step_id,
          0,
          large_json,
          "",
          False,
          secrets,
          limits,
        )
      },
      checkpoint: checkpoint,
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason
    == "workflow_required_output_missing:implementation_pack"
  let manifest = read_output_manifest(root, "run-1")
  assert output_named(manifest.outputs, "implementation_pack").status
    == workflow_contract_manifest.Absent
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "workflow_output_json_source_truncated:implementation_pack",
  )
  let assert Error(_) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/outputs/implementation_pack.json",
    )
}

pub fn contracted_invalid_json_file_output_fails_publication_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-invalid-json-file-output"
  reset_dir(root)
  reset_dir("test/tmp/workflow-run/workspaces/implementation/ABC-123")
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    implementation_pack:\n      type: implementation_pack\n      source:\n        step: materialize\n        path: tmp/execplan-implementation-pack.json\nsteps:\n  - id: materialize\n    kind: command\n    run: ignored\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let base_deps = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base_deps,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        let output_dir = path.join(context.workspace_path, "tmp")
        let assert Ok(Nil) = simplifile.create_directory_all(output_dir)
        let assert Ok(Nil) =
          simplifile.write(
            path.join(output_dir, "execplan-implementation-pack.json"),
            "{invalid-json",
          )
        step_artifact.from_command_result(
          context.step_id,
          0,
          "ok\n",
          "",
          False,
          secrets,
          limits,
        )
      },
      checkpoint: checkpoint,
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason
    == "workflow_required_output_missing:implementation_pack"
  let manifest = read_output_manifest(root, "run-1")
  assert output_named(manifest.outputs, "implementation_pack").status
    == workflow_contract_manifest.Absent
  assert string.contains(
    string.join(manifest.diagnostics, with: "\n"),
    "workflow_output_json_invalid:implementation_pack",
  )
  let assert Error(_) =
    simplifile.read(
      root
      <> "/.scherzo-state/artifacts/runs/run-1/outputs/implementation_pack.json",
    )
}

pub fn contracted_structured_and_inline_json_outputs_are_recorded_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-structured-inline-output"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    artifact_change:\n      type: code_change\n      source:\n        step: review_json\n        structured_output: code_change\n    inline_change:\n      type: code_change\n      source:\n        step: review_json\n        inline_json: code_change\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: review prompt\n    workspace: main\n    structured_output:\n      artifact_name: code_change\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [branch]\n",
    )
  let checkpoint = hidden_local_path_checkpoint(root)

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_result(
        subject,
        review_result_tool_call(
          Some("{\"branch\":\"feature/liv-294\"}"),
          Some("success"),
        ),
        checkpoint,
      ),
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "review_json")
  let assert Some(step_artifact.StructuredOutputValid(metadata)) =
    artifact.structured_output
  assert metadata.local_path == None
  assert metadata.display_path
    == "artifacts://runs/run-1/review_json/attempt-1/structured/code_change.json"

  let manifest = read_output_manifest(root, "run-1")
  let artifact_change = output_named(manifest.outputs, "artifact_change")
  assert artifact_change.status == workflow_contract_manifest.Present
  assert artifact_change.ref_kind
    == Some(workflow_contract_manifest.RunArtifact)
  let inline_change = output_named(manifest.outputs, "inline_change")
  assert inline_change.status == workflow_contract_manifest.Present
  assert inline_change.ref_kind
    == Some(workflow_contract_manifest.InlineJsonRef)
  let assert Some(value) = inline_change.value
  assert string.contains(
    json.to_string(json_value.to_json(value)),
    "feature/liv-294",
  )
}

pub fn contracted_optional_missing_output_allows_success_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-optional-missing-output"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    notes:\n      type: document.markdown\n      required: false\n      source:\n        step: write\n        field: final_response\nsteps:\n  - id: write\n    kind: agent\n    prompt: write prompt\n    workspace: main\n",
    )
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      deps_with_structured_agent_response(subject, None, False, checkpoint),
    )

  let manifest = read_output_manifest(root, "run-1")
  let assert [notes] = manifest.outputs
  assert notes.value.status == workflow_contract_manifest.Absent
}

pub fn resumed_run_without_step_recovery_emits_completed_terminal_outcome_test() {
  let root = "test/tmp/workflow-run/recovered-outcome-clean-success"
  let subject = process.new_subject()
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.new(),
      workspaces: dict.new(),
      next_attempt_indexes: dict.from_list([#("collect", 1)]),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.NoStepRecovery,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )

  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(subject, None),
        checkpoint: recording_checkpoint(root, subject),
      ),
      resume,
    )

  assert receive_event_with_prefix(subject, "workflow_finished:", 20)
    == "workflow_finished:completed"
  assert workflow_finished_outcome(root) == workflow_outcome.completed
}

pub fn resumed_run_with_step_recovery_emits_succeeded_after_recovery_test() {
  let root = "test/tmp/workflow-run/recovered-outcome-recovered-success"
  let subject = process.new_subject()
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.new(),
      workspaces: dict.new(),
      next_attempt_indexes: dict.from_list([#("collect", 1)]),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.StepRecoveryRan,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )

  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(subject, None),
        checkpoint: recording_checkpoint(root, subject),
      ),
      resume,
    )

  assert receive_event_with_prefix(subject, "workflow_finished:", 20)
    == "workflow_finished:succeeded_after_recovery"
  assert workflow_finished_outcome(root)
    == workflow_outcome.succeeded_after_recovery
}

pub fn resumed_run_without_step_recovery_emits_failed_fatal_terminal_outcome_test() {
  let root = "test/tmp/workflow-run/recovered-outcome-clean-failure"
  let subject = process.new_subject()
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.new(),
      workspaces: dict.new(),
      next_attempt_indexes: dict.from_list([#("collect", 1)]),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.NoStepRecovery,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )

  let assert Error(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(subject, Some("collect")),
        checkpoint: recording_checkpoint(root, subject),
      ),
      resume,
    )

  assert receive_event_with_prefix(subject, "workflow_finished:", 20)
    == "workflow_finished:failed_fatal"
  assert workflow_finished_outcome(root) == workflow_outcome.failed_fatal
}

pub fn resumed_run_with_step_recovery_retry_requested_emits_failed_after_recovery_test() {
  let root = "test/tmp/workflow-run/recovered-outcome-recovered-failure"
  let subject = process.new_subject()
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: collect\n    kind: command\n    run: collect\n    workspace: main\n",
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.new(),
      workspaces: dict.new(),
      next_attempt_indexes: dict.from_list([#("collect", 1)]),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.StepRecoveryRetryRequested,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )

  let assert Error(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      workflow_run.Dependencies(
        ..deps(subject, Some("collect")),
        checkpoint: recording_checkpoint(root, subject),
      ),
      resume,
    )

  assert receive_event_with_prefix(subject, "workflow_finished:", 20)
    == "workflow_finished:failed_after_recovery"
  assert workflow_finished_outcome(root)
    == workflow_outcome.failed_after_recovery
  assert step_finished_outcome(root, "collect") == workflow_outcome.failed_fatal
}

pub fn contracted_recovery_with_started_attempt_records_recovery_input_manifest_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-recovery-started-attempt"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\nsteps:\n  - id: collect\n    kind: command\n    run: echo findings\n",
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.new(),
      workspaces: dict.new(),
      next_attempt_indexes: dict.from_list([#("collect", 1)]),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.NoStepRecovery,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
      resume,
    )

  let records = ledger_records(root)
  let assert [first, ..] = records
  assert record.kind(first.body) == "workflow_run_inputs_recorded"
  let input_manifest = read_input_manifest(root, "run-1")
  assert input_manifest.diagnostics == ["recovered_after_steps_started"]
}

pub fn contracted_recovery_records_missing_inputs_once_and_preserves_outputs_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/contract-recovery-idempotence"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  inputs:\n    prompt:\n      type: text\n      source: issue_context\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect\n        field: stdout\nsteps:\n  - id: collect\n    kind: command\n    run: echo findings\n",
    )
  let artifact =
    step_artifact.from_command_result(
      "collect",
      0,
      "# Accepted\n",
      "",
      False,
      [],
      orchestrator().artifact_limits,
    )
  let recorded =
    workflow_checkpoint.ArtifactWritten(
      ref: "runs/run-1/outputs.v1.json",
      sha256: "already-recorded",
      bytes: 1,
    )
  let resume =
    workflow_run.ResumeState(
      artifacts: dict.from_list([#("collect", artifact)]),
      workspaces: dict.new(),
      next_attempt_indexes: dict.new(),
      run_root: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123"),
      recovery_evidence: workflow_outcome.NoStepRecovery,
      pi_session_continuations: dict.new(),
      contract_inputs_recorded: None,
      contract_outputs_recorded: None,
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, None),
      checkpoint: workflow_checkpoint.ledger_writer(root, fn() { 123 }),
    )

  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
      resume,
    )
  let output_manifest = read_output_manifest(root, "run-1")
  let assert [findings] = output_manifest.outputs
  assert findings.value.ref == Some("runs/run-1/outputs/findings.md")
  let assert Ok(blob) =
    simplifile.read(
      root <> "/.scherzo-state/artifacts/runs/run-1/outputs/findings.md",
    )
  assert blob == "# Accepted\n"
  let input_manifest = read_input_manifest(root, "run-1")
  assert input_manifest.diagnostics == ["recovered_after_steps_started"]

  let resume =
    workflow_run.ResumeState(
      ..resume,
      contract_inputs_recorded: Some(recorded),
      contract_outputs_recorded: Some(recorded),
    )
  let assert Ok(_) =
    workflow_run.execute_with_resume(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
      resume,
    )
  let contract_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_run_inputs_recorded"
      || record.kind(ledger_record.body) == "workflow_run_outputs_recorded"
    })
  assert list.length(contract_records) == 2
}

fn workflow_step_recovery_result(
  decision: String,
  summary: String,
  reason: String,
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(None, False, "test", [
    result_artifact.ToolCallSubmission(
      name: "submit_workflow_step_recovery_result",
      arguments_json: Some(
        "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\""
        <> decision
        <> "\",\"summary\":\""
        <> summary
        <> "\",\"reason\":\""
        <> reason
        <> "\"}",
      ),
      status: Some("success"),
      sibling_count: 1,
      receipt_json: None,
    ),
  ])
}

fn first_index_of_kind(
  records: List(record.LedgerRecord),
  kind: String,
  index: Int,
) -> Result(Int, Nil) {
  case records {
    [] -> Error(Nil)
    [ledger_record, ..rest] ->
      case record.kind(ledger_record.body) == kind {
        True -> Ok(index)
        False -> first_index_of_kind(rest, kind, index + 1)
      }
  }
}

pub fn fatal_command_step_recovery_retries_original_step_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-command-retry"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: fixable\n    kind: command\n    run: ignored\n    workspace: main\n    recover:\n      attempts: 1\n      prompt: repair the workspace\n",
    )
  reset_dir("test/tmp/workflow-run/workspaces/implementation/ABC-123")
  let checkpoint = hidden_local_path_checkpoint(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(
          subject,
          "command:"
            <> context.step_id
            <> ":"
            <> int.to_string(context.attempt_index),
        )
        let exit_code = case context.attempt_index == 1 {
          True -> 1
          False -> 0
        }
        step_artifact.from_command_result(
          context.step_id,
          exit_code,
          "stdout:" <> int.to_string(context.attempt_index),
          "stderr:" <> int.to_string(context.attempt_index),
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(
          subject,
          "agent:"
            <> context.step_id
            <> ":"
            <> int.to_string(context.attempt_index)
            <> ":"
            <> prompt_text(prompt_mode),
        )
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "retry_requested",
            "patched",
            "ready for retry",
          )),
        )
      },
      checkpoint: checkpoint,
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "fixable")
  assert artifact.status == step_artifact.StepSucceeded
  assert receive_event(subject) == "prepare:fixable:main:"
  assert receive_event(subject) == "command:fixable:1"
  assert receive_event(subject) == "after:fixable"
  let recovery_event = receive_event(subject)
  assert string.starts_with(
    recovery_event,
    "agent:fixable:1:repair the workspace",
  )
  assert receive_event(subject) == "prepare:fixable:main:"
  assert receive_event(subject) == "command:fixable:2"
  assert receive_event(subject) == "after:fixable"

  let records = ledger_records(root)
  let assert Ok(first_finished_index) =
    first_index_of_kind(records, "step_attempt_finished", 0)
  let assert Ok(recovery_started_index) =
    first_index_of_kind(records, "workflow_step_recovery_started", 0)
  let assert Ok(recovery_finished_index) =
    first_index_of_kind(records, "workflow_step_recovery_finished", 0)
  assert first_finished_index < recovery_started_index
  assert recovery_started_index < recovery_finished_index

  let step_finished_records =
    records
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "step_attempt_finished"
    })
  let assert [first_finished, second_finished] = step_finished_records
  let assert record.StepAttemptFinished(
    _,
    _,
    "fixable",
    1,
    "failed_fatal",
    _,
    _,
    _,
    _,
    _,
    _,
  ) = first_finished.body
  let assert record.StepAttemptFinished(
    _,
    _,
    "fixable",
    2,
    "completed",
    _,
    _,
    _,
    _,
    _,
    _,
  ) = second_finished.body
  let recovery_records =
    records
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
  let assert [recovery_finished] = recovery_records
  let assert record.WorkflowStepRecoveryFinished(
    _,
    _,
    "fixable",
    1,
    1,
    _,
    "retry_requested",
    "patched",
    "ready for retry",
    Some(2),
  ) = recovery_finished.body
}

pub fn continued_failures_do_not_start_step_recovery_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-continue-noop"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: optional\n    kind: command\n    run: ignored\n    workspace: main\n    on_failure: continue\n    recover:\n      attempts: 1\n      prompt: ignored\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, Some("optional")),
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Ok(_) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let recovery_records =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      let kind = record.kind(ledger_record.body)
      kind == "workflow_step_recovery_started"
      || kind == "workflow_step_recovery_finished"
    })
  assert recovery_records == []
}

pub fn disabled_step_recovery_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-disabled"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: broken\n    kind: command\n    run: ignored\n    workspace: main\n    recover:\n      enabled: false\n      attempts: 1\n      prompt: ignored\n",
    )
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, Some("broken")),
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert_no_recovery_records(root)
}

fn assert_no_recovery_records(root: String) {
  assert recovery_records(root) == []
}

fn recovery_records(root: String) -> List(record.LedgerRecord) {
  ledger_records(root)
  |> list.filter(fn(ledger_record) {
    let kind = record.kind(ledger_record.body)
    kind == "workflow_step_recovery_started"
    || kind == "workflow_step_recovery_finished"
  })
}

fn broken_command_recovery_dag(extra_yaml: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: broken\n    kind: command\n    run: ignored\n    workspace: main\n"
      <> extra_yaml,
    )
  dag
}

fn prompt_mode_name(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(_) -> "original"
    workflow_attempt.StructuredOutputRetryPrompt(_) -> "structured_retry"
    workflow_attempt.StepRecoveryPrompt(_) -> "recovery"
    workflow_attempt.RecoveryPrompt(_) -> "recovery"
  }
}

fn structured_output_spec_kind(context: workflow_run.StepContext) -> String {
  case
    list.key_find(
      context.extra_pi_env,
      structured_output_tool_spec.spec_env_var,
    )
  {
    Ok(path) -> {
      let assert Ok(contents) = simplifile.read(path)
      case string.contains(contents, "submit_workflow_step_recovery_result") {
        True -> "recovery"
        False ->
          case string.contains(contents, "submit_review_result") {
            True -> "review"
            False -> "unknown"
          }
      }
    }
    Error(_) -> "missing"
  }
}

fn recovery_tool_call(
  arguments_json: Option(String),
  sibling_count: Int,
) -> result_artifact.ToolCallSubmission {
  result_artifact.ToolCallSubmission(
    name: "submit_workflow_step_recovery_result",
    arguments_json: arguments_json,
    status: Some("success"),
    sibling_count: sibling_count,
    receipt_json: None,
  )
}

fn recovery_result_with_calls(
  calls: List(result_artifact.ToolCallSubmission),
) -> result_artifact.ResultArtifact {
  result_artifact.from_final_response_with_tool_calls(
    None,
    False,
    "test",
    calls,
  )
}

pub fn absent_step_recovery_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-absent"
  reset_dir(root)
  let dependencies =
    workflow_run.Dependencies(
      ..deps(subject, Some("broken")),
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(""),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert_no_recovery_records(root)
}

pub fn fatal_agent_step_recovery_preserves_original_definition_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-agent-retry"
  reset_dir(root)
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nsteps:\n  - id: draft\n    kind: agent\n    prompt: draft prompt\n    workspace: main\n    model: openai/gpt-5.1\n    structured_output:\n      artifact_name: review_result\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      schema:\n        required: [summary, findings]\n    recover:\n      attempts: 1\n      prompt: repair draft workspace\n      model: github-copilot/gpt-5.1-codex\n",
    )
  let checkpoint = hidden_local_path_checkpoint(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        effective: config_types.EffectiveConfig,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(
          subject,
          "agent_call:"
            <> prompt_mode_name(prompt_mode)
            <> ":"
            <> int.to_string(context.attempt_index)
            <> ":"
            <> structured_output_spec_kind(context)
            <> ":"
            <> effective.pi.command
            <> ":"
            <> prompt_text(prompt_mode)
            <> ":"
            <> context.workspace_path,
        )
        case prompt_mode_name(prompt_mode), context.attempt_index {
          "recovery", _ ->
            Ok(
              success_agent_with_result(workflow_step_recovery_result(
                "retry_requested",
                "patched",
                "ready for retry",
              )),
            )
          "original", 1 ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("draft failed")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: Some(issue()),
            ))
          _, _ ->
            Ok(
              success_agent_with_result(review_result_tool_call(
                Some("{\"summary\":\"ok\",\"findings\":[]}"),
                Some("success"),
              )),
            )
        }
      },
      checkpoint: checkpoint,
    )

  let assert Ok(success) =
    workflow_run.execute(
      issue(),
      dag,
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  let assert Ok(artifact) = dict.get(success.artifacts, "draft")
  assert artifact.status == step_artifact.StepSucceeded
  let assert Some(step_artifact.StructuredOutputValid(_)) =
    artifact.structured_output
  assert receive_event(subject) == "prepare:draft:main:"
  let first_original = receive_event(subject)
  assert string.starts_with(first_original, "agent_call:original:1:")
  assert string.contains(first_original, "--model 'openai/gpt-5.1'")
  assert string.contains(first_original, ":draft prompt:")
  assert string.contains(
    first_original,
    ":test/tmp/workflow-run/workspaces/implementation/ABC-123/main",
  )
  assert receive_event(subject) == "after:draft"
  let recovery = receive_event(subject)
  assert string.starts_with(recovery, "agent_call:recovery:1:")
  assert string.contains(recovery, "--model 'github-copilot/gpt-5.1-codex'")
  assert string.contains(recovery, "repair draft workspace")
  assert receive_event(subject) == "prepare:draft:main:"
  let second_original = receive_event(subject)
  assert string.starts_with(second_original, "agent_call:original:2:")
  assert string.contains(second_original, "--model 'openai/gpt-5.1'")
  assert string.contains(second_original, ":draft prompt:")
  assert string.contains(
    second_original,
    ":test/tmp/workflow-run/workspaces/implementation/ABC-123/main",
  )
  assert receive_event(subject) == "after:draft"
}

pub fn step_recovery_gave_up_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-gave-up"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "gave_up",
            "not fixable",
            "needs human help",
          )),
        )
      },
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert failure.failed_step_id == Some("broken")
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "gave_up",
      "not fixable",
      "needs human help",
      None,
    ),
  ] =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })
}

pub fn failed_recovery_worker_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-worker-failed"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("recovery failed")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: Some(issue()),
        ))
      },
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "worker_failed",
      "Recovery worker failed",
      _,
      None,
    ),
  ] =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })
}

pub fn timed_out_recovery_worker_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-worker-timeout"
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiTurnTimeout),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: Some(issue()),
        ))
      },
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert step_finished_outcome(root, "broken") == workflow_outcome.failed_fatal
  let step_attempt_starts =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      case ledger_record.body {
        record.StepAttemptStarted(step_id: step_id, ..) -> step_id == "broken"
        _ -> False
      }
    })
  assert list.length(step_attempt_starts) == 1
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "worker_failed",
      "Recovery worker failed",
      reason,
      None,
    ),
  ] =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })
  assert string.contains(reason, "pi turn timeout elapsed before agent_end")
}

fn assert_invalid_recovery_output_failure(
  root: String,
  result: result_artifact.ResultArtifact,
  expected_code: String,
) {
  let subject = process.new_subject()
  reset_dir(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Ok(success_agent_with_result(result))
      },
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let finished_records =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "invalid_output",
      "Recovery output was invalid",
      reason,
      None,
    ),
  ] = finished_records
  assert string.contains(reason, expected_code)
}

pub fn missing_recovery_result_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-missing-output",
    success_agent_with_response(Some("no tool call"), False).result,
    "recovery_result_missing",
  )
}

pub fn duplicate_recovery_result_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-duplicate-output",
    recovery_result_with_calls([
      recovery_tool_call(
        Some(
          "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"retry_requested\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        1,
      ),
      recovery_tool_call(
        Some(
          "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"retry_requested\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        1,
      ),
    ]),
    "recovery_result_duplicate",
  )
}

pub fn recovery_result_missing_arguments_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-missing-arguments",
    recovery_result_with_calls([recovery_tool_call(None, 1)]),
    "recovery_result_missing_arguments",
  )
}

pub fn malformed_recovery_result_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-malformed-output",
    recovery_result_with_calls([recovery_tool_call(Some("{"), 1)]),
    "recovery_result_malformed",
  )
}

pub fn wrong_recovery_artifact_type_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-wrong-artifact-type",
    recovery_result_with_calls([
      recovery_tool_call(
        Some(
          "{\"schema_version\":1,\"artifact_type\":\"wrong\",\"decision\":\"retry_requested\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        1,
      ),
    ]),
    "recovery_result_wrong_artifact_type",
  )
}

pub fn wrong_recovery_schema_version_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-wrong-schema-version",
    recovery_result_with_calls([
      recovery_tool_call(
        Some(
          "{\"schema_version\":2,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"retry_requested\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        1,
      ),
    ]),
    "recovery_result_wrong_schema_version",
  )
}

pub fn sibling_tool_calls_in_recovery_result_preserve_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-sibling-tool-calls",
    recovery_result_with_calls([
      recovery_tool_call(
        Some(
          "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"retry_requested\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        2,
      ),
    ]),
    "recovery_result_has_sibling_tool_calls",
  )
}

pub fn invalid_recovery_result_preserves_original_failure_test() {
  assert_invalid_recovery_output_failure(
    "test/tmp/workflow-run/recovery-invalid-output",
    recovery_result_with_calls([
      recovery_tool_call(
        Some(
          "{\"schema_version\":1,\"artifact_type\":\"workflow_step_recovery_result\",\"decision\":\"keep_trying\",\"summary\":\"patched\",\"reason\":\"done\"}",
        ),
        1,
      ),
    ]),
    "recovery_result_invalid_decision",
  )
}

fn assert_recovery_artifact_write_failure_preserves_original_failure(
  root: String,
  result: result_artifact.ResultArtifact,
) {
  let subject = process.new_subject()
  reset_dir(root)
  let base = deps(subject, Some("broken"))
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Ok(success_agent_with_result(result))
      },
      checkpoint: workflow_checkpoint.Writer(
        ..hidden_local_path_checkpoint(root),
        write_recovery_artifact: fn(_) {
          Error(workflow_checkpoint.CheckpointArtifactFailed(
            "write failed TOP_SECRET /Users/example/project",
          ))
        },
      ),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      ["TOP_SECRET", "/Users/example/project"],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert step_finished_outcome(root, "broken") == workflow_outcome.failed_fatal
  let step_attempt_starts =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      case ledger_record.body {
        record.StepAttemptStarted(step_id: step_id, ..) -> step_id == "broken"
        _ -> False
      }
    })
  assert list.length(step_attempt_starts) == 1
  let finished_records =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "artifact_write_failed",
      "Recovery artifact write failed",
      reason,
      None,
    ),
  ] = finished_records
  assert string.contains(reason, "artifact_write_failed")
  assert string.contains(reason, "[REDACTED]")
  assert !string.contains(reason, "TOP_SECRET")
  assert !string.contains(reason, "/Users/example/project")
}

pub fn recovery_artifact_write_failure_preserves_original_failure_test() {
  assert_recovery_artifact_write_failure_preserves_original_failure(
    "test/tmp/workflow-run/recovery-artifact-write-failure",
    workflow_step_recovery_result(
      "retry_requested",
      "patched",
      "ready for retry",
    ),
  )
}

pub fn gave_up_recovery_artifact_write_failure_preserves_original_failure_test() {
  assert_recovery_artifact_write_failure_preserves_original_failure(
    "test/tmp/workflow-run/recovery-artifact-write-failure-gave-up",
    workflow_step_recovery_result("gave_up", "not fixable", "needs human help"),
  )
}

pub fn recovery_started_checkpoint_failure_preserves_original_failure_test() {
  let root = "test/tmp/workflow-run/recovery-started-checkpoint-failure"
  reset_dir(root)
  let agent_subject = process.new_subject()
  let base = deps(process.new_subject(), Some("broken"))
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(agent_subject, "agent_called")
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "retry_requested",
            "patched",
            "ready for retry",
          )),
        )
      },
      checkpoint: workflow_checkpoint.Writer(
        ..hidden_local_path_checkpoint(root),
        step_recovery_started: fn(_) {
          Error(workflow_checkpoint.CheckpointAppendFailed("start failed"))
        },
      ),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert recovery_records(root) == []
  test_async.assert_no_extra_message_within(agent_subject, 50)
}

pub fn recovery_finished_checkpoint_failure_preserves_original_failure_test() {
  let root = "test/tmp/workflow-run/recovery-finished-checkpoint-failure"
  reset_dir(root)
  let base = deps(process.new_subject(), Some("broken"))
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "retry_requested",
            "patched",
            "ready for retry",
          )),
        )
      },
      checkpoint: workflow_checkpoint.Writer(
        ..hidden_local_path_checkpoint(root),
        step_recovery_finished: fn(_) {
          Error(workflow_checkpoint.CheckpointAppendFailed("finish failed"))
        },
      ),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let recovery_kinds =
    recovery_records(root)
    |> list.map(fn(ledger_record) { record.kind(ledger_record.body) })
  assert recovery_kinds == ["workflow_step_recovery_started"]
  let step_finished_count =
    ledger_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "step_attempt_finished"
    })
    |> list.length
  assert step_finished_count == 1
}

pub fn step_recovery_finished_redacts_secrets_and_artifact_uses_decision_test() {
  let root = "test/tmp/workflow-run/recovery-redacted-finished"
  reset_dir(root)
  let base = deps(process.new_subject(), Some("broken"))
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      agent_step: fn(
        _issue,
        _context,
        _prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "gave_up",
            "patched TOP_SECRET",
            "reason TOP_SECRET",
          )),
        )
      },
      checkpoint: hidden_local_path_checkpoint(root),
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      ["TOP_SECRET"],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  let assert [
    record.WorkflowStepRecoveryFinished(
      _,
      _,
      "broken",
      1,
      1,
      _,
      "gave_up",
      "patched [REDACTED]",
      "reason [REDACTED]",
      None,
    ),
  ] =
    recovery_records(root)
    |> list.filter(fn(ledger_record) {
      record.kind(ledger_record.body) == "workflow_step_recovery_finished"
    })
    |> list.map(fn(ledger_record) { ledger_record.body })

  let assert Ok(payload) =
    simplifile.read(
      artifact_root(root)
      <> "/"
      <> artifact_store.recovery_artifact_ref(
        "run-1",
        "broken",
        1,
        1,
        "workflow_step_recovery_result",
      ),
    )
  assert string.contains(payload, "\"decision\":\"gave_up\"")
  assert !string.contains(payload, "\"result\"")
  assert !string.contains(payload, "TOP_SECRET")
}

pub fn exhausted_step_recovery_budget_preserves_original_failure_test() {
  let subject = process.new_subject()
  let root = "test/tmp/workflow-run/recovery-budget-exhausted"
  reset_dir(root)
  let checkpoint = hidden_local_path_checkpoint(root)
  let base = deps(subject, None)
  let dependencies =
    workflow_run.Dependencies(
      ..base,
      command_step: fn(
        context: workflow_run.StepContext,
        _command,
        _timeout,
        secrets,
        limits,
      ) {
        process.send(
          subject,
          "command_attempt:" <> int.to_string(context.attempt_index),
        )
        step_artifact.from_command_result(
          context.step_id,
          1,
          "stdout",
          "stderr",
          False,
          secrets,
          limits,
        )
      },
      agent_step: fn(
        _issue,
        context: workflow_run.StepContext,
        prompt_mode,
        _attempt_context,
        _effective,
        _tracker,
        _emit_update,
        _command_ready,
        _record_pi_session,
      ) {
        process.send(
          subject,
          "agent_call:"
            <> prompt_mode_name(prompt_mode)
            <> ":"
            <> int.to_string(context.attempt_index),
        )
        Ok(
          success_agent_with_result(workflow_step_recovery_result(
            "retry_requested",
            "patched",
            "ready for retry",
          )),
        )
      },
      checkpoint: checkpoint,
    )

  let assert Error(failure) =
    workflow_run.execute(
      issue(),
      broken_command_recovery_dag(
        "    recover:\n      attempts: 1\n      prompt: repair\n",
      ),
      orchestrator(),
      empty_tracker(),
      [],
      "run-1",
      dependencies,
    )

  assert failure.reason == "workflow_step_failed"
  assert receive_event(subject) == "prepare:broken:main:"
  assert receive_event(subject) == "command_attempt:1"
  assert receive_event(subject) == "after:broken"
  assert receive_event(subject) == "agent_call:recovery:1"
  assert receive_event(subject) == "prepare:broken:main:"
  assert receive_event(subject) == "command_attempt:2"
  assert receive_event(subject) == "after:broken"
  assert receive_event(subject)
    == "cleanup:test/tmp/workflow-run/workspaces/implementation/ABC-123"
  test_async.assert_no_extra_message_within(subject, 50)
  let kinds =
    recovery_records(root)
    |> list.map(fn(ledger_record) { record.kind(ledger_record.body) })
  assert kinds
    == [
      "workflow_step_recovery_started",
      "workflow_step_recovery_finished",
    ]
}
