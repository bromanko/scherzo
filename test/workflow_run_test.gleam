import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_dag
import scherzo/workflow_run
import scherzo/workflow_scheduler
import scherzo/workspace_run
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
  hooks: config_types.DagHooksConfig,
  source: config_types.WorkspaceProfileSource,
) -> config_types.WorkspaceHookProfile {
  config_types.WorkspaceHookProfile(name: name, hooks: hooks, source: source)
}

fn legacy_workspace_profiles(
  hooks: config_types.DagHooksConfig,
) -> config_types.WorkspaceHookProfiles {
  config_types.WorkspaceHookProfiles(
    default_profile: "default",
    profiles: dict.from_list([
      #(
        "default",
        workspace_profile("default", hooks, config_types.LegacyWorkspaceHooks),
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
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn success_agent(prompt: String) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue()),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.ResultArtifact(
      final_response: Some("response:" <> prompt),
      truncated: False,
      source: "test",
    ),
  )
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

fn implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 3\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement prompt\n    workspace: main\n  - id: test_after_implement\n    kind: command\n    depends_on: [implement]\n    run: test command\n    workspace: main\n    on_failure: continue\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: code review prompt\n    workspace:\n      name: code-review\n      from: main\n  - id: apply_feedback\n    kind: agent\n    depends_on: [test_after_implement, code_review]\n    prompt: apply {{ steps.code_review.final_response }} {{ steps.test_after_implement.exit_code }}\n    workspace: main\n",
    )
  dag
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
    scheduler_statuses: scheduler_statuses,
    artifacts: artifacts,
    prepared_workspaces: prepared_workspaces,
    step_attempts: step_attempts,
    token_totals: session_tokens.zero_token_totals(),
    final_issue: None,
    turns: 0,
    warnings: [],
    pi_session_continuations: dict.new(),
  )
}

pub fn command_default_timeout_uses_selected_workspace_profile_test() {
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
              config_types.LegacyWorkspaceHooks,
            ),
          ),
          #(
            "noop",
            workspace_profile(
              "noop",
              noop_hooks,
              config_types.ConfiguredWorkspaceProfile,
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
  assert receive_event(subject) == "timeout:42"
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
    == "default_step:pi --mode rpc --no-session --model 'google/gemini-2.5-flash' --thinking 'low'"
  assert receive_event(command_subject)
    == "full_override:pi --mode rpc --no-session --model 'github-copilot/gpt-5.1-codex' --thinking 'high'"
  assert receive_event(command_subject)
    == "partial_thinking:pi --mode rpc --no-session --model 'google/gemini-2.5-flash' --thinking 'xhigh'"
  assert receive_event(command_subject)
    == "partial_model:pi --mode rpc --no-session --model 'openai/gpt-5.1' --thinking 'low'"
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
