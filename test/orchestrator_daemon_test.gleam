import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/error
import scherzo/handoff
import scherzo/handoff_format
import scherzo/linear
import scherzo/linear_triage
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/poll_jitter
import scherzo/path
import scherzo/result_artifact
import scherzo/runtime_bundle
import scherzo/scheduled_failure_reporter
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_fingerprint
import scherzo/workflow_policy
import scherzo/workflow_run
import scherzo/workspace_run
import simplifile
import support/expected_crash
import test_async

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn ms(iso: String) -> Int {
  let assert Ok(time) = birl.parse(iso)
  birl.to_unix_milli(time)
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  workflow_text_with_linear_contract(root, max_concurrent, "")
}

fn enforcing_linear_contract_text() -> String {
  "linear_contract:
  workflow_label_prefix: \"workflow:\"
  workflow_labels: [implementation]
  enforce_issue_workflow_labels: true
"
}

fn workflow_text_with_linear_contract(
  root: String,
  max_concurrent: Int,
  linear_contract_text: String,
) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  active_states: [Todo]
  dispatch_states: [Todo]
  terminal_states: [Done]
workspace:
  root: " <> root <> "
  hooks:
    create: |
      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"
    before_step: |
      test -d \"$SCHERZO_WORKSPACE_PATH\"
    after_step: |
      true
    remove: |
      rm -rf \"$SCHERZO_WORKSPACE_PATH\"
    timeout_ms: 60000
polling:
  interval_ms: 1000
agent:
  max_concurrent_agents: " <> int_to_string(max_concurrent) <> "
  max_retry_attempts: 3
  max_sessions_per_issue: 2
pi:
  command: fake
routing:
  workflow_label_prefix: \"workflow:\"
  require_exactly_one_workflow_label: false
  default_workflow: implementation
  workflows:
    implementation: workflows/implementation.yaml
" <> linear_contract_text
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  write_workflow_files(dir, workflow_text(dir <> "/workspaces", max_concurrent))
}

fn write_enforcing_workflow(dir: String, max_concurrent: Int) -> String {
  write_workflow_with_contract(
    dir,
    max_concurrent,
    enforcing_linear_contract_text(),
  )
}

fn write_enforcing_split_state_workflow(
  dir: String,
  max_concurrent: Int,
) -> String {
  reset_dir(dir)
  let config_text =
    workflow_text_with_linear_contract(
      dir <> "/workspaces",
      max_concurrent,
      enforcing_linear_contract_text(),
    )
    |> string.replace(
      each: "  active_states: [Todo]\n  dispatch_states: [Todo]",
      with: "  active_states: [Todo, In Progress]\n  dispatch_states: [Todo]",
    )
  write_workflow_files(dir, config_text)
}

fn write_yaml_agent_workflow(dir: String) -> String {
  reset_dir(dir)
  write_workflow_files(dir, workflow_text(dir <> "/workspaces", 1))
}

fn write_parallel_yaml_agent_workflow(dir: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(config_path, workflow_text(dir <> "/workspaces", 1))
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
max_parallel_steps: 2
steps:
  - id: alpha
    kind: agent
    prompt: prompts/task.md
    workspace: alpha
  - id: beta
    kind: agent
    prompt: prompts/task.md
    workspace: beta
  - id: final
    kind: command
    depends_on: [alpha, beta]
    run: final
    workspace: alpha
",
    )
  config_path
}

fn write_yaml_workflow(dir: String, _marker: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let root = dir <> "/workspaces"
  let assert Ok(Nil) = simplifile.write(config_path, workflow_text(root, 1))
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: final_test
    kind: command
    run: sh -c 'exit 1'
    workspace: main
",
    )
  config_path
}

fn write_scheduled_reporting_workflow(dir: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, 1)
        <> "scheduled_jobs:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n    on_failure:\n      linear:\n        enabled: true\n        state: Triage\n        labels:\n          - job:scheduled-job\n        dedupe: open_issue_per_job\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: scheduled_command
    kind: command
    run: exit 1
    workspace: main
",
    )
  config_path
}

fn write_scheduled_command_workflow(
  dir: String,
  max_concurrent: Int,
) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, max_concurrent)
        <> "scheduled_jobs:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: scheduled_command
    kind: command
    run: echo scheduled
    workspace: main
",
    )
  config_path
}

fn write_real_failing_command_workflow(dir: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, 1)
        <> "artifact_limits:\n  command_stream_max_chars: 40\n  template_field_max_chars: 200\n  workflow_summary_max_chars: 200\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: final_test
    kind: command
    run: |
      touch ../.scherzo-keep-workspace
      printf 'stdout-abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\\n'
      printf 'stderr-abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\\n' >&2
      exit 9
    workspace: main
",
    )
  config_path
}

fn write_workflow_with_contract(
  dir: String,
  max_concurrent: Int,
  linear_contract_text: String,
) -> String {
  reset_dir(dir)
  write_workflow_files(
    dir,
    workflow_text_with_linear_contract(
      dir <> "/workspaces",
      max_concurrent,
      linear_contract_text,
    ),
  )
}

fn write_workflow_files(dir: String, config_text: String) -> String {
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(config_path, config_text)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    workspace: main
",
    )
  config_path
}

fn success(
  final: tracker_issue.Issue,
  workspace_path: String,
) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(final),
    final_classification: agent_types.FinalTerminal,
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(None, False, "none"),
  )
}

fn base_dependencies(
  client: tracker.Client,
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    make_linear_commands: fn(_) { disabled_linear_commands() },
    make_triage: fn(_, _) { linear_triage.disabled_client() },
    make_scheduled_failure_reporter: fn(_) {
      scheduled_failure_reporter.disabled_client()
    },
    workflow_run_dependencies: fake_workflow_run_dependencies(log_subject),
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, _, _) {
      process.send(log_subject, event)
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { hub.start(10, fn() { 42 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

pub fn daemon_schedules_jittered_recurring_poll_after_immediate_tick_test() {
  let workflow_path = write_workflow("test/tmp/daemon-poll-jitter", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let log_fields_subject = process.new_subject()
  let timer_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      logger: fn(_, event, fields, _) {
        process.send(log_fields_subject, #(event, fields))
        Ok(Nil)
      },
      send_after: fn(_, delay_ms, message) {
        process.send(timer_subject, #(delay_ms, message))
        daemon.TestTimer(delay_ms)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  assert process.receive(timer_subject, within: 1000)
    == Ok(#(0, daemon.PollTick(1)))
  process.send(started.data, daemon.PollTick(1))

  let assert Ok(#(delay_ms, daemon.PollTick(2))) =
    process.receive(timer_subject, within: 1000)
  let jitter_bound_ms = poll_jitter.jitter_bound_ms(1000)
  assert delay_ms > 0
  assert delay_ms >= 1000 - jitter_bound_ms
  assert delay_ms <= 1000 + jitter_bound_ms

  let assert Ok(fields) =
    wait_for_log_fields(log_fields_subject, "next_poll_scheduled", 20)
  let field_map = dict.from_list(fields)
  assert dict.get(field_map, "generation") == Ok("2")
  assert dict.get(field_map, "polling_interval_ms") == Ok("1000")
  assert dict.get(field_map, "polling_jitter_bound_ms")
    == Ok(int_to_string(jitter_bound_ms))
  assert dict.get(field_map, "next_poll_delay_ms")
    == Ok(int_to_string(delay_ms))
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn disabled_linear_commands() -> linear.CommandClient {
  linear.CommandClient(fetch_comments: fn(_, _) { Ok([]) }, post_ack: fn(_, _) {
    Ok(Nil)
  })
}

type TestClockMessage {
  GetNow(process.Subject(Int))
  SetNow(Int)
  StopClock
}

fn start_test_clock(initial_ms: Int) -> process.Subject(TestClockMessage) {
  let ready = process.new_subject()
  let _ =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      test_clock_loop(subject, initial_ms)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn test_clock_loop(
  subject: process.Subject(TestClockMessage),
  now_ms: Int,
) -> Nil {
  case process.receive(subject, within: 5000) {
    Ok(GetNow(reply)) -> {
      process.send(reply, now_ms)
      test_clock_loop(subject, now_ms)
    }
    Ok(SetNow(next_ms)) -> test_clock_loop(subject, next_ms)
    Ok(StopClock) -> Nil
    Error(_) -> Nil
  }
}

fn clock_now(clock: process.Subject(TestClockMessage)) -> Int {
  let reply = process.new_subject()
  process.send(clock, GetNow(reply))
  let assert Ok(now_ms) = process.receive(reply, within: 1000)
  now_ms
}

fn set_clock(clock: process.Subject(TestClockMessage), now_ms: Int) -> Nil {
  process.send(clock, SetNow(now_ms))
}

type FetchRequest {
  FetchRequest(process.Subject(FetchDirective))
}

type FetchDirective {
  CrashFetch
  ReturnCandidates(List(tracker_issue.Issue))
}

fn fake_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      _step_id,
      attempt_index,
      workspace_ref,
      orchestrator,
      profile,
      _known,
    ) {
      let run_root =
        orchestrator.effective.workspace.root
        <> "/"
        <> workflow_id
        <> "/"
        <> issue.identifier
        <> "/"
        <> run_id
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        attempt_index: attempt_index,
        workspace_name: workspace_ref.name,
        path: run_root <> "/" <> workspace_ref.name,
        source_workspace_name: workspace_ref.from,
        source_workspace_path: None,
        workspace_profile: profile.name,
      ))
    },
    prepare_recovered_step: fn(
      _issue,
      workflow_id,
      run_id,
      expected_run_root,
      _step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      _known,
    ) {
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: expected_run_root,
        attempt_index: attempt_index,
        workspace_name: workspace_ref.name,
        path: expected_run_root <> "/" <> workspace_ref.name,
        source_workspace_name: workspace_ref.from,
        source_workspace_path: None,
        workspace_profile: profile.name,
      ))
    },
    after_step: fn(_, step_id, _, _, _) {
      process.send(log_subject, "yaml_after:" <> step_id)
    },
    cleanup_run: fn(run_root, _, _) {
      process.send(log_subject, "yaml_cleanup:" <> run_root)
      Ok(Nil)
    },
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command:" <> context.step_id)
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
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      let prompt = prompt_text(prompt_mode)
      process.send(log_subject, "yaml_agent:" <> prompt)
      emit_update(
        agent_types.RunnerPiUpdate(agent_types.PiUpdate(
          event: pi_event.TurnFinished,
          message: Some("hello"),
          raw_json: None,
          turn: Some(1),
          request_id: None,
          method: None,
          pi_session_id: None,
          tokens: session_tokens.zero_token_totals(),
          tool_name: None,
          tool_input: None,
          tool_output: None,
          tool_status: None,
        )),
      )
      Ok(agent_types.WorkerSuccess(
        final_issue: Some(issue),
        final_classification: agent_types.FinalTerminal,
        workspace_path: context.workspace_path,
        tokens: session_tokens.zero_token_totals(),
        turns: 1,
        result: result_artifact.from_final_response(Some(prompt), False, "test"),
      ))
    },
    checkpoint: workflow_checkpoint.noop_writer(),
  )
}

fn failing_command_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command_failed:" <> context.step_id)
      step_artifact.from_command_result(
        context.step_id,
        1,
        "",
        "forced scheduled failure",
        False,
        secrets,
        limits,
      )
    },
  )
}

fn scheduled_reporter_success(
  report_subject: process.Subject(
    scheduled_failure_reporter.FailureReportRequest,
  ),
) -> scheduled_failure_reporter.Client {
  scheduled_failure_reporter.Client(report_failure: fn(request) {
    process.send(report_subject, request)
    Ok(scheduled_failure_reporter.FailureReportCreated("lin-scheduled"))
  })
}

type ScheduledReportDirective {
  ScheduledReportError
  ScheduledReportSuccess
}

type DirectedScheduledReportCall {
  DirectedScheduledReportCall(
    request: scheduled_failure_reporter.FailureReportRequest,
    reply: process.Subject(ScheduledReportDirective),
  )
}

fn scheduled_reporter_directed(
  report_subject: process.Subject(DirectedScheduledReportCall),
) -> scheduled_failure_reporter.Client {
  scheduled_failure_reporter.Client(report_failure: fn(request) {
    let reply = process.new_subject()
    process.send(report_subject, DirectedScheduledReportCall(request, reply))
    case process.receive(reply, within: 1000) {
      Ok(ScheduledReportSuccess) ->
        Ok(scheduled_failure_reporter.FailureReportUpdated("lin-scheduled"))
      Ok(ScheduledReportError) -> Error(error.LinearApiRequest("boom"))
      Error(_) -> Error(error.LinearApiRequest("directive timeout"))
    }
  })
}

fn blocking_command_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command:" <> context.step_id)
      test_async.block_until_released(barrier)
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
}

fn blocking_command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      test_async.block_until_released(barrier)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError(
          "stopped:" <> context.step_id,
        )),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    },
  )
}

fn surviving_agent_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      process.send(log_subject, "agent_started")
      test_async.block_until_released(barrier)
      process.send(log_subject, "agent_survived")
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("survived_abort")),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    },
  )
}

fn command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let prompt = prompt_text(prompt_mode)
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      case process.receive(command_subject, within: 5000) {
        Ok(worker_command.QueuePrompt(message, reply)) -> {
          process.send(log_subject, "prompt:" <> message)
          process.send(reply, worker_command.Queued(Some("queued")))
          Ok(agent_types.WorkerSuccess(
            final_issue: Some(issue),
            final_classification: agent_types.FinalTerminal,
            workspace_path: context.workspace_path,
            tokens: session_tokens.zero_token_totals(),
            turns: 1,
            result: result_artifact.from_final_response(
              Some(prompt <> ":" <> message),
              False,
              "test",
            ),
          ))
        }
        Ok(other) -> {
          let _ = other
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("unexpected_command")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: Some(issue),
          ))
        }
        Error(_) ->
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("command_timeout")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: Some(issue),
          ))
      }
    },
  )
}

fn crashing_command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
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
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      panic as "yaml agent crashed"
    },
  )
}

fn fake_triage(subject: process.Subject(String)) -> linear_triage.TriageClient {
  linear_triage.TriageClient(report_invalid_workflow: fn(issue, violation) {
    process.send(
      subject,
      "triage:" <> issue.id <> ":" <> workflow_policy.violation_code(violation),
    )
    Ok(linear_triage.InvalidWorkflowReportNoop)
  })
}

fn prompt_until_queued(
  subject: process.Subject(daemon.Message),
  session_id: String,
  attempts: Int,
) -> command.CommandResult {
  let assert True = attempts > 0
  let assert Ok(result) =
    daemon.apply_operator_command(
      subject,
      command.PromptSession(session_id, "hello from operator"),
      1000,
    )
  case result.status {
    command.Queued -> result
    command.NotFound -> {
      process.sleep(50)
      prompt_until_queued(subject, session_id, attempts - 1)
    }
    _ -> result
  }
}

fn wait_for_session_status(
  subject: process.Subject(hub.Message),
  session_id: String,
  status: event.SessionStatus,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case hub.get_session(subject, session_id, 100) {
        Ok(Some(summary)) ->
          case summary.status == status {
            True -> True
            False -> {
              process.sleep(50)
              wait_for_session_status(subject, session_id, status, attempts - 1)
            }
          }
        _ -> {
          process.sleep(50)
          wait_for_session_status(subject, session_id, status, attempts - 1)
        }
      }
  }
}

fn find_event(
  events: List(event.SessionEvent),
  name: String,
) -> Option(event.SessionEvent) {
  case events {
    [] -> None
    [stored_event, ..rest] ->
      case event.name_to_string(stored_event.payload.name) == name {
        True -> Some(stored_event)
        False -> find_event(rest, name)
      }
  }
}

fn wait_for_event(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
) -> Bool {
  case wait_for_event_result(subject, event, quiet_attempts) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn wait_for_event_result(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
) -> Result(List(String), List(String)) {
  wait_for_event_result_loop(subject, event, quiet_attempts, [])
}

fn wait_for_log_fields(
  subject: process.Subject(#(String, List(#(String, String)))),
  event: String,
  quiet_attempts: Int,
) -> Result(List(#(String, String)), Nil) {
  case quiet_attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 500) {
        Ok(#(received, fields)) ->
          case received == event {
            True -> Ok(fields)
            False -> wait_for_log_fields(subject, event, quiet_attempts - 1)
          }
        Error(_) -> wait_for_log_fields(subject, event, quiet_attempts - 1)
      }
  }
}

fn wait_for_event_result_loop(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
  seen: List(String),
) -> Result(List(String), List(String)) {
  case quiet_attempts <= 0 {
    True -> Error(list.reverse(seen))
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) -> {
          let seen = [received, ..seen]
          case received == event {
            True -> Ok(list.reverse(seen))
            False ->
              wait_for_event_result_loop(subject, event, quiet_attempts, seen)
          }
        }
        Error(_) ->
          wait_for_event_result_loop(subject, event, quiet_attempts - 1, seen)
      }
  }
}

fn wait_for_event_without_event(
  subject: process.Subject(String),
  wanted: String,
  forbidden: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          case received == forbidden {
            True -> False
            False ->
              case received == wanted {
                True -> True
                False ->
                  wait_for_event_without_event(
                    subject,
                    wanted,
                    forbidden,
                    attempts - 1,
                  )
              }
          }
        Error(_) -> False
      }
  }
}

fn wait_for_monitor_down(monitor: process.Monitor, timeout_ms: Int) -> Bool {
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { True })
  case process.selector_receive(selector, within: timeout_ms) {
    Ok(True) -> True
    Ok(False) -> False
    Error(_) -> False
  }
}

fn load_test_projection(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
}

fn load_test_records(root: String) -> List(record.LedgerRecord) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
}

fn append_test_ledger_bodies(
  root: String,
  bodies: List(record.RecordBody),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = test_records_for_bodies(bodies, 100, 1)
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn test_records_for_bodies(
  bodies: List(record.RecordBody),
  at_ms: Int,
  sequence: Int,
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> []
    [body, ..rest] -> [
      record.new(at_ms, sequence, body),
      ..test_records_for_bodies(rest, at_ms + 100, sequence + 1)
    ]
  }
}

fn append_succeeded_scheduled_run(
  root: String,
  due_at_ms: Int,
  run_id: String,
) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      "automatic",
      due_at_ms,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      due_at_ms,
      due_at_ms + 100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
    record.ScheduledRunSucceeded(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      1,
      due_at_ms + 200,
      0,
      0,
    ),
  ])
}

fn append_started_scheduled_run(root: String, run_id: String) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
      1000,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      1000,
      1100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
  ])
}

fn append_retry_waiting_scheduled_run(root: String, run_id: String) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
      1000,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      1000,
      1100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
    record.ScheduledRunFailed(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      1,
      1200,
      "workflow_command_failed",
      False,
      Some(root <> "/implementation/scheduled/scheduled-job/" <> run_id),
    ),
    record.ScheduledRunRetryScheduled(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      2,
      10_000,
      1,
      "workflow_command_failed",
    ),
  ])
}

fn has_scheduled_due(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledJobDue(_, _, _, body_run_id, _) -> body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_pending(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunPending(_, _, _, body_run_id, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_started(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunStarted(_, _, _, _, body_run_id, _, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_succeeded(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunSucceeded(_, _, _, body_run_id, _, _, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_started_attempt(
  records: List(record.LedgerRecord),
  run_id: String,
  attempt: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunStarted(_, _, _, _, body_run_id, body_attempt, _, _) ->
        body_run_id == run_id && body_attempt == attempt
      _ -> False
    }
  })
}

fn has_scheduled_failed(
  records: List(record.LedgerRecord),
  run_id: String,
  reason: String,
  retry_exhausted: Bool,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        body_reason,
        body_retry_exhausted,
        _,
      ) ->
        body_run_id == run_id
        && body_reason == reason
        && body_retry_exhausted == retry_exhausted
      _ -> False
    }
  })
}

fn has_scheduled_retry_scheduled(
  records: List(record.LedgerRecord),
  run_id: String,
  next_attempt: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunRetryScheduled(
        _,
        _,
        _,
        body_run_id,
        body_next_attempt,
        _,
        _,
        _,
      ) -> body_run_id == run_id && body_next_attempt == next_attempt
      _ -> False
    }
  })
}

fn has_scheduled_failure_reported(
  records: List(record.LedgerRecord),
  run_id: String,
  issue_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledFailureReported(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        body_issue_id,
        _,
      ) -> body_run_id == run_id && body_issue_id == issue_id
      _ -> False
    }
  })
}

fn has_scheduled_failure_report_failed(
  records: List(record.LedgerRecord),
  run_id: String,
  generation: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledFailureReportFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        _,
        _,
        _,
        body_generation,
      ) -> body_run_id == run_id && body_generation == generation
      _ -> False
    }
  })
}

fn has_scheduled_skip(
  records: List(record.LedgerRecord),
  reason: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledJobSkipped(_, _, _, _, body_reason, _) ->
        body_reason == reason
      _ -> False
    }
  })
}

fn has_step_success(
  records: List(record.LedgerRecord),
  step_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.StepAttemptFinished(
        _,
        _,
        body_step_id,
        _,
        outcome,
        _,
        _,
        _,
        _,
        _,
        _,
      ) -> body_step_id == step_id && outcome == "completed"
      _ -> False
    }
  })
}

fn wait_for_records(
  root: String,
  predicate: fn(List(record.LedgerRecord)) -> Bool,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False -> {
      let records = load_test_records(root)
      case predicate(records) {
        True -> True
        False -> {
          process.sleep(50)
          wait_for_records(root, predicate, attempts - 1)
        }
      }
    }
  }
}

fn start_stuck_event_hub() -> process.Subject(hub.Message) {
  let ready = process.new_subject()
  let barrier = test_async.new_barrier()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      test_async.block_until_released(barrier)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

pub fn daemon_shutdown_stops_event_hub_test() {
  let workflow_path = write_workflow("test/tmp/daemon-event-hub-shutdown", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let event_hub_monitor = process.monitor(event_hub_pid)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  let stopped = wait_for_monitor_down(event_hub_monitor, 1000)
  case stopped {
    True -> Nil
    False -> hub.stop(event_hub)
  }
  process.demonitor_process(event_hub_monitor)
  assert stopped
}

pub fn daemon_shutdown_logs_event_hub_timeout_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-event-hub-shutdown-timeout", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let event_hub = start_stuck_event_hub()
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  assert daemon.shutdown(started.data, 3000) == Ok(Nil)
  assert wait_for_event(log_subject, "event_hub_shutdown_timeout", 10)
  process.kill(event_hub_pid)
}

pub fn daemon_skips_invalid_workflow_candidate_and_reports_once_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-invalid-workflow", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:issue-id:missing_workflow_label")
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert dict.has_key(snapshot.invalid_workflow_reports, "issue-id")

  process.send(started.data, daemon.PollTick(2))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_ignores_unlabeled_non_dispatch_state_candidate_test() {
  let workflow_path =
    write_enforcing_split_state_workflow(
      "test/tmp/daemon-invalid-in-progress-ignored",
      1,
    )
  let candidate = issue("issue-id", "ABC-1", "In Progress")
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        process.send(refresh_subject, "refresh")
        Ok([candidate])
      },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  test_async.assert_no_extra_message_within(refresh_subject, 200)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert !dict.has_key(snapshot.invalid_workflow_reports, "issue-id")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_ignores_workflow_labeled_non_dispatch_state_candidate_test() {
  let workflow_path =
    write_enforcing_split_state_workflow(
      "test/tmp/daemon-valid-in-progress-ignored",
      1,
    )
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "In Progress"), labels: [
      "workflow:implementation",
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        process.send(refresh_subject, "refresh")
        Ok([candidate])
      },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  test_async.assert_no_extra_message_within(refresh_subject, 200)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_reports_invalid_workflow_candidate_when_slots_are_full_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-invalid-workflow-full-slots", 1)
  let valid_candidate =
    tracker_issue.Issue(..issue("valid-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let invalid_candidate = issue("invalid-id", "ABC-2", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([invalid_candidate, valid_candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([valid_candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:invalid-id:missing_workflow_label")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_dispatches_valid_workflow_candidate_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-valid-workflow", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "dispatch_started", 10)
  test_async.assert_no_extra_message_within(triage_subject, 100)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_final_validation_blocks_new_dependency_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-validation-blocked", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let refreshed =
    tracker_issue.Issue(..candidate, blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("blocker-id"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Todo")),
      ),
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let claim_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_handoff: fn(_, _) {
        handoff.Client(
          claim_issue: fn(issue, _) {
            process.send(claim_subject, issue.id)
            Ok(Nil)
          },
          report_success: fn(_, _, _) { Ok(Nil) },
          report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
          report_failure: fn(_, _, _) { Ok(Nil) },
          report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
          report_park: fn(_) { Ok(Nil) },
        )
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(reply) = process.receive(refresh_subject, within: 1000)
  process.send(reply, refreshed)

  assert wait_for_event(
    log_subject,
    "linear_dependency_claim_validation_blocked",
    20,
  )
  test_async.assert_no_extra_message_within(claim_subject, 100)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert !dict.has_key(snapshot.claimed, candidate.id)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_final_validation_allows_terminal_blocker_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-validation-terminal", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let refreshed =
    tracker_issue.Issue(..candidate, title: "Refreshed title", blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("blocker-id"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Done")),
      ),
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(reply) = process.receive(refresh_subject, within: 1000)
  process.send(reply, refreshed)

  assert wait_for_event(log_subject, "dispatch_started", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_retry_refresh_dependency_blocked_cancels_retry_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry-dependency", 1)
  let retried = issue("retry-id", "ABC-2", "Todo")
  let blocked =
    tracker_issue.Issue(..retried, blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("blocker-id"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Todo")),
      ),
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([retried]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          _issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("boom")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, retried)
  assert wait_for_event(log_subject, "retry_scheduled", 20)

  process.send(started.data, daemon.RetryTick("retry-id", 1))
  let assert Ok(retry_refresh) = process.receive(refresh_subject, within: 1000)
  process.send(retry_refresh, blocked)
  assert wait_for_event(log_subject, "linear_dependency_retry_blocked", 20)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.retry_attempts, "retry-id")
  assert !dict.has_key(snapshot.claimed, "retry-id")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_agent_steps_get_concrete_sessions_test() {
  let dir = "test/tmp/daemon-yaml-agent-session"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "worker_exited", 20)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(session_ids, "ABC-1-42-1")
  assert list.contains(
    session_ids,
    "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
  )
  let matching_step_sessions =
    list.filter(sessions, fn(summary) {
      summary.session_id == "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483"
    })
  let assert [_step_session] = matching_step_sessions
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_operator_prompt_routes_to_agent_step_session_test() {
  let dir = "test/tmp/daemon-yaml-agent-command"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: command_ready_workflow_run_dependencies(
        log_subject,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)

  let result =
    prompt_until_queued(
      started.data,
      "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
      20,
    )
  assert result.status == command.Queued
  assert wait_for_event(log_subject, "prompt:hello from operator", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_parent_prompt_rejects_multiple_active_step_routes_test() {
  let dir = "test/tmp/daemon-yaml-parent-command-multiple"
  let workflow_path = write_parallel_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: blocking_command_ready_workflow_run_dependencies(
        log_subject,
        worker_barrier,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  assert wait_for_event(log_subject, "agent_ready", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-1-42-1", "hello from operator"),
      1000,
    )
  assert result.status == command.NotAllowed("multiple_step_command_subjects")
  test_async.release_barrier(worker_barrier)
  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_parent_abort_kills_active_step_worker_test() {
  let dir = "test/tmp/daemon-yaml-parent-abort"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: surviving_agent_workflow_run_dependencies(
        log_subject,
        worker_barrier,
      ),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_started", 20)
  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.AbortSession("ABC-1-42-1"),
      1000,
    )
  assert result.status == command.Applied
  test_async.release_barrier_if_waiting_within(worker_barrier, 100)
  assert wait_for_session_status(
    event_hub,
    "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
    event.Exited(reason.OperatorAbort),
    20,
  )
  let post_abort_logs = test_async.drain_subject(log_subject)
  assert !list.contains(post_abort_logs, "agent_survived")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(event_hub)
}

pub fn daemon_yaml_agent_step_crash_cleans_command_route_test() {
  use <- expected_crash.suppressing([
    "test/orchestrator_daemon_test.gleam",
    "crashing_command_ready_workflow_run_dependencies",
    "yaml agent crashed",
  ])
  let dir = "test/tmp/daemon-yaml-agent-command-crash"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: crashing_command_ready_workflow_run_dependencies(
        log_subject,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession(
        "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
        "after crash",
      ),
      1000,
    )
  assert result.status == command.NotFound
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_scheduled_due_tick_runs_command_workflow_test() {
  let dir = "test/tmp/daemon-scheduled-due"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_succeeded(records, run_id) },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_due(records, run_id)
  assert has_scheduled_pending(records, run_id)
  assert has_scheduled_started(records, run_id)
  assert has_scheduled_succeeded(records, run_id)
  assert has_step_success(records, "scheduled_command")
  let projected = load_test_projection(root)
  let assert Ok(status) =
    projection.scheduled_status_for(projected, "scheduled-job")
  assert status.state == projection.ScheduledTerminalSuccess
  assert status.last_success_run_id == Some(run_id)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_startup_clamps_legacy_persisted_due_after_clock_baseline_change_test() {
  let dir = "test/tmp/daemon-scheduled-restart-clock-baseline"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  append_succeeded_scheduled_run(
    root,
    ms("1951-09-27T03:15:00Z"),
    "schedule-scheduled-job-19510927T031500Z",
  )
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(ms("2026-05-05T12:00:10Z"))
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, ms("2026-05-05T12:00:11Z"))
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  let run_id = "schedule-scheduled-job-20260505T120011Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_succeeded(records, run_id) },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_due(records, run_id)
  assert has_scheduled_pending(records, run_id)
  assert has_scheduled_started(records, run_id)
  assert has_scheduled_succeeded(records, run_id)
  assert !has_scheduled_skip(records, "catch_up_disabled")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_overlap_records_skip_without_second_start_test() {
  let dir = "test/tmp/daemon-scheduled-overlap"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let barrier = test_async.new_barrier()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: blocking_command_workflow_run_dependencies(
        command_subject,
        barrier,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  let _ = test_async.drain_subject(command_subject)
  let _ = test_async.drain_subject(log_subject)

  set_clock(clock, 2000)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_event(log_subject, "tick_started", 20)
  test_async.assert_no_extra_message_within(command_subject, 100)
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_skip(records, "overlap_running") },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_skip(records, "overlap_running")

  test_async.release_barrier(barrier)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_startup_recovers_active_run_with_retry_test() {
  let dir = "test/tmp/daemon-scheduled-recover-active"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  append_started_scheduled_run(root, run_id)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(1500)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failed(records, run_id, "daemon_restart", False)
      && has_scheduled_retry_scheduled(records, run_id, 2)
    },
    20,
  )

  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_started_attempt(records, run_id, 2) },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_failure_reports_after_retry_exhaustion_test() {
  let dir = "test/tmp/daemon-scheduled-report"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      make_scheduled_failure_reporter: fn(_) {
        scheduled_reporter_success(report_subject)
      },
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 2) },
    20,
  )
  test_async.assert_no_extra_message_within(report_subject, 50)

  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 3) },
    20,
  )
  test_async.assert_no_extra_message_within(report_subject, 50)

  process.send(started.data, daemon.ScheduledRetryTick(run_id, 2))
  let assert Ok(request) = process.receive(report_subject, within: 1000)
  assert request.dedupe_key == "scheduled-job:scheduled-job"
  assert request.triage_state == "Triage"
  assert request.configured_labels == ["job:scheduled-job"]
  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failure_reported(records, run_id, "lin-scheduled")
    },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_retry_does_not_rerun_workflow_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      make_scheduled_failure_reporter: fn(_) {
        scheduled_reporter_directed(report_subject)
      },
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 2) },
    20,
  )
  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 3) },
    20,
  )
  process.send(started.data, daemon.ScheduledRetryTick(run_id, 2))
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 1000)
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_report_failed(records, run_id, 1) },
    20,
  )
  let _ = test_async.drain_subject(command_subject)

  process.send(started.data, daemon.ScheduledReportRetryTick(run_id, 1))
  let assert Ok(DirectedScheduledReportCall(second_request, second_reply)) =
    process.receive(report_subject, within: 1000)
  process.send(second_reply, ScheduledReportSuccess)
  assert second_request.run_id == run_id
  test_async.assert_no_extra_message_within(command_subject, 100)
  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failure_reported(records, run_id, "lin-scheduled")
    },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_retry_blocks_new_intervals_until_reported_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry-blocks-intervals"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      make_scheduled_failure_reporter: fn(_) {
        scheduled_reporter_directed(report_subject)
      },
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 2) },
    20,
  )
  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_retry_scheduled(records, run_id, 3) },
    20,
  )
  process.send(started.data, daemon.ScheduledRetryTick(run_id, 2))
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 1000)
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_report_failed(records, run_id, 1) },
    20,
  )
  let _ = test_async.drain_subject(command_subject)

  set_clock(clock, 2000)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_skip(records, "overlap_running") },
    20,
  )
  test_async.assert_no_extra_message_within(command_subject, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_startup_restores_retry_waiting_timer_test() {
  let dir = "test/tmp/daemon-scheduled-recover-retry"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  append_retry_waiting_scheduled_run(root, run_id)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(1500)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_started_attempt(records, run_id, 2) },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_yaml_poll_dispatches_command_workflow_test() {
  let dir = "test/tmp/daemon-yaml"
  let assert Ok(marker) = path.absolute(dir <> "/marker")
  let workflow_path = write_yaml_workflow(dir, marker)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "yaml_command:final_test", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert simplifile.is_file(marker) != Ok(True)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(
    session_ids,
    "workflow-step-ABC-1-42-1-final_test-a1-c55a07a40185",
  )
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.completed, "issue-id")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_command_failure_diagnostics_reach_events_and_report_test() {
  let dir = "test/tmp/daemon-command-failure-diagnostics"
  let workflow_path = write_real_failing_command_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let failure_report_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.default_dependencies(),
      make_handoff: fn(_, _) {
        handoff.Client(
          claim_issue: fn(_, _) { Ok(Nil) },
          report_success: fn(_, _, _) { Ok(Nil) },
          report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
          report_failure: fn(issue, failure, run_id) {
            process.send(
              failure_report_subject,
              handoff_format.failure_comment(issue, failure, run_id, []),
            )
            Ok(Nil)
          },
          report_failure_for_workflow: fn(issue, failure, run_id, _) {
            process.send(
              failure_report_subject,
              handoff_format.failure_comment(issue, failure, run_id, []),
            )
            Ok(Nil)
          },
          report_park: fn(_) { Ok(Nil) },
        )
      },
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(_) = wait_for_event_result(log_subject, "worker_exited", 20)
  let step_session_id = "workflow-step-ABC-1-42-1-final_test-a1-c55a07a40185"
  let assert Ok(page) =
    hub.events_after(event_hub, step_session_id, 0, 20, 1000)
  let assert Some(command_event) = find_event(page.events, "command_failed")
  assert command_event.payload.kind == event.Error
  let assert Some(message) = command_event.payload.message
  assert string.contains(message, "step=final_test")
  assert string.contains(message, "exit_code=9")
  assert string.contains(message, "stdout=")
  assert string.contains(message, "stderr=")
  assert string.contains(message, "[truncated]")
  let assert Some(tool_output) = command_event.payload.tool_output
  assert string.contains(tool_output, "full retained artifact:")
  assert string.contains(tool_output, "stdout_truncated: true")
  assert string.contains(tool_output, "stderr_truncated: true")

  let failure_comment =
    test_async.expect_message_within(failure_report_subject, 5000)
  assert string.contains(failure_comment, "workflow_step_failed")
  assert string.contains(failure_comment, "step=final_test")
  assert string.contains(failure_comment, "exit_code=9")
  assert string.contains(failure_comment, "artifact=")
  assert string.contains(failure_comment, "stdout=")
  assert string.contains(failure_comment, "stderr=")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_poll_dispatches_fake_worker_routes_update_and_shutdown_test() {
  let workflow_path = write_workflow("test/tmp/daemon-basic", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "tick_started", 10)
  assert wait_for_event(log_subject, "dispatch_started", 10)
  assert wait_for_event(log_subject, "pi_event", 10)
  assert wait_for_event(log_subject, "worker_exited", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_side_effect_crash_does_not_stall_future_polls_test() {
  use <- expected_crash.suppressing([
    "test/orchestrator_daemon_test.gleam",
    "daemon_side_effect_crash_does_not_stall_future_polls_test",
    "candidate fetch crashed",
  ])
  let workflow_path = write_workflow("test/tmp/daemon-side-effect-crash", 1)
  let fetch_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() {
        let reply = process.new_subject()
        process.send(fetch_subject, FetchRequest(reply))
        case process.receive(reply, within: 1000) {
          Ok(CrashFetch) -> panic as "candidate fetch crashed"
          Ok(ReturnCandidates(candidates)) -> Ok(candidates)
          Error(_) -> Error(error.LinearApiRequest("fetch directive timeout"))
        }
      },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(FetchRequest(first_reply)) =
    process.receive(fetch_subject, within: 1000)
  process.send(first_reply, CrashFetch)
  assert wait_for_event(log_subject, "side_effect_crashed", 20)

  process.send(started.data, daemon.PollTick(2))
  let assert Ok(FetchRequest(second_reply)) =
    process.receive(fetch_subject, within: 1000)
  process.send(second_reply, ReturnCandidates([]))

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_retry_refresh_done_issue_releases_claim_without_rescheduling_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry-terminal", 1)
  let retried = issue("retry-id", "ABC-2", "Todo")
  let done =
    tracker_issue.Issue(
      ..retried,
      state: issue_state.from_string_unchecked("Done"),
    )
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([retried]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          let _ = issue
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("boom")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, retried)
  assert wait_for_event(log_subject, "retry_scheduled", 20)

  process.send(started.data, daemon.RetryTick("retry-id", 1))
  let assert Ok(retry_refresh) = process.receive(refresh_subject, within: 1000)
  process.send(retry_refresh, done)
  assert wait_for_event(log_subject, "claim_released", 20)
  process.send(started.data, daemon.RetryTick("retry-id", 2))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_retry_timer_requeues_failed_worker_once_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry", 1)
  let first = issue("retry-id", "ABC-2", "Todo")
  let second = tracker_issue.Issue(..first, title: "retry succeeds")
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          process.send(log_subject, "agent_run")
          case issue.title == "retry succeeds" {
            False ->
              Error(agent_types.WorkerFailure(
                reason: error.PiFailed(error.PiProtocolError("boom")),
                workspace_path: Some(context.workspace_path),
                tokens: session_tokens.zero_token_totals(),
                final_issue: None,
              ))
            True ->
              Ok(success(
                tracker_issue.Issue(
                  ..issue,
                  state: issue_state.from_string_unchecked("Done"),
                ),
                context.workspace_path,
              ))
          }
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, first)
  assert wait_for_event(log_subject, "retry_scheduled", 20)

  process.send(started.data, daemon.RetryTick("retry-id", 99))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  let assert Ok(retry_refresh) = process.receive(refresh_subject, within: 1000)
  process.send(retry_refresh, second)
  assert wait_for_event(log_subject, "worker_exited", 20)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String

pub fn daemon_startup_resumes_matching_workflow_checkpoint_test() {
  let dir = "test/tmp/daemon-startup-resume-workflow"
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(workspace_root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) =
    simplifile.write(config_path, workflow_text(workspace_root, 1))
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: first
    kind: command
    run: first
    workspace: main
  - id: second
    kind: command
    depends_on: [first]
    run: second
    workspace: main
",
    )
  let candidate = issue("issue-resume", "ABC-99", "Todo")
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, dag)) = runtime_bundle.select_workflow(bundle, candidate)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
  let store = artifact_store.new(workspace_root)
  let completed_artifact =
    step_artifact.from_command_result(
      "first",
      0,
      "already done",
      "",
      False,
      [],
      bundle.orchestrator.artifact_limits,
    )
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-recover",
      "implementation",
      "first",
      1,
      completed_artifact,
    )
  let run_root = workspace_root <> "/implementation/ABC-99/run-recover"
  let first_workspace = run_root <> "/main"
  let assert Ok(Nil) = simplifile.create_directory_all(first_workspace)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1,
          record.WorkflowRunStarted(
            "run-recover",
            "implementation",
            fingerprint,
            candidate.id,
            candidate.identifier,
            core.issue_fingerprint(candidate),
            0,
            run_root,
          ),
        ),
        record.with_id(
          "first-prepared",
          2,
          record.StepAttemptPrepared(
            "run-recover",
            "implementation",
            "first",
            1,
            "main",
            first_workspace,
            run_root,
            None,
            None,
          ),
        ),
        record.with_id(
          "first-started",
          3,
          record.StepAttemptStarted(
            "run-recover",
            "implementation",
            "first",
            1,
            "workflow-step-run-recover-first-a1-a7937b64b8ca",
            None,
            False,
          ),
        ),
        record.with_id(
          "first-finished",
          4,
          record.StepAttemptFinished(
            "run-recover",
            "implementation",
            "first",
            1,
            "completed",
            stored.ref,
            stored.sha256,
            "main",
            first_workspace,
            0,
            0,
          ),
        ),
      ],
      True,
    )
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let assert Ok(started) = daemon.start(Some(config_path), deps)

  assert wait_for_event_without_event(
    log_subject,
    "yaml_command:second",
    "yaml_command:first",
    10,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}
