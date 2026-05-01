import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/step_artifact
import scherzo/tracker
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_run
import scherzo/workspace_run

type CommandStart {
  CommandStart(step_id: String, release: process.Subject(String))
}

fn issue() -> domain.Issue {
  domain.Issue(
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
    created_at: None,
    updated_at: None,
  )
}

fn effective() -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    tracker: domain.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "https://api.linear.app/graphql",
      api_key: Some("test-key"),
      project_slug: Some("TEST"),
      active_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings(["Done"]),
    ),
    polling: config.default_polling_config(),
    workspace: domain.WorkspaceConfig(root: "test/tmp/workflow-run/workspaces"),
    hooks: config.default_hooks_config(),
    agent: domain.AgentConfig(..config.default_agent_config(), max_turns: 1),
    pi: domain.PiConfig(
      ..config.default_pi_config(),
      compatibility_probe: False,
    ),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
  )
}

fn orchestrator() -> domain.OrchestratorConfig {
  domain.OrchestratorConfig(
    effective: effective(),
    config_dir: "test/tmp/workflow-run",
    routing: domain.RoutingConfig(
      workflow_label_prefix: "workflow:",
      require_exactly_one_workflow_label: True,
      default_workflow: None,
      workflows: dict.from_list([#("implementation", "implementation.yaml")]),
    ),
    dag_hooks: domain.DagHooksConfig(
      create: None,
      before_step: None,
      after_step: None,
      remove: None,
      timeout_ms: 1000,
    ),
    artifact_limits: domain.ArtifactLimits(
      command_stream_max_chars: 1000,
      template_field_max_chars: 1000,
      workflow_summary_max_chars: 4000,
    ),
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn success_agent(prompt: String) -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(issue()),
    final_classification: runner.FinalTerminal,
    workspace_path: "workspace",
    tokens: domain.zero_token_totals(),
    turns: 1,
    result: domain.ResultArtifact(
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
      workspace_ref,
      _orchestrator,
      known,
    ) {
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
        "prepare:" <> step_id <> ":" <> workspace_ref.name <> ":" <> source,
      )
      Ok(
        workspace_run.PreparedStepWorkspace(
          workflow_id: workflow_id,
          run_id: run_id,
          run_root: "test/tmp/workflow-run/workspaces/implementation/ABC-123",
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
        ),
      )
    },
    after_step: fn(_issue, step_id, _prepared, _orchestrator) {
      process.send(subject, "after:" <> step_id)
    },
    cleanup_run: fn(run_root, _orchestrator) {
      process.send(subject, "cleanup:" <> run_root)
      Ok(Nil)
    },
    command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
      process.send(subject, "run:" <> step_id)
      let exit_code = case failing_command == Some(step_id) {
        True -> 1
        False -> 0
      }
      step_artifact.from_command_result(
        step_id,
        exit_code,
        "stdout:" <> step_id,
        "stderr:" <> step_id,
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      _issue,
      _step_id,
      prompt,
      _effective,
      _tracker,
      workspace_path,
      _emit_update,
      _command_ready,
    ) {
      process.send(subject, "agent:" <> workspace_path <> ":" <> prompt)
      Ok(success_agent(prompt))
    },
  )
}

fn implementation_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: implementation\nmax_parallel_steps: 3\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement prompt\n    workspace: main\n  - id: test_after_implement\n    kind: command\n    depends_on: [implement]\n    run: test command\n    workspace: main\n    on_failure: continue\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: code review prompt\n    workspace:\n      name: code-review\n      from: main\n  - id: apply_feedback\n    kind: agent\n    depends_on: [test_after_implement, code_review]\n    prompt: apply {{ steps.code_review.final_response }} {{ steps.test_after_implement.exit_code }}\n    workspace: main\n",
    )
  dag
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
  assert process.receive(subject, within: 50) == Error(Nil)
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
      command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
        let release_subject = process.new_subject()
        process.send(command_subject, CommandStart(step_id, release_subject))
        case step_id == "final" {
          True -> Nil
          False -> {
            let _ = process.receive_forever(release_subject)
            Nil
          }
        }
        step_artifact.from_command_result(
          step_id,
          0,
          "stdout:" <> step_id,
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
      command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
        let release_subject = process.new_subject()
        process.send(command_subject, CommandStart(step_id, release_subject))
        let _ = process.receive_forever(release_subject)
        let exit_code = case step_id == "fail" {
          True -> 1
          False -> 0
        }
        step_artifact.from_command_result(
          step_id,
          exit_code,
          "stdout:" <> step_id,
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
  assert process.receive(subject, within: 50) == Error(Nil)
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
      command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
        process.send(subject, "run:" <> step_id)
        case step_id == "first" {
          True -> {
            let release_subject = process.new_subject()
            process.send(
              command_subject,
              CommandStart(step_id, release_subject),
            )
            let _ = process.receive_forever(release_subject)
            Nil
          }
          False -> Nil
        }
        step_artifact.from_command_result(
          step_id,
          0,
          "stdout:" <> step_id,
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
        step_id,
        _command,
        _workspace,
        _timeout,
        _secrets,
        _limits,
      ) {
        process.send(subject, "run:" <> step_id)
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
  assert process.receive(subject, within: 50) == Error(Nil)
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
  assert process.receive(subject, within: 50) == Error(Nil)
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
      workspace_ref,
      orchestrator,
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
            workspace_ref,
            orchestrator,
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
