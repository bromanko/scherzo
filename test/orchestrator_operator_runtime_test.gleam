import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/orchestrator/effects/types as transition_effects
import scherzo/orchestrator/operator_runtime
import scherzo/orchestrator/transition_types
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn operator_issue_resolution_resolves_retry_issue_refs_test() {
  let issue = issue("issue-1", "LIV-724", "Todo")
  let lookup =
    operator_runtime.lookup(
      issue_for_ref: fn(_) { Ok(issue) },
      parked_issue_id_for_ref: fn(_) { Error(command.NotFound) },
    )

  let result =
    operator_runtime.operator_issue_resolution(
      lookup,
      command.RetryIssue(command.IssueIdentifier("LIV-724")),
    )

  assert result == transition_types.OperatorIssueResolved(issue)
}

pub fn parked_issue_resolution_rejects_ambiguous_identifier_test() {
  let lookup =
    operator_runtime.lookup(
      issue_for_ref: fn(_) { Error(command.NotFound) },
      parked_issue_id_for_ref: fn(_) {
        Error(command.Rejected("ambiguous_issue_identifier"))
      },
    )

  let result =
    operator_runtime.parked_issue_resolution(
      lookup,
      command.UnparkIssue(command.IssueIdentifier("LIV-724")),
    )

  assert result
    == transition_types.ParkedIssueRejected("ambiguous_issue_identifier")
}

pub fn apply_shell_operator_command_routes_prompt_session_test() {
  let worker_subject = process.new_subject()
  let request =
    transition_effects.OperatorCommandRequest(
      source: transition_effects.LocalOperatorCommand,
      operator_command: command.PromptSession("session-1", "hello"),
      timeout_ms: 1000,
    )
  let handlers =
    operator_runtime.shell_handlers(
      reload_workflow_for_operator: fn(state, _) {
        #(state, command.applied(command.ReloadWorkflow, None))
      },
      retry_workflow_step_for_operator: fn(state, _, _, _) {
        #(state, command.applied(command.ReloadWorkflow, None))
      },
      schedule_run_now_for_operator: fn(state, _, _) {
        #(state, command.applied(command.ReloadWorkflow, None))
      },
      abort_session_for_operator_sync: fn(state, _, _, _) {
        #(state, command.applied(command.ReloadWorkflow, None))
      },
      route_worker_command_sync: fn(state, _, _, _, send) {
        let reply = process.new_subject()
        send(worker_subject, reply)
        #(state + 1, command.applied(command.PauseDispatch, Some("routed")))
      },
      cleanup_orphan_steps_for_operator: fn(state, _, _, _) {
        #(state, command.applied(command.ReloadWorkflow, None))
      },
    )

  let #(state, result) =
    operator_runtime.apply_shell_operator_command(0, request, handlers)

  assert state == 1
  assert result.message == Some("routed")
  let assert Ok(worker_command.QueuePrompt(message: message, ..)) =
    process.receive(worker_subject, within: 1000)
  assert message == "hello"
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}
