import birl
import gleam/erlang/process
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/runner
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

fn tracker_config() -> domain.TrackerConfig {
  domain.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    active_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn handoff_config() -> domain.HandoffConfig {
  domain.HandoffConfig(
    enabled: True,
    comment_on_claim: True,
    comment_on_success: True,
    comment_on_failure: True,
    claim_state_id: Some("claim-state"),
    success_state_id: None,
    failure_state_id: None,
    include_result_on_success: True,
    result_max_chars: 8000,
  )
}

fn issue() -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-1",
    title: "Title",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn success() -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(
      domain.Issue(..issue(), state: issue_state.from_string_unchecked("Done")),
    ),
    final_classification: runner.FinalTerminal,
    workspace_path: "workspace",
    tokens: domain.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 0,
      cache_write: 0,
      total: 3,
    ),
    turns: 1,
    result: domain.ResultArtifact(
      final_response: Some("Implemented secret-key"),
      truncated: False,
      source: "agent_end_messages",
    ),
  )
}

pub fn comments_only_and_state_handoff_builds_expected_mutations_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    case string.contains(request.body, "issueUpdate") {
      True ->
        Ok(linear.Response(
          status: 200,
          body: "{\"data\":{\"issueUpdate\":{\"success\":true}}}",
        ))
      False ->
        Ok(linear.Response(
          status: 200,
          body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
        ))
    }
  }
  let client =
    handoff.linear_client(tracker_config(), handoff_config(), transport)

  assert client.claim_issue(issue(), "run-1") == Ok(Nil)
  let assert Ok(claim_comment) = process.receive(subject, within: 100)
  let assert Ok(claim_state) = process.receive(subject, within: 100)
  assert string.contains(claim_comment, "ABC-1")
  assert string.contains(claim_comment, "run-1")
  assert string.contains(claim_state, "claim-state")

  assert client.report_success(issue(), success(), "run-2") == Ok(Nil)
  let assert Ok(success_comment) = process.receive(subject, within: 100)
  assert string.contains(success_comment, "run-2")
  assert string.contains(success_comment, "Result:")
  assert string.contains(success_comment, "Implemented [REDACTED]")
  assert string.contains(success_comment, "classification: terminal")
  assert string.contains(
    success_comment,
    "tokens: input=1 output=2 cache_read=0 cache_write=0 total=3",
  )
  assert !string.contains(success_comment, "secret-key")

  let failure =
    runner.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("secret details")),
      workspace_path: None,
      tokens: domain.zero_token_totals(),
      final_issue: None,
    )
  assert client.report_failure(issue(), failure, "run-3") == Ok(Nil)
  let assert Ok(failure_comment) = process.receive(subject, within: 100)
  assert string.contains(failure_comment, "run-3")
  assert string.contains(failure_comment, "agent_pi_failed")
  assert !string.contains(failure_comment, "secret details")
}

pub fn success_handoff_posts_single_structured_result_comment_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(
      status: 200,
      body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
    ))
  }
  let no_state =
    domain.HandoffConfig(
      ..handoff_config(),
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
    )
  let client = handoff.linear_client(tracker_config(), no_state, transport)

  assert client.report_success(issue(), success(), "run-structured") == Ok(Nil)
  let assert Ok(success_comment) = process.receive(subject, within: 100)
  assert process.receive(subject, within: 20) == Error(Nil)
  assert string.contains(success_comment, "commentCreate")
  assert string.contains(success_comment, "run-structured")
  assert string.contains(success_comment, "Result:")
  assert string.contains(success_comment, "Implemented [REDACTED]")
}

pub fn disabled_handoff_performs_no_transport_calls_test() {
  let subject = process.new_subject()
  let transport = fn(request: linear.Request) {
    process.send(subject, request.body)
    Ok(linear.Response(status: 500, body: "{}"))
  }
  let disabled = domain.HandoffConfig(..handoff_config(), enabled: False)
  let client = handoff.linear_client(tracker_config(), disabled, transport)

  assert client.claim_issue(issue(), "run-1") == Ok(Nil)
  assert client.report_success(issue(), success(), "run-2") == Ok(Nil)
  assert process.receive(subject, within: 20) == Error(Nil)
}
