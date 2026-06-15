import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/linear
import scherzo/scheduled_failure_reporter as reporter
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import test_async

type Call {
  Call(String)
}

fn tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.test/graphql",
    api_key: Some("secret-key"),
    project_slug: Some("PROJ"),
    task_scope: None,
    active_states: issue_state.list_from_strings(["Ready for Agent"]),
    dispatch_states: issue_state.list_from_strings(["Ready for Agent"]),
    terminal_states: issue_state.list_from_strings(["Done"]),
  )
}

fn multi_project_tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    ..tracker_config(),
    project_slug: None,
    task_scope: Some(config_types.LinearTaskProjects(["PROJ", "BUGS"])),
  )
}

fn composed_project_tracker_config() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    ..tracker_config(),
    project_slug: None,
    task_scope: Some(
      config_types.LinearTaskOr([
        config_types.LinearTaskProject("PROJ"),
        config_types.LinearTaskAnd([
          config_types.LinearTaskProjects(["BUGS", "OPS"]),
          config_types.LinearTaskProject("BUGS"),
        ]),
      ]),
    ),
  )
}

fn composed_project_task_filter_body_fragment() -> String {
  "\"taskFilter\":{\"or\":[{\"project\":{\"slugId\":{\"eq\":\"PROJ\"}}},{\"and\":[{\"project\":{\"slugId\":{\"in\":[\"BUGS\",\"OPS\"]}}},{\"project\":{\"slugId\":{\"eq\":\"BUGS\"}}}]}]}"
}

fn base_request() -> reporter.FailureReportRequest {
  reporter.FailureReportRequest(
    job_id: "pr-conflict-repair",
    workflow_id: "pr-conflict-repair",
    due_at_ms: 1_746_447_200_000,
    run_id: "schedule-pr-conflict-repair-20250505T120000Z",
    attempt: 5,
    max_attempts: 5,
    reason: "workflow_command_failed:inspect: command exited 1",
    run_root: Some(
      "<workspace-root>/pr-conflict-repair/scheduled/pr-conflict-repair/run",
    ),
    session_id: Some("scheduled-session"),
    dedupe_key: reporter.dedupe_key("pr-conflict-repair"),
    triage_state: "Triage",
    configured_labels: ["job:pr-conflict-repair"],
    previous_issue_id: None,
  )
}

fn receive_call(subject: process.Subject(Call)) -> String {
  let assert Ok(Call(call)) = process.receive(subject, within: 1000)
  call
}

fn receive_call_containing(
  subject: process.Subject(Call),
  needle: String,
  remaining: Int,
) -> String {
  case remaining <= 0 {
    True -> panic as "expected a matching Linear request body"
    False -> {
      let call = receive_call(subject)
      case string.contains(call, needle) {
        True -> call
        False -> receive_call_containing(subject, needle, remaining - 1)
      }
    }
  }
}

pub fn scheduled_failure_reporter_creates_issue_with_reserved_labels_and_marker_test() {
  let calls = process.new_subject()
  let backend =
    reporter.Backend(
      ensure_label: fn(name) {
        process.send(calls, Call("label:" <> name))
        Ok("id:" <> name)
      },
      find_open_issue_by_id: fn(issue_id) {
        process.send(calls, Call("lookup:" <> issue_id))
        Ok(None)
      },
      find_open_issues_by_labels: fn(labels) {
        process.send(calls, Call("find:" <> string.join(labels, with: ",")))
        Ok([])
      },
      create_issue: fn(title, body, state, label_ids) {
        assert title == "Scherzo scheduled job failed: pr-conflict-repair"
        assert state == "Triage"
        assert list.contains(label_ids, "id:scherzo:scheduled")
        assert list.contains(
          label_ids,
          "id:scherzo:scheduled-job:pr-conflict-repair",
        )
        assert list.contains(label_ids, "id:job:pr-conflict-repair")
        assert string.contains(
          body,
          "<!-- scherzo-dedupe: scheduled-job:pr-conflict-repair -->",
        )
        assert string.contains(body, "Scheduled job: pr-conflict-repair")
        assert string.contains(
          body,
          "scherzoctl schedules logs pr-conflict-repair --last",
        )
        process.send(calls, Call("create"))
        Ok("lin-1")
      },
      comment_issue: fn(_, _) {
        process.send(calls, Call("comment"))
        Ok(Nil)
      },
      move_issue_to_state: fn(_, _) {
        process.send(calls, Call("move"))
        Ok(Nil)
      },
    )

  assert reporter.report_with_backend(base_request(), backend)
    == Ok(reporter.FailureReportCreated("lin-1"))
  assert receive_call(calls) == "label:scherzo:scheduled"
  assert receive_call(calls) == "label:scherzo:scheduled-job:pr-conflict-repair"
  assert receive_call(calls) == "label:job:pr-conflict-repair"
  assert receive_call(calls)
    == "find:scherzo:scheduled,scherzo:scheduled-job:pr-conflict-repair"
  assert receive_call(calls) == "create"
}

pub fn scheduled_failure_reporter_updates_remembered_open_issue_test() {
  let calls = process.new_subject()
  let request =
    reporter.FailureReportRequest(
      ..base_request(),
      previous_issue_id: Some("lin-existing"),
    )
  let backend =
    reporter.Backend(
      ensure_label: fn(name) { Ok("id:" <> name) },
      find_open_issue_by_id: fn(issue_id) {
        assert issue_id == "lin-existing"
        process.send(calls, Call("lookup"))
        Ok(
          Some(reporter.ExistingFailureIssue(
            id: "lin-existing",
            updated_at_ms: 1,
          )),
        )
      },
      find_open_issues_by_labels: fn(_) {
        process.send(calls, Call("find"))
        Ok([])
      },
      create_issue: fn(_, _, _, _) {
        process.send(calls, Call("create"))
        Ok("created")
      },
      comment_issue: fn(issue_id, body) {
        assert issue_id == "lin-existing"
        assert string.contains(body, "Current failure issue ID: lin-existing")
        process.send(calls, Call("comment"))
        Ok(Nil)
      },
      move_issue_to_state: fn(issue_id, state) {
        assert issue_id == "lin-existing"
        assert state == "Triage"
        process.send(calls, Call("move"))
        Ok(Nil)
      },
    )

  assert reporter.report_with_backend(request, backend)
    == Ok(reporter.FailureReportUpdated("lin-existing"))
  assert receive_call(calls) == "lookup"
  assert receive_call(calls) == "comment"
  assert receive_call(calls) == "move"
}

pub fn scheduled_failure_reporter_falls_back_when_remembered_issue_is_not_open_test() {
  let calls = process.new_subject()
  let request =
    reporter.FailureReportRequest(
      ..base_request(),
      previous_issue_id: Some("lin-closed"),
    )
  let backend =
    reporter.Backend(
      ensure_label: fn(name) { Ok("id:" <> name) },
      find_open_issue_by_id: fn(issue_id) {
        assert issue_id == "lin-closed"
        process.send(calls, Call("lookup"))
        Ok(None)
      },
      find_open_issues_by_labels: fn(labels) {
        assert labels
          == ["scherzo:scheduled", "scherzo:scheduled-job:pr-conflict-repair"]
        process.send(calls, Call("find"))
        Ok([reporter.ExistingFailureIssue(id: "lin-by-label", updated_at_ms: 2)])
      },
      create_issue: fn(_, _, _, _) {
        process.send(calls, Call("create"))
        Ok("created")
      },
      comment_issue: fn(issue_id, body) {
        assert issue_id == "lin-by-label"
        assert string.contains(body, "Current failure issue ID: lin-by-label")
        process.send(calls, Call("comment"))
        Ok(Nil)
      },
      move_issue_to_state: fn(issue_id, _) {
        assert issue_id == "lin-by-label"
        process.send(calls, Call("move"))
        Ok(Nil)
      },
    )

  assert reporter.report_with_backend(request, backend)
    == Ok(reporter.FailureReportUpdated("lin-by-label"))
  assert receive_call(calls) == "lookup"
  assert receive_call(calls) == "find"
  assert receive_call(calls) == "comment"
  assert receive_call(calls) == "move"
}

pub fn scheduled_failure_reporter_dedupes_by_reserved_labels_test() {
  let calls = process.new_subject()
  let backend =
    reporter.Backend(
      ensure_label: fn(name) { Ok("id:" <> name) },
      find_open_issue_by_id: fn(_) { Ok(None) },
      find_open_issues_by_labels: fn(labels) {
        assert labels
          == ["scherzo:scheduled", "scherzo:scheduled-job:pr-conflict-repair"]
        Ok([
          reporter.ExistingFailureIssue(id: "lin-older", updated_at_ms: 1),
          reporter.ExistingFailureIssue(id: "lin-newer", updated_at_ms: 2),
        ])
      },
      create_issue: fn(_, _, _, _) {
        process.send(calls, Call("create"))
        Ok("created")
      },
      comment_issue: fn(issue_id, body) {
        assert issue_id == "lin-newer"
        assert string.contains(body, "multiple open Linear issues")
        process.send(calls, Call("comment"))
        Ok(Nil)
      },
      move_issue_to_state: fn(issue_id, _) {
        assert issue_id == "lin-newer"
        process.send(calls, Call("move"))
        Ok(Nil)
      },
    )

  assert reporter.report_with_backend(base_request(), backend)
    == Ok(reporter.FailureReportUpdated("lin-newer"))
  assert receive_call(calls) == "comment"
  assert receive_call(calls) == "move"
}

pub fn linear_scheduled_failure_dedupes_one_visible_issue_per_job_test() {
  let calls = process.new_subject()
  let first_request =
    reporter.FailureReportRequest(
      ..base_request(),
      job_id: "nightly",
      workflow_id: "nightly",
      run_id: "schedule-nightly-1",
      dedupe_key: reporter.dedupe_key("nightly"),
      configured_labels: ["job:nightly"],
      previous_issue_id: None,
    )
  let second_request =
    reporter.FailureReportRequest(..first_request, run_id: "schedule-nightly-2")
  let first_backend =
    reporter.Backend(
      ensure_label: fn(name) { Ok("id:" <> name) },
      find_open_issue_by_id: fn(_) { Ok(None) },
      find_open_issues_by_labels: fn(labels) {
        assert labels == ["scherzo:scheduled", "scherzo:scheduled-job:nightly"]
        process.send(calls, Call("find:first"))
        Ok([])
      },
      create_issue: fn(title, body, state, label_ids) {
        assert title == "Scherzo scheduled job failed: nightly"
        assert state == "Triage"
        assert list.contains(label_ids, "id:scherzo:scheduled")
        assert list.contains(label_ids, "id:scherzo:scheduled-job:nightly")
        assert list.contains(label_ids, "id:job:nightly")
        assert string.contains(
          body,
          "<!-- scherzo-dedupe: scheduled-job:nightly -->",
        )
        process.send(calls, Call("create:lin-nightly"))
        Ok("lin-nightly")
      },
      comment_issue: fn(_, _) {
        process.send(calls, Call("comment:first"))
        Ok(Nil)
      },
      move_issue_to_state: fn(_, _) {
        process.send(calls, Call("move:first"))
        Ok(Nil)
      },
    )
  let second_backend =
    reporter.Backend(
      ensure_label: fn(name) { Ok("id:" <> name) },
      find_open_issue_by_id: fn(_) { Ok(None) },
      find_open_issues_by_labels: fn(labels) {
        assert labels == ["scherzo:scheduled", "scherzo:scheduled-job:nightly"]
        process.send(calls, Call("find:second"))
        Ok([reporter.ExistingFailureIssue(id: "lin-nightly", updated_at_ms: 2)])
      },
      create_issue: fn(_, _, _, _) {
        process.send(calls, Call("create:second"))
        Ok("lin-duplicate")
      },
      comment_issue: fn(issue_id, body) {
        assert issue_id == "lin-nightly"
        assert string.contains(body, "Current failure issue ID: lin-nightly")
        assert string.contains(body, "Run ID: schedule-nightly-2")
        process.send(calls, Call("comment:lin-nightly"))
        Ok(Nil)
      },
      move_issue_to_state: fn(issue_id, state) {
        assert issue_id == "lin-nightly"
        assert state == "Triage"
        process.send(calls, Call("move:lin-nightly"))
        Ok(Nil)
      },
    )

  assert reporter.report_with_backend(first_request, first_backend)
    == Ok(reporter.FailureReportCreated("lin-nightly"))
  assert receive_call(calls) == "find:first"
  assert receive_call(calls) == "create:lin-nightly"

  assert reporter.report_with_backend(second_request, second_backend)
    == Ok(reporter.FailureReportUpdated("lin-nightly"))
  assert receive_call(calls) == "find:second"
  assert receive_call(calls) == "comment:lin-nightly"
  assert receive_call(calls) == "move:lin-nightly"
  test_async.assert_no_extra_message_within(calls, 50)
}

pub fn scheduled_failure_reporter_real_search_uses_open_reserved_label_filters_test() {
  let calls = process.new_subject()
  let client =
    reporter.real_client_with_transport(
      tracker_config(),
      real_search_transport(calls),
    )

  assert client.report_failure(base_request())
    == Ok(reporter.FailureReportUpdated("lin-open"))

  let search_body =
    receive_call_containing(calls, "ScherzoScheduledFailureIssues", 10)
  assert string.contains(search_body, "IssueFilter")
  assert string.contains(search_body, "state: { type: { nin:")
  assert string.contains(search_body, "completed")
  assert string.contains(search_body, "canceled")
  assert string.contains(search_body, "duplicate")
  assert string.contains(
    search_body,
    "\"labelFilters\":[{\"labels\":{\"name\":{\"eq\":\"scherzo:scheduled\"}}},{\"labels\":{\"name\":{\"eq\":\"scherzo:scheduled-job:pr-conflict-repair\"}}}]",
  )
  assert !string.contains(search_body, "every")

  let comment_body = receive_call_containing(calls, "commentCreate", 10)
  assert string.contains(comment_body, "lin-open")
  assert !string.contains(comment_body, "lin-closed")
}

pub fn scheduled_failure_reporter_real_search_uses_multi_project_task_filter_test() {
  let calls = process.new_subject()
  let client =
    reporter.real_client_with_transport(
      multi_project_tracker_config(),
      real_search_transport_with_contract(
        calls,
        multi_project_contract_response(),
      ),
    )

  assert client.report_failure(base_request())
    == Ok(reporter.FailureReportUpdated("lin-open"))

  let search_body =
    receive_call_containing(calls, "ScherzoScheduledFailureIssues", 10)
  assert string.contains(
    search_body,
    "and: [$taskFilter, { and: $labelFilters }]",
  )
  assert string.contains(
    search_body,
    "\"taskFilter\":{\"project\":{\"slugId\":{\"in\":[\"PROJ\",\"BUGS\"]}}}",
  )
}

pub fn scheduled_failure_reporter_real_search_uses_composed_task_filter_test() {
  let calls = process.new_subject()
  let client =
    reporter.real_client_with_transport(
      composed_project_tracker_config(),
      real_search_transport_with_contract(
        calls,
        composed_project_contract_response(),
      ),
    )

  assert client.report_failure(base_request())
    == Ok(reporter.FailureReportUpdated("lin-open"))

  let search_body =
    receive_call_containing(calls, "ScherzoScheduledFailureIssues", 10)
  assert string.contains(
    search_body,
    composed_project_task_filter_body_fragment(),
  )
  assert string.contains(
    search_body,
    "and: [$taskFilter, { and: $labelFilters }]",
  )
}

pub fn scheduled_failure_reporter_multi_project_create_uses_single_project_id_test() {
  let calls = process.new_subject()
  let client =
    reporter.real_client_with_transport(
      multi_project_tracker_config(),
      real_create_transport_with_contract(
        calls,
        multi_project_contract_response(),
      ),
    )

  assert client.report_failure(base_request())
    == Ok(reporter.FailureReportCreated("lin-created"))

  let create_body =
    receive_call_containing(calls, "ScherzoScheduledFailureIssueCreate", 20)
  assert string.contains(create_body, "\"projectId\":\"project-PROJ\"")
  assert string.contains(create_body, "\"teamId\":\"team-PROJ\"")
  assert !string.contains(create_body, "project-PROJ,project-BUGS")
}

pub fn scheduled_failure_reporter_label_failure_stops_before_create_test() {
  let calls = process.new_subject()
  let backend =
    reporter.Backend(
      ensure_label: fn(_) { Error(error.LinearApiRequest("label unavailable")) },
      find_open_issue_by_id: fn(_) {
        process.send(calls, Call("lookup"))
        Ok(None)
      },
      find_open_issues_by_labels: fn(_) {
        process.send(calls, Call("find"))
        Ok([])
      },
      create_issue: fn(_, _, _, _) {
        process.send(calls, Call("create"))
        Ok("created")
      },
      comment_issue: fn(_, _) { Ok(Nil) },
      move_issue_to_state: fn(_, _) { Ok(Nil) },
    )

  assert reporter.report_with_backend(base_request(), backend)
    == Error(error.LinearApiRequest("label unavailable"))
  test_async.assert_no_extra_message_within(calls, 50)
}

pub fn scheduled_failure_reporter_disabled_client_is_noop_test() {
  let client = reporter.disabled_client()
  assert client.report_failure(base_request()) == Ok(reporter.FailureReportNoop)
}

fn real_search_transport(
  observed: process.Subject(Call),
) -> fn(linear.Request) -> Result(linear.Response, error.TrackerError) {
  real_search_transport_with_contract(observed, contract_response())
}

fn real_search_transport_with_contract(
  observed: process.Subject(Call),
  contract_body: String,
) -> fn(linear.Request) -> Result(linear.Response, error.TrackerError) {
  fn(request: linear.Request) {
    process.send(observed, Call(request.body))
    case string.contains(request.body, "ScherzoLinearContract") {
      True -> Ok(linear.Response(status: 200, body: contract_body))
      False ->
        case string.contains(request.body, "ScherzoScheduledFailureIssues") {
          True ->
            Ok(linear.Response(status: 200, body: issue_search_response()))
          False ->
            case string.contains(request.body, "commentCreate") {
              True ->
                Ok(linear.Response(
                  status: 200,
                  body: "{\"data\":{\"commentCreate\":{\"success\":true}}}",
                ))
              False ->
                case string.contains(request.body, "issueUpdate") {
                  True ->
                    Ok(linear.Response(
                      status: 200,
                      body: "{\"data\":{\"issueUpdate\":{\"success\":true}}}",
                    ))
                  False -> Error(error.LinearApiRequest("unexpected request"))
                }
            }
        }
    }
  }
}

fn real_create_transport_with_contract(
  observed: process.Subject(Call),
  contract_body: String,
) -> fn(linear.Request) -> Result(linear.Response, error.TrackerError) {
  fn(request: linear.Request) {
    process.send(observed, Call(request.body))
    case string.contains(request.body, "ScherzoLinearContract") {
      True -> Ok(linear.Response(status: 200, body: contract_body))
      False ->
        case string.contains(request.body, "ScherzoScheduledFailureIssues") {
          True ->
            Ok(linear.Response(status: 200, body: empty_issue_search_response()))
          False ->
            case
              string.contains(
                request.body,
                "ScherzoScheduledFailureIssueCreate",
              )
            {
              True ->
                Ok(linear.Response(
                  status: 200,
                  body: "{\"data\":{\"issueCreate\":{\"success\":true,\"issue\":{\"id\":\"lin-created\",\"identifier\":\"LIV-1\",\"url\":\"https://linear/issue/LIV-1\"}}}}",
                ))
              False -> Error(error.LinearApiRequest("unexpected request"))
            }
        }
    }
  }
}

fn contract_response() -> String {
  "{\"data\":{\"projects\":{\"nodes\":["
  <> "{\"id\":\"project-id\",\"name\":\"Project\",\"slugId\":\"PROJ\","
  <> "\"teams\":{\"nodes\":[{\"id\":\"team-id\",\"key\":\"ENG\","
  <> "\"name\":\"Engineering\",\"states\":{\"nodes\":["
  <> "{\"id\":\"state-triage\",\"name\":\"Triage\",\"type\":\"triage\"}],"
  <> "\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},"
  <> "\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,"
  <> "\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,"
  <> "\"endCursor\":null}}}"
  <> "]},\"issueLabels\":{\"nodes\":["
  <> "{\"id\":\"label-scheduled\",\"name\":\"scherzo:scheduled\"},"
  <> "{\"id\":\"label-job\",\"name\":\"scherzo:scheduled-job:pr-conflict-repair\"},"
  <> "{\"id\":\"label-extra\",\"name\":\"job:pr-conflict-repair\"}],"
  <> "\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn multi_project_contract_response() -> String {
  contract_response_for_projects(["PROJ", "BUGS"])
}

fn composed_project_contract_response() -> String {
  contract_response_for_projects(["PROJ", "BUGS", "OPS"])
}

fn contract_response_for_projects(slugs: List(String)) -> String {
  "{\"data\":{\"projects\":{\"nodes\":["
  <> string.join(list.map(slugs, contract_project_node), with: ",")
  <> "]},\"issueLabels\":{\"nodes\":["
  <> "{\"id\":\"label-scheduled\",\"name\":\"scherzo:scheduled\"},"
  <> "{\"id\":\"label-job\",\"name\":\"scherzo:scheduled-job:pr-conflict-repair\"},"
  <> "{\"id\":\"label-extra\",\"name\":\"job:pr-conflict-repair\"}],"
  <> "\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}}"
}

fn contract_project_node(slug: String) -> String {
  "{\"id\":\"project-"
  <> slug
  <> "\",\"name\":\"Project "
  <> slug
  <> "\",\"slugId\":\""
  <> slug
  <> "\",\"teams\":{\"nodes\":[{\"id\":\"team-"
  <> slug
  <> "\",\"key\":\"ENG\",\"name\":\"Engineering\",\"states\":{\"nodes\":[{\"id\":\"state-triage-"
  <> slug
  <> "\",\"name\":\"Triage\",\"type\":\"triage\"}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}},\"labels\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":null}}}"
}

fn issue_search_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":["
  <> issue_node("lin-closed", "completed", "2026-05-08T00:00:03Z")
  <> ","
  <> issue_node("lin-open", "started", "2026-05-08T00:00:01Z")
  <> "]}}}"
}

fn empty_issue_search_response() -> String {
  "{\"data\":{\"issues\":{\"nodes\":[]}}}"
}

fn issue_node(id: String, state_type: String, updated_at: String) -> String {
  "{\"id\":\""
  <> id
  <> "\",\"updatedAt\":\""
  <> updated_at
  <> "\",\"state\":{\"type\":\""
  <> state_type
  <> "\"},\"labels\":{\"nodes\":["
  <> "{\"name\":\"scherzo:scheduled\"},"
  <> "{\"name\":\"scherzo:scheduled-job:pr-conflict-repair\"},"
  <> "{\"name\":\"job:pr-conflict-repair\"}"
  <> "]}}"
}
