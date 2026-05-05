import birl
import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/smoke
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

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
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

pub fn linear_smoke_counts_non_empty_samples_test() {
  let candidate = issue("candidate-id", "ABC-1", "Todo")
  let terminal = issue("terminal-id", "ABC-2", "Done")
  let reader =
    smoke.LinearSmokeReader(
      fetch_candidate_sample: fn() { Ok([candidate]) },
      fetch_terminal_sample: fn(_) { Ok([terminal]) },
      refresh_issue_states_by_ids: fn(ids) {
        assert ids == ["candidate-id"]
        Ok([candidate])
      },
    )

  let assert Ok(result) =
    smoke.linear_read_smoke(reader, issue_state.list_from_strings(["Done"]))
  assert result.candidate_count == 1
  assert result.terminal_count == 1
  assert result.refreshed_count == 1
}

pub fn linear_smoke_succeeds_with_no_samples_test() {
  let reader =
    smoke.LinearSmokeReader(
      fetch_candidate_sample: fn() { Ok([]) },
      fetch_terminal_sample: fn(_) { Ok([]) },
      refresh_issue_states_by_ids: fn(_) { Ok([]) },
    )

  let assert Ok(result) =
    smoke.linear_read_smoke(reader, issue_state.list_from_strings(["Done"]))
  assert result.candidate_count == 0
  assert result.terminal_count == 0
  assert result.refreshed_count == 0
}

pub fn linear_smoke_calls_each_sample_reader_once_test() {
  let subject = process.new_subject()
  let reader =
    smoke.LinearSmokeReader(
      fetch_candidate_sample: fn() {
        process.send(subject, "candidate")
        Ok([])
      },
      fetch_terminal_sample: fn(_) {
        process.send(subject, "terminal")
        Ok([])
      },
      refresh_issue_states_by_ids: fn(_) {
        process.send(subject, "refresh")
        Ok([])
      },
    )

  let assert Ok(result) =
    smoke.linear_read_smoke(reader, issue_state.list_from_strings(["Done"]))
  assert result.refreshed_count == 0
  assert process.receive(subject, within: 20) == Ok("candidate")
  assert process.receive(subject, within: 20) == Ok("terminal")
  assert process.receive(subject, within: 20) == Error(Nil)
}
