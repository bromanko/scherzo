import scherzo/error
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub type Client {
  Client(
    fetch_candidate_issues: fn() ->
      Result(List(tracker_issue.Issue), error.TrackerError),
    fetch_issues_by_states: fn(List(issue_state.IssueState)) ->
      Result(List(tracker_issue.Issue), error.TrackerError),
    fetch_issue_states_by_ids: fn(List(String)) ->
      Result(List(tracker_issue.Issue), error.TrackerError),
  )
}
