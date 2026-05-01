import scherzo/domain
import scherzo/error
import scherzo/tracker/state as issue_state

pub type Client {
  Client(
    fetch_candidate_issues: fn() ->
      Result(List(domain.Issue), error.TrackerError),
    fetch_issues_by_states: fn(List(issue_state.IssueState)) ->
      Result(List(domain.Issue), error.TrackerError),
    fetch_issue_states_by_ids: fn(List(String)) ->
      Result(List(domain.Issue), error.TrackerError),
  )
}
