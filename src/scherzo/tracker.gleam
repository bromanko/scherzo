import scherzo/domain
import scherzo/error

pub type Client {
  Client(
    fetch_candidate_issues: fn() ->
      Result(List(domain.Issue), error.TrackerError),
    fetch_issues_by_states: fn(List(String)) ->
      Result(List(domain.Issue), error.TrackerError),
    fetch_issue_states_by_ids: fn(List(String)) ->
      Result(List(domain.Issue), error.TrackerError),
  )
}
