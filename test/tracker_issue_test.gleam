import birl
import gleam/option.{Some}
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

pub fn issue_records_labels_and_blockers_test() {
  let issue =
    tracker_issue.Issue(
      id: "issue-id",
      identifier: "ABC-123",
      title: "Fix tests",
      description: Some("Broken tests"),
      priority: Some(1),
      state: issue_state.from_string_unchecked("Todo"),
      branch_name: Some("abc-123-fix-tests"),
      url: Some("https://linear.app/example/ABC-123"),
      labels: ["bug", "tests"],
      blocked_by: [
        tracker_issue.BlockerRef(
          id: Some("blocker-id"),
          identifier: Some("ABC-1"),
          state: Some(issue_state.from_string_unchecked("Done")),
        ),
      ],
      created_at: Some(birl.from_unix(0)),
      updated_at: Some(birl.from_unix(1)),
    )

  assert issue.identifier == "ABC-123"
  assert issue.labels == ["bug", "tests"]
  let assert [blocker] = issue.blocked_by
  assert blocker.state == Some(issue_state.from_string_unchecked("Done"))
}
