import birl.{type Time}
import gleam/option.{type Option}
import scherzo/tracker/state as issue_state

pub type BlockerRef {
  BlockerRef(
    id: Option(String),
    identifier: Option(String),
    state: Option(issue_state.IssueState),
  )
}

pub type Issue {
  Issue(
    id: String,
    identifier: String,
    title: String,
    description: Option(String),
    priority: Option(Int),
    state: issue_state.IssueState,
    branch_name: Option(String),
    url: Option(String),
    labels: List(String),
    blocked_by: List(BlockerRef),
    blocked_by_complete: Bool,
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}
