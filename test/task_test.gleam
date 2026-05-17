import gleam/option.{None, Some}
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

fn legacy_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "LIV-266",
    title: "Refresh architecture",
    description: Some("body"),
    priority: Some(2),
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: Some("liv-266-refresh"),
    url: Some("https://linear.app/living-systems/issue/LIV-266"),
    labels: ["workflow:execplan-v2", "kind:feature"],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn legacy_issue_with_blockers(
  blockers: List(tracker_issue.BlockerRef),
) -> tracker_issue.Issue {
  tracker_issue.Issue(..legacy_issue(), blocked_by: blockers)
}

pub fn task_display_key_prefers_key_test() {
  let ref =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some("LIV-266"),
      url: None,
    )

  assert task.display_key(ref) == "LIV-266"
}

pub fn task_display_key_falls_back_to_remote_id_test() {
  let ref =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: None,
      url: None,
    )

  assert task.display_key(ref) == "issue-1"
}

pub fn task_display_key_treats_empty_key_as_missing_test() {
  let ref =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some(""),
      url: None,
    )

  assert task.display_key(ref) == "issue-1"
}

pub fn task_display_key_trims_key_test() {
  let ref =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some(" LIV-266 "),
      url: None,
    )

  assert task.display_key(ref) == "LIV-266"
}

pub fn issue_to_task_preserves_linear_fields_test() {
  let converted = task.from_legacy_issue(legacy_issue())

  assert converted.ref
    == task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some("LIV-266"),
      url: Some("https://linear.app/living-systems/issue/LIV-266"),
    )
  assert converted.title == "Refresh architecture"
  assert converted.description == Some("body")
  assert converted.priority == Some(2)
  assert converted.state
    == task.TaskState(id: None, name: "Todo", category: task.Unknown)
  assert converted.branch_hint == Some("liv-266-refresh")
  assert task.label_names(converted) == ["workflow:execplan-v2", "kind:feature"]
  assert converted.blockers == []
  assert converted.blockers_complete == True
  assert converted.created_at == None
  assert converted.updated_at == None
}

pub fn linear_task_converts_to_legacy_issue_test() {
  let converted = task.from_legacy_issue(legacy_issue())
  let assert Ok(issue) = task.to_legacy_issue(converted)

  assert issue.id == "issue-1"
  assert issue.identifier == "LIV-266"
  assert issue.title == "Refresh architecture"
  assert issue.description == Some("body")
  assert issue.priority == Some(2)
  assert issue.state == issue_state.from_string_unchecked("Todo")
  assert issue.branch_name == Some("liv-266-refresh")
  assert issue.url == Some("https://linear.app/living-systems/issue/LIV-266")
  assert issue.labels == ["workflow:execplan-v2", "kind:feature"]
  assert issue.blocked_by == []
  assert issue.blocked_by_complete == True
  assert issue.created_at == None
  assert issue.updated_at == None
}

pub fn issue_to_task_normalizes_blocker_refs_test() {
  let converted =
    task.from_legacy_issue(
      legacy_issue_with_blockers([
        tracker_issue.BlockerRef(
          id: Some(" blocker-id "),
          identifier: Some(" LIV-267 "),
          state: None,
        ),
        tracker_issue.BlockerRef(
          id: None,
          identifier: Some(" LIV-269 "),
          state: None,
        ),
        tracker_issue.BlockerRef(
          id: Some(""),
          identifier: Some("   "),
          state: None,
        ),
      ]),
    )

  assert converted.blockers
    == [
      task.TaskRef(
        backend_kind: "linear",
        remote_id: "blocker-id",
        key: Some("LIV-267"),
        url: None,
      ),
      task.TaskRef(
        backend_kind: "linear",
        remote_id: "LIV-269",
        key: Some("LIV-269"),
        url: None,
      ),
    ]
}

pub fn linear_task_converts_blockers_to_legacy_issue_test() {
  let converted =
    task.Task(
      ref: task.TaskRef(
        backend_kind: "linear",
        remote_id: "issue-1",
        key: Some("LIV-266"),
        url: None,
      ),
      title: "Refresh architecture",
      description: None,
      priority: None,
      state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
      branch_hint: None,
      labels: [],
      blockers: [
        task.TaskRef(
          backend_kind: "linear",
          remote_id: " blocker-id ",
          key: Some(" LIV-267 "),
          url: None,
        ),
        task.TaskRef(
          backend_kind: "linear",
          remote_id: "",
          key: Some(" LIV-269 "),
          url: None,
        ),
        task.TaskRef(
          backend_kind: "linear",
          remote_id: " ",
          key: None,
          url: None,
        ),
      ],
      blockers_complete: False,
      created_at: None,
      updated_at: None,
    )
  let assert Ok(issue) = task.to_legacy_issue(converted)

  assert issue.blocked_by
    == [
      tracker_issue.BlockerRef(
        id: Some("blocker-id"),
        identifier: Some("LIV-267"),
        state: None,
      ),
      tracker_issue.BlockerRef(
        id: None,
        identifier: Some("LIV-269"),
        state: None,
      ),
    ]
  assert issue.blocked_by_complete == False
}

pub fn non_linear_task_cannot_convert_to_legacy_issue_test() {
  let non_linear =
    task.Task(
      ref: task.TaskRef(
        backend_kind: "test-memory",
        remote_id: "card-1",
        key: Some("CARD-1"),
        url: None,
      ),
      title: "Fake card",
      description: None,
      priority: None,
      state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
      branch_hint: None,
      labels: [],
      blockers: [],
      blockers_complete: True,
      created_at: None,
      updated_at: None,
    )

  assert task.to_legacy_issue(non_linear) == Error(task.RequiresLinearTask)
}

pub fn linear_task_requires_key_for_legacy_issue_test() {
  let linear_without_key =
    task.Task(
      ref: task.TaskRef(
        backend_kind: "linear",
        remote_id: "issue-1",
        key: None,
        url: None,
      ),
      title: "Refresh architecture",
      description: None,
      priority: None,
      state: task.TaskState(id: None, name: "Todo", category: task.Unknown),
      branch_hint: None,
      labels: [],
      blockers: [],
      blockers_complete: True,
      created_at: None,
      updated_at: None,
    )

  assert task.to_legacy_issue(linear_without_key) == Error(task.MissingTaskKey)
}
