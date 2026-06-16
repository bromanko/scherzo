import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/query/work_item_dto
import scherzo/task
import scherzo/work_item

pub fn daemon_id_generation_and_state_preservation_test() {
  let item = base_task(remote_id: "issue-1", display_id: "LIV-1168")
  let summary = work_item.summary_from_task(item, work_item.default_label_limit)

  assert summary.id == "linear:issue-1"
  assert summary.source.provider == "linear"
  assert summary.source.display_id == Some("LIV-1168")
  assert summary.state.id == Some("todo")
  assert summary.state.name == "Todo"
  assert summary.state.category == task.Ready
}

pub fn label_truncation_test() {
  let labels = build_labels(55, [])
  let item =
    task.Task(
      ..base_task(remote_id: "issue-2", display_id: "LIV-2000"),
      labels: labels,
    )
  let summary = work_item.summary_from_task(item, work_item.default_label_limit)

  assert list.length(summary.labels) == work_item.default_label_limit
  assert summary.labels_truncated
}

pub fn subtask_truncation_test() {
  let subtasks = build_subtasks(12, [])
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "parent-1", display_id: "LIV-P1"),
      subtasks,
      work_item.default_label_limit,
      work_item.default_list_subtask_limit,
    )

  assert list.length(detail.subtasks) == work_item.default_list_subtask_limit
  assert detail.subtasks_truncated
}

pub fn zero_child_parent_test() {
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "parent-2", display_id: "LIV-P2"),
      [],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )

  assert detail.subtasks == []
  assert detail.subtasks_truncated == False
}

pub fn work_item_json_excludes_description_and_comment_fields_test() {
  let item =
    task.Task(
      ..base_task(remote_id: "issue-3", display_id: "LIV-3000"),
      description: Some("SECRET_DESCRIPTION"),
    )
  let detail =
    work_item.detail_from_task_and_subtasks(
      item,
      [],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )
  let encoded = work_item_dto.work_item_detail_to_json(detail) |> json.to_string

  assert string.contains(encoded, "Implement work item projection")
  assert !string.contains(encoded, "description")
  assert !string.contains(encoded, "SECRET_DESCRIPTION")
  assert !string.contains(encoded, "comment")
}

fn base_task(
  remote_id remote_id: String,
  display_id display_id: String,
) -> task.Task {
  task.Task(
    ref: task.TaskRef(
      backend_kind: "linear",
      remote_id: remote_id,
      key: Some(display_id),
      url: Some("https://linear.app/living-systems/issue/" <> display_id),
    ),
    title: "Implement work item projection",
    description: None,
    priority: None,
    state: task.TaskState(id: Some("todo"), name: "Todo", category: task.Ready),
    branch_hint: None,
    labels: [
      task.TaskLabel(id: Some("label-workflow"), name: "workflow:execplan"),
    ],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn build_labels(
  remaining: Int,
  acc: List(task.TaskLabel),
) -> List(task.TaskLabel) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      build_labels(remaining - 1, [
        task.TaskLabel(
          id: Some("label-" <> int_to_string(remaining)),
          name: "label:" <> int_to_string(remaining),
        ),
        ..acc
      ])
  }
}

fn build_subtasks(remaining: Int, acc: List(task.Task)) -> List(task.Task) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      build_subtasks(remaining - 1, [
        base_task(
          remote_id: "child-" <> int_to_string(remaining),
          display_id: "LIV-C" <> int_to_string(remaining),
        ),
        ..acc
      ])
  }
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}
