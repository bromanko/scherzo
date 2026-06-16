import birl.{type Time}
import gleam/list
import gleam/option.{type Option, Some}
import scherzo/task

pub const default_page_limit = 50

pub const max_page_limit = 100

pub const default_list_subtask_limit = 10

pub const default_show_subtask_limit = 50

pub const default_label_limit = 50

pub type WorkItemSource {
  WorkItemSource(
    provider: String,
    id: String,
    display_id: Option(String),
    url: Option(String),
  )
}

pub type WorkItemSummary {
  WorkItemSummary(
    id: String,
    source: WorkItemSource,
    title: String,
    state: task.TaskState,
    labels: List(task.TaskLabel),
    labels_truncated: Bool,
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub type WorkItemDetail {
  WorkItemDetail(
    summary: WorkItemSummary,
    subtasks: List(WorkItemSummary),
    subtasks_truncated: Bool,
  )
}

pub type WorkItemLookupRef {
  WorkItemLookupByDisplayId(String)
  WorkItemLookupByRemoteId(provider: Option(String), id: String)
}

pub type WorkItemListRequest {
  WorkItemListRequest(
    state_categories: List(task.TaskStateCategory),
    limit: Int,
    offset: Int,
    subtask_limit: Int,
    label_limit: Int,
  )
}

pub type WorkItemShowRequest {
  WorkItemShowRequest(
    ref: WorkItemLookupRef,
    subtask_limit: Int,
    label_limit: Int,
  )
}

pub type WorkItemProviderPage {
  WorkItemProviderPage(items: List(WorkItemSummary), has_more: Bool)
}

pub type WorkItemPage {
  WorkItemPage(
    items: List(WorkItemSummary),
    next_cursor: Option(String),
    has_more: Bool,
  )
}

pub fn daemon_id(source: WorkItemSource) -> String {
  source.provider <> ":" <> source.id
}

pub fn source_from_task_ref(ref: task.TaskRef) -> WorkItemSource {
  WorkItemSource(
    provider: ref.backend_kind,
    id: ref.remote_id,
    display_id: ref.key,
    url: ref.url,
  )
}

pub fn lookup_ref_from_task_query_ref(ref: task.TaskRef) -> WorkItemLookupRef {
  WorkItemLookupByRemoteId(provider: Some(ref.backend_kind), id: ref.remote_id)
}

pub fn summary_from_task(item: task.Task, label_limit: Int) -> WorkItemSummary {
  let #(labels, labels_truncated) = clamp_labels(item.labels, label_limit)
  let source = source_from_task_ref(item.ref)

  WorkItemSummary(
    id: daemon_id(source),
    source: source,
    title: item.title,
    state: item.state,
    labels: labels,
    labels_truncated: labels_truncated,
    created_at: item.created_at,
    updated_at: item.updated_at,
  )
}

pub fn detail_from_task_and_subtasks(
  item: task.Task,
  subtasks: List(task.Task),
  label_limit: Int,
  subtask_limit: Int,
) -> WorkItemDetail {
  let summary = summary_from_task(item, label_limit)
  let subtask_summaries =
    list.map(subtasks, fn(subtask) { summary_from_task(subtask, label_limit) })
  let #(bounded_subtasks, subtasks_truncated) =
    clamp_subtasks(subtask_summaries, subtask_limit)

  WorkItemDetail(
    summary: summary,
    subtasks: bounded_subtasks,
    subtasks_truncated: subtasks_truncated,
  )
}

pub fn clamp_page_limit(limit: Int) -> Int {
  clamp_limit(limit, default_page_limit, max_page_limit)
}

pub fn clamp_label_limit(limit: Int) -> Int {
  clamp_limit(limit, default_label_limit, default_label_limit)
}

pub fn clamp_list_subtask_limit(limit: Int) -> Int {
  clamp_limit(limit, default_list_subtask_limit, default_list_subtask_limit)
}

pub fn clamp_show_subtask_limit(limit: Int) -> Int {
  clamp_limit(limit, default_show_subtask_limit, default_show_subtask_limit)
}

pub fn clamp_labels(
  labels: List(task.TaskLabel),
  limit: Int,
) -> #(List(task.TaskLabel), Bool) {
  clamp_list(labels, clamp_label_limit(limit))
}

pub fn clamp_subtasks(
  subtasks: List(WorkItemSummary),
  limit: Int,
) -> #(List(WorkItemSummary), Bool) {
  clamp_list(subtasks, limit)
}

fn clamp_list(items: List(a), limit: Int) -> #(List(a), Bool) {
  let bounded = take_first(items, limit)
  #(bounded, list.length(items) > limit)
}

fn clamp_limit(limit: Int, default: Int, max: Int) -> Int {
  case limit < 1 {
    True -> default
    False -> {
      case limit > max {
        True -> max
        False -> limit
      }
    }
  }
}

fn take_first(values: List(a), count: Int) -> List(a) {
  case count <= 0, values {
    True, _ -> []
    _, [] -> []
    False, [first, ..rest] -> [first, ..take_first(rest, count - 1)]
  }
}
