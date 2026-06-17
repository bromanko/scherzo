import birl.{type Time}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string
import scherzo/task
import scherzo/work_item/action

pub const default_page_limit = 50

pub const max_page_limit = 100

pub const default_list_subtask_limit = 10

pub const default_show_subtask_limit = 50

pub const default_label_limit = 50

pub type WorkItemStateFilter {
  ActiveWorkItems
  ArchiveWorkItems
  CategoryWorkItems(List(task.TaskStateCategory))
}

pub type WorkItemSort {
  UpdatedDescWorkItems
}

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
    parent: Option(WorkItemSource),
    title: String,
    state: task.TaskState,
    labels: List(task.TaskLabel),
    labels_truncated: Bool,
    created_at: Option(Time),
    updated_at: Option(Time),
    actions: List(action.WorkItemAction),
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
    search: Option(String),
    sort: WorkItemSort,
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

pub fn default_state_filter() -> WorkItemStateFilter {
  ActiveWorkItems
}

pub fn default_sort() -> WorkItemSort {
  UpdatedDescWorkItems
}

pub fn state_filter_to_string(filter: WorkItemStateFilter) -> String {
  case filter {
    ActiveWorkItems -> "active"
    ArchiveWorkItems -> "archive"
    CategoryWorkItems(_) -> "categories"
  }
}

pub fn sort_to_string(sort: WorkItemSort) -> String {
  case sort {
    UpdatedDescWorkItems -> "updated_desc"
  }
}

pub fn sort_from_string(value: String) -> Result(WorkItemSort, Nil) {
  case value |> string.trim |> string.lowercase {
    "updated_desc" -> Ok(UpdatedDescWorkItems)
    _ -> Error(Nil)
  }
}

pub fn state_filter_categories(
  filter: WorkItemStateFilter,
) -> List(task.TaskStateCategory) {
  case filter {
    ActiveWorkItems -> [task.Backlog, task.Ready, task.Active, task.Unknown]
    ArchiveWorkItems -> [task.Done, task.Canceled, task.Duplicate]
    CategoryWorkItems(categories) -> canonical_categories(categories)
  }
}

pub fn normalize_search(search: Option(String)) -> Option(String) {
  case search {
    Some(search) -> {
      let search = string.trim(search)
      case search == "" {
        True -> None
        False -> Some(search)
      }
    }
    None -> None
  }
}

pub fn query_fingerprint(
  state_filter: WorkItemStateFilter,
  search: Option(String),
  sort: WorkItemSort,
) -> String {
  state_filter_fingerprint(state_filter)
  <> "|"
  <> search_fingerprint(search)
  <> "|"
  <> sort_to_string(sort)
}

pub fn apply_list_request(
  items: List(WorkItemSummary),
  request: WorkItemListRequest,
) -> WorkItemProviderPage {
  let matching =
    items
    |> filter_state_categories(request.state_categories)
    |> filter_search(request.search)
    |> sort_summaries(request.sort)

  let remaining = drop_first(matching, request.offset)
  WorkItemProviderPage(
    items: take_first(remaining, request.limit),
    has_more: list.length(remaining) > request.limit,
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
    parent: None,
    title: item.title,
    state: item.state,
    labels: labels,
    labels_truncated: labels_truncated,
    created_at: item.created_at,
    updated_at: item.updated_at,
    actions: [],
  )
}

pub fn detail_from_task_and_subtasks(
  item: task.Task,
  subtasks: List(task.Task),
  label_limit: Int,
  subtask_limit: Int,
) -> WorkItemDetail {
  let summary = summary_from_task(item, label_limit)
  let parent_source = source_from_task_ref(item.ref)
  let subtask_summaries =
    list.map(subtasks, fn(subtask) {
      let summary = summary_from_task(subtask, label_limit)
      WorkItemSummary(..summary, parent: Some(parent_source))
    })
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

fn drop_first(values: List(a), count: Int) -> List(a) {
  case count <= 0, values {
    True, _ -> values
    _, [] -> []
    False, [_, ..rest] -> drop_first(rest, count - 1)
  }
}

fn canonical_categories(
  categories: List(task.TaskStateCategory),
) -> List(task.TaskStateCategory) {
  categories
  |> dedupe_categories([])
  |> list.sort(by: compare_categories)
}

fn dedupe_categories(
  categories: List(task.TaskStateCategory),
  acc: List(task.TaskStateCategory),
) -> List(task.TaskStateCategory) {
  case categories {
    [] -> acc
    [category, ..rest] ->
      case list.contains(acc, category) {
        True -> dedupe_categories(rest, acc)
        False -> dedupe_categories(rest, [category, ..acc])
      }
  }
}

fn compare_categories(
  left: task.TaskStateCategory,
  right: task.TaskStateCategory,
) -> order.Order {
  string.compare(
    task.state_category_to_string(left),
    task.state_category_to_string(right),
  )
}

fn state_filter_fingerprint(filter: WorkItemStateFilter) -> String {
  case filter {
    ActiveWorkItems -> "active"
    ArchiveWorkItems -> "archive"
    CategoryWorkItems(categories) ->
      "categories:"
      <> string.join(
        list.map(
          canonical_categories(categories),
          task.state_category_to_string,
        ),
        with: ",",
      )
  }
}

fn search_fingerprint(search: Option(String)) -> String {
  case normalize_search(search) {
    Some(search) -> "search:" <> string.lowercase(search)
    None -> "search:"
  }
}

fn filter_state_categories(
  items: List(WorkItemSummary),
  categories: List(task.TaskStateCategory),
) -> List(WorkItemSummary) {
  case categories {
    [] -> items
    categories ->
      list.filter(items, fn(item) {
        list.contains(categories, item.state.category)
      })
  }
}

fn filter_search(
  items: List(WorkItemSummary),
  search: Option(String),
) -> List(WorkItemSummary) {
  case normalize_search(search) {
    Some(search) -> {
      let search = string.lowercase(search)
      list.filter(items, fn(item) { summary_matches_search(item, search) })
    }
    None -> items
  }
}

fn summary_matches_search(item: WorkItemSummary, search: String) -> Bool {
  list.any(summary_search_haystacks(item), fn(value) {
    string.contains(string.lowercase(value), search)
  })
}

fn summary_search_haystacks(item: WorkItemSummary) -> List(String) {
  let label_names = list.map(item.labels, fn(label) { label.name })
  let source_bits = case item.source.display_id {
    Some(display_id) -> [display_id, item.source.id]
    None -> [item.source.id]
  }
  [item.id, item.title, ..source_bits]
  |> list.append(label_names)
}

fn sort_summaries(
  items: List(WorkItemSummary),
  sort: WorkItemSort,
) -> List(WorkItemSummary) {
  case sort {
    UpdatedDescWorkItems -> list.sort(items, by: compare_updated_desc)
  }
}

fn compare_updated_desc(
  left: WorkItemSummary,
  right: WorkItemSummary,
) -> order.Order {
  case compare_optional_times_desc(left.updated_at, right.updated_at) {
    order.Eq -> string.compare(left.id, right.id)
    other -> other
  }
}

fn compare_optional_times_desc(
  left: Option(Time),
  right: Option(Time),
) -> order.Order {
  case left, right {
    Some(left), Some(right) -> compare_millis_desc(left, right)
    Some(_), None -> order.Lt
    None, Some(_) -> order.Gt
    None, None -> order.Eq
  }
}

fn compare_millis_desc(left: Time, right: Time) -> order.Order {
  let left = birl.to_unix_milli(left)
  let right = birl.to_unix_milli(right)
  case left > right {
    True -> order.Lt
    False ->
      case left < right {
        True -> order.Gt
        False -> order.Eq
      }
  }
}
