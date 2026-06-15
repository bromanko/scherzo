import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/query/dto as query_dto
import scherzo/control/query/types as query_types
import scherzo/task
import scherzo/terminal/sanitize

pub type StateCategory =
  task.TaskStateCategory

pub fn state_category_from_string(value: String) -> Result(StateCategory, Nil) {
  task.state_category_from_string(value)
}

pub fn list_json(tasks: query_types.TaskListDto) -> String {
  tasks |> query_dto.task_list_to_json |> json.to_string
}

pub fn detail_json(detail: query_types.TaskDetailDto) -> String {
  detail |> query_dto.task_detail_to_json |> json.to_string
}

pub fn outbox_status_from_string(
  value: String,
) -> Result(query_types.OutboxRecordStatus, Nil) {
  query_types.outbox_status_from_string(value)
}

pub fn outbox_list_json(outbox: query_types.OutboxListDto) -> String {
  outbox |> query_dto.outbox_list_to_json |> json.to_string
}

pub fn outbox_record_json(record: query_types.OutboxRecordDto) -> String {
  record |> query_dto.outbox_record_to_json |> json.to_string
}

pub fn print_list(
  tasks: query_types.TaskListDto,
  line: fn(String) -> Nil,
) -> Nil {
  case tasks.items {
    [] -> line("No tasks found.")
    items -> list.each(items, fn(item) { line(task_summary_line(item)) })
  }
  case tasks.page.next_cursor {
    Some(cursor) -> line("next_cursor: " <> sanitize.text(cursor))
    None -> Nil
  }
}

pub fn print_detail(
  detail: query_types.TaskDetailDto,
  line: fn(String) -> Nil,
) -> Nil {
  let summary = detail.summary
  line(task_summary_line(summary))
  line("provider: " <> sanitize.text(summary.source.provider))
  line("id: " <> sanitize.text(summary.id))
  print_optional_sanitized("url", summary.source.url, line)
  case summary.labels {
    [] -> Nil
    labels ->
      line(
        "labels: "
        <> string.join(
          list.map(labels, fn(label) { sanitize.text(label.name) }),
          with: ", ",
        ),
      )
  }
  print_optional_sanitized("created_at", summary.created_at, line)
  print_optional_sanitized("updated_at", summary.updated_at, line)
  line("description (" <> sanitize.text(detail.description.format) <> "):")
  print_description_body(detail.description.body, line)
}

pub fn print_outbox_list(
  outbox: query_types.OutboxListDto,
  line: fn(String) -> Nil,
) -> Nil {
  case outbox.items {
    [] -> line("No outbox records found.")
    items ->
      list.each(items, fn(item) { line(outbox_record_summary_line(item)) })
  }
  case outbox.page.next_cursor {
    Some(cursor) -> line("next_cursor: " <> sanitize.text(cursor))
    None -> Nil
  }
}

pub fn print_outbox_record(
  record: query_types.OutboxRecordDto,
  line: fn(String) -> Nil,
) -> Nil {
  line("outbox_id: " <> sanitize.text(record.outbox_id))
  line("status: " <> outbox_status_text(record.status))
  line("kind: " <> sanitize.text(record.kind))
  line("task_ref: " <> outbox_task_ref_label(record.task_ref))
  line("task_provider: " <> sanitize.text(record.task_ref.provider))
  line("task_id: " <> sanitize.text(record.task_ref.id))
  print_optional_sanitized("task_url", record.task_ref.url, line)
  print_optional_sanitized("dedupe_key", record.dedupe_key, line)
  print_optional_int("attempt_count", record.attempt_count, line)
  print_optional_int("next_attempt_at_ms", record.next_attempt_at_ms, line)
  print_optional_sanitized("last_error_code", record.last_error_code, line)
  print_optional_int("pending_at_ms", record.pending_at_ms, line)
  print_optional_int("attempted_at_ms", record.attempted_at_ms, line)
  print_optional_int("failed_at_ms", record.failed_at_ms, line)
  print_optional_int("completed_at_ms", record.completed_at_ms, line)
  case record.has_payload {
    True -> line("payload: redacted")
    False -> line("payload: unavailable")
  }
}

fn outbox_record_summary_line(record: query_types.OutboxRecordDto) -> String {
  sanitize.text(record.outbox_id)
  <> " "
  <> outbox_status_text(record.status)
  <> " "
  <> sanitize.text(record.kind)
  <> " "
  <> outbox_task_ref_label(record.task_ref)
  <> outbox_summary_suffix(record)
}

fn outbox_summary_suffix(record: query_types.OutboxRecordDto) -> String {
  let parts =
    []
    |> append_int_part("attempts", record.attempt_count)
    |> append_int_part("next_attempt_at_ms", record.next_attempt_at_ms)
    |> append_string_part("error", record.last_error_code)
  case parts {
    [] -> ""
    _ -> " " <> string.join(parts, with: " ")
  }
}

fn outbox_task_ref_label(task_ref: query_types.OutboxTaskRefDto) -> String {
  sanitize.text(case task_ref.display_id {
    Some(display_id) -> display_id
    None -> task_ref.provider <> ":" <> task_ref.id
  })
}

fn outbox_status_text(status: query_types.OutboxRecordStatus) -> String {
  status |> query_types.outbox_status_to_string |> sanitize.text
}

fn append_int_part(
  parts: List(String),
  label: String,
  value: Option(Int),
) -> List(String) {
  case value {
    Some(value) -> list.append(parts, [label <> "=" <> int.to_string(value)])
    None -> parts
  }
}

fn append_string_part(
  parts: List(String),
  label: String,
  value: Option(String),
) -> List(String) {
  case value {
    Some(value) -> list.append(parts, [label <> "=" <> sanitize.text(value)])
    None -> parts
  }
}

fn task_summary_line(summary: query_types.TaskSummaryDto) -> String {
  sanitize.text(task_display_id(summary.source))
  <> " "
  <> task.state_category_to_string(summary.state)
  <> task_priority_text(summary.priority)
  <> " "
  <> sanitize.text(summary.title)
}

fn print_optional_sanitized(
  label: String,
  value: Option(String),
  line: fn(String) -> Nil,
) -> Nil {
  case value {
    Some(value) -> line(label <> ": " <> sanitize.text(value))
    None -> Nil
  }
}

fn print_optional_int(
  label: String,
  value: Option(Int),
  line: fn(String) -> Nil,
) -> Nil {
  case value {
    Some(value) -> line(label <> ": " <> int.to_string(value))
    None -> Nil
  }
}

fn print_description_body(body: String, line: fn(String) -> Nil) -> Nil {
  case body {
    "" -> Nil
    body -> {
      let #(lines, truncated) =
        sanitize.bounded_body_lines(body, 200, 500, "[truncated]")
      list.each(lines, line)
      case truncated {
        True -> line("[description truncated]")
        False -> Nil
      }
    }
  }
}

fn task_display_id(source: query_types.TaskSourceDto) -> String {
  case source.display_id {
    Some(display_id) -> display_id
    None -> source.provider <> ":" <> source.id
  }
}

fn task_priority_text(priority: Option(query_types.TaskPriorityDto)) -> String {
  case priority {
    Some(priority) -> " [" <> sanitize.text(priority.label) <> "]"
    None -> ""
  }
}
