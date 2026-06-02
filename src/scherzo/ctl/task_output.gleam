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
