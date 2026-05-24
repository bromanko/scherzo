import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/conformance/case_support
import scherzo/tracker/conformance/types

type RemoteEventsValidationError {
  NoRemoteEvents
  MalformedRemoteEvent
  DuplicateRemoteEventId
  MissingFixtureTaskCoverage
  RemoteEventFieldTooLong
  RemoteEventLimitExceeded
}

pub fn validate_remote_events(
  events: List(types.RemoteCommandEventPayload),
  fixture_refs: List(task.TaskRef),
  expect_filtered_event: Option(String),
  max_per_task: Option(Int),
) -> Result(String, String) {
  case events {
    [] -> Error(describe_remote_events_validation_error(NoRemoteEvents))
    _ ->
      case all_events_normalized(events, fixture_refs, expect_filtered_event) {
        False ->
          Error(describe_remote_events_validation_error(MalformedRemoteEvent))
        True ->
          case remote_event_fields_within_limits(events) {
            False ->
              Error(describe_remote_events_validation_error(
                RemoteEventFieldTooLong,
              ))
            True ->
              case unique_event_ids(events) {
                False ->
                  Error(describe_remote_events_validation_error(
                    DuplicateRemoteEventId,
                  ))
                True ->
                  case covers_all_fixture_refs(events, fixture_refs) {
                    False ->
                      Error(describe_remote_events_validation_error(
                        MissingFixtureTaskCoverage,
                      ))
                    True ->
                      case limit_respected(events, max_per_task) {
                        False ->
                          Error(describe_remote_events_validation_error(
                            RemoteEventLimitExceeded,
                          ))
                        True ->
                          Ok(
                            "driver returned "
                            <> int.to_string(count_events(events))
                            <> " normalized remote command event(s)"
                            <> " across "
                            <> int.to_string(count_refs(events))
                            <> " fixture task(s)",
                          )
                      }
                  }
              }
          }
      }
  }
}

fn all_events_normalized(
  events: List(types.RemoteCommandEventPayload),
  fixture_refs: List(task.TaskRef),
  expect_filtered_event: Option(String),
) -> Bool {
  case events {
    [] -> True
    [event, ..rest] ->
      event_normalized(event, fixture_refs, expect_filtered_event)
      && all_events_normalized(rest, fixture_refs, expect_filtered_event)
  }
}

fn event_normalized(
  event: types.RemoteCommandEventPayload,
  fixture_refs: List(task.TaskRef),
  expect_filtered_event: Option(String),
) -> Bool {
  let types.RemoteCommandEventPayload(
    event_id: event_id,
    task: ref,
    author_id: author_id,
    body: body,
    command_name: command_name,
    excerpt: excerpt,
    observed_at_ms: observed_at_ms,
  ) = event
  event_id != ""
  && author_id != ""
  && body != ""
  && command_name != ""
  && excerpt != ""
  && observed_at_ms > 0
  && ref_in_list(fixture_refs, ref)
  && event_not_filtered(expect_filtered_event, event_id)
}

fn remote_event_fields_within_limits(
  events: List(types.RemoteCommandEventPayload),
) -> Bool {
  case events {
    [] -> True
    [event, ..rest] ->
      remote_event_within_limits(event)
      && remote_event_fields_within_limits(rest)
  }
}

fn remote_event_within_limits(event: types.RemoteCommandEventPayload) -> Bool {
  let types.RemoteCommandEventPayload(
    event_id: event_id,
    author_id: author_id,
    body: body,
    command_name: command_name,
    excerpt: excerpt,
    ..,
  ) = event
  string.length(event_id) <= types.max_remote_command_event_id_chars
  && string.length(author_id) <= types.max_remote_command_author_id_chars
  && string.length(command_name) <= types.max_remote_command_name_chars
  && string.length(body) <= types.max_remote_command_body_chars
  && string.length(excerpt) <= types.max_remote_command_excerpt_chars
}

fn unique_event_ids(events: List(types.RemoteCommandEventPayload)) -> Bool {
  case events {
    [] -> True
    [types.RemoteCommandEventPayload(event_id: event_id, ..), ..rest] ->
      !event_id_in_list(rest, event_id) && unique_event_ids(rest)
  }
}

fn event_id_in_list(
  events: List(types.RemoteCommandEventPayload),
  target: String,
) -> Bool {
  case events {
    [] -> False
    [types.RemoteCommandEventPayload(event_id: event_id, ..), ..rest] ->
      event_id == target || event_id_in_list(rest, target)
  }
}

fn covers_all_fixture_refs(
  events: List(types.RemoteCommandEventPayload),
  fixture_refs: List(task.TaskRef),
) -> Bool {
  case fixture_refs {
    [] -> True
    [ref, ..rest] ->
      ref_present_in_events(events, ref)
      && covers_all_fixture_refs(events, rest)
  }
}

fn ref_present_in_events(
  events: List(types.RemoteCommandEventPayload),
  target: task.TaskRef,
) -> Bool {
  case events {
    [] -> False
    [types.RemoteCommandEventPayload(task: ref, ..), ..rest] ->
      case_support.same_ref(ref, target) || ref_present_in_events(rest, target)
  }
}

fn event_not_filtered(
  expect_filtered_event: Option(String),
  event_id: String,
) -> Bool {
  case expect_filtered_event {
    Some(filtered) -> filtered != event_id
    None -> True
  }
}

fn limit_respected(
  events: List(types.RemoteCommandEventPayload),
  max_per_task: Option(Int),
) -> Bool {
  case max_per_task {
    None -> True
    Some(limit) -> max_events_per_task(events) <= limit
  }
}

fn max_events_per_task(events: List(types.RemoteCommandEventPayload)) -> Int {
  case unique_refs_from_events(events) {
    [] -> 0
    refs -> max_count_for_refs(refs, events, 0)
  }
}

fn max_count_for_refs(
  refs: List(task.TaskRef),
  events: List(types.RemoteCommandEventPayload),
  current_max: Int,
) -> Int {
  case refs {
    [] -> current_max
    [ref, ..rest] -> {
      let count = count_events_for_ref(events, ref)
      let next_max = case count > current_max {
        True -> count
        False -> current_max
      }
      max_count_for_refs(rest, events, next_max)
    }
  }
}

fn count_events_for_ref(
  events: List(types.RemoteCommandEventPayload),
  target: task.TaskRef,
) -> Int {
  case events {
    [] -> 0
    [types.RemoteCommandEventPayload(task: ref, ..), ..rest] -> {
      let current = case case_support.same_ref(ref, target) {
        True -> 1
        False -> 0
      }
      current + count_events_for_ref(rest, target)
    }
  }
}

fn unique_refs_from_events(
  events: List(types.RemoteCommandEventPayload),
) -> List(task.TaskRef) {
  case events {
    [] -> []
    [types.RemoteCommandEventPayload(task: ref, ..), ..rest] ->
      add_unique_ref(ref, unique_refs_from_events(rest))
  }
}

fn add_unique_ref(
  ref: task.TaskRef,
  refs: List(task.TaskRef),
) -> List(task.TaskRef) {
  case ref_in_list(refs, ref) {
    True -> refs
    False -> [ref, ..refs]
  }
}

fn ref_in_list(refs: List(task.TaskRef), target: task.TaskRef) -> Bool {
  case refs {
    [] -> False
    [ref, ..rest] ->
      case_support.same_ref(ref, target) || ref_in_list(rest, target)
  }
}

fn count_events(events: List(types.RemoteCommandEventPayload)) -> Int {
  case events {
    [] -> 0
    [_, ..rest] -> 1 + count_events(rest)
  }
}

fn count_refs(events: List(types.RemoteCommandEventPayload)) -> Int {
  count_task_refs(unique_refs_from_events(events))
}

fn count_task_refs(refs: List(task.TaskRef)) -> Int {
  case refs {
    [] -> 0
    [_, ..rest] -> 1 + count_task_refs(rest)
  }
}

fn describe_remote_events_validation_error(
  error: RemoteEventsValidationError,
) -> String {
  case error {
    NoRemoteEvents -> "driver returned no remote command events"
    MalformedRemoteEvent ->
      "at least one event had an empty field, mismatched task ref, or filtered event id"
    DuplicateRemoteEventId ->
      "driver returned duplicate remote command event ids"
    MissingFixtureTaskCoverage ->
      "driver did not return at least one normalized remote command event for every fixture task"
    RemoteEventFieldTooLong ->
      "driver returned a remote command event field longer than the protocol limit"
    RemoteEventLimitExceeded ->
      "driver returned more than the allowed number of events per task"
  }
}
