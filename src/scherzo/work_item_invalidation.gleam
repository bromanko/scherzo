import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/task
import scherzo/tracker/issue as tracker_issue

pub const max_task_refs = 25

pub const max_ref_field_length = 256

pub type Source {
  PollRefresh
  TrackerRefresh
  WorkflowObserved
  ManualRefresh
}

pub type AffectedTaskRef {
  AffectedTaskRef(provider: String, id: String, display_id: Option(String))
}

pub type Event {
  Event(
    source: Source,
    task_refs: List(AffectedTaskRef),
    has_unknown_refs: Bool,
    refs_truncated: Bool,
  )
}

type NormalizeAccumulator {
  NormalizeAccumulator(
    refs: List(AffectedTaskRef),
    seen: Dict(String, Nil),
    dropped_refs: Bool,
    refs_truncated: Bool,
  )
}

pub fn new(
  source: Source,
  task_refs: List(AffectedTaskRef),
  has_unknown_refs has_unknown_refs: Bool,
) -> Event {
  event_from_refs(source, task_refs, fn(ref) { ref }, has_unknown_refs)
}

pub fn from_task_ref(ref: task.TaskRef) -> AffectedTaskRef {
  AffectedTaskRef(
    provider: ref.backend_kind,
    id: ref.remote_id,
    display_id: ref.key,
  )
}

pub fn from_task_refs(source: Source, refs: List(task.TaskRef)) -> Event {
  event_from_refs(source, refs, from_task_ref, False)
}

pub fn from_issue(
  backend_kind: String,
  issue: tracker_issue.Issue,
) -> AffectedTaskRef {
  AffectedTaskRef(
    provider: backend_kind,
    id: issue.id,
    display_id: Some(issue.identifier),
  )
}

pub fn from_issues(
  source: Source,
  backend_kind: String,
  issues: List(tracker_issue.Issue),
) -> Event {
  event_from_refs(source, issues, from_issue(backend_kind, _), False)
}

pub fn unknown(source: Source) -> Event {
  new(source, [], has_unknown_refs: True)
}

fn event_from_refs(
  source: Source,
  values: List(a),
  to_ref: fn(a) -> AffectedTaskRef,
  initial_has_unknown_refs: Bool,
) -> Event {
  let #(bounded_refs, dropped_refs, refs_truncated) =
    normalize_refs(values, to_ref)

  Event(
    source: source,
    task_refs: bounded_refs,
    has_unknown_refs: initial_has_unknown_refs
      || dropped_refs
      || refs_truncated
      || list.is_empty(bounded_refs),
    refs_truncated: refs_truncated,
  )
}

fn normalize_refs(
  values: List(a),
  to_ref: fn(a) -> AffectedTaskRef,
) -> #(List(AffectedTaskRef), Bool, Bool) {
  let accumulator =
    list.fold(
      values,
      NormalizeAccumulator([], dict.new(), False, False),
      fn(accumulator, value) {
        append_normalized_ref(accumulator, to_ref(value))
      },
    )

  #(
    list.reverse(accumulator.refs),
    accumulator.dropped_refs,
    accumulator.refs_truncated,
  )
}

fn append_normalized_ref(
  accumulator: NormalizeAccumulator,
  ref: AffectedTaskRef,
) -> NormalizeAccumulator {
  case normalize_ref(ref) {
    Error(Nil) -> NormalizeAccumulator(..accumulator, dropped_refs: True)
    Ok(ref) -> append_unique_ref(accumulator, ref)
  }
}

fn append_unique_ref(
  accumulator: NormalizeAccumulator,
  ref: AffectedTaskRef,
) -> NormalizeAccumulator {
  let key = ref_key(ref)
  case dict.has_key(accumulator.seen, key) {
    True -> accumulator
    False ->
      case list.length(accumulator.refs) >= max_task_refs {
        True -> NormalizeAccumulator(..accumulator, refs_truncated: True)
        False ->
          NormalizeAccumulator(
            ..accumulator,
            refs: [ref, ..accumulator.refs],
            seen: dict.insert(accumulator.seen, key, Nil),
          )
      }
  }
}

fn normalize_ref(ref: AffectedTaskRef) -> Result(AffectedTaskRef, Nil) {
  use provider <- result_try_nil(bounded_required(ref.provider))
  use id <- result_try_nil(bounded_required(ref.id))
  Ok(AffectedTaskRef(
    provider: provider,
    id: id,
    display_id: bounded_optional(ref.display_id),
  ))
}

fn result_try_nil(
  result: Result(String, Nil),
  next: fn(String) -> Result(AffectedTaskRef, Nil),
) -> Result(AffectedTaskRef, Nil) {
  case result {
    Ok(value) -> next(value)
    Error(Nil) -> Error(Nil)
  }
}

fn bounded_required(value: String) -> Result(String, Nil) {
  let value = string.trim(value)
  case value == "" || string.length(value) > max_ref_field_length {
    True -> Error(Nil)
    False -> Ok(value)
  }
}

fn bounded_optional(value: Option(String)) -> Option(String) {
  case value {
    None -> None
    Some(value) -> {
      let value = string.trim(value)
      case value == "" || string.length(value) > max_ref_field_length {
        True -> None
        False -> Some(value)
      }
    }
  }
}

fn ref_key(ref: AffectedTaskRef) -> String {
  int.to_string(string.length(ref.provider)) <> ":" <> ref.provider <> ref.id
}
