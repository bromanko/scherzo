import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/linear_task_scope
import scherzo/config/types
import yay

pub type Source {
  ExplicitTasksFrom
  LegacyProject(path: String)
  UnknownSource
}

pub fn source_from_yaml(contents: String) -> Source {
  case yay.parse_string(contents) {
    Ok([document]) -> source_from_root(yay.document_root(document))
    _ -> UnknownSource
  }
}

pub fn source_from_root(root: yay.Node) -> Source {
  let tracker_node = get_map(root, "tracker")
  let linear_node = get_map(tracker_node, "linear")
  case node_has_key(linear_node, "tasks_from") {
    True -> ExplicitTasksFrom
    False -> legacy_source(tracker_node, linear_node)
  }
}

pub fn source_field(source: Source) -> String {
  case source {
    ExplicitTasksFrom -> "tracker.linear.tasks_from"
    LegacyProject(path) -> path
    UnknownSource -> "unknown"
  }
}

pub fn legacy_path(source: Source) -> Option(String) {
  case source {
    LegacyProject(path) -> Some(path)
    ExplicitTasksFrom | UnknownSource -> None
  }
}

pub fn message(
  scope: types.LinearTaskScope,
  source: Source,
  warnings: List(String),
) -> String {
  let summary = linear_task_scope.summary(scope)
  let base = "Linear task scope: " <> summary
  let base = case source {
    LegacyProject(path) ->
      base
      <> ". Legacy "
      <> path
      <> " desugars to tracker.linear.tasks_from: "
      <> summary
    ExplicitTasksFrom | UnknownSource -> base
  }
  case warnings {
    [] -> base
    _ -> base <> ". " <> warning_sentence(warnings)
  }
}

fn warning_sentence(warnings: List(String)) -> String {
  warnings
  |> list.map(fn(warning) { "Overlap warning: " <> warning })
  |> string.join(with: ". ")
}

pub fn overlap_warnings(scope: types.LinearTaskScope) -> List(String) {
  overlap_warnings_loop(scope, []) |> list.reverse
}

fn overlap_warnings_loop(
  scope: types.LinearTaskScope,
  acc: List(String),
) -> List(String) {
  case scope {
    types.LinearTaskProject(_) -> acc
    types.LinearTaskProjects(slugs) ->
      projects_overlap_warning(scope, slugs, acc)
    types.LinearTaskAllLabels(_) | types.LinearTaskAnyLabel(_) -> acc
    types.LinearTaskAnd(children) -> {
      let acc = label_narrowing_overlap_warning(scope, children, acc)
      overlap_warnings_for_children(children, acc)
    }
    types.LinearTaskOr(children) -> {
      let acc = or_overlap_warning(scope, children, acc)
      overlap_warnings_for_children(children, acc)
    }
  }
}

fn projects_overlap_warning(
  scope: types.LinearTaskScope,
  slugs: List(String),
  acc: List(String),
) -> List(String) {
  let project_scopes = project_scope_summaries(scope)
  case list.length(project_scopes) > 1 && list.length(slugs) > 1 {
    True -> [
      "this multi-project scope can overlap daemons configured for "
        <> string.join(project_scopes, with: ", ")
        <> "; run only one daemon per non-overlapping Linear task scope/root",
      ..acc
    ]
    False -> acc
  }
}

fn label_narrowing_overlap_warning(
  scope: types.LinearTaskScope,
  children: List(types.LinearTaskScope),
  acc: List(String),
) -> List(String) {
  case has_label_leaf(children) {
    False -> acc
    True -> {
      let project_scopes = project_scope_summaries(scope)
      case project_scopes {
        [] -> acc
        _ -> [
          "this label-narrowed scope can overlap broader daemons configured for "
            <> string.join(project_scopes, with: ", ")
            <> "; keep label predicates anchored and non-overlapping",
          ..acc
        ]
      }
    }
  }
}

fn or_overlap_warning(
  scope: types.LinearTaskScope,
  children: List(types.LinearTaskScope),
  acc: List(String),
) -> List(String) {
  case children {
    [] | [_] -> acc
    _ -> [
      "this or scope can overlap daemons configured for any branch of "
        <> linear_task_scope.summary(scope)
        <> "; compare canonical task scopes before running multiple daemons",
      ..acc
    ]
  }
}

fn overlap_warnings_for_children(
  children: List(types.LinearTaskScope),
  acc: List(String),
) -> List(String) {
  case children {
    [] -> acc
    [child, ..rest] ->
      overlap_warnings_for_children(rest, overlap_warnings_loop(child, acc))
  }
}

fn has_label_leaf(scopes: List(types.LinearTaskScope)) -> Bool {
  case scopes {
    [] -> False
    [scope, ..rest] -> scope_has_label_leaf(scope) || has_label_leaf(rest)
  }
}

fn scope_has_label_leaf(scope: types.LinearTaskScope) -> Bool {
  case scope {
    types.LinearTaskAllLabels(_) | types.LinearTaskAnyLabel(_) -> True
    types.LinearTaskAnd(children) | types.LinearTaskOr(children) ->
      has_label_leaf(children)
    types.LinearTaskProject(_) | types.LinearTaskProjects(_) -> False
  }
}

fn project_scope_summaries(scope: types.LinearTaskScope) -> List(String) {
  linear_task_scope.project_slugs(scope)
  |> list.map(fn(slug) { "project(" <> slug <> ")" })
}

fn legacy_source(tracker_node: yay.Node, linear_node: yay.Node) -> Source {
  case node_has_key(linear_node, "project") {
    True -> LegacyProject("tracker.linear.project")
    False ->
      case node_has_key(linear_node, "project_slug") {
        True -> LegacyProject("tracker.linear.project_slug")
        False ->
          case node_has_key(tracker_node, "project_slug") {
            True -> LegacyProject("tracker.project_slug")
            False -> UnknownSource
          }
      }
  }
}

fn get_map(node: yay.Node, key: String) -> yay.Node {
  case get_node(node, key) {
    Some(value) -> value
    None -> yay.NodeMap([])
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) -> get_pair(pairs, key)
    _ -> None
  }
}

fn get_pair(
  pairs: List(#(yay.Node, yay.Node)),
  key: String,
) -> Option(yay.Node) {
  case pairs {
    [] -> None
    [#(yay.NodeStr(candidate), value), ..rest] ->
      case candidate == key {
        True -> Some(value)
        False -> get_pair(rest, key)
      }
    [#(_, _), ..rest] -> get_pair(rest, key)
  }
}

fn node_has_key(node: yay.Node, key: String) -> Bool {
  case get_node(node, key) {
    Some(_) -> True
    None -> False
  }
}
