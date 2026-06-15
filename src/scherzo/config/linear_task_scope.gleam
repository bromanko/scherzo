import gleam/bit_array
import gleam/int
import gleam/json
import gleam/list
import gleam/string
import scherzo/config/types

pub const max_array_entries = 32

pub const max_predicate_depth = 4

pub const max_predicate_nodes = 64

pub const max_scalar_length = 128

pub const max_issue_filter_payload_bytes = 16_384

pub type Stats {
  Stats(predicate_nodes: Int, max_depth: Int)
}

pub fn project_slugs(scope: types.LinearTaskScope) -> List(String) {
  types.linear_task_scope_project_slugs(scope)
}

pub fn summary(scope: types.LinearTaskScope) -> String {
  case scope {
    types.LinearTaskProject(_) ->
      "project("
      <> first_project_slug(types.linear_task_scope_project_slugs(scope))
      <> ")"
    types.LinearTaskProjects(_) ->
      "projects(["
      <> string.join(types.linear_task_scope_project_slugs(scope), with: ", ")
      <> "])"
    types.LinearTaskAnd(children) ->
      "and(" <> string.join(list.map(children, summary), with: ", ") <> ")"
    types.LinearTaskOr(children) ->
      "or(" <> string.join(list.map(children, summary), with: ", ") <> ")"
  }
}

pub fn stats(scope: types.LinearTaskScope) -> Stats {
  case scope {
    types.LinearTaskProject(_) -> Stats(predicate_nodes: 1, max_depth: 1)
    types.LinearTaskProjects(_) -> Stats(predicate_nodes: 1, max_depth: 1)
    types.LinearTaskAnd(children) | types.LinearTaskOr(children) ->
      composite_stats(children)
  }
}

pub fn matches_project_slug(
  scope: types.LinearTaskScope,
  returned_slug: String,
) -> Bool {
  case scope {
    types.LinearTaskProject(_) | types.LinearTaskProjects(_) ->
      types.linear_task_scope_project_slugs(scope)
      |> list.any(fn(expected_slug) {
        project_slug_matches(expected_slug, returned_slug)
      })
    types.LinearTaskAnd(children) ->
      children
      |> list.all(fn(child) { matches_project_slug(child, returned_slug) })
    types.LinearTaskOr(children) ->
      children
      |> list.any(fn(child) { matches_project_slug(child, returned_slug) })
  }
}

pub fn is_anchored(scope: types.LinearTaskScope) -> Bool {
  case scope {
    types.LinearTaskProject(_) | types.LinearTaskProjects(_) -> True
    types.LinearTaskAnd(children) -> children |> list.any(is_anchored)
    types.LinearTaskOr(children) ->
      children != [] && list.all(children, is_anchored)
  }
}

pub fn issue_filter(scope: types.LinearTaskScope) -> json.Json {
  case scope {
    types.LinearTaskProject(_) ->
      project_slug_filter(
        "eq",
        json.string(
          first_project_slug(types.linear_task_scope_project_slugs(scope)),
        ),
      )
    types.LinearTaskProjects(_) ->
      project_slug_filter(
        "in",
        json.array(
          types.linear_task_scope_project_slugs(scope),
          of: json.string,
        ),
      )
    types.LinearTaskAnd(children) ->
      json.object([#("and", json.array(children, of: issue_filter))])
    types.LinearTaskOr(children) ->
      json.object([#("or", json.array(children, of: issue_filter))])
  }
}

pub fn issue_filter_variables(
  scope: types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  [#(name, issue_filter(scope))]
}

pub fn issue_filter_declaration(name: String) -> String {
  "$" <> name <> ": IssueFilter!"
}

pub fn project_filter(scope: types.LinearTaskScope) -> json.Json {
  case scope {
    types.LinearTaskProject(_) ->
      project_slug_value_filter(
        "eq",
        json.string(
          first_project_slug(types.linear_task_scope_project_slugs(scope)),
        ),
      )
    types.LinearTaskProjects(_) ->
      project_slug_value_filter(
        "in",
        json.array(
          types.linear_task_scope_project_slugs(scope),
          of: json.string,
        ),
      )
    types.LinearTaskAnd(children) ->
      json.object([#("and", json.array(children, of: project_filter))])
    types.LinearTaskOr(children) ->
      json.object([#("or", json.array(children, of: project_filter))])
  }
}

pub fn project_filter_variables(
  scope: types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  [#(name, project_filter(scope))]
}

pub fn project_filter_declaration(name: String) -> String {
  "$" <> name <> ": ProjectFilter!"
}

pub fn configured_project_slug_variables(
  scope: types.LinearTaskScope,
  name: String,
) -> List(#(String, json.Json)) {
  [#(name, json.array(project_slugs(scope), of: json.string))]
}

pub fn matching_project_slugs(scope: types.LinearTaskScope) -> List(String) {
  project_slugs(scope)
  |> list.filter(fn(slug) { matches_project_slug(scope, slug) })
}

pub fn graphql_variables(
  scope: types.LinearTaskScope,
  single_name: String,
  multi_name: String,
) -> List(#(String, json.Json)) {
  case scope {
    types.LinearTaskProject(_) -> [
      #(
        single_name,
        json.string(
          first_project_slug(types.linear_task_scope_project_slugs(scope)),
        ),
      ),
    ]
    types.LinearTaskProjects(_)
    | types.LinearTaskAnd(_)
    | types.LinearTaskOr(_) -> [
      #(
        multi_name,
        json.array(
          types.linear_task_scope_project_slugs(scope),
          of: json.string,
        ),
      ),
    ]
  }
}

pub fn variable_declaration(
  scope: types.LinearTaskScope,
  single_name: String,
  multi_name: String,
) -> String {
  case scope {
    types.LinearTaskProject(_) -> "$" <> single_name <> ": String!"
    types.LinearTaskProjects(_)
    | types.LinearTaskAnd(_)
    | types.LinearTaskOr(_) -> "$" <> multi_name <> ": [String!]!"
  }
}

pub fn contract_project_first(scope: types.LinearTaskScope) -> String {
  case scope {
    types.LinearTaskProject(_) -> "2"
    types.LinearTaskProjects(_) ->
      project_slugs(scope)
      |> list.length
      |> non_zero_count_string
    types.LinearTaskAnd(_) | types.LinearTaskOr(_) ->
      matching_project_slugs(scope)
      |> list.length
      |> non_zero_count_string
  }
}

pub fn contract_configured_project_first(
  scope: types.LinearTaskScope,
) -> String {
  project_slugs(scope)
  |> list.length
  |> non_zero_count_string
}

pub fn issue_filter_payload_bytes(scope: types.LinearTaskScope) -> Int {
  scope
  |> issue_filter
  |> json.to_string
  |> bit_array.from_string
  |> bit_array.byte_size
}

fn first_project_slug(slugs: List(String)) -> String {
  case slugs {
    [slug, ..] -> slug
    [] -> ""
  }
}

fn project_slug_filter(operator: String, operand: json.Json) -> json.Json {
  json.object([#("project", project_slug_value_filter(operator, operand))])
}

fn project_slug_value_filter(
  operator: String,
  operand: json.Json,
) -> json.Json {
  json.object([#("slugId", json.object([#(operator, operand)]))])
}

fn non_zero_count_string(count: Int) -> String {
  int.max(count, 1) |> int.to_string
}

fn project_slug_matches(expected: String, returned: String) -> Bool {
  let expected = string.trim(expected) |> string.lowercase
  let returned = string.trim(returned) |> string.lowercase
  case expected == "" || returned == "" {
    True -> False
    False -> expected == returned || string.ends_with(expected, "-" <> returned)
  }
}

fn composite_stats(children: List(types.LinearTaskScope)) -> Stats {
  let child_stats = list.map(children, stats)
  Stats(
    predicate_nodes: 1 + sum_predicate_nodes(child_stats, 0),
    max_depth: 1 + max_child_depth(child_stats, 0),
  )
}

fn sum_predicate_nodes(stats: List(Stats), acc: Int) -> Int {
  case stats {
    [] -> acc
    [Stats(predicate_nodes: predicate_nodes, ..), ..rest] ->
      sum_predicate_nodes(rest, acc + predicate_nodes)
  }
}

fn max_child_depth(stats: List(Stats), current: Int) -> Int {
  case stats {
    [] -> current
    [Stats(max_depth: max_depth, ..), ..rest] ->
      max_child_depth(rest, int.max(current, max_depth))
  }
}
