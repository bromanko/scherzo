import gleam/string
import scherzo/config/types

pub const max_array_entries = 32

pub const max_predicate_depth = 4

pub const max_predicate_nodes = 64

pub const max_scalar_length = 128

pub type Stats {
  Stats(predicate_nodes: Int, max_depth: Int)
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
  }
}

pub fn stats(scope: types.LinearTaskScope) -> Stats {
  case scope {
    types.LinearTaskProject(_) -> Stats(predicate_nodes: 1, max_depth: 1)
    types.LinearTaskProjects(_) -> Stats(predicate_nodes: 1, max_depth: 1)
  }
}

fn first_project_slug(slugs: List(String)) -> String {
  case slugs {
    [slug, ..] -> slug
    [] -> ""
  }
}
