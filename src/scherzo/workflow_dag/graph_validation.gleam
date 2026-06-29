import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type StepNode {
  StepNode(
    id: String,
    depends_on: List(String),
    workspace_name: String,
    workspace_from: Option(String),
  )
}

pub type GraphError {
  GraphError(code: String, message: String)
}

pub fn validate_unique_step_ids(
  steps: List(StepNode),
) -> Result(Nil, GraphError) {
  validate_unique_step_ids_loop(steps, [])
}

fn validate_unique_step_ids_loop(
  steps: List(StepNode),
  seen: List(String),
) -> Result(Nil, GraphError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] ->
      case list.contains(seen, step.id) {
        True ->
          Error(GraphError(
            "duplicate_step_id",
            "duplicate step id: " <> step.id,
          ))
        False -> validate_unique_step_ids_loop(rest, [step.id, ..seen])
      }
  }
}

pub fn validate_dependencies_exist(
  steps: List(StepNode),
) -> Result(Nil, GraphError) {
  let ids = list.map(steps, fn(step) { step.id })
  validate_dependencies_exist_loop(steps, ids)
}

fn validate_dependencies_exist_loop(
  steps: List(StepNode),
  ids: List(String),
) -> Result(Nil, GraphError) {
  case steps {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(validate_dependency_ids(step.depends_on, ids, step.id))
      validate_dependencies_exist_loop(rest, ids)
    }
  }
}

fn validate_dependency_ids(
  deps: List(String),
  ids: List(String),
  step_id: String,
) -> Result(Nil, GraphError) {
  case deps {
    [] -> Ok(Nil)
    [dep, ..rest] ->
      case list.contains(ids, dep) {
        True -> validate_dependency_ids(rest, ids, step_id)
        False ->
          Error(GraphError(
            "missing_dependency",
            step_id <> " depends on unknown step " <> dep,
          ))
      }
  }
}

pub fn validate_acyclic(steps: List(StepNode)) -> Result(Nil, GraphError) {
  validate_acyclic_loop(steps, steps)
}

fn validate_acyclic_loop(
  remaining: List(StepNode),
  all_steps: List(StepNode),
) -> Result(Nil, GraphError) {
  case remaining {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      use _ <- result.try(detect_cycle(step.id, all_steps, []))
      validate_acyclic_loop(rest, all_steps)
    }
  }
}

fn detect_cycle(
  step_id: String,
  all_steps: List(StepNode),
  path: List(String),
) -> Result(Nil, GraphError) {
  case list.contains(path, step_id) {
    True -> Error(GraphError("cycle", "cycle includes step " <> step_id))
    False -> {
      case find_node(all_steps, step_id) {
        None -> Ok(Nil)
        Some(step) ->
          detect_cycle_deps(step.depends_on, all_steps, [step_id, ..path])
      }
    }
  }
}

fn detect_cycle_deps(
  deps: List(String),
  all_steps: List(StepNode),
  path: List(String),
) -> Result(Nil, GraphError) {
  case deps {
    [] -> Ok(Nil)
    [dep, ..rest] -> {
      use _ <- result.try(detect_cycle(dep, all_steps, path))
      detect_cycle_deps(rest, all_steps, path)
    }
  }
}

pub fn validate_workspace_sources(
  steps: List(StepNode),
) -> Result(Nil, GraphError) {
  validate_workspace_sources_loop(steps, steps)
}

fn validate_workspace_sources_loop(
  remaining: List(StepNode),
  all_steps: List(StepNode),
) -> Result(Nil, GraphError) {
  case remaining {
    [] -> Ok(Nil)
    [step, ..rest] -> {
      case step.workspace_from {
        None -> validate_workspace_sources_loop(rest, all_steps)
        Some(source) -> {
          let dep_ids =
            transitive_dependency_ids(step.depends_on, all_steps, [])
          let workspaces = workspace_names_for(dep_ids, all_steps, [])
          case list.contains(workspaces, source) {
            True -> validate_workspace_sources_loop(rest, all_steps)
            False ->
              Error(GraphError(
                "invalid_workspace_from",
                step.id
                  <> " derives from workspace not produced by transitive dependency: "
                  <> source,
              ))
          }
        }
      }
    }
  }
}

fn transitive_dependency_ids(
  deps: List(String),
  all_steps: List(StepNode),
  seen: List(String),
) -> List(String) {
  case deps {
    [] -> seen
    [dep, ..rest] -> {
      case list.contains(seen, dep) {
        True -> transitive_dependency_ids(rest, all_steps, seen)
        False -> {
          let seen = [dep, ..seen]
          let seen = case find_node(all_steps, dep) {
            Some(step) ->
              transitive_dependency_ids(step.depends_on, all_steps, seen)
            None -> seen
          }
          transitive_dependency_ids(rest, all_steps, seen)
        }
      }
    }
  }
}

fn workspace_names_for(
  ids: List(String),
  all_steps: List(StepNode),
  acc: List(String),
) -> List(String) {
  case ids {
    [] -> acc
    [id, ..rest] -> {
      let acc = case find_node(all_steps, id) {
        Some(step) -> [step.workspace_name, ..acc]
        None -> acc
      }
      workspace_names_for(rest, all_steps, acc)
    }
  }
}

pub fn validate_single_terminal_sink(
  steps: List(StepNode),
) -> Result(Nil, GraphError) {
  case terminal_steps(steps) {
    [] -> Ok(Nil)
    [_] -> Ok(Nil)
    sinks -> {
      let ids =
        sinks |> list.map(fn(step) { step.id }) |> string.join(with: ", ")
      Error(GraphError(
        "multiple_terminal_steps",
        "workflow DAG must have exactly one terminal step; terminal steps: "
          <> ids,
      ))
    }
  }
}

fn terminal_steps(steps: List(StepNode)) -> List(StepNode) {
  steps
  |> list.filter(fn(step) { !has_dependent(steps, step.id) })
}

fn has_dependent(steps: List(StepNode), step_id: String) -> Bool {
  case steps {
    [] -> False
    [step, ..rest] ->
      list.contains(step.depends_on, step_id) || has_dependent(rest, step_id)
  }
}

fn find_node(steps: List(StepNode), id: String) -> Option(StepNode) {
  case steps {
    [] -> None
    [step, ..rest] ->
      case step.id == id {
        True -> Some(step)
        False -> find_node(rest, id)
      }
  }
}
