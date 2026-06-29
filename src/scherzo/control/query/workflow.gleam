import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Gt, Lt}
import gleam/string
import scherzo/config
import scherzo/control/query/types
import scherzo/hash
import scherzo/orchestrator/workflow_reloader
import scherzo/path
import scherzo/runtime_bundle
import scherzo/workflow_dag

pub const max_yaml_source_contents_chars = 65_536

pub fn execute_list(
  state: workflow_reloader.State,
) -> Result(types.QueryResponse, types.QueryError) {
  Ok(types.WorkflowListResponse(workflow_list_from_state(state)))
}

pub fn execute_detail(
  state: workflow_reloader.State,
  query: types.WorkflowDetailQuery,
) -> Result(types.QueryResponse, types.QueryError) {
  let workflow_id = normalize_workflow_id(query.workflow_id)
  case dict.get(state.bundle.workflows, workflow_id) {
    Ok(dag) ->
      Ok(
        types.WorkflowDetailResponse(workflow_detail_from_state(
          state,
          workflow_id,
          dag,
        )),
      )
    Error(Nil) ->
      Error(types.QueryError(
        types.QueryNotFound,
        "workflow not found: " <> query.workflow_id,
      ))
  }
}

pub fn workflow_list_from_state(
  state: workflow_reloader.State,
) -> types.WorkflowListDto {
  let roots = display_roots(state)
  let freshness = freshness_from_state(state)
  let diagnostics = diagnostics_from_state(state, roots)
  let workflows =
    state.bundle.workflows
    |> dict.to_list
    |> list.map(fn(entry) {
      let #(workflow_id, dag) = entry
      summary_from_workflow(state, workflow_id, dag, roots)
    })
    |> list.sort(by: compare_workflow_summary)

  types.WorkflowListDto(
    schema_version: types.workflow_query_schema_version,
    freshness: freshness,
    diagnostics: diagnostics,
    workflows: workflows,
  )
}

pub fn workflow_detail_from_state(
  state: workflow_reloader.State,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
) -> types.WorkflowDetailDto {
  let roots = display_roots(state)
  types.WorkflowDetailDto(
    schema_version: types.workflow_query_schema_version,
    summary: summary_from_workflow(state, workflow_id, dag, roots),
    yaml_sources: yaml_sources_for_workflow(state, workflow_id, roots),
    diagnostics: diagnostics_from_state(state, roots),
    freshness: freshness_from_state(state),
    graph: graph_from_dag(dag),
  )
}

pub fn safe_relative_path(raw_path: String, roots: List(String)) -> String {
  let target = path.absolute_or_original(raw_path)
  case safe_relative_for_roots(target, roots) {
    Some(relative) -> relative
    None -> external_relative_path(raw_path)
  }
}

fn summary_from_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  roots: List(String),
) -> types.WorkflowSummaryDto {
  types.WorkflowSummaryDto(
    id: workflow_id,
    name: workflow_dag.id(dag),
    route: Some(workflow_id),
    label: label_for_workflow(
      state.bundle.orchestrator.routing.workflow_label_prefix,
      workflow_id,
    ),
    yaml_paths: yaml_paths_for_workflow(state, workflow_id, roots),
    step_count: list.length(workflow_dag.steps(dag)),
    status: reload_status_to_string(state.reload_state.current_status),
  )
}

fn yaml_paths_for_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  roots: List(String),
) -> List(String) {
  yaml_sources_for_workflow(state, workflow_id, roots)
  |> list.map(fn(source) { source.path })
}

fn yaml_sources_for_workflow(
  state: workflow_reloader.State,
  workflow_id: String,
  roots: List(String),
) -> List(types.WorkflowYamlSourceDto) {
  workflow_yaml_paths(state, workflow_id)
  |> list.filter_map(fn(source_path) {
    case find_dependency(state.bundle.dependencies, source_path) {
      Some(dependency) ->
        Ok(workflow_yaml_source_from_dependency(dependency, roots))
      None -> Error(Nil)
    }
  })
}

fn workflow_yaml_paths(
  state: workflow_reloader.State,
  workflow_id: String,
) -> List(String) {
  let workflow_path = case
    dict.get(state.bundle.orchestrator.routing.workflows, workflow_id)
  {
    Ok(path) -> [path]
    Error(Nil) -> []
  }
  [state.bundle.config_path, ..workflow_path]
  |> list.filter(is_yaml_path)
  |> dedupe_strings
}

fn find_dependency(
  dependencies: List(runtime_bundle.BundleDependency),
  dependency_path: String,
) -> Option(runtime_bundle.BundleDependency) {
  case dependencies {
    [] -> None
    [dependency, ..rest] ->
      case dependency.path == dependency_path {
        True -> Some(dependency)
        False -> find_dependency(rest, dependency_path)
      }
  }
}

fn workflow_yaml_source_from_dependency(
  dependency: runtime_bundle.BundleDependency,
  roots: List(String),
) -> types.WorkflowYamlSourceDto {
  let redacted_contents = redact_yaml_source_contents(dependency.contents)
  let contents_truncated =
    string.length(redacted_contents) > max_yaml_source_contents_chars
  let contents = case contents_truncated {
    True -> string.slice(redacted_contents, 0, max_yaml_source_contents_chars)
    False -> redacted_contents
  }

  types.WorkflowYamlSourceDto(
    path: safe_relative_path(dependency.path, roots),
    contents: contents,
    contents_sha256: hash.sha256_hex(dependency.contents),
    contents_truncated: contents_truncated,
  )
}

fn redact_yaml_source_contents(contents: String) -> String {
  contents
  |> string.split(on: "\n")
  |> list.map(redact_yaml_source_line)
  |> string.join(with: "\n")
}

fn redact_yaml_source_line(line: String) -> String {
  case string.split_once(line, on: ":") {
    Ok(#(key, _)) ->
      case sensitive_yaml_key(key) {
        True -> key <> ": [REDACTED]"
        False -> line
      }
    Error(Nil) -> line
  }
}

fn sensitive_yaml_key(raw_key: String) -> Bool {
  let key = normalize_yaml_key(raw_key)
  key == "api_key"
  || key == "authorization"
  || key == "password"
  || key == "secret"
  || key == "token"
  || string.ends_with(key, "_api_key")
  || string.ends_with(key, "_password")
  || string.ends_with(key, "_secret")
  || string.ends_with(key, "_token")
}

fn normalize_yaml_key(raw_key: String) -> String {
  let key = raw_key |> string.trim |> string.lowercase
  case string.starts_with(key, "- ") {
    True -> key |> string.drop_start(2) |> string.trim
    False -> key
  }
}

fn freshness_from_state(
  state: workflow_reloader.State,
) -> types.WorkflowFreshnessDto {
  types.WorkflowFreshnessDto(
    source_hash: source_hash(state.bundle.dependencies),
    reload_status: reload_status_to_string(state.reload_state.current_status),
  )
}

fn source_hash(dependencies: List(runtime_bundle.BundleDependency)) -> String {
  dependencies
  |> list.sort(by: compare_dependency_path)
  |> list.map(fn(dependency) {
    dependency.path <> "\n" <> hash.sha256_hex(dependency.contents)
  })
  |> string.join(with: "\n")
  |> hash.sha256_hex
}

fn diagnostics_from_state(
  state: workflow_reloader.State,
  roots: List(String),
) -> List(types.WorkflowDiagnosticDto) {
  case state.reload_state.current_status {
    config.CurrentValid -> []
    config.CurrentInvalid(reason) -> {
      let dependency_diagnostics =
        invalid_dependency_diagnostics(
          state.last_invalid_dependency_snapshot,
          roots,
        )
      [
        types.WorkflowDiagnosticDto(
          severity: "error",
          code: reason,
          message: "workflow reload failed; serving last known good workflows",
          path: None,
        ),
        ..dependency_diagnostics
      ]
    }
  }
}

fn invalid_dependency_diagnostics(
  snapshot: Option(List(workflow_reloader.DependencyRead)),
  roots: List(String),
) -> List(types.WorkflowDiagnosticDto) {
  case snapshot {
    None -> []
    Some(reads) ->
      reads
      |> list.filter_map(fn(read) {
        case read.status {
          workflow_reloader.ReadFailed ->
            Ok(types.WorkflowDiagnosticDto(
              severity: "error",
              code: "dependency_read_failed",
              message: "could not read workflow dependency",
              path: Some(safe_relative_path(read.path, roots)),
            ))
          workflow_reloader.ReadContents(_) -> Error(Nil)
        }
      })
  }
}

fn graph_from_dag(dag: workflow_dag.WorkflowDag) -> types.WorkflowGraphDto {
  types.WorkflowGraphDto(
    nodes: workflow_dag.steps(dag) |> list.map(graph_node_from_step),
    edges: workflow_dag.steps(dag) |> list.flat_map(graph_edges_from_step),
  )
}

fn graph_node_from_step(
  step: workflow_dag.WorkflowStep,
) -> types.WorkflowGraphNodeDto {
  types.WorkflowGraphNodeDto(
    id: step.id,
    label: step.id,
    kind: step_kind_to_string(step.kind),
  )
}

fn graph_edges_from_step(
  step: workflow_dag.WorkflowStep,
) -> List(types.WorkflowGraphEdgeDto) {
  step.depends_on
  |> list.map(fn(dependency) {
    types.WorkflowGraphEdgeDto(from: dependency, to: step.id)
  })
}

fn step_kind_to_string(kind: workflow_dag.StepKind) -> String {
  case kind {
    workflow_dag.AgentStep(_, _) -> "agent"
    workflow_dag.CommandStep(_, _) -> "command"
  }
}

fn display_roots(state: workflow_reloader.State) -> List(String) {
  [
    state.bundle.effective.workspace.root,
    state.bundle.orchestrator.config_dir,
  ]
}

fn safe_relative_for_roots(
  target: String,
  roots: List(String),
) -> Option(String) {
  case roots {
    [] -> None
    [root, ..rest] -> {
      let root_abs = root |> path.absolute_or_original |> trim_trailing_slash
      case path.contains(root_abs, target) {
        True -> {
          let relative = case target == root_abs {
            True -> "."
            False -> string.drop_start(target, string.length(root_abs) + 1)
          }
          case safe_relative_output(relative) {
            True -> Some(relative)
            False -> safe_relative_for_roots(target, rest)
          }
        }
        False -> safe_relative_for_roots(target, rest)
      }
    }
  }
}

fn safe_relative_output(relative: String) -> Bool {
  relative != ""
  && !string.starts_with(relative, "/")
  && !path.has_parent_segment(relative)
  && !path.contains_control_character(relative)
}

fn external_relative_path(raw_path: String) -> String {
  "external/"
  <> hash.short_sha256_hex(raw_path, 12)
  <> "-"
  <> safe_leaf(raw_path)
}

fn safe_leaf(raw_path: String) -> String {
  let leaf = path_leaf(raw_path)
  let sanitized =
    leaf
    |> string.to_graphemes
    |> list.map(fn(ch) {
      case safe_leaf_char(ch) {
        True -> ch
        False -> "-"
      }
    })
    |> string.concat
  case sanitized == "" {
    True -> "file"
    False -> sanitized
  }
}

fn path_leaf(raw_path: String) -> String {
  raw_path
  |> string.replace(each: "\\", with: "/")
  |> string.split(on: "/")
  |> list.filter(fn(part) { part != "" })
  |> last_string("file")
}

fn last_string(values: List(String), fallback: String) -> String {
  case values {
    [] -> fallback
    [value] -> value
    [_, ..rest] -> last_string(rest, fallback)
  }
}

fn safe_leaf_char(ch: String) -> Bool {
  is_between(ch, "a", "z")
  || is_between(ch, "A", "Z")
  || is_between(ch, "0", "9")
  || ch == "."
  || ch == "_"
  || ch == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn label_for_workflow(prefix: String, workflow_id: String) -> Option(String) {
  case prefix == "" {
    True -> None
    False -> Some(prefix <> workflow_id)
  }
}

fn reload_status_to_string(status: config.ReloadStatus) -> String {
  case status {
    config.CurrentValid -> "valid"
    config.CurrentInvalid(_) -> "reload_error"
  }
}

fn normalize_workflow_id(value: String) -> String {
  string.trim(value)
}

fn is_yaml_path(value: String) -> Bool {
  let lower = value |> string.lowercase
  string.ends_with(lower, ".yaml") || string.ends_with(lower, ".yml")
}

fn dedupe_strings(values: List(String)) -> List(String) {
  dedupe_strings_loop(values, []) |> list.reverse
}

fn dedupe_strings_loop(
  values: List(String),
  seen: List(String),
) -> List(String) {
  case values {
    [] -> seen
    [value, ..rest] ->
      case list.contains(seen, value) {
        True -> dedupe_strings_loop(rest, seen)
        False -> dedupe_strings_loop(rest, [value, ..seen])
      }
  }
}

fn compare_workflow_summary(
  left: types.WorkflowSummaryDto,
  right: types.WorkflowSummaryDto,
) -> Order {
  string.compare(left.id, right.id)
}

fn compare_dependency_path(
  left: runtime_bundle.BundleDependency,
  right: runtime_bundle.BundleDependency,
) -> Order {
  string.compare(left.path, right.path)
}

fn trim_trailing_slash(value: String) -> String {
  case value != "/" && string.ends_with(value, "/") {
    True -> string.drop_end(value, 1)
    False -> value
  }
}
