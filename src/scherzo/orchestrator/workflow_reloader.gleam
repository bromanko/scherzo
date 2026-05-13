import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/config/types as config_types
import scherzo/runtime_bundle
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import simplifile

pub type DependencyReadStatus {
  ReadContents(String)
  ReadFailed
}

pub type DependencyRead {
  DependencyRead(path: String, status: DependencyReadStatus)
}

pub type State {
  State(
    workflow_path: Option(String),
    chosen_path: String,
    last_contents: String,
    bundle: runtime_bundle.RuntimeBundle,
    reload_state: config.ReloadState,
    effective: config_types.EffectiveConfig,
    secrets: List(String),
    last_invalid_dependency_snapshot: Option(List(DependencyRead)),
  )
}

pub type Outcome {
  Unchanged(State)
  Reloaded(State)
  Invalid(State, reason: String, message: String)
}

pub fn from_bundle(
  workflow_path: Option(String),
  bundle: runtime_bundle.RuntimeBundle,
) -> State {
  State(
    workflow_path: workflow_path,
    chosen_path: bundle.config_path,
    last_contents: bundle.config_contents,
    bundle: bundle,
    reload_state: config.ReloadState(
      last_known_good: Some(bundle.effective),
      current_status: config.CurrentValid,
    ),
    effective: bundle.effective,
    secrets: bundle.secrets,
    last_invalid_dependency_snapshot: None,
  )
}

pub fn reload_if_changed(state: State) -> Outcome {
  let snapshot =
    probe_dependencies(
      state.bundle.dependencies,
      state.last_invalid_dependency_snapshot,
    )
  case should_suppress_repeated_invalid(state, snapshot) {
    True -> Unchanged(state)
    False ->
      case valid_manifest_unchanged(state, snapshot) {
        True -> Unchanged(state)
        False -> apply_reload_attempt(state, Some(snapshot))
      }
  }
}

pub fn reload_now(state: State) -> Outcome {
  apply_reload_attempt(state, None)
}

pub fn mark_invalid(state: State, reason: String) -> State {
  State(
    ..state,
    reload_state: config.ReloadState(
      last_known_good: Some(state.effective),
      current_status: config.CurrentInvalid(reason),
    ),
  )
}

pub fn select_workflow(
  state: State,
  issue: tracker_issue.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), runtime_bundle.BundleError) {
  runtime_bundle.select_workflow(state.bundle, issue)
}

pub fn invalid_operator_message(outcome: Outcome) -> Option(String) {
  case outcome {
    Invalid(_, _, message) -> Some("workflow reload failed: " <> message)
    _ -> Some("workflow reload failed")
  }
}

pub fn invalid_log_fields(
  reason: String,
  message: String,
) -> List(#(String, String)) {
  [#("error", reason), #("message", message)]
}

fn probe_dependencies(
  dependencies: List(runtime_bundle.BundleDependency),
  last_invalid_dependency_snapshot: Option(List(DependencyRead)),
) -> List(DependencyRead) {
  let dependency_reads =
    dependencies
    |> list.map(fn(dependency) { read_dependency_path(dependency.path) })
  let extra_reads =
    invalid_snapshot_extra_paths(last_invalid_dependency_snapshot, dependencies)
    |> list.map(fn(path) { read_dependency_path(path) })

  list.append(dependency_reads, extra_reads)
  |> normalize_dependency_reads
}

fn read_dependency_path(path: String) -> DependencyRead {
  case simplifile.read(path) {
    Ok(contents) -> DependencyRead(path, ReadContents(contents))
    Error(error) -> dependency_read_failed(path, error)
  }
}

fn invalid_snapshot_extra_paths(
  last_invalid_dependency_snapshot: Option(List(DependencyRead)),
  dependencies: List(runtime_bundle.BundleDependency),
) -> List(String) {
  case last_invalid_dependency_snapshot {
    None -> []
    Some(reads) ->
      reads
      |> list.map(fn(read) { read.path })
      |> list.filter(fn(path) {
        !bundle_has_dependency_path(dependencies, path)
      })
  }
}

fn bundle_has_dependency_path(
  dependencies: List(runtime_bundle.BundleDependency),
  path: String,
) -> Bool {
  case dependencies {
    [] -> False
    [dependency, ..rest] ->
      dependency.path == path || bundle_has_dependency_path(rest, path)
  }
}

fn dependency_read_failed(
  path: String,
  error: simplifile.FileError,
) -> DependencyRead {
  let _description = simplifile.describe_error(error)
  DependencyRead(path, ReadFailed)
}

fn normalize_dependency_reads(
  reads: List(DependencyRead),
) -> List(DependencyRead) {
  reads
  |> list.fold(dict.new(), fn(acc, dependency_read) {
    dict.insert(acc, dependency_read.path, dependency_read.status)
  })
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(path, status) = entry
    DependencyRead(path, status)
  })
  |> list.sort(by: fn(left, right) { string.compare(left.path, right.path) })
}

fn should_suppress_repeated_invalid(
  state: State,
  snapshot: List(DependencyRead),
) -> Bool {
  case
    state.reload_state.current_status,
    state.last_invalid_dependency_snapshot
  {
    config.CurrentInvalid(_), Some(previous) -> previous == snapshot
    _, _ -> False
  }
}

fn valid_manifest_unchanged(
  state: State,
  snapshot: List(DependencyRead),
) -> Bool {
  case state.reload_state.current_status {
    config.CurrentValid ->
      dependencies_match(state.bundle.dependencies, snapshot)
    config.CurrentInvalid(_) -> False
  }
}

fn dependencies_match(
  dependencies: List(runtime_bundle.BundleDependency),
  snapshot: List(DependencyRead),
) -> Bool {
  case dependencies, snapshot {
    [], [] -> True
    [dependency, ..rest_dependencies], [read, ..rest_snapshot] ->
      case read.status {
        ReadContents(contents) ->
          dependency.path == read.path
          && dependency.contents == contents
          && dependencies_match(rest_dependencies, rest_snapshot)
        ReadFailed -> False
      }
    _, _ -> False
  }
}

fn apply_reload_attempt(
  state: State,
  attempted_snapshot: Option(List(DependencyRead)),
) -> Outcome {
  case runtime_bundle.load(Some(state.chosen_path)) {
    Error(runtime_bundle.BundleError(code, message)) -> {
      let state =
        State(
          ..state,
          last_contents: attempted_config_contents(
            attempted_snapshot,
            state.chosen_path,
            state.last_contents,
          ),
          reload_state: config.ReloadState(
            last_known_good: Some(state.effective),
            current_status: config.CurrentInvalid(code),
          ),
          last_invalid_dependency_snapshot: next_invalid_dependency_snapshot(
            state,
            attempted_snapshot,
            code,
            message,
          ),
        )
      Invalid(state, code, message)
    }
    Ok(bundle) -> {
      let effective = bundle.effective
      let state =
        State(
          ..state,
          chosen_path: bundle.config_path,
          last_contents: bundle.config_contents,
          bundle: bundle,
          effective: effective,
          reload_state: config.ReloadState(
            last_known_good: Some(effective),
            current_status: config.CurrentValid,
          ),
          secrets: bundle.secrets,
          last_invalid_dependency_snapshot: None,
        )
      Reloaded(state)
    }
  }
}

fn next_invalid_dependency_snapshot(
  state: State,
  attempted_snapshot: Option(List(DependencyRead)),
  code: String,
  message: String,
) -> Option(List(DependencyRead)) {
  let snapshot = case attempted_snapshot {
    Some(_) -> attempted_snapshot
    None -> state.last_invalid_dependency_snapshot
  }
  case snapshot, missing_file_dependency_path(code, message) {
    None, None -> None
    None, Some(path) -> Some([DependencyRead(path, ReadFailed)])
    Some(snapshot), None -> Some(snapshot)
    Some(snapshot), Some(path) ->
      Some(
        normalize_dependency_reads(
          list.append(snapshot, [
            DependencyRead(path, ReadFailed),
          ]),
        ),
      )
  }
}

fn missing_file_dependency_path(
  code: String,
  message: String,
) -> Option(String) {
  case
    is_missing_file_code(code),
    string.starts_with(message, "could not read ")
  {
    True, True ->
      Some(string.drop_start(message, string.length("could not read ")))
    _, _ -> None
  }
}

fn is_missing_file_code(code: String) -> Bool {
  code == "missing_config_file"
  || code == "missing_workflow_file"
  || code == "missing_prompt_file"
}

fn attempted_config_contents(
  attempted_snapshot: Option(List(DependencyRead)),
  chosen_path: String,
  fallback: String,
) -> String {
  case attempted_snapshot {
    None -> fallback
    Some(snapshot) -> find_read_contents(snapshot, chosen_path, fallback)
  }
}

fn find_read_contents(
  snapshot: List(DependencyRead),
  chosen_path: String,
  fallback: String,
) -> String {
  case snapshot {
    [] -> fallback
    [read, ..rest] ->
      case read.path == chosen_path, read.status {
        True, ReadContents(contents) -> contents
        _, _ -> find_read_contents(rest, chosen_path, fallback)
      }
  }
}
