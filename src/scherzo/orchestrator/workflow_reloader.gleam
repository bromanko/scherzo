import gleam/option.{type Option, Some}
import scherzo/config
import scherzo/domain
import scherzo/runtime_bundle
import scherzo/workflow_dag
import simplifile

pub type State {
  State(
    workflow_path: Option(String),
    chosen_path: String,
    last_contents: String,
    bundle: runtime_bundle.RuntimeBundle,
    reload_state: config.ReloadState,
    effective: domain.EffectiveConfig,
    secrets: List(String),
  )
}

pub type Outcome {
  Unchanged(State)
  Reloaded(State)
  Invalid(State, reason: String)
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
    secrets: config.resolved_secrets(bundle.effective),
  )
}

pub fn reload_if_changed(state: State) -> Outcome {
  case simplifile.read(state.chosen_path) {
    Error(_) ->
      Invalid(
        mark_invalid(state, "missing_workflow_file"),
        "missing_workflow_file",
      )
    Ok(contents) ->
      case contents == state.last_contents {
        True -> Unchanged(state)
        False -> apply_new_contents(state, contents)
      }
  }
}

pub fn reload_now(state: State) -> Outcome {
  case simplifile.read(state.chosen_path) {
    Error(_) ->
      Invalid(
        mark_invalid(state, "missing_workflow_file"),
        "missing_workflow_file",
      )
    Ok(contents) -> apply_new_contents(state, contents)
  }
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
  issue: domain.Issue,
) -> Result(#(String, workflow_dag.WorkflowDag), runtime_bundle.BundleError) {
  runtime_bundle.select_workflow(state.bundle, issue)
}

fn apply_new_contents(state: State, contents: String) -> Outcome {
  case runtime_bundle.load(Some(state.chosen_path)) {
    Error(runtime_bundle.BundleError(code, _)) -> {
      let state =
        State(
          ..state,
          last_contents: contents,
          reload_state: config.ReloadState(
            last_known_good: Some(state.effective),
            current_status: config.CurrentInvalid(code),
          ),
        )
      Invalid(state, code)
    }
    Ok(bundle) -> {
      let effective = bundle.effective
      let state =
        State(
          ..state,
          last_contents: contents,
          bundle: bundle,
          effective: effective,
          reload_state: config.ReloadState(
            last_known_good: Some(effective),
            current_status: config.CurrentValid,
          ),
          secrets: config.resolved_secrets(effective),
        )
      Reloaded(state)
    }
  }
}
