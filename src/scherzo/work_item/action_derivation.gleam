import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq, Gt, Lt}
import gleam/string
import scherzo/state/projection
import scherzo/task
import scherzo/work_item
import scherzo/work_item/action

pub fn page_with_actions(
  page: work_item.WorkItemProviderPage,
  dispatch_paused dispatch_paused: Bool,
) -> work_item.WorkItemProviderPage {
  page_with_actions_in_projection(
    page,
    dispatch_paused,
    projection_state: projection.new(),
  )
}

pub fn page_with_actions_in_projection(
  page: work_item.WorkItemProviderPage,
  dispatch_paused dispatch_paused: Bool,
  projection_state projection_state: projection.Projection,
) -> work_item.WorkItemProviderPage {
  work_item.WorkItemProviderPage(
    items: list.map(page.items, fn(item) {
      case item.parent {
        Some(_) -> subtask_with_actions_in_projection(item, projection_state)
        None ->
          summary_with_actions_in_projection(
            item,
            dispatch_paused,
            projection_state: projection_state,
          )
      }
    }),
    has_more: page.has_more,
  )
}

pub fn detail_with_actions(
  detail: work_item.WorkItemDetail,
  dispatch_paused dispatch_paused: Bool,
) -> work_item.WorkItemDetail {
  detail_with_actions_in_projection(
    detail,
    dispatch_paused,
    projection_state: projection.new(),
  )
}

pub fn detail_for_target_kind_in_projection(
  detail: work_item.WorkItemDetail,
  target_kind target_kind: String,
  dispatch_paused dispatch_paused: Bool,
  projection_state projection_state: projection.Projection,
) -> work_item.WorkItemDetail {
  case target_kind {
    "workflow_subtask" ->
      work_item.WorkItemDetail(
        summary: subtask_with_actions_in_projection(
          detail.summary,
          projection_state,
        ),
        subtasks: list.map(detail.subtasks, fn(subtask) {
          subtask_with_actions_in_projection(subtask, projection_state)
        }),
        subtasks_truncated: detail.subtasks_truncated,
      )
    _ ->
      detail_with_actions_in_projection(
        detail,
        dispatch_paused,
        projection_state: projection_state,
      )
  }
}

pub fn detail_with_actions_in_projection(
  detail: work_item.WorkItemDetail,
  dispatch_paused dispatch_paused: Bool,
  projection_state projection_state: projection.Projection,
) -> work_item.WorkItemDetail {
  work_item.WorkItemDetail(
    summary: summary_with_actions_in_projection(
      detail.summary,
      dispatch_paused,
      projection_state: projection_state,
    ),
    subtasks: list.map(detail.subtasks, fn(subtask) {
      subtask_with_actions_in_projection(subtask, projection_state)
    }),
    subtasks_truncated: detail.subtasks_truncated,
  )
}

pub fn summary_with_actions(
  summary: work_item.WorkItemSummary,
  dispatch_paused dispatch_paused: Bool,
) -> work_item.WorkItemSummary {
  summary_with_actions_in_projection(
    summary,
    dispatch_paused,
    projection_state: projection.new(),
  )
}

pub fn summary_with_actions_in_projection(
  summary: work_item.WorkItemSummary,
  dispatch_paused dispatch_paused: Bool,
  projection_state _projection_state: projection.Projection,
) -> work_item.WorkItemSummary {
  work_item.WorkItemSummary(..summary, actions: [
    run_workflow_action(summary, dispatch_paused),
  ])
}

pub fn subtask_with_actions(
  summary: work_item.WorkItemSummary,
) -> work_item.WorkItemSummary {
  subtask_with_actions_in_projection(summary, projection.new())
}

pub fn subtask_with_actions_in_projection(
  summary: work_item.WorkItemSummary,
  projection_state: projection.Projection,
) -> work_item.WorkItemSummary {
  let latest_run_id = latest_run_id(summary, projection_state)
  let artifacts = retained_artifacts(summary, projection_state)

  work_item.WorkItemSummary(..summary, actions: [
    cancel_action(summary, latest_run_id),
    review_artifacts_action(summary, latest_run_id, artifacts),
    fix_retry_action(summary, latest_run_id),
  ])
}

fn run_workflow_action(
  summary: work_item.WorkItemSummary,
  dispatch_paused: Bool,
) -> action.WorkItemAction {
  let disabled_reason = case
    dispatch_paused,
    is_terminal(summary.state.category)
  {
    True, _ ->
      Some(action.ActionDisabledReason(
        code: "dispatch_paused",
        message: "Dispatch is paused",
      ))
    False, True ->
      Some(action.ActionDisabledReason(
        code: "parent_terminal",
        message: "Workflow cannot be started from a terminal work item",
      ))
    False, False ->
      Some(action.ActionDisabledReason(
        code: "run_workflow_not_enabled",
        message: "Run workflow is not enabled yet",
      ))
  }

  action.mutating(
    action.run_workflow_action_id,
    "Run workflow",
    False,
    disabled_reason,
    target(summary, "work_item", None),
  )
}

fn cancel_action(
  summary: work_item.WorkItemSummary,
  run_id: Option(String),
) -> action.WorkItemAction {
  action.mutating(
    action.cancel_action_id,
    "Cancel",
    False,
    Some(action.ActionDisabledReason(
      code: "cancel_not_enabled",
      message: "Cancel is not enabled for workflow subtasks yet",
    )),
    target(summary, "workflow_subtask", run_id),
  )
}

fn review_artifacts_action(
  summary: work_item.WorkItemSummary,
  run_id: Option(String),
  artifacts: List(action.ActionArtifactSummary),
) -> action.WorkItemAction {
  let enabled = artifacts != []
  action.read_only(
    action.review_artifacts_action_id,
    "Review artifacts",
    enabled,
    case enabled {
      True -> None
      False ->
        Some(action.ActionDisabledReason(
          code: "artifacts_unavailable",
          message: "Retained artifacts are not available for this workflow subtask",
        ))
    },
    target(summary, "workflow_subtask", run_id),
    artifacts,
  )
}

fn fix_retry_action(
  summary: work_item.WorkItemSummary,
  run_id: Option(String),
) -> action.WorkItemAction {
  action.mutating(
    action.fix_retry_action_id,
    "Fix and retry",
    False,
    Some(action.ActionDisabledReason(
      code: "fix_retry_not_enabled",
      message: "Fix and retry is not enabled for workflow subtasks yet",
    )),
    target(summary, "workflow_subtask", run_id),
  )
}

fn retained_artifacts(
  summary: work_item.WorkItemSummary,
  projection_state: projection.Projection,
) -> List(action.ActionArtifactSummary) {
  relevant_run_ids(summary, projection_state)
  |> list.flat_map(fn(run_id) { artifacts_for_run(run_id, projection_state) })
}

fn artifacts_for_run(
  run_id: String,
  projection_state: projection.Projection,
) -> List(action.ActionArtifactSummary) {
  []
  |> append_workflow_manifest_artifact(
    projection.workflow_input_manifest(projection_state, run_id),
    "workflow_input",
    run_id,
  )
  |> append_workflow_manifest_artifact(
    projection.workflow_output_manifest(projection_state, run_id),
    "workflow_output",
    run_id,
  )
  |> list.append(step_artifacts_for_run(run_id, projection_state))
  |> list.append(publication_artifacts_for_run(run_id, projection_state))
}

fn append_workflow_manifest_artifact(
  artifacts: List(action.ActionArtifactSummary),
  manifest: Option(projection.WorkflowContractManifestRef),
  kind: String,
  run_id: String,
) -> List(action.ActionArtifactSummary) {
  case manifest {
    Some(manifest) -> [
      action.ActionArtifactSummary(
        kind: kind,
        ref: manifest.artifact_ref,
        sha256: manifest.artifact_sha256,
        bytes: manifest.artifact_bytes,
        display_path: manifest.artifact_ref,
        run_id: Some(run_id),
        step_id: None,
        publication_id: None,
      ),
      ..artifacts
    ]
    None -> artifacts
  }
}

fn step_artifacts_for_run(
  run_id: String,
  projection_state: projection.Projection,
) -> List(action.ActionArtifactSummary) {
  projection_state.step_attempts
  |> dict.values
  |> list.filter_map(fn(status) {
    case status {
      projection.StepAttemptFinishedStatus(
        run_id: status_run_id,
        step_id: step_id,
        artifact_ref: artifact_ref,
        artifact_sha256: artifact_sha256,
        ..,
      ) ->
        case status_run_id == run_id {
          True ->
            Ok(action.ActionArtifactSummary(
              kind: "step_output",
              ref: artifact_ref,
              sha256: artifact_sha256,
              bytes: 0,
              display_path: artifact_ref,
              run_id: Some(run_id),
              step_id: Some(step_id),
              publication_id: None,
            ))
          False -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> list.sort(by: compare_artifacts)
}

fn publication_artifacts_for_run(
  run_id: String,
  projection_state: projection.Projection,
) -> List(action.ActionArtifactSummary) {
  projection.publication_ids_for_run(projection_state, run_id)
  |> list.filter_map(fn(publication_id) {
    case
      projection.latest_publication_for_run(
        projection_state,
        run_id,
        publication_id,
      )
    {
      Ok(attempt) ->
        case
          attempt.manifest_ref,
          attempt.manifest_sha256,
          attempt.manifest_bytes
        {
          Some(manifest_ref), Some(manifest_sha256), Some(manifest_bytes) ->
            Ok(action.ActionArtifactSummary(
              kind: "publication",
              ref: manifest_ref,
              sha256: manifest_sha256,
              bytes: manifest_bytes,
              display_path: manifest_ref,
              run_id: Some(run_id),
              step_id: None,
              publication_id: Some(publication_id),
            ))
          _, _, _ -> Error(Nil)
        }
      Error(Nil) -> Error(Nil)
    }
  })
  |> list.sort(by: compare_artifacts)
}

fn relevant_run_ids(
  summary: work_item.WorkItemSummary,
  projection_state: projection.Projection,
) -> List(String) {
  let issue_id = summary.source.id
  let expected_workflow_id = workflow_id(summary.labels)

  projection_state.workflow_runs
  |> dict.to_list
  |> list.filter(fn(entry) {
    let #(run_id, status) = entry
    run_matches_issue(
      projection_state,
      run_id,
      status,
      issue_id,
      expected_workflow_id,
    )
  })
  |> list.sort(by: compare_runs_desc)
  |> list.map(fn(entry) {
    let #(run_id, _) = entry
    run_id
  })
}

fn latest_run_id(
  summary: work_item.WorkItemSummary,
  projection_state: projection.Projection,
) -> Option(String) {
  case relevant_run_ids(summary, projection_state) {
    [run_id, ..] -> Some(run_id)
    [] -> None
  }
}

fn run_matches_issue(
  projection_state: projection.Projection,
  run_id: String,
  status: projection.WorkflowRunStatus,
  issue_id: String,
  expected_workflow_id: Option(String),
) -> Bool {
  let workflow_matches = case expected_workflow_id {
    Some(expected) -> workflow_id_for_status(status) == expected
    None -> True
  }

  workflow_matches
  && case projection.workflow_run_provenance(projection_state, run_id) {
    Ok(provenance) -> provenance.issue_id == issue_id
    Error(Nil) -> issue_id_for_status(status) == issue_id
  }
}

fn workflow_id_for_status(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(workflow_id: workflow_id, ..)
    | projection.WorkflowRunFinished(workflow_id: workflow_id, ..)
    | projection.WorkflowRunInterrupted(workflow_id: workflow_id, ..)
    | projection.WorkflowRunSuperseded(workflow_id: workflow_id, ..) ->
      workflow_id
  }
}

fn issue_id_for_status(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(issue_id: issue_id, ..)
    | projection.WorkflowRunFinished(issue_id: issue_id, ..)
    | projection.WorkflowRunInterrupted(issue_id: issue_id, ..)
    | projection.WorkflowRunSuperseded(issue_id: issue_id, ..) -> issue_id
  }
}

fn run_recorded_at_ms(status: projection.WorkflowRunStatus) -> Int {
  case status {
    projection.WorkflowRunActive(started_at_ms: at_ms, ..) -> at_ms
    projection.WorkflowRunFinished(finished_at_ms: at_ms, ..) -> at_ms
    projection.WorkflowRunInterrupted(interrupted_at_ms: at_ms, ..) -> at_ms
    projection.WorkflowRunSuperseded(superseded_at_ms: at_ms, ..) -> at_ms
  }
}

fn compare_runs_desc(
  a: #(String, projection.WorkflowRunStatus),
  b: #(String, projection.WorkflowRunStatus),
) -> Order {
  let #(_, a_status) = a
  let #(_, b_status) = b
  case int.compare(run_recorded_at_ms(a_status), run_recorded_at_ms(b_status)) {
    Eq -> string.compare(run_id_from_entry(a), run_id_from_entry(b))
    Lt -> Gt
    Gt -> Lt
  }
}

fn run_id_from_entry(entry: #(String, projection.WorkflowRunStatus)) -> String {
  let #(run_id, _) = entry
  run_id
}

fn compare_artifacts(
  a: action.ActionArtifactSummary,
  b: action.ActionArtifactSummary,
) -> Order {
  case string.compare(a.kind, b.kind) {
    Eq ->
      case string.compare(a.ref, b.ref) {
        Eq -> string.compare(option_string(a.step_id), option_string(b.step_id))
        order -> order
      }
    order -> order
  }
}

fn target(
  summary: work_item.WorkItemSummary,
  kind: String,
  run_id: Option(String),
) -> action.ActionTargetSummary {
  action.ActionTargetSummary(
    kind: kind,
    provider: summary.source.provider,
    id: summary.source.id,
    display_id: summary.source.display_id,
    workflow_id: workflow_id(summary.labels),
    run_id: run_id,
  )
}

fn workflow_id(labels: List(task.TaskLabel)) -> Option(String) {
  case list.find(labels, fn(label) { starts_with_workflow_label(label.name) }) {
    Ok(label) -> Some(label.name)
    Error(Nil) -> None
  }
}

fn starts_with_workflow_label(value: String) -> Bool {
  string.starts_with(value, "workflow:")
}

fn is_terminal(category: task.TaskStateCategory) -> Bool {
  case category {
    task.Done | task.Canceled | task.Duplicate -> True
    _ -> False
  }
}

fn option_string(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}
