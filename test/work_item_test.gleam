import gleam/dict
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/control/query/work_item_dto
import scherzo/state/projection
import scherzo/state/record
import scherzo/task
import scherzo/work_item
import scherzo/work_item/action
import scherzo/work_item/action_derivation

pub fn daemon_id_generation_and_state_preservation_test() {
  let item = base_task(remote_id: "issue-1", display_id: "LIV-1168")
  let summary = work_item.summary_from_task(item, work_item.default_label_limit)

  assert summary.id == "linear:issue-1"
  assert summary.source.provider == "linear"
  assert summary.source.display_id == Some("LIV-1168")
  assert summary.state.id == Some("todo")
  assert summary.state.name == "Todo"
  assert summary.state.category == task.Ready
}

pub fn label_truncation_test() {
  let labels = build_labels(55, [])
  let item =
    task.Task(
      ..base_task(remote_id: "issue-2", display_id: "LIV-2000"),
      labels: labels,
    )
  let summary = work_item.summary_from_task(item, work_item.default_label_limit)

  assert list.length(summary.labels) == work_item.default_label_limit
  assert summary.labels_truncated
}

pub fn subtask_truncation_test() {
  let subtasks = build_subtasks(12, [])
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "parent-1", display_id: "LIV-P1"),
      subtasks,
      work_item.default_label_limit,
      work_item.default_list_subtask_limit,
    )

  assert list.length(detail.subtasks) == work_item.default_list_subtask_limit
  assert detail.subtasks_truncated
}

pub fn zero_child_parent_test() {
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "parent-2", display_id: "LIV-P2"),
      [],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )

  assert detail.subtasks == []
  assert detail.subtasks_truncated == False
}

pub fn work_item_action_derivation_adds_stable_parent_and_subtask_actions_test() {
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "issue-4", display_id: "LIV-4000"),
      [
        task.Task(
          ..base_task(remote_id: "issue-4-child-active", display_id: "LIV-4001"),
          state: task.TaskState(
            id: Some("started"),
            name: "In Progress",
            category: task.Active,
          ),
        ),
        task.Task(
          ..base_task(remote_id: "issue-4-child-done", display_id: "LIV-4002"),
          state: task.TaskState(
            id: Some("done"),
            name: "Done",
            category: task.Done,
          ),
        ),
      ],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )
    |> action_derivation.detail_with_actions(False)

  let assert [run_workflow] = detail.summary.actions
  assert run_workflow.action_id == action.run_workflow_action_id
  assert run_workflow.enabled == False

  let assert [active_subtask, done_subtask] = detail.subtasks
  let assert [cancel_action, _, _] = active_subtask.actions
  assert cancel_action.action_id == action.cancel_action_id
  assert cancel_action.enabled == False
  let assert Some(cancel_disabled_reason) = cancel_action.disabled_reason
  assert cancel_disabled_reason.code == "cancel_not_enabled"

  let assert [_, review_action, _] = done_subtask.actions
  assert review_action.action_id == action.review_artifacts_action_id
  assert review_action.enabled == False
  let assert Some(review_disabled_reason) = review_action.disabled_reason
  assert review_disabled_reason.code == "artifacts_unavailable"
}

pub fn work_item_action_derivation_enables_review_artifacts_from_projection_test() {
  let detail =
    work_item.detail_from_task_and_subtasks(
      base_task(remote_id: "issue-5", display_id: "LIV-5000"),
      [
        task.Task(
          ..base_task(remote_id: "issue-5-child-done", display_id: "LIV-5001"),
          state: task.TaskState(
            id: Some("done"),
            name: "Done",
            category: task.Done,
          ),
        ),
      ],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )
    |> action_derivation.detail_with_actions_in_projection(
      False,
      projection_state: projection_with_retained_artifacts(
        issue_id: "issue-5-child-done",
        issue_identifier: "LIV-5001",
      ),
    )

  let assert [subtask] = detail.subtasks
  let assert [_, review_action, _] = subtask.actions
  assert review_action.enabled
  let assert None = review_action.disabled_reason
  let assert [first_artifact, ..] = review_action.artifacts
  assert first_artifact.run_id == Some("run-1")
  assert first_artifact.sha256 != ""
}

pub fn work_item_action_descriptor_roundtrip_and_fingerprint_test() {
  let summary =
    work_item.WorkItemSummary(
      ..work_item.summary_from_task(
        base_task(remote_id: "issue-4", display_id: "LIV-4000"),
        work_item.default_label_limit,
      ),
      actions: [
        action.read_only(
          action.review_artifacts_action_id,
          "Review artifacts",
          True,
          None,
          action.ActionTargetSummary(
            kind: "workflow_subtask",
            provider: "linear",
            id: "issue-4",
            display_id: Some("LIV-4000"),
            workflow_id: Some("workflow:execplan-implementation"),
            run_id: Some("run-1"),
          ),
          [
            action.ActionArtifactSummary(
              kind: "publication",
              ref: "artifact://run-1/manifest.json",
              sha256: "abc123",
              bytes: 42,
              display_path: "artifacts/manifest.json",
              run_id: Some("run-1"),
              step_id: Some("publish"),
              publication_id: Some("publication-1"),
            ),
          ],
        ),
      ],
    )
  let assert [expected_action] = summary.actions
  let detail =
    work_item.WorkItemDetail(
      summary: summary,
      subtasks: [],
      subtasks_truncated: False,
    )
  let encoded = work_item_dto.work_item_detail_to_json(detail) |> json.to_string

  assert string.contains(
    encoded,
    "\"action_id\":\"work_subtask.review_artifacts\"",
  )
  assert string.contains(encoded, "\"kind\":\"read_only\"")
  assert string.contains(encoded, "\"fingerprint\":\"")
  assert !string.contains(encoded, "local_path")
  assert !string.contains(encoded, "RAW_PROVIDER_BODY")

  let assert Ok(dynamic_value) = json.parse(encoded, decode.dynamic)
  let assert Ok(decoded) =
    work_item_dto.decode_work_item_detail_dynamic(dynamic_value)
  let assert [decoded_action] = decoded.summary.actions
  assert decoded_action.action_id == action.review_artifacts_action_id
  assert decoded_action.instance_id == expected_action.instance_id
  assert decoded_action.fingerprint == expected_action.fingerprint
}

pub fn work_item_json_excludes_description_and_comment_fields_test() {
  let item =
    task.Task(
      ..base_task(remote_id: "issue-3", display_id: "LIV-3000"),
      description: Some("SECRET_DESCRIPTION"),
    )
  let detail =
    work_item.detail_from_task_and_subtasks(
      item,
      [],
      work_item.default_label_limit,
      work_item.default_show_subtask_limit,
    )
  let encoded = work_item_dto.work_item_detail_to_json(detail) |> json.to_string

  assert string.contains(encoded, "Implement work item projection")
  assert !string.contains(encoded, "description")
  assert !string.contains(encoded, "SECRET_DESCRIPTION")
  assert !string.contains(encoded, "comment")
}

fn base_task(
  remote_id remote_id: String,
  display_id display_id: String,
) -> task.Task {
  task.Task(
    ref: task.TaskRef(
      backend_kind: "linear",
      remote_id: remote_id,
      key: Some(display_id),
      url: Some("https://linear.app/living-systems/issue/" <> display_id),
    ),
    title: "Implement work item projection",
    description: None,
    priority: None,
    state: task.TaskState(id: Some("todo"), name: "Todo", category: task.Ready),
    branch_hint: None,
    labels: [
      task.TaskLabel(id: Some("label-workflow"), name: "workflow:execplan"),
    ],
    blockers: [],
    blockers_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn build_labels(
  remaining: Int,
  acc: List(task.TaskLabel),
) -> List(task.TaskLabel) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      build_labels(remaining - 1, [
        task.TaskLabel(
          id: Some("label-" <> int_to_string(remaining)),
          name: "label:" <> int_to_string(remaining),
        ),
        ..acc
      ])
  }
}

fn build_subtasks(remaining: Int, acc: List(task.Task)) -> List(task.Task) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False ->
      build_subtasks(remaining - 1, [
        base_task(
          remote_id: "child-" <> int_to_string(remaining),
          display_id: "LIV-C" <> int_to_string(remaining),
        ),
        ..acc
      ])
  }
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}

fn projection_with_retained_artifacts(
  issue_id issue_id: String,
  issue_identifier issue_identifier: String,
) -> projection.Projection {
  projection.Projection(
    ..projection.new(),
    workflow_runs: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunFinished(
          workflow_id: "workflow:execplan",
          issue_id: issue_id,
          outcome: "completed",
          token_total: 0,
          turns: 0,
          finished_at_ms: 100,
          run_root: "runs/run-1",
        ),
      ),
    ]),
    workflow_run_provenances: dict.from_list([
      #(
        "run-1",
        projection.WorkflowRunProvenance(
          workflow_id: "workflow:execplan",
          workflow_fingerprint: "wf-1",
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          issue_fingerprint: "issue-fingerprint",
          observed_updated_at_ms: 90,
          run_root: "runs/run-1",
          task_ref: record.linear_task_ref_fields(
            issue_id,
            Some(issue_identifier),
            None,
          ),
        ),
      ),
    ]),
    workflow_output_manifests: dict.from_list([
      #(
        "run-1",
        projection.WorkflowContractManifestRef(
          workflow_id: "workflow:execplan",
          workflow_fingerprint: "wf-1",
          artifact_ref: "artifact://run-1/output.json",
          artifact_sha256: "sha-output",
          artifact_bytes: 128,
          recorded_at_ms: 101,
        ),
      ),
    ]),
    publication_attempts: dict.from_list([
      #("run-1:review_doc", [
        projection.PublicationAttempt(
          run_id: "run-1",
          workflow_id: "workflow:execplan",
          publication_id: "review_doc",
          series_id: "series-1",
          attempt_id: "attempt-1",
          status: "published",
          required: True,
          retryable: False,
          retry_execution_available: False,
          version_id: Some("v1"),
          manifest_ref: Some("artifact://run-1/review-doc.json"),
          manifest_sha256: Some("sha-publication"),
          manifest_bytes: Some(64),
          error_code: None,
          error_message: None,
          recorded_at_ms: 102,
        ),
      ]),
    ]),
  )
}
