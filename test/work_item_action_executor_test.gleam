import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/state/projection
import scherzo/task
import scherzo/work_item
import scherzo/work_item/action
import scherzo/work_item/action_derivation
import scherzo/work_item/action_executor
import scherzo/work_item/action_receipts

pub fn action_executor_rejects_stale_disabled_and_conflicting_requests_test() {
  let disabled_request =
    command.WorkItemActionRequest(
      action_id: "work_item.run_workflow",
      action_instance_id: disabled_action().instance_id,
      target_kind: "work_item",
      target_provider: Some("linear"),
      target_id: "issue-1",
      observed_fingerprint: disabled_action().fingerprint,
      idempotency_key: "idem-disabled",
      params: [],
    )
  let disabled_outcome =
    action_executor.execute(action_receipts.empty(), disabled_request, fn(_) {
      Ok(Some(parent_detail()))
    })
  assert disabled_outcome.result.status
    == command.Rejected("run_workflow_not_enabled")

  let active_cancel = active_cancel_action()
  let stale_request =
    command.WorkItemActionRequest(
      action_id: active_cancel.action_id,
      action_instance_id: active_cancel.instance_id,
      target_kind: "workflow_subtask",
      target_provider: Some("linear"),
      target_id: "issue-1-child-active",
      observed_fingerprint: "stale-fingerprint",
      idempotency_key: "idem-stale",
      params: [],
    )
  let stale_outcome =
    action_executor.execute(action_receipts.empty(), stale_request, fn(_) {
      Ok(Some(parent_detail()))
    })
  assert stale_outcome.result.status == command.Rejected("stale_action")

  let first_request =
    command.WorkItemActionRequest(
      action_id: active_cancel.action_id,
      action_instance_id: active_cancel.instance_id,
      target_kind: "workflow_subtask",
      target_provider: Some("linear"),
      target_id: "issue-1-child-active",
      observed_fingerprint: active_cancel.fingerprint,
      idempotency_key: "idem-shared",
      params: [],
    )
  let first_outcome =
    action_executor.execute(action_receipts.empty(), first_request, fn(_) {
      Ok(Some(parent_detail()))
    })
  assert first_outcome.result.status == command.Rejected("cancel_not_enabled")

  let conflicting_request =
    command.WorkItemActionRequest(..first_request, params: [
      #("confirm", "true"),
    ])
  let conflicting_outcome =
    action_executor.execute(first_outcome.receipts, conflicting_request, fn(_) {
      Ok(Some(parent_detail()))
    })
  assert conflicting_outcome.result.status
    == command.Rejected("idempotency_conflict")
}

pub fn action_executor_replays_duplicate_idempotency_requests_test() {
  let active_cancel = active_cancel_action()
  let request =
    command.WorkItemActionRequest(
      action_id: active_cancel.action_id,
      action_instance_id: active_cancel.instance_id,
      target_kind: "workflow_subtask",
      target_provider: Some("linear"),
      target_id: "issue-1-child-active",
      observed_fingerprint: active_cancel.fingerprint,
      idempotency_key: "idem-replay",
      params: [],
    )
  let first =
    action_executor.execute(action_receipts.empty(), request, fn(_) {
      Ok(Some(parent_detail()))
    })
  let second =
    action_executor.execute(first.receipts, request, fn(_) {
      Error(query_types.QueryError(
        query_types.QueryBackendFailed,
        "should not re-run",
      ))
    })

  assert first.result == second.result
}

pub fn action_executor_bounds_receipt_storage_test() {
  let active_cancel = active_cancel_action()
  let receipts =
    receipt_range(0, action_receipts.max_receipts + 5)
    |> list.fold(action_receipts.empty(), fn(receipts, index) {
      let request =
        command.WorkItemActionRequest(
          action_id: active_cancel.action_id,
          action_instance_id: active_cancel.instance_id,
          target_kind: "workflow_subtask",
          target_provider: Some("linear"),
          target_id: "issue-1-child-active",
          observed_fingerprint: active_cancel.fingerprint,
          idempotency_key: "idem-bounded-" <> int.to_string(index),
          params: [],
        )
      action_executor.execute(receipts, request, fn(_) {
        Ok(Some(parent_detail()))
      }).receipts
    })

  assert dict.size(receipts) <= action_receipts.max_receipts
}

pub fn action_executor_revalidates_subtask_when_loaded_as_summary_test() {
  let active_cancel = active_cancel_action()
  let request =
    command.WorkItemActionRequest(
      action_id: active_cancel.action_id,
      action_instance_id: active_cancel.instance_id,
      target_kind: "workflow_subtask",
      target_provider: Some("linear"),
      target_id: "issue-1-child-active",
      observed_fingerprint: active_cancel.fingerprint,
      idempotency_key: "idem-summary-target",
      params: [],
    )
  let outcome =
    action_executor.execute(action_receipts.empty(), request, fn(_) {
      Ok(Some(active_subtask_detail()))
    })

  assert outcome.result.status == command.Rejected("cancel_not_enabled")
}

pub fn action_executor_reports_missing_live_target_as_not_found_test() {
  let active_cancel = active_cancel_action()
  let request =
    command.WorkItemActionRequest(
      action_id: active_cancel.action_id,
      action_instance_id: active_cancel.instance_id,
      target_kind: "workflow_subtask",
      target_provider: Some("linear"),
      target_id: "issue-missing",
      observed_fingerprint: active_cancel.fingerprint,
      idempotency_key: "idem-missing-target",
      params: [],
    )
  let outcome =
    action_executor.execute(action_receipts.empty(), request, fn(_) {
      Ok(Some(parent_detail()))
    })

  assert outcome.result.status == command.NotFound
}

fn parent_detail() -> work_item.WorkItemDetail {
  work_item.detail_from_task_and_subtasks(
    task.Task(
      ref: task.TaskRef(
        backend_kind: "linear",
        remote_id: "issue-1",
        key: Some("LIV-1"),
        url: None,
      ),
      title: "Parent",
      description: None,
      priority: None,
      state: task.TaskState(
        id: Some("todo"),
        name: "Todo",
        category: task.Ready,
      ),
      branch_hint: None,
      labels: [task.TaskLabel(id: None, name: "workflow:execplan")],
      blockers: [],
      blockers_complete: True,
      created_at: None,
      updated_at: None,
    ),
    [
      task.Task(
        ref: task.TaskRef(
          backend_kind: "linear",
          remote_id: "issue-1-child-active",
          key: Some("LIV-1.1"),
          url: None,
        ),
        title: "Active child",
        description: None,
        priority: None,
        state: task.TaskState(
          id: Some("started"),
          name: "In Progress",
          category: task.Active,
        ),
        branch_hint: None,
        labels: [task.TaskLabel(id: None, name: "workflow:execplan")],
        blockers: [],
        blockers_complete: True,
        created_at: None,
        updated_at: None,
      ),
    ],
    work_item.default_label_limit,
    work_item.default_show_subtask_limit,
  )
  |> action_derivation.detail_with_actions(False)
}

fn receipt_range(start: Int, count: Int) -> List(Int) {
  case count <= 0 {
    True -> []
    False -> [start, ..receipt_range(start + 1, count - 1)]
  }
}

fn active_subtask_detail() -> work_item.WorkItemDetail {
  work_item.detail_from_task_and_subtasks(
    task.Task(
      ref: task.TaskRef(
        backend_kind: "linear",
        remote_id: "issue-1-child-active",
        key: Some("LIV-1.1"),
        url: None,
      ),
      title: "Active child",
      description: None,
      priority: None,
      state: task.TaskState(
        id: Some("started"),
        name: "In Progress",
        category: task.Active,
      ),
      branch_hint: None,
      labels: [task.TaskLabel(id: None, name: "workflow:execplan")],
      blockers: [],
      blockers_complete: True,
      created_at: None,
      updated_at: None,
    ),
    [],
    work_item.default_label_limit,
    work_item.default_show_subtask_limit,
  )
  |> action_derivation.detail_for_target_kind_in_projection(
    target_kind: "workflow_subtask",
    dispatch_paused: False,
    projection_state: projection.new(),
  )
}

fn disabled_action() -> action.WorkItemAction {
  let detail = parent_detail()
  let assert [action_value] = detail.summary.actions
  action_value
}

fn active_cancel_action() -> action.WorkItemAction {
  let detail = parent_detail()
  let assert [subtask] = detail.subtasks
  let assert [action_value, ..] = subtask.actions
  action_value
}
