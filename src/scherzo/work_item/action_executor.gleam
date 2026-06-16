import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/work_item
import scherzo/work_item/action
import scherzo/work_item/action_receipts

pub type Outcome {
  Outcome(
    result: command.CommandResult,
    receipts: Dict(String, action_receipts.Receipt),
  )
}

type LocateActionError {
  TargetNotFound
  UnsupportedAction
}

pub fn execute(
  receipts: Dict(String, action_receipts.Receipt),
  request: command.WorkItemActionRequest,
  load_target: fn(command.WorkItemActionRequest) ->
    Result(Option(work_item.WorkItemDetail), query_types.QueryError),
) -> Outcome {
  let operator_command = command.WorkItemAction(request)
  let receipt_key = action_receipts.receipt_key(request)
  let payload_hash = action_receipts.payload_hash(request)

  case dict.get(receipts, receipt_key) {
    Ok(action_receipts.Receipt(payload_hash: stored_hash, result: stored_result)) ->
      case stored_hash == payload_hash {
        True -> Outcome(result: stored_result, receipts: receipts)
        False ->
          Outcome(
            result: command.rejected(
              operator_command,
              "idempotency_conflict",
              Some(
                "idempotency key was already used for a different work item action payload",
              ),
            ),
            receipts: receipts,
          )
      }
    Error(Nil) ->
      execute_fresh(receipts, request, operator_command, load_target)
  }
}

fn execute_fresh(
  receipts: Dict(String, action_receipts.Receipt),
  request: command.WorkItemActionRequest,
  operator_command: command.OperatorCommand,
  load_target: fn(command.WorkItemActionRequest) ->
    Result(Option(work_item.WorkItemDetail), query_types.QueryError),
) -> Outcome {
  let result = case load_target(request) {
    Ok(Some(detail)) ->
      result_for_live_detail(detail, request, operator_command)
    Ok(None) -> command.not_found(operator_command, Some("work item not found"))
    Error(query_types.QueryError(code: query_types.QueryNotFound, ..)) ->
      command.not_found(operator_command, Some("work item not found"))
    Error(query_types.QueryError(code: code, message: message)) ->
      command.rejected(
        operator_command,
        query_types.error_code_to_string(code),
        Some(message),
      )
  }

  Outcome(
    result: result,
    receipts: action_receipts.store(receipts, request, result),
  )
}

fn result_for_live_detail(
  detail: work_item.WorkItemDetail,
  request: command.WorkItemActionRequest,
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  case locate_action(detail, request) {
    Ok(found_action) ->
      result_for_action(found_action, request, operator_command)
    Error(TargetNotFound) ->
      command.not_found(operator_command, Some("work item not found"))
    Error(UnsupportedAction) ->
      command.rejected(
        operator_command,
        locate_action_error_code(UnsupportedAction),
        Some(action_lookup_message(UnsupportedAction)),
      )
  }
}

fn locate_action(
  detail: work_item.WorkItemDetail,
  request: command.WorkItemActionRequest,
) -> Result(action.WorkItemAction, LocateActionError) {
  case request.target_kind {
    "work_item" ->
      case matches_target(detail.summary, request) {
        True -> find_action(detail.summary.actions, request.action_id)
        False -> Error(TargetNotFound)
      }
    "workflow_subtask" -> locate_subtask_action(detail, request)
    _ -> Error(UnsupportedAction)
  }
}

fn locate_subtask_action(
  detail: work_item.WorkItemDetail,
  request: command.WorkItemActionRequest,
) -> Result(action.WorkItemAction, LocateActionError) {
  case matches_target(detail.summary, request) {
    True -> find_action(detail.summary.actions, request.action_id)
    False -> {
      let matching_subtasks =
        detail.subtasks
        |> list.filter(fn(item) { matches_target(item, request) })
      case matching_subtasks {
        [] -> Error(TargetNotFound)
        _ ->
          matching_subtasks
          |> list.flat_map(fn(item) { item.actions })
          |> find_action(request.action_id)
      }
    }
  }
}

fn find_action(
  actions: List(action.WorkItemAction),
  action_id: String,
) -> Result(action.WorkItemAction, LocateActionError) {
  case list.find(actions, fn(item) { item.action_id == action_id }) {
    Ok(found) -> Ok(found)
    Error(Nil) -> Error(UnsupportedAction)
  }
}

fn matches_target(
  summary: work_item.WorkItemSummary,
  request: command.WorkItemActionRequest,
) -> Bool {
  summary.source.id == request.target_id
  && case request.target_provider {
    Some(provider) -> summary.source.provider == provider
    None -> True
  }
}

fn result_for_action(
  found_action: action.WorkItemAction,
  request: command.WorkItemActionRequest,
  operator_command: command.OperatorCommand,
) -> command.CommandResult {
  case
    found_action.instance_id != request.action_instance_id
    || found_action.fingerprint != request.observed_fingerprint
  {
    True ->
      command.rejected(
        operator_command,
        "stale_action",
        Some("work item action state changed; refresh and try again"),
      )
    False ->
      case found_action.enabled {
        False ->
          case found_action.disabled_reason {
            Some(reason) ->
              command.rejected(
                operator_command,
                reason.code,
                Some(reason.message),
              )
            None ->
              command.rejected(
                operator_command,
                "action_disabled",
                Some("work item action is disabled"),
              )
          }
        True ->
          case found_action.kind {
            action.ReadOnly ->
              command.not_allowed(
                operator_command,
                "read_only_action",
                Some(
                  "read-only work item actions must be handled by query consumers",
                ),
              )
            action.Mutating ->
              command.not_allowed(
                operator_command,
                "action_not_implemented",
                Some(
                  "work item action transport is available but this action is not implemented yet",
                ),
              )
          }
      }
  }
}

fn locate_action_error_code(error: LocateActionError) -> String {
  case error {
    TargetNotFound -> "target_not_found"
    UnsupportedAction -> "unsupported_action"
  }
}

fn action_lookup_message(error: LocateActionError) -> String {
  case error {
    TargetNotFound -> "work item not found"
    UnsupportedAction ->
      "work item action is not available for the current target"
  }
}
