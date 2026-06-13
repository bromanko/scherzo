import gleam/option.{None, Some}
import gleam/string
import scherzo/runtime/identity
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import simplifile

fn test_issue(id: String, identifier: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Task title",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: Some("https://example.test/" <> identifier),
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

pub fn task_identity_encodes_backend_and_remote_id_with_lengths_test() {
  let linear =
    task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some("LIV-266"),
      url: None,
    )
  let memory =
    task.TaskRef(
      backend_kind: "test-memory",
      remote_id: "issue-1",
      key: Some("CARD-1"),
      url: None,
    )

  assert identity.to_string(identity.task_ref(linear)) == "6:linear|7:issue-1"
  assert identity.to_string(identity.task_ref(memory))
    == "11:test-memory|7:issue-1"
  assert identity.to_string(identity.task_ref(linear))
    != identity.to_string(identity.task_ref(memory))
}

pub fn task_identity_length_prefixes_prevent_separator_collisions_test() {
  let separator_in_backend =
    task.TaskRef(
      backend_kind: "lin|ear",
      remote_id: "issue-1",
      key: None,
      url: None,
    )
  let separator_in_remote_id =
    task.TaskRef(
      backend_kind: "lin",
      remote_id: "ear|issue-1",
      key: None,
      url: None,
    )

  assert identity.to_string(identity.task_ref(separator_in_backend))
    == "7:lin|ear|7:issue-1"
  assert identity.to_string(identity.task_ref(separator_in_remote_id))
    == "3:lin|11:ear|issue-1"
  assert identity.to_string(identity.task_ref(separator_in_backend))
    != identity.to_string(identity.task_ref(separator_in_remote_id))
}

pub fn issue_identity_helpers_use_issue_id_and_backend_kind_test() {
  let issue = test_issue("issue-1", "LIV-266")
  let linear_identity = identity.issue(issue)
  let memory_identity = identity.issue_for_backend(issue, "test-memory")

  assert identity.to_string(linear_identity) == "6:linear|7:issue-1"
  assert identity.to_string(identity.linear_issue_id("issue-1"))
    == identity.to_string(linear_identity)
  assert identity.to_string(memory_identity) == "11:test-memory|7:issue-1"
  assert identity.to_string(memory_identity)
    != identity.to_string(linear_identity)
}

pub fn non_task_identity_wrappers_round_trip_boundary_strings_test() {
  assert identity.issue_id_to_string(identity.issue_id_from_string("issue-1"))
    == "issue-1"
  assert identity.run_id_to_string(identity.run_id_from_string("run-1"))
    == "run-1"
  assert identity.session_id_to_string(identity.session_id_from_string(
      "session-1",
    ))
    == "session-1"
}

pub fn high_risk_orchestrator_maps_use_task_identity_keys_test() {
  let assert Ok(transition_types_source) =
    simplifile.read("src/scherzo/orchestrator/transition_types.gleam")
  let assert Ok(state_source) =
    simplifile.read("src/scherzo/runtime/state.gleam")

  assert !string.contains(
    transition_types_source,
    "pending_claims: dict.Dict(String, PendingClaim)",
  )
  assert !string.contains(
    transition_types_source,
    "pending_dispatch_validations: dict.Dict(String, PendingDispatchValidation)",
  )
  assert !string.contains(
    transition_types_source,
    "by_issue: dict.Dict(String, WorkerEntry)",
  )
  assert !string.contains(state_source, "running: Dict(String, RunningEntry)")
  assert !string.contains(state_source, "claimed: Dict(String, String)")
}

pub fn orchestrator_append_effects_use_typed_ledger_batches_test() {
  let assert Ok(effect_types_source) =
    simplifile.read("src/scherzo/orchestrator/effects/types.gleam")
  let assert Ok(transition_types_source) =
    simplifile.read("src/scherzo/orchestrator/transition_types.gleam")

  assert !string.contains(
    effect_types_source,
    "bodies: List(record.RecordBody)",
  )
  assert string.contains(effect_types_source, "batch: ledger_batch.LedgerBatch")
  assert !string.contains(
    transition_types_source,
    "HandoffClaimSucceeded(bodies: List(record.RecordBody))",
  )
  assert !string.contains(transition_types_source, "ClaimLedgerAppendRequested")
}
