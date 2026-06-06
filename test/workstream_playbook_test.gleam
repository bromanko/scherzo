import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/projection as state_projection
import scherzo/state/record
import scherzo/workstream/artifact_store
import scherzo/workstream/artifacts
import scherzo/workstream/ledger
import scherzo/workstream/playbook
import scherzo/workstream/playbook_eval
import scherzo/workstream/start_key
import scherzo/workstream/types
import simplifile
import support/test_helpers

const workstream_id = "linear:LIV-464"

const output_sha = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const output_ref = "workstream-artifacts/sha256/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json"

const bundle_ref = "workstream-artifacts/sha256/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb.json"

const code_sha = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const code_ref = "workstream-artifacts/sha256/cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc.json"

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn recommendation_action(
  actions: List(playbook.PlaybookAction),
  action_id: String,
) -> playbook.PlaybookAction {
  let assert Ok(action) =
    list.find(actions, fn(action) { action.action_id == action_id })
  action
}

fn recommendation_by_action(
  recommendations: List(playbook.Recommendation),
  action_id: String,
) -> playbook.Recommendation {
  let assert Ok(recommendation) =
    list.find(recommendations, fn(recommendation) {
      recommendation.action_id == action_id
    })
  recommendation
}

pub fn checked_in_playbooks_parse_test() {
  let assert Ok(standard) =
    playbook.parse(read_file(".scherzo/playbooks/standard-implementation.yaml"))
  let assert Ok(extended) =
    playbook.parse(read_file(".scherzo/playbooks/extended-implementation.yaml"))

  assert standard.id == "standard-implementation"
  assert standard.auto_enqueue.enabled == False
  assert list.length(standard.phases) == 3
  assert list.length(standard.next_actions) == 2
  assert extended.id == "extended-implementation"
  assert list.length(extended.phases) > list.length(standard.phases)
  assert list.any(extended.next_actions, fn(action) {
    action.action_id == "security_review"
  })
  let publish = recommendation_action(extended.next_actions, "publish_change")
  assert list.contains(publish.required_inputs, "security_review_report")
  assert list.contains(publish.required_inputs, "performance_review_report")
}

pub fn playbook_parser_rejects_unknown_required_input_test() {
  let source =
    read_file(".scherzo/playbooks/standard-implementation.yaml")
    |> string.replace(
      each: "      - exec_plan_bundle",
      with: "      - missing_bundle",
    )

  let assert Error(error) = playbook.parse(source)
  assert playbook.error_code(error) == "playbook_required_input_unknown"
}

pub fn playbook_parser_rejects_duplicate_action_id_test() {
  let source =
    read_file(".scherzo/playbooks/standard-implementation.yaml")
    |> string.replace(
      each: "  - action_id: publish_change",
      with: "  - action_id: implement_exec_plan",
    )

  let assert Error(error) = playbook.parse(source)
  assert playbook.error_code(error) == "playbook_action_duplicate"
}

pub fn suggest_only_playbook_recommends_without_enqueue_test() {
  let root = "test/tmp/workstream-playbook/suggest-only"
  let #(store, status) = status_with_decision(root, "approve", [])
  let playbook =
    implementation_playbook(auto_enabled: False, action_auto: False)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.suggest_only_policy(),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "available"
  assert recommendation.gate_status == "approved"
  assert recommendation.auto_enqueue_status == "not_requested"
  assert recommendation.blocked_reasons == []
  let assert Some(idempotency_key) = recommendation.idempotency_key
  assert idempotency_key
    == start_key.derive_idempotency_key(
      workstream_id,
      "implement_exec_plan",
      [#("exec_plan_bundle", output_sha)],
      ["decision-approve"],
    )

  let next_action =
    playbook.recommendation_to_next_action_artifact(
      workstream_id,
      recommendation,
    )
  assert next_action.state == "available"
  assert next_action.auto_enqueue == False
  assert artifacts.decode_next_action(artifacts.next_action_to_string(
      next_action,
    ))
    == Ok(next_action)
}

pub fn checked_in_standard_playbook_recommends_implementation_test() {
  let root = "test/tmp/workstream-playbook/standard-yaml"
  let #(store, status) = status_with_decision(root, "approve", [])
  let assert Ok(standard) =
    playbook.parse(read_file(".scherzo/playbooks/standard-implementation.yaml"))
  let evaluation =
    playbook_eval.evaluate(
      standard,
      status,
      store,
      playbook.suggest_only_policy(),
    )

  let implement_recommendation =
    recommendation_by_action(evaluation.recommendations, "implement_exec_plan")
  let publish =
    recommendation_by_action(evaluation.recommendations, "publish_change")
  assert implement_recommendation.state == "available"
  assert implement_recommendation.gate_status == "approved"
  assert publish.state == "blocked"
  assert list.contains(publish.missing_inputs, "code_change_bundle")
}

pub fn auto_enqueue_refuses_unsatisfied_gate_test() {
  let root = "test/tmp/workstream-playbook/missing-gate"
  let #(store, status) = status_without_decision(root)
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.gate_status == "pending"
  assert recommendation.idempotency_key == None
  assert recommendation.auto_enqueue_status == "blocked"
  assert list.contains(
    recommendation.blocked_reasons,
    "gate_pending:human_review",
  )
}

pub fn auto_enqueue_requires_policy_and_detects_duplicate_key_test() {
  let root = "test/tmp/workstream-playbook/auto-ready"
  let #(store, status) = status_with_decision(root, "approve", [])
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)

  let disabled_evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.suggest_only_policy(),
    )
  let assert [disabled_recommendation] = disabled_evaluation.recommendations
  assert disabled_recommendation.state == "available"
  assert disabled_recommendation.auto_enqueue_status == "disabled_by_policy"

  let enabled_evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )
  let assert [ready_recommendation] = enabled_evaluation.recommendations
  assert ready_recommendation.auto_enqueue_status == "ready"
  let assert Some(idempotency_key) = ready_recommendation.idempotency_key

  let queued_status = add_phase_run(status, idempotency_key)
  let duplicate_evaluation =
    playbook_eval.evaluate(
      playbook,
      queued_status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )
  let assert [duplicate_recommendation] = duplicate_evaluation.recommendations
  assert duplicate_recommendation.state == "queued"
  assert duplicate_recommendation.auto_enqueue_status == "duplicate"
  assert duplicate_recommendation.duplicate_phase_run_id
    == Some(start_key.phase_run_id(idempotency_key))
}

pub fn deviate_decision_suppresses_recommendation_without_deleting_history_test() {
  let root = "test/tmp/workstream-playbook/deviate"
  let #(store, status) = status_with_decision(root, "deviate", [])
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "deviated"
  assert recommendation.gate_status == "deviated"
  assert recommendation.skipped_by_decision_id == Some("decision-deviate")
  assert list.contains(
    recommendation.blocked_reasons,
    "deviated:decision-deviate",
  )
  assert recommendation.auto_enqueue_status == "blocked"

  let next_action =
    playbook.recommendation_to_next_action_artifact(
      workstream_id,
      recommendation,
    )
  assert next_action.state == "blocked"
}

pub fn reject_decision_blocks_recommendation_test() {
  let root = "test/tmp/workstream-playbook/reject"
  let #(store, status) = status_with_decision(root, "reject", [])
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.gate_status == "rejected"
  assert recommendation.idempotency_key == None
  assert list.contains(
    recommendation.blocked_reasons,
    "gate_rejected:decision-reject",
  )
}

pub fn auto_enqueue_obeys_workstream_hold_test() {
  let root = "test/tmp/workstream-playbook/held"
  let #(store, status) = status_with_decision(root, "approve", [])
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: True),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "available"
  assert recommendation.auto_enqueue_status == "blocked:workstream_held"
}

pub fn newer_matching_gate_decision_wins_test() {
  let root = "test/tmp/workstream-playbook/newer-decision"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff = write_snapshot(store, "handoff.json", handoff_json())
  let older =
    write_snapshot(store, "decision-approve.json", decision_json("approve"))
  let newer =
    write_snapshot(
      store,
      "decision-request_changes.json",
      decision_json("request_changes"),
    )
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      handoff_record(handoff),
      artifact_record(
        "decision-approve",
        types.decision_artifact_type,
        older,
        1003,
      ),
      artifact_record(
        "decision-request_changes",
        types.decision_artifact_type,
        newer,
        1004,
      ),
    ])
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.gate_status == "changes_requested"
  assert recommendation.idempotency_key == None
  assert list.contains(
    recommendation.blocked_reasons,
    "gate_changes_requested:decision-request_changes",
  )
}

pub fn from_phase_output_must_come_from_declared_phase_test() {
  let root = "test/tmp/workstream-playbook/from-phase"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff =
    write_snapshot(
      store,
      "handoff.json",
      handoff_json_for(
        phase_id: "implementation",
        output_name: "exec_plan_bundle",
        output_ref: output_ref,
        output_sha: output_sha,
        contract_type: "exec_plan_bundle",
        producer_workflow_id: "execplan-implementation",
      ),
    )
  let decision =
    write_snapshot(store, "decision-approve.json", decision_json("approve"))
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      handoff_record(handoff),
      artifact_record(
        "decision-approve",
        types.decision_artifact_type,
        decision,
        1003,
      ),
    ])
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.gate_status == "approved"
  assert recommendation.idempotency_key == None
  assert list.contains(
    recommendation.blocked_reasons,
    "from_phase_output_from_unexpected_phase:execplan:exec_plan_bundle",
  )
}

pub fn stale_gate_decision_blocks_until_reapproved_test() {
  let root = "test/tmp/workstream-playbook/stale-decision"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff = write_snapshot(store, "handoff.json", handoff_json())
  let stale =
    write_snapshot(
      store,
      "decision-stale.json",
      decision_json_for(
        artifact_id: "decision-stale",
        action_id: "implement_exec_plan",
        gate_id: "human_review",
        kind: "approve",
        inputs: [
          types.DecisionInputRef(
            name: "exec_plan_bundle",
            ref: "workstream-artifacts/sha256/"
              <> string.repeat("d", times: 64)
              <> ".json",
            sha256: string.repeat("d", times: 64),
          ),
        ],
      ),
    )
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      handoff_record(handoff),
      artifact_record(
        "decision-stale",
        types.decision_artifact_type,
        stale,
        1003,
      ),
    ])
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.gate_status == "stale"
  assert list.contains(
    recommendation.blocked_reasons,
    "gate_stale:human_review",
  )
}

pub fn checked_in_extended_playbook_publish_waits_for_review_reports_test() {
  let root = "test/tmp/workstream-playbook/extended-publish"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff =
    write_snapshot(
      store,
      "code-handoff.json",
      handoff_json_for(
        phase_id: "implementation",
        output_name: "code_change_bundle",
        output_ref: code_ref,
        output_sha: code_sha,
        contract_type: "code_change_bundle",
        producer_workflow_id: "execplan-implementation",
      ),
    )
  let decision =
    write_snapshot(
      store,
      "decision-publish.json",
      decision_json_for(
        artifact_id: "decision-publish",
        action_id: "publish_change",
        gate_id: "performance_review",
        kind: "approve",
        inputs: [
          types.DecisionInputRef(
            name: "code_change_bundle",
            ref: code_ref,
            sha256: code_sha,
          ),
        ],
      ),
    )
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      handoff_record(handoff),
      artifact_record(
        "decision-publish",
        types.decision_artifact_type,
        decision,
        1003,
      ),
    ])
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  let assert Ok(extended) =
    playbook.parse(read_file(".scherzo/playbooks/extended-implementation.yaml"))
  let evaluation =
    playbook_eval.evaluate(
      extended,
      status,
      store,
      playbook.suggest_only_policy(),
    )

  let publish =
    recommendation_by_action(evaluation.recommendations, "publish_change")
  assert publish.state == "blocked"
  assert list.contains(publish.missing_inputs, "security_review_report")
  assert list.contains(publish.missing_inputs, "performance_review_report")
  assert publish.gate_status == "pending"
}

pub fn auto_enqueue_limit_blocks_later_ready_actions_test() {
  let root = "test/tmp/workstream-playbook/auto-limit"
  let #(store, status) = status_without_decision(root)
  let playbook = limit_playbook()
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [first, second] = evaluation.recommendations
  assert first.auto_enqueue_status == "ready"
  assert second.auto_enqueue_status == "blocked:auto_enqueue_limit_reached"
}

pub fn conflicting_queued_run_blocks_auto_enqueue_test() {
  let root = "test/tmp/workstream-playbook/start-conflict"
  let #(store, status) = status_with_decision(root, "approve", [])
  let queued_status = add_phase_run(status, "different-idempotency-key")
  let playbook = implementation_playbook(auto_enabled: True, action_auto: True)
  let evaluation =
    playbook_eval.evaluate(
      playbook,
      queued_status,
      store,
      playbook.auto_enqueue_policy(workstream_held: False),
    )

  let assert [recommendation] = evaluation.recommendations
  assert recommendation.state == "blocked"
  assert recommendation.auto_enqueue_status == "blocked"
  assert list.contains(
    recommendation.blocked_reasons,
    "start_conflict:" <> start_key.phase_run_id("different-idempotency-key"),
  )
}

fn implementation_playbook(
  auto_enabled auto_enabled: Bool,
  action_auto action_auto: Bool,
) -> playbook.Playbook {
  playbook.Playbook(
    id: "standard-implementation",
    display_name: "Standard implementation",
    auto_enqueue: playbook.AutoEnqueueConfig(
      enabled: auto_enabled,
      max_actions_per_evaluation: 1,
    ),
    phases: base_phases(),
    next_actions: [
      playbook.PlaybookAction(
        action_id: "implement_exec_plan",
        label: "Implement ExecPlan",
        from_phase: Some("execplan"),
        to_phase: "implementation",
        workflow_id: "execplan-implementation",
        reason: "Approved plan can be implemented.",
        required_inputs: ["exec_plan_bundle"],
        expected_outputs: [],
        requires_gate: Some("human_review"),
        priority: 10,
        auto_enqueue: action_auto,
      ),
    ],
  )
}

fn limit_playbook() -> playbook.Playbook {
  playbook.Playbook(
    id: "standard-implementation",
    display_name: "Standard implementation",
    auto_enqueue: playbook.AutoEnqueueConfig(
      enabled: True,
      max_actions_per_evaluation: 1,
    ),
    phases: base_phases(),
    next_actions: [
      ungated_auto_action("first_ready", 10),
      ungated_auto_action("second_ready", 20),
    ],
  )
}

fn base_phases() -> List(playbook.PlaybookPhase) {
  [
    playbook.PlaybookPhase(
      phase_id: "execplan",
      display_name: "ExecPlan",
      workflow_id: "execplan",
      required_inputs: [],
      expected_outputs: [
        playbook.ArtifactRequirement(
          name: "exec_plan_bundle",
          contract_type: "exec_plan_bundle",
          artifact_type: Some("scherzo.exec_plan_bundle.v1"),
        ),
      ],
      gates: ["human_review"],
    ),
    playbook.PlaybookPhase(
      phase_id: "implementation",
      display_name: "Implementation",
      workflow_id: "execplan-implementation",
      required_inputs: [
        playbook.ArtifactRequirement(
          name: "exec_plan_bundle",
          contract_type: "exec_plan_bundle",
          artifact_type: Some("scherzo.exec_plan_bundle.v1"),
        ),
      ],
      expected_outputs: [],
      gates: [],
    ),
  ]
}

fn ungated_auto_action(
  action_id: String,
  priority: Int,
) -> playbook.PlaybookAction {
  playbook.PlaybookAction(
    action_id: action_id,
    label: action_id,
    from_phase: Some("execplan"),
    to_phase: "implementation",
    workflow_id: "execplan-implementation",
    reason: "Ready fixture action.",
    required_inputs: ["exec_plan_bundle"],
    expected_outputs: [],
    requires_gate: None,
    priority: priority,
    auto_enqueue: True,
  )
}

fn status_without_decision(
  root: String,
) -> #(state_artifact_store.Store, state_projection.WorkstreamStatus) {
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff = write_snapshot(store, "handoff.json", handoff_json())
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      handoff_record(handoff),
    ])
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  #(store, status)
}

fn status_with_decision(
  root: String,
  kind: String,
  extra_records: List(record.LedgerRecord),
) -> #(state_artifact_store.Store, state_projection.WorkstreamStatus) {
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let handoff = write_snapshot(store, "handoff.json", handoff_json())
  let decision =
    write_snapshot(store, "decision-" <> kind <> ".json", decision_json(kind))
  let projected =
    state_projection.fold(list.append(
      [
        workstream_created_record(),
        assignment_record(),
        handoff_record(handoff),
        artifact_record(
          "decision-" <> kind,
          types.decision_artifact_type,
          decision,
          1003,
        ),
      ],
      extra_records,
    ))
  let assert Ok(status) = dict.get(projected.workstreams, workstream_id)
  #(store, status)
}

fn add_phase_run(
  status: state_projection.WorkstreamStatus,
  idempotency_key: String,
) -> state_projection.WorkstreamStatus {
  let projected =
    state_projection.fold([
      workstream_created_record(),
      assignment_record(),
      ledger.workstream_phase_run_queued(
        1004,
        workstream_id,
        start_key.phase_run_id(idempotency_key),
        "implement_exec_plan",
        "execplan-implementation",
        bundle_ref,
        string.repeat("b", times: 64),
        123,
        idempotency_key,
      ),
    ])
  let assert Ok(queued_only) = dict.get(projected.workstreams, workstream_id)
  state_projection.WorkstreamStatus(
    ..status,
    queued_phase_runs: queued_only.queued_phase_runs,
  )
}

fn write_snapshot(
  store: state_artifact_store.Store,
  original_path: String,
  contents: String,
) -> artifact_store.Snapshot {
  let assert Ok(snapshot) =
    artifact_store.snapshot_bytes(
      store,
      original_path,
      "application/json",
      bit_array.from_string(contents),
    )
  snapshot
}

fn exec_plan_bundle_descriptor() -> types.ContractDescriptorRecord {
  types.ContractDescriptorRecord(
    kind: "artifact_set",
    ref_type: None,
    media_type: Some("application/json"),
    artifact_type: Some("scherzo.exec_plan_bundle.v2"),
    source: None,
    validation: None,
    metadata: None,
  )
}

fn handoff_json() -> String {
  handoff_json_for(
    phase_id: "execplan",
    output_name: "exec_plan_bundle",
    output_ref: output_ref,
    output_sha: output_sha,
    contract_type: "exec_plan_bundle",
    producer_workflow_id: "execplan",
  )
}

fn handoff_json_for(
  phase_id phase_id: String,
  output_name output_name: String,
  output_ref output_ref: String,
  output_sha output_sha: String,
  contract_type contract_type: String,
  producer_workflow_id producer_workflow_id: String,
) -> String {
  artifacts.handoff_to_string(
    types.HandoffArtifact(
      artifact_id: "handoff-" <> phase_id,
      workstream_id: workstream_id,
      phase_id: phase_id,
      summary: output_name <> " ready.",
      outputs: [
        types.HandoffOutput(
          name: output_name,
          snapshot: types.ArtifactSnapshot(
            ref: output_ref,
            sha256: output_sha,
            bytes: 123,
            media_type: "application/json",
            original_path: "tmp/" <> output_name <> ".json",
            descriptor: exec_plan_bundle_descriptor(),
            contract_type: Some(contract_type),
            producer: types.ProducerRef(
              workflow_id: producer_workflow_id,
              run_id: "run-1",
              step_id: "materialize_bundle",
            ),
            validation: types.ValidationSummary(
              status: "valid",
              validator: contract_type,
              checked_at: "2026-05-29T00:00:00Z",
            ),
            summary: output_name <> ".",
          ),
        ),
      ],
      recommended_next_actions: ["implement_exec_plan"],
      open_questions: [],
    ),
  )
}

fn decision_json(kind: String) -> String {
  decision_json_for(
    artifact_id: "decision-" <> kind,
    action_id: "implement_exec_plan",
    gate_id: "human_review",
    kind: kind,
    inputs: [
      types.DecisionInputRef(
        name: "exec_plan_bundle",
        ref: output_ref,
        sha256: output_sha,
      ),
    ],
  )
}

fn decision_json_for(
  artifact_id artifact_id: String,
  action_id action_id: String,
  gate_id gate_id: String,
  kind kind: String,
  inputs inputs: List(types.DecisionInputRef),
) -> String {
  artifacts.decision_to_string(types.DecisionArtifact(
    artifact_id: artifact_id,
    workstream_id: workstream_id,
    action_id: action_id,
    gate_id: gate_id,
    kind: kind,
    decided_at_ms: 1_770_000_000_000,
    decided_by: "reviewer@example.invalid",
    rationale: "Fixture decision.",
    inputs: inputs,
    summary: "Decision fixture.",
  ))
}

fn workstream_created_record() -> record.LedgerRecord {
  ledger.workstream_created(
    1000,
    workstream_id,
    record.linear_task_ref_fields(
      "issue-464",
      Some("LIV-464"),
      Some("https://linear.app/living-systems/issue/LIV-464"),
    ),
    "workstream-create",
  )
}

fn assignment_record() -> record.LedgerRecord {
  ledger.workstream_assigned(
    1001,
    workstream_id,
    "assignment-standard",
    "execplan-implementation",
    Some("standard-implementation"),
    "Operator assigned the standard implementation playbook.",
    "assignment-standard",
  )
}

fn handoff_record(snapshot: artifact_store.Snapshot) -> record.LedgerRecord {
  ledger.workstream_handoff_recorded(
    1002,
    workstream_id,
    "handoff-execplan",
    snapshot.ref,
    snapshot.sha256,
    snapshot.bytes,
    "execplan",
    "run-1",
    "handoff-execplan",
  )
}

fn artifact_record(
  artifact_id: String,
  artifact_type: String,
  snapshot: artifact_store.Snapshot,
  at_ms: Int,
) -> record.LedgerRecord {
  ledger.workstream_artifact_recorded(
    at_ms,
    workstream_id,
    artifact_id,
    artifact_type,
    snapshot.ref,
    snapshot.sha256,
    snapshot.bytes,
    snapshot.original_path,
    "artifact[]",
    snapshot.media_type,
    "operator",
    "reviewer@example.invalid",
    "workstream_decision",
    artifact_id,
  )
}
