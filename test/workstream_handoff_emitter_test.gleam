import gleam/bit_array
import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/state/ledger as state_ledger
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workstream/artifact_store as workstream_artifact_store
import scherzo/workstream/artifacts
import scherzo/workstream/handoff_emitter
import scherzo/workstream/phase_metadata
import support/test_helpers

pub fn handoff_emitter_snapshots_output_handoff_and_next_action_test() {
  let root = "test/tmp/workstream-handoff-emitter/success"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest = success_manifest(root)

  let assert Ok(handoff_emitter.Emitted(
    workstream_id: workstream_id,
    output: output,
    next_actions: [next_action],
    handoff: handoff,
    records: records,
  )) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      Some("https://linear.app/living-systems/issue/LIV-436"),
      success_metadata(),
      manifest,
      checkpoint,
    )

  assert workstream_id == "linear:LIV-436"
  append_records(records, checkpoint)

  let assert Ok(handoff_contents) =
    checkpoint.read_artifact(handoff.snapshot_ref)
  let assert Ok(decoded_handoff) = artifacts.decode_handoff(handoff_contents)
  assert decoded_handoff.phase_id == "execplan"
  assert decoded_handoff.recommended_next_actions == [next_action.artifact_id]
  let assert [handoff_output] = decoded_handoff.outputs
  assert handoff_output.name == "exec_plan_bundle"
  assert handoff_output.snapshot.ref == output.snapshot.ref
  assert handoff_output.snapshot.contract_type == "exec_plan_bundle"
  assert handoff_output.snapshot.artifact_type
    == Some("scherzo.exec_plan_bundle.v2")
  assert handoff_output.snapshot.producer.workflow_id == "execplan"
  assert handoff_output.snapshot.producer.run_id == "run-1"
  assert handoff_output.snapshot.producer.step_id == "materialize_bundle"

  let assert Ok(next_action_contents) =
    checkpoint.read_artifact(next_action.snapshot_ref)
  let assert Ok(decoded_next_action) =
    artifacts.decode_next_action(next_action_contents)
  assert decoded_next_action.action_id == "implement_exec_plan"
  assert decoded_next_action.state == "suggested"
  assert decoded_next_action.priority == 0
  assert decoded_next_action.inputs == ["exec_plan_bundle"]
  assert decoded_next_action.requires_gate == Some("human_review")
  assert decoded_next_action.auto_enqueue == False

  let store = artifact_store.new(root)
  let assert Ok(handoff_bytes) =
    workstream_artifact_store.read_snapshot(
      store,
      handoff.snapshot_ref,
      handoff.snapshot_sha256,
    )
  assert handoff_bytes == bit_array.from_string(handoff_contents)
}

pub fn handoff_emitter_is_idempotent_for_exact_retries_test() {
  let root = "test/tmp/workstream-handoff-emitter/idempotent"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest = success_manifest(root)

  let assert Ok(first) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      success_metadata(),
      manifest,
      checkpoint,
    )
  let assert Ok(second) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      success_metadata(),
      manifest,
      checkpoint,
    )

  let assert handoff_emitter.Emitted(
    handoff: first_handoff,
    records: first_records,
    ..,
  ) = first
  let assert handoff_emitter.Emitted(
    handoff: second_handoff,
    records: second_records,
    ..,
  ) = second
  assert first_handoff.snapshot_ref == second_handoff.snapshot_ref
  assert first_handoff.snapshot_sha256 == second_handoff.snapshot_sha256

  let assert [first_record, ..remaining_records] = first_records
  let assert Ok(state_ledger.Appended) =
    checkpoint.append_workstream_record_idempotent(first_record)
  append_records(remaining_records, checkpoint)
  assert_retry_records(second_records, checkpoint)
}

pub fn handoff_emitter_requires_present_output_metadata_test() {
  let root = "test/tmp/workstream-handoff-emitter/missing-metadata"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest =
    workflow_contract_manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-1",
      outputs: [
        workflow_contract_manifest.NamedManifestValue(
          name: "exec_plan_bundle",
          value: workflow_contract_manifest.ManifestValue(
            type_: workflow_contract.ExecPlanBundle,
            status: workflow_contract_manifest.Present,
            ref_kind: Some(workflow_contract_manifest.RunArtifact),
            ref: Some("runs/run-1/outputs/exec_plan_bundle.json"),
            sha256: None,
            bytes: Some(2),
            media_type: Some("application/json"),
            value: None,
            source: Some(source_json("materialize_bundle")),
            diagnostic: None,
          ),
        ),
      ],
      diagnostics: [],
    )

  let assert Error(error) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      success_metadata(),
      manifest,
      checkpoint,
    )

  assert handoff_emitter.error_code(error)
    == "workstream_handoff_output_sha256_missing"
}

pub fn handoff_emitter_rejects_absent_output_test() {
  let root = "test/tmp/workstream-handoff-emitter/absent-output"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest =
    workflow_contract_manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: "execplan",
      workflow_fingerprint: "wf-1",
      outputs: [
        workflow_contract_manifest.NamedManifestValue(
          name: "exec_plan_bundle",
          value: workflow_contract_manifest.absent(
            workflow_contract.ExecPlanBundle,
            Some("optional output not produced"),
          ),
        ),
      ],
      diagnostics: [],
    )

  let assert Error(error) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      success_metadata(),
      manifest,
      checkpoint,
    )

  assert handoff_emitter.error_code(error) == "workstream_handoff_output_absent"
}

pub fn handoff_emitter_rejects_invalid_next_action_payload_test() {
  let root = "test/tmp/workstream-handoff-emitter/invalid-next-action"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Error(error) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      phase_metadata.PhaseMetadata(..success_metadata(), next_actions: [
        phase_metadata.PhaseNextAction(
          action_id: "implement_exec_plan",
          workflow_id: "execplan-implementation",
          state: "not-a-valid-state",
          priority: 0,
          inputs: ["exec_plan_bundle"],
          requires_gate: Some("human_review"),
          auto_enqueue: False,
        ),
      ]),
      success_manifest(root),
      checkpoint,
    )

  assert handoff_emitter.error_code(error) == "workstream_next_action_invalid"
}

pub fn handoff_emitter_rejects_unsupported_handoff_artifact_type_test() {
  let root = "test/tmp/workstream-handoff-emitter/unsupported-artifact-type"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Error(error) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      phase_metadata.PhaseMetadata(
        ..success_metadata(),
        handoff: Some(phase_metadata.PhaseHandoff(
          output: "exec_plan_bundle",
          artifact_type: "scherzo.handoff.v2",
          snapshot: phase_metadata.SnapshotRequired,
        )),
      ),
      success_manifest(root),
      checkpoint,
    )

  assert handoff_emitter.error_code(error)
    == "workstream_handoff_artifact_type_unsupported"
}

pub fn handoff_emitter_rejects_stale_artifact_hash_test() {
  let root = "test/tmp/workstream-handoff-emitter/hash-mismatch"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let manifest = success_manifest(root)
  let assert [named] = manifest.outputs
  let stale =
    workflow_contract_manifest.ContractOutputManifest(..manifest, outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: named.name,
        value: workflow_contract_manifest.ManifestValue(
          ..named.value,
          sha256: Some(string.repeat("0", times: 64)),
        ),
      ),
    ])

  let assert Error(error) =
    handoff_emitter.emit(
      "execplan",
      "run-1",
      "wf-1",
      "issue-1",
      "LIV-436",
      None,
      success_metadata(),
      stale,
      checkpoint,
    )

  assert handoff_emitter.error_code(error)
    == "workstream_output_snapshot_failed"
}

fn success_metadata() -> phase_metadata.PhaseMetadata {
  phase_metadata.PhaseMetadata(
    phase_id: "execplan",
    display_name: "ExecPlan authored",
    handoff: Some(phase_metadata.PhaseHandoff(
      output: "exec_plan_bundle",
      artifact_type: "scherzo.handoff.v1",
      snapshot: phase_metadata.SnapshotRequired,
    )),
    gates: [],
    next_actions: [
      phase_metadata.PhaseNextAction(
        action_id: "implement_exec_plan",
        workflow_id: "execplan-implementation",
        state: "suggested",
        priority: 0,
        inputs: ["exec_plan_bundle"],
        requires_gate: Some("human_review"),
        auto_enqueue: False,
      ),
    ],
    final_phase: False,
  )
}

fn success_manifest(
  root: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  let store = artifact_store.new(root)
  let contents = "{\"bundle_id\":\"bundle-1\"}"
  let assert Ok(existing) =
    artifact_store.write_output_blob(
      store,
      "run-1",
      "exec_plan_bundle",
      ".json",
      contents,
    )
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "exec_plan_bundle",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.ExecPlanBundle,
          workflow_contract_manifest.ArtifactWritten(
            ref: existing.ref,
            sha256: existing.sha256,
            bytes: existing.bytes,
          ),
          "application/json",
          Some(source_json("materialize_bundle")),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn source_json(step_id: String) -> json_value.JsonValue {
  json_value.JObject([#("step_id", json_value.JString(step_id))])
}

fn append_records(
  records: List(record.LedgerRecord),
  checkpoint: workflow_checkpoint.Writer,
) -> Nil {
  case records {
    [] -> Nil
    [ledger_record, ..rest] -> {
      let assert Ok(_) =
        checkpoint.append_workstream_record_idempotent(ledger_record)
      append_records(rest, checkpoint)
    }
  }
}

fn assert_retry_records(
  records: List(record.LedgerRecord),
  checkpoint: workflow_checkpoint.Writer,
) -> Nil {
  case records {
    [] -> Nil
    [ledger_record, ..rest] -> {
      let assert Ok(state_ledger.AlreadyRecorded(existing_record: existing)) =
        checkpoint.append_workstream_record_idempotent(ledger_record)
      assert existing.body == ledger_record.body
      assert_retry_records(rest, checkpoint)
    }
  }
}
