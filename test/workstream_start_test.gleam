import gleam/bit_array
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import scherzo/hash
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/ledger as state_ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workstream/artifacts
import scherzo/workstream/ledger
import scherzo/workstream/start
import scherzo/workstream/start_key
import scherzo/workstream/types
import simplifile

pub fn start_from_handoff_queues_input_bundle_from_snapshot_refs_test() {
  let root = "test/tmp/workstream-start/from-handoff"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)

  let assert Ok(start.Queued(outcome)) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )

  assert outcome.workstream_id == "linear:LIV-461"
  assert outcome.workflow_id == "execplan-implementation"
  assert outcome.action_id == "implement_exec_plan"
  assert outcome.input_bundle_ref != handoff_snapshot.ref
  let assert Ok(input_bundle_json) =
    checkpoint.read_artifact(outcome.input_bundle_ref)
  let assert Ok(input_bundle) = artifacts.decode_input_bundle(input_bundle_json)
  assert input_bundle.workstream_id == "linear:LIV-461"
  assert input_bundle.workflow_id == "execplan-implementation"
  assert input_bundle.source_handoff_ref == handoff_snapshot.ref
  assert input_bundle.source_kind == Some("handoff")
  let assert [binding] = input_bundle.inputs
  assert binding.name == "exec_plan_bundle"
  assert binding.contract_type == "exec_plan_bundle"
  assert binding.value_ref != ""
  assert binding.sha256 != None

  let assert Ok(after_start) = load_projection(root)
  let assert Ok(workstream) =
    dict.get(after_start.workstreams, "linear:LIV-461")
  assert dict.size(workstream.queued_phase_runs) == 1
}

pub fn start_from_recorded_input_bundle_queues_without_handoff_contents_test() {
  let root = "test/tmp/workstream-start/from-input-bundle"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(first)) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )
  let assert Ok(after_first) = load_projection(root)

  let assert Ok(start.Queued(second)) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      first.input_bundle_ref,
      first.input_bundle_sha256,
      [],
      after_first,
      checkpoint,
    )

  assert second.input_bundle_ref == first.input_bundle_ref
  assert second.action_id == "rerun_from_bundle"
}

pub fn duplicate_start_from_same_handoff_returns_duplicate_test() {
  let root = "test/tmp/workstream-start/duplicate-handoff"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(first)) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )
  let assert Ok(after_first) = load_projection(root)

  let assert Ok(start.Duplicate(second)) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      after_first,
      checkpoint,
    )

  assert second.phase_run_id == first.phase_run_id
  assert second.idempotency_key == first.idempotency_key
}

pub fn idempotency_key_distinguishes_delimiter_containing_inputs_test() {
  let gate_collision_a =
    start_key.derive_idempotency_key(
      "linear:LIV-461",
      "implement_exec_plan",
      [],
      ["a,b", "c"],
    )
  let gate_collision_b =
    start_key.derive_idempotency_key(
      "linear:LIV-461",
      "implement_exec_plan",
      [],
      ["a", "b,c"],
    )
  assert gate_collision_a != gate_collision_b

  let input_collision_a =
    start_key.derive_idempotency_key(
      "linear:LIV-461",
      "implement_exec_plan",
      [#("a", "h1"), #("b", "h2")],
      [],
    )
  let input_collision_b =
    start_key.derive_idempotency_key(
      "linear:LIV-461",
      "implement_exec_plan",
      [#("a=h1,b", "h2")],
      [],
    )
  assert input_collision_a != input_collision_b
}

pub fn manual_start_snapshots_artifacts_and_rejects_conflicting_retry_test() {
  let root = "test/tmp/workstream-start/manual"
  let repo = root <> "/repo"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let assert Ok(Nil) =
    simplifile.write(repo <> "/docs/plan.json", "{\"plan\":1}")
  let manual =
    start.ManualStartContext(
      issue_id: "issue-461",
      issue_identifier: "LIV-461",
      issue_url: None,
      reason: "operator supplied reviewed plan",
    )
  let artifact =
    start.ManualArtifactInput(
      name: "exec_plan_bundle",
      artifact_type: "scherzo.exec_plan_bundle.v1",
      original_path: "docs/plan.json",
      contract_type: None,
      media_type: None,
    )
  let assert Ok(projected) = load_projection(root)

  let assert Ok(start.Queued(outcome)) =
    start.from_manual(
      "execplan-implementation",
      "implement_exec_plan",
      [],
      manual,
      [artifact],
      Some(exec_plan_bundle_contract()),
      repo,
      store,
      projected,
      checkpoint,
    )
  let assert Ok(input_bundle_json) =
    checkpoint.read_artifact(outcome.input_bundle_ref)
  let assert Ok(input_bundle) = artifacts.decode_input_bundle(input_bundle_json)
  assert input_bundle.source_kind == Some("manual")
  assert input_bundle.source_reason == Some("operator supplied reviewed plan")
  let assert [binding] = input_bundle.inputs
  assert binding.media_type == Some("application/json")
  assert binding.source_kind == Some("manual")

  let assert Ok(after_first) = load_projection(root)
  let assert Ok(start.Queued(replayed)) =
    start.from_input_bundle(
      "execplan-implementation",
      "replay_manual_bundle",
      outcome.input_bundle_ref,
      outcome.input_bundle_sha256,
      [],
      after_first,
      checkpoint,
    )
  assert replayed.input_bundle_ref == outcome.input_bundle_ref
  assert replayed.action_id == "replay_manual_bundle"

  let assert Ok(Nil) =
    simplifile.write(repo <> "/docs/plan.json", "{\"plan\":2}")
  let assert Ok(after_replay) = load_projection(root)
  let assert Error(error) =
    start.from_manual(
      "execplan-implementation",
      "implement_exec_plan",
      [],
      manual,
      [artifact],
      Some(exec_plan_bundle_contract()),
      repo,
      store,
      after_replay,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "start_conflict"
  let rejected_ref = start_key.snapshot_ref(hash.sha256_hex("{\"plan\":2}"))
  let assert Error(simplifile.Enoent) =
    simplifile.file_info(root <> "/.scherzo-state/artifacts/" <> rejected_ref)
}

pub fn stale_projection_conflicting_manual_start_is_rejected_at_append_test() {
  let root = "test/tmp/workstream-start/stale-manual-conflict"
  let repo = root <> "/repo"
  reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let assert Ok(Nil) =
    simplifile.write(repo <> "/docs/plan.json", "{\"plan\":1}")
  let manual =
    start.ManualStartContext(
      issue_id: "issue-461",
      issue_identifier: "LIV-461",
      issue_url: None,
      reason: "operator supplied reviewed plan",
    )
  let artifact =
    start.ManualArtifactInput(
      name: "exec_plan_bundle",
      artifact_type: "scherzo.exec_plan_bundle.v1",
      original_path: "docs/plan.json",
      contract_type: None,
      media_type: None,
    )
  let assert Ok(stale_projected) = load_projection(root)

  let assert Ok(start.Queued(_)) =
    start.from_manual(
      "execplan-implementation",
      "implement_exec_plan",
      [],
      manual,
      [artifact],
      Some(exec_plan_bundle_contract()),
      repo,
      store,
      stale_projected,
      checkpoint,
    )
  let assert Ok(Nil) =
    simplifile.write(repo <> "/docs/plan.json", "{\"plan\":2}")

  let assert Error(error) =
    start.from_manual(
      "execplan-implementation",
      "implement_exec_plan",
      [],
      manual,
      [artifact],
      Some(exec_plan_bundle_contract()),
      repo,
      store,
      stale_projected,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "start_conflict"

  let assert Ok(after_retry) = load_projection(root)
  let assert Ok(workstream) =
    dict.get(after_retry.workstreams, "linear:LIV-461")
  assert dict.size(workstream.queued_phase_runs) == 1
}

fn write_recorded_handoff(
  checkpoint: workflow_checkpoint.Writer,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  let assert Ok(output_snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/outputs/exec-plan-bundle.json",
      "application/json",
      bit_array.from_string("{\"bundle\":true}"),
    )
  let artifact_snapshot =
    types.ArtifactSnapshot(
      ref: output_snapshot.ref,
      sha256: output_snapshot.sha256,
      bytes: output_snapshot.bytes,
      media_type: output_snapshot.media_type,
      original_path: output_snapshot.original_path,
      contract_type: "exec_plan_bundle",
      producer: types.ProducerRef(
        workflow_id: "execplan",
        run_id: "run-1",
        step_id: "materialize_bundle",
      ),
      validation: types.ValidationSummary(
        status: "passed",
        validator: "test",
        checked_at: "test",
      ),
      summary: "bundle",
    )
  let handoff =
    types.HandoffArtifact(
      artifact_id: "handoff-1",
      workstream_id: "linear:LIV-461",
      phase_id: "execplan",
      summary: "handoff",
      outputs: [
        types.HandoffOutput(
          name: "exec_plan_bundle",
          snapshot: artifact_snapshot,
        ),
      ],
      recommended_next_actions: [],
      open_questions: [],
    )
  let handoff_json = artifacts.handoff_to_string(handoff)
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/handoffs/handoff-1.json",
      "application/json",
      bit_array.from_string(handoff_json),
    )
  let created =
    ledger.workstream_created(
      123,
      "linear:LIV-461",
      record.linear_task_ref_fields("issue-461", Some("LIV-461"), None),
      "created-1",
    )
  let handoff_record =
    ledger.workstream_handoff_recorded(
      123,
      "linear:LIV-461",
      "handoff-1",
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      "execplan",
      "run-1",
      "handoff-1",
    )
  let assert Ok(_) = checkpoint.append_workstream_record_idempotent(created)
  let assert Ok(_) =
    checkpoint.append_workstream_record_idempotent(handoff_record)
  Ok(workflow_checkpoint.ArtifactWritten(
    ref: snapshot.ref,
    sha256: snapshot.sha256,
    bytes: snapshot.bytes,
  ))
}

fn exec_plan_bundle_contract() -> workflow_contract.Contract {
  workflow_contract.Contract(
    version: 1,
    inputs: [
      workflow_contract.InputSpec(
        name: "exec_plan_bundle",
        type_: workflow_contract.ExecPlanBundle,
        required: True,
        description: None,
        source: Some(workflow_contract.MappedOutputSource),
      ),
    ],
    context: [],
    outputs: [],
  )
}

fn load_projection(
  root: String,
) -> Result(projection.Projection, state_ledger.LedgerError) {
  use path <- result.try(state_ledger.path_for_workspace_root(root))
  state_ledger.load_projection(path)
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}
