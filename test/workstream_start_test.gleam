import gleam/bit_array
import gleam/dict
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/ctl/workstream as ctl_workstream
import scherzo/hash
import scherzo/state/artifact_store as state_artifact_store
import scherzo/state/ledger as state_ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workstream/artifacts
import scherzo/workstream/decision
import scherzo/workstream/ledger
import scherzo/workstream/projection as workstream_projection
import scherzo/workstream/start
import scherzo/workstream/start_key
import scherzo/workstream/types
import simplifile
import support/test_helpers

type CtlOutMsg {
  CtlOutLine(String)
  CtlOutInline(String)
}

fn capture_line(subject: process.Subject(CtlOutMsg)) -> fn(String) -> Nil {
  fn(text) {
    process.send(subject, CtlOutLine(text))
    Nil
  }
}

fn capture_inline(subject: process.Subject(CtlOutMsg)) -> fn(String) -> Nil {
  fn(text) {
    process.send(subject, CtlOutInline(text))
    Nil
  }
}

fn drain_ctl_output(subject: process.Subject(CtlOutMsg)) -> String {
  drain_ctl_output_loop(subject, "")
}

fn drain_ctl_output_loop(
  subject: process.Subject(CtlOutMsg),
  acc: String,
) -> String {
  case process.receive(subject, within: 10) {
    Ok(CtlOutLine(text)) -> drain_ctl_output_loop(subject, acc <> text <> "\n")
    Ok(CtlOutInline(text)) -> drain_ctl_output_loop(subject, acc <> text)
    Error(Nil) -> acc
  }
}

pub fn ctl_start_from_handoff_run_emits_json_and_records_start_test() {
  let root = "test/tmp/workstream-start/ctl-from-handoff-json"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let subject = process.new_subject()

  let assert Ok(Nil) =
    ctl_workstream.run(
      ctl_workstream.StartFromHandoff(
        control_path: None,
        root: Some(root),
        json_output: True,
        workflow_id: "execplan-implementation",
        action_id: "implement_exec_plan",
        handoff_ref: handoff_snapshot.ref,
        handoff_sha256: handoff_snapshot.sha256,
        gate_decision_ids: [],
      ),
      capture_line(subject),
      capture_inline(subject),
    )

  let transcript = drain_ctl_output(subject)
  assert string.contains(transcript, "\"status\":\"queued\"")
  assert string.contains(
    transcript,
    "\"workflow_id\":\"execplan-implementation\"",
  )
  assert string.contains(transcript, "\"action_id\":\"implement_exec_plan\"")
  assert string.contains(
    transcript,
    "\"contract_inputs\":[\"exec_plan_bundle\"]",
  )
  let assert Ok(after_start) = load_projection(root)
  let assert Ok(workstream) =
    dict.get(after_start.workstreams, "linear:LIV-461")
  assert dict.size(workstream.queued_phase_runs) == 1
}

pub fn ctl_start_from_input_bundle_run_emits_human_queued_and_duplicate_test() {
  let root = "test/tmp/workstream-start/ctl-from-input-bundle-human"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(source)) =
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
  let command =
    ctl_workstream.StartFromInputBundle(
      control_path: None,
      root: Some(root),
      json_output: False,
      workflow_id: "execplan-implementation",
      action_id: "rerun_from_bundle",
      input_bundle_ref: source.input_bundle_ref,
      input_bundle_sha256: source.input_bundle_sha256,
      gate_decision_ids: [],
    )

  let first_subject = process.new_subject()
  let assert Ok(Nil) =
    ctl_workstream.run(
      command,
      capture_line(first_subject),
      capture_inline(first_subject),
    )
  let first_transcript = drain_ctl_output(first_subject)
  assert string.contains(first_transcript, "workstream start queued:")
  assert string.contains(first_transcript, "workflow=execplan-implementation")
  assert string.contains(first_transcript, "action=rerun_from_bundle")
  assert string.contains(first_transcript, "contract_inputs: exec_plan_bundle")

  let duplicate_subject = process.new_subject()
  let assert Ok(Nil) =
    ctl_workstream.run(
      command,
      capture_line(duplicate_subject),
      capture_inline(duplicate_subject),
    )
  let duplicate_transcript = drain_ctl_output(duplicate_subject)
  assert string.contains(duplicate_transcript, "workstream start duplicate:")
  assert string.contains(duplicate_transcript, "action=rerun_from_bundle")
}

pub fn ctl_start_from_input_bundle_run_propagates_start_errors_test() {
  let root = "test/tmp/workstream-start/ctl-input-bundle-error"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(source)) =
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
  let subject = process.new_subject()

  let assert Error(error) =
    ctl_workstream.run(
      ctl_workstream.StartFromInputBundle(
        control_path: None,
        root: Some(root),
        json_output: False,
        workflow_id: "execplan-revision",
        action_id: "rerun_from_bundle",
        input_bundle_ref: source.input_bundle_ref,
        input_bundle_sha256: source.input_bundle_sha256,
        gate_decision_ids: [],
      ),
      capture_line(subject),
      capture_inline(subject),
    )
  let #(code, message) = error
  assert code == "input_bundle_workflow_mismatch"
  assert string.contains(message, "execplan-implementation")
  assert drain_ctl_output(subject) == ""
}

pub fn start_from_handoff_queues_input_bundle_from_snapshot_refs_test() {
  let root = "test/tmp/workstream-start/from-handoff"
  test_helpers.reset_dir(root)
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
  assert binding.contract_type == None
  assert binding.descriptor.kind == "artifact_set"
  assert binding.value_ref != ""
  assert binding.sha256 != None

  let assert Ok(after_start) = load_projection(root)
  let assert Ok(workstream) =
    dict.get(after_start.workstreams, "linear:LIV-461")
  assert dict.size(workstream.queued_phase_runs) == 1
}

pub fn start_from_handoff_omits_optional_missing_mapped_contract_input_test() {
  let root = "test/tmp/workstream-start/optional-mapped-handoff-missing"
  test_helpers.reset_dir(root)
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
      Some(optional_missing_bundle_contract()),
      projected,
      checkpoint,
    )

  let assert Ok(input_bundle_json) =
    checkpoint.read_artifact(outcome.input_bundle_ref)
  let assert Ok(input_bundle) = artifacts.decode_input_bundle(input_bundle_json)
  let assert [] = input_bundle.inputs
  assert dict.size(outcome.contract_values.inputs) == 0
}

pub fn start_from_handoff_rejects_required_missing_mapped_contract_input_test() {
  let root = "test/tmp/workstream-start/required-mapped-handoff-missing"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(required_missing_bundle_contract()),
      projected,
      checkpoint,
    )
  let start.StartError(code, message) = error
  assert code == "contract_input_missing"
  assert string.contains(message, "missing_bundle")
}

pub fn start_from_handoff_accepts_legacy_snapshot_without_artifact_type_test() {
  let root = "test/tmp/workstream-start/from-handoff-legacy-artifact-type"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) =
    write_recorded_handoff_payload_with_artifact_type(
      checkpoint,
      "handoff-legacy",
      "{\"bundle\":true}",
      None,
    )
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

  let assert Ok(input_bundle_json) =
    checkpoint.read_artifact(outcome.input_bundle_ref)
  let assert Ok(input_bundle) = artifacts.decode_input_bundle(input_bundle_json)
  let assert [binding] = input_bundle.inputs
  assert binding.contract_type == None
  assert binding.descriptor.artifact_type == None
}

pub fn start_from_handoff_rejects_descriptor_artifact_type_mismatch_test() {
  let root = "test/tmp/workstream-start/from-handoff-artifact-type-mismatch"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) =
    write_recorded_handoff_payload_with_artifact_type(
      checkpoint,
      "handoff-mismatch",
      "{\"bundle\":true}",
      Some("scherzo.implementation_pack.v2"),
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
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
  let start.StartError(code, _) = error
  assert code == "contract_input_artifact_type_mismatch"
}

pub fn start_from_recorded_input_bundle_queues_without_handoff_contents_test() {
  let root = "test/tmp/workstream-start/from-input-bundle"
  test_helpers.reset_dir(root)
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

pub fn start_from_handoff_rejects_unrecommended_workflow_test() {
  let root = "test/tmp/workstream-start/unrecommended-workflow"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(handoff_snapshot) =
    write_recorded_handoff_payload_with_next_actions(
      checkpoint,
      "handoff-1",
      "{\"bundle\":true}",
      ["next-action-implement_exec_plan"],
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
    start.from_handoff(
      "execplan-revision",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )

  let start.StartError(code, _) = error
  assert code == "next_action_mismatch"
}

pub fn start_from_input_bundle_rejects_workflow_mismatch_test() {
  let root = "test/tmp/workstream-start/input-bundle-workflow-mismatch"
  test_helpers.reset_dir(root)
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

  let assert Error(error) =
    start.from_input_bundle(
      "execplan-revision",
      "rerun_from_bundle",
      first.input_bundle_ref,
      first.input_bundle_sha256,
      [],
      after_first,
      checkpoint,
    )

  let start.StartError(code, _) = error
  assert code == "input_bundle_workflow_mismatch"
}

pub fn start_from_handoff_rejects_corrupt_snapshot_test() {
  let root = "test/tmp/workstream-start/corrupt-handoff-snapshot"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(Nil) =
    simplifile.write(
      root <> "/.scherzo-state/artifacts/" <> handoff_snapshot.ref,
      "{}",
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
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

  let start.StartError(code, _) = error
  assert code == "snapshot_hash_mismatch"
}

pub fn start_from_handoff_requires_recorded_next_action_test() {
  let root = "test/tmp/workstream-start/next-action-not-recorded"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) =
    write_recorded_handoff_payload_with_next_actions(
      checkpoint,
      "handoff-1",
      "{\"bundle\":true}",
      ["next-action-implement_exec_plan"],
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
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

  let start.StartError(code, _) = error
  assert code == "next_action_not_recorded"
}

pub fn start_from_handoff_rejects_invalid_recorded_next_action_test() {
  let root = "test/tmp/workstream-start/next-action-invalid"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(Nil) =
    write_invalid_next_action_record(
      checkpoint,
      "next-action-implement_exec_plan",
    )
  let assert Ok(handoff_snapshot) =
    write_recorded_handoff_payload_with_next_actions(
      checkpoint,
      "handoff-1",
      "{\"bundle\":true}",
      ["next-action-implement_exec_plan"],
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
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

  let start.StartError(code, _) = error
  assert string.starts_with(code, "next_action_invalid:")
}

pub fn start_from_input_bundle_requires_recorded_bundle_test() {
  let root = "test/tmp/workstream-start/input-bundle-not-recorded"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(input_bundle) =
    write_unrecorded_input_bundle(checkpoint, handoff_snapshot.ref)
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      input_bundle.ref,
      input_bundle.sha256,
      [],
      projected,
      checkpoint,
    )

  let start.StartError(code, _) = error
  assert code == "input_bundle_not_recorded"
}

pub fn start_from_input_bundle_rejects_record_mismatch_test() {
  let root = "test/tmp/workstream-start/input-bundle-record-mismatch"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(input_bundle) =
    write_unrecorded_input_bundle(checkpoint, handoff_snapshot.ref)
  let assert Ok(Nil) =
    record_input_bundle_snapshot(
      checkpoint,
      input_bundle,
      "0000000000000000000000000000000000000000000000000000000000000000",
    )
  let assert Ok(projected) = load_projection(root)

  let assert Error(error) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      input_bundle.ref,
      input_bundle.sha256,
      [],
      projected,
      checkpoint,
    )

  let start.StartError(code, _) = error
  assert code == "input_bundle_record_mismatch"
}

pub fn gated_start_from_input_bundle_requires_exact_approval_test() {
  let root = "test/tmp/workstream-start/gated-input-bundle-approval"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(bundle_source)) =
    start.from_handoff(
      "execplan-implementation",
      "prepare_bundle",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )
  let assert Ok(Nil) =
    write_recorded_next_action_for(checkpoint, "rerun_from_bundle")
  let assert Ok(projected_with_gate) = load_projection(root)

  let assert Error(missing_error) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      bundle_source.input_bundle_ref,
      bundle_source.input_bundle_sha256,
      [],
      projected_with_gate,
      checkpoint,
    )
  let start.StartError(missing_code, _) = missing_error
  assert missing_code == "gate_decision_missing"

  let assert Ok(_) =
    write_decision_for_input_bundle(
      checkpoint,
      projected_with_gate,
      bundle_source.input_bundle_ref,
      "rerun_from_bundle",
      "approve",
    )
  let assert Ok(after_decision) = load_projection(root)
  let assert Ok(start.Queued(approved)) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      bundle_source.input_bundle_ref,
      bundle_source.input_bundle_sha256,
      [],
      after_decision,
      checkpoint,
    )

  assert approved.action_id == "rerun_from_bundle"
  assert approved.input_bundle_ref == bundle_source.input_bundle_ref
}

pub fn stale_input_bundle_gate_decision_does_not_authorize_new_inputs_test() {
  let root = "test/tmp/workstream-start/gated-input-bundle-stale"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(old_handoff) =
    write_recorded_handoff_payload(checkpoint, "handoff-1", "{\"bundle\":1}")
  let assert Ok(projected) = load_projection(root)
  let assert Ok(start.Queued(old_bundle)) =
    start.from_handoff(
      "execplan-implementation",
      "prepare_bundle",
      old_handoff.ref,
      old_handoff.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected,
      checkpoint,
    )
  let assert Ok(Nil) =
    write_recorded_next_action_for(checkpoint, "rerun_from_bundle")
  let assert Ok(projected_with_gate) = load_projection(root)
  let assert Ok(_) =
    write_decision_for_input_bundle(
      checkpoint,
      projected_with_gate,
      old_bundle.input_bundle_ref,
      "rerun_from_bundle",
      "approve",
    )
  let assert Ok(new_handoff) =
    write_recorded_handoff_payload(checkpoint, "handoff-2", "{\"bundle\":2}")
  let assert Ok(after_new_handoff) = load_projection(root)
  let assert Ok(start.Queued(new_bundle)) =
    start.from_handoff(
      "execplan-implementation",
      "prepare_bundle_new",
      new_handoff.ref,
      new_handoff.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      after_new_handoff,
      checkpoint,
    )
  let assert Ok(after_new_bundle) = load_projection(root)

  let assert Error(error) =
    start.from_input_bundle(
      "execplan-implementation",
      "rerun_from_bundle",
      new_bundle.input_bundle_ref,
      new_bundle.input_bundle_sha256,
      [],
      after_new_bundle,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "gate_decision_stale"
}

pub fn duplicate_start_from_same_handoff_returns_duplicate_test() {
  let root = "test/tmp/workstream-start/duplicate-handoff"
  test_helpers.reset_dir(root)
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

pub fn gated_start_requires_approve_decision_for_exact_snapshot_test() {
  let root = "test/tmp/workstream-start/gated-approval"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)

  let assert Error(missing_error) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      projected_with_gate,
      checkpoint,
    )
  let start.StartError(missing_code, _) = missing_error
  assert missing_code == "gate_decision_missing"

  let assert Ok(_) =
    write_decision_for_handoff(
      checkpoint,
      projected_with_gate,
      handoff_snapshot,
      "approve",
    )
  let assert Ok(after_decision) = load_projection(root)
  let assert Ok(start.Queued(outcome)) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      after_decision,
      checkpoint,
    )

  assert outcome.workstream_id == "linear:LIV-461"
  let assert Ok(reloaded) = load_projection(root)
  let assert Ok(workstream) = dict.get(reloaded.workstreams, "linear:LIV-461")
  let inspection =
    workstream_projection.inspect(state_artifact_store.new(root), workstream)
  let assert [recorded_decision] = inspection.decisions
  assert recorded_decision.action_id == "implement_exec_plan"
  assert recorded_decision.gate_id == "human_review"
  assert recorded_decision.kind == "approve"
  let assert [decision_input] = recorded_decision.inputs
  assert decision_input.name == "exec_plan_bundle"
}

pub fn stale_gate_decision_does_not_authorize_new_snapshot_hash_test() {
  let root = "test/tmp/workstream-start/gated-stale"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(old_handoff) =
    write_recorded_handoff_payload(checkpoint, "handoff-1", "{\"bundle\":1}")
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)
  let assert Ok(_) =
    write_decision_for_handoff(
      checkpoint,
      projected_with_gate,
      old_handoff,
      "approve",
    )
  let assert Ok(new_handoff) =
    write_recorded_handoff_payload(checkpoint, "handoff-2", "{\"bundle\":2}")
  let assert Ok(after_new_handoff) = load_projection(root)

  let assert Error(error) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      new_handoff.ref,
      new_handoff.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      after_new_handoff,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "gate_decision_stale"
}

pub fn request_changes_decision_does_not_satisfy_gate_test() {
  let root = "test/tmp/workstream-start/gated-request-changes"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)
  let assert Ok(_) =
    write_decision_for_handoff(
      checkpoint,
      projected_with_gate,
      handoff_snapshot,
      "request_changes",
    )
  let assert Ok(after_decision) = load_projection(root)

  let assert Error(error) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [],
      Some(exec_plan_bundle_contract()),
      after_decision,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "gate_decision_not_approved"
}

pub fn newer_nonapproval_vetoes_supplied_older_approval_test() {
  let root = "test/tmp/workstream-start/gated-newer-nonapproval"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })
  let assert Ok(handoff_snapshot) = write_recorded_handoff(checkpoint)
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)
  let assert Ok(approved) =
    write_decision_for_handoff_at(
      checkpoint,
      projected_with_gate,
      handoff_snapshot,
      "approve",
      124,
    )
  let assert Ok(after_approval) = load_projection(root)
  let assert Ok(_) =
    write_decision_for_handoff_at(
      checkpoint,
      after_approval,
      handoff_snapshot,
      "request_changes",
      125,
    )
  let assert Ok(after_nonapproval) = load_projection(root)

  let assert Error(error) =
    start.from_handoff(
      "execplan-implementation",
      "implement_exec_plan",
      handoff_snapshot.ref,
      handoff_snapshot.sha256,
      [approved.artifact_id],
      Some(exec_plan_bundle_contract()),
      after_nonapproval,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "gate_decision_not_approved"

  let assert Ok(workstream) =
    dict.get(after_nonapproval.workstreams, "linear:LIV-461")
  assert dict.size(workstream.queued_phase_runs) == 0
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
  test_helpers.reset_dir(root)
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
      artifact_type: "scherzo.exec_plan_bundle.v2",
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

pub fn gated_manual_start_requires_exact_approval_test() {
  let root = "test/tmp/workstream-start/gated-manual-approval"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
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
      artifact_type: "scherzo.exec_plan_bundle.v2",
      original_path: "docs/plan.json",
      contract_type: None,
      media_type: None,
    )
  let assert Ok(Nil) = write_workstream_created(checkpoint)
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)

  let assert Error(missing_error) =
    start.from_manual(
      "execplan-implementation",
      "implement_exec_plan",
      [],
      manual,
      [artifact],
      Some(exec_plan_bundle_contract()),
      repo,
      store,
      projected_with_gate,
      checkpoint,
    )
  let start.StartError(missing_code, _) = missing_error
  assert missing_code == "gate_decision_missing"

  let assert Ok(manual_snapshot) =
    write_manual_snapshot(checkpoint, "{\"plan\":1}")
  let assert Ok(_) =
    write_decision_for_manual_snapshot(
      checkpoint,
      projected_with_gate,
      manual_snapshot,
      "implement_exec_plan",
      "approve",
    )
  let assert Ok(after_decision) = load_projection(root)
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
      after_decision,
      checkpoint,
    )

  assert outcome.action_id == "implement_exec_plan"
  assert outcome.workstream_id == "linear:LIV-461"
}

pub fn stale_manual_gate_decision_does_not_authorize_changed_file_test() {
  let root = "test/tmp/workstream-start/gated-manual-stale"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
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
      artifact_type: "scherzo.exec_plan_bundle.v2",
      original_path: "docs/plan.json",
      contract_type: None,
      media_type: None,
    )
  let assert Ok(Nil) = write_workstream_created(checkpoint)
  let assert Ok(Nil) = write_recorded_next_action(checkpoint)
  let assert Ok(projected_with_gate) = load_projection(root)
  let assert Ok(manual_snapshot) =
    write_manual_snapshot(checkpoint, "{\"plan\":1}")
  let assert Ok(_) =
    write_decision_for_manual_snapshot(
      checkpoint,
      projected_with_gate,
      manual_snapshot,
      "implement_exec_plan",
      "approve",
    )
  let assert Ok(Nil) =
    simplifile.write(repo <> "/docs/plan.json", "{\"plan\":2}")
  let assert Ok(after_decision) = load_projection(root)

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
      after_decision,
      checkpoint,
    )
  let start.StartError(code, _) = error
  assert code == "gate_decision_stale"
  let rejected_ref = start_key.snapshot_ref(hash.sha256_hex("{\"plan\":2}"))
  let assert Error(simplifile.Enoent) =
    simplifile.file_info(root <> "/.scherzo-state/artifacts/" <> rejected_ref)
}

pub fn stale_projection_conflicting_manual_start_is_rejected_at_append_test() {
  let root = "test/tmp/workstream-start/stale-manual-conflict"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
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
      artifact_type: "scherzo.exec_plan_bundle.v2",
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

fn exec_plan_bundle_descriptor(
  artifact_type: Option(String),
) -> types.ContractDescriptorRecord {
  types.ContractDescriptorRecord(
    kind: "artifact_set",
    ref_type: None,
    media_type: Some("application/json"),
    artifact_type: artifact_type,
    source: None,
    validation: None,
    metadata: None,
  )
}

fn write_recorded_handoff(
  checkpoint: workflow_checkpoint.Writer,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  write_recorded_handoff_payload(checkpoint, "handoff-1", "{\"bundle\":true}")
}

fn write_recorded_handoff_payload(
  checkpoint: workflow_checkpoint.Writer,
  handoff_id: String,
  output_json: String,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  write_recorded_handoff_payload_with_artifact_type(
    checkpoint,
    handoff_id,
    output_json,
    Some("scherzo.exec_plan_bundle.v2"),
  )
}

fn write_recorded_handoff_payload_with_artifact_type(
  checkpoint: workflow_checkpoint.Writer,
  handoff_id: String,
  output_json: String,
  artifact_type: Option(String),
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  write_recorded_handoff_payload_with_next_actions_and_artifact_type(
    checkpoint,
    handoff_id,
    output_json,
    [],
    artifact_type,
  )
}

fn write_recorded_handoff_payload_with_next_actions(
  checkpoint: workflow_checkpoint.Writer,
  handoff_id: String,
  output_json: String,
  next_actions: List(String),
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  write_recorded_handoff_payload_with_next_actions_and_artifact_type(
    checkpoint,
    handoff_id,
    output_json,
    next_actions,
    Some("scherzo.exec_plan_bundle.v2"),
  )
}

fn write_recorded_handoff_payload_with_next_actions_and_artifact_type(
  checkpoint: workflow_checkpoint.Writer,
  handoff_id: String,
  output_json: String,
  next_actions: List(String),
  artifact_type: Option(String),
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  let assert Ok(output_snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/outputs/exec-plan-bundle.json",
      "application/json",
      bit_array.from_string(output_json),
    )
  let artifact_snapshot =
    types.ArtifactSnapshot(
      ref: output_snapshot.ref,
      sha256: output_snapshot.sha256,
      bytes: output_snapshot.bytes,
      media_type: output_snapshot.media_type,
      original_path: output_snapshot.original_path,
      descriptor: exec_plan_bundle_descriptor(artifact_type),
      contract_type: None,
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
      artifact_id: handoff_id,
      workstream_id: "linear:LIV-461",
      phase_id: "execplan",
      summary: "handoff",
      outputs: [
        types.HandoffOutput(
          name: "exec_plan_bundle",
          snapshot: artifact_snapshot,
        ),
      ],
      recommended_next_actions: next_actions,
      open_questions: [],
    )
  let handoff_json = artifacts.handoff_to_string(handoff)
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/handoffs/" <> handoff_id <> ".json",
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
      handoff_id,
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      "execplan",
      "run-1",
      handoff_id,
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

fn write_unrecorded_input_bundle(
  checkpoint: workflow_checkpoint.Writer,
  source_handoff_ref: String,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  let input_bundle =
    types.InputBundleArtifact(
      artifact_id: "input-bundle-test",
      workstream_id: "linear:LIV-461",
      source_handoff_ref: source_handoff_ref,
      workflow_id: "execplan-implementation",
      inputs: [],
      source_kind: Some("handoff"),
      source_reason: Some("test"),
    )
  let contents = artifacts.input_bundle_to_string(input_bundle)
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/input-bundles/input-bundle-test.json",
      "application/json",
      bit_array.from_string(contents),
    )
  Ok(workflow_checkpoint.ArtifactWritten(
    ref: snapshot.ref,
    sha256: snapshot.sha256,
    bytes: snapshot.bytes,
  ))
}

fn record_input_bundle_snapshot(
  checkpoint: workflow_checkpoint.Writer,
  snapshot: workflow_checkpoint.ArtifactWritten,
  recorded_sha256: String,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  let record =
    ledger.workstream_artifact_recorded(
      123,
      "linear:LIV-461",
      "input-bundle-test",
      types.input_bundle_artifact_type,
      snapshot.ref,
      recorded_sha256,
      snapshot.bytes,
      "workstream/input-bundles/input-bundle-test.json",
      "input_bundle",
      "application/json",
      "execplan-implementation",
      "run-1",
      "prepare_bundle",
      "input-bundle-test",
    )
  let assert Ok(_) = checkpoint.append_workstream_record_idempotent(record)
  Ok(Nil)
}

fn write_invalid_next_action_record(
  checkpoint: workflow_checkpoint.Writer,
  artifact_id: String,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/next-actions/invalid.json",
      "application/json",
      bit_array.from_string("{}"),
    )
  let record =
    ledger.workstream_artifact_recorded(
      123,
      "linear:LIV-461",
      artifact_id,
      types.next_action_artifact_type,
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      snapshot.original_path,
      "artifact[]",
      snapshot.media_type,
      "execplan",
      "run-1",
      "next_action",
      artifact_id,
    )
  let assert Ok(_) = checkpoint.append_workstream_record_idempotent(record)
  Ok(Nil)
}

fn write_recorded_next_action(
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  write_recorded_next_action_for(checkpoint, "implement_exec_plan")
}

fn write_recorded_next_action_for(
  checkpoint: workflow_checkpoint.Writer,
  action_id: String,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  let artifact_id = "next-action-" <> action_id
  let next_action =
    types.NextActionArtifact(
      artifact_id: artifact_id,
      workstream_id: "linear:LIV-461",
      action_id: action_id,
      workflow_id: "execplan-implementation",
      state: "available",
      priority: 1,
      inputs: ["exec_plan_bundle"],
      requires_gate: Some("human_review"),
      auto_enqueue: False,
    )
  let contents = artifacts.next_action_to_string(next_action)
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "workstream/next-actions/" <> action_id <> ".json",
      "application/json",
      bit_array.from_string(contents),
    )
  let record =
    ledger.workstream_artifact_recorded(
      123,
      "linear:LIV-461",
      artifact_id,
      types.next_action_artifact_type,
      snapshot.ref,
      snapshot.sha256,
      snapshot.bytes,
      snapshot.original_path,
      "artifact[]",
      snapshot.media_type,
      "execplan",
      "run-1",
      "next_action",
      artifact_id,
    )
  let assert Ok(_) = checkpoint.append_workstream_record_idempotent(record)
  Ok(Nil)
}

fn write_workstream_created(
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, workflow_checkpoint.CheckpointError) {
  let created =
    ledger.workstream_created(
      123,
      "linear:LIV-461",
      record.linear_task_ref_fields("issue-461", Some("LIV-461"), None),
      "workstream_manual_start:linear:LIV-461",
    )
  let assert Ok(_) = checkpoint.append_workstream_record_idempotent(created)
  Ok(Nil)
}

fn write_manual_snapshot(
  checkpoint: workflow_checkpoint.Writer,
  contents: String,
) -> Result(
  workflow_checkpoint.ArtifactWritten,
  workflow_checkpoint.CheckpointError,
) {
  let assert Ok(snapshot) =
    checkpoint.snapshot_workstream_bytes(
      "docs/plan.json",
      "application/json",
      bit_array.from_string(contents),
    )
  Ok(workflow_checkpoint.ArtifactWritten(
    ref: snapshot.ref,
    sha256: snapshot.sha256,
    bytes: snapshot.bytes,
  ))
}

fn write_decision_for_handoff(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  handoff_snapshot: workflow_checkpoint.ArtifactWritten,
  kind: String,
) -> Result(decision.RecordedDecision, decision.DecisionError) {
  write_decision_for_handoff_at(
    checkpoint,
    projected,
    handoff_snapshot,
    kind,
    124,
  )
}

fn write_decision_for_handoff_at(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  handoff_snapshot: workflow_checkpoint.ArtifactWritten,
  kind: String,
  decided_at_ms: Int,
) -> Result(decision.RecordedDecision, decision.DecisionError) {
  let assert Ok(handoff_json) = checkpoint.read_artifact(handoff_snapshot.ref)
  let assert Ok(handoff) = artifacts.decode_handoff(handoff_json)
  let assert [output] = handoff.outputs
  decision.record(
    checkpoint,
    projected,
    decision.RecordRequest(
      workstream_id: handoff.workstream_id,
      action_id: "implement_exec_plan",
      gate_id: "human_review",
      kind: kind,
      decided_at_ms: decided_at_ms,
      decided_by: "reviewer@example.invalid",
      rationale: "reviewed exact snapshot",
      inputs: [
        decision.DecisionInput(
          name: output.name,
          ref: output.snapshot.ref,
          sha256: output.snapshot.sha256,
        ),
      ],
      summary: kind <> " exact snapshot",
    ),
  )
}

fn write_decision_for_input_bundle(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  input_bundle_ref: String,
  action_id: String,
  kind: String,
) -> Result(decision.RecordedDecision, decision.DecisionError) {
  let assert Ok(bundle_json) = checkpoint.read_artifact(input_bundle_ref)
  let assert Ok(bundle) = artifacts.decode_input_bundle(bundle_json)
  let assert [binding] = bundle.inputs
  let assert Some(sha256) = binding.sha256
  decision.record(
    checkpoint,
    projected,
    decision.RecordRequest(
      workstream_id: bundle.workstream_id,
      action_id: action_id,
      gate_id: "human_review",
      kind: kind,
      decided_at_ms: 124,
      decided_by: "reviewer@example.invalid",
      rationale: "reviewed exact input bundle snapshots",
      inputs: [
        decision.DecisionInput(
          name: binding.name,
          ref: binding.value_ref,
          sha256: sha256,
        ),
      ],
      summary: kind <> " exact input bundle snapshots",
    ),
  )
}

fn write_decision_for_manual_snapshot(
  checkpoint: workflow_checkpoint.Writer,
  projected: projection.Projection,
  snapshot: workflow_checkpoint.ArtifactWritten,
  action_id: String,
  kind: String,
) -> Result(decision.RecordedDecision, decision.DecisionError) {
  decision.record(
    checkpoint,
    projected,
    decision.RecordRequest(
      workstream_id: "linear:LIV-461",
      action_id: action_id,
      gate_id: "human_review",
      kind: kind,
      decided_at_ms: 124,
      decided_by: "reviewer@example.invalid",
      rationale: "reviewed exact manual snapshot",
      inputs: [
        decision.DecisionInput(
          name: "exec_plan_bundle",
          ref: snapshot.ref,
          sha256: snapshot.sha256,
        ),
      ],
      summary: kind <> " exact manual snapshot",
    ),
  )
}

fn exec_plan_bundle_contract() -> workflow_contract.Contract {
  contract_with_bundle_input(bundle_contract_input(
    "exec_plan_bundle",
    workflow_contract.Required(workflow_contract.MappedOutputSource),
  ))
}

fn optional_missing_bundle_contract() -> workflow_contract.Contract {
  contract_with_bundle_input(bundle_contract_input(
    "optional_bundle",
    workflow_contract.Optional(Some(workflow_contract.MappedOutputSource)),
  ))
}

fn required_missing_bundle_contract() -> workflow_contract.Contract {
  contract_with_bundle_input(bundle_contract_input(
    "missing_bundle",
    workflow_contract.Required(workflow_contract.MappedOutputSource),
  ))
}

fn contract_with_bundle_input(
  input: workflow_contract.InputSpec,
) -> workflow_contract.Contract {
  workflow_contract.Contract(
    version: 1,
    inputs: [input],
    context: [],
    outputs: [],
  )
}

fn bundle_contract_input(
  name: String,
  source: workflow_contract.SourceRequirement(workflow_contract.InputSource),
) -> workflow_contract.InputSpec {
  workflow_contract.InputSpec(
    name: name,
    type_: workflow_contract.ExecPlanBundle,
    description: None,
    source: source,
    descriptor: Some(workflow_contract.ContractDescriptorSpec(
      kind: Some("artifact_set"),
      ref_type: None,
      media_type: Some("application/json"),
      artifact_type: Some("scherzo.exec_plan_bundle.v2"),
    )),
  )
}

fn load_projection(
  root: String,
) -> Result(projection.Projection, state_ledger.LedgerError) {
  use path <- result.try(state_ledger.path_for_workspace_root(root))
  state_ledger.load_projection(path)
}
