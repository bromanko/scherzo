import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest as contract_manifest
import support/test_helpers

const workflow_id = "workflow-alpha"

const workflow_fingerprint = "wf-sha"

const issue_id = "issue-1"

const issue_identifier = "LIV-407"

const output_name = "code_change_bundle"

fn append_repair(root: String, run_id: String, record_id: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append(
      ledger_path,
      record.with_id(
        record_id,
        2,
        record.WorkflowRepairRequested(
          run_id: run_id,
          workflow_id: workflow_id,
          issue_id: issue_id,
          issue_identifier: issue_identifier,
          requested_target: run_id,
          requested_step_id: None,
          selected_step_id: "materialize_code_change_bundle",
          failed_attempt_index: 1,
          next_attempt_index: 2,
          reason: "retry-step",
        ),
      ),
      False,
    )
  Nil
}

fn absent_output_manifest(run_id: String, diagnostic: String) -> String {
  contract_manifest.ContractOutputManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    outputs: [
      contract_manifest.NamedManifestValue(
        name: output_name,
        value: contract_manifest.absent(
          workflow_contract.CodeChangeBundle,
          Some(diagnostic),
        ),
      ),
    ],
    diagnostics: [diagnostic],
  )
  |> contract_manifest.output_manifest_to_string
}

fn present_output_manifest(
  run_id: String,
  artifact: workflow_checkpoint.ArtifactWritten,
) -> String {
  contract_manifest.ContractOutputManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    outputs: [
      contract_manifest.NamedManifestValue(
        name: output_name,
        value: contract_manifest.present_run_artifact(
          workflow_contract.CodeChangeBundle,
          contract_manifest.ArtifactWritten(
            ref: artifact.ref,
            sha256: artifact.sha256,
            bytes: artifact.bytes,
          ),
          "application/json",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
  |> contract_manifest.output_manifest_to_string
}

fn record_outputs_recorded(
  checkpoint: workflow_checkpoint.Writer,
  run_id: String,
  artifact: workflow_checkpoint.ArtifactWritten,
) -> Nil {
  let assert Ok(Nil) =
    checkpoint.workflow_outputs_recorded(
      workflow_checkpoint.WorkflowContractManifestRecorded(
        run_id: run_id,
        workflow_id: workflow_id,
        workflow_fingerprint: workflow_fingerprint,
        artifact: artifact,
      ),
    )
  Nil
}

fn output_record_refs(root: String, run_id: String) -> List(String) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
  |> list.fold([], fn(acc, ledger_record) {
    case ledger_record.body {
      record.WorkflowRunOutputsRecorded(
        run_id: recorded_run_id,
        artifact_ref: ref,
        ..,
      )
        if recorded_run_id == run_id
      -> [ref, ..acc]
      _ -> acc
    }
  })
  |> list.reverse
}

pub fn repaired_workflow_outputs_use_repair_generation_manifest_and_blob_test() {
  let root = "test/tmp/workflow-checkpoint/repaired-output-generation"
  let run_id = "run-1"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let store = artifact_store.new(root)

  let stale_contents =
    absent_output_manifest(
      run_id,
      "workflow_output_source_step_missing:materialize_code_change_bundle",
    )
  let assert Ok(stale_written) =
    checkpoint.write_workflow_outputs_manifest(run_id, stale_contents)
  assert stale_written.ref == "runs/run-1/outputs.v1.json"
  record_outputs_recorded(checkpoint, run_id, stale_written)

  append_repair(root, run_id, "repair-1")

  let repaired_blob_contents = "{\"bundle\":true}"
  let assert Ok(blob) =
    checkpoint.write_workflow_output_blob(
      workflow_checkpoint.WorkflowOutputBlobWrite(
        run_id: run_id,
        output_name: output_name,
        extension: ".json",
        contents: repaired_blob_contents,
      ),
    )
  assert blob.ref == "runs/run-1/repairs/1/outputs/code_change_bundle.json"

  let repaired_contents = present_output_manifest(run_id, blob)
  let assert Ok(repaired_written) =
    checkpoint.write_workflow_outputs_manifest(run_id, repaired_contents)
  assert repaired_written.ref == "runs/run-1/repairs/1/outputs.v1.json"
  record_outputs_recorded(checkpoint, run_id, repaired_written)

  let assert Ok(fixed_contents) =
    artifact_store.read_artifact_unverified(
      store,
      artifact_store.output_manifest_ref(run_id),
    )
  assert fixed_contents == stale_contents
  let assert Ok(repaired_artifact_contents) =
    artifact_store.read_artifact_unverified(store, repaired_written.ref)
  assert repaired_artifact_contents == repaired_contents
  let assert Ok(blob_contents) =
    artifact_store.read_artifact_unverified(store, blob.ref)
  assert blob_contents == repaired_blob_contents
  assert output_record_refs(root, run_id)
    == [
      "runs/run-1/outputs.v1.json",
      "runs/run-1/repairs/1/outputs.v1.json",
    ]
}

pub fn repaired_workflow_outputs_after_compaction_use_next_generation_test() {
  let root = "test/tmp/workflow-checkpoint/repaired-output-compaction"
  let run_id = "run-1"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let store = artifact_store.new(root)

  let stale_contents =
    absent_output_manifest(
      run_id,
      "workflow_output_source_step_missing:materialize_code_change_bundle",
    )
  let assert Ok(stale_written) =
    checkpoint.write_workflow_outputs_manifest(run_id, stale_contents)
  record_outputs_recorded(checkpoint, run_id, stale_written)

  append_repair(root, run_id, "repair-1")
  let first_blob_contents = "{\"bundle\":1}"
  let assert Ok(first_blob) =
    checkpoint.write_workflow_output_blob(
      workflow_checkpoint.WorkflowOutputBlobWrite(
        run_id: run_id,
        output_name: output_name,
        extension: ".json",
        contents: first_blob_contents,
      ),
    )
  assert first_blob.ref
    == "runs/run-1/repairs/1/outputs/code_change_bundle.json"
  let first_repaired_contents = present_output_manifest(run_id, first_blob)
  let assert Ok(first_repaired) =
    checkpoint.write_workflow_outputs_manifest(run_id, first_repaired_contents)
  assert first_repaired.ref == "runs/run-1/repairs/1/outputs.v1.json"
  record_outputs_recorded(checkpoint, run_id, first_repaired)

  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.compact(ledger_path)

  append_repair(root, run_id, "repair-2")
  let second_blob_contents = "{\"bundle\":2}"
  let assert Ok(second_blob) =
    checkpoint.write_workflow_output_blob(
      workflow_checkpoint.WorkflowOutputBlobWrite(
        run_id: run_id,
        output_name: output_name,
        extension: ".json",
        contents: second_blob_contents,
      ),
    )
  assert second_blob.ref
    == "runs/run-1/repairs/2/outputs/code_change_bundle.json"
  let second_repaired_contents = present_output_manifest(run_id, second_blob)
  let assert Ok(second_repaired) =
    checkpoint.write_workflow_outputs_manifest(run_id, second_repaired_contents)
  assert second_repaired.ref == "runs/run-1/repairs/2/outputs.v1.json"
  record_outputs_recorded(checkpoint, run_id, second_repaired)

  let assert Ok(stale_artifact_contents) =
    artifact_store.read_artifact_unverified(
      store,
      artifact_store.output_manifest_ref(run_id),
    )
  assert stale_artifact_contents == stale_contents
  let assert Ok(first_repaired_artifact_contents) =
    artifact_store.read_artifact_unverified(store, first_repaired.ref)
  assert first_repaired_artifact_contents == first_repaired_contents
  let assert Ok(first_blob_artifact_contents) =
    artifact_store.read_artifact_unverified(store, first_blob.ref)
  assert first_blob_artifact_contents == first_blob_contents
  let assert Ok(second_repaired_artifact_contents) =
    artifact_store.read_artifact_unverified(store, second_repaired.ref)
  assert second_repaired_artifact_contents == second_repaired_contents
  let assert Ok(second_blob_artifact_contents) =
    artifact_store.read_artifact_unverified(store, second_blob.ref)
  assert second_blob_artifact_contents == second_blob_contents
}

pub fn repaired_output_manifest_mismatch_reports_repaired_generation_test() {
  let root = "test/tmp/workflow-checkpoint/repaired-output-corruption"
  let run_id = "run-1"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 10 })
  let store = artifact_store.new(root)
  append_repair(root, run_id, "repair-1")

  let existing_contents = absent_output_manifest(run_id, "stale repair output")
  let assert Ok(existing) =
    artifact_store.write_output_manifest_for_generation(
      store,
      run_id,
      1,
      existing_contents,
    )
  assert existing.ref == "runs/run-1/repairs/1/outputs.v1.json"

  let desired_contents =
    absent_output_manifest(run_id, "different repair output")
  let assert Error(workflow_checkpoint.CheckpointArtifactFailed(reason)) =
    checkpoint.write_workflow_outputs_manifest(run_id, desired_contents)
  assert string.starts_with(
    reason,
    "existing_repaired_output_manifest_mismatch:runs/run-1/repairs/1/outputs.v1.json",
  )
}
