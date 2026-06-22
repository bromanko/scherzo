import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/hash
import scherzo/state/artifact_store
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_run/contract_io
import scherzo/workflow_run/contract_io_error
import scherzo/workspace_run
import simplifile

pub type RecollectionError {
  RecollectionError(code: String, message: Option(String))
}

pub type Outcome {
  AlreadyValid(recorded: workflow_checkpoint.ArtifactWritten)
  Recollected(
    recorded: workflow_checkpoint.ArtifactWritten,
    manifest: workflow_contract_manifest.ContractOutputManifest,
  )
}

pub fn describe_error(error: RecollectionError) -> String {
  let RecollectionError(code, _) = error
  code
}

pub fn error_message(error: RecollectionError) -> Option(String) {
  let RecollectionError(_, message) = error
  message
}

pub fn execute(
  projection_state: projection.Projection,
  run_id: String,
  current: recovery.CurrentWorkflowObservation,
  checkpoint: workflow_checkpoint.Writer,
  recollection_checkpoint: workflow_checkpoint.Writer,
  store: artifact_store.Store,
) -> Result(Outcome, RecollectionError) {
  use current <- result.try(require_current_workflow(current))
  use run <- result.try(selected_run(projection_state, run_id))
  use Nil <- result.try(validate_drift(run, current))
  let latest_statuses =
    latest_statuses_by_step(projection.step_attempts_for_run(
      projection_state,
      run_id,
    ))
  use Nil <- result.try(require_all_steps_completed(
    current.dag,
    latest_statuses,
  ))
  case projection.workflow_output_manifest(projection_state, run_id) {
    Some(recorded) -> {
      let written = manifest_ref_to_written(recorded)
      case
        latest_manifest_is_valid(
          current.dag,
          run_id,
          current.workflow_fingerprint,
          written,
          checkpoint,
        )
      {
        True -> Ok(AlreadyValid(written))
        False ->
          recover_and_record_outputs(
            current,
            run_id,
            latest_statuses,
            store,
            recollection_checkpoint,
          )
      }
    }
    None ->
      recover_and_record_outputs(
        current,
        run_id,
        latest_statuses,
        store,
        recollection_checkpoint,
      )
  }
}

fn recover_and_record_outputs(
  current: CurrentWorkflow,
  run_id: String,
  latest_statuses: Dict(String, projection.StepAttemptStatus),
  store: artifact_store.Store,
  recollection_checkpoint: workflow_checkpoint.Writer,
) -> Result(Outcome, RecollectionError) {
  let required_sources = required_sources(current.dag)
  use source_steps <- result.try(recover_source_steps(
    required_sources,
    latest_statuses,
    store,
  ))
  use prepared_workspaces <- result.try(recover_prepared_workspaces(
    current.dag,
    source_steps,
  ))
  record_outputs(
    current,
    run_id,
    recollection_checkpoint,
    source_steps.artifacts,
    prepared_workspaces,
  )
}

type CurrentWorkflow {
  CurrentWorkflow(
    issue: tracker_issue.Issue,
    workflow_id: String,
    workflow_fingerprint: String,
    issue_fingerprint: String,
    dag: workflow_dag.WorkflowDag,
    workspace_root: String,
  )
}

type SelectedRun {
  SelectedRun(
    workflow_id: String,
    workflow_fingerprint: String,
    issue_id: String,
    issue_identifier: String,
    issue_fingerprint: String,
    task_ref: record.TaskRefFields,
  )
}

type SourceRecovery {
  SourceRecovery(
    artifacts: Dict(String, step_artifact.StepArtifact),
    statuses: Dict(String, projection.StepAttemptStatus),
  )
}

type OutputSourceRequirement {
  OutputSourceRequirement(step_id: String, needs_workspace: Bool)
}

fn require_current_workflow(
  current: recovery.CurrentWorkflowObservation,
) -> Result(CurrentWorkflow, RecollectionError) {
  case current {
    recovery.CurrentWorkflow(
      issue,
      workflow_id,
      workflow_fingerprint,
      issue_fingerprint,
      dag,
      workspace_root,
    ) ->
      Ok(CurrentWorkflow(
        issue,
        workflow_id,
        workflow_fingerprint,
        issue_fingerprint,
        dag,
        workspace_root,
      ))
    recovery.IssueUnavailable ->
      Error(RecollectionError("issue_unavailable", Some("issue is unavailable")))
    recovery.TrackerRefreshUnavailable ->
      Error(RecollectionError(
        "tracker_refresh_unavailable",
        Some("tracker refresh is unavailable"),
      ))
    recovery.WorkflowUnavailable(reason) ->
      Error(RecollectionError(
        "workflow_unavailable",
        Some("workflow is unavailable: " <> reason),
      ))
  }
}

fn selected_run(
  projection_state: projection.Projection,
  run_id: String,
) -> Result(SelectedRun, RecollectionError) {
  use run_status <- result.try(
    projection.workflow_run(projection_state, run_id)
    |> result.replace_error(RecollectionError(
      "run_not_found",
      Some("run not found"),
    )),
  )
  use provenance <- result.try(
    projection.workflow_run_provenance(projection_state, run_id)
    |> result.replace_error(RecollectionError(
      "issue_drift",
      Some("task identity drifted"),
    )),
  )
  use task_ref <- result.try(
    projection.workflow_task_ref(projection_state, run_id)
    |> result.replace_error(RecollectionError(
      "issue_drift",
      Some("task identity drifted"),
    )),
  )
  let issue_id = case run_status {
    projection.WorkflowRunActive(issue_id: issue_id, ..)
    | projection.WorkflowRunFinished(issue_id: issue_id, ..)
    | projection.WorkflowRunInterrupted(issue_id: issue_id, ..)
    | projection.WorkflowRunSuperseded(issue_id: issue_id, ..) -> issue_id
  }
  Ok(SelectedRun(
    workflow_id: provenance.workflow_id,
    workflow_fingerprint: provenance.workflow_fingerprint,
    issue_id: issue_id,
    issue_identifier: provenance.issue_identifier,
    issue_fingerprint: provenance.issue_fingerprint,
    task_ref: task_ref,
  ))
}

fn validate_drift(
  run: SelectedRun,
  current: CurrentWorkflow,
) -> Result(Nil, RecollectionError) {
  case run.issue_id != current.issue.id {
    True -> Error(RecollectionError("issue_drift", Some("issue id drifted")))
    False ->
      case
        run.issue_identifier != ""
        && run.issue_identifier != current.issue.identifier
      {
        True ->
          Error(RecollectionError(
            "issue_drift",
            Some("issue identifier drifted"),
          ))
        False ->
          case run.workflow_id != current.workflow_id {
            True ->
              Error(RecollectionError(
                "workflow_drift",
                Some("workflow id drifted"),
              ))
            False ->
              case
                run.workflow_fingerprint != ""
                && run.workflow_fingerprint != current.workflow_fingerprint
              {
                True ->
                  Error(RecollectionError(
                    "workflow_drift",
                    Some("workflow fingerprint drifted"),
                  ))
                False ->
                  case
                    run.issue_fingerprint != ""
                    && !tracker_issue.fingerprint_equivalent(
                      run.issue_fingerprint,
                      current.issue_fingerprint,
                    )
                  {
                    True ->
                      Error(RecollectionError(
                        "issue_drift",
                        Some("issue fingerprint drifted"),
                      ))
                    False ->
                      case task_ref_matches_issue(run.task_ref, current.issue) {
                        True -> Ok(Nil)
                        False ->
                          Error(RecollectionError(
                            "issue_drift",
                            Some("task identity drifted"),
                          ))
                      }
                  }
              }
          }
      }
  }
}

fn task_ref_matches_issue(
  task_ref: record.TaskRefFields,
  issue: tracker_issue.Issue,
) -> Bool {
  task_ref.task_remote_id == issue.id
  && case task_ref.task_key {
    Some(task_key) -> task_key == issue.identifier
    None -> True
  }
}

fn required_sources(
  dag: workflow_dag.WorkflowDag,
) -> List(OutputSourceRequirement) {
  case dag.contract {
    None -> []
    Some(contract) ->
      contract.outputs
      |> list.fold([], fn(acc, output) {
        case output.source {
          Some(workflow_contract.StepField(step_id, _))
          | Some(workflow_contract.StructuredOutput(step_id, _))
          | Some(workflow_contract.InlineJson(step_id, _)) ->
            insert_requirement(acc, step_id, False)
          Some(workflow_contract.StepFile(step_id, _)) ->
            insert_requirement(acc, step_id, True)
          Some(workflow_contract.StaticUrl(_))
          | Some(workflow_contract.StaticGitRef(_))
          | None -> acc
        }
      })
  }
}

fn insert_requirement(
  requirements: List(OutputSourceRequirement),
  step_id: String,
  needs_workspace: Bool,
) -> List(OutputSourceRequirement) {
  case
    list.find(requirements, fn(requirement) { requirement.step_id == step_id })
  {
    Ok(OutputSourceRequirement(
      step_id: existing_step_id,
      needs_workspace: existing_needs_workspace,
    )) -> {
      let rest =
        list.filter(requirements, fn(requirement) {
          requirement.step_id != existing_step_id
        })
      [
        OutputSourceRequirement(
          step_id: existing_step_id,
          needs_workspace: existing_needs_workspace || needs_workspace,
        ),
        ..rest
      ]
    }
    Error(Nil) -> [
      OutputSourceRequirement(
        step_id: step_id,
        needs_workspace: needs_workspace,
      ),
      ..requirements
    ]
  }
}

fn recover_source_steps(
  requirements: List(OutputSourceRequirement),
  latest_statuses: Dict(String, projection.StepAttemptStatus),
  store: artifact_store.Store,
) -> Result(SourceRecovery, RecollectionError) {
  recover_source_steps_loop(
    requirements,
    latest_statuses,
    store,
    dict.new(),
    dict.new(),
  )
}

fn require_all_steps_completed(
  dag: workflow_dag.WorkflowDag,
  latest_statuses: Dict(String, projection.StepAttemptStatus),
) -> Result(Nil, RecollectionError) {
  dag.steps
  |> list.map(fn(step) { step.id })
  |> require_all_steps_completed_loop(latest_statuses)
}

fn require_all_steps_completed_loop(
  step_ids: List(String),
  latest_statuses: Dict(String, projection.StepAttemptStatus),
) -> Result(Nil, RecollectionError) {
  case step_ids {
    [] -> Ok(Nil)
    [step_id, ..rest] -> {
      use _status <- result.try(require_completed_status(
        step_id,
        latest_statuses,
      ))
      require_all_steps_completed_loop(rest, latest_statuses)
    }
  }
}

fn latest_statuses_by_step(
  statuses: List(projection.StepAttemptStatus),
) -> Dict(String, projection.StepAttemptStatus) {
  list.fold(statuses, dict.new(), fn(acc, status) {
    let step_id = status_step_id(status)
    case dict.get(acc, step_id) {
      Error(Nil) -> dict.insert(acc, step_id, status)
      Ok(existing) ->
        case status_attempt_index(status) > status_attempt_index(existing) {
          True -> dict.insert(acc, step_id, status)
          False -> acc
        }
    }
  })
}

fn recover_source_steps_loop(
  requirements: List(OutputSourceRequirement),
  latest_statuses: Dict(String, projection.StepAttemptStatus),
  store: artifact_store.Store,
  artifacts: Dict(String, step_artifact.StepArtifact),
  statuses: Dict(String, projection.StepAttemptStatus),
) -> Result(SourceRecovery, RecollectionError) {
  case requirements {
    [] -> Ok(SourceRecovery(artifacts, statuses))
    [requirement, ..rest] -> {
      use status <- result.try(require_completed_status(
        requirement.step_id,
        latest_statuses,
      ))
      use artifact <- result.try(recover_artifact(
        requirement.step_id,
        status,
        store,
      ))
      recover_source_steps_loop(
        rest,
        latest_statuses,
        store,
        dict.insert(artifacts, requirement.step_id, artifact),
        dict.insert(statuses, requirement.step_id, status),
      )
    }
  }
}

fn require_completed_status(
  step_id: String,
  latest_statuses: Dict(String, projection.StepAttemptStatus),
) -> Result(projection.StepAttemptStatus, RecollectionError) {
  case dict.get(latest_statuses, step_id) {
    Error(Nil) ->
      Error(RecollectionError(
        "run_not_complete",
        Some("run is missing source step " <> step_id),
      ))
    Ok(status) ->
      case status {
        projection.StepAttemptFinishedStatus(outcome: "completed", ..) ->
          Ok(status)
        projection.StepAttemptFinishedStatus(..) ->
          Error(RecollectionError(
            "source_step_failed",
            Some("source step failed: " <> step_id),
          ))
        projection.StepAttemptPending(..)
        | projection.StepAttemptRunning(..)
        | projection.StepAttemptInterruptedStatus(..)
        | projection.StepAttemptSupersededStatus(..) ->
          Error(RecollectionError(
            "run_not_complete",
            Some("run is not complete: " <> step_id),
          ))
      }
  }
}

fn recover_artifact(
  step_id: String,
  status: projection.StepAttemptStatus,
  store: artifact_store.Store,
) -> Result(step_artifact.StepArtifact, RecollectionError) {
  case status {
    projection.StepAttemptFinishedStatus(
      artifact_ref: artifact_ref,
      artifact_sha256: expected_sha256,
      ..,
    ) ->
      case artifact_store.read_artifact_unverified(store, artifact_ref) {
        Error(error) ->
          Error(RecollectionError(
            "artifact_recovery_failed",
            Some(artifact_recovery_message(
              step_id,
              artifact_ref,
              artifact_read_reason(error),
              Some(expected_sha256),
              None,
            )),
          ))
        Ok(contents) -> {
          let current_sha256 = hash.sha256_hex(contents)
          case current_sha256 == expected_sha256 {
            False ->
              Error(RecollectionError(
                "artifact_recovery_failed",
                Some(artifact_recovery_message(
                  step_id,
                  artifact_ref,
                  "sha_mismatch",
                  Some(expected_sha256),
                  Some(current_sha256),
                )),
              ))
            True ->
              artifact_store.decode_step_artifact_contents(contents)
              |> result.map_error(fn(error) {
                RecollectionError(
                  "artifact_recovery_failed",
                  Some(artifact_recovery_message(
                    step_id,
                    artifact_ref,
                    artifact_read_reason(error),
                    Some(expected_sha256),
                    None,
                  )),
                )
              })
          }
        }
      }
    _ ->
      Error(RecollectionError(
        "run_not_complete",
        Some("run is not complete: " <> step_id),
      ))
  }
}

fn artifact_recovery_message(
  step_id: String,
  artifact_ref: String,
  reason: String,
  expected_sha256: Option(String),
  current_sha256: Option(String),
) -> String {
  "artifact_recovery_failed: step_id="
  <> step_id
  <> " artifact_ref="
  <> artifact_ref
  <> " reason="
  <> reason
  <> optional_detail("expected_sha256", expected_sha256)
  <> optional_detail("current_sha256", current_sha256)
}

fn optional_detail(name: String, value: Option(String)) -> String {
  case value {
    Some(value) -> " " <> name <> "=" <> value
    None -> ""
  }
}

fn artifact_read_reason(error: artifact_store.ArtifactError) -> String {
  case error {
    artifact_store.MissingStepArtifact(_) -> "missing"
    artifact_store.DecodeArtifactFailed(_) -> "invalid_json"
    artifact_store.CorruptStepArtifact(_) -> "sha_mismatch"
    artifact_store.InvalidArtifactRef(_) -> "invalid_ref"
    artifact_store.ArtifactIo(_) -> "unreadable"
    artifact_store.ArtifactWriteFailed(_)
    | artifact_store.DirectorySyncUnsupported(_) -> "read_failed"
  }
}

fn recover_prepared_workspaces(
  dag: workflow_dag.WorkflowDag,
  source_steps: SourceRecovery,
) -> Result(
  Dict(String, workspace_run.PreparedStepWorkspace),
  RecollectionError,
) {
  case dag.contract {
    None -> Ok(dict.new())
    Some(contract) ->
      contract.outputs
      |> list.fold(Ok(dict.new()), fn(acc, output) {
        use prepared <- result.try(acc)
        case output.source {
          Some(workflow_contract.StepFile(step_id, _)) ->
            case
              dict.has_key(
                prepared,
                workspace_name_for_step(dag.steps, step_id),
              )
            {
              True -> Ok(prepared)
              False -> {
                use workspace <- result.try(recover_prepared_workspace(
                  dag.steps,
                  step_id,
                  source_steps.statuses,
                ))
                Ok(dict.insert(prepared, workspace.workspace_name, workspace))
              }
            }
          _ -> Ok(prepared)
        }
      })
  }
}

fn workspace_name_for_step(
  steps: List(workflow_dag.WorkflowStep),
  step_id: String,
) -> String {
  case list.find(steps, fn(step) { step.id == step_id }) {
    Ok(step) -> step.workspace.name
    Error(Nil) -> step_id
  }
}

fn recover_prepared_workspace(
  steps: List(workflow_dag.WorkflowStep),
  step_id: String,
  statuses: Dict(String, projection.StepAttemptStatus),
) -> Result(workspace_run.PreparedStepWorkspace, RecollectionError) {
  use step <- result.try(
    list.find(steps, fn(step) { step.id == step_id })
    |> result.replace_error(RecollectionError(
      "run_not_complete",
      Some("run is missing source step " <> step_id),
    )),
  )
  use status <- result.try(
    dict.get(statuses, step_id)
    |> result.replace_error(RecollectionError(
      "run_not_complete",
      Some("run is missing source step " <> step_id),
    )),
  )
  case status {
    projection.StepAttemptFinishedStatus(
      run_id: run_id,
      workflow_id: workflow_id,
      attempt_index: attempt_index,
      workspace_path: workspace_path,
      run_root: run_root,
      source_workspace_name: source_workspace_name,
      source_workspace_path: source_workspace_path,
      ..,
    ) ->
      case simplifile.is_directory(workspace_path) {
        Ok(True) ->
          Ok(workspace_run.PreparedStepWorkspace(
            workflow_id: workflow_id,
            run_id: run_id,
            run_root: run_root,
            workflow_bundle_dir: "",
            attempt_index: attempt_index,
            workspace_name: step.workspace.name,
            path: workspace_path,
            source_workspace_name: source_workspace_name,
            source_workspace_path: source_workspace_path,
            workspace_profile: "",
          ))
        _ ->
          Error(RecollectionError(
            "workspace_recovery_failed",
            Some(
              "workspace_recovery_failed: step_id="
              <> step_id
              <> " workspace_path="
              <> workspace_path,
            ),
          ))
      }
    _ ->
      Error(RecollectionError(
        "run_not_complete",
        Some("run is not complete: " <> step_id),
      ))
  }
}

fn latest_manifest_is_valid(
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  recorded: workflow_checkpoint.ArtifactWritten,
  checkpoint: workflow_checkpoint.Writer,
) -> Bool {
  case checkpoint.read_artifact(recorded.ref) {
    Error(_) -> False
    Ok(contents) -> {
      let contents_bytes = bit_array.from_string(contents)
      let contents_sha256 = hash.sha256_hex_bytes(contents_bytes)
      let contents_size = bit_array.byte_size(contents_bytes)
      case
        contents_sha256 == recorded.sha256 && contents_size == recorded.bytes
      {
        False -> False
        True ->
          case
            workflow_contract_manifest.decode_output_manifest(contents),
            dag.contract
          {
            Ok(manifest), Some(contract) ->
              manifest.run_id == run_id
              && manifest.workflow_id == dag.id
              && manifest.workflow_fingerprint == workflow_fingerprint
              && manifest.diagnostics == []
              && manifest_outputs_valid(contract.outputs, manifest.outputs)
            Ok(manifest), None ->
              manifest.run_id == run_id
              && manifest.workflow_id == dag.id
              && manifest.workflow_fingerprint == workflow_fingerprint
            _, _ -> False
          }
      }
    }
  }
}

fn manifest_outputs_valid(
  expected: List(workflow_contract.OutputSpec),
  actual: List(workflow_contract_manifest.NamedManifestValue),
) -> Bool {
  list.all(expected, fn(spec) {
    case list.find(actual, fn(named) { named.name == spec.name }) {
      Ok(named) ->
        case
          workflow_contract_manifest.validate_value(
            spec.name,
            named.value,
            required: spec.required,
          )
        {
          Ok(Nil) -> True
          Error(_) -> False
        }
      Error(Nil) -> False
    }
  })
}

fn record_outputs(
  current: CurrentWorkflow,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(Outcome, RecollectionError) {
  use Nil <- result.try(validate_outputs_recordable(
    current,
    run_id,
    checkpoint,
    artifacts,
    prepared_workspaces,
  ))
  case
    contract_io.record_outputs_if_contracted(
      current.dag,
      run_id,
      current.workflow_fingerprint,
      None,
      checkpoint,
      artifacts,
      prepared_workspaces,
    )
  {
    Error(error) ->
      Error(RecollectionError(
        "workflow_output_recollection_failed",
        Some(contract_io_error_message(error)),
      ))
    Ok(result) ->
      case validate_dry_run_outputs(result) {
        Error(error) -> Error(error)
        Ok(Nil) ->
          case result.recorded, result.manifest {
            Some(recorded), Some(manifest) ->
              Ok(Recollected(recorded, manifest))
            _, _ ->
              Error(RecollectionError(
                "workflow_output_recollection_failed",
                Some("workflow output recollection did not record outputs"),
              ))
          }
      }
  }
}

fn validate_outputs_recordable(
  current: CurrentWorkflow,
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(Nil, RecollectionError) {
  let dry_run_checkpoint = validation_checkpoint(checkpoint)
  case
    contract_io.record_outputs_if_contracted(
      current.dag,
      run_id,
      current.workflow_fingerprint,
      None,
      dry_run_checkpoint,
      artifacts,
      prepared_workspaces,
    )
  {
    Error(error) ->
      Error(RecollectionError(
        "workflow_output_recollection_failed",
        Some(contract_io_error_message(error)),
      ))
    Ok(result) -> validate_dry_run_outputs(result)
  }
}

fn validation_checkpoint(
  base: workflow_checkpoint.Writer,
) -> workflow_checkpoint.Writer {
  workflow_checkpoint.Writer(
    ..base,
    write_workflow_outputs_manifest: fn(run_id, _contents) {
      Ok(workflow_checkpoint.ArtifactWritten(
        ref: artifact_store.output_manifest_ref_for_recollection(run_id, 0),
        sha256: "dry-run",
        bytes: 0,
      ))
    },
    workflow_outputs_recorded: fn(_) { Ok(Nil) },
    write_workflow_output_blob: fn(
      write: workflow_checkpoint.WorkflowOutputBlobWrite,
    ) {
      Ok(workflow_checkpoint.ArtifactWritten(
        ref: artifact_store.output_blob_ref_for_recollection(
          write.run_id,
          write.output_name,
          write.extension,
          0,
        ),
        sha256: "dry-run",
        bytes: 0,
      ))
    },
  )
}

fn validate_dry_run_outputs(
  result: contract_io.ContractOutputsResult,
) -> Result(Nil, RecollectionError) {
  case result.manifest {
    None ->
      Error(RecollectionError(
        "workflow_output_recollection_failed",
        Some("workflow output recollection did not produce a manifest"),
      ))
    Some(manifest) ->
      case selected_failure_diagnostic(manifest.diagnostics) {
        Some(diagnostic) ->
          Error(recollection_failure_from_diagnostic(diagnostic))
        None ->
          case result.missing {
            [] -> Ok(Nil)
            [first, ..] -> Error(recollection_failure_from_diagnostic(first))
          }
      }
  }
}

fn selected_failure_diagnostic(diagnostics: List(String)) -> Option(String) {
  case diagnostics {
    [] -> None
    _ -> last_diagnostic(diagnostics)
  }
}

fn last_diagnostic(diagnostics: List(String)) -> Option(String) {
  case diagnostics {
    [] -> None
    [diagnostic] -> Some(diagnostic)
    [_, ..rest] -> last_diagnostic(rest)
  }
}

fn recollection_failure_from_diagnostic(
  diagnostic: String,
) -> RecollectionError {
  RecollectionError(diagnostic_code(diagnostic), Some(diagnostic))
}

fn diagnostic_code(diagnostic: String) -> String {
  case string.split_once(diagnostic, on: ":") {
    Ok(#(code, _)) -> code
    Error(Nil) -> "workflow_output_recollection_failed"
  }
}

fn contract_io_error_message(
  error: contract_io_error.ContractIoError,
) -> String {
  case error {
    contract_io_error.RequiredInputMissing(name) ->
      "required input missing: " <> name
    contract_io_error.RequiredContextMissing(name) ->
      "required context missing: " <> name
    contract_io_error.ContractTypeMismatch(name) ->
      "contract type mismatch: " <> name
    contract_io_error.ContractArtifactTypeMismatch(name) ->
      "contract artifact type mismatch: " <> name
    contract_io_error.OutputManifestDecodeFailed(ref) ->
      "output manifest decode failed: " <> ref
    contract_io_error.InputManifestWriteFailed(error)
    | contract_io_error.InputManifestRecordFailed(error)
    | contract_io_error.OutputManifestReadFailed(error)
    | contract_io_error.OutputManifestWriteFailed(error)
    | contract_io_error.OutputManifestRecordFailed(error) ->
      workflow_checkpoint.describe_error(error)
  }
}

fn manifest_ref_to_written(
  manifest: projection.WorkflowContractManifestRef,
) -> workflow_checkpoint.ArtifactWritten {
  workflow_checkpoint.ArtifactWritten(
    ref: manifest.artifact_ref,
    sha256: manifest.artifact_sha256,
    bytes: manifest.artifact_bytes,
  )
}

fn status_step_id(status: projection.StepAttemptStatus) -> String {
  case status {
    projection.StepAttemptPending(step_id: step_id, ..)
    | projection.StepAttemptRunning(step_id: step_id, ..)
    | projection.StepAttemptFinishedStatus(step_id: step_id, ..)
    | projection.StepAttemptInterruptedStatus(step_id: step_id, ..)
    | projection.StepAttemptSupersededStatus(step_id: step_id, ..) -> step_id
  }
}

fn status_attempt_index(status: projection.StepAttemptStatus) -> Int {
  case status {
    projection.StepAttemptPending(attempt_index: attempt_index, ..)
    | projection.StepAttemptRunning(attempt_index: attempt_index, ..)
    | projection.StepAttemptFinishedStatus(attempt_index: attempt_index, ..)
    | projection.StepAttemptInterruptedStatus(attempt_index: attempt_index, ..)
    | projection.StepAttemptSupersededStatus(attempt_index: attempt_index, ..) ->
      attempt_index
  }
}
