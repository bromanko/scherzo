import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/config/types as config_types
import scherzo/json_value
import scherzo/path
import scherzo/step_artifact
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest as contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_run/contract_io_error as contract_error
import scherzo/workflow_run/output_contract_descriptor
import scherzo/workspace_profile
import scherzo/workspace_run
import simplifile

pub type ContractRunValues {
  ContractRunValues(
    inputs: Dict(String, contract_manifest.ManifestValue),
    context: Dict(String, contract_manifest.ManifestValue),
  )
}

pub type ScheduledInvocationContext {
  ScheduledInvocationContext(
    job_id: String,
    workflow_id: String,
    due_at: String,
    started_at: String,
    run_id: String,
    attempt: Int,
  )
}

pub type RunInvocation {
  RunInvocation(
    run_id: String,
    workflow_fingerprint: String,
    supplied_contract_values: ContractRunValues,
    scheduled_context: Option(ScheduledInvocationContext),
  )
}

pub type RecoveredInvocation {
  RecoveredInvocation(
    run_id: String,
    workflow_fingerprint: String,
    steps_started: Bool,
    contract_inputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  )
}

pub type ContractOutputsResult {
  ContractOutputsResult(
    missing: List(String),
    manifest: Option(contract_manifest.ContractOutputManifest),
    recorded: Option(workflow_checkpoint.ArtifactWritten),
  )
}

type ContractValueKind {
  InputValue
  ContextValue
}

pub fn record_recovered_inputs_if_contracted(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  recovered: RecoveredInvocation,
  checkpoint: workflow_checkpoint.Writer,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, contract_error.ContractIoError) {
  case recovered.contract_inputs_recorded, dag.contract {
    Some(_), _ | _, None -> Ok(Nil)
    None, Some(contract) ->
      case recovered.steps_started {
        False ->
          record_inputs_if_contracted(
            issue,
            dag,
            orchestrator,
            RunInvocation(
              run_id: recovered.run_id,
              workflow_fingerprint: recovered.workflow_fingerprint,
              supplied_contract_values: ContractRunValues(
                inputs: dict.new(),
                context: dict.new(),
              ),
              scheduled_context: None,
            ),
            checkpoint,
            profile,
          )
        True -> {
          let diagnostic = "recovered_after_steps_started"
          let manifest =
            contract_manifest.ContractInputManifest(
              run_id: recovered.run_id,
              workflow_id: dag.id,
              workflow_fingerprint: recovered.workflow_fingerprint,
              inputs: list.map(
                contract.inputs,
                recovered_input_value(diagnostic),
              ),
              context: list.map(
                contract.context,
                recovered_context_value(diagnostic),
              ),
              diagnostics: [diagnostic],
            )
          write_recorded_input_manifest(manifest, checkpoint)
        }
      }
  }
}

pub fn record_inputs_if_contracted(
  issue: tracker_issue.Issue,
  dag: workflow_dag.WorkflowDag,
  orchestrator: config_types.OrchestratorConfig,
  invocation: RunInvocation,
  checkpoint: workflow_checkpoint.Writer,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, contract_error.ContractIoError) {
  case dag.contract {
    None -> Ok(Nil)
    Some(contract) -> {
      use inputs <- result.try(
        resolve_contract_inputs(contract.inputs, issue, invocation, []),
      )
      use context <- result.try(
        resolve_contract_context(
          contract.context,
          invocation,
          orchestrator,
          profile,
          [],
        ),
      )
      let manifest =
        contract_manifest.ContractInputManifest(
          run_id: invocation.run_id,
          workflow_id: dag.id,
          workflow_fingerprint: invocation.workflow_fingerprint,
          inputs: inputs,
          context: context,
          diagnostics: [],
        )
      write_recorded_input_manifest(manifest, checkpoint)
    }
  }
}

pub fn record_outputs_if_contracted(
  dag: workflow_dag.WorkflowDag,
  run_id: String,
  workflow_fingerprint: String,
  contract_outputs_recorded: Option(workflow_checkpoint.ArtifactWritten),
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> Result(ContractOutputsResult, contract_error.ContractIoError) {
  case dag.contract, contract_outputs_recorded {
    None, _ -> Ok(ContractOutputsResult([], None, None))
    Some(_), Some(recorded) -> {
      use contents <- result.try(
        checkpoint.read_artifact(recorded.ref)
        |> result.map_error(contract_error.OutputManifestReadFailed),
      )
      use manifest <- result.try(
        contract_manifest.decode_output_manifest(contents)
        |> result.replace_error(contract_error.OutputManifestDecodeFailed(
          recorded.ref,
        )),
      )
      Ok(ContractOutputsResult([], Some(manifest), Some(recorded)))
    }
    Some(contract), None -> {
      let #(values, diagnostics, missing) =
        resolve_contract_outputs(
          contract.outputs,
          dag.steps,
          run_id,
          checkpoint,
          artifacts,
          prepared_workspaces,
          [],
          [],
          [],
        )
      let manifest =
        contract_manifest.ContractOutputManifest(
          run_id: run_id,
          workflow_id: dag.id,
          workflow_fingerprint: workflow_fingerprint,
          outputs: values,
          diagnostics: diagnostics,
        )
      let contents = contract_manifest.output_manifest_to_string(manifest)
      use written <- result.try(
        checkpoint.write_workflow_outputs_manifest(run_id, contents)
        |> result.map_error(contract_error.OutputManifestWriteFailed),
      )
      use Nil <- result.try(
        checkpoint.workflow_outputs_recorded(
          workflow_checkpoint.WorkflowContractManifestRecorded(
            run_id: run_id,
            workflow_id: dag.id,
            workflow_fingerprint: workflow_fingerprint,
            artifact: written,
          ),
        )
        |> result.map_error(contract_error.OutputManifestRecordFailed),
      )
      Ok(ContractOutputsResult(
        missing: list.reverse(missing),
        manifest: Some(manifest),
        recorded: Some(written),
      ))
    }
  }
}

fn recovered_input_value(
  diagnostic: String,
) -> fn(workflow_contract.InputSpec) -> contract_manifest.NamedManifestValue {
  fn(spec: workflow_contract.InputSpec) {
    contract_manifest.NamedManifestValue(
      name: spec.name,
      value: contract_manifest.absent(spec.type_, Some(diagnostic)),
    )
  }
}

fn recovered_context_value(
  diagnostic: String,
) -> fn(workflow_contract.ContextSpec) -> contract_manifest.NamedManifestValue {
  fn(spec: workflow_contract.ContextSpec) {
    contract_manifest.NamedManifestValue(
      name: spec.name,
      value: contract_manifest.absent(spec.type_, Some(diagnostic)),
    )
  }
}

fn write_recorded_input_manifest(
  manifest: contract_manifest.ContractInputManifest,
  checkpoint: workflow_checkpoint.Writer,
) -> Result(Nil, contract_error.ContractIoError) {
  let contents = contract_manifest.input_manifest_to_string(manifest)
  use written <- result.try(
    checkpoint.write_workflow_inputs_manifest(manifest.run_id, contents)
    |> result.map_error(contract_error.InputManifestWriteFailed),
  )
  checkpoint.workflow_inputs_recorded(
    workflow_checkpoint.WorkflowContractManifestRecorded(
      run_id: manifest.run_id,
      workflow_id: manifest.workflow_id,
      workflow_fingerprint: manifest.workflow_fingerprint,
      artifact: written,
    ),
  )
  |> result.map_error(contract_error.InputManifestRecordFailed)
}

fn resolve_contract_inputs(
  inputs: List(workflow_contract.InputSpec),
  issue: tracker_issue.Issue,
  invocation: RunInvocation,
  acc: List(contract_manifest.NamedManifestValue),
) -> Result(
  List(contract_manifest.NamedManifestValue),
  contract_error.ContractIoError,
) {
  case inputs {
    [] -> Ok(list.reverse(acc))
    [spec, ..rest] -> {
      use value <- result.try(resolve_contract_input(spec, issue, invocation))
      resolve_contract_inputs(rest, issue, invocation, [
        contract_manifest.NamedManifestValue(name: spec.name, value: value),
        ..acc
      ])
    }
  }
}

fn resolve_contract_context(
  context: List(workflow_contract.ContextSpec),
  invocation: RunInvocation,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  acc: List(contract_manifest.NamedManifestValue),
) -> Result(
  List(contract_manifest.NamedManifestValue),
  contract_error.ContractIoError,
) {
  case context {
    [] -> Ok(list.reverse(acc))
    [spec, ..rest] -> {
      use value <- result.try(resolve_contract_context_value(
        spec,
        invocation,
        orchestrator,
        profile,
      ))
      resolve_contract_context(rest, invocation, orchestrator, profile, [
        contract_manifest.NamedManifestValue(name: spec.name, value: value),
        ..acc
      ])
    }
  }
}

fn resolve_contract_input(
  spec: workflow_contract.InputSpec,
  issue: tracker_issue.Issue,
  invocation: RunInvocation,
) -> Result(contract_manifest.ManifestValue, contract_error.ContractIoError) {
  case spec.source {
    Some(workflow_contract.IssueContext) ->
      Ok(inline_value(spec.type_, json_value.JString(issue_context_text(issue))))
    Some(workflow_contract.ScheduledContext) ->
      case invocation.scheduled_context {
        Some(scheduled) ->
          Ok(inline_value(spec.type_, scheduled_context_json(scheduled)))
        None -> Error(contract_error.RequiredInputMissing(spec.name))
      }
    Some(workflow_contract.LiteralInput(value)) ->
      Ok(inline_value(spec.type_, json_value.JString(value)))
    Some(workflow_contract.MappedOutputSource) ->
      mapped_contract_value(
        spec.name,
        spec.type_,
        spec.required,
        invocation.supplied_contract_values.inputs,
        InputValue,
      )
    None -> optional_or_missing_input(spec)
  }
}

fn resolve_contract_context_value(
  spec: workflow_contract.ContextSpec,
  invocation: RunInvocation,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(contract_manifest.ManifestValue, contract_error.ContractIoError) {
  case spec.source {
    Some(workflow_contract.WorkspaceDriverBase) ->
      case workspace_driver_base(orchestrator, profile) {
        Some(value) -> Ok(inline_value(spec.type_, json_value.JString(value)))
        None -> optional_or_missing_context(spec)
      }
    Some(workflow_contract.LiteralContext(value)) ->
      Ok(inline_value(spec.type_, json_value.JString(value)))
    Some(workflow_contract.MappedOutputContext) ->
      mapped_contract_value(
        spec.name,
        spec.type_,
        spec.required,
        invocation.supplied_contract_values.context,
        ContextValue,
      )
    None -> optional_or_missing_context(spec)
  }
}

fn mapped_contract_value(
  name: String,
  expected_type: workflow_contract.ContractType,
  required: Bool,
  values: Dict(String, contract_manifest.ManifestValue),
  kind: ContractValueKind,
) -> Result(contract_manifest.ManifestValue, contract_error.ContractIoError) {
  case dict.get(values, name) {
    Ok(value) ->
      case contract_manifest.type_matches(value, expected_type) {
        False -> Error(contract_error.ContractTypeMismatch(name))
        True ->
          case contract_manifest.artifact_type_matches(value, expected_type) {
            True -> Ok(value)
            False -> Error(contract_error.ContractArtifactTypeMismatch(name))
          }
      }
    Error(Nil) ->
      case required {
        True ->
          case kind {
            InputValue -> Error(contract_error.RequiredInputMissing(name))
            ContextValue -> Error(contract_error.RequiredContextMissing(name))
          }
        False -> {
          let diagnostic = case kind {
            InputValue -> "optional mapped input not supplied"
            ContextValue -> "optional mapped context not supplied"
          }
          Ok(contract_manifest.absent(expected_type, Some(diagnostic)))
        }
      }
  }
}

fn optional_or_missing_input(
  spec: workflow_contract.InputSpec,
) -> Result(contract_manifest.ManifestValue, contract_error.ContractIoError) {
  case spec.required {
    True -> Error(contract_error.RequiredInputMissing(spec.name))
    False ->
      Ok(contract_manifest.absent(
        spec.type_,
        Some("optional input source absent"),
      ))
  }
}

fn optional_or_missing_context(
  spec: workflow_contract.ContextSpec,
) -> Result(contract_manifest.ManifestValue, contract_error.ContractIoError) {
  case spec.required {
    True -> Error(contract_error.RequiredContextMissing(spec.name))
    False ->
      Ok(contract_manifest.absent(
        spec.type_,
        Some("optional context source absent"),
      ))
  }
}

fn inline_value(
  type_: workflow_contract.ContractType,
  value: json_value.JsonValue,
) -> contract_manifest.ManifestValue {
  contract_manifest.present_inline_json(type_, value, None)
}

fn issue_context_text(issue: tracker_issue.Issue) -> String {
  let description = option.unwrap(issue.description, "")
  issue.identifier <> "\n" <> issue.title <> "\n\n" <> description
}

fn scheduled_context_json(
  scheduled: ScheduledInvocationContext,
) -> json_value.JsonValue {
  json_value.JObject([
    #("job_id", json_value.JString(scheduled.job_id)),
    #("workflow_id", json_value.JString(scheduled.workflow_id)),
    #("due_at", json_value.JString(scheduled.due_at)),
    #("started_at", json_value.JString(scheduled.started_at)),
    #("run_id", json_value.JString(scheduled.run_id)),
    #("attempt", json_value.JInt(scheduled.attempt)),
  ])
}

fn workspace_driver_base(
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Option(String) {
  let context = workspace_profile.driver_context(profile, orchestrator)
  env_lookup(workspace_profile.driver_context_env_vars(context), [
    "SCHERZO_WORKSPACE_DRIVER_BASE",
    "SCHERZO_BASE_REF",
  ])
}

fn env_lookup(
  env: List(#(String, String)),
  names: List(String),
) -> Option(String) {
  case names {
    [] -> None
    [name, ..rest] ->
      case list.key_find(env, name) {
        Ok(value) -> Some(value)
        Error(Nil) -> env_lookup(env, rest)
      }
  }
}

fn resolve_contract_outputs(
  outputs: List(workflow_contract.OutputSpec),
  steps: List(workflow_dag.WorkflowStep),
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  acc: List(contract_manifest.NamedManifestValue),
  diagnostics: List(String),
  missing: List(String),
) -> #(List(contract_manifest.NamedManifestValue), List(String), List(String)) {
  case outputs {
    [] -> #(list.reverse(acc), list.reverse(diagnostics), missing)
    [spec, ..rest] -> {
      let #(value, output_diagnostics, output_missing) =
        materialize_output(
          spec,
          steps,
          run_id,
          checkpoint,
          artifacts,
          prepared_workspaces,
        )
      let diagnostics =
        list.append(list.reverse(output_diagnostics), diagnostics)
      let missing = case output_missing {
        True -> [spec.name, ..missing]
        False -> missing
      }
      resolve_contract_outputs(
        rest,
        steps,
        run_id,
        checkpoint,
        artifacts,
        prepared_workspaces,
        [
          contract_manifest.NamedManifestValue(name: spec.name, value: value),
          ..acc
        ],
        diagnostics,
        missing,
      )
    }
  }
}

fn materialize_output(
  spec: workflow_contract.OutputSpec,
  steps: List(workflow_dag.WorkflowStep),
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> #(contract_manifest.ManifestValue, List(String), Bool) {
  let #(value, diagnostics) = case spec.source {
    None -> #(
      contract_manifest.absent(spec.type_, Some("output source absent")),
      [
        "workflow_output_source_absent:" <> spec.name,
      ],
    )
    Some(source) ->
      output_value_from_source(
        spec,
        source,
        steps,
        run_id,
        checkpoint,
        artifacts,
        prepared_workspaces,
      )
  }
  case
    contract_manifest.validate_value(spec.name, value, required: spec.required)
  {
    Ok(Nil) -> #(value, diagnostics, False)
    Error(contract_manifest.ManifestError(_, message)) -> {
      let diagnostics = [message, ..diagnostics]
      let missing = spec.required
      #(value, diagnostics, missing)
    }
  }
}

fn output_value_from_source(
  spec: workflow_contract.OutputSpec,
  source: workflow_contract.OutputSource,
  steps: List(workflow_dag.WorkflowStep),
  run_id: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
) -> #(contract_manifest.ManifestValue, List(String)) {
  case source {
    workflow_contract.StepField(step_id, field) ->
      output_value_from_step_field(
        spec,
        run_id,
        step_id,
        field,
        checkpoint,
        artifacts,
      )
    workflow_contract.StepFile(step_id, file_path) ->
      output_value_from_step_file(
        spec,
        run_id,
        step_id,
        file_path,
        checkpoint,
        artifacts,
        prepared_workspaces,
        steps,
      )
    workflow_contract.StructuredOutput(step_id, artifact_name) ->
      output_value_from_structured_output(
        spec,
        run_id,
        step_id,
        artifact_name,
        artifacts,
        checkpoint,
        False,
      )
    workflow_contract.InlineJson(step_id, artifact_name) ->
      output_value_from_structured_output(
        spec,
        run_id,
        step_id,
        artifact_name,
        artifacts,
        checkpoint,
        True,
      )
    workflow_contract.StaticUrl(url) -> #(
      contract_manifest.present_url_with_source(
        spec.type_,
        url,
        output_contract_descriptor.source_for_descriptor(spec),
      ),
      [],
    )
    workflow_contract.StaticGitRef(ref) -> #(
      contract_manifest.present_git_ref_with_source(
        spec.type_,
        ref,
        output_contract_descriptor.source_for_descriptor(spec),
      ),
      [],
    )
  }
}

type OutputText {
  OutputText(contents: String, truncated: Bool)
}

fn output_value_from_step_field(
  spec: workflow_contract.OutputSpec,
  run_id: String,
  step_id: String,
  field: workflow_contract.OutputField,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
) -> #(contract_manifest.ManifestValue, List(String)) {
  case dict.get(artifacts, step_id) {
    Error(Nil) ->
      output_absent(spec, "workflow_output_source_step_missing:" <> step_id)
    Ok(step_artifact.StepArtifact(status: step_artifact.StepFailed, ..)) ->
      output_absent(spec, "workflow_output_source_step_failed:" <> step_id)
    Ok(artifact) ->
      case artifact_field_text(artifact, field) {
        None ->
          output_absent(
            spec,
            "workflow_output_source_field_missing:" <> spec.name,
          )
        Some(OutputText(contents: contents, truncated: truncated)) ->
          write_contract_output_blob(
            spec,
            run_id,
            contents,
            truncated,
            checkpoint,
            step_field_source_json(step_id, field),
          )
      }
  }
}

fn output_value_from_step_file(
  spec: workflow_contract.OutputSpec,
  run_id: String,
  step_id: String,
  file_path: String,
  checkpoint: workflow_checkpoint.Writer,
  artifacts: Dict(String, step_artifact.StepArtifact),
  prepared_workspaces: Dict(String, workspace_run.PreparedStepWorkspace),
  steps: List(workflow_dag.WorkflowStep),
) -> #(contract_manifest.ManifestValue, List(String)) {
  case dict.get(artifacts, step_id) {
    Error(Nil) ->
      output_absent(spec, "workflow_output_source_step_missing:" <> step_id)
    Ok(step_artifact.StepArtifact(status: step_artifact.StepFailed, ..)) ->
      output_absent(spec, "workflow_output_source_step_failed:" <> step_id)
    Ok(_) ->
      case list.find(steps, fn(step) { step.id == step_id }) {
        Error(Nil) ->
          output_absent(spec, "workflow_output_source_step_missing:" <> step_id)
        Ok(step) ->
          case dict.get(prepared_workspaces, step.workspace.name) {
            Error(Nil) ->
              output_absent(
                spec,
                "workflow_output_source_workspace_missing:" <> step_id,
              )
            Ok(workspace) -> {
              let source_path = path.join(workspace.path, file_path)
              case simplifile.read(source_path) {
                Error(error) ->
                  output_absent(
                    spec,
                    "workflow_output_source_file_missing:"
                      <> spec.name
                      <> ":"
                      <> simplifile.describe_error(error),
                  )
                Ok(contents) ->
                  write_contract_output_blob(
                    spec,
                    run_id,
                    contents,
                    False,
                    checkpoint,
                    step_file_source_json(step_id, file_path),
                  )
              }
            }
          }
      }
  }
}

type OutputValidationError {
  OutputJsonSourceTruncated(output_name: String)
  OutputJsonInvalid(output_name: String)
}

fn write_contract_output_blob(
  spec: workflow_contract.OutputSpec,
  run_id: String,
  contents: String,
  truncated: Bool,
  checkpoint: workflow_checkpoint.Writer,
  source: json_value.JsonValue,
) -> #(contract_manifest.ManifestValue, List(String)) {
  case validate_output_contents(spec, contents, truncated) {
    Error(error) -> output_absent(spec, output_validation_diagnostic(error))
    Ok(Nil) -> {
      let #(extension, media_type) =
        output_contract_descriptor.extension_and_media(spec)
      let write =
        workflow_checkpoint.WorkflowOutputBlobWrite(
          run_id: run_id,
          output_name: spec.name,
          extension: extension,
          contents: contents,
        )
      case checkpoint.write_workflow_output_blob(write) {
        Error(error) ->
          output_absent(
            spec,
            "workflow_output_blob_failed:"
              <> workflow_checkpoint.describe_error(error),
          )
        Ok(written) -> #(
          contract_manifest.present_run_artifact(
            spec.type_,
            contract_manifest.ArtifactWritten(
              ref: written.ref,
              sha256: written.sha256,
              bytes: written.bytes,
            ),
            media_type,
            Some(output_contract_descriptor.source_with_descriptor(spec, source)),
          ),
          [],
        )
      }
    }
  }
}

fn validate_output_contents(
  spec: workflow_contract.OutputSpec,
  contents: String,
  truncated: Bool,
) -> Result(Nil, OutputValidationError) {
  case output_type_is_json(spec.type_) {
    False -> Ok(Nil)
    True ->
      case truncated {
        True -> Error(OutputJsonSourceTruncated(spec.name))
        False ->
          case json_value.parse(contents) {
            Ok(_) -> Ok(Nil)
            Error(Nil) -> Error(OutputJsonInvalid(spec.name))
          }
      }
  }
}

fn output_validation_diagnostic(error: OutputValidationError) -> String {
  case error {
    OutputJsonSourceTruncated(output_name) ->
      "workflow_output_json_source_truncated:" <> output_name
    OutputJsonInvalid(output_name) ->
      "workflow_output_json_invalid:" <> output_name
  }
}

fn output_value_from_structured_output(
  spec: workflow_contract.OutputSpec,
  run_id: String,
  step_id: String,
  artifact_name: String,
  artifacts: Dict(String, step_artifact.StepArtifact),
  checkpoint: workflow_checkpoint.Writer,
  inline: Bool,
) -> #(contract_manifest.ManifestValue, List(String)) {
  let missing_diagnostic =
    "workflow_output_structured_artifact_missing:" <> artifact_name
  case dict.get(artifacts, step_id) {
    Error(Nil) ->
      output_absent(spec, "workflow_output_source_step_missing:" <> step_id)
    Ok(step_artifact.StepArtifact(status: step_artifact.StepFailed, ..)) ->
      output_absent(spec, "workflow_output_source_step_failed:" <> step_id)
    Ok(artifact) ->
      case artifact.structured_output {
        Some(step_artifact.StructuredOutputValid(metadata)) ->
          case metadata.artifact_name == artifact_name {
            True ->
              case inline {
                True ->
                  inline_structured_output_value(
                    spec,
                    step_id,
                    metadata,
                    checkpoint,
                  )
                False ->
                  case spec.type_ {
                    workflow_contract.CommitStack ->
                      structured_output_payload_blob(
                        spec,
                        run_id,
                        step_id,
                        metadata,
                        checkpoint,
                      )
                    _ -> #(
                      contract_manifest.present_run_artifact(
                        spec.type_,
                        contract_manifest.ArtifactWritten(
                          ref: metadata.ref,
                          sha256: metadata.sha256,
                          bytes: metadata.bytes,
                        ),
                        "application/json",
                        Some(output_contract_descriptor.source_with_descriptor(
                          spec,
                          structured_source_json(step_id, artifact_name),
                        )),
                      ),
                      [],
                    )
                  }
              }
            False -> output_absent(spec, missing_diagnostic)
          }
        _ -> output_absent(spec, missing_diagnostic)
      }
  }
}

fn structured_output_payload_blob(
  spec: workflow_contract.OutputSpec,
  run_id: String,
  step_id: String,
  metadata: step_artifact.StructuredOutputMetadata,
  checkpoint: workflow_checkpoint.Writer,
) -> #(contract_manifest.ManifestValue, List(String)) {
  case checkpoint.read_artifact(metadata.ref) {
    Error(_error) ->
      output_absent(
        spec,
        "workflow_output_structured_payload_read_failed:" <> spec.name,
      )
    Ok(contents) ->
      case json_value.parse(contents) {
        Error(Nil) ->
          output_absent(
            spec,
            "workflow_output_structured_payload_decode_failed:" <> spec.name,
          )
        Ok(parsed) ->
          case object_field(parsed, "payload") {
            None ->
              output_absent(
                spec,
                "workflow_output_structured_payload_missing:" <> spec.name,
              )
            Some(payload) ->
              write_contract_output_blob(
                spec,
                run_id,
                json_value.to_string(payload),
                False,
                checkpoint,
                structured_source_json(step_id, metadata.artifact_name),
              )
          }
      }
  }
}

fn inline_structured_output_value(
  spec: workflow_contract.OutputSpec,
  step_id: String,
  metadata: step_artifact.StructuredOutputMetadata,
  checkpoint: workflow_checkpoint.Writer,
) -> #(contract_manifest.ManifestValue, List(String)) {
  case checkpoint.read_artifact(metadata.ref) {
    Error(_error) ->
      output_absent(
        spec,
        "workflow_output_inline_json_read_failed:" <> spec.name,
      )
    Ok(contents) ->
      case json_value.parse(contents) {
        Error(Nil) ->
          output_absent(
            spec,
            "workflow_output_inline_json_decode_failed:" <> spec.name,
          )
        Ok(parsed) -> {
          let value = object_field(parsed, "payload") |> option.unwrap(parsed)
          #(
            contract_manifest.present_inline_json(
              spec.type_,
              value,
              Some(output_contract_descriptor.source_with_descriptor(
                spec,
                structured_source_json(step_id, metadata.artifact_name),
              )),
            ),
            [],
          )
        }
      }
  }
}

fn output_absent(
  spec: workflow_contract.OutputSpec,
  diagnostic: String,
) -> #(contract_manifest.ManifestValue, List(String)) {
  #(contract_manifest.absent(spec.type_, Some(diagnostic)), [diagnostic])
}

fn artifact_field_text(
  artifact: step_artifact.StepArtifact,
  field: workflow_contract.OutputField,
) -> Option(OutputText) {
  case field {
    workflow_contract.Stdout ->
      Some(OutputText(artifact.stdout, artifact.stdout_truncated))
    workflow_contract.FinalResponse ->
      artifact.final_response
      |> option.map(fn(contents) {
        OutputText(
          contents: contents,
          truncated: artifact.final_response_truncated,
        )
      })
  }
}

fn output_type_is_json(type_: workflow_contract.ContractType) -> Bool {
  case type_ {
    workflow_contract.CodeChange
    | workflow_contract.ExecPlanBundle
    | workflow_contract.ImplementationPack
    | workflow_contract.CodeChangeBundle
    | workflow_contract.CommitStack
    | workflow_contract.ArtifactList -> True
    workflow_contract.DocumentMarkdown
    | workflow_contract.ExecPlan
    | workflow_contract.Text
    | workflow_contract.Url
    | workflow_contract.GitRef -> False
  }
}

fn step_field_source_json(
  step_id: String,
  field: workflow_contract.OutputField,
) -> json_value.JsonValue {
  json_value.JObject([
    #("step_id", json_value.JString(step_id)),
    #(
      "field",
      json_value.JString(workflow_contract.output_field_to_string(field)),
    ),
  ])
}

fn step_file_source_json(
  step_id: String,
  file_path: String,
) -> json_value.JsonValue {
  json_value.JObject([
    #("step_id", json_value.JString(step_id)),
    #("path", json_value.JString(file_path)),
  ])
}

fn structured_source_json(
  step_id: String,
  artifact_name: String,
) -> json_value.JsonValue {
  json_value.JObject([
    #("step_id", json_value.JString(step_id)),
    #("artifact_name", json_value.JString(artifact_name)),
  ])
}

fn object_field(
  value: json_value.JsonValue,
  key: String,
) -> Option(json_value.JsonValue) {
  case value {
    json_value.JObject(entries) -> object_field_entries(entries, key)
    _ -> None
  }
}

fn object_field_entries(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> object_field_entries(rest, key)
      }
  }
}
