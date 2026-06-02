import scherzo/workflow_checkpoint

pub type ContractIoError {
  RequiredInputMissing(name: String)
  RequiredContextMissing(name: String)
  ContractTypeMismatch(name: String)
  ContractArtifactTypeMismatch(name: String)
  InputManifestWriteFailed(workflow_checkpoint.CheckpointError)
  InputManifestRecordFailed(workflow_checkpoint.CheckpointError)
  OutputManifestReadFailed(workflow_checkpoint.CheckpointError)
  OutputManifestDecodeFailed(ref: String)
  OutputManifestWriteFailed(workflow_checkpoint.CheckpointError)
  OutputManifestRecordFailed(workflow_checkpoint.CheckpointError)
}

pub fn describe_error(error: ContractIoError) -> String {
  case error {
    RequiredInputMissing(name) -> "workflow_required_input_missing:" <> name
    RequiredContextMissing(name) -> "workflow_required_context_missing:" <> name
    ContractTypeMismatch(name) -> "workflow_contract_type_mismatch:" <> name
    ContractArtifactTypeMismatch(name) ->
      "workflow_contract_artifact_type_mismatch:" <> name
    OutputManifestDecodeFailed(ref) ->
      "workflow_output_manifest_decode_failed:" <> ref
    InputManifestWriteFailed(error)
    | InputManifestRecordFailed(error)
    | OutputManifestReadFailed(error)
    | OutputManifestWriteFailed(error)
    | OutputManifestRecordFailed(error) ->
      workflow_checkpoint.describe_error(error)
  }
}
