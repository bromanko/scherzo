import gleam/json
import gleam/option.{None, Some}
import scherzo/json_value
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest as manifest

pub fn validates_run_artifact_url_and_git_ref_values_test() {
  let run_artifact =
    manifest.present_run_artifact(
      workflow_contract.DocumentMarkdown,
      manifest.ArtifactWritten(
        ref: "runs/run-1/outputs/findings.md",
        sha256: "abc",
        bytes: 12,
      ),
      "text/markdown",
      None,
    )
  assert manifest.validate_value("findings", run_artifact, required: True)
    == Ok(Nil)

  let url =
    manifest.present_url(workflow_contract.Url, "https://example.invalid/pr/1")
  assert manifest.validate_value("pr", url, required: True) == Ok(Nil)

  let git_ref =
    manifest.present_git_ref(workflow_contract.GitRef, "feature/liv-292")
  assert manifest.validate_value("base_ref", git_ref, required: True) == Ok(Nil)
}

pub fn rejects_absolute_local_path_artifact_refs_test() {
  let value =
    manifest.present_run_artifact(
      workflow_contract.DocumentMarkdown,
      manifest.ArtifactWritten(
        ref: "<absolute-local-path>/findings.md",
        sha256: "abc",
        bytes: 12,
      ),
      "text/markdown",
      None,
    )
  let assert Error(manifest.ManifestError(code, _)) =
    manifest.validate_value("findings", value, required: True)
  assert code == "manifest_invalid_run_artifact_ref"
}

pub fn absent_optional_value_round_trips_test() {
  let value =
    manifest.absent(
      workflow_contract.ArtifactList,
      Some("optional attachments absent"),
    )
  let encoded = manifest.manifest_value_to_json(value)
  let assert Ok(decoded) =
    manifest.decode_manifest_value(json.to_string(encoded))
  assert decoded.status == manifest.Absent
  assert decoded.type_ == workflow_contract.ArtifactList
  assert decoded.diagnostic == Some("optional attachments absent")
}

pub fn inline_json_code_change_requires_durable_pointer_test() {
  let invalid =
    manifest.present_inline_json(
      workflow_contract.CodeChange,
      json_value.JObject([#("notes", json_value.JString("done"))]),
      None,
    )
  let assert Error(manifest.ManifestError(code, _)) =
    manifest.validate_required_output_value("code_change", invalid)
  assert code == "manifest_code_change_missing_reference"

  let valid =
    manifest.present_inline_json(
      workflow_contract.CodeChange,
      json_value.JObject([#("branch", json_value.JString("feature/liv-292"))]),
      None,
    )
  assert manifest.validate_required_output_value("code_change", valid)
    == Ok(Nil)
}

pub fn manifest_decoders_reject_wrong_header_test() {
  let wrong_input_version =
    "{\"schema_version\":2,\"artifact_type\":\"workflow_contract_inputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"inputs\":[],\"context\":[] }"
  assert manifest.decode_input_manifest(wrong_input_version) == Error(Nil)

  let wrong_output_type =
    "{\"schema_version\":1,\"artifact_type\":\"workflow_contract_inputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"outputs\":[] }"
  assert manifest.decode_output_manifest(wrong_output_type) == Error(Nil)
}

pub fn manifest_documents_round_trip_test() {
  let input_manifest =
    manifest.ContractInputManifest(
      run_id: "run-1",
      workflow_id: "research",
      workflow_fingerprint: "fp",
      inputs: [
        manifest.NamedManifestValue(
          name: "prompt",
          value: manifest.present_inline_json(
            workflow_contract.Text,
            json_value.JString("hello"),
            None,
          ),
        ),
      ],
      context: [],
      diagnostics: [],
    )
  let assert Ok(decoded_input) =
    manifest.decode_input_manifest(manifest.input_manifest_to_string(
      input_manifest,
    ))
  assert decoded_input.run_id == "run-1"
  assert decoded_input.workflow_fingerprint == "fp"

  let output_manifest =
    manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: "research",
      workflow_fingerprint: "fp",
      outputs: [
        manifest.NamedManifestValue(
          name: "findings",
          value: manifest.present_run_artifact(
            workflow_contract.DocumentMarkdown,
            manifest.ArtifactWritten("runs/run-1/outputs/findings.md", "abc", 3),
            "text/markdown",
            None,
          ),
        ),
      ],
      diagnostics: [],
    )
  let assert Ok(decoded_output) =
    manifest.decode_output_manifest(manifest.output_manifest_to_string(
      output_manifest,
    ))
  assert decoded_output.run_id == "run-1"
  assert decoded_output.outputs != []
}
