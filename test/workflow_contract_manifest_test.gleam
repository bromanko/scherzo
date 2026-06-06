import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/json_value
import scherzo/workflow_artifact_descriptor as artifact_descriptor
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

pub fn code_change_accepts_all_legacy_reference_aliases_test() {
  let valid_cases = [
    #("pr_url", json_value.JString("https://example.invalid/pr/1")),
    #("branch", json_value.JString("feature/liv-292")),
    #("merge_commit", json_value.JString("abc123")),
    #("patch_ref", json_value.JString("runs/run-1/outputs/change.patch")),
  ]

  valid_cases
  |> list.each(fn(entry) {
    let #(key, value) = entry
    let manifest_value =
      manifest.present_inline_json(
        workflow_contract.CodeChange,
        json_value.JObject([#(key, value)]),
        None,
      )
    assert manifest.validate_required_output_value(
        "code_change",
        manifest_value,
      )
      == Ok(Nil)
  })
}

pub fn manifest_decoders_reject_wrong_header_test() {
  let wrong_input_version =
    "{\"schema_version\":3,\"artifact_type\":\"workflow_contract_inputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"inputs\":[],\"context\":[] }"
  assert manifest.decode_input_manifest(wrong_input_version) == Error(Nil)

  let wrong_output_type =
    "{\"schema_version\":1,\"artifact_type\":\"workflow_contract_inputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"outputs\":[] }"
  assert manifest.decode_output_manifest(wrong_output_type) == Error(Nil)
}

pub fn descriptor_first_manifest_entries_are_emitted_without_legacy_present_type_fields_test() {
  let manifest_document =
    manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: "research",
      workflow_fingerprint: "fp",
      outputs: [
        manifest.NamedManifestValue(
          name: "exec_plan_bundle",
          value: manifest.present_run_artifact(
            workflow_contract.ExecPlanBundle,
            manifest.ArtifactWritten(
              "runs/run-1/outputs/exec_plan_bundle.json",
              valid_sha256(),
              3,
            ),
            "application/json",
            Some(
              json_value.JObject([#("step_id", json_value.JString("bundle"))]),
            ),
          ),
        ),
        manifest.NamedManifestValue(
          name: "implementation_pack",
          value: manifest.present_run_artifact(
            workflow_contract.ImplementationPack,
            manifest.ArtifactWritten(
              "runs/run-1/outputs/implementation_pack.json",
              valid_sha256(),
              3,
            ),
            "application/json",
            None,
          ),
        ),
        manifest.NamedManifestValue(
          name: "code_change",
          value: manifest.present_inline_json(
            workflow_contract.CodeChange,
            json_value.JObject([
              #("branch", json_value.JString("feature/liv-292")),
            ]),
            None,
          ),
        ),
        manifest.NamedManifestValue(
          name: "review_doc",
          value: manifest.present_url(
            workflow_contract.Url,
            "https://example.invalid/pr/1",
          ),
        ),
        manifest.NamedManifestValue(
          name: "branch",
          value: manifest.present_git_ref(
            workflow_contract.GitRef,
            "feature/liv-292",
          ),
        ),
      ],
      diagnostics: [],
    )

  let text = manifest.output_manifest_to_string(manifest_document)
  let assert Ok(decoded) = manifest.decode_output_manifest(text)
  assert decoded.outputs != []
  assert json_text_contains(text, "\"schema_version\":2")
  assert json_text_contains(text, "\"descriptor\":{")
  assert !json_text_contains(text, "\"type\":\"exec_plan_bundle\"")
  assert !json_text_contains(text, "\"ref_kind\":\"run_artifact\"")
  assert json_text_contains(text, "\"kind\":\"artifact_set\"")
  assert json_text_contains(
    text,
    "\"artifact_type\":\"scherzo.exec_plan_bundle.v2\"",
  )
  assert json_text_contains(text, "\"kind\":\"file\"")
  assert json_text_contains(
    text,
    "\"artifact_type\":\"scherzo.implementation_pack.v2\"",
  )
  assert json_text_contains(text, "\"kind\":\"value\"")
  assert json_text_contains(text, "\"kind\":\"ref\"")
  assert json_text_contains(text, "\"ref_type\":\"url\"")
  assert json_text_contains(text, "\"ref_type\":\"git_ref\"")
}

pub fn descriptor_helper_maps_legacy_contract_types_test() {
  let exec_plan_bundle =
    manifest.descriptor_for_named_value(
      "exec_plan_bundle",
      manifest.present_run_artifact(
        workflow_contract.ExecPlanBundle,
        manifest.ArtifactWritten(
          "runs/run-1/outputs/exec_plan_bundle.json",
          valid_sha256(),
          3,
        ),
        "application/json",
        None,
      ),
    )
  let assert Some(exec_plan_bundle) = exec_plan_bundle
  assert exec_plan_bundle.kind == artifact_descriptor.ArtifactSetKind
  assert exec_plan_bundle.artifact_type == Some("scherzo.exec_plan_bundle.v2")

  let implementation_pack =
    manifest.descriptor_for_named_value(
      "implementation_pack",
      manifest.present_run_artifact(
        workflow_contract.ImplementationPack,
        manifest.ArtifactWritten(
          "runs/run-1/outputs/implementation_pack.json",
          valid_sha256(),
          3,
        ),
        "application/json",
        None,
      ),
    )
  let assert Some(implementation_pack) = implementation_pack
  assert implementation_pack.kind == artifact_descriptor.FileKind
  assert implementation_pack.artifact_type
    == Some("scherzo.implementation_pack.v2")

  let code_change_bundle =
    manifest.descriptor_for_named_value(
      "code_change_bundle",
      manifest.present_run_artifact(
        workflow_contract.CodeChangeBundle,
        manifest.ArtifactWritten(
          "runs/run-1/outputs/code_change_bundle.json",
          valid_sha256(),
          3,
        ),
        "application/json",
        None,
      ),
    )
  let assert Some(code_change_bundle) = code_change_bundle
  assert code_change_bundle.kind == artifact_descriptor.ArtifactSetKind
  assert code_change_bundle.artifact_type
    == Some("scherzo.code_change_bundle.v2")

  let code_change =
    manifest.descriptor_for_named_value(
      "code_change",
      manifest.present_inline_json(
        workflow_contract.CodeChange,
        json_value.JObject([#("branch", json_value.JString("feature/liv-292"))]),
        None,
      ),
    )
  let assert Some(code_change) = code_change
  assert code_change.kind == artifact_descriptor.ValueKind
  assert code_change.artifact_type == Some("code_change")

  let artifacts =
    manifest.descriptor_for_named_value(
      "attachments",
      manifest.present_run_artifact(
        workflow_contract.ArtifactList,
        manifest.ArtifactWritten(
          "runs/run-1/outputs/attachments.json",
          valid_sha256(),
          3,
        ),
        "application/json",
        None,
      ),
    )
  let assert Some(artifacts) = artifacts
  assert artifacts.kind == artifact_descriptor.ArtifactSetKind
  assert artifacts.artifact_type == Some("artifact[]")

  let markdown =
    manifest.descriptor_for_named_value(
      "findings",
      manifest.present_run_artifact(
        workflow_contract.DocumentMarkdown,
        manifest.ArtifactWritten(
          "runs/run-1/outputs/findings.md",
          valid_sha256(),
          3,
        ),
        "text/markdown",
        None,
      ),
    )
  let assert Some(markdown) = markdown
  assert markdown.kind == artifact_descriptor.FileKind
  assert markdown.artifact_type == Some("document.markdown")

  let url =
    manifest.descriptor_for_named_value(
      "review_doc",
      manifest.present_url(
        workflow_contract.Url,
        "https://example.invalid/pr/1",
      ),
    )
  let assert Some(url) = url
  assert url.kind == artifact_descriptor.RefKind
  assert url.artifact_type == Some("url")

  let git_ref =
    manifest.descriptor_for_named_value(
      "branch",
      manifest.present_git_ref(workflow_contract.GitRef, "feature/liv-292"),
    )
  let assert Some(git_ref) = git_ref
  assert git_ref.kind == artifact_descriptor.RefKind
  assert git_ref.artifact_type == Some("git_ref")
}

pub fn generic_artifact_set_manifest_descriptor_preserves_contract_descriptor_test() {
  let value =
    manifest.present_run_artifact(
      workflow_contract.GenericArtifactSet,
      manifest.ArtifactWritten(
        "runs/run-1/outputs/visual_artifacts.json",
        valid_sha256(),
        3,
      ),
      "application/json",
      Some(
        json_value.JObject([
          #(
            "contract_artifact_type",
            json_value.JString("scherzo_ui.visual_artifact_bundle.v1"),
          ),
          #(
            "contract_descriptor",
            json_value.JObject([
              #("kind", json_value.JString("artifact_set")),
              #("media_type", json_value.JString("application/json")),
              #(
                "artifact_type",
                json_value.JString("scherzo_ui.visual_artifact_bundle.v1"),
              ),
            ]),
          ),
        ]),
      ),
    )

  let descriptor =
    manifest.descriptor_for_named_value("visual_artifacts", value)
  let assert Some(descriptor) = descriptor
  assert descriptor.kind == artifact_descriptor.ArtifactSetKind
  assert descriptor.artifact_type
    == Some("scherzo_ui.visual_artifact_bundle.v1")
  assert descriptor.media_type == Some("application/json")
}

pub fn historical_manifest_without_descriptor_still_decodes_test() {
  let legacy =
    "{\"schema_version\":1,\"artifact_type\":\"workflow_contract_outputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"outputs\":[{\"name\":\"findings\",\"value\":{\"type\":\"document.markdown\",\"status\":\"present\",\"ref_kind\":\"run_artifact\",\"ref\":\"runs/run-1/outputs/findings.md\",\"sha256\":\"abc\",\"bytes\":3,\"media_type\":\"text/markdown\",\"value\":null,\"source\":null,\"diagnostic\":null}}],\"diagnostics\":[] }"
  let assert Ok(decoded) = manifest.decode_output_manifest(legacy)
  let assert [findings] = decoded.outputs
  assert findings.name == "findings"
  assert findings.value.ref == Some("runs/run-1/outputs/findings.md")
}

pub fn input_manifest_with_legacy_placeholder_hash_round_trips_without_descriptor_test() {
  let input_manifest =
    manifest.ContractInputManifest(
      run_id: "run-1",
      workflow_id: "research",
      workflow_fingerprint: "fp",
      inputs: [
        manifest.NamedManifestValue(
          name: "exec_plan",
          value: manifest.present_run_artifact(
            workflow_contract.ExecPlan,
            manifest.ArtifactWritten(
              "runs/upstream/outputs/exec_plan.md",
              "abc",
              12,
            ),
            "text/markdown",
            None,
          ),
        ),
      ],
      context: [],
      diagnostics: [],
    )

  let text = manifest.input_manifest_to_string(input_manifest)
  assert json_text_contains(text, "\"descriptor\":null")
  let assert Ok(decoded) = manifest.decode_input_manifest(text)
  let assert [exec_plan] = decoded.inputs
  assert exec_plan.value.sha256 == Some("abc")
}

pub fn manifest_rejects_descriptor_that_disagrees_with_legacy_value_test() {
  let mismatched =
    "{\"schema_version\":1,\"artifact_type\":\"workflow_contract_outputs\",\"run_id\":\"run-1\",\"workflow_id\":\"research\",\"workflow_fingerprint\":\"fp\",\"outputs\":[{\"name\":\"findings\",\"value\":{\"type\":\"document.markdown\",\"status\":\"present\",\"ref_kind\":\"run_artifact\",\"ref\":\"runs/run-1/outputs/findings.md\",\"sha256\":\""
    <> valid_sha256()
    <> "\",\"bytes\":3,\"media_type\":\"text/markdown\",\"value\":null,\"source\":null,\"diagnostic\":null},\"descriptor\":{\"name\":\"findings\",\"kind\":\"ref\",\"artifact_type\":\"url\",\"ref_type\":\"url\",\"ref\":\"https://example.invalid/pr/1\"}}],\"diagnostics\":[] }"

  assert manifest.decode_output_manifest(mismatched) == Error(Nil)
}

pub fn inline_json_null_manifest_value_round_trips_test() {
  let output_manifest =
    manifest.ContractOutputManifest(
      run_id: "run-1",
      workflow_id: "research",
      workflow_fingerprint: "fp",
      outputs: [
        manifest.NamedManifestValue(
          name: "nullable",
          value: manifest.present_inline_json(
            workflow_contract.Text,
            json_value.JNull,
            None,
          ),
        ),
      ],
      diagnostics: [],
    )

  let text = manifest.output_manifest_to_string(output_manifest)
  assert json_text_contains(text, "\"kind\":\"value\"")
  let assert Ok(decoded) = manifest.decode_output_manifest(text)
  let assert [nullable] = decoded.outputs
  assert nullable.value.value == Some(json_value.JNull)
}

pub fn manifest_round_trip_is_idempotent_with_descriptors_test() {
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
            manifest.ArtifactWritten(
              "runs/run-1/outputs/findings.md",
              valid_sha256(),
              3,
            ),
            "text/markdown",
            None,
          ),
        ),
      ],
      diagnostics: [],
    )

  let first = manifest.output_manifest_to_string(output_manifest)
  let assert Ok(decoded) = manifest.decode_output_manifest(first)
  let second = manifest.output_manifest_to_string(decoded)
  assert first == second
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
            manifest.ArtifactWritten(
              "runs/run-1/outputs/findings.md",
              valid_sha256(),
              3,
            ),
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

fn json_text_contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}

fn valid_sha256() -> String {
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
}
