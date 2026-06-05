import gleam/list
import gleam/option.{None, Some}
import scherzo/commit_stack_artifact
import scherzo/json_value
import scherzo/workflow_artifact_descriptor as descriptor
import simplifile

fn parse_ok(contents: String) -> descriptor.ArtifactDescriptor {
  let assert Ok(parsed) = descriptor.parse(contents)
  parsed
}

fn parse_error_code(contents: String) -> String {
  let assert Error(descriptor.DescriptorError(code, _)) =
    descriptor.parse(contents)
  code
}

pub fn parses_target_fixture_and_defaults_value_media_type_test() {
  let assert Ok(contents) =
    simplifile.read(
      "test/fixtures/workflow_artifacts/target_exec_plan_bundle_descriptor.json",
    )
  let parsed = parse_ok(contents)

  assert parsed.name == "exec_plan_bundle"
  assert parsed.kind == descriptor.ArtifactSetKind
  assert parsed.artifact_type == Some("scherzo.exec_plan_bundle.v2")
  assert parsed.media_type == Some("application/json")
  assert list.length(parsed.entries) == 3

  let assert Ok(value_entry) =
    parsed.entries
    |> list.find(fn(entry) { entry.name == "implementation_handoff" })
  assert value_entry.kind == descriptor.ValueKind
  assert value_entry.media_type == Some("application/json")
}

pub fn validates_arbitrary_workflow_owned_artifact_type_test() {
  let custom =
    descriptor.ArtifactDescriptor(
      name: "release_notes",
      kind: descriptor.ValueKind,
      artifact_type: Some("acme.release_notes.v1"),
      description: None,
      source: None,
      validation: None,
      metadata: None,
      ref_type: None,
      ref: None,
      sha256: None,
      bytes: None,
      media_type: Some("application/json"),
      value: Some(json_value.JObject([#("status", json_value.JString("ok"))])),
      entries: [],
    )

  assert descriptor.validate(custom) == Ok(Nil)
}

pub fn value_descriptor_accepts_json_null_test() {
  let parsed =
    parse_ok("{\"name\":\"nullable\",\"kind\":\"value\",\"value\":null}")

  assert parsed.kind == descriptor.ValueKind
  assert parsed.value == Some(json_value.JNull)

  let reparsed = parse_ok(descriptor.to_string(parsed))
  assert reparsed.value == Some(json_value.JNull)
}

pub fn round_trips_commit_stack_descriptor_kind_test() {
  let retained =
    descriptor.ArtifactDescriptor(
      name: "commit_stack",
      kind: descriptor.CommitStackKind,
      artifact_type: Some(commit_stack_artifact.commit_stack_artifact_type),
      description: None,
      source: None,
      validation: None,
      metadata: None,
      ref_type: None,
      ref: Some("runs/run-1/outputs/commit_stack.json"),
      sha256: Some(
        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      ),
      bytes: Some(12),
      media_type: Some(commit_stack_artifact.commit_stack_media_type),
      value: None,
      entries: [],
    )

  let encoded = descriptor.to_string(retained)
  let decoded = parse_ok(encoded)
  assert decoded.kind == descriptor.CommitStackKind
  assert descriptor.kind_from_string("commit_stack")
    == Ok(descriptor.CommitStackKind)
}

pub fn round_trips_file_value_ref_and_artifact_set_descriptors_test() {
  let nested =
    descriptor.ArtifactDescriptor(
      name: "bundle",
      kind: descriptor.ArtifactSetKind,
      artifact_type: Some("scherzo.exec_plan_bundle.v2"),
      description: None,
      source: None,
      validation: None,
      metadata: None,
      ref_type: None,
      ref: Some("runs/run-1/outputs/exec_plan_bundle.json"),
      sha256: Some(
        "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      ),
      bytes: Some(12),
      media_type: Some("application/json"),
      value: None,
      entries: [
        descriptor.ArtifactDescriptor(
          name: "plan",
          kind: descriptor.FileKind,
          artifact_type: None,
          description: None,
          source: None,
          validation: None,
          metadata: None,
          ref_type: None,
          ref: Some("runs/run-1/outputs/plan.md"),
          sha256: Some(
            "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789",
          ),
          bytes: Some(22),
          media_type: Some("text/markdown"),
          value: None,
          entries: [],
        ),
        descriptor.ArtifactDescriptor(
          name: "handoff",
          kind: descriptor.ValueKind,
          artifact_type: None,
          description: None,
          source: None,
          validation: None,
          metadata: None,
          ref_type: None,
          ref: None,
          sha256: None,
          bytes: None,
          media_type: Some("application/json"),
          value: Some(json_value.JObject([#("ok", json_value.JBool(True))])),
          entries: [],
        ),
        descriptor.ArtifactDescriptor(
          name: "review_doc",
          kind: descriptor.RefKind,
          artifact_type: None,
          description: None,
          source: None,
          validation: None,
          metadata: None,
          ref_type: Some("git_ref"),
          ref: Some("feature/liv-726"),
          sha256: None,
          bytes: None,
          media_type: None,
          value: None,
          entries: [],
        ),
      ],
    )

  let encoded = descriptor.to_string(nested)
  let decoded = parse_ok(encoded)
  assert decoded.kind == descriptor.ArtifactSetKind
  assert list.length(decoded.entries) == 3
  assert descriptor.validate(decoded) == Ok(Nil)
}

pub fn rejects_invalid_descriptors_with_stable_codes_test() {
  assert parse_error_code(
      "{\"name\":\"plan\",\"kind\":\"file\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":1,\"media_type\":\"text/markdown\"}",
    )
    == "artifact_descriptor_file_missing_ref"

  assert parse_error_code(
      "{\"name\":\"plan\",\"kind\":\"file\",\"ref\":\"/tmp/plan.md\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":1,\"media_type\":\"text/markdown\"}",
    )
    == "artifact_descriptor_invalid_ref"

  assert parse_error_code(
      "{\"name\":\"plan\",\"kind\":\"file\",\"ref\":\"not-runs/plan.md\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":1,\"media_type\":\"text/markdown\"}",
    )
    == "artifact_descriptor_invalid_ref"

  assert parse_error_code(
      "{\"name\":\"plan\",\"kind\":\"file\",\"ref\":\"runs/run-1/outputs/plan.md\",\"sha256\":\"short\",\"bytes\":1,\"media_type\":\"text/markdown\"}",
    )
    == "artifact_descriptor_invalid_sha256"

  assert parse_error_code(
      "{\"name\":\"plan\",\"kind\":\"file\",\"ref\":\"runs/run-1/outputs/plan.md\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":-1,\"media_type\":\"text/markdown\"}",
    )
    == "artifact_descriptor_negative_bytes"

  assert parse_error_code(
      "{\"name\":\"handoff\",\"kind\":\"value\",\"media_type\":\"text/plain\",\"value\":{}}",
    )
    == "artifact_descriptor_value_invalid_media_type"

  assert parse_error_code(
      "{\"name\":\"review\",\"kind\":\"ref\",\"ref_type\":\"url\",\"ref\":\"ssh://example.invalid/pr\"}",
    )
    == "artifact_descriptor_invalid_url_ref"

  assert parse_error_code(
      "{\"name\":\"branch\",\"kind\":\"ref\",\"ref_type\":\"git_ref\",\"ref\":\"bad\\tref\"}",
    )
    == "artifact_descriptor_invalid_git_ref"

  assert parse_error_code(
      "{\"name\":\"bundle\",\"kind\":\"artifact_set\",\"entries\":[{\"name\":\"dup\",\"kind\":\"value\",\"value\":1},{\"name\":\"dup\",\"kind\":\"value\",\"value\":2}]}",
    )
    == "artifact_descriptor_duplicate_entry_name"

  assert parse_error_code(
      "{\"name\":\"bundle\",\"kind\":\"artifact_set\",\"ref\":\"runs/run-1/outputs/bundle.json\",\"entries\":[]}",
    )
    == "artifact_descriptor_artifact_set_incomplete_retained_metadata"

  assert parse_error_code(
      "{\"name\":\"bundle\",\"kind\":\"artifact_set\",\"ref\":\"/tmp/bundle.json\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":1,\"media_type\":\"application/json\",\"entries\":[]}",
    )
    == "artifact_descriptor_invalid_ref"

  assert parse_error_code(
      "{\"name\":\"bundle\",\"kind\":\"artifact_set\",\"ref\":\"runs/run-1/outputs/bundle.json\",\"sha256\":\"0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\",\"bytes\":1,\"media_type\":\"text/plain\",\"entries\":[]}",
    )
    == "artifact_descriptor_artifact_set_invalid_media_type"
}
