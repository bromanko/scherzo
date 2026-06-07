import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import scherzo/commit_stack_artifact
import scherzo/hash
import scherzo/json_value
import scherzo/state/artifact_store
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

pub fn retained_artifact_set_integrity_verifies_nested_refs_test() {
  let screenshot_contents = "fake png bytes"
  let screenshot =
    screenshot_descriptor(
      sha256: hash.sha256_hex(screenshot_contents),
      bytes: bytes_of(screenshot_contents),
    )
  let nested_contents = artifact_set_contents("nested", [screenshot])
  let nested =
    artifact_set_ref_descriptor(
      name: "nested",
      ref: nested_ref(),
      sha256: hash.sha256_hex(nested_contents),
      bytes: bytes_of(nested_contents),
    )
  let root_contents = artifact_set_contents("visuals", [nested])
  let root =
    artifact_set_ref_descriptor(
      name: "visuals",
      ref: root_ref(),
      sha256: hash.sha256_hex(root_contents),
      bytes: bytes_of(root_contents),
    )
  let store =
    store_with_contents([
      #(root_ref(), root_contents),
      #(nested_ref(), nested_contents),
      #(screenshot_ref(), screenshot_contents),
    ])

  assert descriptor.verify_retained_integrity(root, store) == Ok(Nil)
}

pub fn retained_artifact_set_integrity_rejects_missing_hash_bytes_and_cycles_test() {
  let screenshot_contents = "fake png bytes"
  let screenshot =
    screenshot_descriptor(
      sha256: hash.sha256_hex(screenshot_contents),
      bytes: bytes_of(screenshot_contents),
    )
  let root_contents = artifact_set_contents("visuals", [screenshot])
  let root =
    artifact_set_ref_descriptor(
      name: "visuals",
      ref: root_ref(),
      sha256: hash.sha256_hex(root_contents),
      bytes: bytes_of(root_contents),
    )

  assert verify_error_code(
      root,
      store_with_contents([#(root_ref(), root_contents)]),
    )
    == "artifact_descriptor_missing_ref_artifact"

  let bad_sha_screenshot =
    screenshot_descriptor(
      sha256: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      bytes: bytes_of(screenshot_contents),
    )
  let bad_sha_contents = artifact_set_contents("visuals", [bad_sha_screenshot])
  let bad_sha_root =
    artifact_set_ref_descriptor(
      name: "visuals",
      ref: root_ref(),
      sha256: hash.sha256_hex(bad_sha_contents),
      bytes: bytes_of(bad_sha_contents),
    )
  assert verify_error_code(
      bad_sha_root,
      store_with_contents([
        #(root_ref(), bad_sha_contents),
        #(screenshot_ref(), screenshot_contents),
      ]),
    )
    == "artifact_descriptor_retained_sha256_mismatch"

  let bad_bytes_screenshot =
    screenshot_descriptor(
      sha256: hash.sha256_hex(screenshot_contents),
      bytes: bytes_of(screenshot_contents) + 1,
    )
  let bad_bytes_contents =
    artifact_set_contents("visuals", [bad_bytes_screenshot])
  let bad_bytes_root =
    artifact_set_ref_descriptor(
      name: "visuals",
      ref: root_ref(),
      sha256: hash.sha256_hex(bad_bytes_contents),
      bytes: bytes_of(bad_bytes_contents),
    )
  assert verify_error_code(
      bad_bytes_root,
      store_with_contents([
        #(root_ref(), bad_bytes_contents),
        #(screenshot_ref(), screenshot_contents),
      ]),
    )
    == "artifact_descriptor_retained_bytes_mismatch"

  let cycle_entry =
    artifact_set_ref_descriptor(
      name: "again",
      ref: root_ref(),
      sha256: hash.sha256_hex(root_contents),
      bytes: bytes_of(root_contents),
    )
  let cycle_contents = artifact_set_contents("visuals", [cycle_entry])
  let cycle_root =
    artifact_set_ref_descriptor(
      name: "visuals",
      ref: root_ref(),
      sha256: hash.sha256_hex(cycle_contents),
      bytes: bytes_of(cycle_contents),
    )
  assert verify_error_code(
      cycle_root,
      store_with_contents([#(root_ref(), cycle_contents)]),
    )
    == "artifact_descriptor_retained_cycle"
}

fn verify_error_code(
  root: descriptor.ArtifactDescriptor,
  store: artifact_store.Store,
) -> String {
  let assert Error(descriptor.DescriptorError(code, _)) =
    descriptor.verify_retained_integrity(root, store)
  code
}

fn artifact_set_contents(
  name: String,
  entries: List(descriptor.ArtifactDescriptor),
) -> String {
  descriptor.to_string(descriptor.ArtifactDescriptor(
    name: name,
    kind: descriptor.ArtifactSetKind,
    artifact_type: Some("scherzo_ui.visual_artifact_bundle.v1"),
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: None,
    sha256: None,
    bytes: None,
    media_type: Some("application/json"),
    value: None,
    entries: entries,
  ))
}

fn artifact_set_ref_descriptor(
  name name: String,
  ref ref: String,
  sha256 sha256: String,
  bytes bytes: Int,
) -> descriptor.ArtifactDescriptor {
  descriptor.ArtifactDescriptor(
    name: name,
    kind: descriptor.ArtifactSetKind,
    artifact_type: Some("scherzo_ui.visual_artifact_bundle.v1"),
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: Some(ref),
    sha256: Some(sha256),
    bytes: Some(bytes),
    media_type: Some("application/json"),
    value: None,
    entries: [],
  )
}

fn screenshot_descriptor(
  sha256 sha256: String,
  bytes bytes: Int,
) -> descriptor.ArtifactDescriptor {
  descriptor.ArtifactDescriptor(
    name: "screenshot",
    kind: descriptor.FileKind,
    artifact_type: Some("scherzo_ui.screenshot.v1"),
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: Some(screenshot_ref()),
    sha256: Some(sha256),
    bytes: Some(bytes),
    media_type: Some("image/png"),
    value: None,
    entries: [],
  )
}

fn store_with_contents(
  contents: List(#(String, String)),
) -> artifact_store.Store {
  let refs = dict.from_list(contents)
  artifact_store.custom(
    "workflow-artifact-descriptor-test",
    artifact_store.StoreCallbacks(
      write: fn(_, _) { Ok(Nil) },
      read: fn(ref) {
        case dict.get(refs, ref) {
          Ok(contents) -> Ok(contents)
          Error(Nil) -> Error(artifact_store.MissingStepArtifact(ref))
        }
      },
      write_bytes: fn(_, _) { Ok(Nil) },
      write_immutable_bytes: fn(_, _) { Ok(artifact_store.ImmutableWritten) },
      read_bytes: fn(ref) {
        case dict.get(refs, ref) {
          Ok(contents) -> Ok(bit_array.from_string(contents))
          Error(Nil) -> Error(artifact_store.MissingStepArtifact(ref))
        }
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://test/" <> ref,
          display_path: ref,
          local_path: None,
        ))
      },
    ),
  )
}

fn bytes_of(contents: String) -> Int {
  contents |> bit_array.from_string |> bit_array.byte_size
}

fn root_ref() -> String {
  "runs/run-1/outputs/visuals.json"
}

fn nested_ref() -> String {
  "runs/run-1/outputs/nested.json"
}

fn screenshot_ref() -> String {
  "runs/run-1/outputs/screenshot.png"
}
