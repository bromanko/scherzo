import gleam/bit_array
import gleam/option.{None}
import scherzo/hash
import scherzo/state/artifact_store as state_artifact_store
import scherzo/workstream/artifact_store
import simplifile
import support/artifact_store_fixtures
import support/test_helpers

pub fn snapshot_repository_path_writes_exact_bytes_test() {
  let root = "test/tmp/workstream-artifact-store/repository-path"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let contents = "hello workstream\n"
  let assert Ok(Nil) = simplifile.write(repo <> "/docs/plan.md", contents)

  let assert Ok(snapshot) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "docs/plan.md",
      "text/markdown",
    )

  assert snapshot.ref == expected_snapshot_ref(contents)
  assert snapshot.sha256 == hash.sha256_hex(contents)
  assert snapshot.bytes == bit_array.byte_size(bit_array.from_string(contents))
  assert snapshot.original_path == "docs/plan.md"
  let assert Ok(stored_contents) =
    simplifile.read(snapshot_path(root, snapshot.ref))
  assert stored_contents == contents
  let assert Ok(read_back) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert read_back == bit_array.from_string(contents)
}

pub fn read_snapshot_detects_corrupt_stored_bytes_test() {
  let root = "test/tmp/workstream-artifact-store/read-corrupt-snapshot"
  let contents = "original bytes"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(snapshot) =
    artifact_store.snapshot_bytes(
      store,
      "docs/plan.md",
      "text/plain",
      bit_array.from_string(contents),
    )
  let assert Ok(Nil) =
    simplifile.write(snapshot_path(root, snapshot.ref), "tampered")

  let assert Error(artifact_store.CorruptSnapshot(ref)) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert ref == snapshot.ref
}

pub fn read_snapshot_rejects_noncanonical_ref_before_io_test() {
  let root = "test/tmp/workstream-artifact-store/read-invalid-ref"
  let ref = "../missing"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)

  let assert Error(artifact_store.CorruptSnapshot(rejected_ref)) =
    artifact_store.read_snapshot(store, ref, hash.sha256_hex("missing"))
  assert rejected_ref == ref
}

pub fn snapshot_store_rejects_absolute_or_escaping_paths_test() {
  let root = "test/tmp/workstream-artifact-store/invalid-paths"
  let repo = root <> "/repo"
  let outside = root <> "/outside"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo)
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(Nil) = simplifile.write(outside <> "/secret.txt", "secret")
  let assert Ok(Nil) =
    simplifile.create_symlink("../outside/secret.txt", repo <> "/linked.txt")

  let assert Error(artifact_store.InvalidOriginalPath) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "/tmp/nope",
      "text/plain",
    )
  let assert Error(artifact_store.InvalidOriginalPath) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "../nope",
      "text/plain",
    )
  let assert Error(artifact_store.SourcePathEscapesRepo(_)) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "linked.txt",
      "text/plain",
    )
}

pub fn snapshot_store_missing_file_test() {
  let root = "test/tmp/workstream-artifact-store/missing-file"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo)

  let assert Error(artifact_store.MissingSourcePath("docs/missing.md")) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "docs/missing.md",
      "text/markdown",
    )
}

pub fn snapshot_store_duplicate_write_returns_same_ref_test() {
  let root = "test/tmp/workstream-artifact-store/duplicate-write"
  let repo = root <> "/repo"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let contents = "same bytes"
  let assert Ok(Nil) = simplifile.write(repo <> "/docs/plan.md", contents)

  let assert Ok(first) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "docs/plan.md",
      "text/plain",
    )
  let assert Ok(second) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "docs/plan.md",
      "text/plain",
    )

  assert first == second
}

pub fn snapshot_store_detects_corrupt_existing_ref_test() {
  let root = "test/tmp/workstream-artifact-store/corrupt-existing-ref"
  let repo = root <> "/repo"
  let contents = "expected bytes"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let assert Ok(Nil) = simplifile.write(repo <> "/docs/plan.md", contents)
  let assert Ok(Nil) = simplifile.create_directory_all(snapshot_dir(root))
  let assert Ok(Nil) =
    simplifile.write(
      snapshot_path(root, expected_snapshot_ref(contents)),
      "corrupt",
    )

  let assert Error(artifact_store.SnapshotWriteConflict(ref)) =
    artifact_store.snapshot_repository_path(
      store,
      repo,
      "docs/plan.md",
      "text/plain",
    )
  assert ref == expected_snapshot_ref(contents)
}

pub fn custom_store_without_local_path_snapshots_by_ref_test() {
  let root = "test/tmp/workstream-artifact-store/custom-no-local-path"
  test_helpers.reset_dir(root)
  let store = artifact_store_fixtures.hidden_local_path_store(root)
  let contents = <<0, 255, 10, 123>>

  let assert Ok(snapshot) =
    artifact_store.snapshot_bytes(
      store,
      "handoffs/output.bin",
      "application/octet-stream",
      contents,
    )

  assert snapshot.ref == snapshot_ref(snapshot.sha256)
  assert snapshot.bytes == bit_array.byte_size(contents)
  let assert Ok(location) = state_artifact_store.location(store, snapshot.ref)
  assert location.local_path == None
  let assert Ok(read_back) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert read_back == contents
}

pub fn snapshot_existing_artifact_ref_preserves_hash_bytes_and_display_ref_test() {
  let root = "test/tmp/workstream-artifact-store/existing-artifact"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(existing) =
    state_artifact_store.write_output_blob(
      store,
      "run-1",
      "plan",
      ".md",
      "artifact body",
    )

  let assert Ok(snapshot) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )

  assert snapshot.sha256 == existing.sha256
  assert snapshot.bytes == existing.bytes
  assert snapshot.original_path == existing.ref
  let assert Ok(contents) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert contents == bit_array.from_string("artifact body")
}

pub fn snapshot_existing_artifact_ref_reads_source_from_custom_store_test() {
  let root = "test/tmp/workstream-artifact-store/existing-custom-source"
  test_helpers.reset_dir(root)
  let store = artifact_store_fixtures.hidden_local_path_store(root)
  let assert Ok(existing) =
    state_artifact_store.write_output_blob(
      store,
      "run-1",
      "plan",
      ".md",
      "custom body",
    )

  let assert Ok(snapshot) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )

  let assert Ok(location) = state_artifact_store.location(store, snapshot.ref)
  assert location.local_path == None
  assert snapshot.ref == snapshot_ref(existing.sha256)
  let assert Ok(contents) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert contents == bit_array.from_string("custom body")
}

pub fn snapshot_existing_artifact_ref_binary_custom_store_exact_bytes_test() {
  let root = "test/tmp/workstream-artifact-store/existing-custom-binary-source"
  test_helpers.reset_dir(root)
  let store = artifact_store_fixtures.hidden_local_path_store(root)
  let source_ref = "runs/run-1/outputs/binary.bin"
  let contents = <<0, 255, 10, 123, 128, 0>>
  let expected_sha = hash.sha256_hex_bytes(contents)
  let expected_bytes = bit_array.byte_size(contents)
  let assert Ok(state_artifact_store.ImmutableWritten) =
    state_artifact_store.write_immutable_artifact_bytes(
      store,
      source_ref,
      contents,
    )

  let assert Ok(snapshot) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      source_ref,
      expected_sha,
      expected_bytes,
      source_ref,
      "application/octet-stream",
    )

  let assert Ok(location) = state_artifact_store.location(store, snapshot.ref)
  assert location.local_path == None
  assert snapshot.ref == snapshot_ref(expected_sha)
  assert snapshot.sha256 == expected_sha
  assert snapshot.bytes == expected_bytes
  assert snapshot.original_path == source_ref
  let assert Ok(read_back) =
    artifact_store.read_snapshot(store, snapshot.ref, snapshot.sha256)
  assert read_back == contents
}

pub fn snapshot_existing_artifact_ref_duplicate_write_coalesces_test() {
  let root = "test/tmp/workstream-artifact-store/existing-duplicate"
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(existing) =
    state_artifact_store.write_output_blob(
      store,
      "run-1",
      "plan",
      ".md",
      "body",
    )

  let assert Ok(first) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )
  let assert Ok(second) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )

  assert first == second
}

pub fn snapshot_existing_artifact_ref_missing_or_unresolvable_fails_without_write_test() {
  let root = "test/tmp/workstream-artifact-store/existing-missing"
  let sha = hash.sha256_hex("missing")
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)

  let assert Error(artifact_store.MissingExistingArtifact(_)) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      "runs/run-1/outputs/missing.md",
      sha,
      7,
      "runs/run-1/outputs/missing.md",
      "text/markdown",
    )
  let assert Ok(False) =
    simplifile.is_file(snapshot_path(root, snapshot_ref(sha)))

  let assert Error(artifact_store.InvalidExistingArtifactRef(_)) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      "/absolute",
      sha,
      7,
      "runs/run-1/outputs/missing.md",
      "text/markdown",
    )
  let assert Ok(False) =
    simplifile.is_file(snapshot_path(root, snapshot_ref(sha)))
}

pub fn snapshot_existing_artifact_ref_hash_or_byte_mismatch_fails_without_write_test() {
  let root = "test/tmp/workstream-artifact-store/existing-mismatch"
  let wrong_sha = hash.sha256_hex("wrong")
  test_helpers.reset_dir(root)
  let store = state_artifact_store.new(root)
  let assert Ok(existing) =
    state_artifact_store.write_output_blob(
      store,
      "run-1",
      "plan",
      ".md",
      "body",
    )

  let assert Error(artifact_store.ExistingArtifactMismatch(_)) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      wrong_sha,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )
  let assert Ok(False) =
    simplifile.is_file(snapshot_path(root, snapshot_ref(wrong_sha)))

  let assert Error(artifact_store.ExistingArtifactMismatch(_)) =
    artifact_store.snapshot_existing_artifact_ref(
      store,
      existing.ref,
      existing.sha256,
      existing.bytes + 1,
      existing.ref,
      "text/markdown",
    )
  let assert Ok(False) =
    simplifile.is_file(snapshot_path(root, snapshot_ref(existing.sha256)))
}

fn expected_snapshot_ref(contents: String) -> String {
  snapshot_ref(hash.sha256_hex(contents))
}

fn snapshot_ref(sha256: String) -> String {
  "workstream-artifacts/sha256/" <> sha256 <> ".json"
}

fn artifact_root(root: String) -> String {
  root <> "/.scherzo-state/artifacts"
}

fn snapshot_dir(root: String) -> String {
  artifact_root(root) <> "/workstream-artifacts/sha256"
}

fn snapshot_path(root: String, ref: String) -> String {
  artifact_root(root) <> "/" <> ref
}
