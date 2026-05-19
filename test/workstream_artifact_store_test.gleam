import gleam/bit_array
import scherzo/hash
import scherzo/state/artifact_store as state_artifact_store
import scherzo/workstream/artifact_store
import simplifile

pub fn snapshot_repository_path_writes_exact_bytes_test() {
  let root = "test/tmp/workstream-artifact-store/repository-path"
  let repo = root <> "/repo"
  reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let contents = "hello workstream\n"
  let assert Ok(Nil) = simplifile.write(repo <> "/docs/plan.md", contents)

  let assert Ok(snapshot) =
    artifact_store.snapshot_repository_path(
      root,
      repo,
      "docs/plan.md",
      "text/markdown",
    )

  assert snapshot.ref == expected_snapshot_ref(contents)
  assert snapshot.sha256 == hash.sha256_hex(contents)
  assert snapshot.bytes == bit_array.byte_size(bit_array.from_string(contents))
  assert snapshot.original_path == "docs/plan.md"
  let assert Ok(read_back) =
    artifact_store.read_snapshot(root, snapshot.ref, snapshot.sha256)
  assert read_back == bit_array.from_string(contents)
}

pub fn read_snapshot_detects_corrupt_stored_bytes_test() {
  let root = "test/tmp/workstream-artifact-store/read-corrupt-snapshot"
  let contents = "original bytes"
  reset_dir(root)
  let assert Ok(snapshot) =
    artifact_store.snapshot_bytes(
      root,
      "docs/plan.md",
      "text/plain",
      bit_array.from_string(contents),
    )
  let assert Ok(Nil) =
    simplifile.write(snapshot_path(root, snapshot.ref), "tampered")

  let assert Error(artifact_store.CorruptSnapshot(ref)) =
    artifact_store.read_snapshot(root, snapshot.ref, snapshot.sha256)
  assert ref == snapshot.ref
}

pub fn read_snapshot_rejects_noncanonical_ref_before_io_test() {
  let root = "test/tmp/workstream-artifact-store/read-invalid-ref"
  let ref = "../missing"
  reset_dir(root)

  let assert Error(artifact_store.CorruptSnapshot(rejected_ref)) =
    artifact_store.read_snapshot(root, ref, hash.sha256_hex("missing"))
  assert rejected_ref == ref
}

pub fn snapshot_store_rejects_absolute_or_escaping_paths_test() {
  let root = "test/tmp/workstream-artifact-store/invalid-paths"
  let repo = root <> "/repo"
  let outside = root <> "/outside"
  reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo)
  let assert Ok(Nil) = simplifile.create_directory_all(outside)
  let assert Ok(Nil) = simplifile.write(outside <> "/secret.txt", "secret")
  let assert Ok(Nil) =
    simplifile.create_symlink("../outside/secret.txt", repo <> "/linked.txt")

  let assert Error(artifact_store.InvalidOriginalPath) =
    artifact_store.snapshot_repository_path(
      root,
      repo,
      "/tmp/nope",
      "text/plain",
    )
  let assert Error(artifact_store.InvalidOriginalPath) =
    artifact_store.snapshot_repository_path(root, repo, "../nope", "text/plain")
  let assert Error(artifact_store.SourcePathEscapesRepo(_)) =
    artifact_store.snapshot_repository_path(
      root,
      repo,
      "linked.txt",
      "text/plain",
    )
}

pub fn snapshot_store_missing_file_test() {
  let root = "test/tmp/workstream-artifact-store/missing-file"
  let repo = root <> "/repo"
  reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo)

  let assert Error(artifact_store.MissingSourcePath("docs/missing.md")) =
    artifact_store.snapshot_repository_path(
      root,
      repo,
      "docs/missing.md",
      "text/markdown",
    )
}

pub fn snapshot_store_duplicate_write_returns_same_ref_test() {
  let root = "test/tmp/workstream-artifact-store/duplicate-write"
  let repo = root <> "/repo"
  reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(repo <> "/docs")
  let contents = "same bytes"
  let assert Ok(Nil) = simplifile.write(repo <> "/docs/plan.md", contents)

  let assert Ok(first) =
    artifact_store.snapshot_repository_path(
      root,
      repo,
      "docs/plan.md",
      "text/plain",
    )
  let assert Ok(second) =
    artifact_store.snapshot_repository_path(
      root,
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
  reset_dir(root)
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
      root,
      repo,
      "docs/plan.md",
      "text/plain",
    )
  assert ref == expected_snapshot_ref(contents)
}

pub fn snapshot_existing_artifact_ref_preserves_hash_bytes_and_display_ref_test() {
  let root = "test/tmp/workstream-artifact-store/existing-artifact"
  reset_dir(root)
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
      root,
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
    artifact_store.read_snapshot(root, snapshot.ref, snapshot.sha256)
  assert contents == bit_array.from_string("artifact body")
}

pub fn snapshot_existing_artifact_ref_duplicate_write_coalesces_test() {
  let root = "test/tmp/workstream-artifact-store/existing-duplicate"
  reset_dir(root)
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
      root,
      existing.ref,
      existing.sha256,
      existing.bytes,
      existing.ref,
      "text/markdown",
    )
  let assert Ok(second) =
    artifact_store.snapshot_existing_artifact_ref(
      root,
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
  reset_dir(root)

  let assert Error(artifact_store.MissingExistingArtifact(_)) =
    artifact_store.snapshot_existing_artifact_ref(
      root,
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
      root,
      "/absolute",
      sha,
      7,
      "runs/run-1/outputs/missing.md",
      "text/markdown",
    )
}

pub fn snapshot_existing_artifact_ref_hash_or_byte_mismatch_fails_without_write_test() {
  let root = "test/tmp/workstream-artifact-store/existing-mismatch"
  let wrong_sha = hash.sha256_hex("wrong")
  reset_dir(root)
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
      root,
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
      root,
      existing.ref,
      existing.sha256,
      existing.bytes + 1,
      existing.ref,
      "text/markdown",
    )
}

fn expected_snapshot_ref(contents: String) -> String {
  snapshot_ref(hash.sha256_hex(contents))
}

fn snapshot_ref(sha256: String) -> String {
  "workstream-artifacts/sha256/" <> sha256 <> ".json"
}

fn snapshot_dir(root: String) -> String {
  root <> "/.scherzo-state/artifacts/workstream-artifacts/sha256"
}

fn snapshot_path(root: String, ref: String) -> String {
  root <> "/.scherzo-state/artifacts/" <> ref
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}
