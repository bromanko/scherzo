import scherzo/state/archive_coverage
import simplifile
import support/test_helpers

pub fn coverage_orders_segments_numerically_and_verifies_hashes_test() {
  let root = "test/tmp/archive-coverage/numeric"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) = simplifile.write(root <> "/segment-10.jsonl", "ten\n")
  let assert Ok(Nil) = simplifile.write(root <> "/segment-2.jsonl", "two\n")

  let assert Ok([first, second]) = archive_coverage.segment_paths_numeric(root)
  assert first.0 == 2
  assert second.0 == 10

  let assert Ok(manifest) = archive_coverage.build(root)
  let assert Ok(Nil) = archive_coverage.write(root, manifest)
  let assert Ok(stored) = archive_coverage.verify_stored(root)
  assert stored == manifest

  let assert Ok(Nil) = simplifile.write(root <> "/segment-2.jsonl", "changed\n")
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.verify_stored(root)
}

pub fn malformed_and_noncanonical_segment_names_fail_closed_test() {
  let root = "test/tmp/archive-coverage/malformed"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) = simplifile.write(root <> "/segment-01.jsonl", "value\n")
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.segment_paths_numeric(root)
}

pub fn absent_missing_reordered_and_duplicate_coverage_fail_closed_test() {
  let absent_root = "test/tmp/archive-coverage/absent"
  test_helpers.reset_dir(absent_root)
  let assert Ok(Nil) = simplifile.create_directory_all(absent_root)
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.verify_stored(absent_root)

  let root = "test/tmp/archive-coverage/incomplete"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) = simplifile.write(root <> "/segment-1.jsonl", "one\n")
  let assert Ok(Nil) = simplifile.write(root <> "/segment-2.jsonl", "two\n")
  let assert Ok(manifest) = archive_coverage.build(root)
  let assert [first, second] = manifest.segments

  let assert Ok(Nil) = archive_coverage.write(root, manifest)
  let assert Ok(Nil) = simplifile.delete(root <> "/segment-2.jsonl")
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.verify_stored(root)

  let assert Ok(Nil) = simplifile.write(root <> "/segment-2.jsonl", "two\n")
  let reordered = archive_coverage.Manifest([second, first])
  let assert Ok(Nil) = archive_coverage.write(root, reordered)
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.verify_stored(root)

  let duplicated = archive_coverage.Manifest([first, first, second])
  let assert Ok(Nil) = archive_coverage.write(root, duplicated)
  let assert Error(archive_coverage.CoverageIncomplete(_)) =
    archive_coverage.verify_stored(root)
}

pub fn coverage_manifest_write_is_idempotent_test() {
  let root = "test/tmp/archive-coverage/idempotent"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) = simplifile.write(root <> "/segment-1.jsonl", "one\n")
  let assert Ok(manifest) = archive_coverage.build(root)
  let assert Ok(Nil) = archive_coverage.write(root, manifest)
  let assert Ok(before) = simplifile.read(archive_coverage.manifest_path(root))
  let assert Ok(Nil) = archive_coverage.write(root, manifest)
  let assert Ok(after) = simplifile.read(archive_coverage.manifest_path(root))
  assert before == after
}
