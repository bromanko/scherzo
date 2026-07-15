import gleam/erlang/process
import gleam/list
import gleam/string
import scherzo/state/archive_pruned_index
import simplifile
import support/test_helpers
import test_async

pub fn exact_hit_and_miss_do_not_require_archive_jsonl_test() {
  let root = "test/tmp/archive-pruned-index/exact"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let assert Ok(Nil) =
    simplifile.write(
      root <> "/segment-1.jsonl",
      "not valid JSON and intentionally never opened",
    )
  let assert Ok(Nil) = archive_pruned_index.write_run_ids(root, ["run-1"])

  assert archive_pruned_index.lookup(root, "run-1")
    == Ok(archive_pruned_index.Pruned)
  assert archive_pruned_index.lookup(root, "never-known")
    == Ok(archive_pruned_index.Unknown)
}

pub fn injected_hash_collision_compares_exact_ids_test() {
  let root = "test/tmp/archive-pruned-index/collision"
  test_helpers.reset_dir(root)
  let collision_hash = fn(_id) {
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  }
  let assert Ok(Nil) =
    archive_pruned_index.write_run_ids_with_hash(
      root,
      ["run-a", "run-b"],
      collision_hash,
    )

  assert archive_pruned_index.lookup_with_hash(root, "run-a", collision_hash)
    == Ok(archive_pruned_index.Pruned)
  assert archive_pruned_index.lookup_with_hash(root, "run-b", collision_hash)
    == Ok(archive_pruned_index.Pruned)
  assert archive_pruned_index.lookup_with_hash(root, "run-c", collision_hash)
    == Ok(archive_pruned_index.Unknown)
}

pub fn large_archive_and_repeated_misses_open_one_marker_path_each_test() {
  let root = "test/tmp/archive-pruned-index/large"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.create_directory_all(root)
  let fixed_hash = fn(_) {
    "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  }
  let opened = process.new_subject()
  let lookup = fn() {
    archive_pruned_index.lookup_with_hash_observed(
      root,
      "never-known",
      fixed_hash,
      fn(path) { process.send(opened, path) },
    )
  }

  assert lookup() == Ok(archive_pruned_index.Unknown)
  let assert Ok(before_path) = process.receive(opened, within: 100)
  assert !string.ends_with(before_path, ".jsonl")

  let unrelated_record = "{\"id\":\"unrelated\"}\n"
  let assert Ok(Nil) =
    simplifile.write(
      root <> "/segment-999.jsonl",
      string.repeat(unrelated_record, times: 100_000),
    )
  assert lookup() == Ok(archive_pruned_index.Unknown)
  let assert Ok(after_path) = process.receive(opened, within: 100)
  assert after_path == before_path

  list.repeat(Nil, times: 1000)
  |> list.each(fn(_) {
    assert lookup() == Ok(archive_pruned_index.Unknown)
  })
  let repeated_paths = test_async.drain_subject(opened)
  assert list.length(repeated_paths) == 1000
  assert list.all(repeated_paths, fn(path) { path == before_path })
}

pub fn marker_writes_are_idempotent_and_malformed_markers_fail_closed_test() {
  let root = "test/tmp/archive-pruned-index/idempotent"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) =
    archive_pruned_index.write_run_ids(root, ["run-1", "run-1"])
  let marker = archive_pruned_index.marker_path(root, "run-1")
  let assert Ok(before) = simplifile.read(marker)
  let assert Ok(Nil) = archive_pruned_index.write_run_ids(root, ["run-1"])
  let assert Ok(after) = simplifile.read(marker)
  assert before == after

  let assert Ok(Nil) = simplifile.write(marker, "malformed-without-newline")
  let assert Error(archive_pruned_index.ArchiveIndexUnavailable(_, _)) =
    archive_pruned_index.lookup(root, "run-1")

  let unreadable_root = "test/tmp/archive-pruned-index/unreadable"
  test_helpers.reset_dir(unreadable_root)
  let unreadable_marker =
    archive_pruned_index.marker_path(unreadable_root, "run-unreadable")
  let assert Ok(Nil) = simplifile.create_directory_all(unreadable_marker)
  let assert Error(archive_pruned_index.ArchiveIndexUnavailable(_, _)) =
    archive_pruned_index.lookup(unreadable_root, "run-unreadable")
}
