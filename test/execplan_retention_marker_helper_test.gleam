import gleam/string
import simplifile

pub fn execplan_helper_writes_schema_backed_retention_marker_test() {
  let assert Ok(contents) =
    simplifile.read("workflows/dogfood/scripts/scherzo-execplan")

  assert string.contains(contents, "Schema: scherzo.retained-workspace.v1")
  assert string.contains(contents, "Review state: publication_guard")
  assert string.contains(contents, "Created at ms:")
  assert string.contains(contents, "Source kind:")
  assert string.contains(contents, "Source:")
  assert string.contains(contents, "has not published a PR yet")
}
