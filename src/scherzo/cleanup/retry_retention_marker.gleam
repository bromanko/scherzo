import gleam/result
import scherzo/error
import scherzo/workspace_run
import simplifile

pub fn retain(
  run_root: String,
  run_id: String,
) -> Result(Nil, error.WorkspaceError) {
  let marker = workspace_run.cleanup_retention_marker(run_root)
  case simplifile.is_file(marker) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(simplifile.Enoent) ->
      simplifile.write(
        marker,
        "Scherzo retry-required workspace\nRun: "
          <> run_id
          <> "\nRelease after successful recovery or explicit abandonment.\n",
      )
      |> result.map_error(fn(file_error) {
        error.WorkspaceIo(
          "write retry workspace retention marker failed: "
          <> simplifile.describe_error(file_error),
        )
      })
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "inspect retry workspace retention marker failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}
