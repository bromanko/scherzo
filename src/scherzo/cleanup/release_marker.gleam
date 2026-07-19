import scherzo/cleanup/retention_marker
import scherzo/workspace_run
import simplifile

pub type Release {
  NoRelease
  Released(contents: String)
}

pub type ReleaseError {
  ReleaseError(message: String)
}

pub fn release(run_root: String) -> Result(Release, ReleaseError) {
  let marker_path = workspace_run.cleanup_retention_marker(run_root)
  case simplifile.read(marker_path) {
    Ok(contents) ->
      case retention_marker.parse(contents) {
        retention_marker.SchemaMarker(retention_marker.SafeToDelete, _, _, _)
        | retention_marker.SchemaMarker(retention_marker.Abandoned, _, _, _) ->
          case simplifile.delete(marker_path) {
            Ok(Nil) -> Ok(Released(contents))
            Error(error) ->
              Error(ReleaseError(
                "workspace retention release marker could not be removed: "
                <> simplifile.describe_error(error),
              ))
          }
        _ -> Ok(NoRelease)
      }
    Error(simplifile.Enoent) -> Ok(NoRelease)
    Error(error) ->
      Error(ReleaseError(
        "workspace retention marker could not be read before cleanup: "
        <> simplifile.describe_error(error),
      ))
  }
}

pub fn restore_after_failure(
  run_root: String,
  release: Release,
  cleanup_failure_reason: String,
) -> String {
  case release {
    NoRelease -> cleanup_failure_reason
    Released(contents) ->
      case
        simplifile.write(
          workspace_run.cleanup_retention_marker(run_root),
          contents,
        )
      {
        Ok(Nil) -> cleanup_failure_reason
        Error(error) ->
          cleanup_failure_reason
          <> "; workspace retention release marker restore failed: "
          <> simplifile.describe_error(error)
      }
  }
}
