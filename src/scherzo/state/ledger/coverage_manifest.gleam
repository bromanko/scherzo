import scherzo/path
import simplifile

pub fn exists(archive_dir: String) -> Result(Bool, String) {
  case simplifile.is_file(path.join(archive_dir, "coverage.json")) {
    Ok(exists) -> Ok(exists)
    Error(simplifile.Enoent) -> Ok(False)
    Error(error) -> Error(simplifile.describe_error(error))
  }
}
