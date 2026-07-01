import scherzo/managed_launch/file
import scherzo/path
import simplifile
import support/test_helpers

const grant_json = "{\"version\":1,\"launchId\":\"launch-123\",\"endpoint\":\"https://ui.example.test\",\"credential\":\"launch_secret_1\",\"capabilities\":[\"state\",\"query\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2999-01-01T00:00:00Z\"}"

pub fn managed_launch_file_loads_private_regular_file_once_test() {
  let root = "tmp/managed-launch-file/ok"
  test_helpers.reset_dir(root)
  let path_value = root <> "/grant.json"
  let assert Ok(Nil) = simplifile.write(path_value, grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)
  let assert Ok(Nil) = simplifile.set_permissions_octal(path_value, 0o600)

  let assert Ok(loaded) = file.load_grant(path_value, 0)
  assert loaded.launch_id == "launch-123"
  assert loaded.credential == "launch_secret_1"
  assert simplifile.is_file(path_value) == Ok(False)
}

pub fn managed_launch_file_rejects_loose_permissions_test() {
  let root = "tmp/managed-launch-file/loose"
  test_helpers.reset_dir(root)
  let path_value = root <> "/grant.json"
  let assert Ok(Nil) = simplifile.write(path_value, grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)
  let assert Ok(Nil) = simplifile.set_permissions_octal(path_value, 0o644)

  let assert Error(error) = file.load_grant(path_value, 0)
  assert file.error_code(error) == "grant_file_permissions_loose"
}

pub fn managed_launch_file_rejects_symlink_test() {
  let root = "tmp/managed-launch-file/symlink"
  test_helpers.reset_dir(root)
  let target = root <> "/target.json"
  let link = root <> "/grant.json"
  let assert Ok(Nil) = simplifile.write(target, grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)
  let assert Ok(Nil) = simplifile.set_permissions_octal(target, 0o600)
  let assert Ok(Nil) = path.symlink(target, link)

  let assert Error(error) = file.load_grant(link, 0)
  assert file.error_code(error) == "grant_file_symlink"
}

pub fn managed_launch_file_rejects_non_regular_file_test() {
  let root = "tmp/managed-launch-file/non-regular"
  test_helpers.reset_dir(root)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)

  let assert Error(error) = file.load_grant(root, 0)
  assert file.error_code(error) == "grant_file_non_regular"
}

pub fn managed_launch_file_rejects_unsafe_parent_permissions_test() {
  let root = "tmp/managed-launch-file/unsafe-parent"
  test_helpers.reset_dir(root)
  let path_value = root <> "/grant.json"
  let assert Ok(Nil) = simplifile.write(path_value, grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o755)
  let assert Ok(Nil) = simplifile.set_permissions_octal(path_value, 0o600)

  let assert Error(error) = file.load_grant(path_value, 0)
  assert file.error_code(error) == "grant_file_parent_permissions_loose"
}
