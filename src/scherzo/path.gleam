import gleam/option.{type Option, None, Some}
import gleam/string

pub const caller_cwd_env = "SCHERZO_CALLER_CWD"

pub fn absolute(path: String) -> Result(String, Nil) {
  absname(path)
}

pub fn absolute_or_original(path: String) -> String {
  case absolute(path) {
    Ok(absolute_path) -> absolute_path
    Error(Nil) -> path
  }
}

pub fn resolve_from_caller_cwd(
  value: String,
  env: fn(String) -> Option(String),
) -> String {
  case is_absolute(value) {
    True -> value
    False ->
      case env(caller_cwd_env) {
        Some(cwd) ->
          case cwd == "" {
            True -> value
            False -> join(cwd, value)
          }
        None -> value
      }
  }
}

pub fn is_absolute(value: String) -> Bool {
  string.starts_with(value, "/")
}

pub fn dirname(path: String) -> Result(String, Nil) {
  ffi_dirname(path)
}

pub fn temp_dir() -> Result(String, Nil) {
  tmpdir()
}

pub fn home_dir() -> Result(String, Nil) {
  home()
}

pub fn env(name: String) -> Option(String) {
  case getenv(name) {
    Ok(value) -> Some(value)
    Error(Nil) -> None
  }
}

pub fn set_env(name: String, value: String) -> Result(Nil, Nil) {
  putenv(name, value)
}

pub fn unset_env(name: String) -> Result(Nil, Nil) {
  unsetenv(name)
}

pub fn realpath(path: String) -> Result(String, Nil) {
  ffi_realpath(path)
}

pub fn symlink(target: String, link_name: String) -> Result(Nil, Nil) {
  ffi_symlink(target, link_name)
}

pub fn contains(root: String, path: String) -> Bool {
  let root = trim_trailing_slash(root)
  let path = trim_trailing_slash(path)
  path == root || string.starts_with(path, root <> "/")
}

pub fn has_parent_segment(value: String) -> Bool {
  has_forward_parent_segment(value) || has_backslash_parent_segment(value)
}

pub fn contains_control_character(value: String) -> Bool {
  string.contains(value, "\n")
  || string.contains(value, "\r")
  || string.contains(value, "\t")
}

pub fn join(root: String, child: String) -> String {
  trim_trailing_slash(root) <> "/" <> child
}

fn has_forward_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn has_backslash_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "..\\")
  || string.ends_with(value, "\\..")
  || string.contains(value, "\\..\\")
}

fn trim_trailing_slash(path: String) -> String {
  case path != "/" && string.ends_with(path, "/") {
    True -> string.drop_end(path, 1)
    False -> path
  }
}

@external(erlang, "scherzo_config_ffi", "getenv")
fn getenv(name: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "putenv")
fn putenv(name: String, value: String) -> Result(Nil, Nil)

@external(erlang, "scherzo_config_ffi", "unsetenv")
fn unsetenv(name: String) -> Result(Nil, Nil)

@external(erlang, "scherzo_config_ffi", "home")
fn home() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "tmpdir")
fn tmpdir() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "dirname")
fn ffi_dirname(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "absname")
fn absname(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "realpath")
fn ffi_realpath(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "symlink")
fn ffi_symlink(target: String, link_name: String) -> Result(Nil, Nil)
