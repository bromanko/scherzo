import gleam/option.{type Option, None, Some}
import gleam/string

pub fn absolute(path: String) -> Result(String, Nil) {
  absname(path)
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

pub fn join(root: String, child: String) -> String {
  trim_trailing_slash(root) <> "/" <> child
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
