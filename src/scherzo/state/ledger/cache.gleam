import gleam/option.{type Option}

pub fn get(key: String) -> Option(a) {
  ffi_get(key)
}

pub fn put(key: String, value: a) -> Nil {
  ffi_put(key, value)
}

pub fn delete(key: String) -> Nil {
  ffi_delete(key)
}

@external(erlang, "scherzo_ledger_cache_ffi", "get")
fn ffi_get(key: String) -> Option(a)

@external(erlang, "scherzo_ledger_cache_ffi", "put")
fn ffi_put(key: String, value: a) -> Nil

@external(erlang, "scherzo_ledger_cache_ffi", "delete")
fn ffi_delete(key: String) -> Nil
