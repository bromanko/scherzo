import scherzo/code_snapshot

pub fn ensure_scherzo_modules_loaded_returns_module_count_test() {
  let assert Ok(count) = code_snapshot.ensure_scherzo_modules_loaded()
  assert count > 0
}
