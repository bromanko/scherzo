import gleam/list
import scherzo_lint/test_determinism_guard as guard

pub fn reports_process_sleep_outside_wait_helpers_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import gleam/erlang/process\n\npub fn flaky_test() {\n  process.sleep(50)\n}\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_process_sleep_rule && finding.line == 4
  })
}

pub fn allows_bounded_wait_helper_sleep_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import gleam/erlang/process\n\nfn wait_for_file(attempts: Int) {\n  process.sleep(50)\n}\n",
    )

  assert findings == []
}

pub fn reports_tiny_query_timeout_in_non_timeout_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "pub fn query_and_command_are_compatible_test() {\n  client.Settings(query_timeout_ms: 100)\n}\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_tiny_query_timeout_rule && finding.line == 2
  })
}

pub fn allows_tiny_query_timeout_in_timeout_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "pub fn query_times_out_test() {\n  client.Settings(query_timeout_ms: 100)\n}\n",
    )

  assert findings == []
}

pub fn reports_raw_negative_receive_assertions_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import gleam/erlang/process\n\npub fn no_message_test() {\n  assert process.receive(subject, within: 20) == Error(Nil)\n  let assert Error(Nil) = process.receive(other, within: 100)\n}\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_raw_negative_receive_rule && finding.line == 4
  })
  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_raw_negative_receive_rule && finding.line == 5
  })
}

pub fn reports_raw_negative_receive_case_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import gleam/erlang/process\n\npub fn no_duplicate_test() {\n  case process.receive(completions, within: 50) {\n    Error(_) -> Nil\n    Ok(_) -> panic as \"duplicate completion\"\n  }\n}\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_raw_negative_receive_rule && finding.line == 4
  })
}

pub fn reports_manual_drain_receive_helper_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import gleam/erlang/process\n\nfn drain_output(subject) {\n  case process.receive(subject, within: 10) {\n    Ok(message) -> drain_output(subject)\n    Error(_) -> \"\"\n  }\n}\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_manual_drain_receive_rule && finding.line == 4
  })
}

pub fn reports_global_env_mutation_in_gleam_tests_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import scherzo/path\n\npub fn env_mutation_test() {\n  let _ = path.set_env(\"PATH\", \"/tmp/bin\")\n  let _ = path.unset_env(\"PATH\")\n  let _ = setenv(\"SCHERZO_TEST\", \"1\")\n}\n\n@external(erlang, \"scherzo_test_ffi\", \"setenv\")\nfn setenv(name: String, value: String) -> Result(Nil, Nil)\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_global_env_mutation_rule && finding.line == 4
  })
  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_global_env_mutation_rule && finding.line == 5
  })
  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_global_env_mutation_rule && finding.line == 6
  })
  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_global_env_mutation_rule && finding.line == 10
  })
}

pub fn reports_global_env_mutation_in_erlang_test_helpers_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_ffi.erl",
      source: "-module(example_ffi).\nmutate() -> os:putenv(\"PATH\", \"/tmp/bin\"), os:unsetenv(\"PATH\").\n",
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.no_global_env_mutation_rule && finding.line == 2
  })
}

pub fn allows_env_reads_and_child_envs_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: "import scherzo/path\nimport scherzo/port\n\npub fn child_env_test() {\n  let existing = path.env(\"PATH\")\n  port.start_with_env(\"echo ok\", \".\", [#(\"PATH\", \"/tmp/bin\")])\n  existing\n}\n",
    )

  assert findings == []
}

pub fn reports_oversized_case_tables_test() {
  let findings =
    guard.findings_for_source(
      path: "test/example_test.gleam",
      source: oversized_case_table_source(31),
    )

  assert list.any(findings, fn(finding) {
    finding.rule == guard.split_large_table_tests_rule && finding.line == 2
  })
}

fn oversized_case_table_source(count: Int) -> String {
  "pub fn many_cases_test() {\n  let cases = [\n"
  <> tuple_rows(count)
  <> "  ]\n  cases\n}\n"
}

fn tuple_rows(count: Int) -> String {
  case count <= 0 {
    True -> ""
    False -> "    #(\"case\", \"value\"),\n" <> tuple_rows(count - 1)
  }
}
