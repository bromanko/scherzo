import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo_lint/public_api_style/inventory
import scherzo_lint/public_api_style/report

fn findings_for(source: String) -> List(inventory.Finding) {
  let assert Ok(findings) =
    inventory.findings_for_source(
      path: "src/scherzo/example.gleam",
      source: source,
    )

  findings
}

fn findings_for_candidate(
  source: String,
  candidate: inventory.Candidate,
) -> List(inventory.Finding) {
  findings_for(source)
  |> list.filter(keeping: fn(finding) { finding.candidate == candidate })
}

fn single_candidate(
  source: String,
  candidate: inventory.Candidate,
) -> inventory.Finding {
  let assert [finding] = findings_for_candidate(source, candidate)
  finding
}

pub fn public_four_parameter_function_reports_high_arity_bucket_test() {
  let finding =
    single_candidate(
      "type Config { Config }

pub fn configure(path: String, retries: Int, ratio: Float, config: Config) -> Nil {
  Nil
}",
      inventory.HighArity,
    )

  assert finding.arity == 4
  assert finding.api_shape == "arity 4"
  assert finding.estimated_callsite_churn == 1
}

pub fn public_five_parameter_function_reports_high_arity_bucket_test() {
  let finding =
    single_candidate(
      "type Config { Config }

pub fn configure(path: String, retries: Int, ratio: Float, config: Config, mode: List(String)) -> Nil {
  Nil
}",
      inventory.HighArity,
    )

  assert finding.arity == 5
  assert finding.api_shape == "arity 5"
}

pub fn private_high_arity_function_reports_no_findings_test() {
  assert findings_for(
      "fn configure(path: String, retries: Int, ratio: Float, one: List(String), two: List(Int)) -> Nil {
  Nil
}",
    )
    == []
}

pub fn external_high_arity_function_reports_no_findings_test() {
  assert findings_for(
      "@external(erlang, \"module\", \"configure\")
pub fn configure(path: String, retries: Int, ratio: Float, one: List(String), two: List(Int)) -> Nil",
    )
    == []
}

pub fn duplicate_unlabelled_string_parameters_report_test() {
  let finding =
    single_candidate(
      "pub fn copy(source: String, destination: String) -> Nil {
  Nil
}",
      inventory.DuplicatePrimitiveParameters,
    )

  assert finding.primitive_type == Some("String")
  assert finding.api_shape
    == "duplicate unlabelled String parameters: 'source', 'destination'"
  assert finding.covered_by_existing_rule == False
}

pub fn labelled_duplicate_string_parameters_report_no_findings_test() {
  assert findings_for(
      "pub fn copy(source source: String, destination destination: String) -> Nil {
  Nil
}",
    )
    == []
}

pub fn three_parameter_unlabelled_bool_reports_broader_bool_test() {
  let finding =
    single_candidate(
      "pub fn launch(command: String, retries: Int, auto_retry: Bool) -> Nil {
  Nil
}",
      inventory.BroaderBoolParameters,
    )

  assert finding.arity == 3
  assert finding.primitive_type == Some("Bool")
  assert finding.covered_by_existing_rule == False
}

pub fn single_parameter_unlabelled_bool_reports_broader_bool_test() {
  let finding =
    single_candidate(
      "pub fn enabled(enabled: Bool) -> Nil {
  Nil
}",
      inventory.BroaderBoolParameters,
    )

  assert finding.arity == 1
  assert finding.covered_by_existing_rule == False
}

pub fn existing_two_parameter_bool_is_marked_as_covered_test() {
  let finding =
    single_candidate(
      "pub fn configure(path: String, enabled: Bool) -> Nil {
  Nil
}",
      inventory.BroaderBoolParameters,
    )

  assert finding.covered_by_existing_rule == True
  assert finding.estimated_callsite_churn == 0
}

pub fn discarded_bool_parameter_reports_no_findings_test() {
  assert findings_for(
      "pub fn configure(_: Bool) -> Nil {
  Nil
}",
    )
    == []
}

pub fn unannotated_parameter_reports_no_findings_test() {
  assert findings_for(
      "pub fn configure(enabled) -> Nil {
  Nil
}",
    )
    == []
}

pub fn exception_hints_classify_conventional_shapes_test() {
  let callback =
    single_candidate(
      "pub fn visit(items: List(String), handler: fn(String) -> Nil, retries: Int, ratio: Float) -> Nil {
  Nil
}",
      inventory.HighArity,
    )
  assert callback.likely_exception == Some("callback or comparator convention")

  let comparator =
    single_candidate(
      "pub fn compare(left: String, right: String) -> Nil {
  Nil
}",
      inventory.DuplicatePrimitiveParameters,
    )
  assert comparator.likely_exception
    == Some("callback or comparator convention")

  let decoder =
    single_candidate(
      "pub fn decode_field(raw: String, fallback: String) -> Nil {
  Nil
}",
      inventory.DuplicatePrimitiveParameters,
    )
  assert decoder.likely_exception == Some("decode helper")

  let builder =
    single_candidate(
      "pub fn with_limits(builder: Builder, name: String, retries: Int, ratio: Float) -> Builder {
  builder
}",
      inventory.HighArity,
    )
  assert builder.likely_exception == Some("builder-style helper")
}

pub fn subsystem_and_module_name_use_repository_relative_path_test() {
  let assert Ok([finding]) =
    inventory.findings_for_source(
      path: "src/scherzo/control/server.gleam",
      source: "pub fn configure(path: String, enabled: Bool) -> Nil {
  Nil
}",
    )

  assert finding.subsystem == "control and CLI surfaces"
  assert finding.module_name == "scherzo.control.server"
}

pub fn markdown_report_includes_module_grouping_and_rows_test() {
  let finding =
    single_candidate(
      "type Config { Config }

pub fn collect(path: String, retries: Int, ratio: Float, config: Config) -> Nil {
  Nil
}",
      inventory.HighArity,
    )
  let markdown = report.render_markdown([finding])

  assert string.contains(does: markdown, contain: "## Counts by module")
  assert string.contains(does: markdown, contain: "| scherzo.example | 1 |")
  assert string.contains(
    does: markdown,
    contain: "| Candidate | Subsystem | Module | Path | Function | Arity | API shape | Primitive | Existing rule coverage | Likely exception | Churn estimate |",
  )
  assert string.contains(
    does: markdown,
    contain: "| high-arity public functions | top-level utilities | `scherzo.example` | `src/scherzo/example.gleam` | `collect` | 4 | arity 4 |  | no |  | 1 |",
  )
}

pub fn labelled_other_parameter_does_not_hide_unlabelled_bool_test() {
  let finding =
    single_candidate(
      "pub fn configure(path path: String, enabled: Bool) -> Nil {
  Nil
}",
      inventory.BroaderBoolParameters,
    )

  assert finding.covered_by_existing_rule == True
  assert finding.api_shape
    == "unlabelled Bool parameter(s) in arity 2: 'enabled' (covered by scherzo_public_function_labels)"
}
