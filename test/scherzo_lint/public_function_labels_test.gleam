import glance
import gleam/string
import glinter/rule
import scherzo_lint/rules/public_function_labels

fn errors_for(source: String) -> List(rule.RuleError) {
  let assert Ok(module) = glance.module(source)

  rule.run_on_module(
    rule: public_function_labels.rule(),
    module: module,
    source: source,
  )
}

fn single_error(source: String) -> rule.RuleError {
  let assert [error] = errors_for(source)
  error
}

pub fn public_two_parameter_bool_reports_error_test() {
  let error =
    single_error(
      "pub fn configure(path: String, enabled: Bool) -> Nil {
  Nil
}",
    )

  assert string.contains(does: rule.error_message(error), contain: "configure")
  assert string.contains(does: rule.error_details(error), contain: "enabled")
  assert string.contains(does: rule.error_details(error), contain: "Bool")
}

pub fn labelled_public_bool_parameter_reports_no_errors_test() {
  assert errors_for(
      "pub fn configure(path: String, enabled enabled: Bool) -> Nil {
  Nil
}",
    )
    == []
}

pub fn private_two_parameter_bool_reports_no_errors_test() {
  assert errors_for(
      "fn configure(path: String, enabled: Bool) -> Nil {
  Nil
}",
    )
    == []
}

pub fn external_public_function_reports_no_errors_test() {
  assert errors_for(
      "@external(erlang, \"module\", \"configure\")
pub fn configure(path: String, enabled: Bool) -> Nil",
    )
    == []
}

pub fn discarded_bool_parameter_reports_no_errors_test() {
  assert errors_for(
      "pub fn configure(path: String, _: Bool) -> Nil {
  Nil
}",
    )
    == []
}

pub fn unannotated_parameter_reports_no_errors_test() {
  assert errors_for(
      "pub fn configure(path: String, enabled) -> Nil {
  Nil
}",
    )
    == []
}

pub fn three_parameter_bool_is_deferred_by_rollout_test() {
  assert errors_for(
      "pub fn launch(command: String, cwd: String, auto_retry: Bool) -> Nil {
  Nil
}",
    )
    == []
}

pub fn duplicate_string_parameters_are_deferred_by_rollout_test() {
  assert errors_for(
      "pub fn copy(source: String, destination: String) -> Nil {
  Nil
}",
    )
    == []
}

pub fn high_arity_without_bool_is_deferred_by_rollout_test() {
  assert errors_for(
      "pub fn configure(path: String, mode: String, retries: Int) -> Nil {
  Nil
}",
    )
    == []
}

pub fn labelled_other_parameter_does_not_hide_unlabelled_bool_test() {
  let error =
    single_error(
      "pub fn configure(path path: String, enabled: Bool) -> Nil {
  Nil
}",
    )

  assert string.contains(does: rule.error_message(error), contain: "configure")
  assert string.contains(does: rule.error_details(error), contain: "enabled")
}
