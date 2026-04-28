import birl
import gleam/option.{None, Some}
import gleam/string
import scherzo/domain
import scherzo/template

fn issue() -> domain.Issue {
  domain.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Fix tests",
    description: Some("Tests are broken"),
    priority: Some(1),
    state: "Todo",
    branch_name: None,
    url: None,
    labels: ["bug", "tests"],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(1)),
  )
}

pub fn renders_issue_identifier_and_nested_fields_test() {
  let assert Ok(rendered) =
    template.render(
      "{{ issue.identifier }}: {{ issue.title }} - {{ issue.description }}",
      issue(),
      None,
    )
  assert rendered == "ABC-123: Fix tests - Tests are broken"
}

pub fn renders_labels_through_loop_test() {
  let assert Ok(rendered) =
    template.render(
      "{% for label in issue.labels %}[{{ label }}]{% endfor %}",
      issue(),
      None,
    )
  assert rendered == "[bug][tests]"
}

pub fn attempt_renders_empty_first_run_and_integer_on_retry_test() {
  let assert Ok(first) = template.render("Attempt={{ attempt }}", issue(), None)
  assert first == "Attempt="
  let assert Ok(retry) =
    template.render("Attempt={{ attempt }}", issue(), Some(2))
  assert retry == "Attempt=2"
}

pub fn if_attempt_truthiness_test() {
  let assert Ok(first) =
    template.render(
      "{% if attempt %}retry{% else %}first{% endif %}",
      issue(),
      None,
    )
  assert first == "first"
  let assert Ok(retry) =
    template.render(
      "{% if attempt %}retry{% else %}first{% endif %}",
      issue(),
      Some(1),
    )
  assert retry == "retry"
}

pub fn empty_prompt_uses_fallback_test() {
  let assert Ok(rendered) = template.render("  \n", issue(), None)
  assert rendered == "You are working on an issue from Linear."
}

pub fn unknown_variable_fails_test() {
  let assert Error(_) =
    template.render("Hello {{ issue.unknown }}", issue(), None)
}

pub fn unknown_filter_fails_test() {
  let assert Error(_) =
    template.render("{{ issue.title | upcase }}", issue(), None)
}

pub fn unknown_tag_and_malformed_blocks_fail_test() {
  let assert Error(_) = template.render("{% include x %}", issue(), None)
  let assert Error(_) =
    template.render("{% if attempt %}missing end", issue(), None)
  let assert Error(_) =
    template.render("{% for label issue.labels %}x{% endfor %}", issue(), None)
}

pub fn nested_blocks_render_with_matching_end_tags_test() {
  let assert Ok(nested_if) =
    template.render(
      "{% if issue.description %}outer {% if attempt %}retry{% else %}first{% endif %}{% endif %}",
      issue(),
      None,
    )
  assert nested_if == "outer first"

  let assert Ok(nested_for) =
    template.render(
      "{% for label in issue.labels %}{% for inner in issue.labels %}[{{ label }}:{{ inner }}]{% endfor %}{% endfor %}",
      issue(),
      None,
    )
  assert nested_for == "[bug:bug][bug:tests][tests:bug][tests:tests]"

  let assert Ok(for_containing_if) =
    template.render(
      "{% for label in issue.labels %}{% if attempt %}retry{% else %}{{ label }}{% endif %};{% endfor %}",
      issue(),
      None,
    )
  assert for_containing_if == "bug;tests;"
}

pub fn optional_none_renders_empty_test() {
  let issue = domain.Issue(..issue(), description: None)
  let assert Ok(rendered) =
    template.render("{{ issue.description }}", issue, None)
  assert string.trim(rendered) == ""
}
