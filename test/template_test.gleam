import birl
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import scherzo/error
import scherzo/template
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "ABC-123",
    title: "Fix tests",
    description: Some("Tests are broken"),
    priority: Some(1),
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: ["bug", "tests"],
    blocked_by: [],
    blocked_by_complete: True,
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

pub fn issue_template_variables_are_characterized_test() {
  let issue =
    tracker_issue.Issue(
      ..issue(),
      id: "issue-1",
      identifier: "LIV-266",
      title: "Refresh architecture",
      priority: Some(2),
      state: issue_state.from_string_unchecked("Todo"),
      branch_name: Some("liv-266-refresh"),
      url: Some("https://linear.app/living-systems/issue/LIV-266"),
      labels: ["workflow:execplan", "kind:feature"],
    )
  let assert Ok(rendered) =
    template.render(
      "{{ issue.id }}|{{ issue.identifier }}|{{ issue.title }}|{{ issue.branch_name }}|{{ issue.url }}|{{ issue.state }}|{{ issue.priority }}|{{ issue.labels }}|{% for label in issue.labels %}{{ label }};{% endfor %}",
      issue,
      None,
    )

  assert rendered
    == "issue-1|LIV-266|Refresh architecture|liv-266-refresh|https://linear.app/living-systems/issue/LIV-266|Todo|2||workflow:execplan;kind:feature;"
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

pub fn render_with_locals_renders_step_artifact_variables_test() {
  let assert Ok(rendered) =
    template.render_with_locals(
      "Review: {{ steps.code_review.final_response }}",
      issue(),
      None,
      [#("steps.code_review.final_response", template.VString("Looks good"))],
    )
  assert rendered == "Review: Looks good"
}

pub fn render_with_locals_unknown_artifact_variable_still_fails_test() {
  let assert Error(_) =
    template.render_with_locals(
      "Review: {{ steps.security_review.final_response }}",
      issue(),
      None,
      [#("steps.code_review.final_response", template.VString("Looks good"))],
    )
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
  let issue = tracker_issue.Issue(..issue(), description: None)
  let assert Ok(rendered) =
    template.render("{{ issue.description }}", issue, None)
  assert string.trim(rendered) == ""
}

pub fn renders_scheduled_context_variables_test() {
  let scheduled =
    template.ScheduledTemplateContext(
      job_id: "pr-conflict-repair",
      workflow_id: "pr-conflict-repair",
      due_at: "2026-05-05T12:00:00Z",
      started_at: "2026-05-05T12:00:03Z",
      run_id: "schedule-pr-conflict-repair-20260505T120000Z",
      attempt: 2,
    )
  let assert Ok(rendered) =
    template.render_scheduled(
      "{{ scheduled_job.id }} {{ scheduled_job.workflow }} {{ schedule.due_at }} {{ schedule.started_at }} {{ run.id }} {{ run.attempt }} {{ attempt }}",
      scheduled,
    )
  assert rendered
    == "pr-conflict-repair pr-conflict-repair 2026-05-05T12:00:00Z 2026-05-05T12:00:03Z schedule-pr-conflict-repair-20260505T120000Z 2 2"
}

pub fn scheduled_context_does_not_expose_issue_variables_test() {
  let scheduled =
    template.ScheduledTemplateContext(
      job_id: "repair",
      workflow_id: "repair",
      due_at: "2026-05-05T12:00:00Z",
      started_at: "2026-05-05T12:00:00Z",
      run_id: "schedule-repair-20260505T120000Z",
      attempt: 1,
    )
  let assert Error(_) =
    template.render_scheduled("{{ issue.identifier }}", scheduled)
}

fn include_entry(
  source_path: String,
  include_path: String,
  resolved_path: String,
  contents: String,
) -> #(String, template.IncludeDependency) {
  #(
    source_path <> "::" <> include_path,
    template.IncludeDependency(path: resolved_path, contents: contents),
  )
}

fn include_resolver(
  entries: List(#(String, template.IncludeDependency)),
) -> fn(String, String) ->
  Result(template.IncludeDependency, error.TemplateError) {
  let index = dict.from_list(entries)
  fn(include_path: String, source_path: String) {
    case dict.get(index, source_path <> "::" <> include_path) {
      Ok(dependency) -> Ok(dependency)
      Error(_) ->
        Error(error.TemplateRenderError(
          "missing include " <> include_path <> " from " <> source_path,
        ))
    }
  }
}

pub fn include_expansion_inlines_fragment_before_rendering_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/policy.md",
        "/bundle/prompts/fragments/policy.md",
        "Policy for {{ issue.identifier }}",
      ),
    ])
  let assert Ok(expansion) =
    template.expand_includes(
      "Intro\n{% include \"fragments/policy.md\" %}\nOutro",
      "/bundle/prompts/implement.md",
      resolver,
    )
  let assert Ok(rendered) = template.render(expansion.contents, issue(), None)

  assert rendered == "Intro\nPolicy for ABC-123\nOutro"
}

pub fn nested_include_expands_with_host_context_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/outer.md",
        "/bundle/prompts/fragments/outer.md",
        "Outer {% include \"inner.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/outer.md",
        "inner.md",
        "/bundle/prompts/fragments/inner.md",
        "{{ issue.title }}",
      ),
    ])
  let assert Ok(expansion) =
    template.expand_includes(
      "{% include \"fragments/outer.md\" %}",
      "/bundle/prompts/implement.md",
      resolver,
    )
  let assert Ok(rendered) = template.render(expansion.contents, issue(), None)

  assert rendered == "Outer Fix tests"
}

pub fn include_fragment_can_contain_variables_if_and_for_blocks_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/body.md",
        "/bundle/prompts/fragments/body.md",
        "{{ issue.identifier }}:{% if attempt %} retry{% else %} first{% endif %}:{% for label in issue.labels %}[{{ label }}]{% endfor %}",
      ),
    ])
  let assert Ok(expansion) =
    template.expand_includes(
      "{% include \"fragments/body.md\" %}",
      "/bundle/prompts/implement.md",
      resolver,
    )
  let assert Ok(rendered) =
    template.render(expansion.contents, issue(), Some(1))

  assert rendered == "ABC-123: retry:[bug][tests]"
}

pub fn include_missing_file_returns_template_error_test() {
  let resolver = include_resolver([])
  let assert Error(error.TemplateRenderError(message)) =
    template.expand_includes(
      "{% include \"fragments/missing.md\" %}",
      "/bundle/prompts/implement.md",
      resolver,
    )

  assert string.contains(message, "missing include")
}

pub fn include_cycle_returns_clear_template_error_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/outer.md",
        "/bundle/prompts/fragments/outer.md",
        "{% include \"inner.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/outer.md",
        "inner.md",
        "/bundle/prompts/fragments/inner.md",
        "{% include \"outer.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/inner.md",
        "outer.md",
        "/bundle/prompts/fragments/outer.md",
        "{% include \"inner.md\" %}",
      ),
    ])
  let assert Error(error.TemplateRenderError(message)) =
    template.expand_includes(
      "{% include \"fragments/outer.md\" %}",
      "/bundle/prompts/implement.md",
      resolver,
    )

  assert string.contains(message, "include cycle")
  assert string.contains(message, "/bundle/prompts/fragments/outer.md")
}

pub fn include_depth_limit_returns_clear_template_error_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/one.md",
        "/bundle/prompts/fragments/one.md",
        "{% include \"two.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/one.md",
        "two.md",
        "/bundle/prompts/fragments/two.md",
        "{% include \"three.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/two.md",
        "three.md",
        "/bundle/prompts/fragments/three.md",
        "{% include \"four.md\" %}",
      ),
      include_entry(
        "/bundle/prompts/fragments/three.md",
        "four.md",
        "/bundle/prompts/fragments/four.md",
        "too deep",
      ),
    ])
  let assert Error(error.TemplateRenderError(message)) =
    template.expand_includes_with_limit(
      "{% include \"fragments/one.md\" %}",
      "/bundle/prompts/implement.md",
      3,
      resolver,
    )

  assert string.contains(message, "include depth limit exceeded")
  assert string.contains(message, "four.md")
}

pub fn malformed_include_tag_returns_template_error_test() {
  let resolver = include_resolver([])
  let assert Error(error.TemplateRenderError(message)) =
    template.expand_includes(
      "{% include fragments/policy.md %}",
      "/bundle/prompts/implement.md",
      resolver,
    )

  assert string.contains(message, "malformed include tag")
}

pub fn referenced_variables_with_includes_traverses_fragment_variables_test() {
  let resolver =
    include_resolver([
      include_entry(
        "/bundle/prompts/implement.md",
        "fragments/body.md",
        "/bundle/prompts/fragments/body.md",
        "{{ issue.identifier }}{% if issue.description %}{{ attempt }}{% endif %}{% for label in issue.labels %}{{ label }}{% endfor %}",
      ),
    ])
  let assert Ok(variables) =
    template.referenced_variables_with_includes(
      "{% include \"fragments/body.md\" %}",
      "/bundle/prompts/implement.md",
      resolver,
    )

  assert variables
    == [
      "issue.identifier",
      "issue.description",
      "attempt",
      "issue.labels",
      "label",
    ]
}

pub fn referenced_variables_scans_variables_if_and_for_tags_test() {
  let variables =
    template.referenced_variables(
      "{{ issue.title }}{% if issue.description %}x{% endif %}{% for label in issue.labels %}{{ label }}{% endfor %}",
    )
  assert variables
    == ["issue.title", "issue.description", "issue.labels", "label"]
}
