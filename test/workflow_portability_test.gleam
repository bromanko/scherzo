import gleam/list
import gleam/string
import simplifile

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn assert_contains(contents: String, expected: String) {
  assert string.contains(contents, expected)
}

fn assert_not_contains(contents: String, unexpected: String) {
  assert !string.contains(contents, unexpected)
}

fn assert_not_contains_any(contents: String, unexpected_terms: List(String)) {
  list.each(unexpected_terms, fn(term) { assert_not_contains(contents, term) })
}

fn local_pi_config_terms() -> List(String) {
  [
    "/Users/bromanko",
    "Code/llm-agents",
    "~/.pi",
    "~/.config/pi",
    "prompt-template setup",
  ]
}

pub fn execplan_workflow_skills_are_repo_local_and_checked_in_test() {
  let exec_plan_skill = read_file(".pi/skills/exec-plan/SKILL.md")
  let exec_plan_review_skill = read_file(".pi/skills/exec-plan-review/SKILL.md")

  assert_contains(exec_plan_skill, "\nname: exec-plan\n")
  assert_contains(exec_plan_skill, "# Execution Plans")
  assert_contains(exec_plan_review_skill, "\nname: exec-plan-review\n")
  assert_contains(exec_plan_review_skill, "../exec-plan/SKILL.md")

  let draft_prompt = read_file(".scherzo/workflows/prompts/execplan-draft.md")
  let review_prompt = read_file(".scherzo/workflows/prompts/execplan-review.md")
  let incorporate_prompt =
    read_file(".scherzo/workflows/prompts/execplan-incorporate-review.md")
  let revision_prompt =
    read_file(".scherzo/workflows/prompts/execplan-revision.md")
  let implementation_prompt =
    read_file(".scherzo/workflows/prompts/execplan-implementation-implement.md")

  assert_contains(draft_prompt, ".pi/skills/exec-plan/SKILL.md")
  assert_contains(revision_prompt, ".pi/skills/exec-plan/SKILL.md")
  assert_contains(review_prompt, ".pi/skills/exec-plan-review/SKILL.md")
  assert_contains(review_prompt, ".pi/skills/exec-plan/SKILL.md")
  assert_contains(incorporate_prompt, ".pi/skills/exec-plan/SKILL.md")
  assert_contains(incorporate_prompt, ".pi/skills/exec-plan-review/SKILL.md")
  assert_contains(implementation_prompt, ".pi/skills/exec-plan/SKILL.md")

  list.each(
    [
      draft_prompt,
      revision_prompt,
      review_prompt,
      incorporate_prompt,
      implementation_prompt,
    ],
    fn(prompt) { assert_not_contains_any(prompt, local_pi_config_terms()) },
  )
}

pub fn review_workflows_use_staged_artifacts_instead_of_local_review_skills_test() {
  let implementation_workflow =
    read_file(".scherzo/workflows/implementation.yaml")
  let execplan_implementation_workflow =
    read_file(".scherzo/workflows/execplan-implementation.yaml")
  let implementation_prompt =
    read_file(".scherzo/workflows/prompts/code-review.md")
  let execplan_prompt =
    read_file(".scherzo/workflows/prompts/execplan-implementation-review.md")

  list.each(
    [implementation_workflow, execplan_implementation_workflow],
    fn(workflow) {
      assert_contains(workflow, "correctness_review_lane")
      assert_contains(workflow, "test_quality_review_lane")
      assert_contains(workflow, "idioms_maintainability_review_lane")
      assert_contains(workflow, "security_performance_review_lane")
      assert_contains(workflow, "synthesize_review")
      assert_contains(workflow, "scripts/scherzo-review")
    },
  )

  list.each([implementation_prompt, execplan_prompt], fn(prompt) {
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "scripts/scherzo-review")
    assert_contains(prompt, "Do not invoke local pi slash commands")
    assert_not_contains(prompt, "`/review")
    assert_not_contains(prompt, ".pi/skills/gleam")
    assert_not_contains(prompt, "gleam-review")
  })

  let assert Ok(False) = simplifile.is_directory(".pi/skills/gleam-review")
  let assert Ok(False) = simplifile.is_directory(".pi/skills/gleam-code-review")
  let assert Ok(False) = simplifile.is_directory(".pi/skills/gleam-test-review")
  let assert Ok(False) =
    simplifile.is_directory(".pi/skills/gleam-security-review")
  let assert Ok(False) =
    simplifile.is_directory(".pi/skills/gleam-performance-review")
}

pub fn workflow_docs_explain_vendored_skill_update_and_validation_test() {
  let docs = read_file(".scherzo/README.md")

  assert_contains(docs, "## Vendored pi skills and portability")
  assert_contains(docs, ".pi/skills/exec-plan/SKILL.md")
  assert_contains(docs, ".pi/skills/exec-plan-review/SKILL.md")
  assert_contains(docs, "copy the canonical skill content")
  assert_contains(docs, "frontmatter `name:`")
  assert_contains(docs, "workflow portability validation")
  assert_contains(docs, "doctor --check workflow-config")
  assert_contains(docs, "scripts/scherzo-review")
  assert_contains(docs, "workspace.profiles.dogfood-jj")
  assert_contains(docs, "workspace_profile: dogfood-jj")
  assert_contains(docs, "do not add language-specific review skills")
  assert_contains(docs, "LIV-115")
}
