import gleam/list
import gleam/string
import simplifile

const exec_plan_guidance = "workflows/dogfood/guidance/exec-plan.md"

const exec_plan_review_guidance = "workflows/dogfood/guidance/exec-plan-review.md"

pub fn execplan_workflow_guidance_is_vendored_under_workflows_test() {
  let authoring = read_file(exec_plan_guidance)
  assert string.contains(authoring, "# Execution Plans (ExecPlans)")
  assert string.contains(authoring, "## Living Document Sections")
  assert string.contains(authoring, "## Mode 2: Implementing an ExecPlan")
  assert_all_present(authoring, required_living_document_headings())
  assert_all_present(authoring, required_execution_context_headings())
  assert_avoids_machine_local_skill_paths(authoring)

  let review = read_file(exec_plan_review_guidance)
  assert string.contains(review, "# Adversarial ExecPlan Review")
  assert string.contains(review, exec_plan_guidance)
  assert_avoids_machine_local_skill_paths(review)
}

pub fn execplan_prompts_load_repo_local_guidance_test() {
  list.each(authoring_prompt_paths(), fn(path) {
    let prompt = read_file(path)
    assert string.contains(prompt, exec_plan_guidance)
    assert_avoids_machine_local_skill_paths(prompt)
  })

  let review_prompt = read_file("workflows/dogfood/prompts/execplan-review.md")
  assert string.contains(review_prompt, exec_plan_review_guidance)
  assert string.contains(review_prompt, exec_plan_guidance)
  assert_avoids_machine_local_skill_paths(review_prompt)
}

pub fn execplan_review_doc_prompts_preserve_living_sections_test() {
  list.each(review_doc_prompt_paths(), fn(path) {
    let prompt = read_file(path)
    assert_all_present(prompt, required_living_document_sections())
  })
}

pub fn execplan_workflow_prompts_avoid_machine_local_skill_paths_test() {
  list.each(execplan_prompt_paths(), fn(path) {
    let prompt = read_file(path)
    assert_avoids_machine_local_skill_paths(prompt)
  })
}

fn assert_all_present(haystack: String, needles: List(String)) -> Nil {
  list.each(needles, fn(needle) {
    assert string.contains(haystack, needle)
  })
}

fn assert_avoids_machine_local_skill_paths(contents: String) -> Nil {
  assert !string.contains(contents, "/Users/")
  assert !string.contains(contents, "/home/")
  assert !string.contains(contents, "llm-agents/skills")
  assert !string.contains(contents, ".pi/skills")
  assert !string.contains(contents, "SKILL.md")
}

fn required_living_document_sections() -> List(String) {
  [
    "Progress",
    "Surprises & Discoveries",
    "Decision Log",
    "Outcomes & Retrospective",
  ]
}

fn required_living_document_headings() -> List(String) {
  [
    "## Progress",
    "## Surprises & Discoveries",
    "## Decision Log",
    "## Outcomes & Retrospective",
  ]
}

fn required_execution_context_headings() -> List(String) {
  [
    "## Context and Orientation",
    "## Validation and Acceptance",
    "## Rollout, Recovery, and Idempotence",
  ]
}

fn review_doc_prompt_paths() -> List(String) {
  [
    "workflows/dogfood/prompts/execplan-draft.md",
    "workflows/dogfood/prompts/execplan-incorporate-review.md",
    "workflows/dogfood/prompts/execplan-review.md",
    "workflows/dogfood/prompts/execplan-revision.md",
  ]
}

fn authoring_prompt_paths() -> List(String) {
  [
    "workflows/dogfood/prompts/execplan-draft.md",
    "workflows/dogfood/prompts/execplan-incorporate-review.md",
    "workflows/dogfood/prompts/execplan-revision.md",
    "workflows/dogfood/prompts/execplan-implementation-implement.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-late-plan-completion-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-final-plan-completion-feedback.md",
  ]
}

fn execplan_prompt_paths() -> List(String) {
  [
    "workflows/dogfood/prompts/execplan-draft.md",
    "workflows/dogfood/prompts/execplan-incorporate-review.md",
    "workflows/dogfood/prompts/execplan-pr-body.md",
    "workflows/dogfood/prompts/execplan-recover-failed-step.md",
    "workflows/dogfood/prompts/execplan-review.md",
    "workflows/dogfood/prompts/execplan-revision.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-final-plan-completion-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-late-plan-completion-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-implement.md",
    "workflows/dogfood/prompts/execplan-implementation-recover-failed-step.md",
    "workflows/dogfood/prompts/execplan-implementation-repair-base-drift.md",
    "workflows/dogfood/prompts/execplan-implementation-review.md",
    "workflows/dogfood/prompts/execplan-implementation-verify-completion.md",
    "workflows/dogfood/prompts/execplan-implementation-verify-completion-after-feedback.md",
    "workflows/dogfood/prompts/execplan-implementation-verify-completion-after-final-repair.md",
    "workflows/dogfood/prompts/execplan-implementation-verify-completion-after-late-repair.md",
    "workflows/dogfood/prompts/execplan-implementation-verify-completion-before-final-validation.md",
  ]
}

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}
