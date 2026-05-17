import gleam/list
import gleam/string
import simplifile

fn read_file(path: String) -> String {
  let assert Ok(contents) = simplifile.read(path)
  contents
}

fn assert_contains(contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = "expected text not found: " <> expected
      panic as message
    }
  }
}

fn assert_not_contains(contents: String, unexpected: String) -> Nil {
  case string.contains(contents, unexpected) {
    False -> Nil
    True -> {
      let message = "unexpected text still present: " <> unexpected
      panic as message
    }
  }
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

fn active_operator_guidance_paths() -> List(String) {
  [
    ".scherzo/README.md",
    "docs/GETTING_STARTED.md",
    "docs/review-artifacts.md",
    ".config/selfci/ci.sh",
    ".pi/skills/scherzo-operator/references/commands.md",
  ]
}

fn execplan_prompt_paths() -> List(String) {
  [
    ".scherzo/workflows/prompts/execplan-draft.md",
    ".scherzo/workflows/prompts/execplan-review.md",
    ".scherzo/workflows/prompts/execplan-incorporate-review.md",
    ".scherzo/workflows/prompts/execplan-revision.md",
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
  ]
}

fn retired_v2_suffix_paths() -> List(String) {
  [
    ".scherzo/workflows/execplan-v2.yaml",
    ".scherzo/workflows/execplan-revision-v2.yaml",
    ".scherzo/workflows/execplan-implementation-v2.yaml",
    ".scherzo/workflows/prompts/execplan-v2-draft.md",
    ".scherzo/workflows/prompts/execplan-v2-review.md",
    ".scherzo/workflows/prompts/execplan-v2-incorporate-review.md",
    ".scherzo/workflows/prompts/execplan-revision-v2.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-implementation-v2-verify-completion-before-final-validation.md",
    "scripts/scherzo-execplan-v2",
  ]
}

pub fn execplan_v2_suffix_workflow_files_are_retired_test() {
  list.each(retired_v2_suffix_paths(), fn(path) {
    let assert Ok(False) = simplifile.is_file(path)
  })
}

pub fn execplan_workflows_resolve_helpers_from_repo_root_test() {
  list.each(
    [
      ".scherzo/workflows/execplan.yaml",
      ".scherzo/workflows/execplan-revision.yaml",
      ".scherzo/workflows/execplan-implementation.yaml",
    ],
    fn(path) {
      let workflow = read_file(path)
      assert_contains(workflow, "repo_root=${SCHERZO_REPO_ROOT:-$(cd")
      assert_contains(workflow, "\"$repo_root/scripts/scherzo-execplan\"")
      assert_not_contains(workflow, "run: scripts/scherzo-execplan")
    },
  )
}

pub fn execplan_family_prompts_do_not_reference_repo_local_skill_files_test() {
  list.each(execplan_prompt_paths(), fn(path) {
    let prompt = read_file(path)
    assert_not_contains(prompt, ".pi/skills")
    assert_not_contains_any(prompt, local_pi_config_terms())
    assert_not_contains(prompt, "`scripts/scherzo-")
    assert_not_contains(prompt, "python3 scripts/scherzo-")
    assert_not_contains(prompt, "run scripts/scherzo-")
    assert_not_contains(prompt, "Run scripts/scherzo-")
    assert_not_contains(prompt, "use scripts/scherzo-")
    assert_not_contains(prompt, "Use scripts/scherzo-")
  })

  let assert Ok(False) = simplifile.is_directory(".pi/skills/exec-plan")
  let assert Ok(False) = simplifile.is_directory(".pi/skills/exec-plan-review")
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
      assert_contains(workflow, "lane_correctness")
      assert_contains(workflow, "lane_test_quality")
      assert_contains(workflow, "lane_idioms_maintainability")
      assert_contains(workflow, "lane_security_performance")
      assert_contains(workflow, "submit_review_lane_draft")
      assert_contains(workflow, "prepare-native")
      assert_contains(workflow, "synthesize_review")
      assert_contains(workflow, "scripts/scherzo-review")
      assert_not_contains(workflow, "run-lane --lane")
    },
  )

  list.each([implementation_prompt, execplan_prompt], fn(prompt) {
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "\"$repo_root/scripts/scherzo-review\"")
    assert_contains(prompt, "Do not invoke local pi slash commands")
    assert_not_contains(prompt, "`/review")
    assert_not_contains(prompt, "`scripts/scherzo-review")
    assert_not_contains(prompt, ".pi/skills/gleam")
    assert_not_contains(prompt, "gleam-review")
  })
}

pub fn implementation_like_workflows_use_workspace_driver_language_test() {
  let workflow_expectations = [
    #(
      ".scherzo/workflows/implementation.yaml",
      "workspace_capabilities: [status, diff, changed-files, baseline, refresh-base, publish-change]",
    ),
    #(
      ".scherzo/workflows/execplan.yaml",
      "workspace_capabilities: [status, diff, changed-files, publish-change]",
    ),
    #(
      ".scherzo/workflows/execplan-revision.yaml",
      "workspace_capabilities: [status, diff, changed-files, refresh-base, publish-change]",
    ),
    #(
      ".scherzo/workflows/execplan-implementation.yaml",
      "workspace_capabilities: [status, diff, changed-files, baseline, refresh-base, publish-change]",
    ),
    #(
      ".scherzo/workflows/merge-conflict-resolution.yaml",
      "workspace_capabilities: [status, diff, changed-files, publish-change]",
    ),
  ]

  list.each(workflow_expectations, fn(expectation) {
    let #(path, capabilities) = expectation
    let workflow = read_file(path)
    assert_contains(workflow, "workspace_profile: dogfood-jj")
    assert_contains(workflow, capabilities)
    assert_not_contains(workflow, "--from @- --to @")
  })

  let prompt_paths = [
    ".scherzo/workflows/prompts/implement.md",
    ".scherzo/workflows/prompts/code-review.md",
    ".scherzo/workflows/prompts/apply-feedback.md",
    ".scherzo/workflows/prompts/repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-draft.md",
    ".scherzo/workflows/prompts/execplan-review.md",
    ".scherzo/workflows/prompts/execplan-incorporate-review.md",
    ".scherzo/workflows/prompts/execplan-revision.md",
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-repair-base-drift.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
    ".scherzo/workflows/prompts/resolve-merge-conflicts.md",
  ]

  list.each(prompt_paths, fn(path) {
    let prompt = read_file(path)
    assert_not_contains_any(prompt, [
      "dedicated jj workspace",
      "jj status --color=never",
      "jj diff --color=never",
      "jj diff --from @-",
      "manage jj workspaces",
    ])
  })
}

pub fn execplan_prompts_describe_bundle_handoff_test() {
  let draft_prompt = read_file(".scherzo/workflows/prompts/execplan-draft.md")
  let review_prompt = read_file(".scherzo/workflows/prompts/execplan-review.md")
  let incorporate_prompt =
    read_file(".scherzo/workflows/prompts/execplan-incorporate-review.md")
  let implementation_prompt =
    read_file(".scherzo/workflows/prompts/execplan-implementation-implement.md")
  let revision_prompt =
    read_file(".scherzo/workflows/prompts/execplan-revision.md")

  assert_contains(draft_prompt, "workflow:execplan")
  assert_contains(draft_prompt, "submit_implementation_pack_submission")
  assert_contains(draft_prompt, "Bundle ref:")
  assert_contains(review_prompt, "workflow:execplan")
  assert_contains(incorporate_prompt, "implementation pack")
  assert_contains(implementation_prompt, "workflow:execplan-implementation")
  assert_contains(implementation_prompt, "tmp/execplan-review-doc.md")
  assert_contains(
    implementation_prompt,
    "tmp/execplan-implementation-pack.json",
  )
  assert_contains(implementation_prompt, "tmp/execplan-conflict.md")
  assert_contains(revision_prompt, "workflow:execplan-revision")
  assert_contains(revision_prompt, "Bundle ref:")
}

pub fn workflow_docs_explain_canonical_execplan_routing_and_validation_test() {
  let docs = read_file(".scherzo/README.md")

  assert_contains(docs, "## Workflow-packaged guidance and portability")
  assert_contains(docs, "Bundle-based ExecPlan workflows")
  assert_contains(docs, ".scherzo/workflows/schemas/")
  assert_contains(docs, "repo_root=${SCHERZO_REPO_ROOT:-$(cd")
  assert_contains(docs, "\"$repo_root/scripts/scherzo-execplan\"")
  assert_contains(docs, "workflow portability validation")
  assert_contains(docs, "doctor --check workflow-config")
  assert_contains(docs, "scripts/scherzo-review")
  assert_contains(docs, "workflow:execplan")
  assert_contains(docs, "workflow:execplan-revision")
  assert_contains(docs, "workflow:execplan-implementation")
  assert_contains(docs, "The former `-v2` ExecPlan workflow labels are retired")
  assert_not_contains(docs, "workflow:execplan-v2")
  assert_not_contains(docs, "workflow:execplan-revision-v2")
  assert_not_contains(docs, "workflow:execplan-implementation-v2")
  assert_contains(docs, "workspace.profiles.dogfood-jj")
  assert_contains(docs, "workspace_profile: dogfood-jj")
  assert_contains(docs, "do not add language-specific review skills")
}

pub fn active_operator_guidance_uses_canonical_execplan_names_test() {
  list.each(active_operator_guidance_paths(), fn(path) {
    let docs = read_file(path)
    assert_not_contains(docs, "workflow:execplan-v2")
    assert_not_contains(docs, "workflow:execplan-revision-v2")
    assert_not_contains(docs, "workflow:execplan-implementation-v2")
    assert_not_contains(docs, ".scherzo/workflows/execplan-v2.yaml")
    assert_not_contains(docs, ".scherzo/workflows/execplan-revision-v2.yaml")
    assert_not_contains(
      docs,
      ".scherzo/workflows/execplan-implementation-v2.yaml",
    )
    assert_not_contains(docs, "scripts/scherzo-execplan-v2")
    assert_not_contains(docs, "execplan-v2:")
    assert_not_contains(docs, "execplan-revision-v2")
    assert_not_contains(docs, "execplan-implementation-v2")
  })

  assert_contains(read_file("docs/GETTING_STARTED.md"), "      execplan:\n")
  assert_contains(
    read_file("docs/review-artifacts.md"),
    "--workflow .scherzo/workflows/execplan-implementation.yaml",
  )
}
