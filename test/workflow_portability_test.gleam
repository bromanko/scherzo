import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
import scherzo/path as scherzo_path
import scherzo/runtime_bundle
import scherzo/step_artifact
import scherzo/workflow_dag
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

fn execplan_prompt_paths() -> List(String) {
  [
    ".scherzo/workflows/prompts/execplan-draft.md",
    ".scherzo/workflows/prompts/execplan-repair-validation.md",
    ".scherzo/workflows/prompts/execplan-review.md",
    ".scherzo/workflows/prompts/execplan-incorporate-review.md",
    ".scherzo/workflows/prompts/execplan-revision.md",
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-before-final-validation.md",
  ]
}

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("TEST")
    _ -> None
  }
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn command_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn absolute_path(path: String) -> String {
  let assert Ok(absolute) = scherzo_path.absolute(path)
  absolute
}

fn chmod_executable(path: String) -> Nil {
  let artifact =
    command_step.run(
      "chmod_workflow_portability_driver",
      "chmod +x " <> shell_quote(path),
      ".",
      5000,
      [],
      command_limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn write_driver_script(dir: String) -> Nil {
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/scripts")
  let path = dir <> "/scripts/fake-workspace-driver"
  let assert Ok(Nil) =
    simplifile.write(
      path,
      "#!/bin/sh\nif [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"status\",\"diff\",\"changed-files\",\"publish-change\"]}'\n  exit 0\nfi\nexit 2\n",
    )
  chmod_executable(path)
}

fn write_execplan_helper(dir: String) -> String {
  let assert Ok(Nil) = simplifile.create_directory_all(dir <> "/scripts")
  let calls_path = absolute_path(dir <> "/execplan-helper-calls.txt")
  let helper_path = dir <> "/scripts/scherzo-execplan"
  let assert Ok(Nil) = simplifile.write(calls_path, "")
  let assert Ok(Nil) =
    simplifile.write(
      helper_path,
      "#!/bin/sh\nprintf '%s\\n' \"$*\" >> "
        <> shell_quote(calls_path)
        <> "\nprintf 'helper:%s\\n' \"$*\"\n",
    )
  chmod_executable(helper_path)
  calls_path
}

fn write_execplan_consumer_bundle(dir: String) -> String {
  let config_dir = dir <> "/.scherzo"
  let prompt_dir = config_dir <> "/workflows/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_dir <> "/workflows/execplan.yaml",
      read_file(".scherzo/workflows/execplan.yaml"),
    )
  list.each(
    [
      "execplan-draft.md",
      "execplan-repair-validation.md",
      "execplan-review.md",
      "execplan-incorporate-review.md",
    ],
    fn(name) {
      let assert Ok(Nil) =
        simplifile.write(
          prompt_dir <> "/" <> name,
          read_file(".scherzo/workflows/prompts/" <> name),
        )
      Nil
    },
  )

  let config_path = config_dir <> "/scherzo.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: dogfood-jj\n  profiles:\n    dogfood-jj:\n      driver:\n        command: scripts/fake-workspace-driver\n        lifecycle: [create, before-step, after-step, remove]\n        timeout_ms: 60000\nrouting:\n  workflows:\n    execplan: workflows/execplan.yaml\n",
    )
  config_path
}

fn run_execplan_step(
  dag: workflow_dag.WorkflowDag,
  step_id: String,
  workspace_dir: String,
  env: List(#(String, String)),
) -> Nil {
  let assert Ok(step) = workflow_dag.step_by_id(dag, step_id)
  let assert workflow_dag.CommandStep(run, _) = step.kind
  let artifact =
    command_step.run_with_env(
      "workflow_portability_" <> step_id,
      run,
      workspace_dir,
      5000,
      env,
      [],
      command_limits(),
    )
  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
}

fn execplan_command_calls(
  dag: workflow_dag.WorkflowDag,
  calls_path: String,
  workspace_dir: String,
  env: List(#(String, String)),
) -> List(String) {
  let assert Ok(Nil) = simplifile.write(calls_path, "")
  list.each(
    ["validate_draft", "create_pr", "create_implementation_issue"],
    fn(step_id) { run_execplan_step(dag, step_id, workspace_dir, env) },
  )
  read_file(calls_path) |> string.trim |> string.split(on: "\n")
}

pub fn workflow_execplan_embeds_guidance_and_avoids_repo_local_skills_test() {
  let workflow = read_file(".scherzo/workflows/execplan.yaml")
  let draft_prompt = read_file(".scherzo/workflows/prompts/execplan-draft.md")
  let repair_prompt =
    read_file(".scherzo/workflows/prompts/execplan-repair-validation.md")
  let review_prompt = read_file(".scherzo/workflows/prompts/execplan-review.md")
  let incorporate_prompt =
    read_file(".scherzo/workflows/prompts/execplan-incorporate-review.md")

  assert_contains(workflow, "repo_root=${SCHERZO_REPO_ROOT:-$(cd")
  assert_contains(workflow, "\"$repo_root/scripts/scherzo-execplan\" validate")
  assert_contains(workflow, "\"$repo_root/scripts/scherzo-execplan\" create-pr")
  assert_contains(
    workflow,
    "\"$repo_root/scripts/scherzo-execplan\" create-implementation-issue",
  )
  assert_not_contains(workflow, "run: scripts/scherzo-execplan")

  assert_contains(draft_prompt, "Workflow-packaged ExecPlan authoring standard")
  assert_contains(
    review_prompt,
    "Workflow-packaged adversarial ExecPlan review standard",
  )
  assert_contains(
    incorporate_prompt,
    "Workflow-packaged ExecPlan incorporation standard",
  )
  assert_contains(repair_prompt, "root-resolved `scherzo-execplan validate`")

  list.each(
    [draft_prompt, repair_prompt, review_prompt, incorporate_prompt],
    fn(prompt) {
      assert_not_contains(prompt, ".pi/skills")
      assert_not_contains_any(prompt, local_pi_config_terms())
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

pub fn workflow_execplan_prompt_bundle_loads_without_repo_local_skills_test() {
  let dir = "test/tmp/execplan-consumer-no-skills"
  reset_dir(dir)
  write_driver_script(dir)
  let assert Ok(Nil) =
    simplifile.create_directory_all(dir <> "/workflows/prompts")
  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/workflows/execplan.yaml",
      read_file(".scherzo/workflows/execplan.yaml"),
    )

  list.each(
    [
      "execplan-draft.md",
      "execplan-repair-validation.md",
      "execplan-review.md",
      "execplan-incorporate-review.md",
    ],
    fn(name) {
      let assert Ok(Nil) =
        simplifile.write(
          dir <> "/workflows/prompts/" <> name,
          read_file(".scherzo/workflows/prompts/" <> name),
        )
      Nil
    },
  )

  let assert Ok(Nil) =
    simplifile.write(
      dir <> "/scherzo.yaml",
      "version: 1\ntracker:\n  kind: linear\n  api_key: linearkey\n  project_slug: TEST\n  dispatch_states: [Todo]\nworkspace:\n  root: workspaces\n  default_profile: dogfood-jj\n  profiles:\n    dogfood-jj:\n      driver:\n        command: scripts/fake-workspace-driver\n        lifecycle: [create, before-step, after-step, remove]\n        timeout_ms: 60000\nrouting:\n  workflows:\n    execplan: workflows/execplan.yaml\n",
    )

  let assert Ok(False) = simplifile.is_directory(dir <> "/.pi")
  let assert Ok(bundle) =
    runtime_bundle.load_with_env(Some(dir <> "/scherzo.yaml"), env)
  let assert Ok(dag) = dict.get(bundle.workflows, "execplan")
  let prompts =
    dag.steps
    |> list.filter_map(fn(step) {
      case step.kind {
        workflow_dag.AgentStep(workflow_dag.PromptInline(prompt), _) ->
          Ok(prompt)
        _ -> Error(Nil)
      }
    })
  let assert [draft_prompt, repair_prompt, review_prompt, incorporate_prompt] =
    prompts

  list.each(
    [draft_prompt, repair_prompt, review_prompt, incorporate_prompt],
    fn(prompt) { assert_not_contains(prompt, ".pi/skills") },
  )
}

pub fn workflow_execplan_command_steps_resolve_helper_from_repo_root_test() {
  let dir = "test/tmp/execplan-command-consumer"
  let workspace_dir = "test/tmp/execplan-command-workspace"
  reset_dir(dir)
  reset_dir(workspace_dir)
  write_driver_script(dir)
  let calls_path = write_execplan_helper(dir)
  let config_path = write_execplan_consumer_bundle(dir)
  let repo_root = absolute_path(dir)
  let config_dir = absolute_path(dir <> "/.scherzo")
  let workspace_dir = absolute_path(workspace_dir)
  let expected_calls = [
    "validate",
    "create-pr --publish-context tmp/scherzo-execplan-publish-context.json",
    "create-implementation-issue --publish-context tmp/scherzo-execplan-publish-context.json",
  ]

  let assert Ok(False) = simplifile.is_directory(dir <> "/.pi")
  let assert Ok(bundle) = runtime_bundle.load_with_env(Some(config_path), env)
  let assert Ok(dag) = dict.get(bundle.workflows, "execplan")

  let explicit_root_calls =
    execplan_command_calls(dag, calls_path, workspace_dir, [
      #("SCHERZO_REPO_ROOT", repo_root),
      #("SCHERZO_CONFIG_DIR", config_dir),
    ])
  assert explicit_root_calls == expected_calls

  let fallback_root_calls =
    execplan_command_calls(dag, calls_path, workspace_dir, [
      #("SCHERZO_REPO_ROOT", ""),
      #("SCHERZO_CONFIG_DIR", config_dir),
    ])
  assert fallback_root_calls == expected_calls
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

  assert_contains(implementation_workflow, "lane_correctness")
  assert_contains(implementation_workflow, "lane_test_quality")
  assert_contains(implementation_workflow, "lane_idioms_maintainability")
  assert_contains(implementation_workflow, "lane_security_performance")
  assert_contains(implementation_workflow, "submit_structured_output")
  assert_contains(implementation_workflow, "prepare-native")
  assert_contains(implementation_workflow, "synthesize_review")
  assert_contains(implementation_workflow, "scripts/scherzo-review")

  assert_contains(execplan_implementation_workflow, "lane_correctness")
  assert_contains(execplan_implementation_workflow, "lane_test_quality")
  assert_contains(
    execplan_implementation_workflow,
    "lane_idioms_maintainability",
  )
  assert_contains(execplan_implementation_workflow, "lane_security_performance")
  assert_contains(execplan_implementation_workflow, "submit_structured_output")
  assert_contains(execplan_implementation_workflow, "prepare-native")
  assert_contains(execplan_implementation_workflow, "synthesize_review")
  assert_contains(execplan_implementation_workflow, "scripts/scherzo-review")
  assert_not_contains(execplan_implementation_workflow, "run-lane --lane")

  list.each([implementation_prompt, execplan_prompt], fn(prompt) {
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "\"$repo_root/scripts/scherzo-review\"")
    assert_contains(prompt, "Do not invoke local pi slash commands")
    assert_not_contains(prompt, "`/review")
    assert_not_contains(prompt, "`scripts/scherzo-review")
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
    ".scherzo/workflows/prompts/execplan-repair-validation.md",
    ".scherzo/workflows/prompts/execplan-review.md",
    ".scherzo/workflows/prompts/execplan-incorporate-review.md",
    ".scherzo/workflows/prompts/execplan-revision.md",
    ".scherzo/workflows/prompts/execplan-implementation-implement.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-verify-completion-after-feedback.md",
    ".scherzo/workflows/prompts/execplan-implementation-review.md",
    ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
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

pub fn execplan_authoring_prompts_use_markdown_source_of_truth_test() {
  let draft_prompt = read_file(".scherzo/workflows/prompts/execplan-draft.md")
  let repair_prompt =
    read_file(".scherzo/workflows/prompts/execplan-repair-validation.md")
  let review_prompt = read_file(".scherzo/workflows/prompts/execplan-review.md")
  let incorporate_prompt =
    read_file(".scherzo/workflows/prompts/execplan-incorporate-review.md")

  assert_contains(draft_prompt, "checked-in Markdown ExecPlan proposal")
  assert_contains(
    draft_prompt,
    "docs/plans/{{ issue.identifier }}-<short-kebab-title>.md",
  )
  assert_contains(
    draft_prompt,
    "Do not create a tracked `docs/plans/*.html` file",
  )
  assert_contains(draft_prompt, "HTML is a derived viewer artifact only")
  assert_contains(
    draft_prompt,
    "\"$repo_root/scripts/scherzo-execplan\" validate docs/plans/{{ issue.identifier }}-<short-kebab-title>.md",
  )
  assert_not_contains(
    draft_prompt,
    "final tracked artifact must be the `.html` file",
  )
  assert_not_contains(draft_prompt, "render tmp/execplan-source.md")

  assert_contains(repair_prompt, "single changed Markdown plan artifact")
  assert_contains(
    repair_prompt,
    "Revise only the `docs/plans/*.md` plan artifact",
  )
  assert_contains(
    repair_prompt,
    "The only tracked plan artifact must remain `docs/plans/*.md`",
  )

  assert_contains(review_prompt, "single changed Markdown plan artifact")
  assert_contains(review_prompt, "Markdown plan content")
  assert_not_contains(review_prompt, "PLAN_HTML_PATH")

  assert_contains(incorporate_prompt, "Markdown plan artifact")
  assert_contains(
    incorporate_prompt,
    "Revise only the `docs/plans/*.md` plan artifact",
  )
}

pub fn execplan_prompts_use_bounded_plan_context_and_safe_html_edits_test() {
  let implementation_prompt =
    read_file(".scherzo/workflows/prompts/execplan-implementation-implement.md")
  let verifier_prompt =
    read_file(
      ".scherzo/workflows/prompts/execplan-implementation-verify-completion.md",
    )
  let revision_prompt =
    read_file(".scherzo/workflows/prompts/execplan-revision.md")
  let apply_feedback_prompt =
    read_file(
      ".scherzo/workflows/prompts/execplan-implementation-apply-feedback.md",
    )
  let apply_plan_completion_prompt =
    read_file(
      ".scherzo/workflows/prompts/execplan-implementation-apply-plan-completion-feedback.md",
    )

  assert_contains(
    implementation_prompt,
    "New ExecPlans are checked in as Markdown under `docs/plans/*.md`",
  )
  assert_contains(implementation_prompt, "PLAN_BRIEF_STATUS")
  assert_contains(implementation_prompt, "PLAN_BRIEF_PATH")
  assert_contains(implementation_prompt, "PLAN_INDEX_PATH")
  assert_contains(
    implementation_prompt,
    "the generated brief plus named `section` reads are the required first pass",
  )
  assert_contains(
    implementation_prompt,
    "\"$repo_root/scripts/scherzo-execplan-html\" section",
  )
  assert_contains(
    implementation_prompt,
    "\"$repo_root/scripts/scherzo-implementation\" plan-brief --check",
  )
  assert_contains(
    implementation_prompt,
    "\"$repo_root/scripts/scherzo-implementation\" plan-brief --refresh-if-stale",
  )
  assert_contains(implementation_prompt, "full plan remains authoritative")
  assert_contains(
    implementation_prompt,
    "For Markdown plans, edit `PLAN_PATH` directly",
  )
  assert_contains(implementation_prompt, "legacy HTML plans")
  assert_contains(implementation_prompt, "extract-md")
  assert_contains(implementation_prompt, "render tmp/current-execplan.md")

  assert_contains(verifier_prompt, "PLAN_BRIEF_STATUS")
  assert_contains(verifier_prompt, "PLAN_BRIEF_PATH")
  assert_contains(verifier_prompt, "PLAN_INDEX_PATH")
  assert_contains(
    verifier_prompt,
    "\"$repo_root/scripts/scherzo-implementation\" plan-brief --check",
  )
  assert_contains(
    verifier_prompt,
    "\"$repo_root/scripts/scherzo-implementation\" plan-brief --refresh-if-stale",
  )
  assert_contains(
    verifier_prompt,
    "\"$repo_root/scripts/scherzo-execplan-html\" section",
  )
  assert_contains(verifier_prompt, "full plan remains authoritative fallback")

  assert_contains(
    revision_prompt,
    "New plans are Markdown source files under `docs/plans/*.md`",
  )
  assert_contains(
    revision_prompt,
    "If it is Markdown, edit `PLAN_PATH` directly",
  )
  assert_contains(revision_prompt, "extract-md")
  assert_contains(revision_prompt, "tmp/execplan-revision-source.md")
  assert_contains(revision_prompt, "render tmp/execplan-revision-source.md")
  assert_contains(revision_prompt, "Direct HTML edits are only a fallback")

  assert_contains(
    apply_feedback_prompt,
    "For Markdown plan living-document edits, edit `PLAN_PATH` directly",
  )
  assert_contains(
    apply_feedback_prompt,
    "legacy HTML plan living-document edits",
  )
  assert_contains(
    apply_plan_completion_prompt,
    "For Markdown plan living-document edits, edit `PLAN_PATH` directly",
  )
  assert_contains(
    apply_plan_completion_prompt,
    "legacy HTML plan living-document edits",
  )
}

pub fn workflow_docs_explain_packaged_guidance_and_validation_test() {
  let docs = read_file(".scherzo/README.md")

  assert_contains(docs, "## Workflow-packaged guidance and portability")
  assert_contains(docs, "embed the required ExecPlan authoring")
  assert_contains(docs, "without committing local ExecPlan skill files")
  assert_contains(docs, ".scherzo/workflows/schemas/")
  assert_contains(docs, "without copying a separate schema directory")
  assert_contains(docs, "repo_root=${SCHERZO_REPO_ROOT:-$(cd")
  assert_contains(docs, "\"$repo_root/scripts/scherzo-execplan\" validate")
  assert_contains(docs, "workflow portability validation")
  assert_contains(docs, "doctor --check workflow-config")
  assert_contains(docs, "scripts/scherzo-review")
  assert_contains(docs, "checked-in Markdown ExecPlan source file")
  assert_contains(docs, "PLAN_MARKDOWN_PATH")
  assert_contains(docs, "HTML previews are derived viewer artifacts")
  assert_contains(docs, "legacy `.html` still accepted")
  assert_not_contains(docs, "PLAN_HTML_PATH")
  assert_contains(docs, "workspace.profiles.dogfood-jj")
  assert_contains(docs, "workspace_profile: dogfood-jj")
  assert_contains(docs, "do not add language-specific review skills")
  assert_contains(docs, "LIV-115")
}
