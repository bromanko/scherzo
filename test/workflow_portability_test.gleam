import gleam/list
import gleam/option.{Some}
import gleam/string
import scherzo/command_step
import scherzo/config/types as config_types
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

fn assert_step_contains(
  path: String,
  step_id: String,
  contents: String,
  expected: String,
) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message =
        "expected text not found in "
        <> path
        <> " step "
        <> step_id
        <> ": "
        <> expected
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

fn limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 4000,
    template_field_max_chars: 4000,
    workflow_summary_max_chars: 4000,
  )
}

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn run_command(command: String) -> step_artifact.StepArtifact {
  command_step.run("workflow-portability", command, ".", 120_000, [], limits())
}

fn run_command_with_env(
  command: String,
  env: List(#(String, String)),
) -> step_artifact.StepArtifact {
  command_step.run_with_env(
    "workflow-portability",
    command,
    ".",
    120_000,
    env,
    [],
    limits(),
  )
}

fn workflow_runner_probe_command() -> String {
  "python3 -c '"
  <> "import json,sys; "
  <> "from importlib.machinery import SourceFileLoader; "
  <> "sys.path.insert(0,\".scherzo/workflows/scripts\"); "
  <> "mod=SourceFileLoader("
  <> "\"scherzo_review_script\",\".scherzo/workflows/scripts/scherzo-review\").load_module(); "
  <> "print(json.dumps(mod.workflow_runner_command()))"
  <> "'"
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

fn workflow_paths_with_packaged_helper_commands() -> List(String) {
  [
    ".scherzo/workflows/execplan.yaml",
    ".scherzo/workflows/execplan-revision.yaml",
    ".scherzo/workflows/execplan-implementation.yaml",
    ".scherzo/workflows/implementation.yaml",
    ".scherzo/workflows/github-pr-conflict-scout.yaml",
    ".scherzo/workflows/merge-conflict-resolution.yaml",
    ".scherzo/workflows/origin-sync.yaml",
    ".scherzo/workflows/research.yaml",
    ".scherzo/workflows/workspace-cleanup.yaml",
  ]
}

fn assert_bundle_dir_initialized_when_referenced(
  path: String,
  step: workflow_dag.WorkflowStep,
) -> Nil {
  case step.kind {
    workflow_dag.CommandStep(run, _) -> {
      case string.contains(run, "$bundle_dir") {
        True -> {
          assert_step_contains(
            path,
            step.id,
            run,
            "bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}",
          )
          assert_step_contains(
            path,
            step.id,
            run,
            "if [ -z \"$bundle_dir\" ]; then",
          )
          assert_step_contains(
            path,
            step.id,
            run,
            "bundle_dir=\"$(cd \"$SCHERZO_CONFIG_DIR/workflows\" && pwd -P)\"",
          )
        }
        False -> Nil
      }
    }
    _ -> Nil
  }
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
    ".scherzo/workflows/scripts/scherzo-execplan-v2",
  ]
}

pub fn execplan_v2_suffix_workflow_files_are_retired_test() {
  list.each(retired_v2_suffix_paths(), fn(path) {
    let assert Ok(False) = simplifile.is_file(path)
  })
}

pub fn review_runner_uses_packaged_scherzo_cli_test() {
  let script = read_file(".scherzo/workflows/scripts/scherzo-review")

  assert_contains(script, "SCHERZO_WORKFLOW_RUNNER")
  assert_contains(script, "return [\"scherzo\"]")
  assert_contains(script, "\"workflow\",\n        \"run\",")
  assert_not_contains(
    script,
    "\"gleam\",\n        \"run\",\n        \"--\",\n        \"workflow\",\n        \"run\",",
  )
}

pub fn review_runner_command_behavior_test() {
  let command = workflow_runner_probe_command()
  let default =
    run_command_with_env(command, [#("SCHERZO_WORKFLOW_RUNNER", "")])
  assert default.status == step_artifact.StepSucceeded
  assert default.exit_code == Some(0)
  assert_contains(default.stdout, "[\"scherzo\"]")

  let override =
    run_command_with_env(command, [
      #("SCHERZO_WORKFLOW_RUNNER", "custom --flag 'two words'"),
    ])
  assert override.status == step_artifact.StepSucceeded
  assert override.exit_code == Some(0)
  assert_contains(override.stdout, "[\"custom\", \"--flag\", \"two words\"]")

  let invalid =
    run_command_with_env(command, [
      #("SCHERZO_WORKFLOW_RUNNER", "'unterminated"),
    ])
  assert invalid.status == step_artifact.StepFailed
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
      assert_contains(workflow, "bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}")
      assert_contains(workflow, "\"$bundle_dir/scripts/scherzo-execplan\"")
      assert_not_contains(
        workflow,
        "run: .scherzo/workflows/scripts/scherzo-execplan",
      )
    },
  )
}

pub fn workflow_command_bundle_dir_references_are_initialized_test() {
  list.each(workflow_paths_with_packaged_helper_commands(), fn(path) {
    let assert Ok(source) = simplifile.read(path)
    let assert Ok(dag) = workflow_dag.parse(source)
    list.each(dag.steps, fn(step) {
      assert_bundle_dir_initialized_when_referenced(path, step)
    })
  })
}

pub fn execplan_html_fallback_commands_use_bundle_helper_test() {
  let script = read_file(".scherzo/workflows/scripts/scherzo-execplan-html")

  assert_contains(script, ".scherzo/workflows/scripts/scherzo-execplan-html")
  assert_not_contains(script, "return f'scripts/scherzo-execplan-html")
  assert_not_contains(script, "use scripts/scherzo-execplan-html")
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
      assert_contains(workflow, "$bundle_dir/scripts/scherzo-review")
      assert_not_contains(workflow, "run-lane --lane")
    },
  )

  list.each([implementation_prompt, execplan_prompt], fn(prompt) {
    assert_contains(prompt, "REVIEW_FINAL_ARTIFACT_PATH")
    assert_contains(prompt, "\"$bundle_dir/scripts/scherzo-review\"")
    assert_contains(prompt, "Do not invoke local pi slash commands")
    assert_not_contains(prompt, "`/review")
    assert_not_contains(prompt, "`.scherzo/workflows/scripts/scherzo-review")
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
  assert_contains(
    implementation_prompt,
    "implementation_handoff.issue_identifier` may differ from `source_issue.identifier",
  )
  assert_contains(implementation_prompt, "expected for handoff tasks")
  assert_contains(
    implementation_prompt,
    "source-plan provenance beyond that expected split as blocking",
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
  assert_contains(docs, "bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}")
  assert_contains(docs, "\"$bundle_dir/scripts/scherzo-execplan\"")
  assert_contains(docs, "workflow portability validation")
  assert_contains(docs, "doctor --check workflow-config")
  assert_contains(docs, ".scherzo/workflows/scripts/scherzo-review")
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
    assert_not_contains(docs, ".scherzo/workflows/scripts/scherzo-execplan-v2")
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

pub fn workflow_portability_harness_writes_report_test() {
  let dir = "test/tmp/workflow-portability-harness"
  reset_dir(dir)
  let fake_scherzo = dir <> "/fake-scherzo.sh"
  let output_dir = dir <> "/out"
  let assert Ok(cwd) = simplifile.current_directory()
  let call_log = cwd <> "/" <> dir <> "/fake-scherzo-call.txt"
  let assert Ok(Nil) =
    simplifile.write(
      fake_scherzo,
      "#!/bin/sh\n"
        <> "printf '%s' \"$*\" > '"
        <> call_log
        <> "'\n"
        <> "echo 'workflow-config: OK'\n",
    )
  let chmod = run_command("chmod +x " <> fake_scherzo)
  assert chmod.status == step_artifact.StepSucceeded
  assert chmod.exit_code == Some(0)

  let artifact =
    run_command(
      "python3 scripts/scherzo-workflow-portability check --repo-root . --scherzo "
      <> fake_scherzo
      <> " --output-dir "
      <> output_dir,
    )

  assert artifact.status == step_artifact.StepSucceeded
  assert artifact.exit_code == Some(0)
  assert string.contains(artifact.stdout, "WORKFLOW_PORTABILITY=ok")
  assert string.contains(artifact.stdout, "WORKFLOW_PORTABILITY_WORKFLOWS=9")

  let report = read_file(output_dir <> "/workflow-portability-report.v1.json")
  let invocation = read_file(call_log)
  let staged_config = read_file(output_dir <> "/stage/.scherzo/scherzo.yaml")
  assert_contains(report, "\"artifact_type\": \"workflow_portability_report\"")
  assert_contains(report, "\"packaged_cli\": true")
  assert_contains(report, "\"remote_mutations\": \"none\"")
  assert_contains(report, "\"mode\": \"load-only\"")
  assert_contains(report, ".scherzo/workflows/execplan.yaml")
  assert_contains(report, ".scherzo/workflows/execplan-revision.yaml")
  assert_contains(report, ".scherzo/workflows/execplan-implementation.yaml")
  assert_contains(report, ".scherzo/workflows/implementation.yaml")
  assert_contains(report, ".scherzo/workflows/research.yaml")
  assert_contains(report, ".scherzo/workflows/github-pr-conflict-scout.yaml")
  assert_contains(report, ".scherzo/workflows/merge-conflict-resolution.yaml")
  assert_contains(report, ".scherzo/workflows/origin-sync.yaml")
  assert_contains(report, ".scherzo/workflows/workspace-cleanup.yaml")
  assert_contains(report, "exercise helper scherzo-review")
  assert_contains(
    report,
    "add fake structured-output and agent artifact fixtures",
  )
  assert_contains(invocation, "doctor --check workflow-config")
  assert_contains(staged_config, "command: scherzo-workspace-jj")
  assert_contains(staged_config, "command: scherzo-workspace-noop")
  assert_contains(staged_config, "command: 'exec pi'")
  assert_contains(staged_config, "argv:")
  assert_contains(staged_config, "exec pi \"$@\"")
  assert_contains(staged_config, "argv_env:")
  assert_contains(staged_config, "SCHERZO_PI_SESSION_PERSISTENCE")
  assert_contains(staged_config, "session_persistence:")
  assert_contains(staged_config, "enabled: true")
  assert_not_contains(staged_config, "$repo_root/scripts/scherzo-pi")
  let _ = simplifile.delete(dir)
}

pub fn workflow_portability_gate_is_wired_into_flake_test() {
  let flake = read_file("flake.nix")
  let nix_file = read_file("nix/workflow-portability.nix")
  let docs = read_file(".scherzo/README.md")

  assert_contains(
    flake,
    "workflow-portability = (workflowPortabilityFor system).check",
  )
  assert_contains(
    flake,
    "workflow-portability = (workflowPortabilityFor system).devShell",
  )
  assert_contains(nix_file, "scripts/scherzo-workflow-portability")
  assert_contains(nix_file, "workflow-portability-report.v1.json")
  assert_contains(nix_file, "workflow portability debug shell")
  assert_contains(
    docs,
    "nix build .#checks.$(nix eval --raw --impure --expr builtins.currentSystem).workflow-portability",
  )
  assert_contains(docs, "nix develop .#workflow-portability")
  assert_contains(
    docs,
    "scripts/scherzo-workflow-portability check --repo-root .",
  )
  assert_contains(docs, "do not restore `gleam run -- workflow run`")
}
