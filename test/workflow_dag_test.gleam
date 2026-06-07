import gleam/option.{None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/structured_output_source
import scherzo/workflow_contract
import scherzo/workflow_dag

fn parse_ok(source: String) -> workflow_dag.WorkflowDag {
  let assert Ok(dag) = workflow_dag.parse(source)
  dag
}

fn error_code(source: String) -> String {
  let assert Error(workflow_dag.DagError(code, _)) = workflow_dag.parse(source)
  code
}

fn error_message(source: String) -> String {
  let assert Error(workflow_dag.DagError(_, message)) =
    workflow_dag.parse(source)
  message
}

fn minimal() -> String {
  "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n"
}

pub fn parses_minimal_workflow_dag_test() {
  let dag = parse_ok(minimal())
  assert dag.id == "research"
  assert dag.workspace_profile == None
  assert dag.workspace_capabilities == []
  assert dag.max_parallel_steps == 1
  assert dag.recover == None
  assert dag.contract == None
  assert dag.publication_routes == []
  let assert [step] = dag.steps
  assert step.id == "main"
  assert step.depends_on == []
  assert step.workspace == workflow_dag.WorkspaceRef(name: "main", from: None)
  assert step.on_failure == workflow_dag.FailWorkflow
  assert step.model_settings == model_config.default_settings()
  assert step.recover == None
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/research.md"),
    None,
  ) = step.kind
}

pub fn parses_artifact_publications_test() {
  let dag =
    parse_ok(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      required: false\n      pull_request:\n        title: \"Publish {{ publication.id }}\"\n        body_template: docs/pr-body.md\n      files:\n        - select:\n            output: exec_plan_bundle\n            entry: plan\n          path: docs/plans/{{ work.identifier }}.md\n",
    )

  let assert [route] = dag.publication_routes
  assert route.id == "review_doc"
  assert route.repository == "github.docs"
  assert route.required == False
  assert route.pull_request
    == Some(artifact_publication_config.PublicationPullRequestOverride(
      title: Some("Publish {{ publication.id }}"),
      body_template: Some("docs/pr-body.md"),
    ))
  let assert [artifact_publication_config.PublicationFileRoute(selector, path)] =
    route.files
  assert selector
    == artifact_publication_config.PublicationFileSelector(
      output: "exec_plan_bundle",
      entry: Some("plan"),
    )
  assert path == "docs/plans/{{ work.identifier }}.md"
}

pub fn parses_commit_stack_existing_pr_branch_publication_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\n    merge_conflict_target:\n      type: code_change\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: conflict_resolution\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_conflict_target\n",
    )

  let assert [route] = dag.publication_routes
  assert route.mode == artifact_publication_config.CommitStackPublication
  let assert Some(commit_stack) = route.commit_stack
  let artifact_publication_config.PublicationCommitStackRoute(selector:) =
    commit_stack
  assert selector
    == artifact_publication_config.PublicationCommitStackSelector(
      output: "commit_stack",
    )
  let assert artifact_publication_config.ExistingPrBranchTarget(source) =
    route.target
  assert source
    == artifact_publication_config.PublicationTargetSource(
      output: "merge_conflict_target",
    )
}

pub fn rejects_commit_stack_publication_files_selector_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\n    merge_conflict_target:\n      type: code_change\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: conflict_resolution\n      repository: github.code\n      required: true\n      mode: commit_stack\n      files:\n        - select:\n            output: commit_stack\n          path: tmp/commit-stack.json\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_conflict_target\n",
    )
    == "commit_stack_publication_files_unsupported"
}

pub fn rejects_commit_stack_publication_pull_request_override_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\n    merge_conflict_target:\n      type: code_change\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: conflict_resolution\n      repository: github.code\n      required: true\n      mode: commit_stack\n      pull_request:\n        title: Should not be used\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_conflict_target\n",
    )
    == "commit_stack_pull_request_unsupported"
}

pub fn rejects_commit_stack_existing_pr_branch_non_code_change_target_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\n    merge_conflict_target:\n      type: text\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: conflict_resolution\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: existing_pr_branch\n        source:\n          output: merge_conflict_target\n",
    )
    == "publication_target_output_type_mismatch"
}

pub fn rejects_commit_stack_publication_without_existing_target_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: conflict_resolution\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n",
    )
    == "missing_commit_stack_publication_target"
}

pub fn parses_commit_stack_stable_branch_publication_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/implementation.md\nartifacts:\n  publications:\n    - id: implementation_commit_stack\n      repository: github.code\n      required: true\n      mode: commit_stack\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: stable_branch\n",
    )

  let assert [route] = dag.publication_routes
  assert route.mode == artifact_publication_config.CommitStackPublication
  let assert artifact_publication_config.StableBranchTarget = route.target
}

pub fn rejects_invalid_publication_route_shapes_test() {
  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review-2.md\n",
    )
    == "duplicate_publication_id"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: ReviewDoc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "invalid_publication_id"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "missing_publication_repository"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: [github.docs]\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "publication_repository_not_string"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "invalid_publication_repository_ref"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs.extra\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "invalid_publication_repository_ref"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files: []\n",
    )
    == "publication_files_empty"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select: {}\n          path: docs/review.md\n",
    )
    == "missing_output"

  let unsupported_selector_message =
    error_message(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n            field: stdout\n          path: docs/review.md\n",
    )
  assert string.contains(
    unsupported_selector_message,
    "artifacts.publications[].files[].select contains unsupported key: field",
  )
}

pub fn rejects_invalid_publication_templates_and_paths_test() {
  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/{{ unknown.value }}.md\n",
    )
    == "unknown_publication_template_variable"

  let unsafe_path_message =
    error_message(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: ../docs/review.md\n",
    )
  assert string.contains(
    unsafe_path_message,
    "artifacts.publications[].files[].path must not contain ..",
  )

  let unsafe_body_template_message =
    error_message(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      pull_request:\n        body_template: ../docs/pr-body.md\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
  assert string.contains(
    unsafe_body_template_message,
    "artifacts.publications[].pull_request.body_template must not contain ..",
  )

  let control_tag_path_message =
    error_message(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: \"docs/{% if work.id %}..{% endif %}/secrets.md\"\n",
    )
  assert string.contains(
    control_tag_path_message,
    "control tags are not supported",
  )
}

pub fn rejects_invalid_publication_pull_request_titles_test() {
  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      pull_request:\n        title: 123\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "publication_pull_request_title_not_string"

  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      pull_request:\n        title: \"Review {{ unknown.value }}\"\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "unknown_publication_template_variable"
}

pub fn rejects_publication_with_duplicate_config_keys_test() {
  let message =
    error_message(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      repository: github.other\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )

  assert string.contains(
    message,
    "artifacts.publications[] contains duplicate key: repository",
  )
}

pub fn rejects_publication_routes_without_contract_test() {
  assert error_code(
      "version: 1\nid: execplan\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: exec_plan_bundle\n          path: docs/review.md\n",
    )
    == "missing_publication_contract"
}

pub fn rejects_publication_with_unknown_contract_output_test() {
  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    exec_plan_bundle:\n      type: exec_plan_bundle\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: missing_output\n          path: docs/plans/{{ work.identifier }}.md\n",
    )
    == "unknown_publication_output"
}

pub fn rejects_publication_entry_on_non_aggregate_output_test() {
  assert error_code(
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    plan:\n      type: exec_plan\n      source:\n        step: main\n        field: final_response\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\nartifacts:\n  publications:\n    - id: review_doc\n      repository: github.docs\n      files:\n        - select:\n            output: plan\n            entry: body\n          path: docs/plans/{{ work.identifier }}.md\n",
    )
    == "publication_selector_entry_not_supported"
}

pub fn infers_step_kind_from_prompt_or_run_test() {
  let dag =
    parse_ok(
      "version: 1\nid: infer\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n  - id: validate\n    depends_on: [draft]\n    run: gleam test\n",
    )
  let assert [draft, validate] = dag.steps
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/draft.md"),
    None,
  ) = draft.kind
  let assert workflow_dag.CommandStep("gleam test", None) = validate.kind
}

pub fn rejects_ambiguous_inferred_step_kind_test() {
  assert error_code(
      "version: 1\nid: infer\nsteps:\n  - id: both\n    prompt: prompts/draft.md\n    run: gleam test\n",
    )
    == "ambiguous_step_kind"
}

pub fn rejects_malformed_step_discriminator_fields_test() {
  assert error_code(
      "version: 1\nid: infer\nsteps:\n  - id: kind\n    kind: 123\n    prompt: prompts/draft.md\n",
    )
    == "step_kind_not_string"
  assert error_code(
      "version: 1\nid: infer\nsteps:\n  - id: prompt\n    prompt: 123\n    run: gleam test\n",
    )
    == "prompt_not_string"
  assert error_code(
      "version: 1\nid: infer\nsteps:\n  - id: run\n    prompt: prompts/draft.md\n    run: [gleam, test]\n",
    )
    == "run_not_string"
}

pub fn parses_workflow_and_step_recover_configs_test() {
  let dag =
    parse_ok(
      "version: 1\nid: recovery\nrecovery:\n  attempts: 2\n  model: gpt-5\n  prompt: prompts/recover.md\nsteps:\n  - id: test\n    kind: command\n    run: gleam test\n  - id: fix\n    kind: agent\n    prompt: prompts/fix.md\n    depends_on: [test]\n    recovery:\n      attempts: 1\n      prompt: prompts/step-recover.md\n",
    )
  let assert Some(workflow_dag.RecoveryConfigPatch(
    enabled,
    attempts,
    model,
    prompt,
  )) = dag.recover
  assert enabled == None
  assert attempts == Some(2)
  assert model == Some("gpt-5")
  assert prompt == Some(workflow_dag.PromptFile("prompts/recover.md"))

  let assert [test_step, fix_step] = dag.steps
  let assert Ok(Some(test_recover)) =
    workflow_dag.effective_recovery_config(dag, test_step)
  assert test_recover
    == workflow_dag.EffectiveRecoveryConfig(
      attempts: 2,
      model: Some("gpt-5"),
      prompt: workflow_dag.PromptFile("prompts/recover.md"),
    )
  let assert Ok(Some(fix_recover)) =
    workflow_dag.effective_recovery_config(dag, fix_step)
  assert fix_recover
    == workflow_dag.EffectiveRecoveryConfig(
      attempts: 1,
      model: Some("gpt-5"),
      prompt: workflow_dag.PromptFile("prompts/step-recover.md"),
    )
}

pub fn parses_step_only_recover_and_default_attempts_test() {
  let dag =
    parse_ok(
      "version: 1\nid: recovery\nsteps:\n  - id: fix\n    kind: agent\n    prompt: prompts/fix.md\n    recovery:\n      prompt: prompts/recover.md\n",
    )
  let assert [step] = dag.steps
  let assert Ok(Some(recover)) =
    workflow_dag.effective_recovery_config(dag, step)
  assert recover
    == workflow_dag.EffectiveRecoveryConfig(
      attempts: 1,
      model: None,
      prompt: workflow_dag.PromptFile("prompts/recover.md"),
    )
}

pub fn recover_enabled_false_disables_step_recovery_test() {
  let dag =
    parse_ok(
      "version: 1\nid: recovery\nrecovery:\n  prompt: prompts/recover.md\nsteps:\n  - id: test\n    kind: command\n    run: gleam test\n    recovery:\n      enabled: false\n",
    )
  let assert [step] = dag.steps
  assert workflow_dag.effective_recovery_config(dag, step) == Ok(None)
}

pub fn rejects_invalid_recover_configs_test() {
  assert error_code(
      "version: 1\nid: recovery\nrecovery: true\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "recovery_not_map"
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  enabled: nope\n  prompt: prompts/recover.md\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "recovery_enabled_not_bool"
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  attempts: once\n  prompt: prompts/recover.md\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "recovery_attempts_not_int"
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  attempts: 0\n  prompt: prompts/recover.md\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "invalid_recovery_attempts"
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  model: bad model\n  prompt: prompts/recover.md\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "invalid_model"
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  prompt: 123\nsteps:\n  - id: main\n    kind: command\n    run: true\n",
    )
    == "recovery_prompt_not_string"
}

pub fn rejects_missing_effective_recover_prompt_test() {
  assert error_code(
      "version: 1\nid: recovery\nrecovery:\n  attempts: 2\nsteps:\n  - id: main\n    kind: command\n    run: 'true'\n",
    )
    == "missing_recovery_prompt"
}

pub fn parses_agent_structured_output_defaults_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/review.md"),
    Some(spec),
  ) = step.kind
  assert spec.format == workflow_dag.StructuredJson
  assert spec.artifact_name == "review_json"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review_json",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema == workflow_dag.StructuredObjectSchema([])
  assert spec.validators == []
  assert spec.validation_retries == 1
}

pub fn parses_agent_structured_output_json_contract_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      format: json\n      artifact_name: review_result\n      required: true\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_result\n      validator: review_lane_draft\n      validation_retries: 0\n      schema:\n        type: object\n        required:\n          - summary\n          - findings\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(
    workflow_dag.PromptFile("prompts/review.md"),
    Some(spec),
  ) = step.kind
  assert spec.format == workflow_dag.StructuredJson
  assert spec.artifact_name == "review_result"
  assert spec.required == True
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_review_result",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
  assert spec.schema
    == workflow_dag.StructuredObjectSchema(["summary", "findings"])
  assert spec.validators
    == [
      workflow_dag.CommandValidator(
        name: "review_lane_draft_compat",
        argv: [
          "python3",
          ".scherzo/workflows/scripts/scherzo-review",
          "validate-structured-output",
          "--validator",
          "review_lane_draft",
        ],
        timeout_ms: 30_000,
        working_directory: workflow_dag.ValidatorInRepository,
        env: [],
      ),
    ]
  assert spec.validation_retries == 0
}

pub fn parses_agent_structured_output_pi_tool_call_source_test() {
  let dag =
    parse_ok(
      "version: 1\nid: structured_review\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: true\n        reject_sibling_tool_calls: true\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  assert spec.source
    == structured_output_source.PiToolCallSource(
      tool_name: "submit_example_artifact",
      require_single: True,
      reject_sibling_tool_calls: True,
      parameters_schema_path: None,
    )
}

pub fn rejects_invalid_structured_output_source_contracts_test() {
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output: {}\n",
    )
    == "missing_structured_output_source"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: unknown\n",
    )
    == "unsupported_structured_output_source_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: final_response\n",
    )
    == "unsupported_structured_output_source_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n",
    )
    == "missing_structured_output_source_tool_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: bad tool\n",
    )
    == "invalid_structured_output_source_tool_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        require_single: false\n",
    )
    == "unsupported_structured_output_source_require_single"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_example_artifact\n        reject_sibling_tool_calls: false\n",
    )
    == "unsupported_structured_output_source_reject_sibling_tool_calls"
}

pub fn rejects_invalid_structured_output_contracts_test() {
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: command\n    run: echo ok\n    structured_output: {}\n",
    )
    == "structured_output_on_command_step"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      format: yaml\n",
    )
    == "unsupported_structured_output_format"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      artifact_name: bad-name\n",
    )
    == "invalid_structured_artifact_name"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        type: array\n",
    )
    == "unsupported_structured_output_schema_type"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        required: summary\n",
    )
    == "structured_output_schema_required_not_list"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      schema:\n        required:\n          - 123\n",
    )
    == "structured_output_schema_required_entry_not_string"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validator: unknown_contract\n",
    )
    == "unknown_structured_output_validator"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validator: 123\n",
    )
    == "structured_output_validator_not_string"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validation_retries: 2\n",
    )
    == "invalid_structured_output_validation_retries"
  assert error_code(
      "version: 1\nid: structured_review\nsteps:\n  - id: review_json\n    kind: agent\n    prompt: prompts/review.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_json\n      validation_retries: once\n",
    )
    == "structured_output_validation_retries_not_int"
}

pub fn parses_workspace_requires_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nworkspace:\n  requires: [assert-only, changed-files]\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  assert dag.workspace_capabilities
    == [
      config_types.WorkspaceAssertOnly,
      config_types.WorkspaceChangedFiles,
    ]
}

pub fn rejects_invalid_workspace_requires_test() {
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  requires: assert-only\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_requires_not_list"
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  requires: [123]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_requires_entry_not_string"
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  requires: [pull-request]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "unknown_workspace_capability"
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  requires: [assert-only, assert-only]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "duplicate_workspace_capability"
}

pub fn parses_top_level_workspace_driver_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nworkspace:\n  driver: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n",
    )
  assert dag.workspace_profile == Some("noop")
}

pub fn rejects_invalid_workspace_driver_test() {
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  driver: 123\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "workspace_driver_not_string"
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  driver: ../noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_workspace_driver"
  assert error_code(
      "version: 1\nid: research\nworkspace:\n  driver: Noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_workspace_driver"
}

pub fn rejects_unknown_workflow_workspace_keys_test() {
  let profile_source =
    "version: 1\nid: research\nworkspace:\n  profile: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(profile_source) == "unknown_workflow_workspace_key"
  assert string.contains(error_message(profile_source), "workspace.profile")
  assert string.contains(error_message(profile_source), "workspace.driver")

  let require_source =
    "version: 1\nid: research\nworkspace:\n  require: [assert-only]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(require_source) == "unknown_workflow_workspace_key"
  assert string.contains(error_message(require_source), "workspace.require")
  assert string.contains(error_message(require_source), "workspace.requires")
}

pub fn rejects_removed_workspace_keys_test() {
  let profile_source =
    "version: 1\nid: research\nworkspace_profile: noop\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(profile_source) == "removed_workspace_profile"
  let profile_message = error_message(profile_source)
  assert string.contains(profile_message, "workspace_profile")
  assert string.contains(profile_message, "workspace.driver")
  assert string.contains(profile_message, "SCHERZO_YAML_SIMPLIFIED_V1")

  let fail_fast_source = "version: 1\nmax_parallel_steps: 2\n"
  assert error_code(fail_fast_source) == "removed_max_parallel_steps"
  let fail_fast_message = error_message(fail_fast_source)
  assert string.contains(fail_fast_message, "max_parallel_steps")
  assert string.contains(fail_fast_message, "concurrency")
  assert string.contains(fail_fast_message, "SCHERZO_YAML_SIMPLIFIED_V1")

  let capabilities_source =
    "version: 1\nid: research\nworkspace_capabilities: [assert-only]\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(capabilities_source) == "removed_workspace_capabilities"
  assert string.contains(
    error_message(capabilities_source),
    "workspace.requires",
  )

  let concurrency_source =
    "version: 1\nid: research\nmax_parallel_steps: 2\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(concurrency_source) == "removed_max_parallel_steps"
  assert string.contains(error_message(concurrency_source), "concurrency")

  let recover_source =
    "version: 1\nid: research\nrecover:\n  prompt: prompts/recover.md\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n"
  assert error_code(recover_source) == "removed_recover"
  assert string.contains(error_message(recover_source), "recovery")
}

pub fn rejects_removed_step_workspace_keys_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace_profile: noop\n    prompt: a.md\n",
    )
    == "removed_workspace_profile"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace_capabilities: [assert-only]\n    prompt: a.md\n",
    )
    == "removed_workspace_capabilities"

  let run_in_source =
    "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    workspace: main\n    prompt: a.md\n"
  assert error_code(run_in_source) == "removed_workspace"
  assert string.contains(error_message(run_in_source), "run_in")

  let timeout_source =
    "version: 1\nid: research\nsteps:\n  - timeout_ms: 120000\n"
  assert error_code(timeout_source) == "removed_timeout_ms"
  let timeout_message = error_message(timeout_source)
  assert string.contains(timeout_message, "timeout_ms")
  assert string.contains(timeout_message, "timeout")
  assert string.contains(timeout_message, "SCHERZO_YAML_SIMPLIFIED_V1")
}

pub fn parses_command_step_duration_timeout_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    timeout: 2m\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.CommandStep(_, Some(timeout_ms)) = step.kind
  assert timeout_ms == 120_000
}

pub fn rejects_invalid_command_step_duration_timeout_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    timeout: 120000\n",
    )
    == "command_timeout_not_duration"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    timeout: 2d\n",
    )
    == "invalid_command_timeout"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    timeout: 0ms\n",
    )
    == "invalid_command_timeout"
}

pub fn rejects_removed_command_step_timeout_ms_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n    timeout_ms: 120000\n",
    )
    == "removed_timeout_ms"
}

pub fn parses_optional_description_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\ndescription: Test description\nsteps:\n  - id: main\n    kind: command\n    run: echo ok\n",
    )
  assert dag.description == Some("Test description")
}

pub fn parses_per_step_model_settings_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: prompts/research.md\n    model: github-copilot/gpt-5.1-codex\n    thinking: xhigh\n",
    )
  let assert [step] = dag.steps
  assert step.model_settings.model == Some("github-copilot/gpt-5.1-codex")
  assert step.model_settings.thinking == Some(model_config.ThinkingXHigh)
}

pub fn rejects_invalid_per_step_model_settings_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    model: \"sonnet:high\"\n",
    )
    == "invalid_model"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    thinking: extreme\n",
    )
    == "invalid_thinking"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n    provider: openai\n    model: gpt-5\n",
    )
    == "unsupported_provider_field"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: test\n    kind: command\n    run: gleam test\n    thinking: high\n",
    )
    == "model_settings_on_command_step"
}

pub fn rejects_duplicate_step_ids_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n  - id: main\n    kind: command\n    run: gleam test\n",
    )
  assert code == "duplicate_step_id"
}

pub fn rejects_missing_dependencies_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: review\n    kind: agent\n    depends_on: [implement]\n    prompt: r.md\n",
    )
  assert code == "missing_dependency"
}

pub fn rejects_cycles_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: a\n    kind: command\n    depends_on: [b]\n    run: one\n  - id: b\n    kind: command\n    depends_on: [a]\n    run: two\n",
    )
  assert code == "cycle"
}

pub fn rejects_multiple_terminal_steps_test() {
  let code =
    error_code(
      "version: 1\nid: research\nsteps:\n  - id: a\n    kind: command\n    run: one\n  - id: b\n    kind: command\n    run: two\n",
    )
  assert code == "multiple_terminal_steps"
}

pub fn terminal_step_uses_dependency_sink_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: final\n    kind: command\n    depends_on: [implement]\n    run: done\n  - id: implement\n    kind: agent\n    prompt: implement.md\n",
    )
  let assert Some(step) = workflow_dag.terminal_step(dag)
  assert step.id == "final"
}

pub fn accepts_string_workspace_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: command\n    run_in: main\n    run: gleam test\n",
    )
  let assert [step] = dag.steps
  assert step.workspace == workflow_dag.WorkspaceRef(name: "main", from: None)
}

pub fn accepts_derived_workspace_from_transitive_dependency_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement.md\n    run_in: main\n  - id: code_review\n    kind: agent\n    depends_on: [implement]\n    prompt: review.md\n    run_in:\n      name: code-review\n      from: main\n",
    )
  let assert [_, review] = dag.steps
  assert review.workspace
    == workflow_dag.WorkspaceRef(name: "code-review", from: Some("main"))
}

pub fn rejects_derived_workspace_without_transitive_source_test() {
  let code =
    error_code(
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: implement.md\n    run_in: main\n  - id: code_review\n    kind: agent\n    prompt: review.md\n    run_in:\n      name: code-review\n      from: main\n",
    )
  assert code == "invalid_workspace_from"
}

pub fn rejects_invalid_identifiers_test() {
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: bad-step\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_step_id"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    run_in: \"\"\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    run_in: ../main\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
  assert error_code(
      "version: 1\nid: research\nsteps:\n  - id: main\n    kind: agent\n    run_in: dir/main\n    prompt: a.md\n",
    )
    == "invalid_workspace_name"
}

pub fn rejects_zero_parallelism_test() {
  assert error_code(
      "version: 1\nid: research\nconcurrency: 0\nsteps:\n  - id: main\n    kind: agent\n    prompt: a.md\n",
    )
    == "invalid_concurrency"
}

pub fn defaults_depends_on_and_on_failure_test() {
  let dag = parse_ok(minimal())
  let assert [step] = dag.steps
  assert step.depends_on == []
  assert step.on_failure == workflow_dag.FailWorkflow
}

pub fn parses_on_failure_continue_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\nsteps:\n  - id: test_step\n    kind: command\n    run: gleam test\n    on_failure: continue\n",
    )
  let assert [step] = dag.steps
  assert step.on_failure == workflow_dag.ContinueWorkflow
}

pub fn rejects_generic_pi_tool_call_without_matching_json_schema_validator_test() {
  let missing =
    "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n"
  assert error_code(missing)
    == "structured_output_parameters_schema_missing_json_schema_validator"

  let mismatched =
    "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n"
  assert error_code(mismatched)
    == "structured_output_parameters_schema_path_mismatch"
}

pub fn accepts_generic_pi_tool_call_with_matching_json_schema_validator_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\nsteps:\n  - id: example_json\n    kind: agent\n    prompt: prompts/example.md\n    structured_output:\n      artifact_name: example_artifact\n      source:\n        type: pi_tool_call\n        tool_name: submit_structured_output\n        parameters_schema_path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      validators:\n        - name: shape\n          type: json_schema\n          path: .scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json\n      schema:\n        required: [schema_version, artifact_type]\n",
    )
  let assert [step] = dag.steps
  let assert workflow_dag.AgentStep(_, Some(spec)) = step.kind
  assert structured_output_source.parameters_schema_path(spec.source)
    == Some(
      ".scherzo/workflows/schemas/review-lane-draft.correctness.v1.schema.json",
    )
}

pub fn validates_contract_output_step_sources_test() {
  let dag =
    parse_ok(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: printf '# Findings'\n",
    )
  let assert Some(contract) = dag.contract
  let assert [output] = contract.outputs
  assert output.source
    == Some(workflow_contract.StepField(
      "collect_findings",
      workflow_contract.Stdout,
    ))
}

pub fn rejects_contract_output_unknown_step_test() {
  assert error_code(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: missing\n        field: stdout\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo ok\n",
    )
    == "contract_output_unknown_step"
}

pub fn rejects_contract_final_response_on_command_test() {
  assert error_code(
      "version: 1\nid: research\ncontract:\n  version: 1\n  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: final_response\nsteps:\n  - id: collect_findings\n    kind: command\n    run: echo ok\n",
    )
    == "contract_output_field_invalid_for_step"
}

pub fn validates_contract_structured_output_sources_test() {
  let dag =
    parse_ok(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change:\n      type: code_change\n      source:\n        step: summarize_change\n        structured_output: code_change\n    inline_change:\n      type: code_change\n      source:\n        step: summarize_change\n        inline_json: code_change\nsteps:\n  - id: summarize_change\n    kind: agent\n    prompt: prompts/summarize.md\n    structured_output:\n      artifact_name: code_change\n      source:\n        type: pi_tool_call\n        tool_name: submit_code_change\n",
    )
  let assert Some(contract) = dag.contract
  let assert [structured, inline] = contract.outputs
  assert structured.source
    == Some(workflow_contract.StructuredOutput(
      "summarize_change",
      "code_change",
    ))
  assert inline.source
    == Some(workflow_contract.InlineJson("summarize_change", "code_change"))
}

pub fn rejects_contract_structured_output_missing_artifact_test() {
  assert error_code(
      "version: 1\nid: implementation\ncontract:\n  version: 1\n  outputs:\n    code_change:\n      type: code_change\n      source:\n        step: summarize_change\n        structured_output: code_change\nsteps:\n  - id: summarize_change\n    kind: agent\n    prompt: prompts/summarize.md\n    structured_output:\n      artifact_name: other_change\n      source:\n        type: pi_tool_call\n        tool_name: submit_code_change\n",
    )
    == "contract_output_structured_artifact_missing"
}
