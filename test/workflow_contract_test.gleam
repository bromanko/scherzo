import gleam/dict
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import scherzo/workflow_checkpoint
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_run/contract_io
import support/test_helpers
import yay

fn root(source: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(source)
  yay.document_root(document)
}

fn parse_contract(source: String) -> workflow_contract.Contract {
  let assert Ok(Some(contract)) = workflow_contract.parse(root(source))
  contract
}

fn error_code(source: String) -> String {
  let assert Error(workflow_contract.ContractError(code, _)) =
    workflow_contract.parse(root(source))
  code
}

fn minimal_contract(body: String) -> String {
  "contract:\n  version: 1\n" <> body
}

fn static_ref_descriptor_dag() -> workflow_dag.WorkflowDag {
  let assert Ok(dag) =
    workflow_dag.parse(
      "version: 1\nid: static-ref-output\ncontract:\n  version: 1\n  outputs:\n    review_link:\n      kind: ref\n      ref_type: url\n      artifact_type: scherzo.review_link.v1\n      required: true\n      source:\n        type: url\n        value: https://example.invalid/review\n    branch_ref:\n      kind: ref\n      ref_type: git_ref\n      artifact_type: scherzo.branch_ref.v1\n      required: true\n      source:\n        type: git_ref\n        value: feature/liv-869\nsteps:\n  - id: noop\n    kind: command\n    run: echo ok\n",
    )
  dag
}

pub fn parses_contract_type_strings_test() {
  assert workflow_contract.type_from_string("text")
    == Ok(workflow_contract.Text)
  assert workflow_contract.type_from_string("artifact[]")
    == Ok(workflow_contract.ArtifactList)
  assert workflow_contract.type_from_string("document.markdown")
    == Ok(workflow_contract.DocumentMarkdown)
  assert workflow_contract.type_from_string("exec_plan")
    == Ok(workflow_contract.ExecPlan)
  assert workflow_contract.type_from_string("exec_plan_bundle")
    == Ok(workflow_contract.ExecPlanBundle)
  assert workflow_contract.type_from_string("implementation_pack")
    == Ok(workflow_contract.ImplementationPack)
  assert workflow_contract.type_from_string("code_change_bundle")
    == Ok(workflow_contract.CodeChangeBundle)
  assert workflow_contract.type_from_string("git_ref")
    == Ok(workflow_contract.GitRef)
  assert workflow_contract.type_from_string("url") == Ok(workflow_contract.Url)
  assert workflow_contract.type_from_string("code_change")
    == Ok(workflow_contract.CodeChange)
  let assert Error(workflow_contract.ContractError(code, _)) =
    workflow_contract.type_from_string("markdown_document")
  assert code == "unknown_contract_type"
}

pub fn validates_contract_names_test() {
  assert workflow_contract.valid_contract_name("prompt")
  assert workflow_contract.valid_contract_name("base_ref")
  assert workflow_contract.valid_contract_name("collect-findings")
  assert !workflow_contract.valid_contract_name("")
  assert !workflow_contract.valid_contract_name("with space")
  assert !workflow_contract.valid_contract_name("document.markdown")
  assert !workflow_contract.valid_contract_name("outputs/findings")
}

pub fn parses_valid_research_contract_test() {
  let contract =
    parse_contract(minimal_contract(
      "  inputs:\n    prompt:\n      type: text\n      required: true\n      source: issue_context\n    attachments: artifact[]\n  outputs:\n    findings:\n      type: document.markdown\n      required: true\n      source:\n        step: collect_findings\n        field: stdout\n",
    ))
  assert contract.version == 1
  let assert [prompt, attachments] = contract.inputs
  assert prompt.name == "prompt"
  assert prompt.type_ == workflow_contract.Text
  assert prompt.required
  assert prompt.source == Some(workflow_contract.IssueContext)
  assert attachments.name == "attachments"
  assert attachments.required == False
  let assert [findings] = contract.outputs
  assert findings.name == "findings"
  assert findings.type_ == workflow_contract.DocumentMarkdown
  assert findings.source
    == Some(workflow_contract.StepField(
      "collect_findings",
      workflow_contract.Stdout,
    ))
}

pub fn rejects_invalid_contract_shapes_test() {
  assert error_code(minimal_contract(
      "  inputs:\n    prompt:\n      type: markdown_document\n      required: false\n",
    ))
    == "unknown_contract_type"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      required: false\n      primary: true\n",
    ))
    == "contract_primary_not_supported"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      required: true\n",
    ))
    == "contract_required_output_missing_source"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      required: false\n      unknown: true\n",
    ))
    == "unknown_contract_entry_key"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      required: false\n    findings:\n      type: text\n      required: false\n",
    ))
    == "duplicate_contract_output"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      media_type: application/xml\n      required: false\n",
    ))
    == "unsupported_contract_descriptor_media_type"
  assert error_code(minimal_contract(
      "  outputs:\n    plan:\n      kind: file\n      media_type: application/xml\n      artifact_type: scherzo.exec_plan.v1\n      required: true\n      source:\n        step: draft\n        path: tmp/plan.md\n",
    ))
    == "unknown_contract_descriptor_type"
  assert error_code(minimal_contract(
      "  outputs:\n    plan:\n      type: exec_plan\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.exec_plan_bundle.v2\n      required: true\n      source:\n        step: draft\n        path: tmp/plan.md\n",
    ))
    == "contract_descriptor_type_mismatch"
}

pub fn parses_input_and_context_sources_test() {
  let contract =
    parse_contract(minimal_contract(
      "  inputs:\n    prompt:\n      type: text\n      source: issue_context\n    scheduled:\n      type: text\n      source: scheduled_context\n    plan:\n      type: exec_plan\n      source: mapped_output\n    literal_prompt:\n      type: text\n      source:\n        type: literal\n        value: hi\n  context:\n    base_ref:\n      type: git_ref\n      source: workspace_driver_base\n    mapped_base:\n      type: git_ref\n      required: true\n      source: mapped_output\n    literal_base:\n      type: git_ref\n      required: true\n      source:\n        type: literal\n        value: feature/liv-292\n",
    ))
  let assert [prompt, scheduled, plan, literal_prompt] = contract.inputs
  assert prompt.source == Some(workflow_contract.IssueContext)
  assert scheduled.source == Some(workflow_contract.ScheduledContext)
  assert plan.source == Some(workflow_contract.MappedOutputSource)
  assert literal_prompt.source == Some(workflow_contract.LiteralInput("hi"))
  let assert [base_ref, mapped_base, literal_base] = contract.context
  assert base_ref.source == Some(workflow_contract.WorkspaceDriverBase)
  assert mapped_base.source == Some(workflow_contract.MappedOutputContext)
  assert literal_base.source
    == Some(workflow_contract.LiteralContext("feature/liv-292"))
}

pub fn rejects_invalid_input_context_sources_test() {
  assert error_code(minimal_contract(
      "  inputs:\n    prompt:\n      type: text\n      source:\n        type: literal\n",
    ))
    == "invalid_contract_input_source"
  assert error_code(minimal_contract(
      "  context:\n    base_ref:\n      type: git_ref\n      required: true\n      source: issue_context\n",
    ))
    == "invalid_contract_context_source"
}

pub fn parses_descriptor_contract_entries_test() {
  let contract =
    parse_contract(minimal_contract(
      "  inputs:\n    exec_plan_bundle:\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.exec_plan_bundle.v2\n      required: true\n      source: mapped_output\n  outputs:\n    plan:\n      kind: file\n      media_type: text/markdown\n      artifact_type: scherzo.exec_plan.v1\n      required: true\n      source:\n        step: materialize_bundle\n        path: tmp/execplan-review-doc.md\n    implementation_pack:\n      kind: file\n      media_type: application/json\n      artifact_type: scherzo.implementation_pack.v2\n      required: true\n      source:\n        step: materialize_pack\n        path: tmp/execplan-implementation-pack.json\n    code_change_bundle:\n      kind: artifact_set\n      media_type: application/json\n      artifact_type: scherzo.code_change_bundle.v2\n      required: true\n      source:\n        step: materialize_code_change_bundle\n        path: tmp/execplan-code-change-bundle.json\n",
    ))

  let assert [exec_plan_bundle] = contract.inputs
  assert exec_plan_bundle.type_ == workflow_contract.ExecPlanBundle
  let assert [plan, implementation_pack, code_change_bundle] = contract.outputs
  assert plan.type_ == workflow_contract.ExecPlan
  let assert Some(plan_descriptor) = plan.descriptor
  assert plan_descriptor.kind == Some("file")
  assert plan_descriptor.media_type == Some("text/markdown")
  assert plan_descriptor.artifact_type == Some("scherzo.exec_plan.v1")
  assert implementation_pack.type_ == workflow_contract.ImplementationPack
  assert code_change_bundle.type_ == workflow_contract.CodeChangeBundle
}

pub fn parses_custom_markdown_file_descriptor_test() {
  let contract =
    parse_contract(minimal_contract(
      "  outputs:\n    findings:\n      kind: file\n      media_type: text/markdown\n      artifact_type: scherzo.research_findings.v1\n      required: true\n      source:\n        step: collect_findings\n        field: stdout\n",
    ))

  let assert [findings] = contract.outputs
  assert findings.type_ == workflow_contract.DocumentMarkdown
  let assert Some(descriptor) = findings.descriptor
  assert descriptor.kind == Some("file")
  assert descriptor.media_type == Some("text/markdown")
  assert descriptor.artifact_type == Some("scherzo.research_findings.v1")
}

pub fn static_ref_outputs_preserve_contract_artifact_types_test() {
  let root = "test/tmp/workflow-contract/static-ref-descriptor"
  test_helpers.reset_dir(root)
  let checkpoint = workflow_checkpoint.ledger_writer(root, fn() { 123 })

  let assert Ok(result) =
    contract_io.record_outputs_if_contracted(
      static_ref_descriptor_dag(),
      "run-1",
      "fp",
      None,
      checkpoint,
      dict.new(),
      dict.new(),
    )

  assert result.missing == []
  let assert Some(manifest) = result.manifest
  let assert [review_link, branch_ref] = manifest.outputs
  let assert Some(review_link_descriptor) =
    workflow_contract_manifest.descriptor_for_named_value(
      "review_link",
      review_link.value,
    )
  assert review_link_descriptor.artifact_type == Some("scherzo.review_link.v1")
  assert review_link_descriptor.ref_type == Some("url")
  assert review_link_descriptor.ref == Some("https://example.invalid/review")

  let assert Some(branch_ref_descriptor) =
    workflow_contract_manifest.descriptor_for_named_value(
      "branch_ref",
      branch_ref.value,
    )
  assert branch_ref_descriptor.artifact_type == Some("scherzo.branch_ref.v1")
  assert branch_ref_descriptor.ref_type == Some("git_ref")
  assert branch_ref_descriptor.ref == Some("feature/liv-869")
}

pub fn parses_output_sources_test() {
  let contract =
    parse_contract(minimal_contract(
      "  outputs:\n    stdout_doc:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\n    final_plan:\n      type: exec_plan\n      source:\n        step: draft_execplan\n        field: final_response\n    structured_change:\n      type: code_change\n      source:\n        step: summarize_change\n        structured_output: code_change\n    inline_change:\n      type: code_change\n      source:\n        step: summarize_change\n        inline_json: code_change\n    bundle:\n      type: exec_plan_bundle\n      source:\n        step: materialize_bundle\n        path: tmp/execplan-bundle.json\n    pack:\n      type: implementation_pack\n      source:\n        step: materialize_pack\n        path: tmp/execplan-implementation-pack.json\n    code_bundle:\n      type: code_change_bundle\n      source:\n        step: materialize_code_change_bundle\n        path: tmp/execplan-code-change-bundle.json\n    pr:\n      type: url\n      source:\n        type: url\n        value: https://example.invalid/pr/1\n    branch:\n      type: git_ref\n      source:\n        type: git_ref\n        value: feature/liv-292\n",
    ))
  let assert [
    stdout_doc,
    final_plan,
    structured_change,
    inline_change,
    bundle,
    pack,
    code_bundle,
    pr,
    branch,
  ] = contract.outputs
  assert stdout_doc.source
    == Some(workflow_contract.StepField(
      "collect_findings",
      workflow_contract.Stdout,
    ))
  assert final_plan.source
    == Some(workflow_contract.StepField(
      "draft_execplan",
      workflow_contract.FinalResponse,
    ))
  assert structured_change.source
    == Some(workflow_contract.StructuredOutput(
      "summarize_change",
      "code_change",
    ))
  assert inline_change.source
    == Some(workflow_contract.InlineJson("summarize_change", "code_change"))
  assert bundle.source
    == Some(workflow_contract.StepFile(
      "materialize_bundle",
      "tmp/execplan-bundle.json",
    ))
  assert pack.source
    == Some(workflow_contract.StepFile(
      "materialize_pack",
      "tmp/execplan-implementation-pack.json",
    ))
  assert code_bundle.source
    == Some(workflow_contract.StepFile(
      "materialize_code_change_bundle",
      "tmp/execplan-code-change-bundle.json",
    ))
  assert pr.source
    == Some(workflow_contract.StaticUrl("https://example.invalid/pr/1"))
  assert branch.source
    == Some(workflow_contract.StaticGitRef("feature/liv-292"))
}

pub fn rejects_invalid_output_sources_test() {
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      source: url\n",
    ))
    == "invalid_contract_output_source"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stderr\n",
    ))
    == "invalid_contract_output_field"
  assert error_code(minimal_contract(
      "  outputs:\n    findings:\n      type: document.markdown\n      source:\n        step: collect_findings\n        field: stdout\n        structured_output: findings\n",
    ))
    == "invalid_contract_output_source"
  assert error_code(minimal_contract(
      "  outputs:\n    pack:\n      type: implementation_pack\n      source:\n        step: materialize_pack\n        path: ../implementation_pack.json\n",
    ))
    == "invalid_contract_output_path"
  assert error_code(minimal_contract(
      "  outputs:\n    pr:\n      type: url\n      source:\n        type: url\n        value: not-a-url\n",
    ))
    == "invalid_contract_output_url"
  assert error_code(minimal_contract(
      "  outputs:\n    branch:\n      type: git_ref\n      source:\n        type: git_ref\n        value: "
      <> "\"bad\tref\""
      <> "\n",
    ))
    == "invalid_contract_output_git_ref"
}

pub fn canonical_source_json_is_stable_test() {
  let json_text =
    workflow_contract.input_source_to_canonical_json(
      workflow_contract.IssueContext,
    )
    |> json.to_string
  assert string.contains(json_text, "issue_context")
}

pub fn compatibility_requires_explicit_mode_test() {
  assert workflow_contract.compatible(
    workflow_contract.ExecPlan,
    workflow_contract.ExecPlan,
    workflow_contract.DirectMapping,
  )
  assert !workflow_contract.compatible(
    workflow_contract.DocumentMarkdown,
    workflow_contract.Text,
    workflow_contract.DirectMapping,
  )
  assert workflow_contract.compatible(
    workflow_contract.DocumentMarkdown,
    workflow_contract.ArtifactList,
    workflow_contract.AppendMapping,
  )
}
