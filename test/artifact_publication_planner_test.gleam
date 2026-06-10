import gleam/bit_array
import gleam/dict
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_planner_decode
import scherzo/commit_stack_artifact
import scherzo/hash
import scherzo/json_value
import scherzo/state/artifact_store
import scherzo/workflow_artifact_descriptor as artifact_descriptor
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest

pub fn plans_leaf_output_publication_manifest_test() {
  let store =
    store_with_contents([
      #(plan_ref(), plan_contents()),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  assert manifest.dry_run == True
  assert manifest.series_id
    == "work/task-1/workflow/workflow.execplan/publication/review_doc"
  assert manifest.repository_id == "github.docs"
  assert manifest.github_repo == Some("scherzo-systems/scherzo")
  assert manifest.branch == "scherzo/workflow.execplan/LIV-761/review_doc"
  let assert [planned] = manifest.files
  assert planned.destination_path == "docs/plans/LIV-761.md"
  assert planned.source.ref == plan_ref()
  assert planned.source.sha256 == plan_sha()
  assert manifest.pull_request.enabled == True
  assert manifest.pull_request.title == Some("LIV-761 publication")
  let assert Some(body) = manifest.pull_request.body
  assert string.contains(body, "docs/plans/LIV-761.md")
  assert string.contains(body, manifest.version_id)
  assert string.contains(body, "Plan publication metadata")
  assert string.contains(body, "https://linear.example/LIV-761")

  let json = artifact_publication_planner.manifest_to_string(manifest)
  assert string.contains(json, "\"dry_run\":true")
  assert string.contains(json, "\"destination_path\":\"docs/plans/LIV-761.md\"")
  assert !string.contains(json, "\"pr_url\"")
  assert !string.contains(json, "\"commit_sha\"")
  assert !string.contains(json, "\"push_result\"")
  assert !string.contains(json, "\"mutation_status\"")
}

pub fn non_linear_publication_templates_render_unavailable_source_metadata_test() {
  let store =
    store_with_contents([
      #(plan_ref(), plan_contents()),
    ])
  let template =
    "Title {% if work.title %}{{ work.title }}{% else %}Unavailable{% endif %}\nURL {% if work.url %}{{ work.url }}{% else %}Unavailable{% endif %}"
  let route =
    leaf_route_with_title(
      "{{ work.identifier }}: implement {% if work.title %}{{ work.title }}{% else %}implementation changes{% endif %}",
    )

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      route,
      store,
      scheduled_work(),
      "run-1",
      dict.from_list([#("templates/publication.md", template)]),
    )

  assert manifest.pull_request.title
    == Some("nightly-job: implement implementation changes")
  let assert Some(body) = manifest.pull_request.body
  assert string.contains(body, "Title Unavailable")
  assert string.contains(body, "URL Unavailable")
}

pub fn plans_artifact_set_entry_publication_test() {
  let bundle_descriptor = execplan_bundle_descriptor(plan_sha(), plan_bytes())
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let assert [planned] = manifest.files
  assert planned.destination_path == "docs/review/LIV-761.md"
  assert planned.source.output == "exec_plan_bundle"
  assert planned.source.entry == Some("plan")
  assert planned.source.ref == plan_ref()
}

pub fn plans_generic_artifact_set_entry_publication_test() {
  let screenshot_contents = "fake png bytes"
  let bundle_descriptor =
    visual_bundle_descriptor(
      hash.sha256_hex(screenshot_contents),
      bytes_of(screenshot_contents),
    )
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(visual_bundle_ref(), bundle_contents),
      #(visual_screenshot_ref(), screenshot_contents),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      visual_bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      leaf_route_with_selector("visual_artifacts", Some("screenshot")),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let assert [planned] = manifest.files
  assert planned.destination_path == "docs/plans/LIV-761.png"
  assert planned.source.output == "visual_artifacts"
  assert planned.source.entry == Some("screenshot")
  assert planned.source.ref == visual_screenshot_ref()
}

pub fn plans_artifact_set_entry_can_render_destination_from_metadata_test() {
  let bundle_descriptor =
    execplan_bundle_descriptor_with_destination(
      plan_sha(),
      plan_bytes(),
      "docs/review/custom/LIV-761-plan.md",
    )
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let assert [planned] = manifest.files
  assert planned.destination_path == "docs/review/custom/LIV-761-plan.md"
  let json = artifact_publication_planner.manifest_to_string(manifest)
  assert string.contains(
    json,
    "\"destination_path\":\"docs/review/custom/LIV-761-plan.md\"",
  )
}

pub fn plans_materialized_execplan_bundle_entry_publication_test() {
  let bundle_contents =
    materialized_execplan_bundle_contents(
      plan_sha(),
      plan_bytes(),
      "docs/review/materialized/LIV-761-plan.md",
    )
  assert !string.contains(bundle_contents, "\"name\":\"exec_plan_bundle\"")
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let assert [planned] = manifest.files
  assert planned.destination_path == "docs/review/materialized/LIV-761-plan.md"
  assert planned.source.output == "exec_plan_bundle"
  assert planned.source.entry == Some("plan")
  assert planned.source.ref == plan_ref()
}

pub fn metadata_bearing_manifest_round_trips_through_decoder_test() {
  let bundle_descriptor =
    execplan_bundle_descriptor_with_destination(
      plan_sha(),
      plan_bytes(),
      "docs/review/custom/LIV-761-plan.md",
    )
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let json = artifact_publication_planner.manifest_to_string(manifest)
  let assert Ok(decoded) =
    artifact_publication_planner_decode.decode_manifest_json(json)
  let assert [decoded_file] = decoded.files
  assert decoded_file.destination_path == "docs/review/custom/LIV-761-plan.md"
  assert metadata_destination_path(decoded_file.source.metadata)
    == Some("docs/review/custom/LIV-761-plan.md")
}

pub fn stable_branch_template_can_share_execplan_pr_series_across_revision_workflows_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let repositories =
    repositories_with_branch(
      "scherzo/{{ work.identifier }}/{{ publication.id }}",
    )
  let assert Ok(authored) =
    artifact_publication_planner.plan_publication(
      leaf_manifest_with_workflow_id(
        "workflow.execplan",
        "run-1",
        plan_ref(),
        plan_sha(),
        plan_bytes(),
      ),
      repositories,
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  let assert Ok(revised) =
    artifact_publication_planner.plan_publication(
      leaf_manifest_with_workflow_id(
        "workflow.execplan-revision",
        "run-2",
        plan_ref(),
        plan_sha(),
        plan_bytes(),
      ),
      repositories,
      leaf_route(),
      store,
      work(),
      "run-2",
      body_templates,
    )

  assert authored.branch == "scherzo/LIV-761/review_doc"
  assert revised.branch == authored.branch
}

pub fn identical_inputs_keep_same_version_id_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )

  assert first.version_id == second.version_id
}

pub fn unchanged_bytes_and_mapping_keep_version_id_across_run_refs_test() {
  let run_1_ref = plan_ref_for_run("run-1")
  let run_2_ref = plan_ref_for_run("run-2")
  let store =
    store_with_contents([
      #(run_1_ref, plan_contents()),
      #(run_2_ref, plan_contents()),
    ])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest_with_ref("run-1", run_1_ref, plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest_with_ref("run-2", run_2_ref, plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-2",
      body_templates,
    )

  assert first.version_id == second.version_id
}

pub fn changed_body_template_contents_change_version_id_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([
        #("templates/publication.md", body_template() <> "\nExtra detail"),
      ]),
    )

  assert first.version_id != second.version_id
}

pub fn changed_bytes_change_version_id_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let changed_store =
    store_with_contents([#(plan_ref(), plan_contents() <> " updated")])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  let changed_contents = plan_contents() <> " updated"
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(
        hash.sha256_hex(changed_contents),
        bytes_of(changed_contents),
      ),
      repositories(),
      leaf_route(),
      changed_store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  assert first.version_id != second.version_id
}

pub fn commit_stack_existing_pr_branch_plans_retained_target_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert manifest.branch == existing_branch()
  assert manifest.files == []
  assert manifest.pull_request.enabled == False
  let assert Some(_) = manifest.commit_stack
  let assert artifact_publication_planner.ExistingPrBranchTargetPlan(target) =
    manifest.target
  assert target.pr_number == 42
  assert target.pr_url == "https://example.test/pr/42"
}

pub fn commit_stack_existing_pr_branch_plans_retained_value_target_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest_with_target(
        stack_contents,
        retained_value_target_output(target_contents),
      ),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert manifest.branch == existing_branch()
  let assert artifact_publication_planner.ExistingPrBranchTargetPlan(target) =
    manifest.target
  assert target.pr_number == 42
  assert target.pr_url == "https://example.test/pr/42"
}

pub fn commit_stack_existing_branch_plans_retained_target_without_pr_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents = existing_branch_target_contents_without_pr()
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert manifest.branch == existing_branch()
  let assert artifact_publication_planner.ExistingPrBranchTargetPlan(target) =
    manifest.target
  assert target.pr_number == 0
  assert target.pr_url == ""
}

pub fn commit_stack_manifest_round_trips_through_decoder_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  let json = artifact_publication_planner.manifest_to_string(manifest)
  let assert Ok(decoded) =
    artifact_publication_planner_decode.decode_manifest_json(json)
  assert decoded.branch == existing_branch()
  assert decoded.files == []
  let assert Some(decoded_stack) = decoded.commit_stack
  assert decoded_stack.output == "commit_stack"
  assert decoded_stack.manifest_ref == commit_stack_ref()
  assert decoded_stack.stack.head_sha == commit_stack_head_sha()
  assert decoded_stack.stack.carrier.ref == commit_stack_carrier_ref()
  assert decoded.work.title == Some("Plan publication metadata")
  assert decoded.work.url == Some("https://linear.example/LIV-761")
  let assert artifact_publication_planner.ExistingPrBranchTargetPlan(target) =
    decoded.target
  assert target.pr_number == 42
  assert target.pr_url == "https://example.test/pr/42"
}

pub fn legacy_manifest_without_work_decodes_with_fallback_work_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  let json = artifact_publication_planner.manifest_to_string(manifest)
  let legacy_json = manifest_json_without_work(json)
  let assert Ok(decoded) =
    artifact_publication_planner_decode.decode_manifest_json(legacy_json)

  assert decoded.publication_id == manifest.publication_id
  assert decoded.work.kind == artifact_publication_planner.ScheduledWork
  assert decoded.work.id == ""
  assert decoded.work.identifier == ""
  assert decoded.work.slug == ""
  assert decoded.work.title == None
  assert decoded.work.url == None
}

pub fn commit_stack_stable_branch_target_plans_driver_publication_test() {
  let stack_contents =
    commit_stack_manifest_contents_with_base_ref(
      "scherzo-systems/scherzo",
      "main",
    )
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store = store_with_contents([#(commit_stack_ref(), stack_contents)])

  let assert Ok(manifest) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_stable_branch_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  assert manifest.branch
    == "scherzo/workflow.implementation/LIV-761/conflict_resolution"
  assert manifest.files == []
  assert manifest.pull_request.enabled == True
  assert manifest.pull_request.title == Some("LIV-761 publication")
  let assert Some(body) = manifest.pull_request.body
  assert string.contains(body, manifest.version_id)
  assert string.contains(body, "Plan publication metadata")
  assert string.contains(body, "https://linear.example/LIV-761")
  let assert artifact_publication_planner.StableBranchTargetPlan =
    manifest.target
  let assert Some(_) = manifest.commit_stack
}

pub fn commit_stack_artifact_rejects_malformed_payloads_test() {
  let assert Error(json_error) =
    commit_stack_artifact.parse_commit_stack("{not-json")
  assert commit_stack_artifact.error_code(json_error)
    == "commit_stack_json_invalid"

  let assert Error(type_error) =
    commit_stack_artifact.parse_commit_stack(
      commit_stack_manifest_contents_with(
        "scherzo-systems/scherzo",
        "scherzo.not_a_commit_stack.v1",
        existing_branch(),
        expected_existing_head_sha(),
        commit_stack_head_sha(),
        commit_stack_head_tree(),
        commit_stack_artifact.bundle_media_type,
      ),
    )
  assert commit_stack_artifact.error_code(type_error)
    == "commit_stack_artifact_type_mismatch"

  let assert Error(media_error) =
    commit_stack_artifact.parse_commit_stack(
      commit_stack_manifest_contents_with(
        "scherzo-systems/scherzo",
        commit_stack_artifact.commit_stack_artifact_type,
        existing_branch(),
        expected_existing_head_sha(),
        commit_stack_head_sha(),
        commit_stack_head_tree(),
        "application/octet-stream",
      ),
    )
  assert commit_stack_artifact.error_code(media_error)
    == "commit_stack_carrier_media_type_mismatch"
}

pub fn commit_stack_artifact_rejects_non_oid_revisions_test() {
  let assert Error(stack_error) =
    commit_stack_artifact.parse_commit_stack(
      commit_stack_manifest_contents_with(
        "scherzo-systems/scherzo",
        commit_stack_artifact.commit_stack_artifact_type,
        existing_branch(),
        "refs/heads/main",
        commit_stack_head_sha(),
        commit_stack_head_tree(),
        commit_stack_artifact.bundle_media_type,
      ),
    )
  assert commit_stack_artifact.error_code(stack_error)
    == "artifact_git_oid_invalid"

  let assert Error(target_error) =
    commit_stack_artifact.parse_existing_pr_branch_target(
      existing_target_contents_with(
        "scherzo-systems/scherzo",
        "HEAD",
        expected_base_sha(),
      ),
    )
  assert commit_stack_artifact.error_code(target_error)
    == "artifact_git_oid_invalid"
}

pub fn commit_stack_existing_pr_branch_rejects_cross_repo_head_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents = existing_target_contents("fork/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert artifact_publication_planner.code(error)
    == "existing_target_head_repository_mismatch"
}

pub fn commit_stack_existing_pr_branch_rejects_stack_repo_mismatch_test() {
  let stack_contents = commit_stack_manifest_contents("other/repo")
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert artifact_publication_planner.code(error)
    == "commit_stack_repository_mismatch"
}

pub fn commit_stack_existing_pr_branch_rejects_base_branch_mismatch_test() {
  let stack_contents = commit_stack_manifest_contents("scherzo-systems/scherzo")
  let target_contents =
    existing_target_contents_with_base_branch(
      "scherzo-systems/scherzo",
      expected_existing_head_sha(),
      "develop",
      expected_base_sha(),
    )
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert artifact_publication_planner.code(error)
    == "existing_target_base_branch_mismatch"
}

pub fn commit_stack_existing_pr_branch_rejects_stack_base_sha_mismatch_test() {
  let stack_contents =
    commit_stack_manifest_contents_with(
      "scherzo-systems/scherzo",
      commit_stack_artifact.commit_stack_artifact_type,
      existing_branch(),
      "5555555555555555555555555555555555555555",
      commit_stack_head_sha(),
      commit_stack_head_tree(),
      commit_stack_artifact.bundle_media_type,
    )
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), stack_contents),
      #(existing_target_ref(), target_contents),
    ])

  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      commit_stack_output_manifest(stack_contents, target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert artifact_publication_planner.code(error)
    == "commit_stack_base_mismatch"
}

pub fn commit_stack_selector_rejects_non_commit_stack_descriptor_test() {
  let target_contents = existing_target_contents("scherzo-systems/scherzo")
  let store =
    store_with_contents([
      #(commit_stack_ref(), plan_contents()),
      #(existing_target_ref(), target_contents),
    ])

  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      non_commit_stack_output_manifest(target_contents),
      repositories(),
      commit_stack_existing_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )

  assert artifact_publication_planner.code(error)
    == "non_commit_stack_descriptor"
}

pub fn changed_target_mapping_changes_version_id_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route_with_path("docs/alt/{{ work.identifier }}.md"),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )

  assert first.version_id != second.version_id
}

pub fn unknown_output_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route_with_selector("missing", None),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unknown_output"
}

pub fn absent_output_returns_error_test() {
  let store = store_with_contents([])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      absent_manifest(),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "absent_output"
}

pub fn invalid_selector_entry_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route_with_selector("review_doc", Some("plan")),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "invalid_selector_entry"
}

pub fn non_file_root_selection_returns_error_test() {
  let bundle_descriptor = execplan_bundle_descriptor(plan_sha(), plan_bytes())
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store = store_with_contents([#(bundle_ref(), bundle_contents)])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      leaf_route_with_selector("exec_plan_bundle", None),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "non_file_descriptor"
}

pub fn missing_artifact_set_entry_returns_error_test() {
  let bundle_descriptor = execplan_bundle_descriptor(plan_sha(), plan_bytes())
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      leaf_route_with_selector("exec_plan_bundle", Some("missing")),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error)
    == "missing_artifact_set_entry"
}

pub fn missing_ref_returns_error_test() {
  let store = store_with_contents([])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      missing_ref_manifest(),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "missing_ref"
}

pub fn missing_artifact_bytes_returns_error_test() {
  let store = store_with_contents([])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "missing_artifact_bytes"
}

pub fn hash_mismatch_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(
        "0000000000000000000000000000000000000000000000000000000000000000",
        plan_bytes(),
      ),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "hash_mismatch"
}

pub fn byte_count_mismatch_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes() + 1),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "byte_count_mismatch"
}

pub fn unsafe_rendered_path_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route_with_path("../outside.md"),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_rendered_path"
}

pub fn trailing_control_char_destination_path_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route_with_path("docs/plans/{{ work.identifier }}.md\n"),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_rendered_path"
}

pub fn unsafe_branch_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories_with_branch("../bad-branch"),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_branch"
}

pub fn trailing_control_or_invalid_git_ref_branch_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])

  let assert Error(control_error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories_with_branch("scherzo/topic\n"),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  assert artifact_publication_planner.code(control_error) == "unsafe_branch"

  let assert Error(space_error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories_with_branch("scherzo/topic with space"),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  assert artifact_publication_planner.code(space_error) == "unsafe_branch"

  let assert Error(dotdot_error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories_with_branch("scherzo/foo..bar"),
      leaf_route(),
      store,
      work(),
      "run-1",
      body_templates,
    )
  assert artifact_publication_planner.code(dotdot_error) == "unsafe_branch"
}

pub fn duplicate_destination_paths_return_error_test() {
  let store =
    store_with_contents([
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      manifest_with_two_leaf_outputs(),
      repositories(),
      duplicate_path_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error)
    == "duplicate_destination_path"
}

pub fn missing_body_template_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.new(),
    )
  assert artifact_publication_planner.code(error) == "missing_body_template"
}

pub fn unavailable_template_variable_returns_error_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      leaf_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", "{{ unknown.variable }}")]),
    )
  assert artifact_publication_planner.code(error) == "template_render_failure"
}

pub fn missing_artifact_metadata_variable_returns_error_test() {
  let bundle_descriptor = execplan_bundle_descriptor(plan_sha(), plan_bytes())
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "template_render_failure"
}

pub fn artifact_metadata_absolute_path_returns_error_test() {
  let bundle_descriptor =
    execplan_bundle_descriptor_with_destination(
      plan_sha(),
      plan_bytes(),
      "/tmp/absolute.md",
    )
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_rendered_path"
}

pub fn artifact_metadata_parent_traversal_returns_error_test() {
  let bundle_descriptor =
    execplan_bundle_descriptor_with_destination(
      plan_sha(),
      plan_bytes(),
      "../outside.md",
    )
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_rendered_path"
}

pub fn artifact_metadata_empty_path_returns_error_test() {
  let bundle_descriptor =
    execplan_bundle_descriptor_with_destination(plan_sha(), plan_bytes(), "")
  let bundle_contents = artifact_descriptor.to_string(bundle_descriptor)
  let bundle_sha = hash.sha256_hex(bundle_contents)
  let bundle_bytes = bit_array.byte_size(bit_array.from_string(bundle_contents))
  let store =
    store_with_contents([
      #(bundle_ref(), bundle_contents),
      #(plan_ref(), plan_contents()),
      #(pack_ref(), pack_contents()),
    ])
  let assert Error(error) =
    artifact_publication_planner.plan_publication(
      bundle_manifest(bundle_sha, bundle_bytes),
      repositories(),
      bundle_entry_metadata_route(),
      store,
      work(),
      "run-1",
      dict.from_list([#("templates/publication.md", body_template())]),
    )
  assert artifact_publication_planner.code(error) == "unsafe_rendered_path"
}

fn repositories() -> artifact_publication_config.ArtifactRepositories {
  repositories_with_branch(
    "scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}",
  )
}

fn repositories_with_branch(
  branch_template: String,
) -> artifact_publication_config.ArtifactRepositories {
  artifact_publication_config.ArtifactRepositories(
    github: dict.from_list([
      #(
        "docs",
        artifact_publication_config.GithubRepositoryTarget(
          name: "docs",
          repo: "scherzo-systems/scherzo",
          base: "main",
          checkout: artifact_publication_config.GithubCheckoutConfig(
            strategy: artifact_publication_config.ManagedGit,
          ),
          branch: artifact_publication_config.GithubBranchConfig(
            strategy: artifact_publication_config.StablePerWork,
            template: branch_template,
          ),
          pull_request: artifact_publication_config.GithubPullRequestConfig(
            enabled: True,
            strategy: artifact_publication_config.UpdateExisting,
            draft: True,
            title: Some("{{ work.identifier }} publication"),
            body_template: Some("templates/publication.md"),
          ),
        ),
      ),
    ]),
  )
}

fn work() -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.TaskWork,
    id: "task-1",
    identifier: "LIV-761",
    slug: "LIV-761",
    title: Some("Plan publication metadata"),
    url: Some("https://linear.example/LIV-761"),
  )
}

fn scheduled_work() -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.ScheduledWork,
    id: "schedule-1",
    identifier: "nightly-job",
    slug: "nightly-job",
    title: None,
    url: None,
  )
}

fn leaf_route() -> artifact_publication_config.PublicationRoute {
  leaf_route_with_selector("review_doc", None)
}

fn leaf_route_with_title(
  title: String,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    ..leaf_route(),
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some(title),
        body_template: Some("templates/publication.md"),
      ),
    ),
  )
}

fn commit_stack_existing_route() -> artifact_publication_config.PublicationRoute {
  commit_stack_route_with_target(
    artifact_publication_config.ExistingPrBranchTarget(
      artifact_publication_config.PublicationTargetSource(
        output: "merge_conflict_target",
      ),
    ),
  )
}

fn commit_stack_stable_branch_route() -> artifact_publication_config.PublicationRoute {
  commit_stack_route_with_target(artifact_publication_config.StableBranchTarget)
}

fn commit_stack_route_with_target(
  target: artifact_publication_config.PublicationTarget,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "conflict_resolution",
    repository: "github.docs",
    required: True,
    pull_request: None,
    mode: artifact_publication_config.CommitStackPublication,
    files: [],
    commit_stack: Some(
      artifact_publication_config.PublicationCommitStackRoute(
        selector: artifact_publication_config.PublicationCommitStackSelector(
          output: "commit_stack",
        ),
      ),
    ),
    target: target,
  )
}

fn bundle_entry_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "exec_plan_bundle",
          entry: Some("plan"),
        ),
        path: "docs/review/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
  )
}

fn bundle_entry_metadata_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "exec_plan_bundle",
          entry: Some("plan"),
        ),
        path: "{{ artifact.metadata.publication.destination_path }}",
      ),
    ],
  )
}

fn leaf_route_with_path(
  path: String,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "review_doc",
          entry: None,
        ),
        path: path,
      ),
    ],
  )
}

fn leaf_route_with_selector(
  output: String,
  entry: Option(String),
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: output,
          entry: entry,
        ),
        path: "docs/plans/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
  )
}

fn duplicate_path_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    mode: artifact_publication_config.FilePublication,
    commit_stack: None,
    target: artifact_publication_config.StableBranchTarget,
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "review_doc",
          entry: None,
        ),
        path: "docs/dup.md",
      ),
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "implementation_pack",
          entry: None,
        ),
        path: "docs/dup.md",
      ),
    ],
  )
}

fn leaf_manifest(
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
  leaf_manifest_with_ref("run-1", plan_ref(), sha256, bytes)
}

fn leaf_manifest_with_ref(
  run_id: String,
  ref: String,
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
  leaf_manifest_with_workflow_id(
    "workflow.execplan",
    run_id,
    ref,
    sha256,
    bytes,
  )
}

fn leaf_manifest_with_workflow_id(
  workflow_id: String,
  run_id: String,
  ref: String,
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: run_id,
    workflow_id: workflow_id,
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.DocumentMarkdown,
          workflow_contract_manifest.ArtifactWritten(
            ref: ref,
            sha256: sha256,
            bytes: bytes,
          ),
          "text/markdown",
          Some(source_metadata()),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn absent_manifest() -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.absent(
          workflow_contract.DocumentMarkdown,
          Some("not produced"),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn manifest_with_two_leaf_outputs() -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.DocumentMarkdown,
          workflow_contract_manifest.ArtifactWritten(
            ref: plan_ref(),
            sha256: plan_sha(),
            bytes: plan_bytes(),
          ),
          "text/markdown",
          Some(source_metadata()),
        ),
      ),
      workflow_contract_manifest.NamedManifestValue(
        name: "implementation_pack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.ImplementationPack,
          workflow_contract_manifest.ArtifactWritten(
            ref: pack_ref(),
            sha256: pack_sha(),
            bytes: pack_bytes(),
          ),
          "application/json",
          Some(source_metadata()),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn missing_ref_manifest() -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "review_doc",
        value: workflow_contract_manifest.ManifestValue(
          type_: workflow_contract.DocumentMarkdown,
          status: workflow_contract_manifest.Present,
          ref_kind: Some(workflow_contract_manifest.RunArtifact),
          ref: None,
          sha256: Some(plan_sha()),
          bytes: Some(plan_bytes()),
          media_type: Some("text/markdown"),
          value: None,
          source: Some(source_metadata()),
          diagnostic: None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn commit_stack_output_manifest(
  stack_contents: String,
  target_contents: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  commit_stack_output_manifest_with_target(
    stack_contents,
    target_output(target_contents),
  )
}

fn commit_stack_output_manifest_with_target(
  stack_contents: String,
  target: workflow_contract_manifest.NamedManifestValue,
) -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.implementation",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "commit_stack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.CommitStack,
          workflow_contract_manifest.ArtifactWritten(
            ref: commit_stack_ref(),
            sha256: hash.sha256_hex(stack_contents),
            bytes: bytes_of(stack_contents),
          ),
          "application/vnd.scherzo.git-commit-stack+json",
          None,
        ),
      ),
      target,
    ],
    diagnostics: [],
  )
}

fn non_commit_stack_output_manifest(
  target_contents: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.implementation",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "commit_stack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.Text,
          workflow_contract_manifest.ArtifactWritten(
            ref: commit_stack_ref(),
            sha256: hash.sha256_hex(plan_contents()),
            bytes: bytes_of(plan_contents()),
          ),
          "text/plain",
          None,
        ),
      ),
      target_output(target_contents),
    ],
    diagnostics: [],
  )
}

fn target_output(
  target_contents: String,
) -> workflow_contract_manifest.NamedManifestValue {
  workflow_contract_manifest.NamedManifestValue(
    name: "merge_conflict_target",
    value: workflow_contract_manifest.present_run_artifact(
      workflow_contract.CodeChange,
      workflow_contract_manifest.ArtifactWritten(
        ref: existing_target_ref(),
        sha256: hash.sha256_hex(target_contents),
        bytes: bytes_of(target_contents),
      ),
      "application/json",
      None,
    ),
  )
}

fn retained_value_target_output(
  target_contents: String,
) -> workflow_contract_manifest.NamedManifestValue {
  workflow_contract_manifest.NamedManifestValue(
    name: "merge_conflict_target",
    value: workflow_contract_manifest.present_run_artifact(
      workflow_contract.CodeChange,
      workflow_contract_manifest.ArtifactWritten(
        ref: existing_target_ref(),
        sha256: hash.sha256_hex(target_contents),
        bytes: bytes_of(target_contents),
      ),
      "application/json",
      Some(value_target_source()),
    ),
  )
}

fn value_target_source() -> json_value.JsonValue {
  json_value.JObject([
    #(
      "contract_artifact_type",
      json_value.JString(
        commit_stack_artifact.existing_pr_branch_target_artifact_type,
      ),
    ),
    #(
      "contract_descriptor",
      json_value.JObject([
        #("kind", json_value.JString("value")),
        #("media_type", json_value.JString("application/json")),
        #(
          "artifact_type",
          json_value.JString(
            commit_stack_artifact.existing_pr_branch_target_artifact_type,
          ),
        ),
      ]),
    ),
  ])
}

fn bundle_manifest(
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.execplan",
    workflow_fingerprint: "wf-1",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "exec_plan_bundle",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.ExecPlanBundle,
          workflow_contract_manifest.ArtifactWritten(
            ref: bundle_ref(),
            sha256: sha256,
            bytes: bytes,
          ),
          "application/json",
          Some(source_metadata()),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn visual_bundle_manifest(
  sha256: String,
  bytes: Int,
) -> workflow_contract_manifest.ContractOutputManifest {
  workflow_contract_manifest.ContractOutputManifest(
    run_id: "run-1",
    workflow_id: "workflow.visual-review",
    workflow_fingerprint: "wf-visual",
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "visual_artifacts",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.GenericArtifactSet,
          workflow_contract_manifest.ArtifactWritten(
            ref: visual_bundle_ref(),
            sha256: sha256,
            bytes: bytes,
          ),
          "application/json",
          Some(visual_bundle_source_metadata()),
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn materialized_execplan_bundle_contents(
  plan_sha256: String,
  plan_size: Int,
  destination_path: String,
) -> String {
  let plan_artifact_ref = plan_ref()
  let pack_artifact_ref = pack_ref()
  json.object([
    #("schema_version", json.int(2)),
    #("kind", json.string("artifact_set")),
    #("media_type", json.string("application/json")),
    #("artifact_type", json.string("scherzo.exec_plan_bundle.v2")),
    #("bundle_id", json.string("bundle-liv-761-run-1")),
    #(
      "source_issue",
      json.object([
        #("identifier", json.string("LIV-761")),
        #("title", json.string("Review artifact publication")),
        #(
          "url",
          json.string(
            "https://linear.app/living-systems/issue/LIV-761/review-artifact-publication",
          ),
        ),
      ]),
    ),
    #(
      "workflow",
      json.object([
        #("workflow_id", json.string("workflow.execplan")),
        #("run_id", json.string("run-1")),
        #("workflow_fingerprint", json.string("wf-1")),
      ]),
    ),
    #(
      "revision",
      json.object([
        #("status", json.string("created")),
        #("number", json.int(1)),
        #("supersedes", json.null()),
      ]),
    ),
    #(
      "plan",
      json.object([
        #("ref", json.string(plan_artifact_ref)),
        #("sha256", json.string(plan_sha256)),
        #("bytes", json.int(plan_size)),
        #("media_type", json.string("text/markdown")),
      ]),
    ),
    #(
      "review_doc",
      json.object([
        #("path", json.string(destination_path)),
        #("sha256", json.string(plan_sha256)),
        #("bytes", json.int(plan_size)),
      ]),
    ),
    #(
      "implementation_pack",
      json.object([
        #("ref", json.string(pack_artifact_ref)),
        #("sha256", json.string(pack_sha())),
        #("bytes", json.int(pack_bytes())),
        #(
          "schema",
          json.string(
            ".scherzo/workflows/schemas/implementation-pack.v2.schema.json",
          ),
        ),
        #("derived_from_review_doc_sha256", json.string(plan_sha256)),
      ]),
    ),
    #(
      "entries",
      json.array(
        [
          json.object([
            #("name", json.string("plan")),
            #("kind", json.string("file")),
            #("artifact_type", json.string("scherzo.exec_plan.v1")),
            #("ref", json.string(plan_artifact_ref)),
            #("sha256", json.string(plan_sha256)),
            #("bytes", json.int(plan_size)),
            #("media_type", json.string("text/markdown")),
            #(
              "metadata",
              json.object([
                #(
                  "publication",
                  json.object([
                    #("destination_path", json.string(destination_path)),
                  ]),
                ),
              ]),
            ),
          ]),
          json.object([
            #("name", json.string("implementation_pack")),
            #("kind", json.string("file")),
            #("artifact_type", json.string("scherzo.implementation_pack.v2")),
            #("ref", json.string(pack_artifact_ref)),
            #("sha256", json.string(pack_sha())),
            #("bytes", json.int(pack_bytes())),
            #("media_type", json.string("application/json")),
          ]),
        ],
        of: fn(entry) { entry },
      ),
    ),
    #(
      "review_surface",
      json.object([
        #("status", json.string("not_applicable")),
        #("pr_url", json.null()),
        #("branch", json.null()),
        #("source_bundle_ref", json.null()),
        #("head_revision", json.null()),
        #("review_doc_path", json.string(destination_path)),
      ]),
    ),
    #(
      "implementation_handoff",
      json.object([
        #("issue_identifier", json.string("LIV-762")),
        #(
          "issue_url",
          json.string(
            "https://linear.app/living-systems/issue/LIV-762/implement-review-artifact-publication",
          ),
        ),
        #("workflow_label", json.string("workflow:execplan-implementation")),
        #("bundle_ref", json.string(bundle_ref())),
      ]),
    ),
    #(
      "validation",
      json.array(
        [
          json.object([
            #("name", json.string("materialize-bundle")),
            #("status", json.string("passed")),
          ]),
        ],
        of: fn(entry) { entry },
      ),
    ),
  ])
  |> json.to_string
}

fn execplan_bundle_descriptor(
  plan_sha256: String,
  plan_size: Int,
) -> artifact_descriptor.ArtifactDescriptor {
  execplan_bundle_descriptor_with_optional_destination(
    plan_sha256,
    plan_size,
    None,
  )
}

fn execplan_bundle_descriptor_with_destination(
  plan_sha256: String,
  plan_size: Int,
  destination_path: String,
) -> artifact_descriptor.ArtifactDescriptor {
  execplan_bundle_descriptor_with_optional_destination(
    plan_sha256,
    plan_size,
    Some(destination_path),
  )
}

fn execplan_bundle_descriptor_with_optional_destination(
  plan_sha256: String,
  plan_size: Int,
  destination_path: Option(String),
) -> artifact_descriptor.ArtifactDescriptor {
  let plan_metadata = case destination_path {
    Some(path) ->
      Some(
        json_value.JObject([
          #(
            "publication",
            json_value.JObject([
              #("destination_path", json_value.JString(path)),
            ]),
          ),
        ]),
      )
    None -> None
  }
  artifact_descriptor.ArtifactDescriptor(
    name: "exec_plan_bundle",
    kind: artifact_descriptor.ArtifactSetKind,
    artifact_type: Some("scherzo.exec_plan_bundle.v2"),
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: None,
    sha256: None,
    bytes: None,
    media_type: None,
    value: None,
    entries: [
      artifact_descriptor.ArtifactDescriptor(
        name: "plan",
        kind: artifact_descriptor.FileKind,
        artifact_type: Some("scherzo.exec_plan.v1"),
        description: None,
        source: None,
        validation: None,
        metadata: plan_metadata,
        ref_type: None,
        ref: Some(plan_ref()),
        sha256: Some(plan_sha256),
        bytes: Some(plan_size),
        media_type: Some("text/markdown"),
        value: None,
        entries: [],
      ),
      artifact_descriptor.ArtifactDescriptor(
        name: "implementation_pack",
        kind: artifact_descriptor.FileKind,
        artifact_type: Some("scherzo.implementation_pack.v2"),
        description: None,
        source: None,
        validation: None,
        metadata: None,
        ref_type: None,
        ref: Some(pack_ref()),
        sha256: Some(pack_sha()),
        bytes: Some(pack_bytes()),
        media_type: Some("application/json"),
        value: None,
        entries: [],
      ),
    ],
  )
}

fn visual_bundle_descriptor(
  screenshot_sha256: String,
  screenshot_size: Int,
) -> artifact_descriptor.ArtifactDescriptor {
  artifact_descriptor.ArtifactDescriptor(
    name: "visual_artifacts",
    kind: artifact_descriptor.ArtifactSetKind,
    artifact_type: Some("scherzo_ui.visual_artifact_bundle.v1"),
    description: None,
    source: None,
    validation: None,
    metadata: None,
    ref_type: None,
    ref: None,
    sha256: None,
    bytes: None,
    media_type: Some("application/json"),
    value: None,
    entries: [
      artifact_descriptor.ArtifactDescriptor(
        name: "screenshot",
        kind: artifact_descriptor.FileKind,
        artifact_type: Some("scherzo_ui.screenshot.v1"),
        description: None,
        source: None,
        validation: None,
        metadata: None,
        ref_type: None,
        ref: Some(visual_screenshot_ref()),
        sha256: Some(screenshot_sha256),
        bytes: Some(screenshot_size),
        media_type: Some("image/png"),
        value: None,
        entries: [],
      ),
    ],
  )
}

fn metadata_destination_path(
  metadata: Option(json_value.JsonValue),
) -> Option(String) {
  case metadata {
    Some(json_value.JObject([
      #(
        "publication",
        json_value.JObject([#("destination_path", json_value.JString(path))]),
      ),
    ])) -> Some(path)
    _ -> None
  }
}

fn source_metadata() -> json_value.JsonValue {
  json_value.JObject([#("step_id", json_value.JString("publish_review_doc"))])
}

fn visual_bundle_source_metadata() -> json_value.JsonValue {
  json_value.JObject([
    #("step_id", json_value.JString("capture_visuals")),
    #(
      "contract_artifact_type",
      json_value.JString("scherzo_ui.visual_artifact_bundle.v1"),
    ),
    #(
      "contract_descriptor",
      json_value.JObject([
        #("kind", json_value.JString("artifact_set")),
        #("media_type", json_value.JString("application/json")),
        #(
          "artifact_type",
          json_value.JString("scherzo_ui.visual_artifact_bundle.v1"),
        ),
      ]),
    ),
  ])
}

fn plan_contents() -> String {
  "# Review\n\nThis is the dry-run review doc.\n"
}

fn pack_contents() -> String {
  "{\"artifact_type\":\"implementation_pack\"}"
}

fn commit_stack_manifest_contents(repo: String) -> String {
  commit_stack_manifest_contents_with_base_ref(repo, existing_branch())
}

fn commit_stack_manifest_contents_with_base_ref(
  repo: String,
  base_ref: String,
) -> String {
  commit_stack_manifest_contents_with(
    repo,
    commit_stack_artifact.commit_stack_artifact_type,
    base_ref,
    expected_existing_head_sha(),
    commit_stack_head_sha(),
    commit_stack_head_tree(),
    commit_stack_artifact.bundle_media_type,
  )
}

fn commit_stack_manifest_contents_with(
  repo: String,
  artifact_type: String,
  base_ref: String,
  base_sha: String,
  head_sha: String,
  head_tree: String,
  carrier_media_type: String,
) -> String {
  json.object([
    #("schema_version", json.int(1)),
    #("artifact_type", json.string(artifact_type)),
    #("repository", json.object([#("repo", json.string(repo))])),
    #(
      "base",
      json.object([
        #("ref", json.string(base_ref)),
        #("sha", json.string(base_sha)),
      ]),
    ),
    #(
      "head",
      json.object([
        #("sha", json.string(head_sha)),
        #("tree", json.string(head_tree)),
      ]),
    ),
    #(
      "carrier",
      json.object([
        #("ref", json.string(commit_stack_carrier_ref())),
        #("sha256", json.string(hash.sha256_hex(commit_stack_carrier()))),
        #("bytes", json.int(bytes_of(commit_stack_carrier()))),
        #("media_type", json.string(carrier_media_type)),
      ]),
    ),
  ])
  |> json.to_string
}

fn existing_target_contents(head_repo: String) -> String {
  existing_target_contents_with(
    head_repo,
    expected_existing_head_sha(),
    expected_base_sha(),
  )
}

fn existing_target_contents_with(
  head_repo: String,
  expected_head_sha: String,
  base_sha: String,
) -> String {
  existing_target_contents_with_base_branch(
    head_repo,
    expected_head_sha,
    "main",
    base_sha,
  )
}

fn existing_target_contents_with_base_branch(
  head_repo: String,
  expected_head_sha: String,
  base_branch: String,
  base_sha: String,
) -> String {
  json.object([
    #("schema_version", json.int(1)),
    #(
      "artifact_type",
      json.string("scherzo.github_existing_pr_branch_target.v1"),
    ),
    #(
      "repository",
      json.object([#("repo", json.string("scherzo-systems/scherzo"))]),
    ),
    #(
      "head",
      json.object([
        #("repo", json.string(head_repo)),
        #("branch", json.string(existing_branch())),
        #("sha", json.string(expected_head_sha)),
      ]),
    ),
    #(
      "base",
      json.object([
        #("branch", json.string(base_branch)),
        #("sha", json.string(base_sha)),
      ]),
    ),
    #(
      "pull_request",
      json.object([
        #("number", json.int(42)),
        #("url", json.string("https://example.test/pr/42")),
      ]),
    ),
  ])
  |> json.to_string
}

fn existing_branch_target_contents_without_pr() -> String {
  json.object([
    #("schema_version", json.int(1)),
    #(
      "artifact_type",
      json.string("scherzo.github_existing_pr_branch_target.v1"),
    ),
    #(
      "repository",
      json.object([#("repo", json.string("scherzo-systems/scherzo"))]),
    ),
    #(
      "head",
      json.object([
        #("repo", json.string("scherzo-systems/scherzo")),
        #("branch", json.string(existing_branch())),
        #("sha", json.string(expected_existing_head_sha())),
      ]),
    ),
    #(
      "base",
      json.object([
        #("branch", json.string("main")),
        #("sha", json.string(expected_base_sha())),
      ]),
    ),
  ])
  |> json.to_string
}

fn plan_ref() -> String {
  plan_ref_for_run("run-1")
}

fn plan_ref_for_run(run_id: String) -> String {
  "runs/" <> run_id <> "/outputs/review_doc.md"
}

fn pack_ref() -> String {
  "runs/run-1/outputs/implementation_pack.json"
}

fn bundle_ref() -> String {
  "runs/run-1/outputs/exec_plan_bundle.json"
}

fn visual_bundle_ref() -> String {
  "runs/run-1/outputs/visual_artifacts.json"
}

fn visual_screenshot_ref() -> String {
  "runs/run-1/outputs/screenshot.png"
}

fn commit_stack_ref() -> String {
  "runs/run-1/outputs/commit_stack.json"
}

fn existing_target_ref() -> String {
  "runs/run-1/outputs/merge_conflict_target.json"
}

fn commit_stack_carrier_ref() -> String {
  "runs/run-1/outputs/commit_stack.bundle"
}

fn existing_branch() -> String {
  "feature/conflict-resolution"
}

fn expected_existing_head_sha() -> String {
  "1111111111111111111111111111111111111111"
}

fn expected_base_sha() -> String {
  "2222222222222222222222222222222222222222"
}

fn commit_stack_head_sha() -> String {
  "3333333333333333333333333333333333333333"
}

fn commit_stack_head_tree() -> String {
  "4444444444444444444444444444444444444444"
}

fn commit_stack_carrier() -> String {
  "bundle bytes"
}

fn plan_sha() -> String {
  hash.sha256_hex(plan_contents())
}

fn pack_sha() -> String {
  hash.sha256_hex(pack_contents())
}

fn plan_bytes() -> Int {
  bytes_of(plan_contents())
}

fn pack_bytes() -> Int {
  bytes_of(pack_contents())
}

fn bytes_of(contents: String) -> Int {
  bit_array.byte_size(bit_array.from_string(contents))
}

fn manifest_json_without_work(payload: String) -> String {
  let assert Ok(#(prefix, _work_json)) =
    string.split_once(payload, on: ",\"work\":")
  prefix <> "}"
}

fn body_template() -> String {
  "Work {{ work.title }} {{ work.url }}\nVersion {{ publication.version_id }}\n{{ publication.files_markdown }}"
}

fn store_with_contents(
  contents: List(#(String, String)),
) -> artifact_store.Store {
  let refs = dict.from_list(contents)
  artifact_store.custom(
    "publication-planner-test",
    artifact_store.StoreCallbacks(
      write: fn(_, _) { Ok(Nil) },
      read: fn(ref) {
        case dict.get(refs, ref) {
          Ok(contents) -> Ok(contents)
          Error(Nil) -> Error(artifact_store.MissingStepArtifact(ref))
        }
      },
      write_bytes: fn(_, _) { Ok(Nil) },
      write_immutable_bytes: fn(_, _) { Ok(artifact_store.ImmutableWritten) },
      read_bytes: fn(ref) {
        case dict.get(refs, ref) {
          Ok(contents) -> Ok(bit_array.from_string(contents))
          Error(Nil) -> Error(artifact_store.MissingStepArtifact(ref))
        }
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://test/" <> ref,
          display_path: ref,
          local_path: None,
        ))
      },
    ),
  )
}
