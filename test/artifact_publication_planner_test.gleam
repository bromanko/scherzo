import gleam/bit_array
import gleam/dict
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_planner_decode
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

  let json = artifact_publication_planner.manifest_to_string(manifest)
  assert string.contains(json, "\"dry_run\":true")
  assert string.contains(json, "\"destination_path\":\"docs/plans/LIV-761.md\"")
  assert !string.contains(json, "\"pr_url\"")
  assert !string.contains(json, "\"commit_sha\"")
  assert !string.contains(json, "\"push_result\"")
  assert !string.contains(json, "\"mutation_status\"")
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

pub fn changed_issue_title_changes_pr_title_and_version_id_test() {
  let store = store_with_contents([#(plan_ref(), plan_contents())])
  let body_templates =
    dict.from_list([#("templates/publication.md", body_template())])
  let route = leaf_route_with_title("{{ work.identifier }}: {{ issue.title }}")
  let assert Ok(first) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      route,
      store,
      work_with_title("First title"),
      "run-1",
      body_templates,
    )
  let assert Ok(second) =
    artifact_publication_planner.plan_publication(
      leaf_manifest(plan_sha(), plan_bytes()),
      repositories(),
      route,
      store,
      work_with_title("Second title"),
      "run-1",
      body_templates,
    )

  assert first.pull_request.title == Some("LIV-761: First title")
  assert second.pull_request.title == Some("LIV-761: Second title")
  assert first.work_title == Some("First title")
  assert first.version_id != second.version_id
  let manifest_json = artifact_publication_planner.manifest_to_string(first)
  assert string.contains(manifest_json, "\"work_title\":\"First title\"")
  let assert Ok(decoded) =
    artifact_publication_planner_decode.decode_manifest_json(manifest_json)
  assert decoded.work_title == Some("First title")
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
  work_with_title("Publication test")
}

fn work_with_title(
  title: String,
) -> artifact_publication_planner.PublicationWork {
  artifact_publication_planner.PublicationWork(
    kind: artifact_publication_planner.TaskWork,
    id: "task-1",
    identifier: "LIV-761",
    slug: "LIV-761",
    title: Some(title),
  )
}

fn leaf_route() -> artifact_publication_config.PublicationRoute {
  leaf_route_with_selector("review_doc", None)
}

fn bundle_entry_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    mode: artifact_publication_config.FilesPublication,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "exec_plan_bundle",
          entry: Some("plan"),
        ),
        path: "docs/review/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
    commit_stack: None,
  )
}

fn bundle_entry_metadata_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    mode: artifact_publication_config.FilesPublication,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "exec_plan_bundle",
          entry: Some("plan"),
        ),
        path: "{{ artifact.metadata.publication.destination_path }}",
      ),
    ],
    commit_stack: None,
  )
}

fn leaf_route_with_path(
  path: String,
) -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    mode: artifact_publication_config.FilesPublication,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: "review_doc",
          entry: None,
        ),
        path: path,
      ),
    ],
    commit_stack: None,
  )
}

fn leaf_route_with_title(
  title: String,
) -> artifact_publication_config.PublicationRoute {
  let route = leaf_route()
  artifact_publication_config.PublicationRoute(
    ..route,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some(title),
        body_template: Some("templates/publication.md"),
      ),
    ),
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
    mode: artifact_publication_config.FilesPublication,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
    files: [
      artifact_publication_config.PublicationFileRoute(
        selector: artifact_publication_config.PublicationFileSelector(
          output: output,
          entry: entry,
        ),
        path: "docs/plans/{{ work.identifier }}{{ artifact.default_extension }}",
      ),
    ],
    commit_stack: None,
  )
}

fn duplicate_path_route() -> artifact_publication_config.PublicationRoute {
  artifact_publication_config.PublicationRoute(
    id: "review_doc",
    repository: "github.docs",
    required: True,
    mode: artifact_publication_config.FilesPublication,
    pull_request: Some(
      artifact_publication_config.PublicationPullRequestOverride(
        title: Some("{{ work.identifier }} publication"),
        body_template: Some("templates/publication.md"),
      ),
    ),
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
    commit_stack: None,
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

fn plan_contents() -> String {
  "# Review\n\nThis is the dry-run review doc.\n"
}

fn pack_contents() -> String {
  "{\"artifact_type\":\"implementation_pack\"}"
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

fn body_template() -> String {
  "Version {{ publication.version_id }}\n{{ publication.files_markdown }}"
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
