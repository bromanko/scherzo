import gleam/list
import gleam/string
import simplifile

fn read_file(path: String) -> String {
  case simplifile.read(path) {
    Ok(contents) -> contents
    Error(_) -> {
      let message = path <> " could not be read"
      panic as message
    }
  }
}

fn assert_contains(path: String, contents: String, expected: String) -> Nil {
  case string.contains(contents, expected) {
    True -> Nil
    False -> {
      let message = path <> " is missing expected text: " <> expected
      panic as message
    }
  }
}

fn assert_contains_all(
  path: String,
  contents: String,
  expected: List(String),
) -> Nil {
  list.each(expected, fn(text) { assert_contains(path, contents, text) })
}

fn assert_contains_validation_row(
  path: String,
  contents: String,
  feature: String,
  capability: String,
  config_path: String,
  message: String,
) -> Nil {
  assert_contains(
    path,
    contents,
    "| `"
      <> feature
      <> "` | `"
      <> capability
      <> "` | "
      <> config_path
      <> " | `"
      <> message
      <> "` |",
  )
}

pub fn tracker_adapter_spec_is_normative_contract_test() {
  let path = "docs/specs/TRACKER_ADAPTER_SPEC.md"
  let spec = read_file(path)

  assert_contains_all(path, spec, [
    "RFC 2119",
    "task system",
    "tracker adapter",
    "backend kind",
    "TaskRef",
    "Task",
    "TaskStateCategory",
    "TaskState",
    "TaskLabel",
    "TaskComment",
    "TaskAttachment",
    "TaskLink",
    "legacy issue compatibility",
    "task_source",
    "comments",
    "remote_commands",
    "state_transitions",
    "routing_metadata",
    "links",
    "handoff",
    "scheduled_failures",
    "readiness",
    "smoke",
    "attachments",
    "Unauthorized",
    "NotFound",
    "Transient",
    "Permanent",
    "UnsupportedCapability",
    "DecodeFailed",
    "`author_id`, `created_at`, and `updated_at` are optional",
    "tracker_capability_missing feature=<feature>",
    "remote command acknowledgements require comments capability",
    "at-least-once",
    "returns the source `event_id` as the acknowledgement receipt id",
    "dedupe_key",
    "at most one visible open failure task",
    "issue.*",
    "SCHERZO_ISSUE_*",
    "linear_command_seen",
    "Linear is the only production adapter",
  ])
}

pub fn tracker_adapter_spec_uses_canonical_capability_vocabulary_test() {
  let spec_path = "docs/specs/TRACKER_ADAPTER_SPEC.md"
  let adapter_path = "src/scherzo/tracker/adapter.gleam"
  let spec = read_file(spec_path)
  let adapter_source = read_file(adapter_path)
  let capability_names = [
    "task_source",
    "comments",
    "remote_commands",
    "state_transitions",
    "routing_metadata",
    "links",
    "handoff",
    "scheduled_failures",
    "readiness",
    "smoke",
    "attachments",
  ]

  assert_contains_all(spec_path, spec, capability_names)
  assert_contains_all(adapter_path, adapter_source, capability_names)
}

pub fn tracker_adapter_spec_documents_startup_validation_table_test() {
  let path = "docs/specs/TRACKER_ADAPTER_SPEC.md"
  let spec = read_file(path)

  list.each(
    [
      #(
        "remote_commands",
        "remote_commands",
        "`linear_commands.enabled` unless caller supplied another path",
        "linear_commands.enabled requires tracker adapter <kind> to expose remote_commands",
      ),
      #(
        "remote_command_ack",
        "comments",
        "same remote-command config path",
        "remote command acknowledgements require comments capability",
      ),
      #(
        "handoff_comments",
        "handoff",
        "`handoff.comments` or caller-supplied handoff path",
        "handoff comments require handoff capability",
      ),
      #(
        "handoff_state_moves",
        "state_transitions",
        "`handoff.states` or caller-supplied handoff path",
        "handoff state moves require state_transitions capability",
      ),
      #(
        "workflow_label_routing",
        "routing_metadata",
        "each configured workflow label path",
        "workflow label routing requires routing_metadata capability",
      ),
      #(
        "scheduled_failures",
        "scheduled_failures",
        "each enabled scheduled failure path",
        "scheduled failure publication requires scheduled_failures capability",
      ),
      #(
        "tracker_contract",
        "readiness",
        "`doctor.checks.tracker-contract`",
        "tracker contract checks require readiness capability",
      ),
      #(
        "tracker_smoke",
        "smoke",
        "`doctor.checks.tracker-smoke`",
        "tracker smoke checks require smoke capability",
      ),
    ],
    fn(row) {
      let #(feature, capability, config_path, message) = row
      assert_contains_validation_row(
        path,
        spec,
        feature,
        capability,
        config_path,
        message,
      )
    },
  )
}

pub fn tracker_adapter_spec_documents_durable_recovery_compatibility_test() {
  let path = "docs/specs/TRACKER_ADAPTER_SPEC.md"
  let spec = read_file(path)

  assert_contains_all(path, spec, [
    "`TaskRef.backend_kind` and `TaskRef.remote_id` MUST be stable",
    "Durable task fields are `task_backend_kind`, `task_remote_id`, `task_key`, and `task_url`.",
    "Decoding old workflow records without task fields MUST continue to synthesize a Linear task ref",
    "`linear_command_seen`, `linear_command_started`, `linear_command_completed`, and `linear_command_acked`",
    "`remote_command_seen`, `remote_command_started`, `remote_command_completed`, and `remote_command_acked`",
  ])
}

pub fn tracker_adapter_runbook_documents_capability_matrix_test() {
  let path = "docs/runbooks/tracker-adapters.md"
  let runbook = read_file(path)

  assert_contains(path, runbook, "task")
  assert_contains(path, runbook, "tracker adapter")
  assert_contains(path, runbook, "Linear issue")
  assert_contains(path, runbook, "tracker-smoke")
  assert_contains(path, runbook, "linear-smoke")
  assert_contains(path, runbook, "scripts/scherzo-execplan")
  assert_contains(path, runbook, "scheduled_failures")
  assert_contains(path, runbook, "remote_commands")
  assert_contains(path, runbook, "| Linear | Production |")
  assert_contains(path, runbook, "| Jira follow-up | Future |")
  assert_contains(path, runbook, "| Trello follow-up | Future |")
  assert_contains(path, runbook, "| test-memory | Test fixture |")
}

pub fn tracker_adapter_runbook_is_linked_from_operator_docs_test() {
  let readme_path = "README.md"
  let getting_started_path = "docs/GETTING_STARTED.md"
  let architecture_path = "docs/ARCHITECTURE.md"
  let runbook_path = "docs/runbooks/tracker-adapters.md"
  let scheduled_path = "docs/runbooks/scheduled-jobs.md"

  assert_contains(
    readme_path,
    read_file(readme_path),
    "docs/runbooks/tracker-adapters.md",
  )
  assert_contains(
    getting_started_path,
    read_file(getting_started_path),
    "runbooks/tracker-adapters.md",
  )
  assert_contains(
    getting_started_path,
    read_file(getting_started_path),
    "specs/TRACKER_ADAPTER_SPEC.md",
  )
  assert_contains(
    architecture_path,
    read_file(architecture_path),
    "runbooks/tracker-adapters.md",
  )
  assert_contains(
    architecture_path,
    read_file(architecture_path),
    "specs/TRACKER_ADAPTER_SPEC.md",
  )
  assert_contains(
    runbook_path,
    read_file(runbook_path),
    "../specs/TRACKER_ADAPTER_SPEC.md",
  )
  assert_contains(scheduled_path, read_file(scheduled_path), "tracker adapter")
}

pub fn getting_started_prefers_tracker_doctor_aliases_test() {
  let path = "docs/GETTING_STARTED.md"
  let guide = read_file(path)

  assert_contains(path, guide, "--check tracker-contract")
  assert_contains(path, guide, "--check tracker-smoke")
  assert_contains(
    path,
    guide,
    "linear-contract` and `linear-smoke` remain compatibility aliases",
  )
}

pub fn execplan_v2_workflows_use_task_operator_language_test() {
  let dogfood_readme_path = ".scherzo/README.md"
  let dogfood_readme = read_file(dogfood_readme_path)
  assert_contains(dogfood_readme_path, dogfood_readme, "workflow:execplan")
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "workflow:execplan-revision",
  )
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "workflow:execplan-implementation",
  )
  assert_contains(
    dogfood_readme_path,
    dogfood_readme,
    "Linear-backed implementation task",
  )

  let draft_prompt_path = ".scherzo/workflows/prompts/execplan-draft.md"
  let draft_prompt = read_file(draft_prompt_path)
  assert_contains(draft_prompt_path, draft_prompt, "for this task")
  assert_contains(draft_prompt_path, draft_prompt, "Task:")
  assert_contains(
    draft_prompt_path,
    draft_prompt,
    "source_issue` compatibility",
  )

  let implementation_workflow_path =
    ".scherzo/workflows/execplan-implementation.yaml"
  assert_contains(
    implementation_workflow_path,
    read_file(implementation_workflow_path),
    "execplan-implementation-verify-completion.md",
  )

  let pi_wrapper_path = "scripts/scherzo-pi"
  let pi_wrapper = read_file(pi_wrapper_path)
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan")
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan-revision")
  assert_contains(pi_wrapper_path, pi_wrapper, "execplan-implementation")
}
