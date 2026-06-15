import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/config
import scherzo/json_schema_self_check
import scherzo/json_value
import scherzo/structured_output_json_schema
import scherzo/structured_output_validator
import scherzo/workflow_dag
import simplifile
import yay

fn env(name: String) {
  case name {
    "LINEAR_API_KEY" -> Some("lin_api_test")
    "LINEAR_PROJECT_SLUG" -> Some("demo-project")
    "UI_SERVER_TOKEN" -> Some("ui-token")
    _ -> None
  }
}

fn validator(
  name: String,
  path: String,
) -> workflow_dag.StructuredOutputValidator {
  workflow_dag.JsonSchemaValidator(
    name: name,
    path: path,
    draft: Some("2020-12"),
  )
}

fn context(
  validator: workflow_dag.StructuredOutputValidator,
) -> structured_output_validator.ValidatorContext {
  structured_output_validator.base_context(
    ".scherzo",
    ".",
    "test/tmp/public-yaml-schema",
    "public_yaml_schema",
    ".scherzo/workflows",
    "run-1",
    "schema_check",
    0,
    ".",
    "yaml_payload",
    "json",
    "yaml",
    None,
  )
  |> structured_output_validator.for_validator(validator, 0)
}

fn root_from_yaml(contents: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(contents)
  yay.document_root(document)
}

fn root_from_file(path: String) -> yay.Node {
  let assert Ok(contents) = simplifile.read(path)
  root_from_yaml(contents)
}

fn yaml_node_to_json(node: yay.Node) -> Result(json_value.JsonValue, String) {
  case node {
    yay.NodeMap(entries) -> map_entries_to_json(entries, [])
    yay.NodeSeq(values) -> values_to_json(values, [])
    yay.NodeStr(value) -> Ok(json_value.JString(value))
    yay.NodeInt(value) -> Ok(json_value.JInt(value))
    yay.NodeFloat(value) -> Ok(json_value.JFloat(value))
    yay.NodeBool(value) -> Ok(json_value.JBool(value))
    yay.NodeNil -> Ok(json_value.JNull)
  }
}

fn map_entries_to_json(
  entries: List(#(yay.Node, yay.Node)),
  acc: List(#(String, json_value.JsonValue)),
) -> Result(json_value.JsonValue, String) {
  case entries {
    [] -> Ok(json_value.JObject(list.reverse(acc)))
    [#(yay.NodeStr(key), value), ..rest] -> {
      use value <- result.try(yaml_node_to_json(value))
      map_entries_to_json(rest, [#(key, value), ..acc])
    }
    [#(_, _), ..] ->
      Error("YAML map keys must be strings for JSON Schema validation")
  }
}

fn values_to_json(
  values: List(yay.Node),
  acc: List(json_value.JsonValue),
) -> Result(json_value.JsonValue, String) {
  case values {
    [] -> Ok(json_value.JArray(list.reverse(acc)))
    [value, ..rest] -> {
      use value <- result.try(yaml_node_to_json(value))
      values_to_json(rest, [value, ..acc])
    }
  }
}

fn validate_yaml_against_schema(
  schema_name: String,
  schema_path: String,
  yaml_path: String,
) {
  let validator = validator(schema_name, schema_path)
  let root = root_from_file(yaml_path)
  validate_root_against_schema(validator, root)
}

fn validate_yaml_source_against_schema(
  schema_name: String,
  schema_path: String,
  yaml: String,
) {
  let validator = validator(schema_name, schema_path)
  let root = root_from_yaml(yaml)
  validate_root_against_schema(validator, root)
}

fn validate_root_against_schema(
  validator: workflow_dag.StructuredOutputValidator,
  root: yay.Node,
) {
  let assert Ok(payload) = yaml_node_to_json(root)
  assert structured_output_json_schema.run_json_schema_validator(
      validator,
      payload,
      context(validator),
      [],
    )
    == Ok(structured_output_validator.ValidatorPass)
}

fn reject_yaml_against_schema(
  schema_name: String,
  schema_path: String,
  yaml: String,
) {
  let validator = validator(schema_name, schema_path)
  let root = root_from_yaml(yaml)
  let assert Ok(payload) = yaml_node_to_json(root)
  let assert Error(_) =
    structured_output_json_schema.run_json_schema_validator(
      validator,
      payload,
      context(validator),
      [],
    )
}

fn assert_config_parses(path: String) {
  let root = root_from_file(path)
  let assert Ok(_) = config.resolve_orchestrator_root(root, path, env)
}

fn assert_config_source_parses(yaml: String) {
  let root = root_from_yaml(yaml)
  let assert Ok(_) =
    config.resolve_orchestrator_root(root, "test/tmp/scherzo.yaml", env)
  Nil
}

fn assert_config_rejected(yaml: String) {
  let root = root_from_yaml(yaml)
  let assert Error(_) =
    config.resolve_orchestrator_root(root, "test/tmp/scherzo.yaml", env)
  Nil
}

fn assert_workflow_parses(path: String) {
  let assert Ok(contents) = simplifile.read(path)
  let assert Ok(_) = workflow_dag.parse(contents)
}

fn assert_workflow_rejected(yaml: String) {
  let assert Error(_) = workflow_dag.parse(yaml)
  Nil
}

fn minimal_config() -> String {
  "version: 1\n"
  <> "tracker:\n"
  <> "  linear:\n"
  <> "    project: demo-project\n"
  <> "workflows:\n"
  <> "  research: workflows/research.yaml\n"
}

fn minimal_workflow() -> String {
  "version: 1\n"
  <> "id: sample\n"
  <> "steps:\n"
  <> "  - id: draft\n"
  <> "    prompt: prompts/draft.md\n"
}

pub fn config_schema_self_check_test() {
  assert json_schema_self_check.run(
      ".",
      "schemas/scherzo.config.v1.schema.json",
      "test/fixtures/public_yaml_schema/config-valid.json",
    )
    == Ok(Nil)
}

pub fn workflow_schema_self_check_test() {
  assert json_schema_self_check.run(
      ".",
      "schemas/scherzo.workflow.v1.schema.json",
      "test/fixtures/public_yaml_schema/workflow-valid.json",
    )
    == Ok(Nil)
}

pub fn config_examples_validate_and_parse_test() {
  let config_paths = [
    "examples/scherzo.yaml",
    "examples/scherzo-packaged-noop.yaml",
    "examples/scherzo-packaged-jj.yaml",
    ".scherzo/scherzo.yaml",
    "test/fixtures/schema/orchestrator_config_complete.yaml",
  ]

  list.each(config_paths, fn(path) {
    validate_yaml_against_schema(
      "public_config_schema",
      "schemas/scherzo.config.v1.schema.json",
      path,
    )
    assert_config_parses(path)
  })
}

pub fn workflow_examples_validate_and_parse_test() {
  let workflow_paths = [
    "examples/workflows/research.yaml",
    "examples/workflows/implementation.yaml",
    "examples/workflows/commit-stack-publication.yaml",
    "examples/workflows/github-pr-conflict-scout.yaml",
    "examples/workflows/merge-conflict-resolution.yaml",
    "workflows/dogfood/execplan.yaml",
    "workflows/dogfood/execplan-revision.yaml",
    "workflows/dogfood/execplan-implementation.yaml",
    "workflows/dogfood/research.yaml",
    "workflows/dogfood/implementation.yaml",
    "workflows/dogfood/github-pr-conflict-scout.yaml",
    "workflows/dogfood/merge-conflict-resolution.yaml",
    "workflows/dogfood/origin-sync.yaml",
    "workflows/dogfood/workspace-cleanup.yaml",
    "test/fixtures/schema/workflow_dag_complete.yaml",
  ]

  list.each(workflow_paths, fn(path) {
    validate_yaml_against_schema(
      "public_workflow_schema",
      "schemas/scherzo.workflow.v1.schema.json",
      path,
    )
    assert_workflow_parses(path)
  })
}

pub fn yaml_schema_modeline_comments_are_checked_in_test() {
  let config_paths = [
    ".scherzo/scherzo.yaml",
    "examples/scherzo.yaml",
    "examples/scherzo-packaged-noop.yaml",
    "examples/scherzo-packaged-jj.yaml",
    "test/fixtures/schema/orchestrator_config_complete.yaml",
  ]
  let workflow_paths = [
    "examples/workflows/research.yaml",
    "examples/workflows/implementation.yaml",
    "examples/workflows/commit-stack-publication.yaml",
    "examples/workflows/github-pr-conflict-scout.yaml",
    "examples/workflows/merge-conflict-resolution.yaml",
    "workflows/dogfood/execplan.yaml",
    "workflows/dogfood/execplan-revision.yaml",
    "workflows/dogfood/execplan-implementation.yaml",
    "workflows/dogfood/research.yaml",
    "workflows/dogfood/implementation.yaml",
    "workflows/dogfood/github-pr-conflict-scout.yaml",
    "workflows/dogfood/merge-conflict-resolution.yaml",
    "workflows/dogfood/origin-sync.yaml",
    "workflows/dogfood/workspace-cleanup.yaml",
    "test/fixtures/schema/workflow_dag_complete.yaml",
  ]

  list.each(config_paths, fn(path) {
    assert_file_starts_with(path, config_modeline_for(path))
  })
  list.each(workflow_paths, fn(path) {
    assert_file_starts_with(path, workflow_modeline_for(path))
  })
}

pub fn config_parser_schema_parity_edge_cases_are_accepted_test() {
  let cases = [
    "version: 1\n"
      <> "tracker:\n"
      <> "  linear:\n"
      <> "    tasks_from:\n"
      <> "      project: demo-project\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n",
    "version: 1\n"
      <> "tracker:\n"
      <> "  linear:\n"
      <> "    tasks_from:\n"
      <> "      projects: [demo-project, bugs]\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n",
    "version: 1\n"
      <> "tracker:\n"
      <> "  linear:\n"
      <> "    tasks_from:\n"
      <> "      or:\n"
      <> "        - project: demo-project\n"
      <> "        - and:\n"
      <> "            - projects: [bugs, ops]\n"
      <> "            - project: bugs\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n",
    "version: 1\n"
      <> "tracker:\n"
      <> "  linear:\n"
      <> "    project_slug: demo-project\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n",
    "version: 1\n"
      <> "tracker:\n"
      <> "  project_slug: demo-project\n"
      <> "  linear:\n"
      <> "    api_key_env: LINEAR_API_KEY\n"
      <> "workflows:\n"
      <> "  research: workflows/research.yaml\n",
    minimal_config()
      <> "agents:\n"
      <> "  recovery:\n"
      <> "    attempts: 0\n"
      <> "  runtime:\n"
      <> "    type: pi\n"
      <> "    stall_timeout: 0ms\n",
    minimal_config()
      <> "schedules:\n"
      <> "  - id: disabled_research\n"
      <> "    workflow: research\n"
      <> "    enabled: false\n",
    minimal_config()
      <> "ui_server:\n"
      <> "  enabled: true\n"
      <> "  endpoint: https://scherzo.example\n"
      <> "  credential_ref: work-laptop\n"
      <> "  daemon_label: Project Foo / MacBook\n",
    minimal_config()
      <> "ui_server:\n"
      <> "  enabled: true\n"
      <> "  endpoint: https://scherzo.example\n"
      <> "  credential_ref: work-laptop\n"
      <> "  daemon_label: \"  "
      <> string.repeat("x", times: 80)
      <> "  \"\n",
    minimal_config()
      <> "ui_server:\n"
      <> "  enabled: true\n"
      <> "  endpoint: http://127.0.0.1:4000\n"
      <> "  credential_ref: work-laptop\n",
    minimal_config()
      <> "task_updates:\n"
      <> "  enabled: true\n"
      <> "  states:\n"
      <> "    success: In Review\n"
      <> "    no_review_success: Done\n"
      <> "    failure: Triage\n"
      <> "  workflows:\n"
      <> "    research:\n"
      <> "      requires_review: false\n"
      <> "      states:\n"
      <> "        no_review_success: Done\n",
  ]

  list.each(cases, fn(yaml) {
    assert_config_source_parses(yaml)
    validate_yaml_source_against_schema(
      "public_config_schema",
      "schemas/scherzo.config.v1.schema.json",
      yaml,
    )
  })
}

pub fn config_removed_keys_and_invalid_shapes_are_rejected_test() {
  let cases = [
    #(
      "routing.workflows",
      minimal_config()
        <> "routing:\n"
        <> "  workflows:\n"
        <> "    legacy: workflows/legacy.yaml\n",
    ),
    #(
      "polling.interval_ms",
      minimal_config() <> "polling:\n  interval_ms: 30000\n",
    ),
    #(
      "remote_commands",
      minimal_config() <> "remote_commands:\n  enabled: true\n",
    ),
    #(
      "linear_commands",
      minimal_config() <> "linear_commands:\n  enabled: true\n",
    ),
    #(
      "scheduled_jobs",
      minimal_config()
        <> "scheduled_jobs:\n"
        <> "  - id: legacy\n"
        <> "    workflow: research\n"
        <> "    every: 1h\n",
    ),
    #(
      "artifact_limits.command_stream_max_chars",
      minimal_config()
        <> "artifact_limits:\n"
        <> "  command_stream_max_chars: 20000\n",
    ),
    #("agent.max_turns", minimal_config() <> "agent:\n  max_turns: 1\n"),
    #("pi.command", minimal_config() <> "pi:\n  command: pi --mode rpc\n"),
    #(
      "workspace.default_profile",
      minimal_config() <> "workspace:\n  default_profile: jj\n",
    ),
    #(
      "workspace.profiles",
      minimal_config()
        <> "workspace:\n"
        <> "  profiles:\n"
        <> "    main:\n"
        <> "      driver:\n"
        <> "        command: scripts/driver\n",
    ),
    #(
      "workspace.drivers.old.timeout_ms",
      minimal_config()
        <> "workspace:\n"
        <> "  driver: old\n"
        <> "  drivers:\n"
        <> "    old:\n"
        <> "      type: noop\n"
        <> "      timeout_ms: 1000\n",
    ),
    #(
      "workspace.drivers.old.hooks",
      minimal_config()
        <> "workspace:\n"
        <> "  driver: old\n"
        <> "  drivers:\n"
        <> "    old:\n"
        <> "      type: noop\n"
        <> "      hooks:\n"
        <> "        create: scripts/create.sh\n",
    ),
    #(
      "workspace.drivers.old.capabilities",
      minimal_config()
        <> "workspace:\n"
        <> "  driver: old\n"
        <> "  drivers:\n"
        <> "    old:\n"
        <> "      type: noop\n"
        <> "      capabilities: [status]\n",
    ),
    #(
      "workflows list instead of map",
      "version: 1\ntracker:\n  linear:\n    project: demo-project\nworkflows: []\n",
    ),
    #(
      "tasks_from conflicts with compatibility project",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "    tasks_from:\n"
        <> "      projects: [demo-project, bugs]\n"
        <> "workflows:\n"
        <> "  research: workflows/research.yaml\n",
    ),
    #(
      "tasks_from conflicts with nested project_slug",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project_slug: demo-project\n"
        <> "    tasks_from:\n"
        <> "      project: bugs\n"
        <> "workflows:\n"
        <> "  research: workflows/research.yaml\n",
    ),
    #(
      "tasks_from conflicts with flat project_slug",
      "version: 1\n"
        <> "tracker:\n"
        <> "  project_slug: demo-project\n"
        <> "  linear:\n"
        <> "    tasks_from:\n"
        <> "      project: bugs\n"
        <> "workflows:\n"
        <> "  research: workflows/research.yaml\n",
    ),
    #(
      "tasks_from has both project and projects",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    tasks_from:\n"
        <> "      project: demo-project\n"
        <> "      projects: [demo-project, bugs]\n"
        <> "workflows:\n"
        <> "  research: workflows/research.yaml\n",
    ),
    #(
      "task_updates.comment_on invalid event",
      minimal_config() <> "task_updates:\n" <> "  comment_on: [unknown]\n",
    ),
    #(
      "task_updates.workflows rejects flat success key",
      minimal_config()
        <> "task_updates:\n"
        <> "  workflows:\n"
        <> "    research:\n"
        <> "      success: Done\n",
    ),
    #(
      "task_updates.workflows.states rejects claim key",
      minimal_config()
        <> "task_updates:\n"
        <> "  workflows:\n"
        <> "    research:\n"
        <> "      states:\n"
        <> "        claim: In Progress\n",
    ),
    #(
      "custom driver without command",
      minimal_config()
        <> "workspace:\n"
        <> "  driver: custom\n"
        <> "  drivers:\n"
        <> "    custom:\n"
        <> "      type: custom\n",
    ),
    #(
      "agents.runtime.pi.args Scherzo-owned flag",
      minimal_config()
        <> "agents:\n"
        <> "  runtime:\n"
        <> "    type: pi\n"
        <> "    pi:\n"
        <> "      args: [--session]\n",
    ),
    #(
      "tracker.linear.endpoint must be https",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "    endpoint: http://api.linear.app/graphql\n"
        <> "workflows:\n"
        <> "  research: workflows/research.yaml\n",
    ),
    #(
      "ui_server.endpoint rejects non-loopback http",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: http://scherzo.example\n"
        <> "  credential_ref: work-laptop\n",
    ),
    #(
      "ui_server.endpoint rejects userinfo",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: https://user@scherzo.example\n"
        <> "  credential_ref: work-laptop\n",
    ),
    #(
      "ui_server.endpoint is required when enabled",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  credential_ref: work-laptop\n",
    ),
    #(
      "ui_server.credential_ref is required when enabled",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: https://scherzo.example/enroll\n",
    ),
    #(
      "ui_server.daemon_label rejects whitespace-only",
      minimal_config() <> "ui_server:\n  daemon_label: \"   \"\n",
    ),
    #(
      "ui_server.daemon_label rejects overlength",
      minimal_config()
        <> "ui_server:\n  daemon_label: "
        <> string.repeat("x", times: 81)
        <> "\n",
    ),
    #(
      "ui_server.daemon_label rejects control characters",
      minimal_config()
        <> "ui_server:\n  daemon_label: |\n    Project\n    Foo\n",
    ),
    #(
      "ui_server.enrollment_token_env is removed",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: https://scherzo.example/enroll\n"
        <> "  enrollment_token_env: UI_SERVER_TOKEN\n"
        <> "  credential_ref: work-laptop\n",
    ),
    #(
      "agents.provider is not a public config field",
      minimal_config()
        <> "agents:\n"
        <> "  model: openai/gpt-5\n"
        <> "  provider: openai\n",
    ),
  ]

  list.each(cases, fn(case_) {
    let #(_, yaml) = case_
    assert_config_rejected(yaml)
    reject_yaml_against_schema(
      "public_config_schema",
      "schemas/scherzo.config.v1.schema.json",
      yaml,
    )
  })
}

pub fn config_schema_only_invalid_paths_are_rejected_test() {
  let cases = [
    #(
      "workflow path Windows drive root",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "workflows:\n"
        <> "  research: 'C:\\workflows\\research.yaml'\n",
    ),
    #(
      "workflow path rooted backslash",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "workflows:\n"
        <> "  research: '\\workflows\\research.yaml'\n",
    ),
    #(
      "workflow path environment placeholder",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "workflows:\n"
        <> "  research: $WORKFLOW_PATH\n",
    ),
    #(
      "workflow path backslash traversal",
      "version: 1\n"
        <> "tracker:\n"
        <> "  linear:\n"
        <> "    project: demo-project\n"
        <> "workflows:\n"
        <> "  research: '..\\workflows\\research.yaml'\n",
    ),
  ]

  list.each(cases, fn(case_) {
    let #(_, yaml) = case_
    reject_yaml_against_schema(
      "public_config_schema",
      "schemas/scherzo.config.v1.schema.json",
      yaml,
    )
  })
}

pub fn workflow_removed_keys_and_invalid_shapes_are_rejected_test() {
  let cases = [
    #(
      "workflow max_parallel_steps",
      minimal_workflow() <> "max_parallel_steps: 2\n",
    ),
    #("workflow recover", minimal_workflow() <> "recover:\n  attempts: 1\n"),
    #(
      "workflow workspace_profile",
      minimal_workflow() <> "workspace_profile: jj\n",
    ),
    #(
      "workflow workspace_capabilities",
      minimal_workflow() <> "workspace_capabilities: [status]\n",
    ),
    #(
      "workflow workspace.extra",
      minimal_workflow()
        <> "workspace:\n"
        <> "  driver: noop\n"
        <> "  extra: true\n",
    ),
    #(
      "step workspace",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    workspace: main\n",
    ),
    #(
      "step timeout_ms",
      "version: 1\nid: sample\nsteps:\n  - id: run_tests\n    run: gleam test\n    timeout_ms: 1000\n",
    ),
    #(
      "step recover",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    recover:\n      attempts: 1\n",
    ),
    #(
      "command step structured_output",
      "version: 1\nid: sample\nsteps:\n  - id: run_tests\n    run: gleam test\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n",
    ),
    #(
      "agent step without prompt",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    kind: agent\n",
    ),
    #(
      "ambiguous inferred step kind",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    run: echo hi\n",
    ),
    #(
      "validator timeout_ms",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n      validators:\n        - type: command\n          argv: [python3]\n          timeout_ms: 1000\n",
    ),
    #(
      "structured_output has validator and validators",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n      validator: review_lane_draft\n      validators:\n        - type: command\n          argv: [python3]\n",
    ),
    #(
      "absolute parameters_schema_path",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n        parameters_schema_path: /tmp/schema.json\n",
    ),
    #(
      "traversal parameters_schema_path",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    structured_output:\n      source:\n        type: pi_tool_call\n        tool_name: submit_review_lane_draft\n        parameters_schema_path: ../schema.json\n",
    ),
    #(
      "step provider is not a public workflow field",
      "version: 1\nid: sample\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n    model: openai/gpt-5\n    provider: openai\n",
    ),
    #(
      "contract output source path rejects traversal",
      "version: 1\nid: sample\ncontract:\n  version: 1\n  outputs:\n    review_doc:\n      type: document.markdown\n      source:\n        step: draft\n        path: ../tmp/review.md\nsteps:\n  - id: draft\n    prompt: prompts/draft.md\n",
    ),
    #(
      "publication file path rejects traversal",
      publication_workflow_source(
        "      files:\n"
        <> "        - select:\n"
        <> "            output: exec_plan_bundle\n"
        <> "          path: ../docs/review.md\n",
      ),
    ),
    #(
      "publication pull request template rejects traversal",
      publication_workflow_source(
        "      pull_request:\n"
        <> "        body_template: ../docs/pr-body.md\n"
        <> "      files:\n"
        <> "        - select:\n"
        <> "            output: exec_plan_bundle\n"
        <> "          path: docs/review.md\n",
      ),
    ),
    #(
      "commit_stack publication rejects files selectors",
      commit_stack_publication_workflow_source(
        "      mode: commit_stack\n"
        <> "      files:\n"
        <> "        - select:\n"
        <> "            output: commit_stack\n"
        <> "          path: tmp/commit-stack.json\n"
        <> "      commit_stack:\n"
        <> "        select:\n"
        <> "          output: commit_stack\n"
        <> "      target:\n"
        <> "        kind: existing_pr_branch\n"
        <> "        source:\n"
        <> "          output: merge_conflict_target\n",
      ),
    ),
    #(
      "commit_stack pull request body template rejects traversal",
      commit_stack_publication_workflow_source(
        "      mode: commit_stack\n"
        <> "      pull_request:\n"
        <> "        body_template: ../docs/pr-body.md\n"
        <> "      commit_stack:\n"
        <> "        select:\n"
        <> "          output: commit_stack\n"
        <> "      target:\n"
        <> "        kind: stable_branch\n",
      ),
    ),
    #(
      "invalid next action state",
      minimal_workflow()
        <> "workstream_phase:\n"
        <> "  phase_id: implementation\n"
        <> "  next_actions:\n"
        <> "    - action_id: follow_up\n"
        <> "      workflow_id: research\n"
        <> "      state: queued\n",
    ),
  ]

  list.each(cases, fn(case_) {
    let #(_, yaml) = case_
    assert_workflow_rejected(yaml)
    reject_yaml_against_schema(
      "public_workflow_schema",
      "schemas/scherzo.workflow.v1.schema.json",
      yaml,
    )
  })
}

pub fn workflow_schema_only_invalid_shapes_are_rejected_test() {
  let structured_output_prefix =
    "version: 1\n"
    <> "id: sample\n"
    <> "steps:\n"
    <> "  - id: draft\n"
    <> "    prompt: prompts/draft.md\n"
    <> "    structured_output:\n"
    <> "      source:\n"
    <> "        type: pi_tool_call\n"
    <> "        tool_name: submit_review_lane_draft\n"

  let cases = [
    #(
      "unexpected agent step key",
      "version: 1\n"
        <> "id: sample\n"
        <> "steps:\n"
        <> "  - id: draft\n"
        <> "    prompt: prompts/draft.md\n"
        <> "    promt: prompts/typo.md\n",
    ),
    #(
      "Windows drive parameters_schema_path",
      structured_output_prefix
        <> "        parameters_schema_path: 'C:\\schema.json'\n",
    ),
    #(
      "rooted backslash parameters_schema_path",
      structured_output_prefix
        <> "        parameters_schema_path: '\\schema.json'\n",
    ),
    #(
      "backslash traversal parameters_schema_path",
      structured_output_prefix
        <> "        parameters_schema_path: '..\\schema.json'\n",
    ),
  ]

  list.each(cases, fn(case_) {
    let #(_, yaml) = case_
    reject_yaml_against_schema(
      "public_workflow_schema",
      "schemas/scherzo.workflow.v1.schema.json",
      yaml,
    )
  })
}

pub fn workflow_schema_accepts_sourced_commit_stack_target_test() {
  let yaml =
    commit_stack_publication_workflow_source(
      "      mode: commit_stack\n"
      <> "      commit_stack:\n"
      <> "        select:\n"
      <> "          output: commit_stack\n"
      <> "      target:\n"
      <> "        kind: sourced\n"
      <> "        source:\n"
      <> "          output: merge_conflict_target\n",
    )
  validate_yaml_source_against_schema(
    "public_workflow_schema",
    "schemas/scherzo.workflow.v1.schema.json",
    yaml,
  )
}

pub fn workflow_schema_accepts_generic_descriptor_contract_entries_test() {
  let yaml =
    "version: 1\n"
    <> "id: sample\n"
    <> "contract:\n"
    <> "  version: 1\n"
    <> "  outputs:\n"
    <> "    screenshot:\n"
    <> "      kind: file\n"
    <> "      media_type: image/png\n"
    <> "      artifact_type: scherzo_ui.screenshot.v1\n"
    <> "      source:\n"
    <> "        step: draft\n"
    <> "        path: tmp/final.png\n"
    <> "steps:\n"
    <> "  - id: draft\n"
    <> "    prompt: prompts/draft.md\n"
  validate_yaml_source_against_schema(
    "public_workflow_schema",
    "schemas/scherzo.workflow.v1.schema.json",
    yaml,
  )
}

pub fn workflow_schema_rejects_invalid_descriptor_contract_entries_test() {
  let yaml =
    "version: 1\n"
    <> "id: sample\n"
    <> "contract:\n"
    <> "  version: 1\n"
    <> "  outputs:\n"
    <> "    screenshot:\n"
    <> "      kind: file\n"
    <> "      media_type: image\n"
    <> "      artifact_type: scherzo/ui.screenshot.v1\n"
    <> "      source:\n"
    <> "        step: draft\n"
    <> "        path: tmp/final.png\n"
    <> "steps:\n"
    <> "  - id: draft\n"
    <> "    prompt: prompts/draft.md\n"
  reject_yaml_against_schema(
    "public_workflow_schema",
    "schemas/scherzo.workflow.v1.schema.json",
    yaml,
  )
}

fn publication_workflow_source(publication_fields: String) -> String {
  "version: 1\n"
  <> "id: sample\n"
  <> "contract:\n"
  <> "  version: 1\n"
  <> "  outputs:\n"
  <> "    exec_plan_bundle:\n"
  <> "      type: exec_plan_bundle\n"
  <> "      source:\n"
  <> "        step: draft\n"
  <> "        field: final_response\n"
  <> "steps:\n"
  <> "  - id: draft\n"
  <> "    prompt: prompts/draft.md\n"
  <> "artifacts:\n"
  <> "  publications:\n"
  <> "    - id: review_doc\n"
  <> "      repository: github.docs\n"
  <> publication_fields
}

fn commit_stack_publication_workflow_source(
  publication_fields: String,
) -> String {
  "version: 1\n"
  <> "id: sample\n"
  <> "contract:\n"
  <> "  version: 1\n"
  <> "  outputs:\n"
  <> "    commit_stack:\n"
  <> "      type: commit_stack\n"
  <> "      source:\n"
  <> "        step: draft\n"
  <> "        field: final_response\n"
  <> "    merge_conflict_target:\n"
  <> "      type: code_change\n"
  <> "      source:\n"
  <> "        step: draft\n"
  <> "        field: final_response\n"
  <> "steps:\n"
  <> "  - id: draft\n"
  <> "    prompt: prompts/draft.md\n"
  <> "artifacts:\n"
  <> "  publications:\n"
  <> "    - id: implementation_commit_stack\n"
  <> "      repository: github.code\n"
  <> publication_fields
}

fn assert_file_starts_with(path: String, expected: String) {
  let assert Ok(source) = simplifile.read(path)
  assert string.starts_with(source, expected <> "\n")
}

fn config_modeline_for(path: String) -> String {
  case string.starts_with(path, "test/fixtures/") {
    True ->
      "# yaml-language-server: $schema=../../../schemas/scherzo.config.v1.schema.json"
    False ->
      "# yaml-language-server: $schema=../schemas/scherzo.config.v1.schema.json"
  }
}

fn workflow_modeline_for(path: String) -> String {
  case string.starts_with(path, "test/fixtures/") {
    True ->
      "# yaml-language-server: $schema=../../../schemas/scherzo.workflow.v1.schema.json"
    False ->
      "# yaml-language-server: $schema=../../schemas/scherzo.workflow.v1.schema.json"
  }
}
