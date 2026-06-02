import gleam/list
import gleam/option.{None, Some}
import gleam/result
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
      "task_updates.comment_on invalid event",
      minimal_config() <> "task_updates:\n" <> "  comment_on: [unknown]\n",
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
      "ui_server.endpoint must be https",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: http://scherzo.example/enroll\n"
        <> "  enrollment_token_env: UI_SERVER_TOKEN\n",
    ),
    #(
      "ui_server.endpoint rejects userinfo",
      minimal_config()
        <> "ui_server:\n"
        <> "  enabled: true\n"
        <> "  endpoint: https://user@scherzo.example/enroll\n"
        <> "  enrollment_token_env: UI_SERVER_TOKEN\n",
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
