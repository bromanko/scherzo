import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/artifact_publication_config
import scherzo/json_decode_error
import scherzo/structured_output_source
import scherzo/workflow_contract
import scherzo/workflow_dag

pub type WorkflowInterfaceSnapshot {
  WorkflowInterfaceSnapshot(
    workflow_id: String,
    workflow_fingerprint: String,
    step_interfaces: List(StepInterface),
    contract_inputs: List(ContractEntryInterface),
    contract_context: List(ContractEntryInterface),
    contract_outputs: List(ContractEntryInterface),
    publication_routes: List(PublicationRouteInterface),
  )
}

pub type StepInterface {
  StepInterface(
    step_id: String,
    step_kind: String,
    depends_on: List(String),
    workspace_name: String,
    workspace_from: Option(String),
    structured_output: Option(StructuredOutputInterface),
    execution_fingerprint: String,
  )
}

pub type StructuredOutputInterface {
  StructuredOutputInterface(
    artifact_name: String,
    required: Bool,
    source_type: String,
    source_tool_name: Option(String),
    source_parameters_schema_path: Option(String),
    format: String,
    schema_required_keys: List(String),
    validators: List(StructuredOutputValidatorInterface),
    validation_retries: Int,
  )
}

pub type StructuredOutputValidatorInterface {
  StructuredOutputValidatorInterface(
    name: String,
    type_: String,
    path: Option(String),
    draft: Option(String),
    argv: List(String),
    timeout_ms: Option(Int),
    working_directory: Option(String),
    env: List(#(String, String)),
  )
}

pub type ContractEntryInterface {
  ContractEntryInterface(
    name: String,
    type_: String,
    required: Bool,
    source_kind: Option(String),
    source_json: Option(String),
    descriptor_kind: Option(String),
    descriptor_ref_type: Option(String),
    descriptor_media_type: Option(String),
    descriptor_artifact_type: Option(String),
  )
}

pub type PublicationRouteInterface {
  PublicationRouteInterface(
    id: String,
    repository: String,
    required: Bool,
    mode: String,
    selector_output: Option(String),
    selector_entry: Option(String),
    destination_path: Option(String),
    target_kind: String,
    target_output: Option(String),
    pull_request_title: Option(String),
    pull_request_body_template: Option(String),
  )
}

pub type DecodeError {
  InvalidSnapshot(String)
}

pub type SnapshotLoadStatus {
  InterfaceSnapshotMissing
  InterfaceSnapshotCorrupt(reason: String)
  InterfaceSnapshotLoaded(snapshot: WorkflowInterfaceSnapshot)
}

pub fn from_dag(
  dag: workflow_dag.WorkflowDag,
  workflow_fingerprint: String,
) -> WorkflowInterfaceSnapshot {
  let contract = workflow_dag.contract(dag)
  WorkflowInterfaceSnapshot(
    workflow_id: workflow_dag.id(dag),
    workflow_fingerprint: workflow_fingerprint,
    step_interfaces: workflow_dag.steps(dag) |> list.map(step_interface),
    contract_inputs: contract_input_interfaces(contract),
    contract_context: contract_context_interfaces(contract),
    contract_outputs: contract_output_interfaces(contract),
    publication_routes: workflow_dag.publication_routes(dag)
      |> list.map(publication_route_interface),
  )
}

pub fn to_string(snapshot: WorkflowInterfaceSnapshot) -> String {
  to_json(snapshot) |> json.to_string
}

pub fn decode_string(
  contents: String,
) -> Result(WorkflowInterfaceSnapshot, DecodeError) {
  case json.parse(contents, snapshot_decoder()) {
    Ok(snapshot) -> Ok(snapshot)
    Error(error) -> Error(InvalidSnapshot(json_decode_error.to_string(error)))
  }
}

pub fn compatible_prefix(
  recorded: WorkflowInterfaceSnapshot,
  current: WorkflowInterfaceSnapshot,
  repair_step_id: String,
) -> Option(#(List(String), String)) {
  case recorded.workflow_id != current.workflow_id {
    True -> None
    False ->
      compatible_prefix_steps(
        recorded.step_interfaces,
        current.step_interfaces,
        repair_step_id,
        [],
      )
  }
}

fn compatible_prefix_steps(
  recorded: List(StepInterface),
  current: List(StepInterface),
  repair_step_id: String,
  preserved: List(String),
) -> Option(#(List(String), String)) {
  case recorded, current {
    [recorded_step, ..recorded_rest], [current_step, ..current_rest] ->
      case recorded_step.step_id == repair_step_id {
        True -> Some(#(list.reverse(preserved), repair_step_id))
        False ->
          case recorded_step == current_step {
            True ->
              compatible_prefix_steps(
                recorded_rest,
                current_rest,
                repair_step_id,
                [recorded_step.step_id, ..preserved],
              )
            False -> Some(#(list.reverse(preserved), recorded_step.step_id))
          }
      }
    [recorded_step, ..], [] ->
      Some(#(list.reverse(preserved), recorded_step.step_id))
    [], _ -> Some(#(list.reverse(preserved), repair_step_id))
  }
}

fn step_interface(step: workflow_dag.WorkflowStep) -> StepInterface {
  let structured_output = case step.kind {
    workflow_dag.AgentStep(_, Some(spec)) ->
      Some(structured_output_interface(spec))
    _ -> None
  }
  StepInterface(
    step_id: step.id,
    step_kind: step_kind_name(step.kind),
    depends_on: step.depends_on,
    workspace_name: step.workspace.name,
    workspace_from: step.workspace.from,
    structured_output: structured_output,
    execution_fingerprint: execution_fingerprint(step),
  )
}

fn structured_output_interface(
  spec: workflow_dag.StructuredOutputSpec,
) -> StructuredOutputInterface {
  StructuredOutputInterface(
    artifact_name: spec.artifact_name,
    required: spec.required,
    source_type: structured_output_source.type_to_string(spec.source),
    source_tool_name: structured_output_source.tool_name(spec.source),
    source_parameters_schema_path: structured_output_source.parameters_schema_path(
      spec.source,
    ),
    format: workflow_dag.structured_output_format_to_string(spec.format),
    schema_required_keys: structured_schema_required_keys(spec.schema),
    validators: spec.validators
      |> list.map(structured_output_validator_interface),
    validation_retries: spec.validation_retries,
  )
}

fn structured_schema_required_keys(
  schema: workflow_dag.StructuredOutputSchema,
) -> List(String) {
  case schema {
    workflow_dag.StructuredObjectSchema(required_keys) -> required_keys
  }
}

fn structured_output_validator_interface(
  validator: workflow_dag.StructuredOutputValidator,
) -> StructuredOutputValidatorInterface {
  case validator {
    workflow_dag.JsonSchemaValidator(name, path, draft) ->
      StructuredOutputValidatorInterface(
        name: name,
        type_: "json_schema",
        path: Some(path),
        draft: draft,
        argv: [],
        timeout_ms: None,
        working_directory: None,
        env: [],
      )
    workflow_dag.CommandValidator(
      name,
      argv,
      timeout_ms,
      working_directory,
      env,
    ) ->
      StructuredOutputValidatorInterface(
        name: name,
        type_: "command",
        path: None,
        draft: None,
        argv: argv,
        timeout_ms: Some(timeout_ms),
        working_directory: Some(
          workflow_dag.validator_working_directory_to_string(working_directory),
        ),
        env: env,
      )
  }
}

fn step_kind_name(kind: workflow_dag.StepKind) -> String {
  case kind {
    workflow_dag.AgentStep(_, _) -> "agent"
    workflow_dag.CommandStep(_, _) -> "command"
  }
}

fn execution_fingerprint(step: workflow_dag.WorkflowStep) -> String {
  let kind_fingerprint = case step.kind {
    workflow_dag.AgentStep(prompt, structured_output) ->
      "agent:"
      <> prompt_fingerprint(prompt)
      <> ":"
      <> option.unwrap(
        option.map(structured_output, fn(spec) {
          structured_output_interface(spec) |> structured_output_fingerprint
        }),
        "none",
      )
    workflow_dag.CommandStep(run, timeout_ms) ->
      "command:"
      <> run
      <> ":"
      <> option.unwrap(option.map(timeout_ms, int.to_string), "none")
  }
  string.join(
    [
      step.id,
      kind_fingerprint,
      step.workspace.name,
      option.unwrap(step.workspace.from, ""),
      string.join(step.depends_on, with: ","),
    ],
    with: "|",
  )
}

fn structured_output_fingerprint(output: StructuredOutputInterface) -> String {
  string.join(
    [
      output.artifact_name,
      bool_to_string(output.required),
      output.source_type,
      option.unwrap(output.source_tool_name, ""),
      option.unwrap(output.source_parameters_schema_path, ""),
      output.format,
      string.join(output.schema_required_keys, with: ","),
      output.validators
        |> list.map(validator_fingerprint)
        |> string.join(with: ";"),
      int.to_string(output.validation_retries),
    ],
    with: ":",
  )
}

fn validator_fingerprint(
  validator: StructuredOutputValidatorInterface,
) -> String {
  string.join(
    [
      validator.name,
      validator.type_,
      option.unwrap(validator.path, ""),
      option.unwrap(validator.draft, ""),
      string.join(validator.argv, with: ","),
      option.unwrap(option.map(validator.timeout_ms, int.to_string), ""),
      option.unwrap(validator.working_directory, ""),
      validator.env
        |> list.map(fn(pair) {
          let #(key, value) = pair
          key <> "=" <> value
        })
        |> string.join(with: ","),
    ],
    with: ":",
  )
}

fn prompt_fingerprint(prompt: workflow_dag.PromptRef) -> String {
  case prompt {
    workflow_dag.PromptFile(path) -> "file:" <> path
    workflow_dag.PromptInline(contents) ->
      "inline:" <> int.to_string(string.length(contents))
    workflow_dag.PromptResolvedFile(_, contents) ->
      "inline:" <> int.to_string(string.length(contents))
  }
}

fn contract_input_interfaces(
  contract: Option(workflow_contract.Contract),
) -> List(ContractEntryInterface) {
  case contract {
    None -> []
    Some(contract) -> contract.inputs |> list.map(contract_input_interface)
  }
}

fn contract_context_interfaces(
  contract: Option(workflow_contract.Contract),
) -> List(ContractEntryInterface) {
  case contract {
    None -> []
    Some(contract) -> contract.context |> list.map(contract_context_interface)
  }
}

fn contract_output_interfaces(
  contract: Option(workflow_contract.Contract),
) -> List(ContractEntryInterface) {
  case contract {
    None -> []
    Some(contract) -> contract.outputs |> list.map(contract_output_interface)
  }
}

fn contract_input_interface(
  input: workflow_contract.InputSpec,
) -> ContractEntryInterface {
  ContractEntryInterface(
    name: input.name,
    type_: workflow_contract.type_to_string(input.type_),
    required: workflow_contract.requirement_required(input.source),
    source_kind: workflow_contract.requirement_source(input.source)
      |> option.map(input_source_kind),
    source_json: workflow_contract.requirement_source(input.source)
      |> option.map(fn(source) {
        workflow_contract.input_source_to_canonical_json(source)
        |> json.to_string
      }),
    descriptor_kind: descriptor_kind(input.descriptor),
    descriptor_ref_type: descriptor_ref_type(input.descriptor),
    descriptor_media_type: descriptor_media_type(input.descriptor),
    descriptor_artifact_type: descriptor_artifact_type(input.descriptor),
  )
}

fn contract_context_interface(
  context: workflow_contract.ContextSpec,
) -> ContractEntryInterface {
  ContractEntryInterface(
    name: context.name,
    type_: workflow_contract.type_to_string(context.type_),
    required: workflow_contract.requirement_required(context.source),
    source_kind: workflow_contract.requirement_source(context.source)
      |> option.map(context_source_kind),
    source_json: workflow_contract.requirement_source(context.source)
      |> option.map(fn(source) {
        workflow_contract.context_source_to_canonical_json(source)
        |> json.to_string
      }),
    descriptor_kind: descriptor_kind(context.descriptor),
    descriptor_ref_type: descriptor_ref_type(context.descriptor),
    descriptor_media_type: descriptor_media_type(context.descriptor),
    descriptor_artifact_type: descriptor_artifact_type(context.descriptor),
  )
}

fn contract_output_interface(
  output: workflow_contract.OutputSpec,
) -> ContractEntryInterface {
  ContractEntryInterface(
    name: output.name,
    type_: workflow_contract.type_to_string(output.type_),
    required: workflow_contract.requirement_required(output.source),
    source_kind: workflow_contract.requirement_source(output.source)
      |> option.map(output_source_kind),
    source_json: workflow_contract.requirement_source(output.source)
      |> option.map(fn(source) {
        workflow_contract.output_source_to_canonical_json(source)
        |> json.to_string
      }),
    descriptor_kind: descriptor_kind(output.descriptor),
    descriptor_ref_type: descriptor_ref_type(output.descriptor),
    descriptor_media_type: descriptor_media_type(output.descriptor),
    descriptor_artifact_type: descriptor_artifact_type(output.descriptor),
  )
}

fn descriptor_kind(
  descriptor: Option(workflow_contract.ContractDescriptorSpec),
) -> Option(String) {
  descriptor |> option.map(fn(value) { value.kind }) |> option.flatten
}

fn descriptor_ref_type(
  descriptor: Option(workflow_contract.ContractDescriptorSpec),
) -> Option(String) {
  descriptor |> option.map(fn(value) { value.ref_type }) |> option.flatten
}

fn descriptor_media_type(
  descriptor: Option(workflow_contract.ContractDescriptorSpec),
) -> Option(String) {
  descriptor |> option.map(fn(value) { value.media_type }) |> option.flatten
}

fn descriptor_artifact_type(
  descriptor: Option(workflow_contract.ContractDescriptorSpec),
) -> Option(String) {
  descriptor |> option.map(fn(value) { value.artifact_type }) |> option.flatten
}

fn input_source_kind(source: workflow_contract.InputSource) -> String {
  case source {
    workflow_contract.IssueContext -> "issue_context"
    workflow_contract.ScheduledContext -> "scheduled_context"
    workflow_contract.LiteralInput(_) -> "literal"
    workflow_contract.MappedOutputSource -> "mapped_output"
  }
}

fn context_source_kind(source: workflow_contract.ContextSource) -> String {
  case source {
    workflow_contract.WorkspaceDriverBase -> "workspace_driver_base"
    workflow_contract.LiteralContext(_) -> "literal"
    workflow_contract.MappedOutputContext -> "mapped_output"
  }
}

fn output_source_kind(source: workflow_contract.OutputSource) -> String {
  case source {
    workflow_contract.StepField(_, _) -> "field"
    workflow_contract.StepFile(_, _) -> "file"
    workflow_contract.StructuredOutput(_, _) -> "structured_output"
    workflow_contract.StaticUrl(_) -> "url"
    workflow_contract.StaticGitRef(_) -> "git_ref"
    workflow_contract.InlineJson(_, _) -> "inline_json"
  }
}

fn publication_route_interface(
  route: artifact_publication_config.PublicationRoute,
) -> PublicationRouteInterface {
  let #(mode, selector_output, selector_entry, destination_path) =
    publication_route_selector(route.publication)
  let #(target_kind, target_output) = publication_target(route.target)
  let #(pull_request_title, pull_request_body_template) = case
    route.pull_request
  {
    None -> #(None, None)
    Some(override) -> #(override.title, override.body_template)
  }
  PublicationRouteInterface(
    id: route.id,
    repository: route.repository,
    required: route.required,
    mode: mode,
    selector_output: selector_output,
    selector_entry: selector_entry,
    destination_path: destination_path,
    target_kind: target_kind,
    target_output: target_output,
    pull_request_title: pull_request_title,
    pull_request_body_template: pull_request_body_template,
  )
}

fn publication_route_selector(
  publication: artifact_publication_config.PublicationRoutePublication,
) -> #(String, Option(String), Option(String), Option(String)) {
  case publication {
    artifact_publication_config.FilePublicationRoute(files) ->
      case files {
        [artifact_publication_config.PublicationFileRoute(selector, path), ..] -> #(
          "files",
          Some(selector.output),
          selector.entry,
          Some(path),
        )
        [] -> #("files", None, None, None)
      }
    artifact_publication_config.CommitStackPublicationRoute(commit_stack) -> #(
      "commit_stack",
      Some(commit_stack.selector.output),
      None,
      None,
    )
  }
}

fn publication_target(
  target: artifact_publication_config.PublicationTarget,
) -> #(String, Option(String)) {
  case target {
    artifact_publication_config.StableBranchTarget -> #("stable_branch", None)
    artifact_publication_config.ExistingPrBranchTarget(source) -> #(
      "existing_pr_branch",
      Some(source.output),
    )
    artifact_publication_config.SourcedTarget(source) -> #(
      "sourced",
      Some(source.output),
    )
  }
}

fn to_json(snapshot: WorkflowInterfaceSnapshot) -> json.Json {
  json.object([
    #("schema_version", json.int(1)),
    #("workflow_id", json.string(snapshot.workflow_id)),
    #("workflow_fingerprint", json.string(snapshot.workflow_fingerprint)),
    #(
      "step_interfaces",
      json.array(snapshot.step_interfaces, step_interface_to_json),
    ),
    #(
      "contract_inputs",
      json.array(snapshot.contract_inputs, contract_entry_to_json),
    ),
    #(
      "contract_context",
      json.array(snapshot.contract_context, contract_entry_to_json),
    ),
    #(
      "contract_outputs",
      json.array(snapshot.contract_outputs, contract_entry_to_json),
    ),
    #(
      "publication_routes",
      json.array(snapshot.publication_routes, publication_route_to_json),
    ),
  ])
}

fn step_interface_to_json(step: StepInterface) -> json.Json {
  json.object([
    #("step_id", json.string(step.step_id)),
    #("step_kind", json.string(step.step_kind)),
    #("depends_on", json.array(step.depends_on, json.string)),
    #("workspace_name", json.string(step.workspace_name)),
    #("workspace_from", option_string_json(step.workspace_from)),
    #(
      "structured_output",
      option_structured_output_json(step.structured_output),
    ),
    #("execution_fingerprint", json.string(step.execution_fingerprint)),
  ])
}

fn structured_output_to_json(output: StructuredOutputInterface) -> json.Json {
  json.object([
    #("artifact_name", json.string(output.artifact_name)),
    #("required", json.bool(output.required)),
    #("source_type", json.string(output.source_type)),
    #("source_tool_name", option_string_json(output.source_tool_name)),
    #(
      "source_parameters_schema_path",
      option_string_json(output.source_parameters_schema_path),
    ),
    #("format", json.string(output.format)),
    #(
      "schema_required_keys",
      json.array(output.schema_required_keys, json.string),
    ),
    #(
      "validators",
      json.array(output.validators, structured_output_validator_to_json),
    ),
    #("validation_retries", json.int(output.validation_retries)),
  ])
}

fn structured_output_validator_to_json(
  validator: StructuredOutputValidatorInterface,
) -> json.Json {
  json.object([
    #("name", json.string(validator.name)),
    #("type", json.string(validator.type_)),
    #("path", option_string_json(validator.path)),
    #("draft", option_string_json(validator.draft)),
    #("argv", json.array(validator.argv, json.string)),
    #("timeout_ms", option_int_json(validator.timeout_ms)),
    #("working_directory", option_string_json(validator.working_directory)),
    #("env", json.array(validator.env, env_pair_to_json)),
  ])
}

fn contract_entry_to_json(entry: ContractEntryInterface) -> json.Json {
  json.object([
    #("name", json.string(entry.name)),
    #("type", json.string(entry.type_)),
    #("required", json.bool(entry.required)),
    #("source_kind", option_string_json(entry.source_kind)),
    #("source_json", option_string_json(entry.source_json)),
    #("descriptor_kind", option_string_json(entry.descriptor_kind)),
    #("descriptor_ref_type", option_string_json(entry.descriptor_ref_type)),
    #("descriptor_media_type", option_string_json(entry.descriptor_media_type)),
    #(
      "descriptor_artifact_type",
      option_string_json(entry.descriptor_artifact_type),
    ),
  ])
}

fn publication_route_to_json(route: PublicationRouteInterface) -> json.Json {
  json.object([
    #("id", json.string(route.id)),
    #("repository", json.string(route.repository)),
    #("required", json.bool(route.required)),
    #("mode", json.string(route.mode)),
    #("selector_output", option_string_json(route.selector_output)),
    #("selector_entry", option_string_json(route.selector_entry)),
    #("destination_path", option_string_json(route.destination_path)),
    #("target_kind", json.string(route.target_kind)),
    #("target_output", option_string_json(route.target_output)),
    #("pull_request_title", option_string_json(route.pull_request_title)),
    #(
      "pull_request_body_template",
      option_string_json(route.pull_request_body_template),
    ),
  ])
}

fn option_structured_output_json(
  value: Option(StructuredOutputInterface),
) -> json.Json {
  case value {
    Some(value) -> structured_output_to_json(value)
    None -> json.null()
  }
}

fn option_string_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn option_int_json(value: Option(Int)) -> json.Json {
  case value {
    Some(value) -> json.int(value)
    None -> json.null()
  }
}

fn env_pair_to_json(pair: #(String, String)) -> json.Json {
  let #(name, value) = pair
  json.object([#("name", json.string(name)), #("value", json.string(value))])
}

fn snapshot_decoder() -> decode.Decoder(WorkflowInterfaceSnapshot) {
  use schema_version <- decode.field("schema_version", decode.int)
  case schema_version == 1 {
    False ->
      decode.failure(empty_snapshot(), expected: "WorkflowInterfaceSnapshot")
    True -> {
      use workflow_id <- decode.field("workflow_id", decode.string)
      use workflow_fingerprint <- decode.field(
        "workflow_fingerprint",
        decode.string,
      )
      use step_interfaces <- decode.field(
        "step_interfaces",
        decode.list(step_interface_decoder()),
      )
      use contract_inputs <- decode.optional_field(
        "contract_inputs",
        [],
        decode.list(contract_entry_decoder()),
      )
      use contract_context <- decode.optional_field(
        "contract_context",
        [],
        decode.list(contract_entry_decoder()),
      )
      use contract_outputs <- decode.optional_field(
        "contract_outputs",
        [],
        decode.list(contract_entry_decoder()),
      )
      use publication_routes <- decode.optional_field(
        "publication_routes",
        [],
        decode.list(publication_route_decoder()),
      )
      decode.success(WorkflowInterfaceSnapshot(
        workflow_id: workflow_id,
        workflow_fingerprint: workflow_fingerprint,
        step_interfaces: step_interfaces,
        contract_inputs: contract_inputs,
        contract_context: contract_context,
        contract_outputs: contract_outputs,
        publication_routes: publication_routes,
      ))
    }
  }
}

fn step_interface_decoder() -> decode.Decoder(StepInterface) {
  use step_id <- decode.field("step_id", decode.string)
  use step_kind <- decode.field("step_kind", decode.string)
  use depends_on <- decode.field("depends_on", decode.list(decode.string))
  use workspace_name <- decode.field("workspace_name", decode.string)
  use workspace_from <- decode.optional_field(
    "workspace_from",
    None,
    decode.optional(decode.string),
  )
  use structured_output <- decode.optional_field(
    "structured_output",
    None,
    decode.optional(structured_output_decoder()),
  )
  use execution_fingerprint <- decode.field(
    "execution_fingerprint",
    decode.string,
  )
  decode.success(StepInterface(
    step_id: step_id,
    step_kind: step_kind,
    depends_on: depends_on,
    workspace_name: workspace_name,
    workspace_from: workspace_from,
    structured_output: structured_output,
    execution_fingerprint: execution_fingerprint,
  ))
}

fn structured_output_decoder() -> decode.Decoder(StructuredOutputInterface) {
  use artifact_name <- decode.field("artifact_name", decode.string)
  use required <- decode.field("required", decode.bool)
  use source_type <- decode.field("source_type", decode.string)
  use source_tool_name <- decode.optional_field(
    "source_tool_name",
    None,
    decode.optional(decode.string),
  )
  use source_parameters_schema_path <- decode.optional_field(
    "source_parameters_schema_path",
    None,
    decode.optional(decode.string),
  )
  use format <- decode.field("format", decode.string)
  use schema_required_keys <- decode.field(
    "schema_required_keys",
    decode.list(decode.string),
  )
  use validators <- decode.field(
    "validators",
    decode.list(structured_output_validator_decoder()),
  )
  use validation_retries <- decode.field("validation_retries", decode.int)
  decode.success(StructuredOutputInterface(
    artifact_name: artifact_name,
    required: required,
    source_type: source_type,
    source_tool_name: source_tool_name,
    source_parameters_schema_path: source_parameters_schema_path,
    format: format,
    schema_required_keys: schema_required_keys,
    validators: validators,
    validation_retries: validation_retries,
  ))
}

fn structured_output_validator_decoder() -> decode.Decoder(
  StructuredOutputValidatorInterface,
) {
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", decode.string)
  use path <- decode.optional_field(
    "path",
    None,
    decode.optional(decode.string),
  )
  use draft <- decode.optional_field(
    "draft",
    None,
    decode.optional(decode.string),
  )
  use argv <- decode.field("argv", decode.list(decode.string))
  use timeout_ms <- decode.optional_field(
    "timeout_ms",
    None,
    decode.optional(decode.int),
  )
  use working_directory <- decode.optional_field(
    "working_directory",
    None,
    decode.optional(decode.string),
  )
  use env <- decode.field("env", decode.list(env_pair_decoder()))
  decode.success(StructuredOutputValidatorInterface(
    name: name,
    type_: type_,
    path: path,
    draft: draft,
    argv: argv,
    timeout_ms: timeout_ms,
    working_directory: working_directory,
    env: env,
  ))
}

fn contract_entry_decoder() -> decode.Decoder(ContractEntryInterface) {
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", decode.string)
  use required <- decode.field("required", decode.bool)
  use source_kind <- decode.optional_field(
    "source_kind",
    None,
    decode.optional(decode.string),
  )
  use source_json <- decode.optional_field(
    "source_json",
    None,
    decode.optional(decode.string),
  )
  use descriptor_kind <- decode.optional_field(
    "descriptor_kind",
    None,
    decode.optional(decode.string),
  )
  use descriptor_ref_type <- decode.optional_field(
    "descriptor_ref_type",
    None,
    decode.optional(decode.string),
  )
  use descriptor_media_type <- decode.optional_field(
    "descriptor_media_type",
    None,
    decode.optional(decode.string),
  )
  use descriptor_artifact_type <- decode.optional_field(
    "descriptor_artifact_type",
    None,
    decode.optional(decode.string),
  )
  decode.success(ContractEntryInterface(
    name: name,
    type_: type_,
    required: required,
    source_kind: source_kind,
    source_json: source_json,
    descriptor_kind: descriptor_kind,
    descriptor_ref_type: descriptor_ref_type,
    descriptor_media_type: descriptor_media_type,
    descriptor_artifact_type: descriptor_artifact_type,
  ))
}

fn publication_route_decoder() -> decode.Decoder(PublicationRouteInterface) {
  use id <- decode.field("id", decode.string)
  use repository <- decode.field("repository", decode.string)
  use required <- decode.field("required", decode.bool)
  use mode <- decode.field("mode", decode.string)
  use selector_output <- decode.optional_field(
    "selector_output",
    None,
    decode.optional(decode.string),
  )
  use selector_entry <- decode.optional_field(
    "selector_entry",
    None,
    decode.optional(decode.string),
  )
  use destination_path <- decode.optional_field(
    "destination_path",
    None,
    decode.optional(decode.string),
  )
  use target_kind <- decode.field("target_kind", decode.string)
  use target_output <- decode.optional_field(
    "target_output",
    None,
    decode.optional(decode.string),
  )
  use pull_request_title <- decode.optional_field(
    "pull_request_title",
    None,
    decode.optional(decode.string),
  )
  use pull_request_body_template <- decode.optional_field(
    "pull_request_body_template",
    None,
    decode.optional(decode.string),
  )
  decode.success(PublicationRouteInterface(
    id: id,
    repository: repository,
    required: required,
    mode: mode,
    selector_output: selector_output,
    selector_entry: selector_entry,
    destination_path: destination_path,
    target_kind: target_kind,
    target_output: target_output,
    pull_request_title: pull_request_title,
    pull_request_body_template: pull_request_body_template,
  ))
}

fn env_pair_decoder() -> decode.Decoder(#(String, String)) {
  use name <- decode.field("name", decode.string)
  use value <- decode.field("value", decode.string)
  decode.success(#(name, value))
}

fn empty_snapshot() -> WorkflowInterfaceSnapshot {
  WorkflowInterfaceSnapshot(
    workflow_id: "",
    workflow_fingerprint: "",
    step_interfaces: [],
    contract_inputs: [],
    contract_context: [],
    contract_outputs: [],
    publication_routes: [],
  )
}

fn bool_to_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
