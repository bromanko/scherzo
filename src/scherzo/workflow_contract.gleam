import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/workflow_contract_descriptor_compat.{DescriptorCompatError}
import yay

pub type Contract {
  Contract(
    version: Int,
    inputs: List(InputSpec),
    context: List(ContextSpec),
    outputs: List(OutputSpec),
  )
}

pub type ContractType {
  Text
  ArtifactList
  DocumentMarkdown
  ExecPlan
  ExecPlanBundle
  ImplementationPack
  CodeChangeBundle
  GitRef
  Url
  CodeChange
}

pub type InputSpec {
  InputSpec(
    name: String,
    type_: ContractType,
    required: Bool,
    description: Option(String),
    source: Option(InputSource),
  )
}

pub type ContextSpec {
  ContextSpec(
    name: String,
    type_: ContractType,
    required: Bool,
    description: Option(String),
    source: Option(ContextSource),
  )
}

pub type OutputDescriptorSpec {
  OutputDescriptorSpec(
    kind: Option(String),
    ref_type: Option(String),
    media_type: Option(String),
    artifact_type: Option(String),
  )
}

pub type OutputSpec {
  OutputSpec(
    name: String,
    type_: ContractType,
    required: Bool,
    description: Option(String),
    source: Option(OutputSource),
    descriptor: Option(OutputDescriptorSpec),
  )
}

pub type InputSource {
  IssueContext
  ScheduledContext
  LiteralInput(value: String)
  MappedOutputSource
}

pub type ContextSource {
  WorkspaceDriverBase
  LiteralContext(value: String)
  MappedOutputContext
}

pub type OutputSource {
  StepField(step_id: String, field: OutputField)
  StepFile(step_id: String, path: String)
  StructuredOutput(step_id: String, artifact_name: String)
  StaticUrl(url: String)
  StaticGitRef(ref: String)
  InlineJson(step_id: String, artifact_name: String)
}

pub type OutputField {
  Stdout
  FinalResponse
}

pub type MappingMode {
  DirectMapping
  AppendMapping
}

pub type ContractError {
  ContractError(code: String, message: String)
}

pub fn parse(root: yay.Node) -> Result(Option(Contract), ContractError) {
  case get_node(root, "contract") {
    None -> Ok(None)
    Some(node) -> {
      use contract <- result.try(parse_contract_node(node))
      Ok(Some(contract))
    }
  }
}

pub fn validate_static(contract: Contract) -> Result(Nil, ContractError) {
  use Nil <- result.try(validate_unique_inputs(contract.inputs, []))
  use Nil <- result.try(validate_unique_context(contract.context, []))
  use Nil <- result.try(validate_unique_outputs(contract.outputs, []))
  use Nil <- result.try(validate_required_inputs(contract.inputs))
  use Nil <- result.try(validate_required_context(contract.context))
  validate_required_outputs(contract.outputs)
}

pub fn type_to_string(type_: ContractType) -> String {
  case type_ {
    Text -> "text"
    ArtifactList -> "artifact[]"
    DocumentMarkdown -> "document.markdown"
    ExecPlan -> "exec_plan"
    ExecPlanBundle -> "exec_plan_bundle"
    ImplementationPack -> "implementation_pack"
    CodeChangeBundle -> "code_change_bundle"
    GitRef -> "git_ref"
    Url -> "url"
    CodeChange -> "code_change"
  }
}

pub fn type_from_string(raw: String) -> Result(ContractType, ContractError) {
  case string.trim(raw) |> string.lowercase {
    "text" -> Ok(Text)
    "artifact[]" -> Ok(ArtifactList)
    "document.markdown" -> Ok(DocumentMarkdown)
    "exec_plan" -> Ok(ExecPlan)
    "exec_plan_bundle" -> Ok(ExecPlanBundle)
    "implementation_pack" -> Ok(ImplementationPack)
    "code_change_bundle" -> Ok(CodeChangeBundle)
    "git_ref" -> Ok(GitRef)
    "url" -> Ok(Url)
    "code_change" -> Ok(CodeChange)
    other -> error("unknown_contract_type", "unknown contract type: " <> other)
  }
}

pub fn valid_contract_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_lower_or_digit(first) && all(rest, is_name_char)
  }
}

pub fn parse_input_source(
  source: yay.Node,
  entry_name: String,
) -> Result(InputSource, ContractError) {
  case source {
    yay.NodeStr(raw) ->
      case string.trim(raw) |> string.lowercase {
        "issue_context" -> Ok(IssueContext)
        "scheduled_context" -> Ok(ScheduledContext)
        "mapped_output" -> Ok(MappedOutputSource)
        other ->
          error(
            "invalid_contract_input_source",
            "contract input " <> entry_name <> " has invalid source: " <> other,
          )
      }
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        source,
        "contract input " <> entry_name <> " source",
      ))
      use Nil <- result.try(require_exact_keys(
        entries,
        ["type", "value"],
        "contract input " <> entry_name <> " literal source",
      ))
      case get_entry(entries, "type"), get_entry(entries, "value") {
        Some(yay.NodeStr("literal")), Some(yay.NodeStr(value)) ->
          Ok(LiteralInput(value))
        Some(yay.NodeStr(other)), _ ->
          error(
            "invalid_contract_input_source",
            "contract input "
              <> entry_name
              <> " source type is not literal: "
              <> other,
          )
        _, _ ->
          error(
            "invalid_contract_input_source",
            "contract input "
              <> entry_name
              <> " literal source requires string type and value",
          )
      }
    }
    _ ->
      error(
        "invalid_contract_input_source",
        "contract input "
          <> entry_name
          <> " source must be a string or literal map",
      )
  }
}

pub fn parse_context_source(
  source: yay.Node,
  entry_name: String,
) -> Result(ContextSource, ContractError) {
  case source {
    yay.NodeStr(raw) ->
      case string.trim(raw) |> string.lowercase {
        "workspace_driver_base" -> Ok(WorkspaceDriverBase)
        "mapped_output" -> Ok(MappedOutputContext)
        other ->
          error(
            "invalid_contract_context_source",
            "contract context "
              <> entry_name
              <> " has invalid source: "
              <> other,
          )
      }
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        source,
        "contract context " <> entry_name <> " source",
      ))
      use Nil <- result.try(require_exact_keys(
        entries,
        ["type", "value"],
        "contract context " <> entry_name <> " literal source",
      ))
      case get_entry(entries, "type"), get_entry(entries, "value") {
        Some(yay.NodeStr("literal")), Some(yay.NodeStr(value)) ->
          Ok(LiteralContext(value))
        Some(yay.NodeStr(other)), _ ->
          error(
            "invalid_contract_context_source",
            "contract context "
              <> entry_name
              <> " source type is not literal: "
              <> other,
          )
        _, _ ->
          error(
            "invalid_contract_context_source",
            "contract context "
              <> entry_name
              <> " literal source requires string type and value",
          )
      }
    }
    _ ->
      error(
        "invalid_contract_context_source",
        "contract context "
          <> entry_name
          <> " source must be a string or literal map",
      )
  }
}

pub fn parse_output_source(
  source: yay.Node,
  entry_name: String,
) -> Result(OutputSource, ContractError) {
  case source {
    yay.NodeMap(_) -> parse_output_source_map(source, entry_name)
    _ ->
      error(
        "invalid_contract_output_source",
        "contract output " <> entry_name <> " source must be a map",
      )
  }
}

pub fn input_source_to_canonical_json(source: InputSource) -> json.Json {
  case source {
    IssueContext -> json.object([#("kind", json.string("issue_context"))])
    ScheduledContext ->
      json.object([#("kind", json.string("scheduled_context"))])
    MappedOutputSource -> json.object([#("kind", json.string("mapped_output"))])
    LiteralInput(value) ->
      json.object([
        #("kind", json.string("literal")),
        #("value", json.string(value)),
      ])
  }
}

pub fn context_source_to_canonical_json(source: ContextSource) -> json.Json {
  case source {
    WorkspaceDriverBase ->
      json.object([#("kind", json.string("workspace_driver_base"))])
    MappedOutputContext ->
      json.object([#("kind", json.string("mapped_output"))])
    LiteralContext(value) ->
      json.object([
        #("kind", json.string("literal")),
        #("value", json.string(value)),
      ])
  }
}

pub fn output_source_to_canonical_json(source: OutputSource) -> json.Json {
  case source {
    StepField(step_id, field) ->
      json.object([
        #("kind", json.string("field")),
        #("step", json.string(step_id)),
        #("field", json.string(output_field_to_string(field))),
      ])
    StepFile(step_id, path) ->
      json.object([
        #("kind", json.string("file")),
        #("step", json.string(step_id)),
        #("path", json.string(path)),
      ])
    StructuredOutput(step_id, artifact_name) ->
      json.object([
        #("kind", json.string("structured_output")),
        #("step", json.string(step_id)),
        #("artifact_name", json.string(artifact_name)),
      ])
    InlineJson(step_id, artifact_name) ->
      json.object([
        #("kind", json.string("inline_json")),
        #("step", json.string(step_id)),
        #("artifact_name", json.string(artifact_name)),
      ])
    StaticUrl(url) ->
      json.object([#("kind", json.string("url")), #("value", json.string(url))])
    StaticGitRef(ref) ->
      json.object([
        #("kind", json.string("git_ref")),
        #("value", json.string(ref)),
      ])
  }
}

pub fn contract_to_canonical_json(contract: Contract) -> json.Json {
  json.object([
    #("version", json.int(contract.version)),
    #("inputs", json.array(sorted_inputs(contract.inputs), of: input_to_json)),
    #(
      "context",
      json.array(sorted_context(contract.context), of: context_to_json),
    ),
    #(
      "outputs",
      json.array(sorted_outputs(contract.outputs), of: output_to_json),
    ),
  ])
}

pub fn compatible(
  source: ContractType,
  target: ContractType,
  mode: MappingMode,
) -> Bool {
  case mode {
    DirectMapping -> source == target
    AppendMapping -> target == ArtifactList && appendable_source(source)
  }
}

pub fn output_field_to_string(field: OutputField) -> String {
  case field {
    Stdout -> "stdout"
    FinalResponse -> "final_response"
  }
}

fn parse_contract_node(node: yay.Node) -> Result(Contract, ContractError) {
  case node {
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(node, "contract"))
      use Nil <- result.try(validate_contract_root_keys(entries))
      use version <- result.try(read_version(entries))
      use inputs <- result.try(read_inputs(entries))
      use context <- result.try(read_context(entries))
      use outputs <- result.try(read_outputs(entries))
      let contract =
        Contract(
          version: version,
          inputs: inputs,
          context: context,
          outputs: outputs,
        )
      use Nil <- result.try(validate_static(contract))
      Ok(contract)
    }
    _ -> error("contract_not_map", "contract must be a map")
  }
}

fn validate_contract_root_keys(
  entries: List(#(String, yay.Node)),
) -> Result(Nil, ContractError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, _), ..rest] ->
      case key {
        "version" | "inputs" | "context" | "outputs" ->
          validate_contract_root_keys(rest)
        other ->
          error("unknown_contract_key", "unknown contract key: " <> other)
      }
  }
}

fn read_version(
  entries: List(#(String, yay.Node)),
) -> Result(Int, ContractError) {
  case get_entry(entries, "version") {
    Some(yay.NodeInt(1)) -> Ok(1)
    Some(_) -> error("invalid_contract_version", "contract.version must be 1")
    None -> error("missing_contract_version", "contract.version is required")
  }
}

fn read_inputs(
  entries: List(#(String, yay.Node)),
) -> Result(List(InputSpec), ContractError) {
  case get_entry(entries, "inputs") {
    None -> Ok([])
    Some(yay.NodeMap(pairs)) -> read_input_entries(pairs, [], [])
    Some(_) -> error("contract_inputs_not_map", "contract.inputs must be a map")
  }
}

fn read_context(
  entries: List(#(String, yay.Node)),
) -> Result(List(ContextSpec), ContractError) {
  case get_entry(entries, "context") {
    None -> Ok([])
    Some(yay.NodeMap(pairs)) -> read_context_entries(pairs, [], [])
    Some(_) ->
      error("contract_context_not_map", "contract.context must be a map")
  }
}

fn read_outputs(
  entries: List(#(String, yay.Node)),
) -> Result(List(OutputSpec), ContractError) {
  case get_entry(entries, "outputs") {
    None -> Ok([])
    Some(yay.NodeMap(pairs)) -> read_output_entries(pairs, [], [])
    Some(_) ->
      error("contract_outputs_not_map", "contract.outputs must be a map")
  }
}

fn read_input_entries(
  entries: List(#(yay.Node, yay.Node)),
  seen: List(String),
  acc: List(InputSpec),
) -> Result(List(InputSpec), ContractError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(name), value), ..rest] -> {
      use Nil <- result.try(validate_entry_name("input", name, seen))
      use spec <- result.try(read_input_spec(name, value))
      read_input_entries(rest, [name, ..seen], [spec, ..acc])
    }
    [#(_, _), ..] ->
      error(
        "contract_input_name_not_string",
        "contract input names must be strings",
      )
  }
}

fn read_context_entries(
  entries: List(#(yay.Node, yay.Node)),
  seen: List(String),
  acc: List(ContextSpec),
) -> Result(List(ContextSpec), ContractError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(name), value), ..rest] -> {
      use Nil <- result.try(validate_entry_name("context", name, seen))
      use spec <- result.try(read_context_spec(name, value))
      read_context_entries(rest, [name, ..seen], [spec, ..acc])
    }
    [#(_, _), ..] ->
      error(
        "contract_context_name_not_string",
        "contract context names must be strings",
      )
  }
}

fn read_output_entries(
  entries: List(#(yay.Node, yay.Node)),
  seen: List(String),
  acc: List(OutputSpec),
) -> Result(List(OutputSpec), ContractError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(name), value), ..rest] -> {
      use Nil <- result.try(validate_entry_name("output", name, seen))
      use spec <- result.try(read_output_spec(name, value))
      read_output_entries(rest, [name, ..seen], [spec, ..acc])
    }
    [#(_, _), ..] ->
      error(
        "contract_output_name_not_string",
        "contract output names must be strings",
      )
  }
}

fn validate_entry_name(
  kind: String,
  name: String,
  seen: List(String),
) -> Result(Nil, ContractError) {
  case list.contains(seen, name) {
    True ->
      error(
        "duplicate_contract_" <> kind,
        "duplicate contract " <> kind <> " name: " <> name,
      )
    False ->
      case valid_contract_name(name) {
        True -> Ok(Nil)
        False ->
          error(
            "invalid_contract_" <> kind <> "_name",
            "invalid contract " <> kind <> " name: " <> name,
          )
      }
  }
}

fn read_input_spec(
  name: String,
  node: yay.Node,
) -> Result(InputSpec, ContractError) {
  case node {
    yay.NodeStr(raw_type) -> {
      use type_ <- result.try(type_from_string(raw_type))
      Ok(InputSpec(name, type_, False, None, None))
    }
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        node,
        "contract input " <> name,
      ))
      use Nil <- result.try(validate_entry_keys(entries, "input", name))
      use type_ <- result.try(read_entry_type(entries, "input", name))
      use required <- result.try(read_entry_required(
        entries,
        True,
        "input",
        name,
      ))
      use description <- result.try(read_entry_description(
        entries,
        "input",
        name,
      ))
      use source <- result.try(read_input_source_option(entries, name))
      Ok(InputSpec(name, type_, required, description, source))
    }
    _ ->
      error(
        "contract_input_not_map",
        "contract input " <> name <> " must be a map or type string",
      )
  }
}

fn read_context_spec(
  name: String,
  node: yay.Node,
) -> Result(ContextSpec, ContractError) {
  case node {
    yay.NodeStr(raw_type) -> {
      use type_ <- result.try(type_from_string(raw_type))
      Ok(ContextSpec(name, type_, False, None, None))
    }
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        node,
        "contract context " <> name,
      ))
      use Nil <- result.try(validate_entry_keys(entries, "context", name))
      use type_ <- result.try(read_entry_type(entries, "context", name))
      use required <- result.try(read_entry_required(
        entries,
        False,
        "context",
        name,
      ))
      use description <- result.try(read_entry_description(
        entries,
        "context",
        name,
      ))
      use source <- result.try(read_context_source_option(entries, name))
      Ok(ContextSpec(name, type_, required, description, source))
    }
    _ ->
      error(
        "contract_context_not_map",
        "contract context " <> name <> " must be a map or type string",
      )
  }
}

fn read_output_spec(
  name: String,
  node: yay.Node,
) -> Result(OutputSpec, ContractError) {
  case node {
    yay.NodeStr(raw_type) -> {
      use type_ <- result.try(type_from_string(raw_type))
      Ok(OutputSpec(name, type_, False, None, None, None))
    }
    yay.NodeMap(_) -> {
      use entries <- result.try(read_map_entries(
        node,
        "contract output " <> name,
      ))
      use Nil <- result.try(validate_entry_keys(entries, "output", name))
      use type_ <- result.try(read_entry_type(entries, "output", name))
      use required <- result.try(read_entry_required(
        entries,
        True,
        "output",
        name,
      ))
      use description <- result.try(read_entry_description(
        entries,
        "output",
        name,
      ))
      use source <- result.try(read_output_source_option(entries, name))
      use descriptor <- result.try(read_output_descriptor(
        entries,
        "output",
        name,
      ))
      Ok(OutputSpec(name, type_, required, description, source, descriptor))
    }
    _ ->
      error(
        "contract_output_not_map",
        "contract output " <> name <> " must be a map or type string",
      )
  }
}

fn validate_entry_keys(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(Nil, ContractError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, _), ..rest] ->
      case key {
        "type"
        | "kind"
        | "ref_type"
        | "media_type"
        | "artifact_type"
        | "description"
        | "required"
        | "source" -> validate_entry_keys(rest, kind, name)
        "primary" ->
          error(
            "contract_primary_not_supported",
            "contract "
              <> kind
              <> " "
              <> name
              <> " uses primary; v1 selects outputs by name",
          )
        "main" | "default" ->
          error(
            "contract_selector_not_supported",
            "contract "
              <> kind
              <> " "
              <> name
              <> " uses "
              <> key
              <> "; v1 selects outputs by name",
          )
        other ->
          error(
            "unknown_contract_entry_key",
            "contract " <> kind <> " " <> name <> " has unknown key: " <> other,
          )
      }
  }
}

fn read_entry_type(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(ContractType, ContractError) {
  case get_entry(entries, "type"), get_entry(entries, "kind") {
    Some(yay.NodeStr(raw)), None -> type_from_string(raw)
    Some(yay.NodeStr(raw)), Some(yay.NodeStr(_)) -> {
      use explicit <- result.try(type_from_string(raw))
      use inferred <- result.try(type_from_descriptor(entries, kind, name))
      case explicit == inferred {
        True -> Ok(explicit)
        False ->
          error(
            "contract_descriptor_type_mismatch",
            "contract "
              <> kind
              <> " "
              <> name
              <> " type and descriptor fields disagree",
          )
      }
    }
    Some(_), _ ->
      error(
        "contract_entry_type_not_string",
        "contract " <> kind <> " " <> name <> " type must be a string",
      )
    None, Some(yay.NodeStr(_)) -> type_from_descriptor(entries, kind, name)
    None, Some(_) ->
      error(
        "contract_descriptor_kind_not_string",
        "contract " <> kind <> " " <> name <> " kind must be a string",
      )
    None, None ->
      error(
        "missing_contract_entry_type",
        "contract "
          <> kind
          <> " "
          <> name
          <> " type or descriptor kind is required",
      )
  }
}

fn type_from_descriptor(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(ContractType, ContractError) {
  use type_name <- result.try(
    workflow_contract_descriptor_compat.type_name_from_entries(
      entries,
      kind,
      name,
    )
    |> result.map_error(fn(error_value) {
      let DescriptorCompatError(code, message) = error_value
      ContractError(code, message)
    }),
  )
  type_from_string(type_name)
}

fn read_entry_required(
  entries: List(#(String, yay.Node)),
  default: Bool,
  kind: String,
  name: String,
) -> Result(Bool, ContractError) {
  case get_entry(entries, "required") {
    None -> Ok(default)
    Some(yay.NodeBool(value)) -> Ok(value)
    Some(_) ->
      error(
        "contract_entry_required_not_bool",
        "contract " <> kind <> " " <> name <> " required must be a boolean",
      )
  }
}

fn read_entry_description(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(Option(String), ContractError) {
  case get_entry(entries, "description") {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) ->
      error(
        "contract_entry_description_not_string",
        "contract " <> kind <> " " <> name <> " description must be a string",
      )
  }
}

fn read_output_descriptor(
  entries: List(#(String, yay.Node)),
  kind: String,
  name: String,
) -> Result(Option(OutputDescriptorSpec), ContractError) {
  case
    get_entry(entries, "kind"),
    get_entry(entries, "ref_type"),
    get_entry(entries, "media_type"),
    get_entry(entries, "artifact_type")
  {
    None, None, None, None -> Ok(None)
    _, _, _, _ -> {
      use descriptor_kind <- result.try(read_optional_descriptor_string(
        entries,
        "kind",
        kind,
        name,
      ))
      use ref_type <- result.try(read_optional_descriptor_string(
        entries,
        "ref_type",
        kind,
        name,
      ))
      use media_type <- result.try(read_optional_descriptor_string(
        entries,
        "media_type",
        kind,
        name,
      ))
      use artifact_type <- result.try(read_optional_descriptor_string(
        entries,
        "artifact_type",
        kind,
        name,
      ))
      use Nil <- result.try(validate_descriptor_media_type(
        media_type,
        kind,
        name,
      ))
      Ok(
        Some(OutputDescriptorSpec(
          kind: descriptor_kind,
          ref_type: ref_type,
          media_type: media_type,
          artifact_type: artifact_type,
        )),
      )
    }
  }
}

fn read_optional_descriptor_string(
  entries: List(#(String, yay.Node)),
  key: String,
  kind: String,
  name: String,
) -> Result(Option(String), ContractError) {
  case get_entry(entries, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(string.trim(value) |> string.lowercase))
    Some(_) ->
      error(
        "contract_descriptor_field_not_string",
        "contract " <> kind <> " " <> name <> " " <> key <> " must be a string",
      )
  }
}

fn validate_descriptor_media_type(
  media_type: Option(String),
  kind: String,
  name: String,
) -> Result(Nil, ContractError) {
  case media_type {
    None -> Ok(Nil)
    Some("application/json") | Some("text/markdown") | Some("text/plain") ->
      Ok(Nil)
    Some(_) ->
      error(
        "unsupported_contract_descriptor_media_type",
        "contract "
          <> kind
          <> " "
          <> name
          <> " media_type must be one of application/json, text/markdown, or text/plain",
      )
  }
}

fn read_input_source_option(
  entries: List(#(String, yay.Node)),
  name: String,
) -> Result(Option(InputSource), ContractError) {
  case get_entry(entries, "source") {
    None -> Ok(None)
    Some(node) -> {
      use source <- result.try(parse_input_source(node, name))
      Ok(Some(source))
    }
  }
}

fn read_context_source_option(
  entries: List(#(String, yay.Node)),
  name: String,
) -> Result(Option(ContextSource), ContractError) {
  case get_entry(entries, "source") {
    None -> Ok(None)
    Some(node) -> {
      use source <- result.try(parse_context_source(node, name))
      Ok(Some(source))
    }
  }
}

fn read_output_source_option(
  entries: List(#(String, yay.Node)),
  name: String,
) -> Result(Option(OutputSource), ContractError) {
  case get_entry(entries, "source") {
    None -> Ok(None)
    Some(node) -> {
      use source <- result.try(parse_output_source(node, name))
      Ok(Some(source))
    }
  }
}

fn parse_output_source_map(
  node: yay.Node,
  entry_name: String,
) -> Result(OutputSource, ContractError) {
  use entries <- result.try(read_map_entries(
    node,
    "contract output " <> entry_name <> " source",
  ))
  case
    get_entry(entries, "step"),
    get_entry(entries, "field"),
    get_entry(entries, "path"),
    get_entry(entries, "structured_output"),
    get_entry(entries, "inline_json"),
    get_entry(entries, "type"),
    get_entry(entries, "value")
  {
    Some(yay.NodeStr(step_id)),
      Some(yay.NodeStr(field)),
      None,
      None,
      None,
      None,
      None
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["step", "field"],
        "contract output " <> entry_name <> " field source",
      ))
      use field <- result.try(output_field_from_string(field, entry_name))
      Ok(StepField(step_id, field))
    }
    Some(yay.NodeStr(step_id)),
      None,
      Some(yay.NodeStr(path)),
      None,
      None,
      None,
      None
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["step", "path"],
        "contract output " <> entry_name <> " file source",
      ))
      case valid_output_path(path) {
        True -> Ok(StepFile(step_id, path))
        False ->
          error(
            "invalid_contract_output_path",
            "contract output "
              <> entry_name
              <> " source path must be relative and contain no control characters",
          )
      }
    }
    Some(yay.NodeStr(step_id)),
      None,
      None,
      Some(yay.NodeStr(artifact_name)),
      None,
      None,
      None
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["step", "structured_output"],
        "contract output " <> entry_name <> " structured_output source",
      ))
      Ok(StructuredOutput(step_id, artifact_name))
    }
    Some(yay.NodeStr(step_id)),
      None,
      None,
      None,
      Some(yay.NodeStr(artifact_name)),
      None,
      None
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["step", "inline_json"],
        "contract output " <> entry_name <> " inline_json source",
      ))
      Ok(InlineJson(step_id, artifact_name))
    }
    None,
      None,
      None,
      None,
      None,
      Some(yay.NodeStr("url")),
      Some(yay.NodeStr(url))
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["type", "value"],
        "contract output " <> entry_name <> " url source",
      ))
      case valid_http_url(url) {
        True -> Ok(StaticUrl(url))
        False ->
          error(
            "invalid_contract_output_url",
            "contract output "
              <> entry_name
              <> " source URL must be http or https",
          )
      }
    }
    None,
      None,
      None,
      None,
      None,
      Some(yay.NodeStr("git_ref")),
      Some(yay.NodeStr(ref))
    -> {
      use Nil <- result.try(require_exact_keys(
        entries,
        ["type", "value"],
        "contract output " <> entry_name <> " git_ref source",
      ))
      case valid_git_ref(ref) {
        True -> Ok(StaticGitRef(ref))
        False ->
          error(
            "invalid_contract_output_git_ref",
            "contract output "
              <> entry_name
              <> " source git_ref must be non-empty and contain no control characters",
          )
      }
    }
    _, _, _, _, _, _, _ ->
      error(
        "invalid_contract_output_source",
        "contract output " <> entry_name <> " source shape is invalid",
      )
  }
}

fn output_field_from_string(
  raw: String,
  entry_name: String,
) -> Result(OutputField, ContractError) {
  case string.trim(raw) |> string.lowercase {
    "stdout" -> Ok(Stdout)
    "final_response" -> Ok(FinalResponse)
    other ->
      error(
        "invalid_contract_output_field",
        "contract output "
          <> entry_name
          <> " source field is invalid: "
          <> other,
      )
  }
}

fn require_exact_keys(
  entries: List(#(String, yay.Node)),
  expected: List(String),
  context: String,
) -> Result(Nil, ContractError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, _), ..rest] ->
      case list.contains(expected, key) {
        True -> require_exact_keys(rest, expected, context)
        False ->
          error(
            "contract_source_extra_key",
            context <> " contains unsupported key: " <> key,
          )
      }
  }
}

fn validate_unique_inputs(
  inputs: List(InputSpec),
  seen: List(String),
) -> Result(Nil, ContractError) {
  case inputs {
    [] -> Ok(Nil)
    [input, ..rest] ->
      case list.contains(seen, input.name) {
        True ->
          error(
            "duplicate_contract_input",
            "duplicate contract input name: " <> input.name,
          )
        False -> validate_unique_inputs(rest, [input.name, ..seen])
      }
  }
}

fn validate_unique_context(
  context: List(ContextSpec),
  seen: List(String),
) -> Result(Nil, ContractError) {
  case context {
    [] -> Ok(Nil)
    [value, ..rest] ->
      case list.contains(seen, value.name) {
        True ->
          error(
            "duplicate_contract_context",
            "duplicate contract context name: " <> value.name,
          )
        False -> validate_unique_context(rest, [value.name, ..seen])
      }
  }
}

fn validate_unique_outputs(
  outputs: List(OutputSpec),
  seen: List(String),
) -> Result(Nil, ContractError) {
  case outputs {
    [] -> Ok(Nil)
    [output, ..rest] ->
      case list.contains(seen, output.name) {
        True ->
          error(
            "duplicate_contract_output",
            "duplicate contract output name: " <> output.name,
          )
        False -> validate_unique_outputs(rest, [output.name, ..seen])
      }
  }
}

fn validate_required_inputs(
  inputs: List(InputSpec),
) -> Result(Nil, ContractError) {
  case inputs {
    [] -> Ok(Nil)
    [input, ..rest] ->
      case input.required, input.source {
        True, None ->
          error(
            "contract_required_input_missing_source",
            "required contract input " <> input.name <> " must declare a source",
          )
        _, _ -> validate_required_inputs(rest)
      }
  }
}

fn validate_required_context(
  context: List(ContextSpec),
) -> Result(Nil, ContractError) {
  case context {
    [] -> Ok(Nil)
    [value, ..rest] ->
      case value.required, value.source {
        True, None ->
          error(
            "contract_required_context_missing_source",
            "required contract context "
              <> value.name
              <> " must declare a source",
          )
        _, _ -> validate_required_context(rest)
      }
  }
}

fn validate_required_outputs(
  outputs: List(OutputSpec),
) -> Result(Nil, ContractError) {
  case outputs {
    [] -> Ok(Nil)
    [output, ..rest] ->
      case output.required, output.source {
        True, None ->
          error(
            "contract_required_output_missing_source",
            "required contract output "
              <> output.name
              <> " must declare a source",
          )
        _, _ -> validate_required_outputs(rest)
      }
  }
}

fn input_to_json(input: InputSpec) -> json.Json {
  json.object([
    #("name", json.string(input.name)),
    #("type", json.string(type_to_string(input.type_))),
    #("required", json.bool(input.required)),
    #("description", option_string_to_json(input.description)),
    #("source", option_input_source_to_json(input.source)),
  ])
}

fn context_to_json(context: ContextSpec) -> json.Json {
  json.object([
    #("name", json.string(context.name)),
    #("type", json.string(type_to_string(context.type_))),
    #("required", json.bool(context.required)),
    #("description", option_string_to_json(context.description)),
    #("source", option_context_source_to_json(context.source)),
  ])
}

fn output_to_json(output: OutputSpec) -> json.Json {
  let fields = [
    #("name", json.string(output.name)),
    #("type", json.string(type_to_string(output.type_))),
    #("required", json.bool(output.required)),
    #("description", option_string_to_json(output.description)),
    #("source", option_output_source_to_json(output.source)),
  ]
  let fields = case output.descriptor {
    Some(descriptor) -> [
      #("descriptor", output_descriptor_to_json(descriptor)),
      ..fields
    ]
    None -> fields
  }
  json.object(fields)
}

fn output_descriptor_to_json(descriptor: OutputDescriptorSpec) -> json.Json {
  json.object([
    #("kind", option_string_to_json(descriptor.kind)),
    #("ref_type", option_string_to_json(descriptor.ref_type)),
    #("media_type", option_string_to_json(descriptor.media_type)),
    #("artifact_type", option_string_to_json(descriptor.artifact_type)),
  ])
}

fn option_input_source_to_json(value: Option(InputSource)) -> json.Json {
  case value {
    Some(source) -> input_source_to_canonical_json(source)
    None -> json.null()
  }
}

fn option_context_source_to_json(value: Option(ContextSource)) -> json.Json {
  case value {
    Some(source) -> context_source_to_canonical_json(source)
    None -> json.null()
  }
}

fn option_output_source_to_json(value: Option(OutputSource)) -> json.Json {
  case value {
    Some(source) -> output_source_to_canonical_json(source)
    None -> json.null()
  }
}

fn sorted_inputs(inputs: List(InputSpec)) -> List(InputSpec) {
  list.sort(inputs, by: fn(left, right) {
    string.compare(left.name, right.name)
  })
}

fn sorted_context(context: List(ContextSpec)) -> List(ContextSpec) {
  list.sort(context, by: fn(left, right) {
    string.compare(left.name, right.name)
  })
}

fn sorted_outputs(outputs: List(OutputSpec)) -> List(OutputSpec) {
  list.sort(outputs, by: fn(left, right) {
    string.compare(left.name, right.name)
  })
}

fn option_string_to_json(value: Option(String)) -> json.Json {
  case value {
    Some(value) -> json.string(value)
    None -> json.null()
  }
}

fn appendable_source(source: ContractType) -> Bool {
  case source {
    DocumentMarkdown
    | ExecPlan
    | ExecPlanBundle
    | ImplementationPack
    | CodeChangeBundle
    | Text
    | Url
    | GitRef
    | CodeChange -> True
    ArtifactList -> True
  }
}

fn valid_http_url(value: String) -> Bool {
  string.starts_with(value, "https://") || string.starts_with(value, "http://")
}

pub fn valid_git_ref(value: String) -> Bool {
  string.trim(value) != "" && !has_control_character(value)
}

fn valid_output_path(value: String) -> Bool {
  string.trim(value) != ""
  && !string.starts_with(value, "/")
  && !string.starts_with(value, "../")
  && !string.contains(value, "/../")
  && !string.ends_with(value, "/..")
  && value != ".."
  && !has_control_character(value)
}

fn has_control_character(value: String) -> Bool {
  value
  |> string.to_graphemes
  |> list.any(fn(ch) { ch == "\n" || ch == "\r" || ch == "\t" })
}

fn read_map_entries(
  node: yay.Node,
  context: String,
) -> Result(List(#(String, yay.Node)), ContractError) {
  case node {
    yay.NodeMap(pairs) -> read_map_entry_list(pairs, context, [])
    _ -> error("contract_map_expected", context <> " must be a map")
  }
}

fn read_map_entry_list(
  pairs: List(#(yay.Node, yay.Node)),
  context: String,
  acc: List(#(String, yay.Node)),
) -> Result(List(#(String, yay.Node)), ContractError) {
  case pairs {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(key), value), ..rest] ->
      read_map_entry_list(rest, context, [#(key, value), ..acc])
    [#(_, _), ..] ->
      error("contract_map_key_not_string", context <> " keys must be strings")
  }
}

fn get_entry(
  entries: List(#(String, yay.Node)),
  key: String,
) -> Option(yay.Node) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> get_entry(rest, key)
      }
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}

fn is_name_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
}

fn is_lower_or_digit(ch: String) -> Bool {
  is_lower(ch) || is_digit(ch)
}

fn is_lower(ch: String) -> Bool {
  string.compare(ch, "a") != Lt && string.compare(ch, "z") != Gt
}

fn is_digit(ch: String) -> Bool {
  string.compare(ch, "0") != Lt && string.compare(ch, "9") != Gt
}

fn all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && all(rest, predicate)
  }
}

fn error(code: String, message: String) -> Result(a, ContractError) {
  Error(ContractError(code, message))
}
