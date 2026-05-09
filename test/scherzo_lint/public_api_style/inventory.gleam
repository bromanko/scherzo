import glance
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order, Eq}
import gleam/string

pub type Candidate {
  HighArity
  DuplicatePrimitiveParameters
  BroaderBoolParameters
}

pub type Finding {
  Finding(
    candidate: Candidate,
    path: String,
    module_name: String,
    subsystem: String,
    function_name: String,
    arity: Int,
    api_shape: String,
    primitive_type: Option(String),
    covered_by_existing_rule: Bool,
    likely_exception: Option(String),
    estimated_callsite_churn: Int,
  )
}

pub type InventoryError {
  ParseError(path: String, message: String)
}

pub fn findings_for_source(
  path path: String,
  source source: String,
) -> Result(List(Finding), InventoryError) {
  case glance.module(source) {
    Error(error) -> Error(ParseError(path, describe_parse_error(error)))
    Ok(module_) ->
      module_.functions
      |> list.flat_map(fn(definition) {
        findings_for_definition(definition, path: path)
      })
      |> sort_findings
      |> Ok
  }
}

pub fn sort_findings(findings: List(Finding)) -> List(Finding) {
  findings
  |> list.sort(by: fn(left, right) {
    compare_strings(finding_sort_key(left), finding_sort_key(right))
  })
}

pub fn candidate_name(candidate: Candidate) -> String {
  case candidate {
    HighArity -> "high-arity public functions"
    DuplicatePrimitiveParameters -> "duplicate primitive public parameters"
    BroaderBoolParameters -> "broader unlabelled public Bool parameters"
  }
}

pub fn candidate_key(candidate: Candidate) -> String {
  case candidate {
    HighArity -> "1-high-arity"
    DuplicatePrimitiveParameters -> "2-duplicate-primitive"
    BroaderBoolParameters -> "3-broader-bool"
  }
}

pub fn subsystem_for_path(path: String) -> String {
  case
    string.starts_with(path, "src/scherzo/control/")
    || path == "src/scherzo/ctl.gleam"
  {
    True -> "control and CLI surfaces"
    False -> subsystem_after_control(path)
  }
}

fn subsystem_after_control(path: String) -> String {
  case string.starts_with(path, "src/scherzo/orchestrator/") {
    True -> "orchestration"
    False -> subsystem_after_orchestration(path)
  }
}

fn subsystem_after_orchestration(path: String) -> String {
  case
    string.starts_with(path, "src/scherzo/agent/")
    || string.starts_with(path, "src/scherzo/pi/")
  {
    True -> "agent and pi integration"
    False -> subsystem_after_agent(path)
  }
}

fn subsystem_after_agent(path: String) -> String {
  case
    path == "src/scherzo/config.gleam"
    || path == "src/scherzo/model_config.gleam"
    || string.starts_with(path, "src/scherzo/config/")
  {
    True -> "configuration"
    False -> subsystem_after_configuration(path)
  }
}

fn subsystem_after_configuration(path: String) -> String {
  case string.starts_with(path, "src/scherzo/session/") {
    True -> "session tracking"
    False -> subsystem_after_session(path)
  }
}

fn subsystem_after_session(path: String) -> String {
  case string.starts_with(path, "src/scherzo/state/") {
    True -> "persisted state and projection"
    False -> subsystem_after_state(path)
  }
}

fn subsystem_after_state(path: String) -> String {
  case string.starts_with(path, "src/scherzo/linear") {
    True -> "Linear integration"
    False -> subsystem_after_linear(path)
  }
}

fn subsystem_after_linear(path: String) -> String {
  case string.starts_with(path, "src/scherzo/workspace") {
    True -> "workspace management"
    False -> subsystem_after_workspace(path)
  }
}

fn subsystem_after_workspace(path: String) -> String {
  case string.starts_with(path, "src/scherzo/") {
    True -> "top-level utilities"
    False -> "other"
  }
}

pub fn module_name_for_path(path: String) -> String {
  let without_src = case string.starts_with(path, "src/") {
    True -> string.drop_start(path, 4)
    False -> path
  }

  let without_extension = case string.ends_with(without_src, ".gleam") {
    True -> string.drop_end(without_src, 6)
    False -> without_src
  }

  string.replace(without_extension, each: "/", with: ".")
}

fn findings_for_definition(
  definition: glance.Definition(glance.Function),
  path path: String,
) -> List(Finding) {
  let function = definition.definition

  case
    function.publicity == glance.Public && !is_external(definition.attributes)
  {
    False -> []
    True -> findings_for_function(path, function)
  }
}

fn findings_for_function(
  path: String,
  function: glance.Function,
) -> List(Finding) {
  let arity = list.length(function.parameters)

  list.append(
    high_arity_findings(path, function, arity),
    list.append(
      duplicate_primitive_findings(path, function, arity),
      broader_bool_findings(path, function, arity),
    ),
  )
}

fn high_arity_findings(
  path: String,
  function: glance.Function,
  arity: Int,
) -> List(Finding) {
  case arity >= 4 {
    False -> []
    True -> [
      base_finding(
        path: path,
        function: function,
        candidate: HighArity,
        arity: arity,
        api_shape: arity_bucket(arity),
        primitive_type: None,
        covered_by_existing_rule: False,
        estimated_callsite_churn: 1,
      ),
    ]
  }
}

fn duplicate_primitive_findings(
  path: String,
  function: glance.Function,
  arity: Int,
) -> List(Finding) {
  ["Bool", "String", "Int", "Float"]
  |> list.filter_map(fn(primitive) {
    let names = unlabelled_primitive_names(function.parameters, primitive)

    case list.length(names) >= 2 {
      False -> Error(Nil)
      True ->
        Ok(base_finding(
          path: path,
          function: function,
          candidate: DuplicatePrimitiveParameters,
          arity: arity,
          api_shape: "duplicate unlabelled "
            <> primitive
            <> " parameters: "
            <> join_names(names),
          primitive_type: Some(primitive),
          covered_by_existing_rule: primitive == "Bool" && arity == 2,
          estimated_callsite_churn: estimated_churn(
            primitive == "Bool" && arity == 2,
          ),
        ))
    }
  })
}

fn broader_bool_findings(
  path: String,
  function: glance.Function,
  arity: Int,
) -> List(Finding) {
  let names = unlabelled_primitive_names(function.parameters, "Bool")

  case list.is_empty(names) {
    True -> []
    False -> {
      let covered_by_existing_rule = arity == 2
      [
        base_finding(
          path: path,
          function: function,
          candidate: BroaderBoolParameters,
          arity: arity,
          api_shape: bool_shape(arity, names, covered_by_existing_rule),
          primitive_type: Some("Bool"),
          covered_by_existing_rule: covered_by_existing_rule,
          estimated_callsite_churn: estimated_churn(covered_by_existing_rule),
        ),
      ]
    }
  }
}

fn base_finding(
  path path: String,
  function function: glance.Function,
  candidate candidate: Candidate,
  arity arity: Int,
  api_shape api_shape: String,
  primitive_type primitive_type: Option(String),
  covered_by_existing_rule covered_by_existing_rule: Bool,
  estimated_callsite_churn estimated_callsite_churn: Int,
) -> Finding {
  Finding(
    candidate: candidate,
    path: path,
    module_name: module_name_for_path(path),
    subsystem: subsystem_for_path(path),
    function_name: function.name,
    arity: arity,
    api_shape: api_shape,
    primitive_type: primitive_type,
    covered_by_existing_rule: covered_by_existing_rule,
    likely_exception: likely_exception(function),
    estimated_callsite_churn: estimated_callsite_churn,
  )
}

fn estimated_churn(covered_by_existing_rule: Bool) -> Int {
  case covered_by_existing_rule {
    True -> 0
    False -> 1
  }
}

fn is_external(attributes: List(glance.Attribute)) -> Bool {
  list.any(attributes, fn(attribute) { attribute.name == "external" })
}

fn unlabelled_primitive_names(
  params: List(glance.FunctionParameter),
  primitive: String,
) -> List(String) {
  params
  |> list.filter_map(fn(param) {
    case param.label, param.name, param.type_ {
      None, glance.Named(name), Some(type_) -> {
        case primitive_type_name(type_) {
          Ok(type_name) if type_name == primitive -> Ok(name)
          _ -> Error(Nil)
        }
      }
      _, _, _ -> Error(Nil)
    }
  })
}

fn primitive_type_name(type_: glance.Type) -> Result(String, Nil) {
  case type_ {
    glance.NamedType(name: name, module: None, ..) -> {
      case name {
        "Bool" | "String" | "Int" | "Float" -> Ok(name)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn likely_exception(function: glance.Function) -> Option(String) {
  let lower_name = string.lowercase(function.name)

  case string.contains(does: lower_name, contain: "decode") {
    True -> Some("decode helper")
    False -> likely_exception_after_decode(function, lower_name)
  }
}

fn likely_exception_after_decode(
  function: glance.Function,
  lower_name: String,
) -> Option(String) {
  case is_callback_like(function.parameters) || is_comparator_like(function) {
    True -> Some("callback or comparator convention")
    False -> likely_exception_after_callback(lower_name)
  }
}

fn likely_exception_after_callback(lower_name: String) -> Option(String) {
  case is_builder_like(lower_name) {
    True -> Some("builder-style helper")
    False -> likely_exception_after_builder(lower_name)
  }
}

fn likely_exception_after_builder(lower_name: String) -> Option(String) {
  case string.contains(does: lower_name, contain: "ffi") {
    True -> Some("FFI wrapper")
    False -> None
  }
}

fn is_callback_like(params: List(glance.FunctionParameter)) -> Bool {
  list.any(params, fn(param) {
    case param.type_ {
      Some(glance.FunctionType(..)) -> True
      _ -> False
    }
  })
}

fn is_comparator_like(function: glance.Function) -> Bool {
  let lower_name = string.lowercase(function.name)

  string.contains(does: lower_name, contain: "compare")
  || has_named_parameter_pair(function.parameters, "left", "right")
  || has_named_parameter_pair(function.parameters, "before", "after")
}

fn has_named_parameter_pair(
  params: List(glance.FunctionParameter),
  first: String,
  second: String,
) -> Bool {
  let names =
    params
    |> list.filter_map(fn(param) {
      case param.name {
        glance.Named(name) -> Ok(name)
        glance.Discarded(_) -> Error(Nil)
      }
    })

  list.any(names, fn(name) { name == first })
  && list.any(names, fn(name) { name == second })
}

fn is_builder_like(lower_name: String) -> Bool {
  string.starts_with(lower_name, "with_")
  || string.starts_with(lower_name, "set_")
  || string.starts_with(lower_name, "add_")
  || string.starts_with(lower_name, "configure")
}

fn arity_bucket(arity: Int) -> String {
  case arity >= 6 {
    True -> "arity 6+"
    False -> "arity " <> int.to_string(arity)
  }
}

fn bool_shape(
  arity: Int,
  names: List(String),
  covered_by_existing_rule: Bool,
) -> String {
  let suffix = case covered_by_existing_rule {
    True -> " (covered by scherzo_public_function_labels)"
    False -> ""
  }

  "unlabelled Bool parameter(s) in "
  <> arity_bucket(arity)
  <> ": "
  <> join_names(names)
  <> suffix
}

fn join_names(names: List(String)) -> String {
  names
  |> list.map(fn(name) { "'" <> name <> "'" })
  |> string.join(", ")
}

fn finding_sort_key(finding: Finding) -> String {
  candidate_key(finding.candidate)
  <> "|"
  <> finding.subsystem
  <> "|"
  <> finding.path
  <> "|"
  <> finding.function_name
  <> "|"
  <> finding.api_shape
}

fn compare_strings(left: String, right: String) -> Order {
  case string.compare(left, right) {
    Eq -> Eq
    order -> order
  }
}

fn describe_parse_error(error: glance.Error) -> String {
  case error {
    glance.UnexpectedEndOfInput -> "unexpected end of input"
    glance.UnexpectedToken(..) -> "unexpected token"
  }
}
