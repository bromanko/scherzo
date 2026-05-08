import glance
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import glinter/rule

pub fn rule() -> rule.Rule {
  rule.new(name: "scherzo_public_function_labels")
  |> rule.with_default_severity(severity: rule.Error)
  |> rule.with_simple_function_visitor(visitor: check_function)
  |> rule.to_module_rule()
}

fn check_function(
  definition: glance.Definition(glance.Function),
  span: glance.Span,
) -> List(rule.RuleError) {
  let function = definition.definition

  case is_public(function) && !is_external_attribute(definition.attributes) {
    False -> []
    True -> {
      let bool_names = ambiguous_bool_parameter_names(function.parameters)

      case should_report(function.parameters, bool_names) {
        False -> []
        True -> [
          rule.error(
            message: "Public function '"
              <> function.name
              <> "' should use labels for ambiguous parameters",
            details: details(bool_names),
            location: span,
          ),
        ]
      }
    }
  }
}

fn is_external_attribute(attributes: List(glance.Attribute)) -> Bool {
  list.any(attributes, fn(attribute) { attribute.name == "external" })
}

fn is_public(function: glance.Function) -> Bool {
  function.publicity == glance.Public
}

fn should_report(
  params: List(glance.FunctionParameter),
  bool_names: List(String),
) -> Bool {
  list.length(params) == 2 && !list.is_empty(bool_names)
}

fn ambiguous_bool_parameter_names(
  params: List(glance.FunctionParameter),
) -> List(String) {
  params
  |> list.filter_map(fn(param) {
    case param.label, param.name, param.type_ {
      None, glance.Named(name), Some(type_) -> {
        case primitive_type_name(type_) {
          Ok("Bool") -> Ok(name)
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

fn join_names(names: List(String)) -> String {
  names
  |> list.map(fn(name) { "'" <> name <> "'" })
  |> string.join(", ")
}

fn details(bool_names: List(String)) -> String {
  "Reasons:\n"
  <> "- two-parameter public function has unlabelled Bool parameter(s): "
  <> join_names(bool_names)
  <> "\n\nAdd labels with the `label name: Type` form. "
  <> "Use `// nolint: scherzo_public_function_labels -- reason` only for a genuine exception."
}
