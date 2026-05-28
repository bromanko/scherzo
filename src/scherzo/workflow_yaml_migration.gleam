import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import yay

pub type RemovedKeyError {
  RemovedKeyError(code: String, message: String)
}

pub fn reject_key(
  node: yay.Node,
  old_key: String,
  new_key: String,
  hint: String,
) -> Result(Nil, RemovedKeyError) {
  case get_node(node, old_key) {
    None -> Ok(Nil)
    Some(_) ->
      Error(RemovedKeyError(
        removed_key_code(old_key),
        old_key
          <> " was removed. Use "
          <> new_key
          <> " instead. "
          <> hint
          <> " See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
      ))
  }
}

pub fn reject_workflow_keys(root: yay.Node) -> Result(Nil, RemovedKeyError) {
  use _ <- result.try(reject_key(
    root,
    "workspace_profile",
    "workspace.driver",
    "Move workflow driver selection under workspace.driver.",
  ))
  use _ <- result.try(reject_key(
    root,
    "workspace_capabilities",
    "workspace.requires",
    "Move required workspace capabilities under workspace.requires.",
  ))
  use _ <- result.try(reject_key(
    root,
    "max_parallel_steps",
    "concurrency",
    "Rename max_parallel_steps to concurrency.",
  ))
  reject_key(
    root,
    "recover",
    "recovery",
    "Rename workflow recovery config to recovery.",
  )
}

pub fn reject_step_keys(node: yay.Node) -> Result(Nil, RemovedKeyError) {
  use _ <- result.try(reject_key(
    node,
    "workspace_profile",
    "workflow workspace.driver",
    "Step-level workspace_profile is not supported.",
  ))
  use _ <- result.try(reject_key(
    node,
    "workspace_capabilities",
    "workflow workspace.requires",
    "Step-level workspace_capabilities is not supported.",
  ))
  use _ <- result.try(reject_key(
    node,
    "workspace",
    "run_in",
    "Rename step workspace selection to run_in.",
  ))
  use _ <- result.try(reject_key(
    node,
    "timeout_ms",
    "timeout",
    "Use a duration string such as timeout: 5m.",
  ))
  reject_key(
    node,
    "recover",
    "recovery",
    "Rename step recovery config to recovery.",
  )
}

pub fn reject_unexpected_workspace_keys(
  node: yay.Node,
) -> Result(Nil, RemovedKeyError) {
  reject_unexpected_keys(
    node,
    "workspace",
    ["driver", "requires"],
    "unknown_workflow_workspace_key",
    "expected workspace.driver or workspace.requires",
  )
}

pub fn reject_step_discriminator_non_strings(
  node: yay.Node,
) -> Result(Nil, RemovedKeyError) {
  use _ <- result.try(reject_non_string_key(
    node,
    "kind",
    "step_kind_not_string",
  ))
  use _ <- result.try(reject_non_string_key(node, "prompt", "prompt_not_string"))
  reject_non_string_key(node, "run", "run_not_string")
}

pub fn code(error: RemovedKeyError) -> String {
  let RemovedKeyError(code, _) = error
  code
}

pub fn message(error: RemovedKeyError) -> String {
  let RemovedKeyError(_, message) = error
  message
}

fn reject_unexpected_keys(
  node: yay.Node,
  parent: String,
  allowed: List(String),
  code: String,
  expected: String,
) -> Result(Nil, RemovedKeyError) {
  case node {
    yay.NodeMap(pairs) ->
      reject_unexpected_key_pairs(pairs, parent, allowed, code, expected)
    _ -> Ok(Nil)
  }
}

fn reject_unexpected_key_pairs(
  pairs: List(#(yay.Node, yay.Node)),
  parent: String,
  allowed: List(String),
  code: String,
  expected: String,
) -> Result(Nil, RemovedKeyError) {
  case pairs {
    [] -> Ok(Nil)
    [#(yay.NodeStr(key), _), ..rest] ->
      case list.contains(allowed, key) {
        True ->
          reject_unexpected_key_pairs(rest, parent, allowed, code, expected)
        False ->
          Error(RemovedKeyError(
            code,
            parent <> "." <> key <> " is not supported; " <> expected,
          ))
      }
    [#(_, _), ..] ->
      Error(RemovedKeyError(code, parent <> " keys must be strings"))
  }
}

fn reject_non_string_key(
  node: yay.Node,
  key: String,
  code: String,
) -> Result(Nil, RemovedKeyError) {
  case get_node(node, key) {
    None -> Ok(Nil)
    Some(yay.NodeStr(_)) -> Ok(Nil)
    Some(_) -> Error(RemovedKeyError(code, key <> " must be a string"))
  }
}

fn removed_key_code(key: String) -> String {
  let suffix = string.replace(key, each: ".", with: "_")
  "removed_" <> suffix
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
