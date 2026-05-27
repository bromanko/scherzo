import gleam/option.{type Option, None, Some}
import scherzo/config
import scherzo/error
import yay

const removed_remote_commands_message = "remote_commands has been removed; remove this section and use scherzoctl for operator control"

const removed_linear_commands_message = "linear_commands has been removed; remove this section and use scherzoctl for operator control"

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    _ -> None
  }
}

fn definition(front: String) {
  let assert Ok([document]) = yay.parse_string(front)
  yay.document_root(document)
}

fn minimal_front() -> String {
  "tracker:\n  kind: linear\n  project_slug: TEST\n  states:\n    ready: [Todo]\nhooks:\n  before_run: test -d .git\n"
}

pub fn default_config_disables_linear_commands_test() {
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured.linear_commands.enabled == False
  assert configured.linear_commands.prefix == "/scherzo"
  assert configured.linear_commands.authorized_user_ids == []
  assert configured.linear_commands.poll_limit_per_issue == 25
  assert configured.linear_commands.max_comments_per_tick == 50
  assert configured.linear_commands.acknowledge_success == True
  assert configured.linear_commands.acknowledge_rejection == True
}

pub fn remote_commands_section_is_rejected_test() {
  let front =
    minimal_front()
    <> "remote_commands:\n  enabled: true\n  authorized_user_ids:\n    - lin_user_1\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert message == removed_remote_commands_message
}

pub fn legacy_linear_commands_section_is_rejected_test() {
  let front =
    minimal_front()
    <> "linear_commands:\n  enabled: true\n  authorized_user_ids:\n    - lin_user_1\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert message == removed_linear_commands_message
}

pub fn disabled_remote_commands_section_is_rejected_test() {
  let front =
    minimal_front()
    <> "remote_commands:\n  enabled: false\n  prefix: \"   \"\n  authorized_user_ids:\n    - lin_user_1\n  poll_limit_per_issue: 0\n  max_comments_per_tick: 0\n  acknowledge_success: false\n  acknowledge_rejection: false\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert message == removed_remote_commands_message
}

pub fn disabled_legacy_linear_commands_section_is_rejected_test() {
  let front =
    minimal_front()
    <> "linear_commands:\n  enabled: false\n  prefix: \"!legacy\"\n  authorized_user_ids:\n    - lin_legacy\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert message == removed_linear_commands_message
}

pub fn non_map_remote_commands_section_is_rejected_test() {
  let front = minimal_front() <> "remote_commands: false\n"
  let assert Error(error.InvalidConfig(message)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert message == removed_remote_commands_message
}
