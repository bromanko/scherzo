import gleam/option.{type Option, None, Some}
import scherzo/config
import scherzo/error
import yay

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
  "tracker:\n  kind: linear\n  project_slug: TEST\n  dispatch_states: [Todo]\nhooks:\n  before_run: test -d .git\n"
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

pub fn enabled_config_requires_authorized_linear_user_ids_test() {
  let front =
    minimal_front()
    <> "linear_commands:\n  enabled: true\n  authorized_user_ids: []\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
}

pub fn parses_custom_prefix_and_trims_authorized_user_ids_test() {
  let front =
    minimal_front()
    <> "remote_commands:\n  enabled: true\n  prefix: \"!s\"\n  authorized_user_ids:\n    - \" lin_user_1 \"\n    - \"\"\n    - lin_user_2\n  poll_limit_per_issue: 7\n  max_comments_per_tick: 8\n  acknowledge_success: false\n  acknowledge_rejection: true\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.linear_commands.enabled == True
  assert configured.linear_commands.prefix == "!s"
  assert configured.linear_commands.authorized_user_ids
    == ["lin_user_1", "lin_user_2"]
  assert configured.linear_commands.poll_limit_per_issue == 7
  assert configured.linear_commands.max_comments_per_tick == 8
  assert configured.linear_commands.acknowledge_success == False
  assert configured.linear_commands.acknowledge_rejection == True
}

pub fn legacy_linear_commands_alias_still_parses_test() {
  let front =
    minimal_front()
    <> "linear_commands:\n  enabled: true\n  authorized_user_ids:\n    - lin_user_1\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.linear_commands.enabled == True
  assert configured.linear_commands.authorized_user_ids == ["lin_user_1"]
}

pub fn remote_commands_takes_precedence_over_legacy_alias_test() {
  let front =
    minimal_front()
    <> "linear_commands:\n  enabled: true\n  prefix: \"!legacy\"\n  authorized_user_ids:\n    - lin_legacy\nremote_commands:\n  enabled: true\n  prefix: \"!remote\"\n  authorized_user_ids:\n    - remote_user\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)

  assert configured.linear_commands.prefix == "!remote"
  assert configured.linear_commands.authorized_user_ids == ["remote_user"]
}

pub fn rejects_invalid_linear_command_limits_and_prefix_test() {
  let invalid_prefix =
    minimal_front() <> "remote_commands:\n  prefix: \"   \"\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_prefix),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_poll_limit =
    minimal_front() <> "remote_commands:\n  poll_limit_per_issue: 0\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_poll_limit),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_tick_limit =
    minimal_front() <> "remote_commands:\n  max_comments_per_tick: 0\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_tick_limit),
      "test/tmp/scherzo.yaml",
      env,
    )
}
