import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/error
import yay

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "LINEAR_PROJECT_SLUG" -> Some("ENV-PROJECT")
    "OTHER_VAR" -> Some("other-secret")
    "WORKSPACE_ROOT" -> Some("test/tmp/env-workspaces")
    "EMPTY" -> None
    _ -> None
  }
}

fn definition(front: String) -> yay.Node {
  let assert Ok([document]) = yay.parse_string(front)
  yay.document_root(document)
}

fn minimal_front() -> String {
  "tracker:\n  kind: linear\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n"
}

pub fn default_values_test() {
  let tracker = config.default_tracker_config()
  assert tracker.endpoint == "https://api.linear.app/graphql"
  assert tracker.active_states == ["Todo", "In Progress"]
  assert tracker.terminal_states
    == ["Closed", "Cancelled", "Canceled", "Duplicate", "Done"]

  let agent = config.default_agent_config()
  assert agent.max_concurrent_agents == 10
  assert agent.max_turns == 20
  assert agent.max_retry_backoff_ms == 300_000
  assert agent.max_retry_attempts == 5
  assert agent.max_sessions_per_issue == 3

  let pi = config.default_pi_config()
  assert pi.command == "pi --mode rpc --no-session"
  assert pi.turn_timeout_ms == 3_600_000
  assert pi.read_timeout_ms == 5000
  assert pi.stall_timeout_ms == 300_000
  assert pi.auto_retry == True
  assert pi.ui_request_policy == domain.Cancel
  assert pi.ui_request_timeout_ms == 300_000
  assert pi.compatibility_probe == True
}

pub fn tracker_validation_and_env_resolution_test() {
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: github\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )

  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\n  endpoint: http://api.linear.test/graphql\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/scherzo.yaml",
      env,
    )

  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured.tracker.api_key == Some("linearkey")

  let env_project =
    "tracker:\n  kind: linear\n  project_slug: \"$LINEAR_PROJECT_SLUG\"\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured_env_project) =
    config.resolve_with_env(
      definition(env_project),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_env_project.tracker.project_slug == Some("ENV-PROJECT")

  let explicit =
    "tracker:\n  kind: linear\n  project_slug: TEST\n  api_key: \"$OTHER_VAR\"\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured_explicit) =
    config.resolve_with_env(definition(explicit), "test/tmp/scherzo.yaml", env)
  assert configured_explicit.tracker.api_key == Some("other-secret")
}

pub fn path_resolution_and_env_indirection_test() {
  let front = minimal_front() <> "workspace:\n  root: relative-workspaces\n"
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(front),
      "test/tmp/workflows/scherzo.yaml",
      env,
    )
  assert string.ends_with(
    configured.workspace.root,
    "/test/tmp/workflows/relative-workspaces",
  )

  let front_env = minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT\"\n"
  let assert Ok(configured_env) =
    config.resolve_with_env(definition(front_env), "test/tmp/scherzo.yaml", env)
  assert string.ends_with(
    configured_env.workspace.root,
    "/test/tmp/env-workspaces",
  )

  let front_inline =
    minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT/sub\"\n"
  let assert Ok(configured_inline) =
    config.resolve_with_env(
      definition(front_inline),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert string.contains(
    configured_inline.workspace.root,
    "$WORKSPACE_ROOT/sub",
  )
}

pub fn hooks_and_agent_limit_validation_test() {
  let assert Ok(no_hooks) =
    config.resolve_with_env(
      definition("tracker:\n  kind: linear\n  project_slug: TEST\n"),
      "test/tmp/scherzo.yaml",
      env,
    )
  let assert Error(error.DispatchValidationFailed(_)) =
    config.validate_dispatch(no_hooks)
  let assert Ok(prepop) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert prepop.hooks.before_run == Some("test -d .git")

  let paused_front =
    minimal_front()
    <> "agent:\n  max_concurrent_agents: 0\n  max_turns: 1\n  max_retry_attempts: 1\n  max_sessions_per_issue: 1\n  max_concurrent_agents_by_state:\n    todo: 2\n    bad: 0\n"
  let assert Ok(paused) =
    config.resolve_with_env(
      definition(paused_front),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert paused.agent.max_concurrent_agents == 0
  assert dict.get(paused.agent.max_concurrent_agents_by_state, "todo") == Ok(2)
  assert dict.get(paused.agent.max_concurrent_agents_by_state, "bad")
    == Error(Nil)

  let invalid_front = minimal_front() <> "agent:\n  max_concurrent_agents: -1\n"
  let assert Error(_) =
    config.resolve_with_env(
      definition(invalid_front),
      "test/tmp/scherzo.yaml",
      env,
    )
}

pub fn pi_validation_and_unknown_keys_ignored_test() {
  let front =
    minimal_front()
    <> "pi:\n  command: \"custom pi --mode rpc\"\n  compatibility_probe: false\nunknown: ignored\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.pi.command == "custom pi --mode rpc"
  assert configured.pi.compatibility_probe == False

  let operator_policy =
    minimal_front()
    <> "pi:\n  ui_request_policy: operator\n  ui_request_timeout_ms: 1234\n"
  let assert Ok(configured_operator_policy) =
    config.resolve_with_env(
      definition(operator_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_operator_policy.pi.ui_request_policy == domain.Operator
  assert configured_operator_policy.pi.ui_request_timeout_ms == 1234

  let explicit_timeout =
    minimal_front() <> "pi:\n  ui_request_timeout_ms: 1234\n"
  let assert Ok(configured_timeout) =
    config.resolve_with_env(
      definition(explicit_timeout),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_timeout.pi.ui_request_timeout_ms == 1234

  let fail_policy = minimal_front() <> "pi:\n  ui_request_policy: fail\n"
  let assert Ok(configured_fail_policy) =
    config.resolve_with_env(
      definition(fail_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_fail_policy.pi.ui_request_policy == domain.Fail

  let ignore_policy = minimal_front() <> "pi:\n  ui_request_policy: ignore\n"
  let assert Ok(configured_ignore_policy) =
    config.resolve_with_env(
      definition(ignore_policy),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_ignore_policy.pi.ui_request_policy == domain.Ignore

  let invalid_policy = minimal_front() <> "pi:\n  ui_request_policy: surprise\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_policy),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_timeout = minimal_front() <> "pi:\n  ui_request_timeout_ms: 0\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_timeout),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid = minimal_front() <> "pi:\n  command: \"\"\n"
  let assert Error(_) =
    config.resolve_with_env(definition(invalid), "test/tmp/scherzo.yaml", env)
}

pub fn handoff_defaults_and_parsing_test() {
  let assert Ok(defaulted) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert defaulted.handoff.enabled == False
  assert defaulted.handoff.comment_on_claim == False

  let comments_only = minimal_front() <> "handoff:\n  enabled: true\n"
  let assert Ok(enabled) =
    config.resolve_with_env(
      definition(comments_only),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert enabled.handoff.enabled == True
  assert enabled.handoff.comment_on_claim == True
  assert enabled.handoff.comment_on_success == True
  assert enabled.handoff.comment_on_failure == True

  let with_states =
    minimal_front()
    <> "handoff:\n  enabled: true\n  comment_on_failure: false\n  claim_state_id: state-claim\n  success_state_id: state-success\n  failure_state_id: state-fail\n"
  let assert Ok(parsed) =
    config.resolve_with_env(
      definition(with_states),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert parsed.handoff.comment_on_failure == False
  assert parsed.handoff.claim_state_id == Some("state-claim")
  assert parsed.handoff.success_state_id == Some("state-success")
  assert parsed.handoff.failure_state_id == Some("state-fail")
}

pub fn handoff_result_defaults_follow_success_comments_test() {
  let front = minimal_front() <> "handoff:\n  enabled: true\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.handoff.comment_on_success == True
  assert configured.handoff.include_result_on_success == True
  assert configured.handoff.result_max_chars == 8000
}

pub fn handoff_can_disable_result_in_success_comment_test() {
  let front =
    minimal_front()
    <> "handoff:\n  enabled: true\n  include_result_on_success: false\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.handoff.comment_on_success == True
  assert configured.handoff.include_result_on_success == False
}

pub fn handoff_result_max_chars_must_be_positive_test() {
  let front =
    minimal_front() <> "handoff:\n  enabled: true\n  result_max_chars: 0\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
}

pub fn linear_contract_defaults_test() {
  let defaults = config.default_linear_contract_config()
  assert defaults.enabled == False
  assert defaults.workflow_label_prefix == "workflow:"
  assert defaults.workflow_labels == []
  assert defaults.support_labels == []
  assert dict.to_list(defaults.required_states) == []
  assert dict.to_list(defaults.handoff_state_bindings) == []
  assert defaults.enforce_issue_workflow_labels == False
  assert defaults.invalid_workflow_state_id == None
  assert defaults.comment_on_invalid_workflow == False

  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured.linear_contract == defaults
}

pub fn linear_contract_parses_and_normalizes_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  enabled: true\n  workflow_label_prefix: \" Workflow: \"\n  workflow_labels: [Bugfix, \" bugfix \", Research, \"\"]\n  support_labels: [Needs-Workflow, \" needs-workflow \", Needs-Clarification]\n  required_states:\n    Ready: \"Ready for Agent\"\n    in_progress: \" In Progress \"\n    done: Done\n  handoff_state_bindings:\n    claim: IN_PROGRESS\n    success: done\n  enforce_issue_workflow_labels: true\n  invalid_workflow_state_id: \" state-needs-workflow \"\n  comment_on_invalid_workflow: true\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  let contract = configured.linear_contract
  assert contract.enabled == True
  assert contract.workflow_label_prefix == "workflow:"
  assert contract.workflow_labels == ["bugfix", "research"]
  assert contract.support_labels == ["needs-workflow", "needs-clarification"]
  assert dict.get(contract.required_states, "ready") == Ok("Ready for Agent")
  assert dict.get(contract.required_states, "in_progress") == Ok("In Progress")
  assert dict.get(contract.handoff_state_bindings, "claim") == Ok("in_progress")
  assert dict.get(contract.handoff_state_bindings, "success") == Ok("done")
  assert contract.enforce_issue_workflow_labels == True
  assert contract.invalid_workflow_state_id == Some("state-needs-workflow")
  assert contract.comment_on_invalid_workflow == True
}

pub fn linear_contract_optional_dispatch_policy_defaults_test() {
  let front =
    minimal_front()
    <> "linear_contract:\n  workflow_labels: []\n  invalid_workflow_state_id: null\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/scherzo.yaml", env)
  assert configured.linear_contract.enforce_issue_workflow_labels == False
  assert configured.linear_contract.workflow_labels == []
  assert configured.linear_contract.invalid_workflow_state_id == None
  assert configured.linear_contract.comment_on_invalid_workflow == False

  let blank_state_id =
    minimal_front()
    <> "linear_contract:\n  invalid_workflow_state_id: \"   \"\n"
  let assert Ok(configured_blank) =
    config.resolve_with_env(
      definition(blank_state_id),
      "test/tmp/scherzo.yaml",
      env,
    )
  assert configured_blank.linear_contract.invalid_workflow_state_id == None
}

pub fn linear_contract_rejects_invalid_values_test() {
  let empty_prefix =
    minimal_front()
    <> "linear_contract:\n  enabled: true\n  workflow_label_prefix: \"  \"\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(empty_prefix),
      "test/tmp/scherzo.yaml",
      env,
    )

  let enforcement_without_labels =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: true\n  workflow_labels: []\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(enforcement_without_labels),
      "test/tmp/scherzo.yaml",
      env,
    )

  let enforcement_empty_prefix =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: true\n  workflow_label_prefix: \"  \"\n  workflow_labels: [bugfix]\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(enforcement_empty_prefix),
      "test/tmp/scherzo.yaml",
      env,
    )

  let invalid_bool =
    minimal_front()
    <> "linear_contract:\n  enforce_issue_workflow_labels: yes\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(invalid_bool),
      "test/tmp/scherzo.yaml",
      env,
    )

  let unknown_binding_key =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    done: Done\n  handoff_state_bindings:\n    surprise: done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(unknown_binding_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let missing_binding_target =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    done: Done\n  handoff_state_bindings:\n    success: closed\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(missing_binding_target),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_string_list_entry =
    minimal_front() <> "linear_contract:\n  workflow_labels: [bugfix, 123]\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_list_entry),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_string_map_key =
    minimal_front() <> "linear_contract:\n  required_states:\n    123: Done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_map_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_string_map_value =
    minimal_front() <> "linear_contract:\n  required_states:\n    ready: 123\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_string_map_value),
      "test/tmp/scherzo.yaml",
      env,
    )

  let blank_map_key =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    \"  \": Done\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(blank_map_key),
      "test/tmp/scherzo.yaml",
      env,
    )

  let blank_map_value =
    minimal_front()
    <> "linear_contract:\n  required_states:\n    ready: \"  \"\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(blank_map_value),
      "test/tmp/scherzo.yaml",
      env,
    )

  let non_map_section = minimal_front() <> "linear_contract: true\n"
  let assert Error(error.InvalidConfig(_)) =
    config.resolve_with_env(
      definition(non_map_section),
      "test/tmp/scherzo.yaml",
      env,
    )
}

pub fn reload_state_preserves_last_good_and_blocks_dispatch_test() {
  let state = config.initial_reload_state()
  let good = definition(minimal_front())
  let config.ReloadResult(state: loaded, resolved_secrets: secrets) =
    config.apply_reload(state, good, "test/tmp/scherzo.yaml", env)
  assert config.can_dispatch(loaded)
  assert secrets == ["linearkey"]
  let assert Some(_) = loaded.last_known_good

  let bad = definition("tracker:\n  kind: linear\n")
  let config.ReloadResult(state: invalid, resolved_secrets: bad_secrets) =
    config.apply_reload(loaded, bad, "test/tmp/scherzo.yaml", env)
  assert !config.can_dispatch(invalid)
  assert bad_secrets == []
  let assert Some(_) = invalid.last_known_good

  let paused =
    definition(minimal_front() <> "agent:\n  max_concurrent_agents: 0\n")
  let config.ReloadResult(state: reloaded, resolved_secrets: _) =
    config.apply_reload(invalid, paused, "test/tmp/scherzo.yaml", env)
  assert config.can_dispatch(reloaded)
  let assert Some(effective) = reloaded.last_known_good
  assert effective.agent.max_concurrent_agents == 0
}
