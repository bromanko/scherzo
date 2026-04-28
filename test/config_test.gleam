import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config
import scherzo/domain
import scherzo/error
import scherzo/workflow

fn env(name: String) -> Option(String) {
  case name {
    "LINEAR_API_KEY" -> Some("linearkey")
    "OTHER_VAR" -> Some("other-secret")
    "WORKSPACE_ROOT" -> Some("test/tmp/env-workspaces")
    "EMPTY" -> None
    _ -> None
  }
}

fn definition(front: String) -> domain.WorkflowDefinition {
  let assert Ok(definition) =
    workflow.parse("---\n" <> front <> "\n---\nPrompt")
  definition
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
  assert pi.compatibility_probe == True
}

pub fn tracker_validation_and_env_resolution_test() {
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/WORKFLOW.md",
      env,
    )
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: github\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/WORKFLOW.md",
      env,
    )
  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/WORKFLOW.md",
      env,
    )

  let assert Error(_) =
    config.resolve_with_env(
      definition(
        "tracker:\n  kind: linear\n  endpoint: http://api.linear.test/graphql\n  project_slug: TEST\nhooks:\n  before_run: test -d .git\n",
      ),
      "test/tmp/WORKFLOW.md",
      env,
    )

  let assert Ok(configured) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/WORKFLOW.md",
      env,
    )
  assert configured.tracker.api_key == Some("linearkey")

  let explicit =
    "tracker:\n  kind: linear\n  project_slug: TEST\n  api_key: \"$OTHER_VAR\"\nhooks:\n  before_run: test -d .git\n"
  let assert Ok(configured_explicit) =
    config.resolve_with_env(definition(explicit), "test/tmp/WORKFLOW.md", env)
  assert configured_explicit.tracker.api_key == Some("other-secret")
}

pub fn path_resolution_and_env_indirection_test() {
  let front = minimal_front() <> "workspace:\n  root: relative-workspaces\n"
  let assert Ok(configured) =
    config.resolve_with_env(
      definition(front),
      "test/tmp/workflows/WORKFLOW.md",
      env,
    )
  assert string.ends_with(
    configured.workspace.root,
    "/test/tmp/workflows/relative-workspaces",
  )

  let front_env = minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT\"\n"
  let assert Ok(configured_env) =
    config.resolve_with_env(definition(front_env), "test/tmp/WORKFLOW.md", env)
  assert string.ends_with(
    configured_env.workspace.root,
    "/test/tmp/env-workspaces",
  )

  let front_inline =
    minimal_front() <> "workspace:\n  root: \"$WORKSPACE_ROOT/sub\"\n"
  let assert Ok(configured_inline) =
    config.resolve_with_env(
      definition(front_inline),
      "test/tmp/WORKFLOW.md",
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
      "test/tmp/WORKFLOW.md",
      env,
    )
  let assert Error(error.DispatchValidationFailed(_)) =
    config.validate_dispatch(no_hooks)
  let assert Ok(prepop) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/WORKFLOW.md",
      env,
    )
  assert prepop.hooks.before_run == Some("test -d .git")

  let paused_front =
    minimal_front()
    <> "agent:\n  max_concurrent_agents: 0\n  max_turns: 1\n  max_retry_attempts: 1\n  max_sessions_per_issue: 1\n  max_concurrent_agents_by_state:\n    todo: 2\n    bad: 0\n"
  let assert Ok(paused) =
    config.resolve_with_env(
      definition(paused_front),
      "test/tmp/WORKFLOW.md",
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
      "test/tmp/WORKFLOW.md",
      env,
    )
}

pub fn pi_validation_and_unknown_keys_ignored_test() {
  let front =
    minimal_front()
    <> "pi:\n  command: \"custom pi --mode rpc\"\n  compatibility_probe: false\nunknown: ignored\n"
  let assert Ok(configured) =
    config.resolve_with_env(definition(front), "test/tmp/WORKFLOW.md", env)
  assert configured.pi.command == "custom pi --mode rpc"
  assert configured.pi.compatibility_probe == False

  let invalid = minimal_front() <> "pi:\n  command: \"\"\n"
  let assert Error(_) =
    config.resolve_with_env(definition(invalid), "test/tmp/WORKFLOW.md", env)
}

pub fn handoff_defaults_and_parsing_test() {
  let assert Ok(defaulted) =
    config.resolve_with_env(
      definition(minimal_front()),
      "test/tmp/WORKFLOW.md",
      env,
    )
  assert defaulted.handoff.enabled == False
  assert defaulted.handoff.comment_on_claim == False

  let comments_only = minimal_front() <> "handoff:\n  enabled: true\n"
  let assert Ok(enabled) =
    config.resolve_with_env(
      definition(comments_only),
      "test/tmp/WORKFLOW.md",
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
      "test/tmp/WORKFLOW.md",
      env,
    )
  assert parsed.handoff.comment_on_failure == False
  assert parsed.handoff.claim_state_id == Some("state-claim")
  assert parsed.handoff.success_state_id == Some("state-success")
  assert parsed.handoff.failure_state_id == Some("state-fail")
}

pub fn reload_state_preserves_last_good_and_blocks_dispatch_test() {
  let state = config.initial_reload_state()
  let good = definition(minimal_front())
  let config.ReloadResult(state: loaded, resolved_secrets: secrets) =
    config.apply_reload(state, good, "test/tmp/WORKFLOW.md", env)
  assert config.can_dispatch(loaded)
  assert secrets == ["linearkey"]
  let assert Some(_) = loaded.last_known_good

  let bad = definition("tracker:\n  kind: linear\n")
  let config.ReloadResult(state: invalid, resolved_secrets: bad_secrets) =
    config.apply_reload(loaded, bad, "test/tmp/WORKFLOW.md", env)
  assert !config.can_dispatch(invalid)
  assert bad_secrets == []
  let assert Some(_) = invalid.last_known_good

  let paused =
    definition(minimal_front() <> "agent:\n  max_concurrent_agents: 0\n")
  let config.ReloadResult(state: reloaded, resolved_secrets: _) =
    config.apply_reload(invalid, paused, "test/tmp/WORKFLOW.md", env)
  assert config.can_dispatch(reloaded)
  let assert Some(effective) = reloaded.last_known_good
  assert effective.agent.max_concurrent_agents == 0
}
