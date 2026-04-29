---
tracker:
  kind: linear
  endpoint: https://api.linear.app/graphql
  api_key: "$LINEAR_API_KEY"
  project_slug: YOUR_PROJECT_SLUG
  active_states: [Todo, In Progress]
  terminal_states: [Closed, Cancelled, Canceled, Duplicate, Done]
polling:
  interval_ms: 30000
workspace:
  root: .scherzo/workspaces
hooks:
  after_create: |
    test -n "$REPO_URL"
    git clone "$REPO_URL" .
    test -d .git
  before_run: |
    test -d .git
    git status --short
  timeout_ms: 60000
agent:
  # Keep this conservative for first real-board runs. Set to 0 to pause new
  # dispatch while the daemon still reloads the workflow and reconciles workers.
  max_concurrent_agents: 1
  max_turns: 20
  max_retry_backoff_ms: 300000
  max_retry_attempts: 5
  max_sessions_per_issue: 3
pi:
  command: "pi --mode rpc --no-session"
  turn_timeout_ms: 3600000
  read_timeout_ms: 5000
  stall_timeout_ms: 300000
  auto_retry: true
  ui_request_policy: cancel
  compatibility_probe: true
# Optional Linear-visible handoff. Disabled is safest for smoke/probe and first
# fake-pi daemon tests. When enabled, comments include a Scherzo run ID.
# State IDs are Linear workflow state IDs, not state names. If claim_state_id
# moves an issue to In Progress, include In Progress in tracker.active_states or
# omit claim_state_id and use comments-only claims plus success_state_id.
handoff:
  enabled: false
  comment_on_claim: true
  comment_on_success: true
  comment_on_failure: true
  # claim_state_id: "linear-state-id-for-in-progress"
  # success_state_id: "linear-state-id-for-done"
  # failure_state_id: "linear-state-id-for-needs-attention"
# Optional board contract and workflow-label dispatch policy. Run:
#   direnv exec . gleam run -- --linear-contract-check examples/WORKFLOW.md
# before enabling workflow-label enforcement, invalid-workflow state moves, or
# handoff state updates. The check queries Linear metadata only; it does not
# create labels, states, comments, or issue updates.
linear_contract:
  enabled: false
  workflow_label_prefix: "workflow:"
  workflow_labels: [bugfix, feature, research, review, docs, chore]
  support_labels: [needs-workflow, needs-clarification]
  required_states:
    todo: Todo
    in_progress: In Progress
    done: Done
    needs_workflow: Needs Workflow
  handoff_state_bindings:
    claim: in_progress
    success: done
    failure: needs_workflow
  # When true, Scherzo skips issues unless they have exactly one allowed
  # workflow label such as workflow:bugfix. Enforcement alone is log-only.
  enforce_issue_workflow_labels: false
  # Uncomment to post a concise triage comment for invalid workflow labels.
  comment_on_invalid_workflow: false
  # Uncomment to move invalid workflow issues to Needs Workflow. This must be a
  # Linear workflow state ID, not the state name, and should be verified with
  # --linear-contract-check before use.
  # invalid_workflow_state_id: "linear-state-id-for-needs-workflow"
# Optional Linear comment command transport. Keep disabled until you have a
# private test issue and the Linear user id for each authorized operator.
linear_commands:
  enabled: false
  prefix: "/scherzo"
  authorized_user_ids: []
  poll_limit_per_issue: 25
  max_comments_per_tick: 50
  acknowledge_success: true
  acknowledge_rejection: true
---
You are working on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Description:
{{ issue.description }}

Labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Follow the issue's workflow label when choosing how to work:
- workflow:bugfix means make a minimal corrective change and include a regression test when practical.
- workflow:feature means implement the requested behavior with focused tests.
- workflow:research means investigate and report findings before broad code changes.
- workflow:review means inspect the requested code or plan and provide review findings.
- workflow:docs means update documentation or examples.
- workflow:chore means perform maintenance without widening product behavior.

If Scherzo handoff is enabled, the daemon may add Linear comments or move the issue to configured Linear state IDs after this run. If handoff is disabled, use repository or workflow-provided tooling to add comments, update status, or link pull requests when the task requires handoff.
