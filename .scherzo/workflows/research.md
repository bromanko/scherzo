---
tracker:
  kind: linear
  endpoint: https://api.linear.app/graphql
  api_key: "$LINEAR_API_KEY"
  project_slug: "$LINEAR_PROJECT_SLUG"
  active_states: [Todo, In Progress]
  terminal_states: [Canceled, Duplicate, Done]
polling:
  interval_ms: 30000
workspace:
  # Paths are resolved relative to this workflow file. This lands at
  # repo-root/.scherzo/workspaces/research.
  root: ../workspaces/research
hooks:
  after_create: |
    set -eu
    repo_root=${SCHERZO_REPO_ROOT:-$(cd "$(pwd -P)/../../../.." && pwd -P)}
    sh "$repo_root/scripts/scherzo-jj-workspace" after-create research
  before_run: |
    set -eu
    repo_root=${SCHERZO_REPO_ROOT:-$(cd "$(pwd -P)/../../../.." && pwd -P)}
    sh "$repo_root/scripts/scherzo-jj-workspace" before-run research
  before_remove: |
    set -eu
    repo_root=${SCHERZO_REPO_ROOT:-$(cd "$(pwd -P)/../../../.." && pwd -P)}
    sh "$repo_root/scripts/scherzo-jj-workspace" before-remove research
  timeout_ms: 60000
agent:
  # First dogfood runs should be supervised and serial. Set to 0 to pause new
  # dispatch while preserving workflow reload and running-worker reconciliation.
  max_concurrent_agents: 1
  max_turns: 12
  max_retry_backoff_ms: 300000
  max_retry_attempts: 2
  max_sessions_per_issue: 2
pi:
  command: "pi --mode rpc --no-session"
  turn_timeout_ms: 3600000
  read_timeout_ms: 5000
  stall_timeout_ms: 300000
  auto_retry: true
  # Route extension UI requests to the operator through scherzoctl attach/ui.
  ui_request_policy: operator
  ui_request_timeout_ms: 600000
  compatibility_probe: true
handoff:
  # Comments-only handoff is safe enough for dogfood and keeps state movement in
  # human hands until result comments and state IDs feel proven.
  enabled: true
  comment_on_claim: true
  comment_on_success: true
  comment_on_failure: true
linear_contract:
  enabled: false
  workflow_label_prefix: "workflow:"
  workflow_labels: [research]
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
  enforce_issue_workflow_labels: true
  comment_on_invalid_workflow: false
linear_commands:
  # Keep Linear comment commands off for the first dogfood loop. Use the local
  # control API (`scripts/scherzoctl`) for supervision and follow-up prompts.
  enabled: false
  prefix: "/scherzo"
  authorized_user_ids: []
  poll_limit_per_issue: 25
  max_comments_per_tick: 50
  acknowledge_success: true
  acknowledge_rejection: true
---
You are running Scherzo's checked-in research workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Issue labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Workflow contract:

- This workflow is for `workflow:research` issues only.
- Prefer investigation, evidence gathering, and concise recommendations.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, or switch jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Do not edit files unless the issue explicitly asks for a tiny documentation update or an operator prompt authorizes a change.
- Do not commit, squash, abandon, or otherwise integrate changes.
- If you need operator direction, finish with a clear question or wait for an operator prompt when Scherzo exposes one.

Research process:

1. Restate the question you are answering.
2. Inspect the relevant files and commands with the smallest useful scope.
3. Capture concrete evidence: file paths, commands run, observed output, and uncertainty.
4. Avoid broad refactors, speculative implementation, or unrelated cleanup.
5. If you discover a likely next implementation task, describe it as a follow-up rather than starting it.

Research budget for dogfood runs:

- Prefer a useful answer in one pi turn over exhaustive coverage.
- Use at most 8 tool calls unless an operator prompt explicitly asks for more.
- Do not run the full test suite unless the issue asks for validation; cite existing test status from the operator context when relevant.

Final response format:

## Summary
One short paragraph with the answer.

## Evidence
- Bullet list of the key files, commands, logs, or behavior you inspected.

## Recommendation
- What should happen next.

## Linear update draft
A concise comment that a human or future Scherzo result-comment feature could post back to Linear.
