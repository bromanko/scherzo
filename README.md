# Scherzo

Scherzo is a Gleam/Erlang daemon that polls one Linear project, prepares one workspace per issue, and runs a pi coding-agent session in that workspace using pi RPC mode.

The current implementation is ready for cautious use against one real Linear board from one Scherzo instance and one canonical workspace root. It includes reusable real Linear HTTPS reads, bounded smoke checks, a long-lived daemon actor with poll and retry timers, monitored pi workers, workflow reload by file contents, no-prompt pi probing, optional Linear handoff comments/state updates, Linear command comments, an authenticated local control API, `scherzoctl`, and a local instance lock. It is not a distributed job system: do not run multiple hosts or multiple independent workspace roots against the same Linear project until a durable claim backend exists.

## Development

Install Nix, `devenv`, and `direnv` on the host. Then run:

    direnv allow
    direnv exec . gleam test

Useful validation commands are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help
    direnv exec . gleam run -- ctl --help

## CLI modes

With no mode flag, Scherzo runs daemon mode and keeps polling until the Erlang VM process is terminated:

    direnv exec . gleam run -- path/to/WORKFLOW.md

Use `--once` to run one deterministic poll/dispatch tick and exit:

    direnv exec . gleam run -- --once path/to/WORKFLOW.md

Use `--linear-smoke` before dispatching on a real board. It resolves Linear credentials and performs bounded read-only Linear API calls. It does not prepare workspaces, require dispatch hooks, acquire the instance lock, run pi, or send a prompt:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke path/to/WORKFLOW.md

Use `--linear-contract-check` to compare the local workflow state and label contract to the configured Linear project board. It is read-only: it queries project teams, workflow states, team labels, and workspace labels, but does not create labels, create states, update issues, add comments, prepare workspaces, acquire the instance lock, run hooks, or launch pi:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check path/to/WORKFLOW.md

Use `--pi-probe` before allowing a real prompt. It validates dispatch hooks, acquires the local instance lock, prepares a scratch workspace named from `SCHERZO-PROBE`, launches pi RPC, runs `set_session_name`, `set_auto_retry`, `get_state`, and `get_session_stats`, then terminates pi without sending `prompt`:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe path/to/WORKFLOW.md

Use `ctl` through `direnv exec . gleam run -- ctl ...` or the development wrapper `scripts/scherzoctl` to inspect or control a running daemon through the local authenticated control API. See the `Local control API and scherzoctl` section below.

## Runtime requirements

Runtime operation requires:

- `LINEAR_API_KEY` or `tracker.api_key` in the workflow.
- A Linear project slug in `tracker.project_slug`; a single `$ENV_VAR` reference such as `"$LINEAR_PROJECT_SLUG"` is resolved from the environment.
- A `pi` executable that supports JSON Lines RPC mode through `pi --mode rpc`.
- A trusted workspace population or verification hook. The example uses `REPO_URL` with `git clone "$REPO_URL" .` in `hooks.after_create`.

Scherzo acquires a local lock at `workspace.root/.scherzo-state/instance.lock` for `--once`, `--pi-probe`, and daemon mode. The lock prevents another Scherzo process using the same canonical workspace root on the same filesystem from starting normally. It is not a distributed lock and does not protect different hosts or different workspace roots.

If startup reports an existing instance lock, first verify that no Scherzo process is still running with that workspace root. Only then remove the stale `instance.lock` file manually and restart.

## Workflow file convention

Reusable, checked-in workflow definitions should live under `.scherzo/workflows/*.md`. Runtime workspaces and local state should stay under ignored `.scherzo/workspaces/<workflow-name>/` roots. Relative paths are resolved from the workflow file directory, so a checked-in workflow under `.scherzo/workflows/` should use `workspace.root: ../workspaces/<workflow-name>`. Machine-specific variants can use `.scherzo/workflows/*.local.md`, which is ignored by git.

This repository includes `.scherzo/workflows/research.md` as the first dogfood workflow. It uses `LINEAR_API_KEY` and `LINEAR_PROJECT_SLUG`, creates per-issue jj workspaces with `jj workspace add` instead of separate git clones, enforces exactly one `workflow:research` label, enables comments-only handoff, and leaves Linear comment commands disabled so the first runs can be supervised through `scripts/scherzoctl`.

## Workflow schema

`WORKFLOW.md` is Markdown with optional YAML front matter. Unknown top-level keys are ignored. The core keys are:

    tracker:
      kind: linear
      endpoint: https://api.linear.app/graphql
      api_key: "$LINEAR_API_KEY"
      project_slug: "$LINEAR_PROJECT_SLUG"
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
      after_run: |
        echo done
      before_remove: |
        echo removing
      timeout_ms: 60000
    agent:
      max_concurrent_agents: 1
      max_turns: 20
      max_retry_backoff_ms: 300000
      max_retry_attempts: 5
      max_sessions_per_issue: 3
      max_concurrent_agents_by_state:
        todo: 1
    pi:
      command: "pi --mode rpc --no-session"
      turn_timeout_ms: 3600000
      read_timeout_ms: 5000
      stall_timeout_ms: 300000
      auto_retry: true
      ui_request_policy: cancel
      ui_request_timeout_ms: 300000
      compatibility_probe: true
    handoff:
      enabled: false
      comment_on_claim: true
      comment_on_success: true
      comment_on_failure: true
      claim_state_id: null
      success_state_id: null
      failure_state_id: null
    linear_contract:
      enabled: false
      workflow_label_prefix: "workflow:"
      workflow_labels: []
      support_labels: []
      required_states: {}
      handoff_state_bindings: {}
      enforce_issue_workflow_labels: false
      invalid_workflow_state_id: null
      comment_on_invalid_workflow: false
    linear_commands:
      enabled: false
      prefix: "/scherzo"
      authorized_user_ids: []
      poll_limit_per_issue: 25
      max_comments_per_tick: 50
      acknowledge_success: true
      acknowledge_rejection: true

See `examples/WORKFLOW.md` for a runnable template.

## Linear board contract check

The board contract check proves that the configured Linear project contains the state names and issue labels the local workflow expects before agents start work. Run it before enabling stricter workflow labels or handoff state updates:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check path/to/WORKFLOW.md

The check always validates `tracker.active_states` and `tracker.terminal_states` for every Linear team associated with `tracker.project_slug`, because those states control candidate reads and terminal reconciliation. If `linear_contract.enabled: true`, it also validates explicit `linear_contract.required_states`, workflow labels formed from `workflow_label_prefix` plus each `workflow_labels` suffix, and full `support_labels`. If `linear_contract.enforce_issue_workflow_labels: true`, the checker validates the workflow labels even when the broader board contract is disabled; support labels still belong to the broader board contract and are checked only when `enabled: true`. A required label passes when it is assignable to issues in every project team, either as a team label or as a workspace-level issue label.

A typical contract section looks like this:

    linear_contract:
      enabled: true
      workflow_label_prefix: "workflow:"
      workflow_labels: [bugfix, feature, research, review, docs, chore]
      support_labels: [needs-workflow, needs-clarification]
      required_states:
        ready: "Ready for Agent"
        in_progress: "In Progress"
        done: "Done"
        needs_workflow: "Needs Workflow"
      handoff_state_bindings:
        claim: in_progress
        success: done
        failure: needs_workflow
      enforce_issue_workflow_labels: true
      invalid_workflow_state_id: "linear-state-id-for-needs-workflow"
      comment_on_invalid_workflow: true

`handoff_state_bindings` optionally ties configured handoff state IDs to named required states. For example, `success: done` means `handoff.success_state_id` must exist on the single checked team and point to a Linear state named `Done`. Linear workflow state IDs are team-scoped, so a multi-team project with enabled global handoff state IDs currently fails closed with `multi_team_handoff_state_unsupported` until the workflow disables handoff mutations or a future per-team handoff configuration exists. When `invalid_workflow_state_id` is configured, the checker also verifies that the ID exists on a single-team project; if `required_states.needs_workflow` is configured, the ID must point to a state with that name.

This is detection-only. Scherzo does not create, rename, delete, or migrate Linear labels or workflow states.

## Linear workflow-label dispatch policy

By default, existing deployments dispatch exactly as before. To require every dispatched issue to carry exactly one explicit workflow label, set `linear_contract.enforce_issue_workflow_labels: true` and configure at least one allowed suffix in `workflow_labels`. For a prefix of `workflow:`, an issue with `workflow:bugfix` dispatches as the `bugfix` workflow. An issue with no `workflow:*` label, more than one `workflow:*` label, or an unconfigured `workflow:*` label is skipped before workspace preparation, handoff claim, or pi launch.

Invalid workflow issues are per-issue scheduler input errors, not agent failures. Enforcement alone is log-only and does not mutate Linear. Set `comment_on_invalid_workflow: true` to post a concise Linear comment explaining the expected labels. Set `invalid_workflow_state_id` to additionally move invalid issues to a triage state such as `Needs Workflow`. Comments and state movement are separate opt-ins so operators can roll out safely: first create the labels and state in Linear, run `--linear-contract-check`, enable enforcement without mutations, then enable comments and state moves once the diagnostics are clean.

## Linear handoff

Handoff is disabled by default. When `handoff.enabled: true`, Scherzo can add Linear comments and optionally update issue state by configured Linear state IDs. Comments include the daemon run ID so repeated comments after a crash or retry can be correlated with logs.

Start with comments only:

    handoff:
      enabled: true
      comment_on_claim: true
      comment_on_success: true
      comment_on_failure: true

State transitions are optional and must use Linear state IDs, not state names:

    handoff:
      enabled: true
      claim_state_id: "workflow-state-id-for-in-progress"
      success_state_id: "workflow-state-id-for-done"
      failure_state_id: "workflow-state-id-for-needs-attention"

If `claim_state_id` moves the issue to a state such as `In Progress`, include that state in `tracker.active_states` for workflows whose workers may run longer than one poll interval. The daemon reconciles running issues on each poll and stops a worker if Linear reports the issue in a non-active, non-terminal state. If you want Scherzo to only select new work from `Todo`, leave `claim_state_id` unset and use comments-only claim handoff plus a `success_state_id`.

A failed claim comment or claim state update prevents dispatch for that tick because the operator-visible claim did not land. A failed success or failure report is logged but does not rerun the worker.

## Workspace population contract

Scherzo creates deterministic per-issue directories under `workspace.root`, but it does not know how to check out project code. A dispatchable workflow must include either:

- `hooks.after_create`, normally a population hook such as `git clone "$REPO_URL" .`; or
- `hooks.before_run`, an explicit verification hook for pre-populated workspaces, such as `test -d .git`.

Hooks are trusted shell scripts run with the issue workspace as cwd. Failed `after_create` on a newly-created workspace is cleaned up before retry when possible. If cleanup cannot finish, Scherzo uses a sidecar marker under `.scherzo-state/<key>.populating` to avoid treating partial workspaces as prepared.

Set `agent.max_concurrent_agents: 0` to pause new dispatch. The daemon still reloads the workflow and reconciles already-running issues.

## Prompt template inputs

The prompt body receives:

- `issue.id`
- `issue.identifier`
- `issue.title`
- `issue.description`
- `issue.priority`
- `issue.state`
- `issue.branch_name`
- `issue.url`
- `issue.labels`
- `attempt`

The renderer supports `{{ variable }}`, `{% if variable %}...{% else %}...{% endif %}`, and `{% for label in issue.labels %}...{% endfor %}`. Unknown variables, unknown tags, malformed blocks, and unknown filters such as `{{ issue.title | upcase }}` fail the attempt instead of rendering silently.

## Daemon behavior and shutdown

Daemon mode owns the in-memory runtime state. It polls Linear on `polling.interval_ms`, reconciles running issues, dispatches eligible candidates, schedules retry timers for failed workers and active continuations, reloads `WORKFLOW.md` when file contents change, and blocks new candidate/retry dispatch while the current workflow contents are invalid. Running worker reconciliation continues using the last known good config.

Workers are monitored. If a worker exits without sending a normal result, the daemon handles the monitor message and schedules failure retry or parking through the same pure core path as ordinary worker failure. pi updates are routed through the daemon and logged as `pi_event` with redaction.

Programmatic `daemon.shutdown` cancels poll/retry timers and stops daemon-owned workers. Daemon CLI mode installs a SIGTERM handler while the daemon is running; process-manager SIGTERM now logs `daemon_stop_requested reason=sigterm`, calls `daemon.shutdown`, stops the local control server, removes `workspace.root/.scherzo-state/control.json`, releases `workspace.root/.scherzo-state/instance.lock`, and exits after `daemon_shutdown_complete`. Ctrl-C/SIGINT is still not a graceful path in this runtime phase, and `kill -9`, host power loss, or a BEAM VM crash can still leave stale local state. Remove a stale `instance.lock` manually only after checking no Scherzo process remains active.

## Session event model

Daemon mode also starts an in-memory EventHub for internal session visibility. The hub records one session summary per live worker attempt and keeps a bounded buffer of recent events for that session, including lifecycle transitions, pi event names, assistant message deltas, UI request metadata, token totals, and worker exit events. The daemon and worker hot paths publish to the hub with fire-and-forget actor messages, while tests and future control clients can query summaries and replay events by cursor.

Event history is not durable across daemon restart. Raw pi JSON payloads are retained only after recursive secret redaction and a 16 KiB per-event cap, so replay is useful for control clients and future renderers without treating the event buffer as an audit log.

## Local control API and scherzoctl

Daemon mode starts a small authenticated control server on `127.0.0.1` after the daemon actor and EventHub are available. The server uses line-delimited JSON over loopback TCP, chooses an OS-assigned port, generates a fresh token for each daemon start, and writes connection details to `workspace.root/.scherzo-state/control.json`. Graceful SIGTERM shutdown removes this control file through the same daemon shutdown path used by programmatic tests, so a normal process-manager restart should not require manual control-file cleanup. The daemon logs the control file path and port with an event like:

    level=info service=scherzo event=control_server_started control_file=... host=127.0.0.1 port=54321

The token is stored in the control file and is required on every request, but it is not logged. The control file has this shape:

    {"version":1,"host":"127.0.0.1","port":54321,"token":"...","workspace_root":".scherzo/workspaces","started_at_ms":42}

Use the logged path explicitly, or export it for repeated commands:

    export SCHERZO_CONTROL_FILE=<logged-control-file>
    scripts/scherzoctl ping
    scripts/scherzoctl ps
    scripts/scherzoctl events <session-id>
    scripts/scherzoctl attach <session-id>

`attach` replays retained events and then follows new events with a human-readable terminal renderer. It groups output by turn, streams assistant deltas as continuous text, shows tool activity and blocking UI requests distinctly, and prints token summaries when they are available. Use `--no-follow` for replay only, `--since-cursor <n>` to resume after a known cursor, and `--color=auto|always|never` to control ANSI styling:

    scripts/scherzoctl attach --no-follow <session-id>
    scripts/scherzoctl attach --since-cursor 40 <session-id>

Example pretty output:

    ABC-123 Fix flaky tests
    workspace: /workspaces/ABC-123
    session: ABC-123-42-1
    status: running

    ▶ turn 1 started
    assistant:
      I will run the tests and inspect the failure.
    tool bash
      input: gleam test
      output: 2 failures
    tokens: input=1200 output=340 cache_read=0 cache_write=0 total=1540

For compatibility and automation, raw and JSON modes remain available. `events` stays compact by default, while `events --pretty` provides paginated human-readable replay without following:

    scripts/scherzoctl attach --raw <session-id>
    scripts/scherzoctl attach --json <session-id>
    scripts/scherzoctl events --pretty <session-id>

Every command also accepts `--control-file <path>`. Non-streaming commands accept `--json` for automation:

    scripts/scherzoctl ps --json
    scripts/scherzoctl session <session-id> --json
    scripts/scherzoctl events <session-id> --json

The control API is local-only and token-authenticated. Read-only requests can list session summaries, fetch one session, replay retained events, and follow events with `attach`. Mutating requests decode into the shared `control/command.OperatorCommand` model before reaching the daemon, so future transports such as Linear comment commands reuse the same command semantics instead of implementing separate scheduler mutations. Valid command-level rejections are returned as successful protocol responses with `ok: true` and a command `status` such as `rejected`, `not_found`, or `not_allowed`; malformed requests, wrong tokens, connection failures, and command backend timeouts return `ok: false`.

Available local mutating commands include:

    scripts/scherzoctl pause
    scripts/scherzoctl resume
    scripts/scherzoctl reload
    scripts/scherzoctl retry ABC-123
    scripts/scherzoctl park ABC-123 --reason "manual cleanup" --yes
    scripts/scherzoctl unpark ABC-123
    scripts/scherzoctl abort <session-id> --yes
    scripts/scherzoctl stop-after-turn <session-id> --yes
    scripts/scherzoctl prompt <session-id> "summarize progress"
    scripts/scherzoctl ui respond <session-id> ui-1 --cancel
    scripts/scherzoctl ui respond <session-id> ui-1 --value ok

`pause` is runtime-only and blocks new dispatch while allowing reconciliation, cleanup, and shutdown to continue. `park`, `abort`, and `stop-after-turn` require `--yes` because they are destructive safety controls. `park` also requires `--reason <text>`. `retry` rejects running, claimed, or pending-claim issues to avoid duplicate work; when accepted, it explicitly releases any existing park for that issue before attempting dispatch.

Live workers are command-aware. `prompt <session-id> ...` sends the prompt to the worker that owns the pi RPC process; during an active turn the prompt is acknowledged as `queued` and becomes the next turn's pi `prompt` instead of interrupting the current stream. Each worker keeps at most 10 queued operator prompts and rejects additional prompts with `prompt_queue_full`. If a worker exits before using queued prompts, it emits `operator_prompt_dropped` session events with redacted, truncated prompt text.

`abort <session-id> --yes` first asks the worker to send pi's graceful `abort` RPC command. If the worker command subject is unavailable or does not acknowledge before the local command timeout, the daemon falls back to the older kill-and-park safety path and still parks the issue with reason `operator_abort`. Abort-created parks are explicit safety parks: Scherzo will not work the issue again until an operator runs `unpark` or `retry`. `stop-after-turn <session-id> --yes` is non-destructive: it is routed to the worker and returns `rejected(worker_command_timeout)` or `not_allowed(worker_command_subject_unavailable)` if the worker cannot acknowledge it, because killing the process would be abort semantics rather than stop-after-current-turn semantics.

For extension UI requests, the existing `pi.ui_request_policy` values `cancel`, `fail`, and `ignore` remain available. When `pi.ui_request_policy: operator`, a blocking pi UI request puts the session into `waiting_ui`; `scripts/scherzoctl ui respond <session-id> <request-id> --cancel` or `--value <text>` sends `extension_ui_response` through the worker-owned pi RPC session. If no operator responds before `pi.ui_request_timeout_ms`, the worker sends a cancel response, emits `operator_ui_timeout`, and resumes the turn. The ordinary stall timeout is paused while pi is intentionally waiting for operator UI input.

The backing EventHub data, queued prompts, pending UI requests, and runtime pause/park state are in memory and disappear on daemon restart. A stale control file after a crash is recoverable: `scherzoctl ping` fails cleanly, and operators should restart the daemon and use the newly logged control file path.

## Linear command comments

Linear command comments are disabled by default. When `linear_commands.enabled: true`, Scherzo polls comments only on issues it is already observing in the current daemon tick: running issues, retrying issues, parked issues, and candidate issues fetched from the configured active states. It does not scan the whole Linear project, historical terminal issues, or unrelated issues.

Enable the transport with an explicit Linear user-id allowlist:

    linear_commands:
      enabled: true
      prefix: "/scherzo"
      authorized_user_ids:
        - lin_user_123
      poll_limit_per_issue: 25
      max_comments_per_tick: 50
      acknowledge_success: true
      acknowledge_rejection: true

Authorization is by Linear user id only. Matching email addresses or display names do not authorize commands. Scherzo records command-like comment ids in memory after their first terminal outcome, including malformed and unauthorized commands, so the same comment is not executed or acknowledged repeatedly during one daemon run. Edits to an already-processed comment are ignored; post a new comment for a new command.

Supported comment commands are one per comment:

    /scherzo retry
    /scherzo park --reason waiting-for-review
    /scherzo unpark
    /scherzo abort
    /scherzo stop-after-turn
    /scherzo prompt Please continue with the smaller fix.
    /scherzo ui respond ui-17 --cancel
    /scherzo ui respond ui-17 --value approved

The command prefix must start a comment line after leading whitespace and must be followed by whitespace or the end of the line, so `/scherzoed retry` is ignored. Commands inside triple-backtick Markdown code fences are ignored. `/scherzo help`, `/scherzo status`, `/scherzo stop`, `/scherzo continue`, and multiple `/scherzo` lines in one comment are not supported in this version.

Issue-targeted commands (`retry`, `park`, and `unpark`) target the Linear issue containing the comment. Session-targeted commands (`abort`, `stop-after-turn`, `prompt`, and `ui respond`) target the current Scherzo session for that issue and are acknowledged as `not_found` if no live session exists. `/scherzo abort` parks the issue explicitly after the session stops, so later Linear comments and acknowledgement comments do not release it; use `/scherzo unpark` or `/scherzo retry` when the issue should be eligible again. Acknowledgement comments are concise receipts that include the source comment id, command name, status, and target when known; prompt text is redacted/truncated and not quoted in full.

This transport is runtime-only. Commands posted while Scherzo is down are missed because old comments are ignored at daemon startup, and processed receipts are not durable across restart. Local `scherzoctl` remains the fallback control path.

## Safety posture

Scherzo is intended for trusted repositories and trusted workflow files. Hooks are arbitrary shell. pi tool execution follows the operator's `pi.command` and host OS environment. Scherzo enforces workspace cwd and root containment, but it does not provide a VM or container sandbox.

pi compatibility probes and prompted sessions launch only from prepared workspaces. Extension UI dialogs default to automatic cancellation; `pi.ui_request_policy` may be set to `cancel`, `fail`, `ignore`, or `operator`; unknown policy strings are rejected. `operator` waits for an operator response through the local control API or Linear command comments until `pi.ui_request_timeout_ms` expires. Short `pi.read_timeout_ms` values are polling intervals during active turns; a turn fails only when `pi.stall_timeout_ms` expires without a valid pi line or `pi.turn_timeout_ms` expires before `agent_end`.

Retry and session caps park issues in memory rather than spending tokens forever. Operator parks and abort parks are explicit and clear only on process restart, `unpark`, or `retry`. System cap parks auto-release only when Scherzo sees a core issue field change: issue id, identifier, title, description, priority, state, or branch name. URL-only, label-only, blocker-only, timestamp-only, and comment-only changes do not release auto parks, so Linear comments and Scherzo acknowledgement comments cannot accidentally redispatch an aborted or parked issue.

## Implemented coverage and current limits

Implemented:

- Workflow front matter loading, config defaults, env/path resolution, dispatch validation, content-based reloads, and pause semantics.
- Linear GraphQL read requests, real HTTPS transport through `gleam_httpc`, bounded smoke reads, mutation builders, fake-response normalization, and typed error mapping.
- Safe workspace key sanitization, root containment checks, lifecycle hooks, sidecar population markers, cleanup by stored workspace path, and local instance locking.
- pi JSON Lines RPC launch, command/response correlation, compatibility probing with stats, prompt execution, turn/stall timeout handling, stats decoding, extension UI cancellation, and fake-pi integration tests.
- Pure in-memory scheduling decisions for dispatch eligibility, retries, parking, continuation caps, reconciliation, and token accounting.
- Long-lived daemon actor with poll/retry timers, monitored workers, WorkerUpdate logging, in-memory session event replay, local authenticated control API, shared operator command model, Linear command comments, `scherzoctl`, programmatic shutdown, graceful SIGTERM daemon shutdown, and optional Linear handoff.
- Structured key-value log formatting with secret redaction.

Still intentionally out of scope:

- Distributed exactly-once claiming across hosts or workspace roots.
- Durable scheduler state across BEAM restarts.
- CLI Ctrl-C/SIGINT graceful shutdown hooks and crash recovery after `kill -9`, host power loss, or BEAM VM termination.
- HTTP dashboard, Scherzo-to-Linear final result reporting, SSH workers, and the optional `linear_graphql` pi tool extension.
- Automatic discovery of Linear workflow state IDs by state name.

## Operational rollout

1. Run deterministic validation:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -- --help
       direnv exec . gleam run -- ctl --help

2. Run read-only Linear smoke against a private test project:

       LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke ./examples/WORKFLOW.md

3. Run no-prompt pi probe with a scratch workflow:

       LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe ./examples/WORKFLOW.md

4. Run daemon mode with fake pi and one harmless test issue before enabling a real pi issue:

       LINEAR_API_KEY=lin_api_... REPO_URL=git@github.com:example/repo.git direnv exec . gleam run -- ./examples/WORKFLOW.md

Stop CLI daemon mode with Ctrl-C or your process manager, then check for stale lock files as described above.
