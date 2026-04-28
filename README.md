# Scherzo

Scherzo is a Gleam/Erlang daemon that polls one Linear project, prepares one workspace per issue, and runs a pi coding-agent session in that workspace using pi RPC mode.

The current implementation is ready for cautious use against one real Linear board from one Scherzo instance and one canonical workspace root. It includes reusable real Linear HTTPS reads, bounded smoke checks, a long-lived daemon actor with poll and retry timers, monitored pi workers, workflow reload by file contents, no-prompt pi probing, optional Linear handoff comments/state updates, an authenticated local read-only control API, `scherzoctl`, and a local instance lock. It is not a distributed job system: do not run multiple hosts or multiple independent workspace roots against the same Linear project until a durable claim backend exists.

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

Use `--pi-probe` before allowing a real prompt. It validates dispatch hooks, acquires the local instance lock, prepares a scratch workspace named from `SCHERZO-PROBE`, launches pi RPC, runs `set_session_name`, `set_auto_retry`, `get_state`, and `get_session_stats`, then terminates pi without sending `prompt`:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe path/to/WORKFLOW.md

## Runtime requirements

Runtime operation requires:

- `LINEAR_API_KEY` or `tracker.api_key` in the workflow.
- A Linear project slug in `tracker.project_slug`.
- A `pi` executable that supports JSON Lines RPC mode through `pi --mode rpc`.
- A trusted workspace population or verification hook. The example uses `REPO_URL` with `git clone "$REPO_URL" .` in `hooks.after_create`.

Scherzo acquires a local lock at `workspace.root/.scherzo-state/instance.lock` for `--once`, `--pi-probe`, and daemon mode. The lock prevents another Scherzo process using the same canonical workspace root on the same filesystem from starting normally. It is not a distributed lock and does not protect different hosts or different workspace roots.

If startup reports an existing instance lock, first verify that no Scherzo process is still running with that workspace root. Only then remove the stale `instance.lock` file manually and restart.

## Workflow schema

`WORKFLOW.md` is Markdown with optional YAML front matter. Unknown top-level keys are ignored. The core keys are:

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
      compatibility_probe: true
    handoff:
      enabled: false
      comment_on_claim: true
      comment_on_success: true
      comment_on_failure: true
      claim_state_id: null
      success_state_id: null
      failure_state_id: null

See `examples/WORKFLOW.md` for a runnable template.

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

Programmatic `daemon.shutdown` cancels poll/retry timers and stops daemon-owned workers. The CLI path does not install SIGINT or SIGTERM handlers in this phase. If the shell or process manager terminates the VM, Erlang port ownership and OS teardown normally clean up children, but Scherzo may leave a stale local instance lock. Remove it manually only after checking no Scherzo process remains active.

## Session event model

Daemon mode also starts an in-memory EventHub for internal session visibility. The hub records one session summary per live worker attempt and keeps a bounded buffer of recent events for that session, including lifecycle transitions, pi event names, assistant message deltas, UI request metadata, token totals, and worker exit events. The daemon and worker hot paths publish to the hub with fire-and-forget actor messages, while tests and future control clients can query summaries and replay events by cursor.

Event history is not durable across daemon restart. Raw pi JSON payloads are retained only after recursive secret redaction and a 16 KiB per-event cap, so replay is useful for control clients and future renderers without treating the event buffer as an audit log.

## Read-only control API

Daemon mode starts a small authenticated control server on `127.0.0.1` after the EventHub is available. The server uses line-delimited JSON over loopback TCP, chooses an OS-assigned port, generates a fresh token for each daemon start, and writes connection details to `workspace.root/.scherzo-state/control.json`. The daemon logs the control file path and port with an event like:

    level=info service=scherzo event=control_server_started control_file=... host=127.0.0.1 port=54321

The token is stored in the control file and is required on every request, but it is not logged. The control file has this shape:

    {"version":1,"host":"127.0.0.1","port":54321,"token":"...","workspace_root":".scherzo/workspaces","started_at_ms":42}

Use the logged path explicitly, or export it for repeated commands:

    export SCHERZO_CONTROL_FILE=<logged-control-file>
    scripts/scherzoctl ping
    scripts/scherzoctl ps
    scripts/scherzoctl events <session-id>
    scripts/scherzoctl attach --raw <session-id>

Every command also accepts `--control-file <path>`. Non-streaming commands accept `--json` for automation:

    scripts/scherzoctl ps --json
    scripts/scherzoctl session <session-id> --json
    scripts/scherzoctl events <session-id> --json

The control API is local-only, token-authenticated, and read-only in this phase. It can list session summaries, fetch one session, replay retained events, and follow events with `attach --raw`; it cannot abort workers, send prompts, answer UI requests, retry, pause, resume, or mutate Linear. The backing data is the in-memory EventHub, so it is bounded and disappears on daemon restart. A stale control file after a crash is recoverable: `scherzoctl ping` fails cleanly, and operators should restart the daemon and use the newly logged control file path.

## Safety posture

Scherzo is intended for trusted repositories and trusted workflow files. Hooks are arbitrary shell. pi tool execution follows the operator's `pi.command` and host OS environment. Scherzo enforces workspace cwd and root containment, but it does not provide a VM or container sandbox.

pi compatibility probes and prompted sessions launch only from prepared workspaces. Extension UI dialogs are cancelled automatically; fire-and-forget UI notifications are ignored after logging. Short `pi.read_timeout_ms` values are polling intervals during active turns; a turn fails only when `pi.stall_timeout_ms` expires without a valid pi line or `pi.turn_timeout_ms` expires before `agent_end`.

Retry and session caps park issues in memory rather than spending tokens forever. Parking clears on process restart or when Linear reports the issue with a newer `updated_at` value.

## Implemented coverage and current limits

Implemented:

- Workflow front matter loading, config defaults, env/path resolution, dispatch validation, content-based reloads, and pause semantics.
- Linear GraphQL read requests, real HTTPS transport through `gleam_httpc`, bounded smoke reads, mutation builders, fake-response normalization, and typed error mapping.
- Safe workspace key sanitization, root containment checks, lifecycle hooks, sidecar population markers, cleanup by stored workspace path, and local instance locking.
- pi JSON Lines RPC launch, command/response correlation, compatibility probing with stats, prompt execution, turn/stall timeout handling, stats decoding, extension UI cancellation, and fake-pi integration tests.
- Pure in-memory scheduling decisions for dispatch eligibility, retries, parking, continuation caps, reconciliation, and token accounting.
- Long-lived daemon actor with poll/retry timers, monitored workers, WorkerUpdate logging, in-memory session event replay, local read-only control API, `scherzoctl`, programmatic shutdown, and optional Linear handoff.
- Structured key-value log formatting with secret redaction.

Still intentionally out of scope:

- Distributed exactly-once claiming across hosts or workspace roots.
- Durable scheduler state across BEAM restarts.
- CLI SIGINT/SIGTERM graceful shutdown hooks.
- HTTP dashboard, mutating operator controls, SSH workers, and the optional `linear_graphql` pi tool extension.
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
