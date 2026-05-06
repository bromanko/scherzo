# Scherzo

Scherzo is a Gleam/Erlang orchestration daemon for running pi coding-agent workflows against Linear issues. It is now YAML-orchestrator / YAML-DAG only: runtime settings live in a `scherzo.yaml` orchestrator config, and issue workflows live in YAML DAG files that reference Markdown prompt templates.

Legacy Markdown runtime workflows (`WORKFLOW.md` or `.scherzo/workflows/*.md`) are no longer supported. Markdown remains supported for prompt templates only.

## Quick start

```sh
direnv allow
# Fast deterministic unit suite (default; run for every PR and before pushing)
direnv exec . gleam test

# Report the source/build identity to include in bug reports and operator logs
direnv exec . gleam run -- --version

# Readiness validation before dispatching work
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml

# Read-only readiness subset when you do not want workspace hooks or pi probing
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check workflow-config --check linear-contract --check linear-smoke .scherzo/scherzo.yaml

# Run one eligible issue and exit
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --once .scherzo/scherzo.yaml

# Run daemon mode through the Ctrl-C-friendly devenv helper
LINEAR_API_KEY=lin_api_... direnv exec . scherzo-start .scherzo/scherzo.yaml
```

## Test suites

Scherzo keeps the default Gleam test command as the fast unit suite. It must be deterministic, must not contact external services, and is the suite run by SelfCI for every PR:

```sh
direnv exec . gleam test
# equivalent helper when you want the suite name to be explicit
direnv exec . scherzo-test-unit
```

Longer validations live in explicit suites and are not selected by default:

| Suite | Command | When to run | Required dependencies |
| --- | --- | --- | --- |
| Unit | `direnv exec . gleam test` or `direnv exec . scherzo-test-unit` | Every PR, before pushing, and during normal implementation feedback | Devenv Gleam/Erlang toolchain only; no network or real Linear/pi calls |
| Local integration | `direnv exec . scherzo-test-local-integration` or `direnv exec . gleam test -- --suite local-integration` | Before changing workspace hooks, workflow-run workspace behavior, pre-release checks, and scheduled/nightly local infrastructure validation | Local shell tools plus `jj`; creates temporary repositories under `test/tmp/` |
| External real-pi validation | `direnv exec . scherzo-test-real-pi-validation` or `direnv exec . gleam test -- --suite real-pi-validation` | Manual operator validation before releases or after pi session-persistence changes | `pi` on `PATH`, model/provider credentials, network access, and enough time for live RPC turns |

Use `direnv exec . gleam test -- --suite all` only for an explicit full local sweep; it includes the external real-pi validation and will fail if those dependencies are absent.

## Local final validation with SelfCI

Scherzo dogfood implementation workflows use SelfCI as the canonical final validation gate. To run the same validation locally against the configured pull-request base:

```sh
direnv allow .
direnv exec . selfci check --base main@origin --candidate @ --print-output
```

`selfci check` runs the checked-in `.config/selfci/ci.sh`, which performs format checks, the default unit suite, and `nix flake check --print-build-logs`, and `--print-output` surfaces the failing step output for repair. The workflow helper resolves the base from `tmp/scherzo-implementation-refresh-base-latest.json` when present; otherwise it uses `${SCHERZO_PR_BASE:-main}@${SCHERZO_PR_REMOTE:-origin}`.

Bootstrap caveat: the `--base` revision must already contain this repository's SelfCI configuration and local development tooling. If a branch is based before the SelfCI bootstrap landed, first refresh or rebase onto a SelfCI-capable `main`, or use the raw bootstrap checks only long enough to reach a base that can run `selfci check`.

## Using Scherzo from another devenv

This repository is also a Nix flake. The default package builds a precompiled Erlang shipment and exposes these commands:

- `scherzo` — daemon, one-shot, doctor, and control subcommands.
- `scherzo-start` — Ctrl-C-friendly daemon wrapper that translates SIGINT to Scherzo's graceful SIGTERM shutdown path. The packaged command and local devenv helper share `scripts/scherzo-start-runner` so shutdown behavior is dogfooded locally.
- `scherzoctl` — shorthand for `scherzo ctl`.

In a consuming devenv project, add the flake input and package:

```yaml
# devenv.yaml
inputs:
  nixpkgs:
    url: github:NixOS/nixpkgs/nixos-25.11
  scherzo:
    url: github:bromanko/scherzo
```

```nix
# devenv.nix
{ pkgs, inputs, ... }:

let
  system = pkgs.stdenv.hostPlatform.system;
in
{
  packages = [
    inputs.scherzo.packages.${system}.default
    # Add pi and any tools used by your workspace hooks/workflows separately.
  ];
}
```

Then run it from the consuming repository so relative config and workflow paths resolve there. Use `scherzo --version` to report the packaged source/build identity in bug reports and operator logs:

```sh
direnv exec . scherzo --version
LINEAR_API_KEY=lin_api_... direnv exec . scherzo doctor .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . scherzo-start .scherzo/scherzo.yaml
direnv exec . scherzoctl ps
```

You can also run the flake directly while developing this repository:

```sh
nix run .#scherzo -- --help
nix run .#scherzo-start -- .scherzo/scherzo.yaml
```

If no path is provided, Scherzo looks for the first existing default config in this order:

1. `.scherzo/scherzo.yaml`
2. `.scherzo/scherzo.yml`
3. `scherzo.yaml`
4. `scherzo.yml`

Passing a `.md` path fails with a clear startup error.

## Repository conventions

A typical repo-local setup is:

```text
.scherzo/
  scherzo.yaml                 # orchestrator/runtime config
  workflows/
    research.yaml              # workflow DAG
    implementation.yaml        # workflow DAG
    prompts/
      research.md              # prompt template
      implement.md             # prompt template
```

This repository dogfoods that layout with:

- `.scherzo/scherzo.yaml`
- `.scherzo/workflows/research.yaml`
- `.scherzo/workflows/prompts/research.md`

Reusable examples live under `examples/`:

- `examples/scherzo.yaml`
- `examples/workflows/research.yaml`
- `examples/workflows/implementation.yaml`
- `examples/workflows/prompts/*.md`

## Planning posture

Scherzo is still moving quickly, so implementation plans should not assume backward compatibility unless the plan or operator explicitly calls it out. Prefer the simpler clean change over compatibility shims: update schemas, tests, docs, examples, and local state handling together. Local development state such as `.scherzo-state/ledger` may be deleted or regenerated after breaking internal changes.

## Orchestrator config

The orchestrator config owns runtime policy: tracker settings, polling, workspace hooks, pi command and timeouts, agent limits, handoff, workflow routing, artifact limits, Linear contract checking, and Linear comment command transport.

Minimal shape:

```yaml
version: 1
tracker:
  kind: linear
  endpoint: https://api.linear.app/graphql
  api_key: "$LINEAR_API_KEY"
  project_slug: YOUR_PROJECT_SLUG
  active_states: [Todo, In Progress]
  terminal_states: [Done, Canceled, Duplicate]

workspace:
  root: .scherzo/workspaces
  hooks:
    create: |
      mkdir -p "$SCHERZO_WORKSPACE_PATH"
      git clone "$REPO_URL" "$SCHERZO_WORKSPACE_PATH"
    before_step: |
      test -d "$SCHERZO_WORKSPACE_PATH/.git"
    after_step: |
      true
    remove: |
      rm -rf "$SCHERZO_WORKSPACE_PATH"
    timeout_ms: 60000

agent:
  max_concurrent_agents: 1
  max_turns: 20
  max_retry_attempts: 5
  max_sessions_per_issue: 3

pi:
  # Default non-persistent launch path. Scherzo uses this shell command while
  # pi.session_persistence.enabled is false.
  command: "pi --mode rpc --no-session"
  # Opt-in persistent launches use structured argv so Scherzo can safely append
  # --session <recorded-session-file> during recovery without mutating a shell
  # command string. Do not include --session or --no-session in pi.argv.
  # argv: ["pi", "--mode", "rpc"]
  # session_persistence:
  #   enabled: true
  # Optional project defaults for workflow agent steps. `model` is the full
  # pi model/provider selection key; `thinking` is configured separately.
  model: google/gemini-2.5-flash
  thinking: low
  compatibility_probe: true

routing:
  workflow_label_prefix: "workflow:"
  require_exactly_one_workflow_label: true
  workflows:
    research: workflows/research.yaml
    implementation: workflows/implementation.yaml
```

Relative paths are resolved from the orchestrator config file directory.

## Workflow DAG files

A workflow file describes one routed issue workflow. Steps may be `agent` steps that run pi with a Markdown prompt template, or `command` steps that run shell commands in a prepared workflow workspace.

```yaml
version: 1
id: implementation
description: Implement, test, review, apply feedback, and validate.
max_parallel_steps: 4
steps:
  - id: implement
    kind: agent
    prompt: prompts/implement.md
    workspace: main

  - id: test_after_implement
    kind: command
    depends_on: [implement]
    run: gleam test
    workspace: main
    on_failure: continue

  - id: code_review
    kind: agent
    depends_on: [implement]
    prompt: prompts/code-review.md
    # Per-step overrides are independent: this step keeps the project default
    # model/provider but asks pi for a higher thinking level.
    thinking: high
    workspace:
      name: code-review
      from: main

  - id: apply_feedback
    kind: agent
    depends_on: [test_after_implement, code_review]
    prompt: prompts/apply-feedback.md
    workspace: main
```

Important rules:

- `id` must match the routing key used by `routing.workflows`.
- Step ids are unique within a workflow.
- `depends_on` forms a DAG; cycles are rejected.
- Prompt paths are relative to the workflow YAML file and must stay within that workflow directory.
- Steps sharing the same logical workspace are serialized.
- Steps using different logical workspaces may run concurrently up to `max_parallel_steps` and global agent limits.
- A derived workspace (`name` + `from`) is prepared from the named source workspace before the step runs.
- Agent steps inherit `pi.model` and `pi.thinking` from the orchestrator config. An agent step can override `model`, `thinking`, or both; unspecified values continue to inherit the project default.
- Command steps do not run pi, so `model` and `thinking` are only valid on agent steps.
- `model` is passed to pi as one selection key (for example `google/gemini-2.5-flash` or `github-copilot/gpt-5.1-codex`). Do not include pi's `:<thinking>` shorthand in `model`; set `thinking` separately (`off`, `minimal`, `low`, `medium`, `high`, or `xhigh`).

## Prompt templates

Prompt files are Markdown templates rendered for pi. Templates can reference the issue and prior step artifacts. For example:

```md
Implement {{ issue.identifier }}: {{ issue.title }}

Description:
{{ issue.description }}

Previous test output:
{{ steps.test_after_implement.stdout }}
```

Markdown prompt templates are not runtime workflow files; they are only prompt bodies referenced by YAML DAG steps.

## Workspace hooks

Workspace hooks are trusted shell snippets from the orchestrator config. Scherzo creates and prepares per-issue/per-run workflow workspaces and calls hooks with environment such as:

- `SCHERZO_CONFIG_DIR`
- `SCHERZO_WORKFLOW_ID`
- `SCHERZO_RUN_ID`
- `SCHERZO_ISSUE_ID`
- `SCHERZO_ISSUE_IDENTIFIER`
- `SCHERZO_WORKSPACE_NAME`
- `SCHERZO_WORKSPACE_PATH`
- `SCHERZO_SOURCE_WORKSPACE_NAME`
- `SCHERZO_SOURCE_WORKSPACE_PATH`

Use `agent.max_concurrent_agents: 0` to pause new dispatch while keeping daemon reload and reconciliation alive.

## Control and observability

Daemon mode starts a local control server and writes a control file under the configured workspace root. Use `scripts/scherzoctl` to inspect and supervise sessions:

```sh
scripts/scherzoctl ping
scripts/scherzoctl ps
scripts/scherzoctl session <session-id>
scripts/scherzoctl events --pretty <session-id>
scripts/scherzoctl attach <session-id>
scripts/scherzoctl attach --verbose <session-id>
scripts/scherzoctl prompt <session-id> "continue with tests"
scripts/scherzoctl stop-after-turn <session-id> --yes
scripts/scherzoctl abort <session-id> --yes
```

`ps` prints a bounded human table: long session IDs are middle-ellipsized for readability, and `LAST_EVENT` is a daemon-relative age such as `12s ago`. Use `ps --json` when scripts need full session IDs or raw timestamp fields (including the daemon `now_ms` used for age calculations), and pass the full session ID to `session`, `events`, `attach`, and mutating commands.

`attach` replays retained events and then follows new events with a human-readable renderer. It groups visible output by Scherzo pass, streams assistant deltas as thinking text, shows tool input and output as readable blocks, highlights blocking UI requests, and prints token summaries when available. Use `--verbose` for pi lifecycle and raw diagnostic lines, `--no-follow` for replay only, `--since-cursor <n>` to resume after a known cursor, and `--color=auto|always|never` to control ANSI styling.

Every command accepts `--control-file <path>`. Non-streaming commands accept `--json` for automation.

Available local mutating commands include:

```sh
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
```

When YAML DAG agent steps run, Scherzo creates concrete step sessions such as `ABC-123-42-1-implement`. Operator prompts sent to the top-level issue session are routed to the active agent step when that step exposes a command subject.

## Workflow recovery operator status

Session summaries expose live worker status and recovery meaning separately. The existing `status` and `exit_reason` fields still describe the current or final worker process state. The additive `recovery` field is `null` when no recovery fact is known, and is an object when Scherzo can project a backed durable fact into operator guidance. Human `scripts/scherzoctl ps` includes a `RECOVERY` column; JSON output includes the same nullable object.

Backed statuses in this release are `recovered`, `interrupted`, `parked`, `cleanup`, and offline `old_state_reset_required`. `interrupted` means a run was active without a durable finish record; live Erlang ports and live pi processes do not survive a daemon restart. `parked` means dispatch is suppressed until the issue is unparked or its configured release policy fires. `cleanup` means local artifacts are in a retention phase, not that the workflow itself succeeded. `old_state_reset_required` is reported by offline state commands when local ledger or snapshot schema markers are unsupported by this tree.

The reserved strings `resumed`, `inspection_needed`, `blocked`, and `drift_detected` are documented vocabulary only. Scherzo does not emit them from real recovery projection until durable source facts exist for workflow checkpoints or previous pi sessions, operator inspection holds, unsafe side-effecting step holds, or drift rejection. Automation must not treat those reserved strings as observable in this release.

Example JSON shape:

```json
{
  "session_id": "ABC-123-1000-1",
  "status": "running",
  "exit_reason": null,
  "pi_session_id": "pi-current",
  "recovery": {
    "status": "interrupted",
    "source": "projection.run_interrupted",
    "message": "daemon_restart",
    "safe_actions": ["inspect", "view_events", "retry", "park"],
    "workflow_run_id": "ABC-123-1000-1",
    "workflow_step_id": null,
    "current_pi_session_id": "pi-current",
    "previous_pi_session_id": null,
    "park_reason": null,
    "park_release_policy": null,
    "parked_at_ms": null,
    "drift_kind": null,
    "retention_until_ms": null,
    "cleanup_eligible_at_ms": null,
    "cleanup_phase": null
  }
}
```

## Local retention and cleanup

Local cleanup is conservative and starts as a dry run. Run:

```sh
scripts/scherzoctl cleanup
scripts/scherzoctl cleanup --dry-run
scripts/scherzoctl cleanup --json --dry-run
```

Dry run classifies verified artifacts under `<workspace-root>/.scherzo-state/`, reports `would_delete`, `retained`, `warnings`, `roots`, and `transcript_root_status`, and deletes nothing. Unknown, malformed, unsupported, missing-owner, missing-terminal-time, interrupted, parked, old-state-reset-required, path-unsafe, or symlink-unsafe artifacts are retained with warnings. Pi transcript deletion is unavailable unless a concrete transcript root is verified; transcript data may contain prompts, tool inputs, tool outputs, and Linear excerpts.

Apply requires confirmation:

```sh
scripts/scherzoctl cleanup --yes
scripts/scherzoctl cleanup --json --yes
```

Apply first performs the same inventory, aborts on root containment or symlink safety failures, writes redacted tombstones under `<workspace-root>/.scherzo-state/cleanup/tombstones/`, and deletes only artifacts classified eligible under verified `.scherzo-state` roots. Deletion is irreversible unless you have an external backup; tombstones record identifiers, display paths, reasons, and results, not artifact content. Default retention is 30 days for terminal workflow artifacts, 14 days for pi transcripts when a transcript root is available, and 30 days for cleanup tombstones.

Recovery and cleanup logs use structured events such as `workflow_recovery_status` and `workflow_cleanup_completed`. Recovery messages, cleanup warnings, and state reasons are redacted and truncated before display or logging; do not put API keys, raw pi JSON, full prompts, full Linear comment bodies, or unredacted tool payloads in the ledger or cleanup metadata.

## Unsupported old local state

Unsupported local durable state may prevent the daemon from starting, so state maintenance is available offline and does not require Linear, pi, or a running control server:

```sh
scripts/scherzoctl state status --root <workspace-root> --json
scripts/scherzoctl state archive-old --root <workspace-root> --yes
scripts/scherzoctl state discard-old --root <workspace-root> --yes
scripts/scherzoctl state reinitialize --root <workspace-root> --yes
```

`state status` is read-only and reports `current`, `unsupported`, `corrupt`, `missing`, or `archived`. `archive-old` moves only state classified as unsupported into `<workspace-root>/.scherzo-state/archive/old-state/<id>/ledger/`. `discard-old` deletes only unsupported active ledger state and is irreversible. Corrupt or malformed state is retained for manual inspection. `reinitialize` creates the current empty layout after old state has been archived or discarded; it does not synthesize recovered runs or fake snapshots.

## Using pi as an operator UI

The repository includes a project pi skill at `.pi/skills/scherzo-operator` for supervising a running daemon through `scripts/scherzoctl` with `--json`. Start Scherzo daemon mode in one terminal, then copy the control file path from the `control_server_started` log line or export the repository default when that file exists:

```sh
export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
```

In a second terminal from the repository root, start pi and load the skill with `/skill:scherzo-operator`. If slash skill commands are disabled, start pi with the skill explicitly loaded:

```sh
pi --skill .pi/skills/scherzo-operator
```

Ask pi for read-only summaries first, for example `summarize current Scherzo sessions`. The skill should begin with `scripts/scherzoctl ps --json`, then use `session --json` or bounded `events --json` calls for detail. Before `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, or any command using `--yes`, pi must ask you to confirm the exact target and action.

## Doctor readiness checks

Run `doctor` before cautious real-board operation. The command loads the YAML orchestrator config, routed workflow DAGs, and prompt templates, then prints a human-readable readiness report inspired by tools such as `flutter doctor`: each selected check is marked with `✓`, `!`, `✗`, or `-`, followed by a readable summary and remediation hints for failures. The default check set runs in this order: `workflow-config`, `linear-contract`, `linear-smoke`, `instance-lock`, `workspace-hooks`, and `pi-probe`. Use `--logfmt` when you need the previous machine-readable `doctor_check_*` events and `doctor_summary` fields.

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor .scherzo/scherzo.yaml
direnv exec . gleam run -- doctor --list-checks
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --logfmt .scherzo/scherzo.yaml
```

A successful default run looks like:

```text
Scherzo doctor
Config: .scherzo/scherzo.yaml

✓ Workflow config
  Loaded YAML orchestrator config and 1 workflow DAG.

✓ Linear contract
  Project board matches configured states and labels.
  Team count: 1, states: 7, labels: 7.

✓ Linear smoke
  Read-only Linear API check succeeded.
  Candidates: 2, terminal sample: 3, refreshed: 1.

✓ Instance lock
  Local instance lock can be acquired and released.

✓ Workspace hooks
  Scratch workspace was prepared and cleaned up.
  Hooks: create,before_step,remove.

✓ Pi probe
  pi RPC launched successfully and no prompt was sent.

Summary: 6 passed, 0 warnings, 0 failed, 0 skipped

Ready for cautious real-board operation.
```

Use repeated `--check` flags to run a subset. This read-only subset loads config and queries Linear metadata/issues, but it does not acquire the local instance lock, run workspace hooks, prepare a scratch workspace, or launch pi:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check workflow-config --check linear-contract --check linear-smoke .scherzo/scherzo.yaml
```

The default doctor run includes local checks. `workspace-hooks` prepares and cleans up a scratch workflow-run workspace using the configured `workspace.hooks.create`, `workspace.hooks.before_step`, and `workspace.hooks.remove` snippets. `pi-probe` launches pi RPC in that scratch workspace and performs the compatibility probe without sending a task prompt.

The focused one-off readiness modes remain available for troubleshooting individual surfaces:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-contract-check .scherzo/scherzo.yaml
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe .scherzo/scherzo.yaml
```

A mutating Linear attachment diagnostic is also available for throwaway or quiescent comments. It uploads a local Markdown file and attaches it to an existing Linear comment id:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-attach-comment-file <comment-id> notes.md .scherzo/scherzo.yaml
```

This command first tries Linear's native comment-file representation. It uses Linear's internal `Comment.bodyData` rich-text field to append a top-level `file` node, which is the shape Linear's web UI uses for files on comments. Because `bodyData` is internal to Linear and may change, Scherzo enables a fallback for this operation: if native bodyData parsing fails, it uploads the file and updates the comment body with a normal Markdown link to the uploaded asset. Scherzo does not request public Linear uploads and rejects non-HTTPS asset URLs before writing links back to comments; still treat uploaded agent output as sensitive Linear-hosted content.

## Handoff result attachments

Linear handoff comments are disabled unless `handoff.enabled: true`. Existing behavior is unchanged by default: Scherzo posts text comments and state updates according to the configured handoff booleans, and `handoff.attach_result_on_success` defaults to `false`.

To attach successful agent result text as a Markdown file instead of including it inline, enable result attachments:

```yaml
handoff:
  enabled: true
  attach_result_on_success: true
  include_result_on_success: false
  attachment_fallback_to_markdown_link: true
```

When enabled, Scherzo creates the normal success comment, uploads the captured final result as a deterministic Markdown filename containing the issue identifier and run id, and attaches that upload to the success comment. Native attachment uses Linear's internal `Comment.bodyData` rich-text JSON and appends a `file` node while preserving existing comment content. If Linear changes that internal field and `attachment_fallback_to_markdown_link: true`, Scherzo falls back to appending a normal Markdown link to the comment body after upload. `attach_result_on_success: true` requires success comments to be enabled, and attachment payloads are capped to avoid uploading unexpectedly large agent results. Set `attach_result_on_success: false` to return to text-only handoff behavior.

## Linear workflow labels

Scherzo routes issues by label. With the default prefix `workflow:`, an issue labeled `workflow:research` is routed to the `research` workflow key in `routing.workflows`.

Use `doctor --check linear-contract` or `--linear-contract-check` before enforcing labels or state handoff on a real board:

```sh
LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- doctor --check linear-contract .scherzo/scherzo.yaml
```

## Linear command comments

Linear command comments are disabled by default. When `linear_commands.enabled: true`, Scherzo watches configured Linear issue comments for commands with the configured prefix, normally `/scherzo`.

Supported comments include:

```text
/scherzo retry
/scherzo park --reason waiting-for-review
/scherzo unpark
/scherzo abort
/scherzo stop-after-turn
/scherzo prompt Please continue with the smaller fix.
/scherzo ui respond ui-17 --cancel
/scherzo ui respond ui-17 --value approved
```

Authorization is by explicit Linear user id allowlist only. Scherzo persists command receipts in the local ledger as comments are seen, started, completed, and acknowledged. After restart, acked commands are skipped, completed-but-unacked commands are acknowledged from their recorded result without reapplying the command, and commands that were started but not durably completed are acknowledged with `unknown_after_restart` so an operator can inspect state before issuing a new command. If an acknowledgement post fails while the daemon remains running, Scherzo keeps it pending and retries it on later polls without reapplying the source command. Commands posted while Scherzo was down are processed when they are on currently observed issues and still appear in the bounded `poll_limit_per_issue` comment results. Local `scherzoctl` remains the fallback control path.

## Local durable ledger

Scherzo includes a local durable state ledger under `workspace.root/.scherzo-state/ledger/`. Daemon startup now replays this ledger before the first poll tick and uses it for single-instance restart recovery for the same canonical workspace root. Recovery restores durable retry counters, worker-session counters, parked issues, retry timers, known workspace paths, Linear command receipt state, and replayable pending Linear outbox entries that include bounded v2 payloads. Started runs that lack a finish record are marked interrupted because live pi sessions and Erlang ports cannot survive a BEAM restart.

The ledger layout is:

```text
.scherzo-state/ledger/current.jsonl
.scherzo-state/ledger/snapshot.json
.scherzo-state/ledger/archive/segment-<n>.jsonl
```

`current.jsonl` is append-only JSON Lines. Each line is one schema-versioned record with `schema_version`, `record_id`, `kind`, and `at_ms`, plus fields for run, retry, park, counters, known workspaces, Linear command, or outbox facts. Replay rejects unsupported schema versions and malformed middle records, while tolerating one truncated trailing JSON record from a crash during append. Compaction writes a projection snapshot through a temporary file and then archives the old current segment before starting a fresh `current.jsonl`.

Ledger records are operational state, not transcripts. They should contain identifiers, statuses, bounded excerpts, result codes, and redacted strings only. Outbox replay requires `outbox_pending_v2` records with bounded, redacted payload JSON; payload-less old pending outbox records fail startup clearly instead of being silently dropped. Do not store API keys, raw pi JSON, full prompts, or full Linear comment bodies in the ledger.

## Daemon behavior and restart recovery

On startup, Scherzo resolves config, builds Linear clients, replays the local ledger, fetches current Linear state only for ledger-known issue ids in chunks of at most 50, appends any recovery records with fsync, installs the recovered runtime state, schedules recovered retry timers, enqueues known terminal workspace cleanup, and only then allows polling. If Linear cannot refresh the known issue ids, startup fails before dispatching new work; starting from stale local facts would be less safe than refusing to start.

Interrupted active runs are counted as one failure using their run id as the durable counter source, so restarting repeatedly does not double-count the same interrupted run. If retry caps are exhausted, the issue remains parked with its release policy. Explicit operator parks survive issue edits until explicitly unparked. Auto-unpark parks are released only when the refreshed issue fingerprint has changed, matching the normal runtime policy.

This recovery is at-least-once rather than exactly-once. A crash after a real Linear side effect succeeds but before its `outbox_completed` record is written can replay the side effect on restart. Dedupe keys, run ids, and source comment ids are recorded to make duplicates auditable, but Linear-side idempotency is not guaranteed.

## Implemented coverage

The deterministic test suite covers ledger record roundtrips for counters, known workspaces, v2 parking, Linear command receipts, and v2 outbox payloads; projection helpers for retry due time, command receipt state, and pending outbox replay; pure recovery for interrupted, parked, terminal, overdue-retry, future-retry, and payload-less outbox cases; durable Linear command behavior for completed-unacked replay, started-unknown acknowledgements, same-process acknowledgement retry, and old bounded observed comments; and daemon startup ordering through the existing actor tests. Real Linear and real pi are not required for these recovery tests.

## Safety posture

Scherzo is intended for trusted repositories and trusted workflow files. Hooks are arbitrary shell. pi tool execution follows the operator's `pi.command` or, when session persistence is enabled, the structured `pi.argv` plus host OS environment. Scherzo enforces workspace cwd and root containment, but it does not provide a VM or container sandbox.

`pi.session_persistence.enabled` is opt-in. When enabled, Scherzo records the local pi `sessionFile` path for each continuation-capable workflow agent step attempt and may reopen that file with `--session` from the recorded step workspace during crash recovery. These session files and their paths are local sensitive data: they can contain pi transcript state and references to local files. Scherzo stores only the path in the local durable ledger for recovery, does not upload transcript contents to Linear, and redacts raw session paths from operator-facing diagnostics. Retention and cleanup of pi's local transcript files remain an operator responsibility until the separate retention UX lands.

Run only one Scherzo instance per Linear project and canonical workspace root. The local durable ledger supports single-instance restart recovery, not multi-host or multi-workspace exactly-once behavior. Daemon mode handles SIGTERM gracefully by shutting down workers, removing the control file, and releasing the local instance lock before exit. For interactive daemon runs, prefer `direnv exec . scherzo-start .scherzo/scherzo.yaml`; that devenv helper wraps `gleam run -- ...` and translates Ctrl-C/SIGINT into SIGTERM so the graceful shutdown path runs. Direct `gleam run` Ctrl-C, `kill -9`, host power loss, or BEAM VM crashes may leave a stale `workspace.root/.scherzo-state/instance.lock`; operators must remove it only after checking no Scherzo process remains active. Linear command recovery remains bounded to observed issues and the configured comment poll limit; webhooks and Linear-side idempotency are future work.

## Legacy Markdown removal

The old Markdown runtime loader and tests have been removed. The following are no longer valid runtime entrypoints:

```sh
gleam run -- WORKFLOW.md
gleam run -- .scherzo/workflows/research.md
```

Use a YAML orchestrator config instead. For short-lived modes, direct `gleam run` is fine; for long-running interactive daemon mode, prefer the `scherzo-start` devenv helper so Ctrl-C becomes graceful SIGTERM:

```sh
gleam run -- .scherzo/scherzo.yaml
direnv exec . scherzo-start .scherzo/scherzo.yaml
```
