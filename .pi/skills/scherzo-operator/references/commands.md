# Scherzo operator command reference

Use this reference from the `scherzo-operator` skill when you need exact command shapes. Run commands from the repository root. Prefer `scripts/scherzoctl` for live daemon inspection/control because it enters the repository toolchain for you, and prefer `--json` for bounded machine-readable output.

## CLI split: daemon control vs offline retained state

There are two operator surfaces:

1. **Live daemon control**: `scripts/scherzoctl <command> [options]`. These commands talk to the running daemon through a control file and generally return a daemon protocol JSON envelope with `ok`, `target`, and `data` when `--json` is supplied.
2. **Offline retained-state maintenance**: top-level Scherzo offline commands. In this development checkout, run them as `direnv exec . gleam run -- <offline-command> ...`; packaged releases may expose the same surface as `scherzo <offline-command> ...`. These commands inspect or mutate disk-backed state directly and are not daemon protocol envelopes.

Do not teach deprecated daemon-wrapper aliases for offline maintenance. For example, `scripts/scherzoctl cleanup --json --dry-run` currently warns and should be replaced with `direnv exec . gleam run -- cleanup --json --dry-run` or packaged `scherzo cleanup --json --dry-run`.

## Control file selection

Command-first ordering is required: put the Scherzo subcommand immediately after `scripts/scherzoctl`, then options such as `--json` or `--control-file <path>`.

If the user provides a control file, put `--control-file <path>` after the Scherzo subcommand:

```sh
scripts/scherzoctl ps --json --control-file .scherzo/workspaces/.scherzo-state/control.json
# Inspect non-secret target fields in JSON output: target.control_file_path and target.workspace_root.
```

Relative `--control-file` paths resolve from the directory where `scripts/scherzoctl` was invoked, even though the wrapper enters the Scherzo source checkout. If no explicit path is provided, `scherzoctl` honors `SCHERZO_CONTROL_FILE`, then falls back to the repository default in the caller directory when it exists:

```sh
export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
scripts/scherzoctl ps --json
```

If the wrapper is unavailable, use the same subcommand ordering through Gleam:

```sh
direnv exec . gleam run -- ctl ps --json
```

Do not search outside the repository for control files. Do not print or paste the raw control file because it contains a `token`.

## Read-only live-daemon inspection

Use these commands before considering any operator action:

```sh
scripts/scherzoctl ping --json
scripts/scherzoctl ps --json
scripts/scherzoctl query status --json
scripts/scherzoctl query metrics --json
scripts/scherzoctl task list --state ready --json --limit 20
scripts/scherzoctl task show <task|id:<id>> --json
scripts/scherzoctl outbox --json --limit 20
scripts/scherzoctl outbox --status retryable --json --limit 20
scripts/scherzoctl outbox <outbox-id> --json
scripts/scherzoctl session <session-id> --json
scripts/scherzoctl events <session-id> --json
scripts/scherzoctl events <session-id> --json --since-cursor <n>
scripts/scherzoctl attach --json --no-follow <session-id>
```

`ping`, `ps`, `query`, `task`, `outbox`, `session`, and `events` are bounded commands. `events <session-id> --json` returns one JSON event page and is the normal choice for summaries. `attach` follows by default, so use `--no-follow` for bounded replay unless the user explicitly asks for live watching.

`task list --state` uses canonical tracker states. For Linear-backed trackers, use states such as `backlog`, `ready`, `active`, `done`, `canceled`, `duplicate`, or `unknown`; avoid Linear UI labels such as `Todo` or `In Progress` unless the CLI help explicitly supports them.

## Session target selection

Use full session ids from `ps --json` or `session --json`; human tables shorten long ids. Top-level issue sessions are workflow run ids such as `ABC-123-1700000000000-1`. Workflow DAG steps create step sessions whose ids start with `workflow-step-`, for example `workflow-step-ABC-123-1700000000000-1-implement-a1-<hash>`.

Before sending `prompt`, `stop-after-turn`, `abort`, or `ui respond`, disambiguate whether the user means the top-level issue run or a concrete step session:

- `prompt`, `stop-after-turn`, and `ui respond` sent to a top-level YAML workflow session route to the active step only when exactly one step currently accepts operator commands; if multiple step sessions are active, target the step session.
- `abort` on a top-level issue session stops the whole workflow run and its step sessions. `abort` on a step session sends the abort to that step command subject when available.
- Command steps do not run pi, but they still get `workflow-step-...` sessions and failure events.

## Recovery commands

The normal recovery surface is consolidated around two commands:

```sh
# Preview retained-run retry without mutating state.
scripts/scherzoctl retry <task|id:<id>|run:<run-id>> --step <step-id> --dry-run --json

# Retained-run retry using the safe rewind lattice.
scripts/scherzoctl retry <task|id:<id>|run:<run-id>> --step <step-id> --json

# Fresh run from current task payload and current workflow definition.
scripts/scherzoctl retry all <task|id:<id>> --json
```

`retry --dry-run` prints the chosen safe point and preserved/discarded steps. Use it before mutating retained state unless the user has already supplied the exact plan.

Salvage override forms remain available for inspected maintenance, but do not lead with them as the normal decision tree:

```sh
# Exact fail-closed retained-run step repair override. Use a bare run id.
scripts/scherzoctl run retry-step <run-id> --step <step-id> --json

# Reconstruct workflow contract outputs without rerunning completed steps.
scripts/scherzoctl run recollect-outputs <run-id> --json

# Plan or perform manual retained-run finalization. Always dry-run first.
scripts/scherzoctl run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason "manual recovery" --dry-run --json
scripts/scherzoctl run finalize <run-id> --validate --outputs auto --publish --update-tracker --reason "manual recovery" --yes --json

# Replay failed publication through the daemon queue using already-materialized outputs.
scripts/scherzoctl publication retry <run-id> --json
scripts/scherzoctl publication retry <run-id> --publication <publication-id> --json

# Clean orphaned YAML child steps for a retained run; dry-run by default.
scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --dry-run --json
scripts/scherzoctl recovery cleanup-orphan-steps run:<run-id> --yes --json
```

Legacy `retry-step` and `recollect-outputs` top-level spellings are deprecated compatibility aliases. `task retry --start-fresh/--from-scratch --reason <text>` remains a scripted compatibility spelling for fresh runs, but prefer `retry all` in operator guidance.

### Choosing a recovery path

Prefer retained-run recovery over fresh restart when completed upstream work or retained artifacts are useful:

1. Inspect `ps`, `session`, `events`, retained artifacts, and any previous queued operation status.
2. Identify the failed or interrupted `run_id` and optional `step_id`.
3. Run `retry <task|run:run-id> --step <step-id> --dry-run` and check the safe point plus preserved/discarded steps.
4. Run the same `retry` command without `--dry-run` when the plan is expected.
5. Use `run recollect-outputs <run-id>` when the run work is complete but contract outputs need reconstruction.
6. Use `publication retry <run-id>` when outputs exist and publication failed.
7. Use `run finalize ... --dry-run`, then `--yes`, only for explicit manual finalization after reviewing the dry-run plan.
8. Use `retry all <task>` when the operator intentionally wants a fresh superseding run.

Retry/recovery rejections include a stable `reason`, human `message`, and exact `Next safe command:`. Report those fields verbatim. Manual operator holds may point at `unpark`; for quarantined non-manual parks that `retry` can release itself, follow `retry` rather than inventing a manual unpark step.

Queued recovery commands return `status: queued` and an `operation_id` when accepted. Poll before declaring completion:

```sh
scripts/scherzoctl query operation-status <operation-id> --json
```

`query operation-status` can time out under daemon load because it reads a live projection. A timeout means the status query did not complete; it does not by itself prove the queued recovery failed. Re-check later and corroborate with `ps`, `session`, and `events`.

## Other operator controls

Use exact task ids, run ids, step ids, session ids, and request ids from JSON inspection. Commands with `--yes` are destructive confirmations at the CLI layer.

```sh
scripts/scherzoctl pause --json
scripts/scherzoctl resume --json
scripts/scherzoctl reload --json
scripts/scherzoctl park <task> --reason "manual cleanup" --yes --json
scripts/scherzoctl unpark <task> --json
scripts/scherzoctl abort <session-id> --yes --json
scripts/scherzoctl stop-after-turn <session-id> --yes --json
scripts/scherzoctl prompt <session-id> "summarize progress" --json
scripts/scherzoctl ui respond <session-id> <request-id> --cancel --json
scripts/scherzoctl ui respond <session-id> <request-id> --value "approved" --json
scripts/scherzoctl run-schedule <job> --now --json
```

A successful `retry` or `retry all` response acknowledges acceptance after synchronous safety checks. Later claim, Linear reporting, workspace setup, worker start, or run failures are reported through normal ledger/session/failure evidence; inspect `ps`, `session`, `events`, and `query operation-status` when a retry was accepted but does not later run successfully.

## JSON response handling

For daemon inspection/control commands, `--json` returns one protocol JSON document with a non-secret `target` object. Check `target.control_file_path` and `target.workspace_root` before trusting or mutating a daemon. `ok: true` means the control server accepted and decoded the request. `ok: false` means the request failed before a command result could be applied, such as authentication failure, timeout, malformed request, or protocol error; report `error.code` and `error.message` without exposing secrets.

Daemon controls return command data with statuses such as:

- `applied`: the daemon applied the action immediately.
- `queued`: the daemon accepted the action and queued it for a worker/session/recovery operation. Capture `operation_id` when present.
- `rejected`: the daemon understood the request but refused it; report the reason.
- `not_found`: the target task, run, session, operation, or UI request was not found.
- `not_allowed`: the target exists but the requested action is not allowed in its current state; report the reason.

When shaping JSON with `jq` or Python, first inspect the top-level shape instead of assuming arrays or `.sessions` directly:

```sh
scripts/scherzoctl ps --json > tmp/ps.json
jq 'keys' tmp/ps.json
jq '.data | keys' tmp/ps.json
```

Do not pipe `attach --json` into parsers expecting one JSON document; attach JSON is a stream of event envelopes. Do not print control tokens, `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, or large raw event payloads unless the user explicitly asks for a raw excerpt.

## Retained workflow runs and command-step diagnostics

Read session recovery and events first:

```sh
scripts/scherzoctl session <session-id> --json
scripts/scherzoctl events <session-id> --json
scripts/scherzoctl attach --json --no-follow <session-id>
```

A retained implementation or execplan-implementation run has a `.scherzo-keep-workspace` marker under its run root, for example `.scherzo/workspaces/<workflow>/<issue>/<run>/.scherzo-keep-workspace`. Do not remove that marker or delete the run until inspection shows that unpushed work is disposable or already published.

Failed command steps publish a `workflow command <step-id>` failure event. The event `tool_output` contains bounded diagnostics; if the output was truncated, the step artifact may include `diagnostic_path`, usually below the step workspace as `.scherzo/command-step-diagnostics/<step-id>.txt`. Durable step artifacts are stored under `.scherzo/workspaces/.scherzo-state/artifacts/runs/<run-id>/<step-component>/attempt-<n>.json`.

For direct artifact inspection, read only the fields you need and redact before sharing:

```sh
jq '.artifact | {step_id,status,exit_code,failure_code,diagnostic_path,stdout_truncated,stderr_truncated}' .scherzo/workspaces/.scherzo-state/artifacts/runs/<run-id>/<step-component>/attempt-1.json
```

## Offline cleanup, publication, and state maintenance

Start read-only. Relative `--root` paths resolve from the caller working directory.

Development checkout examples:

```sh
direnv exec . gleam run -- cleanup --root .scherzo/workspaces --json --dry-run
direnv exec . gleam run -- cleanup --root .scherzo/workspaces --json --dry-run --limit 100 --max-runtime-ms 240000
direnv exec . gleam run -- state status --root .scherzo/workspaces --json
direnv exec . gleam run -- artifact publication list --run <run-id> --root .scherzo/workspaces --json
direnv exec . gleam run -- artifact publication show --run <run-id> --publication <publication-id> --root .scherzo/workspaces --json
```

Packaged CLI equivalents, when `scherzo` is available on `PATH`:

```sh
scherzo cleanup --root .scherzo/workspaces --json --dry-run
scherzo state status --root .scherzo/workspaces --json
scherzo artifact publication list --run <run-id> --root .scherzo/workspaces --json
```

`cleanup --dry-run` reports provider summaries, `would_delete`, `deleted`, `retained`, `warnings`, roots, and resume metadata for bounded cleanup. It deletes nothing. `state status` is read-only and reports retained local state schema status. Cleanup/state JSON is local maintenance output, not a daemon protocol envelope with `ok`.

Run local cleanup or offline state mutations with explicit `--yes` so the CLI receives an intentional destructive-operation acknowledgement:

```sh
direnv exec . gleam run -- cleanup --root .scherzo/workspaces --json --yes
direnv exec . gleam run -- state archive-old --root .scherzo/workspaces --yes --json
direnv exec . gleam run -- state discard-old --root .scherzo/workspaces --yes --json
direnv exec . gleam run -- state reinitialize --root .scherzo/workspaces --yes --json
direnv exec . gleam run -- state compact --root .scherzo/workspaces --dry-run --json
direnv exec . gleam run -- state compact --root .scherzo/workspaces --yes --json
direnv exec . gleam run -- artifact publication retry --run <run-id> --publication <publication-id> --root .scherzo/workspaces --json
```

Use daemon-backed `scripts/scherzoctl publication retry <run-id> ...` when a daemon is running and the publication should be replayed through the daemon queue. Use offline `artifact publication retry --root ...` only for daemon-stopped retained-state recovery or break-glass maintenance.

For dangling jj workflow workspaces, prefer letting Scherzo publish/cleanup run the configured remove hook. If manual cleanup is chosen, run the remove hook before deleting a run root so jj workspace records are forgotten first:

```sh
repo_root=$(pwd -P)
run_root=.scherzo/workspaces/<workflow>/<issue>/<run>
workflow=<workflow>
SCHERZO_REPO_ROOT="$repo_root" SCHERZO_CONFIG_DIR="$repo_root/.scherzo" SCHERZO_RUN_ROOT="$run_root" SCHERZO_WORKSPACE_PATH="$run_root" SCHERZO_WORKFLOW_ID="$workflow" scripts/scherzo-workspace-jj lifecycle remove
rm -rf "$run_root"
```

Do not use manual deletion as a substitute for inspecting recovery, checking `.scherzo-keep-workspace`, or reading cleanup dry-run output.

## Linear CLI operations

Use the repository `linear` wrapper through `direnv exec .` when Linear needs to be read or updated. The wrapper uses `LINEAR_API_KEY`, falls back to `SCHERZO_AGENT_LINEAR_API_KEY`, and applies `LINEAR_DEFAULT_PROJECT` as the default `--project` for `linear issue create` when the command omits one; never print tokens.

Read-only examples:

```sh
direnv exec . linear issue view LIV-104 --json --no-download
direnv exec . linear issue comment list LIV-104 --json
direnv exec . linear issue query --team LIV --json
```

Linear mutations:

```sh
direnv exec . linear issue create --team LIV --state Todo --label workflow:research --title "Research example" --description-file tmp/issue.md --no-interactive
direnv exec . linear issue comment add LIV-104 --body-file tmp/operator-comment.md
direnv exec . linear issue update LIV-104 --state "Triage"
direnv exec . linear issue update LIV-104 --label needs-clarification
```

Current `linear-cli` v2 uses `issue view` for issue reads and `issue comment list/add` for comments. `issue create` and `issue update` do not currently support `--json`; use `--no-interactive`, summarize their text output, and run `issue view <id> --json --no-download` afterward if machine-readable fields are required. If you see older notes saying `linear issue get` or `linear comment list/add`, translate them to `direnv exec . linear issue view ... --json` and `direnv exec . linear issue comment list/add ...`. Do not assume stale forms such as top-level `linear comment ...` or `linear issue get ...` exist; check `direnv exec . linear issue --help` if unsure.

Use `direnv exec . linear api ...` or direct `curl https://api.linear.app/graphql` only when the CLI lacks the needed operation. Prefer `linear api` over hand-written `curl`, keep GraphQL variables in files when practical, and keep API keys out of logs.
