# Scherzo operator command reference

Use this reference from the `scherzo-operator` skill when you need exact command shapes. Run commands from the repository root. Prefer `scripts/scherzoctl` because it enters the repository toolchain for you, and prefer `--json` for daemon inspection/control.

## Control file selection

Command-first ordering is required: put the Scherzo subcommand immediately after `scripts/scherzoctl`, then options such as `--json`, `--control-file <path>`, or `--root <workspace-root>`.

If the user provides a control file, put `--control-file <path>` after the Scherzo subcommand:

```sh
scripts/scherzoctl ps --json --control-file .scherzo/workspaces/.scherzo-state/control.json
# Inspect the non-secret target fields in JSON output: target.control_file_path and target.workspace_root.
```

Relative `--control-file` paths are resolved from the directory where `scripts/scherzoctl` was invoked, even when `scripts` is a symlink into the Scherzo source checkout. If no explicit path is provided, `scherzoctl` honors `SCHERZO_CONTROL_FILE`, then falls back to the repository default in that caller directory when it exists:

```sh
export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
scripts/scherzoctl ps --json
```

If the wrapper is unavailable, use the same subcommand ordering through Gleam:

```sh
direnv exec . gleam run -- ctl ps --json
```

Do not search outside the repository for control files. Do not print or paste the raw control file because it contains a `token`.

## Read-only inspection

Use these commands before considering any operator action:

```sh
scripts/scherzoctl ping --json
scripts/scherzoctl ps --json
scripts/scherzoctl session <session-id> --json
scripts/scherzoctl events <session-id> --json
scripts/scherzoctl events <session-id> --json --since-cursor <n>
scripts/scherzoctl attach --json --no-follow <session-id>
```

`ping`, `ps`, `session`, and `events` are bounded commands. `events <session-id> --json` returns one JSON event page and is the normal choice for summaries. `attach --json` prints one JSON stream object per event and follows by default, so use `--no-follow` for bounded replay unless the user explicitly asks for live watching.

## Session target selection

Use full session ids from `ps --json` or `session --json`; human tables shorten long ids. Top-level issue sessions are workflow run ids such as `ABC-123-1700000000000-1`. Workflow DAG steps create step sessions whose ids start with `workflow-step-`, for example `workflow-step-ABC-123-1700000000000-1-implement-a1-<hash>`.

Before sending `prompt`, `stop-after-turn`, `abort`, or `ui respond`, disambiguate whether the user means the top-level issue run or a concrete step session:

- `prompt`, `stop-after-turn`, and `ui respond` sent to a top-level YAML workflow session route to the active step only when exactly one step currently accepts operator commands; if multiple step sessions are active, target the step session.
- `abort` on a top-level issue session stops the whole workflow run and its step sessions. `abort` on a step session sends the abort to that step command subject when available.
- Command steps do not run pi, but they still get `workflow-step-...` sessions and failure events.

## Step-level workflow retry

Prefer `retry-step` over whole-task `retry` when a workflow failed or was interrupted after completing useful upstream steps. It preserves completed upstream attempts and retries the selected failed/interrupted step plus downstream descendants.

Select targets from inspected session/events/artifacts:

- `retry-step ABC-123` selects the latest repairable failed/interrupted run for an issue identifier.
- `retry-step id:<issue-id>` targets by Linear issue id.
- `retry-step run:<run-id>` targets a specific retained workflow run and is the safest choice when multiple failed runs may match.
- Add `--step <step-id>` if multiple failed/interrupted step boundaries exist, or when the user names a specific step.

`retry-step` may be rejected if no failed/interrupted run or step is repairable, the issue already has an active/pending workflow, the issue is parked, workflow or issue fingerprint drift is detected, the selected step is not failed/interrupted, or upstream artifacts/workspace recovery fail. Report that reason; only fall back to full `retry` when step repair is unavailable, unsafe, rejected in a way a full retry can address, or explicitly requested.

## Operator controls

Use exact issue ids, run ids, step ids, session ids, and request ids from JSON inspection. Commands with `--yes` are destructive confirmations at the CLI layer.

```sh
scripts/scherzoctl pause --json
scripts/scherzoctl resume --json
scripts/scherzoctl reload --json
scripts/scherzoctl retry-step ABC-123 --json
scripts/scherzoctl retry-step run:<run-id> --step <step-id> --json
scripts/scherzoctl retry ABC-123 --json
scripts/scherzoctl park ABC-123 --reason "manual cleanup" --yes --json
scripts/scherzoctl unpark ABC-123 --json
scripts/scherzoctl abort <session-id> --yes --json
scripts/scherzoctl stop-after-turn <session-id> --yes --json
scripts/scherzoctl prompt <session-id> "summarize progress" --json
scripts/scherzoctl ui respond <session-id> <request-id> --cancel --json
scripts/scherzoctl ui respond <session-id> <request-id> --value "approved" --json
```

A successful whole-task `retry` response acknowledges acceptance of the retry intent after synchronous safety checks. Later claim, Linear reporting, workspace setup, worker start, or run failures are reported through normal ledger/session/failure evidence; inspect `ps`, `session`, and `events` when a retry was accepted but does not later run successfully.

## JSON response handling

For daemon inspection/control commands, `--json` returns one protocol JSON document with a non-secret `target` object. Check `target.control_file_path` and `target.workspace_root` before trusting or mutating a daemon. `ok: true` means the control server accepted and decoded the request. `ok: false` means the request failed before a command result could be applied, such as authentication failure, timeout, malformed request, or protocol error; report `error.code` and `error.message` without exposing secrets.

Daemon controls return command data with one of these statuses:

- `applied`: the daemon applied the action immediately.
- `queued`: the daemon accepted the action and queued it for a worker or session.
- `rejected`: the daemon understood the request but refused it; report the reason.
- `not_found`: the target issue, session, or UI request was not found.
- `not_allowed`: the target exists but the requested action is not allowed in its current state; report the reason.

When reporting results, summarize the status and user-relevant message. Do not print control tokens, `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, or large raw event payloads unless the user explicitly asks for a raw excerpt.

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

## Local cleanup and offline state maintenance

Start read-only. Relative `--root` paths are resolved from the directory where `scripts/scherzoctl` was invoked:

```sh
scripts/scherzoctl cleanup --json --dry-run
scripts/scherzoctl cleanup --root .scherzo/workspaces --json --dry-run
scripts/scherzoctl state status --root .scherzo/workspaces --json
```

`cleanup --dry-run` reports `would_delete`, `retained`, `warnings`, `roots`, and `transcript_root_status` and deletes nothing. `state status` is read-only and reports `current`, `unsupported`, `corrupt`, `missing`, or `archived`. Cleanup/state JSON is local maintenance output, not a daemon protocol envelope with `ok`.

Run local cleanup or offline state mutations with explicit `--yes` so the CLI receives an intentional destructive-operation acknowledgement:

```sh
scripts/scherzoctl cleanup --json --yes
scripts/scherzoctl state archive-old --root .scherzo/workspaces --yes --json
scripts/scherzoctl state discard-old --root .scherzo/workspaces --yes --json
scripts/scherzoctl state reinitialize --root .scherzo/workspaces --yes --json
```

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
