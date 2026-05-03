---
name: scherzo-operator
description: Operate a running Scherzo daemon from pi by using scherzoctl to inspect sessions, summarize worker progress, and perform confirmed operator controls. Use when the user asks to inspect Scherzo, summarize sessions, attach to workers, pause or resume dispatch, abort or stop workers, send follow-up prompts, retry, park or unpark issues, or answer Scherzo UI requests.
---

# Scherzo Operator

Use this skill when the user wants pi to inspect or operate a running Scherzo daemon from this repository. Scherzo control is local to the daemon host and all real actions must go through `scripts/scherzoctl` with machine-readable `--json` output whenever possible.

See [the command reference](references/commands.md) for exact `scherzoctl` command shapes and response-status meanings.

## Operating policy

Start read-only. Before proposing any intervention, inspect the daemon with `scripts/scherzoctl ps --json`, then use `scripts/scherzoctl session <session-id> --json` and `scripts/scherzoctl events <session-id> --json` for session detail. Prefer `events --json` for summaries instead of `attach` because `attach` follows live output by default and can keep a tool call open. Use `attach --json --no-follow <session-id>` only for bounded replay, or live `attach` only when the user explicitly asks to watch a stream.

Find the control file safely. If the user gives a path, pass it after the subcommand with `--control-file <path>`. Otherwise honor `SCHERZO_CONTROL_FILE`. If neither is available and the repository default `.scherzo/workspaces/.scherzo-state/control.json` exists, use that path. Do not scan broad filesystem locations for control files. If the control file is missing, ask the user for the `control_server_started` log line or an explicit control file path.

Use command-first forms. The Scherzo subcommand comes immediately after `scripts/scherzoctl`, and options such as `--json` and `--control-file <path>` come after that subcommand, for example `scripts/scherzoctl ps --json --control-file .scherzo/workspaces/.scherzo-state/control.json`.

Require confirmation before every state-changing command. Ask the user to confirm the exact target id and action before running `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, or any command that uses `--yes`. Treat this as mandatory even when a command seems harmless.

Never reveal secrets. The control file contains a `token`; never reveal token values or paste the raw control file. Never print `LINEAR_API_KEY` or other API keys. Summarize raw JSON and event payloads by default, redact sensitive issue or repository content, and quote raw excerpts only when the user explicitly asks for them.

Report outcomes clearly. For read-only commands, summarize active sessions, issues, workflow steps, current status, last event age, and any blocking UI request. For confirmed mutating commands, run the JSON command once, then report whether the response had `ok: true` or `ok: false` and include the command result status such as `applied`, `queued`, `rejected`, `not_found`, or `not_allowed` without exposing secrets.
