# Scherzo operator command reference

Use this reference from the `scherzo-operator` skill when you need exact `scherzoctl` command shapes. Run commands from the repository root. Prefer the wrapper because it enters the repository toolchain for you.

## Control file selection

If the user provides a control file, put `--control-file <path>` after the Scherzo subcommand:

```sh
scripts/scherzoctl ps --json --control-file .scherzo/workspaces/.scherzo-state/control.json
```

If the wrapper is unavailable, use the same subcommand ordering through Gleam:

```sh
direnv exec . gleam run -- ctl ps --json
```

You may also honor `SCHERZO_CONTROL_FILE` from the environment. Do not search outside the repository for control files.

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

`ping`, `ps`, `session`, and `events` are bounded commands. `events <session-id> --json` returns one JSON event page and is the normal choice for summaries. `attach --json` prints one JSON stream envelope per event and follows by default, so use `--no-follow` for bounded replay unless the user explicitly asks for live watching.

## Confirmed operator controls

Ask the user to confirm the exact action and target before running any command in this section. Commands with `--yes` are destructive confirmations at the CLI layer, but the skill still needs user confirmation before invoking them.

```sh
scripts/scherzoctl pause --json
scripts/scherzoctl resume --json
scripts/scherzoctl reload --json
scripts/scherzoctl retry ABC-123 --json
scripts/scherzoctl park ABC-123 --reason "manual cleanup" --yes --json
scripts/scherzoctl unpark ABC-123 --json
scripts/scherzoctl abort <session-id> --yes --json
scripts/scherzoctl stop-after-turn <session-id> --yes --json
scripts/scherzoctl prompt <session-id> "summarize progress" --json
scripts/scherzoctl ui respond <session-id> <request-id> --cancel --json
scripts/scherzoctl ui respond <session-id> <request-id> --value "approved" --json
```

Use full session ids from `ps --json` or `session --json`. YAML workflow runs may create step sessions such as `ABC-123-42-1-implement`; confirm whether the user means the top-level issue session or the active step session before sending `prompt`, `abort`, `stop-after-turn`, or `ui respond`.

## JSON response handling

For non-streaming commands, `--json` returns one protocol JSON document. A protocol response with `ok: true` means the control server accepted and decoded the request. A protocol response with `ok: false` means the request failed before a command result could be applied, such as authentication failure, timeout, malformed request, or protocol error.

Confirmed operator controls return a command result with one of these statuses:

- `applied`: the daemon applied the action immediately.
- `queued`: the daemon accepted the action and queued it for a worker or session.
- `rejected`: the daemon understood the request but refused it; report the reason.
- `not_found`: the target issue, session, or UI request was not found.
- `not_allowed`: the target exists but the requested action is not allowed in its current state; report the reason.

When reporting results, summarize the status and user-relevant message. Do not print control tokens, `LINEAR_API_KEY`, or large raw event payloads unless the user explicitly asks for a raw excerpt.
