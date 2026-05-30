Title: `doctor --check workflow-config` falsely times out on `describe --json` after successful stdout

## Summary

`scherzo doctor --check workflow-config` can fail with:

- `workspace_driver_discovery_failed`
- `describe --json timed out`

even when the workspace driver's `describe --json` command succeeds immediately outside Scherzo.

In the failing case, the driver prints valid JSON in about `0.05s`, but Scherzo does not return for about `61s` and then reports a timeout.

This appears to be a bug in Scherzo's port/process shutdown logic during workspace-driver discovery, not a bug in the driver itself.

## Environment

- Scherzo revision: `c877a6d5a7cf415056a36de7522f970efd346125`
- Observed on: `2026-05-29` and `2026-05-30`
- OS: Linux (exe.dev VM)

## Symptom

Running:

```bash
cd /home/exedev/code/shoreline-web
set -a
source .env
set +a
/usr/bin/time -f 'EXIT:%x ELAPSED:%e' timeout 75s scherzo doctor --check workflow-config .scherzo/scherzo.yaml
```

produces:

```text
Scherzo doctor
Config: .scherzo/scherzo.yaml

✗ Workflow config
  Problem: workspace driver discovery failed for profile shoreline command /home/exedev/code/shoreline-web/.scherzo/bin/shoreline-workspace-driver: describe --json timed out

  Code: workspace_driver_discovery_failed
  Impact: Scherzo cannot safely start because config, workflow DAGs, or prompt templates did not load.
  Try:
    - Confirm the YAML path is correct and ends in .yaml or .yml.
    - Confirm LINEAR_API_KEY and any referenced environment variables are set.
    - Confirm routed workflow DAG and prompt-template files exist.

Summary: 0 passed, 0 warnings, 1 failed, 0 skipped

Not ready.
EXIT:1 ELAPSED:61.41
```

## Expected Behavior

If the driver returns valid `describe --json` output and exits successfully, `doctor --check workflow-config` should pass quickly.

## Actual Behavior

The driver returns valid output immediately, but Scherzo still waits until the full timeout window and then reports `describe --json timed out`.

## Direct Reproduction Evidence

The exact discovery command Scherzo uses succeeds immediately outside Scherzo:

```bash
cd /home/exedev/code/shoreline-web
/usr/bin/time -f 'ELAPSED:%e EXIT:%x' \
  env -i \
    PATH="$PATH" \
    SCHERZO_GITHUB_REPO=Shoreline-Medical-Admin/shoreline-web \
    SCHERZO_GIT_BASE_BRANCH=main \
    SCHERZO_GIT_REMOTE=origin \
    SCHERZO_CONFIG_DIR=/home/exedev/code/shoreline-web/.scherzo \
    SCHERZO_REPO_ROOT=/home/exedev/code/shoreline-web \
    SCHERZO_WORKSPACE_DRIVER=/home/exedev/code/shoreline-web/.scherzo/bin/shoreline-workspace-driver \
    /home/exedev/code/shoreline-web/.scherzo/bin/shoreline-workspace-driver describe --json
```

Output:

```text
{"version": 1, "capabilities": ["status", "diff", "changed-files", "assert-only", "baseline", "refresh-base", "publish-change"]}
ELAPSED:0.05 EXIT:0
```

Running the same driver under `bash -lc` also succeeds immediately:

```bash
cd /home/exedev/code/shoreline-web
/usr/bin/time -f 'ELAPSED:%e EXIT:%x' \
  bash -lc '/home/exedev/code/shoreline-web/.scherzo/bin/shoreline-workspace-driver describe --json'
```

Output:

```text
{"version": 1, "capabilities": ["status", "diff", "changed-files", "assert-only", "baseline", "refresh-base", "publish-change"]}
ELAPSED:0.03 EXIT:0
```

## Likely Faulting Code Path

Workspace-driver discovery reads one stdout line successfully, then waits for the process to exit:

- `src/scherzo/workspace_driver_discovery.gleam`
  - `read_description(...)`
  - `wait_for_description(...)`

Relevant lines:

- `src/scherzo/workspace_driver_discovery.gleam:188`
- `src/scherzo/workspace_driver_discovery.gleam:249`

That path delegates to the port wrapper:

- `src/scherzo_port_ffi.erl`

Relevant lines:

- `src/scherzo_port_ffi.erl:439`
- `src/scherzo_port_ffi.erl:489`
- `src/scherzo_port_ffi.erl:511`
- `src/scherzo_port_ffi.erl:598`

The suspicious logic is:

```erlang
child_target_alive({ok, ChildPid}) ->
    pid_alive(ChildPid) orelse process_group_alive(ChildPid);
```

and:

```erlang
process_group_alive(Pid) ->
    case os:cmd("/bin/kill -0 -" ++ integer_to_list(Pid) ++ " >/dev/null 2>&1 && printf alive || true") of
        "alive" -> true;
        _ -> false
    end.
```

## Why This Looks Wrong

Based on tracing:

1. The launched driver emits valid `describe --json` output.
2. Scherzo then repeatedly probes child PID / process-group liveness with `kill -0`.
3. That post-stdout wait lasts until the full timeout window.
4. Only then does Scherzo report `describe --json timed out`.

So the timeout is not in command execution or stdout production. It is in Scherzo's "wait until launched process is gone" logic after successful stdout.

## `strace` Behavior

Live `strace` of the BEAM process showed repeated shell-port probes of the form:

```text
(/bin/kill -0 703093 >/dev/null 2>&1 && printf alive || true)
(/bin/kill -0 -703093 >/dev/null 2>&1 && printf alive || true)
```

This loop continued during the apparent hang.

## Suspected Root Cause

False-positive process-group liveness for short-lived commands launched through the Bash wrapper.

In other words:

- the command has already produced output and exited
- but `await_exit` still considers the launched process alive because `process_group_alive(ChildPid)` remains true long enough to exhaust the timeout

## Suggested Fix Direction

One of:

1. Tighten `await_exit` / `child_target_alive` so fast-exiting commands are not held open by process-group checks after successful stdout.
2. Avoid `process_group_alive(ChildPid)` in this path, or only use it when explicit background descendants are expected.
3. Special-case workspace-driver discovery after `describe --json` line receipt so it does not depend on long process-group shutdown semantics.

## Suggested Regression Tests

### Low-level port test

Add a test in `test/port_test.gleam` that:

1. launches a fast-exit command through the same port wrapper path
2. reads one stdout line
3. asserts `port.await_exit(process, 1000) == Ok(0)`

### Higher-level workspace-driver discovery test

Add a test in `test/workspace_driver_discovery_test.gleam` that:

1. runs a valid `describe --json` helper that prints one JSON line and exits immediately
2. asserts capability discovery succeeds quickly rather than timing out

## Notes

- This initially looked like "`doctor` hangs", but the longer run showed it is a slow false timeout rather than a total deadlock.
- `pi-probe` was not the primary problem in this case; it was skipped because `workflow-config` failed first.
