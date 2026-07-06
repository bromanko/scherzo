"""Shared helpers for the Scherzo workspace-driver JSON contract."""

from __future__ import annotations

import json
import os
import subprocess
from dataclasses import dataclass
from typing import Any, Callable, Mapping, Sequence

from . import process

RunProc = Callable[[Sequence[str]], subprocess.CompletedProcess[str]]
MAX_WORKSPACE_DRIVER_DETAIL_CHARS = 4000


@dataclass(frozen=True)
class WorkspaceDriverError(Exception):
    """A user-facing workspace-driver failure."""

    message: str
    code: str = "workspace_driver_failed"
    process: subprocess.CompletedProcess[str] | None = None

    def __str__(self) -> str:
        return self.message


def command(environ: Mapping[str, str] | None = None) -> str | None:
    env = os.environ if environ is None else environ
    value = env.get("SCHERZO_WORKSPACE_DRIVER", "").strip()
    return value or None


def available(environ: Mapping[str, str] | None = None) -> bool:
    return command(environ) is not None


def run_json(
    args: Sequence[str],
    *,
    driver: str | None = None,
    environ: Mapping[str, str] | None = None,
    run_proc: RunProc | None = None,
    check: bool = True,
    failure_code: str = "workspace_driver_failed",
) -> tuple[dict[str, Any], subprocess.CompletedProcess[str]]:
    """Run a workspace-driver command and validate the versioned JSON object."""

    resolved_driver = driver or command(environ)
    if not resolved_driver:
        raise WorkspaceDriverError(
            "SCHERZO_WORKSPACE_DRIVER is required for this driver-backed workflow operation",
            code="workspace_driver_unavailable",
        )

    runner = run_proc or process.run_proc
    proc = runner([resolved_driver, *args])
    arg_text = " ".join(args)
    try:
        payload = json.loads(proc.stdout) if proc.stdout.strip() else None
    except json.JSONDecodeError as exc:
        raise WorkspaceDriverError(
            f"workspace driver returned malformed JSON for `{arg_text}`: {exc}",
            code=failure_code,
            process=proc,
        ) from exc

    if not isinstance(payload, dict):
        details = process.subprocess_failure_details(proc.stdout, proc.stderr, proc.returncode)
        raise WorkspaceDriverError(
            f"workspace driver did not return a JSON object for `{arg_text}`\n{details}",
            code=failure_code,
            process=proc,
        )

    if payload.get("version") != 1:
        raise WorkspaceDriverError(
            f"workspace driver JSON for `{arg_text}` must set version=1",
            code=failure_code,
            process=proc,
        )

    if check and proc.returncode != 0:
        message = str(payload.get("message") or "workspace driver command failed")
        raise WorkspaceDriverError(
            f"workspace driver command failed: `{arg_text}`\n{message}",
            code=str(payload.get("failure_code") or failure_code),
            process=proc,
        )

    return payload, proc


def require_files_list(payload: dict[str, Any], *, label: str = "changed-files") -> list[Any]:
    files = payload.get("files")
    if not isinstance(files, list):
        raise WorkspaceDriverError(
            f"workspace driver {label} JSON must contain a files list",
            code="workspace_driver_changed_files_failed",
        )
    return files


def changed_file_records(
    payload: dict[str, Any],
    *,
    validate_status: bool = False,
    require_status: bool = True,
) -> list[dict[str, str]]:
    """Normalize ``changed-files --json`` records from a validated payload."""

    records: list[dict[str, str]] = []
    for item in require_files_list(payload):
        if not isinstance(item, dict):
            raise WorkspaceDriverError(
                "workspace driver changed-files JSON field 'files' must contain objects",
                code="workspace_driver_changed_files_failed",
            )
        path = item.get("path")
        status = item.get("status")
        if not isinstance(path, str) or not path.strip():
            raise WorkspaceDriverError(
                "workspace driver changed-files record is missing a non-empty path",
                code="workspace_driver_changed_files_failed",
            )
        if require_status and not isinstance(status, str):
            raise WorkspaceDriverError(
                "workspace driver changed-files entries must include path and status strings",
                code="workspace_driver_changed_files_failed",
            )
        if validate_status and status not in {"added", "modified", "deleted", "renamed", "conflicted"}:
            raise WorkspaceDriverError(
                f"workspace driver changed-files record for {path!r} has invalid status {status!r}",
                code="workspace_driver_changed_files_failed",
            )
        record = {"path": path.strip()}
        if isinstance(status, str):
            record["status"] = status
        old_path = item.get("old_path")
        if isinstance(old_path, str) and old_path.strip():
            record["old_path"] = old_path.strip()
        records.append(record)
    return records


def diff_text(payload: dict[str, Any]) -> str:
    text = payload.get("text")
    truncated = payload.get("truncated")
    if not isinstance(text, str) or not isinstance(truncated, bool):
        raise WorkspaceDriverError(
            "workspace driver diff JSON must include text and truncated fields",
            code="workspace_driver_diff_failed",
        )
    return text
