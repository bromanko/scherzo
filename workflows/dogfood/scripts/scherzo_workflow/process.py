"""Subprocess helpers shared by workflow-bundle scripts."""

from __future__ import annotations

import re
import shutil
import subprocess
import time
from dataclasses import dataclass
from typing import Sequence

COMMAND_DIAGNOSTIC_MAX_CHARS = 3500
SAFE_SHELL_DISPLAY_RE = re.compile(r"^[A-Za-z0-9_./:=+@^-]+$")


@dataclass(frozen=True)
class ProcessError(Exception):
    """A user-facing process helper failure."""

    message: str

    def __str__(self) -> str:
        return self.message


def diagnostic_excerpt(value: str, max_chars: int = COMMAND_DIAGNOSTIC_MAX_CHARS) -> str:
    value = value.strip()
    if len(value) <= max_chars:
        return value
    marker = "\n... truncated ...\n"
    available = max(0, max_chars - len(marker))
    omitted = len(value) - available
    marker = f"\n... truncated {omitted} chars ...\n"
    available = max(0, max_chars - len(marker))
    head_chars = available // 2
    tail_chars = available - head_chars
    return value[:head_chars] + marker + value[-tail_chars:]


def subprocess_failure_details(stdout: str, stderr: str, returncode: int) -> str:
    parts = [f"exit_code: {returncode}"]
    if stderr.strip():
        parts.append("stderr:\n" + diagnostic_excerpt(stderr))
    if stdout.strip():
        parts.append("stdout:\n" + diagnostic_excerpt(stdout))
    return "\n\n".join(parts)


def shell_display(value: str) -> str:
    if SAFE_SHELL_DISPLAY_RE.match(value):
        return value
    return "'" + value.replace("'", "'\\''") + "'"


def command_display(args: Sequence[str]) -> str:
    return " ".join(shell_display(arg) for arg in args)


def subprocess_text(value: str | bytes | None) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return value


def format_seconds(value: float) -> str:
    number = float(value)
    if number.is_integer():
        return str(int(number))
    return str(number)


def run_proc(
    args: Sequence[str],
    *,
    env: dict[str, str] | None = None,
    timeout: float | None = None,
) -> subprocess.CompletedProcess[str]:
    """Run a command, capturing text output.

    A timeout is converted to a CompletedProcess with return code 124 so callers
    can keep their existing command-failure reporting paths.
    """

    try:
        return subprocess.run(
            list(args),
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as exc:
        stdout = subprocess_text(exc.stdout)
        stderr = subprocess_text(exc.stderr).strip()
        timeout_text = format_seconds(timeout or 0.0)
        message = f"command timed out after {timeout_text}s"
        detail = (stderr + "\n" + message).strip() if stderr else message
        return subprocess.CompletedProcess(list(args), 124, stdout, detail)


def run_proc_with_heartbeat(
    args: Sequence[str],
    *,
    env: dict[str, str] | None = None,
    heartbeat_seconds: int = 60,
    label: str = "scherzo-workflow",
) -> subprocess.CompletedProcess[str]:
    proc = subprocess.Popen(
        list(args),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
    )
    started = time.monotonic()
    command = command_display(args)
    while True:
        try:
            stdout, stderr = proc.communicate(timeout=heartbeat_seconds)
            return subprocess.CompletedProcess(
                list(args),
                proc.returncode,
                stdout or "",
                stderr or "",
            )
        except subprocess.TimeoutExpired:
            elapsed = int(time.monotonic() - started)
            print(f"[{label}] still running after {elapsed}s: {command}", flush=True)


def command_exists(name: str) -> bool:
    return shutil.which(name) is not None


def require_command(name: str) -> None:
    if not command_exists(name):
        raise ProcessError(f"required command not found: {name}")
