"""Shared failure formatting for workflow-bundle helper scripts.

The scripts under ``.scherzo/workflows/scripts`` are command-step CLIs.  They
should report deterministic, grep-friendly diagnostics without depending on any
repository code outside the workflow bundle.
"""

from __future__ import annotations

import sys
from dataclasses import dataclass
from typing import NoReturn, TextIO

FAILURE_PREFIX = "SCHERZO_FAILURE_CODE="


@dataclass(frozen=True)
class WorkflowError(Exception):
    """User-facing helper failure with an optional Scherzo failure code."""

    message: str
    code: str | None = None
    exit_code: int = 1

    def __str__(self) -> str:
        return self.message


def failure_lines(
    script_name: str,
    message: str,
    *,
    code: str | None = None,
    failure_prefix: str = FAILURE_PREFIX,
) -> list[str]:
    """Return stderr lines for a script-scoped failure."""

    lines: list[str] = []
    if code:
        lines.append(f"{failure_prefix}{code}")
    lines.append(f"{script_name}: {message}")
    return lines


def print_failure(
    script_name: str,
    message: str,
    *,
    code: str | None = None,
    failure_prefix: str = FAILURE_PREFIX,
    stream: TextIO | None = None,
) -> None:
    """Print a script-scoped failure to stderr or the provided stream."""

    output = stream or sys.stderr
    for line in failure_lines(
        script_name,
        message,
        code=code,
        failure_prefix=failure_prefix,
    ):
        print(line, file=output)


def exit_with_failure(
    script_name: str,
    message: str,
    *,
    code: str | None = None,
    exit_code: int = 1,
    failure_prefix: str = FAILURE_PREFIX,
) -> NoReturn:
    """Print a command-step failure and terminate with ``SystemExit``."""

    print_failure(script_name, message, code=code, failure_prefix=failure_prefix)
    raise SystemExit(exit_code)
