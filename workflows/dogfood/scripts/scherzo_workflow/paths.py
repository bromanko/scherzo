"""Environment and path resolution helpers for workflow-bundle scripts."""

from __future__ import annotations

import os
from pathlib import Path
from typing import Mapping


def env_value(
    name: str,
    default: str = "",
    *,
    legacy: str | None = None,
    environ: Mapping[str, str] | None = None,
) -> str:
    env = os.environ if environ is None else environ
    value = env.get(name, "").strip()
    if value:
        return value
    if legacy:
        value = env.get(legacy, "").strip()
        if value:
            return value
    return default


def env_enabled(
    name: str,
    *,
    legacy: str | None = None,
    environ: Mapping[str, str] | None = None,
) -> bool:
    return bool(env_value(name, legacy=legacy, environ=environ))


def run_root(environ: Mapping[str, str] | None = None) -> Path | None:
    env = os.environ if environ is None else environ
    value = env.get("SCHERZO_RUN_ROOT", "").strip()
    if not value:
        return None
    return Path(value)


def repo_root(default: str | None = None, *, environ: Mapping[str, str] | None = None) -> Path:
    if default:
        return Path(default)
    env = os.environ if environ is None else environ
    value = env.get("SCHERZO_REPO_ROOT", "").strip()
    if value:
        return Path(value)
    return Path.cwd()


def resolve_run_root_relative_path(
    path: Path,
    *,
    environ: Mapping[str, str] | None = None,
    support_workspace_state_prefix: bool = False,
) -> Path:
    """Resolve canonical run-root state paths used by workflow commands.

    ``state/...`` paths are rooted under ``SCHERZO_RUN_ROOT`` when present.  Some
    legacy ExecPlan artifacts also spell run-root state as
    ``workspaces/<id>/state/...``; callers can opt in to resolving that form too.
    """

    root = run_root(environ)
    if root is None or path.is_absolute():
        return path
    if path.parts[:1] == ("state",):
        return root / path
    if (
        support_workspace_state_prefix
        and len(path.parts) >= 3
        and path.parts[0] == "workspaces"
        and path.parts[2] == "state"
    ):
        return root.joinpath(*path.parts[2:])
    return path


def display_path(path: Path, *, environ: Mapping[str, str] | None = None) -> str:
    if not path.is_absolute():
        return path.as_posix()
    root = run_root(environ)
    if root is not None:
        try:
            return path.resolve(strict=False).relative_to(root.resolve(strict=False)).as_posix()
        except (OSError, ValueError):
            pass
    return path.as_posix()


def schema_path(path: Path, root: Path) -> Path:
    """Resolve schema paths in a portable checked-in workflow bundle."""

    if path.is_absolute():
        return path
    parts = path.parts
    if len(parts) >= 2 and parts[0] == ".scherzo" and parts[1] == "workflows":
        if (root / ".scherzo").exists():
            return root / path
        return root / Path(*parts[2:])
    return root / path
