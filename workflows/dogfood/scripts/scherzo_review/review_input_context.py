"""Shared input artifact helpers for native Scherzo review lanes."""

from __future__ import annotations

from typing import Any

MAX_CONTEXT_SNAPSHOT_BYTES = 40_000


def _file_attr(file: Any, name: str, default: Any = None) -> Any:
    return getattr(file, name, default)


def changed_file_entry(file: Any) -> dict[str, Any]:
    path = str(_file_attr(file, "path", ""))
    return {
        "path": path,
        "previous_path": _file_attr(file, "old_path", None),
        "change_kind": str(_file_attr(file, "change_kind", "modified")),
        "language": language_for_path(path),
        "subsystem": subsystem_for_path(path),
        "additions": int(_file_attr(file, "additions", 0) or 0),
        "deletions": int(_file_attr(file, "deletions", 0) or 0),
        "hunks": int(_file_attr(file, "hunks", 0) or 0),
        "hunk_headers": list(_file_attr(file, "hunk_headers", []) or []),
        "added_line_samples": list(_file_attr(file, "added_lines", []) or [])[:20],
        "deleted_line_samples": list(_file_attr(file, "deleted_lines", []) or [])[:20],
    }


def language_for_path(path: str) -> str:
    lower = path.lower()
    suffixes = {
        ".gleam": "gleam",
        ".erl": "erlang",
        ".hrl": "erlang",
        ".py": "python",
        ".ts": "typescript",
        ".tsx": "typescript",
        ".js": "javascript",
        ".jsx": "javascript",
        ".rs": "rust",
        ".go": "go",
        ".fs": "fsharp",
        ".fsx": "fsharp",
        ".elm": "elm",
        ".md": "markdown",
        ".yaml": "yaml",
        ".yml": "yaml",
        ".json": "json",
        ".toml": "toml",
        ".nix": "nix",
        ".sh": "shell",
    }
    for suffix, language in suffixes.items():
        if lower.endswith(suffix):
            return language
    if "/scripts/" in lower or lower.startswith("scripts/"):
        return "script"
    return "other"


def subsystem_for_path(path: str) -> str:
    normalized = path.strip("/")
    if normalized.startswith("src/scherzo/"):
        parts = normalized.split("/")
        if len(parts) > 2:
            return "runtime:" + parts[2].removesuffix(".gleam")
        return "runtime"
    if normalized.startswith("test/"):
        return "tests"
    if normalized.startswith("docs/"):
        return "documentation"
    if normalized.startswith("workflows/") or normalized.startswith(".scherzo/workflows/"):
        return "workflow"
    if normalized.startswith("scripts/"):
        return "scripts"
    if normalized.startswith("nix/") or normalized.endswith(".nix"):
        return "nix"
    return "repository"
