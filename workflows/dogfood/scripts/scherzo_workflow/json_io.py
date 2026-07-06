"""JSON and hash helpers shared by workflow-bundle scripts."""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, TypeVar

DEFAULT_HASH_CHUNK_BYTES = 65_536
T = TypeVar("T")
PathResolver = Callable[[Path], Path]


@dataclass(frozen=True)
class JsonIoError(Exception):
    """A user-facing JSON/file IO failure."""

    message: str

    def __str__(self) -> str:
        return self.message


def canonical_json(value: Any) -> str:
    """Return the bundle's stable pretty JSON representation."""

    return json.dumps(value, indent=2, sort_keys=True, separators=(",", ": ")) + "\n"


def resolve_path(path: Path, resolver: PathResolver | None = None) -> Path:
    return resolver(path) if resolver else path


def write_json(path: Path, value: Any, *, resolver: PathResolver | None = None) -> None:
    """Create parent directories and write stable UTF-8 JSON."""

    destination = resolve_path(path, resolver)
    destination.parent.mkdir(parents=True, exist_ok=True)
    destination.write_text(canonical_json(value), encoding="utf-8")


def load_json(
    path: Path,
    *,
    resolver: PathResolver | None = None,
    expected_type: type[T] | tuple[type[Any], ...] | None = None,
) -> Any | T:
    """Read UTF-8 JSON, optionally enforcing the top-level Python type."""

    source = resolve_path(path, resolver)
    try:
        value = json.loads(source.read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise JsonIoError(f"missing JSON file: {source}") from exc
    except json.JSONDecodeError as exc:
        raise JsonIoError(f"invalid JSON in {source}: {exc}") from exc
    except OSError as exc:
        raise JsonIoError(f"could not read JSON file {source}: {exc}") from exc

    if expected_type is not None and not isinstance(value, expected_type):
        raise JsonIoError(f"JSON must be {type_label(expected_type)}: {source}")
    return value


def type_label(expected_type: type[Any] | tuple[type[Any], ...]) -> str:
    if isinstance(expected_type, tuple):
        return " or ".join(t.__name__ for t in expected_type)
    return expected_type.__name__


def path_bytes(path: Path, *, resolver: PathResolver | None = None) -> bytes:
    """Read bytes from a path, reporting deterministic user-facing errors."""

    source = resolve_path(path, resolver)
    try:
        return source.read_bytes()
    except FileNotFoundError as exc:
        raise JsonIoError(f"missing file: {source}") from exc
    except OSError as exc:
        raise JsonIoError(f"could not read {source}: {exc}") from exc


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_text(value: str) -> str:
    return sha256_bytes(value.encode("utf-8"))


def sha256_file(
    path: Path,
    *,
    missing_ok: bool = False,
    chunk_bytes: int = DEFAULT_HASH_CHUNK_BYTES,
) -> str | None:
    """Return a file's SHA-256 hex digest.

    When ``missing_ok`` is true, unreadable files return ``None`` to preserve the
    legacy structured-output reference matching behavior.
    """

    try:
        digest = hashlib.sha256()
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(chunk_bytes), b""):
                digest.update(chunk)
        return digest.hexdigest()
    except OSError:
        if missing_ok:
            return None
        raise


def file_meta(path: Path, *, resolver: PathResolver | None = None) -> dict[str, Any]:
    data = path_bytes(path, resolver=resolver)
    return {"sha256": sha256_bytes(data), "bytes": len(data)}
