"""JSON Schema validation helpers for workflow-bundle scripts."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

from . import json_io
from . import paths

try:  # pragma: no cover - absence is surfaced through require_jsonschema.
    import jsonschema
except Exception:  # pragma: no cover
    jsonschema = None  # type: ignore[assignment]


@dataclass(frozen=True)
class SchemaValidationError(Exception):
    """A user-facing JSON Schema validation failure."""

    message: str

    def __str__(self) -> str:
        return self.message


def require_jsonschema() -> Any:
    if jsonschema is None:
        raise SchemaValidationError("python jsonschema package is required")
    return jsonschema


def schema_path(path: Path, root: Path) -> Path:
    return paths.schema_path(path, root)


def validate_schema(value: Any, schema_rel: Path, *, root: Path) -> None:
    """Validate a JSON value against a schema path resolved from ``root``."""

    jsonschema_module = require_jsonschema()
    schema_file = schema_path(schema_rel, root)
    try:
        schema = json_io.load_json(schema_file)
    except json_io.JsonIoError as exc:
        raise SchemaValidationError(str(exc)) from exc
    validator = jsonschema_module.Draft202012Validator(schema)
    errors = sorted(validator.iter_errors(value), key=lambda error: list(error.path))
    if errors:
        first = errors[0]
        location = "/".join(str(part) for part in first.path) or "<root>"
        raise SchemaValidationError(
            f"schema validation failed for {schema_rel} at {location}: {first.message}"
        )
