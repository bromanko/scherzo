from __future__ import annotations

from pathlib import Path

import pytest

from scherzo_workflow import json_io
from scherzo_workflow import schema


def test_validate_schema_accepts_valid_payload(tmp_path) -> None:  # type: ignore[no-untyped-def]
    schema_path = tmp_path / "schemas/example.json"
    json_io.write_json(
        schema_path,
        {
            "type": "object",
            "required": ["name"],
            "properties": {"name": {"type": "string"}},
        },
    )

    schema.validate_schema({"name": "ok"}, Path("schemas/example.json"), root=tmp_path)


def test_validate_schema_reports_first_error_location(tmp_path) -> None:  # type: ignore[no-untyped-def]
    schema_path = tmp_path / "schema.json"
    json_io.write_json(
        schema_path,
        {
            "type": "object",
            "properties": {"count": {"type": "integer"}},
        },
    )

    with pytest.raises(schema.SchemaValidationError) as raised:
        schema.validate_schema({"count": "one"}, Path("schema.json"), root=tmp_path)

    assert "at count" in str(raised.value)
    assert "is not of type 'integer'" in str(raised.value)


def test_schema_path_handles_checked_in_workflow_prefix(tmp_path) -> None:  # type: ignore[no-untyped-def]
    assert schema.schema_path(Path(".scherzo/workflows/schemas/x.json"), tmp_path) == (
        tmp_path / "schemas/x.json"
    )
