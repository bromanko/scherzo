from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from scherzo_workflow import json_io


def test_canonical_json_is_sorted_pretty_and_newline_terminated() -> None:
    assert json_io.canonical_json({"b": 1, "a": {"z": 2}}) == (
        '{\n  "a": {\n    "z": 2\n  },\n  "b": 1\n}\n'
    )


def test_write_and_load_json_with_resolver(tmp_path) -> None:  # type: ignore[no-untyped-def]
    root = tmp_path / "root"

    def resolver(path):  # type: ignore[no-untyped-def]
        return root / path

    path = Path("nested/value.json")
    json_io.write_json(path, {"ok": True}, resolver=resolver)

    assert (root / "nested/value.json").is_file()
    assert json_io.load_json(path, resolver=resolver, expected_type=dict) == {
        "ok": True,
    }


def test_load_json_reports_invalid_json(tmp_path) -> None:  # type: ignore[no-untyped-def]
    path = tmp_path / "bad.json"
    path.write_text("{", encoding="utf-8")

    with pytest.raises(json_io.JsonIoError) as raised:
        json_io.load_json(path)

    assert "invalid JSON" in str(raised.value)


def test_sha256_helpers_share_hashing_logic(tmp_path) -> None:  # type: ignore[no-untyped-def]
    path = tmp_path / "payload.txt"
    path.write_text("hello", encoding="utf-8")
    expected = hashlib.sha256(b"hello").hexdigest()

    assert json_io.sha256_text("hello") == expected
    assert json_io.sha256_bytes(b"hello") == expected
    assert json_io.sha256_file(path) == expected
    assert json_io.file_meta(path) == {"sha256": expected, "bytes": 5}
