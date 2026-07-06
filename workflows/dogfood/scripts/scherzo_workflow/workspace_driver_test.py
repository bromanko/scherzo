from __future__ import annotations

import subprocess
from typing import Sequence

import pytest

from scherzo_workflow import workspace_driver


def completed(stdout: str, *, stderr: str = "", returncode: int = 0) -> subprocess.CompletedProcess[str]:
    return subprocess.CompletedProcess(["driver"], returncode, stdout, stderr)


def test_run_json_uses_configured_driver_and_validates_version(monkeypatch) -> None:  # type: ignore[no-untyped-def]
    calls: list[list[str]] = []

    def fake_run(args: Sequence[str]) -> subprocess.CompletedProcess[str]:
        calls.append(list(args))
        return completed('{"version":1,"ok":true}\n')

    monkeypatch.setenv("SCHERZO_WORKSPACE_DRIVER", "driver-bin")

    payload, proc = workspace_driver.run_json(["status", "--json"], run_proc=fake_run)

    assert calls == [["driver-bin", "status", "--json"]]
    assert payload == {"version": 1, "ok": True}
    assert proc.returncode == 0


def test_run_json_reports_missing_driver(monkeypatch) -> None:  # type: ignore[no-untyped-def]
    monkeypatch.delenv("SCHERZO_WORKSPACE_DRIVER", raising=False)

    with pytest.raises(workspace_driver.WorkspaceDriverError) as raised:
        workspace_driver.run_json(["status", "--json"], run_proc=lambda _args: completed("{}"))

    assert raised.value.code == "workspace_driver_unavailable"


def test_run_json_reports_malformed_json(monkeypatch) -> None:  # type: ignore[no-untyped-def]
    monkeypatch.setenv("SCHERZO_WORKSPACE_DRIVER", "driver-bin")

    with pytest.raises(workspace_driver.WorkspaceDriverError) as raised:
        workspace_driver.run_json(
            ["status", "--json"],
            run_proc=lambda _args: completed("not-json"),
            failure_code="bad_driver_json",
        )

    assert raised.value.code == "bad_driver_json"
    assert "malformed JSON" in str(raised.value)


def test_run_json_uses_payload_failure_code_on_nonzero(monkeypatch) -> None:  # type: ignore[no-untyped-def]
    monkeypatch.setenv("SCHERZO_WORKSPACE_DRIVER", "driver-bin")

    with pytest.raises(workspace_driver.WorkspaceDriverError) as raised:
        workspace_driver.run_json(
            ["publish", "--json"],
            run_proc=lambda _args: completed(
                '{"version":1,"message":"nope","failure_code":"publish_blocked"}',
                returncode=1,
            ),
        )

    assert raised.value.code == "publish_blocked"
    assert "nope" in str(raised.value)


def test_changed_file_records_can_validate_status_and_old_path() -> None:
    records = workspace_driver.changed_file_records(
        {
            "files": [
                {"path": " new.txt ", "status": "added"},
                {"path": "renamed.txt", "status": "renamed", "old_path": "old.txt"},
            ],
        },
        validate_status=True,
    )

    assert records == [
        {"path": "new.txt", "status": "added"},
        {"path": "renamed.txt", "status": "renamed", "old_path": "old.txt"},
    ]


def test_diff_text_requires_text_and_truncated() -> None:
    assert workspace_driver.diff_text({"text": "diff", "truncated": False}) == "diff"
    with pytest.raises(workspace_driver.WorkspaceDriverError):
        workspace_driver.diff_text({"text": "diff"})
