from __future__ import annotations

import pytest

from scherzo_workflow import errors


def test_failure_lines_include_optional_code() -> None:
    assert errors.failure_lines("tool", "nope", code="bad") == [
        "SCHERZO_FAILURE_CODE=bad",
        "tool: nope",
    ]


def test_exit_with_failure_prints_and_exits(capsys: pytest.CaptureFixture[str]) -> None:
    with pytest.raises(SystemExit) as raised:
        errors.exit_with_failure("tool", "stopped", code="stop", exit_code=7)

    assert raised.value.code == 7
    assert capsys.readouterr().err == "SCHERZO_FAILURE_CODE=stop\ntool: stopped\n"
