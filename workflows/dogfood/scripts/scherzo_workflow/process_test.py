from __future__ import annotations

import sys

from scherzo_workflow import process


def test_diagnostic_excerpt_keeps_head_and_tail() -> None:
    excerpt = process.diagnostic_excerpt("a" * 80 + "tail", max_chars=50)

    assert "truncated" in excerpt
    assert excerpt.startswith("a")
    assert excerpt.endswith("tail")


def test_shell_and_command_display_quote_only_when_needed() -> None:
    assert process.shell_display("safe/path-1") == "safe/path-1"
    assert process.shell_display("two words") == "'two words'"
    assert process.shell_display("it's") == "'it'\\''s'"
    assert process.command_display(["cmd", "two words"]) == "cmd 'two words'"


def test_run_proc_captures_text_output() -> None:
    proc = process.run_proc([sys.executable, "-c", "print('ok')"])

    assert proc.returncode == 0
    assert proc.stdout == "ok\n"
    assert proc.stderr == ""


def test_subprocess_failure_details_includes_streams() -> None:
    details = process.subprocess_failure_details("out", "err", 2)

    assert "exit_code: 2" in details
    assert "stderr:\nerr" in details
    assert "stdout:\nout" in details
