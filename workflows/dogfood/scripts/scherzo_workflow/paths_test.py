from __future__ import annotations

from pathlib import Path

from scherzo_workflow import paths


def test_env_value_prefers_primary_then_legacy_then_default() -> None:
    env = {"PRIMARY": "", "LEGACY": " old "}

    assert paths.env_value("PRIMARY", "default", legacy="LEGACY", environ=env) == "old"
    assert paths.env_value("MISSING", "default", environ=env) == "default"
    assert paths.env_enabled("LEGACY", environ=env)


def test_resolve_run_root_relative_state_path() -> None:
    env = {"SCHERZO_RUN_ROOT": "/tmp/run"}

    assert paths.resolve_run_root_relative_path(Path("state/x.json"), environ=env) == Path(
        "/tmp/run/state/x.json"
    )
    assert paths.resolve_run_root_relative_path(Path("tmp/x.json"), environ=env) == Path(
        "tmp/x.json"
    )
    assert paths.resolve_run_root_relative_path(Path("/abs/x.json"), environ=env) == Path(
        "/abs/x.json"
    )


def test_resolve_legacy_workspace_state_prefix_when_requested() -> None:
    env = {"SCHERZO_RUN_ROOT": "/tmp/run"}

    assert paths.resolve_run_root_relative_path(
        Path("workspaces/main/state/x.json"),
        environ=env,
        support_workspace_state_prefix=True,
    ) == Path("/tmp/run/state/x.json")


def test_schema_path_accepts_portable_scherzo_workflow_prefix(tmp_path) -> None:  # type: ignore[no-untyped-def]
    bundle_root = tmp_path / "bundle"
    bundle_root.mkdir()

    assert paths.schema_path(
        Path(".scherzo/workflows/schemas/example.json"),
        bundle_root,
    ) == bundle_root / "schemas/example.json"
