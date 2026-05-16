import importlib.machinery
import importlib.util
import json
import os
import tempfile
import unittest
from contextlib import contextmanager
from pathlib import Path
from unittest.mock import patch


SCRIPT_PATH = Path(__file__).resolve().parents[2] / "scripts" / "scherzo-execplan-v2"


def load_module():
    loader = importlib.machinery.SourceFileLoader("scherzo_execplan_v2", str(SCRIPT_PATH))
    spec = importlib.util.spec_from_loader("scherzo_execplan_v2", loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


@contextmanager
def chdir(path: Path):
    previous = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(previous)


class ResolveStructuredSubmissionTests(unittest.TestCase):
    def test_resolves_workspace_scoped_structured_output_from_run_root_fallback(self):
        module = load_module()
        run_id = "LIV-311-1778903143150-1"
        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            run_root = repo_root / ".scherzo" / "workspaces" / "execplan-v2" / "LIV-311" / run_id
            workspace_main = run_root / "workspaces" / "main"
            artifact_dir = repo_root / ".scherzo" / "workspaces" / ".scherzo-state" / "artifacts" / "runs" / run_id
            structured_dir = artifact_dir / "incorporate_review" / "attempt-2" / "structured"
            structured_dir.mkdir(parents=True)
            (structured_dir / "implementation_pack_submission.json").write_text(
                json.dumps(
                    {
                        "artifact_name": "implementation_pack_submission",
                        "payload": {"artifact_type": "implementation_pack_submission", "sections": {}, "conflict_policy": "keep"},
                    }
                ),
                encoding="utf-8",
            )
            workspace_main.mkdir(parents=True)

            env = {
                "SCHERZO_RUN_ID": run_id,
                "SCHERZO_REPO_ROOT": str(repo_root),
                "SCHERZO_RUN_ROOT": str(run_root),
            }
            with patch.dict(os.environ, env, clear=False), chdir(workspace_main):
                resolved = module.resolve_structured_submission("incorporate_review", "implementation_pack_submission")

            self.assertEqual(resolved.resolve(), (structured_dir / "implementation_pack_submission.json").resolve())

    def test_resolves_run_artifact_ref_from_run_root_fallback(self):
        module = load_module()
        run_id = "LIV-311-1778903143150-1"
        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            run_root = repo_root / ".scherzo" / "workspaces" / "execplan-v2" / "LIV-311" / run_id
            workspace_main = run_root / "workspaces" / "main"
            artifact_file = (
                repo_root
                / ".scherzo"
                / "workspaces"
                / ".scherzo-state"
                / "artifacts"
                / "runs"
                / run_id
                / "outputs"
                / "implementation_pack.json"
            )
            artifact_file.parent.mkdir(parents=True)
            artifact_file.write_text("{}\n", encoding="utf-8")
            workspace_main.mkdir(parents=True)

            env = {
                "SCHERZO_RUN_ID": run_id,
                "SCHERZO_REPO_ROOT": str(repo_root),
                "SCHERZO_RUN_ROOT": str(run_root),
            }
            with patch.dict(os.environ, env, clear=False), chdir(workspace_main):
                resolved = module.resolve_artifact_ref(f"runs/{run_id}/outputs/implementation_pack.json")

            self.assertEqual(resolved.resolve(), artifact_file.resolve())


if __name__ == "__main__":
    unittest.main()
