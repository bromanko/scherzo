import importlib.machinery
import importlib.util
import io
import json
import os
import shutil
import tempfile
import unittest
from contextlib import contextmanager, redirect_stdout
from pathlib import Path
from unittest.mock import patch


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "scherzo-execplan"


def load_module():
    loader = importlib.machinery.SourceFileLoader("scherzo_execplan", str(SCRIPT_PATH))
    spec = importlib.util.spec_from_loader("scherzo_execplan", loader)
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


class BundleContextParsingTests(unittest.TestCase):
    def test_parse_bundle_metadata_when_linear_collapses_lines(self):
        module = load_module()
        text = (
            "Implement the bundle. Bundle ref: runs/LIV-335-1778980165271-3/outputs/exec_plan_bundle.json "
            "Bundle sha256: 688756952480882940f67d5097e83584fb34ac1be565127e4821ec2c4f442cb0"
        )

        ref, sha = module.parse_bundle_ref_and_sha(text)

        self.assertEqual(ref, "runs/LIV-335-1778980165271-3/outputs/exec_plan_bundle.json")
        self.assertEqual(sha, "688756952480882940f67d5097e83584fb34ac1be565127e4821ec2c4f442cb0")

    def test_parse_bundle_metadata_accepts_markdown_backticks(self):
        module = load_module()
        text = (
            "- Bundle ref: `runs/LIV-335-1778980165271-3/outputs/exec_plan_bundle.json`\n"
            "- Bundle sha256: `688756952480882940f67d5097e83584fb34ac1be565127e4821ec2c4f442cb0`\n"
        )

        ref, sha = module.parse_bundle_ref_and_sha(text)

        self.assertEqual(ref, "runs/LIV-335-1778980165271-3/outputs/exec_plan_bundle.json")
        self.assertEqual(sha, "688756952480882940f67d5097e83584fb34ac1be565127e4821ec2c4f442cb0")


class ImplementationPrepareTests(unittest.TestCase):
    def test_writes_canonical_handoff_files_and_stdout_markers(self):
        module = load_module()
        fixture_root = REPO_ROOT / "test" / "fixtures" / "execplan_v2"

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            shutil.copytree(
                REPO_ROOT / ".scherzo" / "workflows" / "schemas",
                repo_root / ".scherzo" / "workflows" / "schemas",
            )
            target_fixture_root = repo_root / "test" / "fixtures" / "execplan_v2"
            target_fixture_root.mkdir(parents=True)
            shutil.copyfile(
                fixture_root / "review-doc.valid.md",
                target_fixture_root / "review-doc.valid.md",
            )
            artifact_outputs = repo_root / ".scherzo-state" / "artifacts" / "runs" / "run-1" / "outputs"
            artifact_outputs.mkdir(parents=True)
            for name in ["exec_plan_bundle.json", "implementation_pack.json"]:
                shutil.copyfile(
                    fixture_root / "artifacts" / "runs" / "run-1" / "outputs" / name,
                    artifact_outputs / name,
                )

            bundle_path = artifact_outputs / "exec_plan_bundle.json"
            pre_cutover_bundle = json.loads(bundle_path.read_text(encoding="utf-8"))
            pre_cutover_bundle["implementation_handoff"]["workflow_label"] = "workflow:execplan-implementation-v2"
            bundle_path.write_text(module.canonical_json(pre_cutover_bundle), encoding="utf-8")
            bundle_sha = module.sha256_bytes(bundle_path.read_bytes())
            issue_context = (
                "Bundle ref: runs/run-1/outputs/exec_plan_bundle.json\n"
                f"Bundle sha256: {bundle_sha}\n"
            )
            stdout = io.StringIO()
            env = {"SCHERZO_ISSUE_CONTEXT": issue_context}

            with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
                module.command_implementation_prepare(["--from-issue-context"])

            output = stdout.getvalue()
            self.assertIn("IMPLEMENTATION_PREPARE_STATUS=ok", output)
            self.assertIn("REVIEW_DOC=tmp/execplan-review-doc.md", output)
            self.assertIn("IMPLEMENTATION_PACK=tmp/execplan-implementation-pack.json", output)
            self.assertIn("BUNDLE=tmp/execplan-bundle.json", output)
            self.assertTrue((repo_root / "tmp" / "execplan-review-doc.md").is_file())
            self.assertTrue((repo_root / "tmp" / "execplan-implementation-pack.json").is_file())
            self.assertTrue((repo_root / "tmp" / "execplan-bundle.json").is_file())
            self.assertFalse((repo_root / "tmp" / "execplan-v2-review-doc.md").exists())
            self.assertFalse((repo_root / "tmp" / "execplan-v2-implementation-pack.json").exists())
            self.assertFalse((repo_root / "tmp" / "execplan-v2-bundle.json").exists())

            prepared_bundle = json.loads((repo_root / "tmp" / "execplan-bundle.json").read_text(encoding="utf-8"))
            self.assertEqual(prepared_bundle["implementation_handoff"]["workflow_label"], "workflow:execplan-implementation")
            metadata = json.loads((repo_root / "tmp" / "scherzo-implementation.json").read_text(encoding="utf-8"))
            self.assertEqual(metadata["source_kind"], "execplan")
            self.assertEqual(metadata["base_change_id"], "unknown-base")
            self.assertEqual(metadata["plan_path"], "test/fixtures/execplan_v2/review-doc.valid.md")
            self.assertEqual(metadata["execplan_v2_bundle_path"], "tmp/execplan-bundle.json")
            self.assertEqual(metadata["execplan_v2_implementation_pack_path"], "tmp/execplan-implementation-pack.json")


class ResolveStructuredSubmissionTests(unittest.TestCase):
    def test_resolves_workspace_scoped_structured_output_from_run_root_fallback(self):
        module = load_module()
        run_id = "LIV-311-1778903143150-1"
        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            run_root = repo_root / ".scherzo" / "workspaces" / "execplan" / "LIV-311" / run_id
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
            run_root = repo_root / ".scherzo" / "workspaces" / "execplan" / "LIV-311" / run_id
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
