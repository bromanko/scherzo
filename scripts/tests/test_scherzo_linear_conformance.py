import importlib.machinery
import importlib.util
import io
import json
import os
import shutil
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from unittest.mock import patch


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "scherzo-linear-conformance"


def load_module():
    loader = importlib.machinery.SourceFileLoader("scherzo_linear_conformance", str(SCRIPT_PATH))
    spec = importlib.util.spec_from_loader("scherzo_linear_conformance", loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def repo_temp_manifest_dir():
    tmp_root = REPO_ROOT / "test" / "tmp" / "scherzo-linear-conformance-tests"
    tmp_root.mkdir(parents=True, exist_ok=True)
    return tempfile.TemporaryDirectory(dir=tmp_root)


class WrapperValidationTests(unittest.TestCase):
    def test_rejects_missing_dedicated_credential(self):
        module = load_module()
        stderr = io.StringIO()
        with patch.dict(os.environ, {}, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
                "--run-id",
                "missing-token",
            ])
        self.assertEqual(code, 1)
        self.assertIn("missing SCHERZO_LINEAR_CONFORMANCE_API_KEY", stderr.getvalue())

    def test_rejects_linear_api_key_even_when_dedicated_credential_exists(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "LINEAR_API_KEY": "live-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
                "--run-id",
                "reject-linear-api-key",
            ])
        self.assertEqual(code, 1)
        self.assertIn("LINEAR_API_KEY must be unset", stderr.getvalue())

    def test_rejects_unsafe_production_project_slug(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-unsafe-production.manifest.json",
                "--run-id",
                "unsafe-project",
            ])
        self.assertEqual(code, 1)
        self.assertIn("refusing unsafe Linear project slug", stderr.getvalue())

    def test_rejects_non_explicit_fixture_ids(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-task-source.template.manifest.json",
                "--run-id",
                "placeholder-fixture",
            ])
        self.assertEqual(code, 1)
        self.assertIn("non-placeholder Linear fixture ids", stderr.getvalue())

    def test_rejects_live_side_effect_packs(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-side-effects-disabled.manifest.json",
                "--run-id",
                "disabled-packs",
            ])
        self.assertEqual(code, 1)
        self.assertIn("forbids requested pack: comments", stderr.getvalue())

    def test_rejects_run_id_path_traversal(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
                "--run-id",
                "../escape",
            ])
        self.assertEqual(code, 1)
        self.assertIn("run-id must contain only", stderr.getvalue())

    def test_driver_mode_rejects_unsafe_project_without_network(self):
        module = load_module()
        stdout = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        request = json.dumps({"schema_version": 1, "request_id": "unsafe-driver"})
        with patch.dict(os.environ, env, clear=True), patch("sys.stdin", io.StringIO(request)), redirect_stdout(stdout):
            code = module.main(["driver", "--mode", "live", "--project", "scherzo-core"])
        response = json.loads(stdout.getvalue())
        self.assertEqual(code, 0)
        self.assertFalse(response["ok"])
        self.assertIn("refusing unsafe Linear project slug", response["error"]["message"])

    def test_rejects_untrusted_manifest_driver_command(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with repo_temp_manifest_dir() as tmp:
            manifest_path = Path(tmp) / "linear-untrusted-driver.manifest.json"
            manifest = json.loads((REPO_ROOT / "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json").read_text(encoding="utf-8"))
            manifest["driver"]["command"]["executable"] = "sh"
            manifest["driver"]["command"]["args"] = ["-c", "cat"]
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
            manifest_arg = os.path.relpath(manifest_path, REPO_ROOT)
            with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
                code = module.main(["run", "--manifest", manifest_arg, "--run-id", "untrusted-driver"])
        self.assertEqual(code, 1)
        self.assertIn("manifest.driver.command must use scripts/scherzo-linear-conformance", stderr.getvalue())

    def test_rejects_driver_endpoint_override_env(self):
        module = load_module()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": os.environ.get("PATH", ""),
        }
        with repo_temp_manifest_dir() as tmp:
            manifest_path = Path(tmp) / "linear-endpoint-override.manifest.json"
            manifest = json.loads((REPO_ROOT / "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json").read_text(encoding="utf-8"))
            manifest["driver"]["command"]["env"].append({
                "name": "SCHERZO_LINEAR_CONFORMANCE_ENDPOINT",
                "value": "https://attacker.example/graphql",
            })
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
            manifest_arg = os.path.relpath(manifest_path, REPO_ROOT)
            with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
                code = module.main(["run", "--manifest", manifest_arg, "--run-id", "endpoint-override"])
        self.assertEqual(code, 1)
        self.assertIn("unsupported variable: SCHERZO_LINEAR_CONFORMANCE_ENDPOINT", stderr.getvalue())

    def test_run_mode_fails_cleanly_when_gleam_is_missing(self):
        module = load_module()
        module.GLEAM_CANDIDATES = []
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "PATH": "",
        }
        with patch.dict(os.environ, env, clear=True), redirect_stderr(stderr):
            code = module.main([
                "run",
                "--manifest",
                "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
                "--run-id",
                "missing-gleam",
            ])
        self.assertEqual(code, 1)
        self.assertIn("requires a local gleam executable", stderr.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())


class WrapperRunBehaviorTests(unittest.TestCase):
    def run_wrapper(self, manifest: str, run_id: str, extra_env: dict[str, str] | None = None) -> tuple[int, str, str, Path]:
        module = load_module()
        stdout = io.StringIO()
        stderr = io.StringIO()
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "SCHERZO_LINEAR_CONFORMANCE_FIXTURE_SECRET": "linear-fixture-secret",
            "PATH": os.environ.get("PATH", ""),
        }
        if extra_env:
            env.update(extra_env)
        report_path = module.default_report_path(run_id, REPO_ROOT / manifest)
        if report_path.parent.exists():
            shutil.rmtree(report_path.parent)
        with patch.dict(os.environ, env, clear=True), redirect_stdout(stdout), redirect_stderr(stderr):
            code = module.main(["run", "--manifest", manifest, "--run-id", run_id])
        return code, stdout.getvalue(), stderr.getvalue(), report_path

    def test_run_mode_invokes_hidden_conformance_entrypoint(self):
        module = load_module()
        module.GLEAM_CANDIDATES = [str(SCRIPT_PATH)]
        stdout = io.StringIO()
        stderr = io.StringIO()
        manifest = "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json"
        report_path = module.default_report_path("hidden-entrypoint", REPO_ROOT / manifest)
        if report_path.parent.exists():
            shutil.rmtree(report_path.parent)
        env = {
            "SCHERZO_LINEAR_CONFORMANCE_API_KEY": "fake-linear-token",
            "SCHERZO_LINEAR_CONFORMANCE_FIXTURE_SECRET": "linear-fixture-secret",
            "PATH": os.environ.get("PATH", ""),
        }
        captured_args = []

        def fake_run(args, **_kwargs):
            captured_args.append(args)
            report_path.parent.mkdir(parents=True, exist_ok=True)
            report_path.write_text(
                '{"ok": true, "redacted": "[REDACTED]"}\n',
                encoding="utf-8",
            )
            return module.subprocess.CompletedProcess(
                args,
                0,
                stdout="tracker-conformance adapter=linear\n",
                stderr="",
            )

        with (
            patch.dict(os.environ, env, clear=True),
            patch.object(module.subprocess, "run", side_effect=fake_run),
            redirect_stdout(stdout),
            redirect_stderr(stderr),
        ):
            code = module.main([
                "run",
                "--manifest",
                manifest,
                "--run-id",
                "hidden-entrypoint",
            ])

        self.assertEqual(code, 0)
        self.assertEqual(len(captured_args), 1)
        runner_args = captured_args[0]
        self.assertEqual(runner_args[1:4], ["run", "--", "__tracker-conformance-run"])
        self.assertNotEqual(runner_args[4], "run")
        self.assertNotIn("tracker-conformance", runner_args)
        self.assertIn("tracker-conformance adapter=linear", stdout.getvalue())
        self.assertEqual(stderr.getvalue(), "")

    def test_selects_report_path_and_redacts_emitted_evidence(self):
        code, stdout, stderr, report_path = self.run_wrapper(
            "test/fixtures/tracker_conformance/linear-redaction.manifest.json",
            "redaction-ok",
        )
        self.assertEqual(code, 0)
        self.assertTrue(report_path.is_file())
        self.assertIn("tracker-conformance adapter=linear", stdout)
        self.assertNotIn("fake-linear-token", stdout)
        self.assertNotIn("fake-linear-token", stderr)
        self.assertNotIn("linear-fixture-secret", stdout)
        self.assertNotIn("linear-fixture-secret", stderr)
        report_text = report_path.read_text(encoding="utf-8")
        self.assertNotIn("fake-linear-token", report_text)
        self.assertNotIn("linear-fixture-secret", report_text)
        self.assertIn("[REDACTED]", report_text)

    def test_repeated_run_id_is_cleanup_idempotent(self):
        first = self.run_wrapper(
            "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
            "repeatable-run",
        )
        second = self.run_wrapper(
            "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json",
            "repeatable-run",
        )
        self.assertEqual(first[0], 0)
        self.assertEqual(second[0], 0)
        self.assertTrue(first[3].is_file())
        self.assertTrue(second[3].is_file())

    def test_probe_failures_remain_visible_in_report(self):
        with repo_temp_manifest_dir() as tmp:
            manifest_path = Path(tmp) / "linear-probe-fails.manifest.json"
            manifest = json.loads((REPO_ROOT / "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json").read_text(encoding="utf-8"))
            manifest["probes"][0]["command"] = {
                "executable": "sh",
                "args": ["-c", "printf 'probe failed linear-fixture-secret\\n' >&2; exit 1"],
                "cwd": ".",
            }
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
            code, _stdout, _stderr, report_path = self.run_wrapper(
                os.path.relpath(manifest_path, REPO_ROOT),
                "probe-failure",
            )
        self.assertEqual(code, 1)
        report = json.loads(report_path.read_text(encoding="utf-8"))
        self.assertEqual(report["probe_failed"], 1)
        self.assertEqual(report["cleanup_failed"], 0)

    def test_cleanup_failures_remain_visible_after_partial_failure(self):
        with repo_temp_manifest_dir() as tmp:
            manifest_path = Path(tmp) / "linear-cleanup-fails.manifest.json"
            manifest = json.loads((REPO_ROOT / "test/fixtures/tracker_conformance/linear-cleanup-idempotent.manifest.json").read_text(encoding="utf-8"))
            manifest["hooks"]["cleanup"] = {
                "executable": "sh",
                "args": ["-c", "printf 'cleanup failed linear-fixture-secret\\n' >&2; exit 1"],
                "cwd": ".",
            }
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
            code, _stdout, _stderr, report_path = self.run_wrapper(
                os.path.relpath(manifest_path, REPO_ROOT),
                "cleanup-failure",
            )
        self.assertEqual(code, 1)
        report = json.loads(report_path.read_text(encoding="utf-8"))
        self.assertEqual(report["cleanup_failed"], 1)


if __name__ == "__main__":
    unittest.main()
