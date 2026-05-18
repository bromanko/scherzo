import os
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "scherzo-pi"


class ScherzoPiRoutingTests(unittest.TestCase):
    def run_wrapper(self, workspace_kind: str) -> list[str]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            workspace = root / ".scherzo" / "workspaces" / workspace_kind / "LIV-346" / "run-1" / "workspaces" / "main"
            workspace.mkdir(parents=True)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            capture = root / "pi-args.txt"
            fake_pi = bin_dir / "pi"
            fake_pi.write_text(
                "#!/bin/sh\n"
                "for arg in \"$@\"; do\n"
                "  printf '%s\\n' \"$arg\" >> \"$SCHERZO_PI_CAPTURE\"\n"
                "done\n",
                encoding="utf-8",
            )
            fake_pi.chmod(0o755)
            env = {
                **os.environ,
                "PATH": str(bin_dir) + os.pathsep + os.environ.get("PATH", ""),
                "SCHERZO_PI_CAPTURE": str(capture),
                "SCHERZO_EXECPLAN_PI_MODEL": "execplan-model",
                "SCHERZO_RESEARCH_PI_MODEL": "research-model",
            }
            env.pop("SCHERZO_PI_SESSION_PERSISTENCE", None)

            result = subprocess.run(
                [str(SCRIPT_PATH), "--prompt", "hello"],
                cwd=workspace,
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            return capture.read_text(encoding="utf-8").splitlines()

    def test_canonical_execplan_workspace_kinds_use_execplan_model(self):
        for workspace_kind in ["execplan", "execplan-revision", "execplan-implementation"]:
            with self.subTest(workspace_kind=workspace_kind):
                args = self.run_wrapper(workspace_kind)
                self.assertEqual(args[:4], ["--model", "execplan-model", "--mode", "rpc"])
                self.assertEqual(args[-1], "--no-session")

    def test_retired_v2_workspace_kind_uses_default_model_routing(self):
        args = self.run_wrapper("execplan-v2")

        self.assertNotIn("execplan-model", args)
        self.assertEqual(args[:2], ["--mode", "rpc"])
        self.assertEqual(args[-1], "--no-session")

    def test_research_workspace_uses_research_model(self):
        args = self.run_wrapper("research")

        self.assertEqual(args[:4], ["--model", "research-model", "--mode", "rpc"])
        self.assertEqual(args[-1], "--no-session")


if __name__ == "__main__":
    unittest.main()
