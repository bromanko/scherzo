import os
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts" / "scherzo-pi"


class ScherzoPiRoutingTests(unittest.TestCase):
    def run_wrapper(
        self,
        workspace_kind: str,
        *,
        provide_pi_bin: bool = True,
        provide_core_direnv: bool = False,
    ) -> list[str]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            workspace = root / ".scherzo" / "workspaces" / workspace_kind / "LIV-346" / "run-1" / "workspaces" / "main"
            workspace.mkdir(parents=True)
            bin_dir = root / "bin"
            bin_dir.mkdir()
            capture = root / "pi-args.txt"
            packaged_pi = root / "packaged-pi"
            packaged_pi.write_text(
                "#!/bin/sh\n"
                "for arg in \"$@\"; do\n"
                "  printf '%s\\n' \"$arg\" >> \"$SCHERZO_PI_CAPTURE\"\n"
                "done\n",
                encoding="utf-8",
            )
            packaged_pi.chmod(0o755)
            path_pi = bin_dir / "pi"
            path_pi.write_text(
                "#!/bin/sh\n"
                "echo 'unexpected PATH pi' >&2\n"
                "exit 42\n",
                encoding="utf-8",
            )
            path_pi.chmod(0o755)
            if provide_core_direnv:
                fake_direnv = bin_dir / "direnv"
                fake_direnv.write_text(
                    "#!/bin/sh\n"
                    "if [ \"$1\" != exec ]; then\n"
                    "  echo 'unexpected direnv invocation' >&2\n"
                    "  exit 43\n"
                    "fi\n"
                    "shift 2\n"
                    "SCHERZO_PI_BIN=$TEST_CORE_PACKAGED_PI exec \"$@\"\n",
                    encoding="utf-8",
                )
                fake_direnv.chmod(0o755)
            env = {
                **os.environ,
                "PATH": str(bin_dir) + os.pathsep + os.environ.get("PATH", ""),
                "SCHERZO_PI_CAPTURE": str(capture),
                "SCHERZO_EXECPLAN_PI_MODEL": "execplan-model",
                "SCHERZO_RESEARCH_PI_MODEL": "research-model",
                "TEST_CORE_PACKAGED_PI": str(packaged_pi),
            }
            if provide_pi_bin:
                env["SCHERZO_PI_BIN"] = str(packaged_pi)
            else:
                env.pop("SCHERZO_PI_BIN", None)
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
                self.assertEqual(args[-3:], ["--no-session", "--rpc-message-updates", "off"])

    def test_retired_v2_workspace_kind_uses_default_model_routing(self):
        args = self.run_wrapper("execplan-v2")

        self.assertNotIn("execplan-model", args)
        self.assertEqual(args[:2], ["--mode", "rpc"])
        self.assertEqual(args[-3:], ["--no-session", "--rpc-message-updates", "off"])

    def test_uses_core_direnv_packaged_pi_when_scherzo_pi_bin_unset(self):
        args = self.run_wrapper(
            "execplan",
            provide_pi_bin=False,
            provide_core_direnv=True,
        )

        self.assertEqual(args[:4], ["--model", "execplan-model", "--mode", "rpc"])
        self.assertEqual(args[-3:], ["--no-session", "--rpc-message-updates", "off"])

    def test_research_workspace_uses_research_model(self):
        args = self.run_wrapper("research")

        self.assertEqual(args[:4], ["--model", "research-model", "--mode", "rpc"])
        self.assertEqual(args[-3:], ["--no-session", "--rpc-message-updates", "off"])


if __name__ == "__main__":
    unittest.main()
