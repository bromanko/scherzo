from __future__ import annotations

import subprocess
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "scherzo-review"
FIXTURES = ROOT / "test" / "fixtures" / "review_lane_draft"


class ValidateLaneDraftCommandTest(unittest.TestCase):
    def run_cmd(self, *args: str, stdin: str | None = None) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            input=stdin,
            text=True,
            cwd=ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )

    def test_valid_fixture_passes_path_and_stdin_forms(self) -> None:
        path_result = self.run_cmd(
            "validate-lane-draft",
            "--lane",
            "correctness",
            "--draft",
            "test/fixtures/review_lane_draft/valid-minimal.json",
        )
        self.assertEqual(path_result.returncode, 0, path_result.stderr)
        self.assertIn("REVIEW_LANE_DRAFT_VALIDATION=ok", path_result.stdout)
        self.assertIn("REVIEW_LANE=correctness", path_result.stdout)
        self.assertIn("REVIEW_REMOTE_MUTATIONS=none", path_result.stdout)

        stdin_result = self.run_cmd(
            "validate-lane-draft",
            "--lane",
            "correctness",
            "--draft-json",
            "-",
            stdin=(FIXTURES / "valid-minimal.json").read_text(),
        )
        self.assertEqual(stdin_result.returncode, 0, stdin_result.stderr)
        self.assertIn("REVIEW_SCHEMA_VERSION=1", stdin_result.stdout)

    def test_invalid_fixtures_fail_with_domain_diagnostics(self) -> None:
        cases = {
            "invalid-remote-mutations.json": "review lane draft remote_mutations must be none",
            "invalid-duplicate-finding-id.json": "draft finding ids must be unique",
            "invalid-missing-evidence-link.json": "evidence requests must link to an existing draft finding",
            "invalid-unsafe-path.json": "locations.path must be repository-relative and must not escape the repository",
        }
        for fixture, diagnostic in cases.items():
            with self.subTest(fixture=fixture):
                result = self.run_cmd(
                    "validate-lane-draft",
                    "--lane",
                    "correctness",
                    "--draft",
                    f"test/fixtures/review_lane_draft/{fixture}",
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(diagnostic, result.stderr)

    def test_lane_argument_must_match_draft_lane_id(self) -> None:
        result = self.run_cmd(
            "validate-lane-draft",
            "--lane",
            "test-quality",
            "--draft",
            "test/fixtures/review_lane_draft/valid-minimal.json",
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("review lane draft lane.id must match --lane test-quality", result.stderr)


if __name__ == "__main__":
    unittest.main()
