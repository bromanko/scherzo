"""Focused tests for scherzo-execplan pack source-issue resolution.

Run from the repository root:

    python3 -m unittest discover -s .scherzo/workflows/scripts/tests
"""

import importlib.machinery
import importlib.util
import io
import os
import sys
import unittest
from contextlib import redirect_stderr
from pathlib import Path

SCRIPTS_DIR = Path(__file__).resolve().parents[1]

_ISSUE_ENV_KEYS = (
    "SCHERZO_ISSUE_IDENTIFIER",
    "SCHERZO_ISSUE_TITLE",
    "SCHERZO_ISSUE_URL",
)


def load_execplan_module():
    if str(SCRIPTS_DIR) not in sys.path:
        sys.path.insert(0, str(SCRIPTS_DIR))
    loader = importlib.machinery.SourceFileLoader(
        "scherzo_execplan_under_test", str(SCRIPTS_DIR / "scherzo-execplan")
    )
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class PackSourceIssueTests(unittest.TestCase):
    def setUp(self):
        self.module = load_execplan_module()
        self._saved_env = {key: os.environ.get(key) for key in _ISSUE_ENV_KEYS}

    def tearDown(self):
        for key, value in self._saved_env.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value

    def set_issue_env(self, identifier, title, url):
        os.environ["SCHERZO_ISSUE_IDENTIFIER"] = identifier
        os.environ["SCHERZO_ISSUE_TITLE"] = title
        os.environ["SCHERZO_ISSUE_URL"] = url

    def test_placeholder_identifier_is_repaired_from_task_environment(self):
        """A submission echoing the workflow label must not survive into the pack."""
        self.set_issue_env(
            "LIV-1427",
            "ExecPlan: event-stream ingestion backpressure and dispatch batching",
            "https://linear.app/living-systems/issue/LIV-1427/execplan",
        )
        payload = {
            "source_issue": {
                "identifier": "workflow:execplan",
                "title": "Event-stream ingestion backpressure and dispatch batching",
                "url": "local://workflow/execplan",
            }
        }
        stderr = io.StringIO()
        with redirect_stderr(stderr):
            source_issue = self.module.authoritative_pack_source_issue(payload)
        self.assertEqual(source_issue["identifier"], "LIV-1427")
        self.assertEqual(
            source_issue["title"],
            "ExecPlan: event-stream ingestion backpressure and dispatch batching",
        )
        self.assertEqual(
            source_issue["url"],
            "https://linear.app/living-systems/issue/LIV-1427/execplan",
        )
        self.assertIn("workflow:execplan", stderr.getvalue())
        self.assertIn("SCHERZO_ISSUE_IDENTIFIER", stderr.getvalue())

    def test_matching_identifier_keeps_submission_metadata(self):
        self.set_issue_env(
            "LIV-1427",
            "Env title that should not win",
            "https://linear.app/living-systems/issue/LIV-1427",
        )
        payload = {
            "source_issue": {
                "identifier": "LIV-1427",
                "title": "Submission title",
                "url": "https://linear.app/living-systems/issue/LIV-1427/full-slug",
            }
        }
        source_issue = self.module.authoritative_pack_source_issue(payload)
        self.assertEqual(source_issue["identifier"], "LIV-1427")
        self.assertEqual(source_issue["title"], "Submission title")
        self.assertEqual(
            source_issue["url"],
            "https://linear.app/living-systems/issue/LIV-1427/full-slug",
        )

    def test_without_task_environment_submission_is_trusted(self):
        for key in _ISSUE_ENV_KEYS:
            os.environ.pop(key, None)
        payload = {
            "source_issue": {
                "identifier": "workflow:execplan",
                "title": "Local run",
                "url": "local://workflow/execplan",
            }
        }
        source_issue = self.module.authoritative_pack_source_issue(payload)
        self.assertEqual(source_issue["identifier"], "workflow:execplan")

    def test_missing_submission_source_issue_falls_back_to_environment(self):
        self.set_issue_env(
            "LIV-1427",
            "Env title",
            "https://linear.app/living-systems/issue/LIV-1427",
        )
        source_issue = self.module.authoritative_pack_source_issue({})
        self.assertEqual(source_issue["identifier"], "LIV-1427")
        self.assertEqual(source_issue["title"], "Env title")


if __name__ == "__main__":
    unittest.main()
