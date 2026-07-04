from __future__ import annotations

import importlib.util
from importlib.machinery import SourceFileLoader
import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scherzo_review import review_lane_contract as contract


SCRIPT_PATH = Path(__file__).resolve().parents[1] / "scherzo-review"
SCRIPT_LOADER = SourceFileLoader("scherzo_review_script_under_test", str(SCRIPT_PATH))
SCRIPT_SPEC = importlib.util.spec_from_loader(SCRIPT_LOADER.name, SCRIPT_LOADER)
if SCRIPT_SPEC is None or SCRIPT_SPEC.loader is None:  # pragma: no cover - import guard
    raise RuntimeError(f"could not import {SCRIPT_PATH}")
review_script = importlib.util.module_from_spec(SCRIPT_SPEC)
SCRIPT_SPEC.loader.exec_module(review_script)


class ProviderSchemaContractTest(unittest.TestCase):
    def write_schema(self, schema: dict) -> Path:
        tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(tmpdir.cleanup)
        path = Path(tmpdir.name) / "schema.json"
        path.write_text(json.dumps(schema), encoding="utf-8")
        return path

    def test_check_provider_schema_is_pure_python(self) -> None:
        schema_path = self.write_schema(
            {
                "type": "object",
                "required": ["summary"],
                "properties": {
                    "summary": {"type": "string", "minLength": 1},
                    "blocking": {"type": "boolean"},
                },
                "additionalProperties": False,
            }
        )

        with mock.patch.object(contract.subprocess, "run", side_effect=AssertionError("no subprocess")):
            schema = contract.check_provider_schema(schema_path)

        self.assertEqual(schema["type"], "object")

    def test_check_provider_schema_rejects_gleam_only_schema_features(self) -> None:
        schema_path = self.write_schema(
            {
                "type": "object",
                "$defs": {"x": {"type": "string"}},
                "properties": {"summary": {"$ref": "#/$defs/x"}},
            }
        )

        with self.assertRaises(contract.ContractError) as raised:
            contract.check_provider_schema(schema_path)

        self.assertEqual(raised.exception.code, "structured_output_tool_spec_provider_incompatible_schema")
        self.assertIn("disallowed keyword $defs", raised.exception.message)


class NativeReviewFinalizeLanesTest(unittest.TestCase):
    def test_lane_loop_records_materialize_and_verify_failures_then_normalizes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            prepare_dir = root / "prepare_review"
            artifact_dir = root / "run-artifacts"
            review_root = root / "review"
            prepare_dir.mkdir()
            artifact_dir.mkdir()

            normalized_lanes: list[str] = []

            def fake_normalize(args: object) -> None:
                lane = str(getattr(args, "lane"))
                normalized_lanes.append(lane)
                output_dir = Path(str(getattr(args, "output_dir")))
                output_dir.mkdir(parents=True, exist_ok=True)
                (output_dir / f"review-lane-{lane}.v1.json").write_text("{}\n", encoding="utf-8")

            with mock.patch.object(
                review_script,
                "assert_review_lane_commit_unchanged",
            ), mock.patch.object(
                review_script.lane_contract,
                "resolve_submission_path",
                side_effect=contract.ContractError(
                    "review_lane_submission_artifact_not_found",
                    "missing structured output",
                ),
            ), mock.patch.object(
                review_script,
                "verify_evidence_command",
                side_effect=review_script.ReviewError("draft missing"),
            ), mock.patch.object(
                review_script,
                "normalize_lane_result_command",
                side_effect=fake_normalize,
            ), mock.patch.object(
                review_script,
                "synthesize_command",
            ), mock.patch.object(
                review_script,
                "validate_native_review_synthesis_artifacts",
            ), mock.patch.object(
                review_script,
                "assert_publishable_final_review",
            ):
                review_script.finalize_lanes_command(review_script.argparse.Namespace(
                    prepare_dir=str(prepare_dir),
                    review_root=str(review_root),
                    dirty_tree_dir=str(review_root / "dirty_tree"),
                    artifact_dir=str(artifact_dir),
                    lane=["correctness"],
                    synthesis_output_dir="",
                ))

            self.assertEqual(["correctness"], normalized_lanes)
            result_path = review_root / "lanes" / "correctness" / "review-lane-correctness.v1.json"
            self.assertTrue(result_path.is_file())
            failure_log = review_root / "lanes" / "correctness" / "lane-pipeline-failures.ndjson"
            events = [json.loads(line) for line in failure_log.read_text(encoding="utf-8").splitlines()]
            self.assertEqual(["materialize", "verify-evidence"], [event["stage"] for event in events])

    def test_assert_publishable_blocks_lane_failures_and_execution_issues(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            final_review = Path(tmp) / "final-review.v1.json"
            final_review.write_text(
                json.dumps({"finding_counts": {"lane_failed": 1}, "execution_issues": []}),
                encoding="utf-8",
            )
            with self.assertRaises(review_script.ReviewError) as lane_failed:
                review_script.assert_publishable_command(review_script.argparse.Namespace(
                    final_review=str(final_review),
                ))
            self.assertIn("lane_failed=1", str(lane_failed.exception))

            final_review.write_text(
                json.dumps({"finding_counts": {"lane_failed": 0}, "execution_issues": [{"kind": "lane_failure"}]}),
                encoding="utf-8",
            )
            with self.assertRaises(review_script.ReviewError) as execution_issue:
                review_script.assert_publishable_command(review_script.argparse.Namespace(
                    final_review=str(final_review),
                ))
            self.assertIn("execution_issues=1", str(execution_issue.exception))

    def test_clean_tree_mismatch_records_after_snapshot_then_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            dirty_tree_dir = Path(tmp)
            (dirty_tree_dir / "before-lanes.txt").write_text("before-commit", encoding="utf-8")
            (dirty_tree_dir / "before-lanes-status.txt").write_text("clean before\n", encoding="utf-8")

            def fake_run(args: list[str], **_: object) -> subprocess.CompletedProcess[str]:
                stdout = "after-commit" if args[:2] == ["jj", "log"] else "clean after\n"
                return subprocess.CompletedProcess(args=args, returncode=0, stdout=stdout, stderr="")

            with mock.patch.object(review_script.subprocess, "run", side_effect=fake_run):
                with self.assertRaises(review_script.ReviewError) as raised:
                    review_script.assert_review_lane_commit_unchanged(dirty_tree_dir)

            self.assertIn("commit ids differ", str(raised.exception))
            self.assertEqual("after-commit", (dirty_tree_dir / "after-lanes.txt").read_text(encoding="utf-8"))
            self.assertEqual("clean after\n", (dirty_tree_dir / "after-lanes-status.txt").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
