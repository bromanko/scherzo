import importlib.machinery
import importlib.util
import io
import json
import os
import shutil
import sys
import tempfile
import unittest
from contextlib import contextmanager, redirect_stderr, redirect_stdout
from pathlib import Path
from unittest.mock import patch


REPO_ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_SCRIPTS = REPO_ROOT / "workflows" / "dogfood" / "scripts"
sys.path.insert(0, str(WORKFLOW_SCRIPTS))
SCRIPT_PATH = WORKFLOW_SCRIPTS / "scherzo-execplan"


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


def prepare_execplan_repo(repo_root: Path) -> None:
    (repo_root / ".scherzo" / "workflows").mkdir(parents=True)
    shutil.copytree(
        REPO_ROOT / ".scherzo" / "workflows" / "schemas",
        repo_root / ".scherzo" / "workflows" / "schemas",
    )
    target_fixture_root = repo_root / "test" / "fixtures" / "execplan_v2"
    target_fixture_root.mkdir(parents=True)
    shutil.copyfile(
        REPO_ROOT / "test" / "fixtures" / "execplan_v2" / "review-doc.valid.md",
        target_fixture_root / "review-doc.valid.md",
    )


def write_retained_execplan_bundle(
    module,
    repo_root: Path,
    run_id: str,
    *,
    pr_url: str = "https://github.com/living-systems/scherzo/pull/314",
    source_identifier: str = "LIV-314",
    handoff_identifier: str = "LIV-315",
    include_bundle: bool = True,
    include_manifest: bool = True,
):
    fixture_root = REPO_ROOT / "test" / "fixtures" / "execplan_v2"
    outputs = repo_root / ".scherzo-state" / "artifacts" / "runs" / run_id / "outputs"
    outputs.mkdir(parents=True)
    plan_path = outputs / "plan.md"
    pack_path = outputs / "implementation_pack.json"
    shutil.copyfile(fixture_root / "artifacts" / "runs" / "run-1" / "outputs" / "plan.md", plan_path)
    shutil.copyfile(fixture_root / "artifacts" / "runs" / "run-1" / "outputs" / "implementation_pack.json", pack_path)
    plan_bytes = plan_path.read_bytes()
    pack_bytes = pack_path.read_bytes()
    bundle_ref = f"runs/{run_id}/outputs/exec_plan_bundle.json"
    plan_ref = f"runs/{run_id}/outputs/plan.md"
    pack_ref = f"runs/{run_id}/outputs/implementation_pack.json"
    bundle = json.loads((fixture_root / "artifacts" / "runs" / "run-1" / "outputs" / "exec_plan_bundle.json").read_text(encoding="utf-8"))
    bundle["bundle_id"] = f"fixture-bundle-{source_identifier.lower()}-{run_id}"
    bundle["workflow"]["run_id"] = run_id
    bundle["implementation_handoff"]["bundle_ref"] = bundle_ref
    bundle["implementation_handoff"]["issue_identifier"] = handoff_identifier
    bundle["implementation_handoff"]["issue_url"] = f"https://linear.app/living-systems/issue/{handoff_identifier}/implement-fixture"
    bundle["plan"]["ref"] = plan_ref
    bundle["plan"]["sha256"] = module.sha256_bytes(plan_bytes)
    bundle["plan"]["bytes"] = len(plan_bytes)
    bundle["implementation_pack"]["ref"] = pack_ref
    bundle["implementation_pack"]["sha256"] = module.sha256_bytes(pack_bytes)
    bundle["implementation_pack"]["bytes"] = len(pack_bytes)
    for entry in bundle["entries"]:
        if entry.get("name") == "plan":
            entry["ref"] = plan_ref
            entry["sha256"] = module.sha256_bytes(plan_bytes)
            entry["bytes"] = len(plan_bytes)
        if entry.get("name") == "implementation_pack":
            entry["ref"] = pack_ref
            entry["sha256"] = module.sha256_bytes(pack_bytes)
            entry["bytes"] = len(pack_bytes)
    bundle["review_surface"]["pr_url"] = pr_url
    bundle["source_issue"]["identifier"] = source_identifier
    bundle["source_issue"]["title"] = f"Fixture source {source_identifier}"
    bundle["source_issue"]["url"] = f"https://linear.app/living-systems/issue/{source_identifier}/fixture-source"
    bundle_text = module.canonical_json(bundle)
    bundle_sha = module.sha256_bytes(bundle_text.encode("utf-8"))
    if include_bundle:
        (outputs / "exec_plan_bundle.json").write_text(bundle_text, encoding="utf-8")
    manifest = {
        "schema_version": 1,
        "artifact_type": "workflow_contract_outputs",
        "run_id": run_id,
        "workflow_id": "execplan",
        "workflow_fingerprint": "fixture-fingerprint",
        "outputs": [
            {
                "name": "exec_plan_bundle",
                "value": {
                    "type": "exec_plan_bundle",
                    "status": "present",
                    "ref_kind": "run_artifact",
                    "ref": bundle_ref,
                    "sha256": bundle_sha,
                    "bytes": len(bundle_text.encode("utf-8")),
                    "media_type": "application/json",
                    "value": None,
                    "source": None,
                    "diagnostic": None,
                },
            }
        ],
        "diagnostics": [],
    }
    if include_manifest:
        (outputs.parent / "outputs.v1.json").write_text(module.canonical_json(manifest), encoding="utf-8")
    return bundle_ref, bundle_sha, bundle


def write_retained_review_publication_manifest(
    module,
    repo_root: Path,
    run_id: str,
    *,
    branch: str,
    head_revision: str,
    pr_url: str,
    generated_at_ms: int = 1000,
    attempt_id: str = "attempt-1",
) -> Path:
    publication_dir = (
        repo_root
        / ".scherzo-state"
        / "artifacts"
        / "runs"
        / run_id
        / "publications"
        / module.EXECPLAN_REVIEW_DOC_PUBLICATION_ID
    )
    publication_dir.mkdir(parents=True, exist_ok=True)
    manifest = {
        "schema_version": 1,
        "artifact_type": module.ARTIFACT_PUBLICATION_MANIFEST_ARTIFACT_TYPE,
        "publication_id": module.EXECPLAN_REVIEW_DOC_PUBLICATION_ID,
        "attempt_id": attempt_id,
        "status": "published",
        "branch": branch,
        "pr_url": pr_url,
        "head_revision": head_revision,
        "generated_at_ms": generated_at_ms,
    }
    path = publication_dir / f"{attempt_id}.json"
    path.write_text(module.canonical_json(manifest), encoding="utf-8")
    return path


def write_fake_execplan_driver(repo_root: Path, *, changed_path: str) -> tuple[Path, Path]:
    driver_path = repo_root / "fake-workspace-driver.py"
    log_path = repo_root / "fake-workspace-driver.jsonl"
    script = f"""#!/usr/bin/env python3
import json
import sys
from pathlib import Path

log_path = Path({str(log_path)!r})
argv = sys.argv[1:]
with log_path.open("a", encoding="utf-8") as handle:
    handle.write(json.dumps({{"argv": argv}}, sort_keys=True) + "\\n")
if argv == ["changed-files", "--json"]:
    print(json.dumps({{"version": 1, "files": [{{"path": {changed_path!r}, "status": "modified"}}]}}))
    sys.exit(0)
if argv[:1] == ["refresh-base"]:
    print(json.dumps({{"version": 1, "status": "fresh"}}))
    sys.exit(0)
print("unsupported fake driver invocation: " + " ".join(argv), file=sys.stderr)
sys.exit(2)
"""
    driver_path.write_text(script, encoding="utf-8")
    driver_path.chmod(0o755)
    return driver_path, log_path


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


class PrepareRevisionDiscoveryTests(unittest.TestCase):
    def run_prepare_revision(
        self,
        module,
        repo_root: Path,
        issue_context: str,
        *,
        extra_env: dict[str, str] | None = None,
    ) -> str:
        stdout = io.StringIO()
        env = {"SCHERZO_ISSUE_CONTEXT": issue_context}
        if extra_env:
            env.update(extra_env)
        with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
            module.command_prepare_revision([
                "--from-issue-context",
                "--write-bundle",
                "tmp/execplan-previous-bundle.json",
                "--write-review-doc-path",
                "tmp/execplan-review-doc.path",
                "--write-pack",
                "tmp/execplan-previous-pack.json",
            ])
        return stdout.getvalue()

    def test_discovers_single_matching_retained_bundle_from_pr_url(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            ref, sha, _bundle = write_retained_execplan_bundle(module, repo_root, "run-1")
            stdout = io.StringIO()
            env = {
                "SCHERZO_ISSUE_CONTEXT": "Review PR: https://github.com/living-systems/scherzo/pull/314\nFeedback: clarify rollback.",
            }

            with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
                module.command_prepare_revision([
                    "--from-issue-context",
                    "--write-bundle",
                    "tmp/execplan-previous-bundle.json",
                    "--write-review-doc-path",
                    "tmp/execplan-review-doc.path",
                    "--write-pack",
                    "tmp/execplan-previous-pack.json",
                ])

            output = stdout.getvalue()
            self.assertIn("PREPARE_REVISION_STATUS=ok", output)
            self.assertIn("BUNDLE_DISCOVERY_STATUS=discovered", output)
            self.assertIn(f"BUNDLE_REF={ref}", output)
            self.assertIn(f"BUNDLE_SHA256={sha}", output)
            self.assertIn("pr:", output)
            self.assertTrue((repo_root / "tmp" / "execplan-previous-bundle.json").is_file())
            self.assertTrue((repo_root / "tmp" / "execplan-previous-pack.json").is_file())
            self.assertEqual(
                (repo_root / "tmp" / "execplan-review-doc.path").read_text(encoding="utf-8"),
                "test/fixtures/execplan_v2/review-doc.valid.md\n",
            )

    def test_ambiguous_discovery_lists_matching_retained_bundles(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            write_retained_execplan_bundle(module, repo_root, "run-1")
            write_retained_execplan_bundle(module, repo_root, "run-2")
            stderr = io.StringIO()
            env = {
                "SCHERZO_ISSUE_CONTEXT": "Review PR: https://github.com/living-systems/scherzo/pull/314\nFeedback: split milestones.",
            }

            with self.assertRaises(SystemExit) as raised:
                with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stderr(stderr):
                    module.command_prepare_revision([
                        "--from-issue-context",
                        "--write-bundle",
                        "tmp/execplan-previous-bundle.json",
                        "--write-review-doc-path",
                        "tmp/execplan-review-doc.path",
                        "--write-pack",
                        "tmp/execplan-previous-pack.json",
                    ])

            self.assertEqual(raised.exception.code, 2)
            error = stderr.getvalue()
            self.assertIn("SCHERZO_FAILURE_CODE=execplan_v2_ambiguous_bundle_discovery", error)
            self.assertIn("ambiguous-bundle-discovery", error)
            self.assertIn("runs/run-1/outputs/exec_plan_bundle.json", error)
            self.assertIn("runs/run-2/outputs/exec_plan_bundle.json", error)
            self.assertIn("source_issue=LIV-314", error)
            self.assertIn("pr=https://github.com/living-systems/scherzo/pull/314", error)

    def test_missing_discovery_reports_pruned_or_required_input(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            write_retained_execplan_bundle(module, repo_root, "run-pruned", include_bundle=False)
            stderr = io.StringIO()
            env = {
                "SCHERZO_ISSUE_CONTEXT": "Review PR: https://github.com/living-systems/scherzo/pull/314\nFeedback: missing artifact case.",
            }

            with self.assertRaises(SystemExit) as raised:
                with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stderr(stderr):
                    module.command_prepare_revision([
                        "--from-issue-context",
                        "--write-bundle",
                        "tmp/execplan-previous-bundle.json",
                        "--write-review-doc-path",
                        "tmp/execplan-review-doc.path",
                        "--write-pack",
                        "tmp/execplan-previous-pack.json",
                    ])

            self.assertEqual(raised.exception.code, 2)
            error = stderr.getvalue()
            self.assertIn("SCHERZO_FAILURE_CODE=execplan_v2_missing_bundle_discovery", error)
            self.assertIn("missing-bundle-discovery", error)
            self.assertIn("artifacts were pruned", error)
            self.assertIn("Bundle ref:", error)
            self.assertFalse((repo_root / "tmp" / "execplan-previous-bundle.json").exists())

    def test_explicit_bundle_lines_take_precedence_over_ambiguous_discovery(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            ref, sha, _bundle = write_retained_execplan_bundle(module, repo_root, "run-1")
            write_retained_execplan_bundle(module, repo_root, "run-2")
            stdout = io.StringIO()
            env = {
                "SCHERZO_ISSUE_CONTEXT": "Review PR: https://github.com/living-systems/scherzo/pull/314\n"
                f"Bundle ref: {ref}\nBundle sha256: {sha}\nFeedback: use the explicit bundle.",
            }

            with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
                module.command_prepare_revision([
                    "--from-issue-context",
                    "--write-bundle",
                    "tmp/execplan-previous-bundle.json",
                    "--write-review-doc-path",
                    "tmp/execplan-review-doc.path",
                    "--write-pack",
                    "tmp/execplan-previous-pack.json",
                ])

            output = stdout.getvalue()
            self.assertIn("PREPARE_REVISION_STATUS=ok", output)
            self.assertIn("BUNDLE_DISCOVERY_STATUS=explicit", output)
            self.assertIn(f"BUNDLE_REF={ref}", output)
            prepared = json.loads((repo_root / "tmp" / "execplan-previous-bundle.json").read_text(encoding="utf-8"))
            self.assertEqual(prepared["workflow"]["run_id"], "run-1")

    def test_discovers_from_supported_non_pr_anchors(self):
        module = load_module()

        cases = [
            (
                "source issue identifier",
                lambda ref, sha, bundle: "Source issue LIV-314 needs a revision.",
                "source_issue:LIV-314",
            ),
            (
                "source issue url",
                lambda ref, sha, bundle: "Source: https://linear.app/living-systems/issue/LIV-314/fixture-source",
                "source_issue:https://linear.app/living-systems/issue/LIV-314/fixture-source",
            ),
            (
                "handoff issue identifier",
                lambda ref, sha, bundle: "Handoff LIV-315 needs the plan revised.",
                "handoff_issue:LIV-315",
            ),
            (
                "review doc path",
                lambda ref, sha, bundle: "Review doc: test/fixtures/execplan_v2/review-doc.valid.md",
                "review_doc:test/fixtures/execplan_v2/review-doc.valid.md",
            ),
            (
                "bundle ref",
                lambda ref, sha, bundle: f"Previous artifact ref {ref}",
                "bundle_ref:runs/run-1/outputs/exec_plan_bundle.json",
            ),
            (
                "bundle sha",
                lambda ref, sha, bundle: f"Trusted retained bundle hash {sha}",
                "bundle_sha256",
            ),
            (
                "implementation pack sha",
                lambda ref, sha, bundle: f"Implementation pack hash {bundle['implementation_pack']['sha256']}",
                "implementation_pack_sha256",
            ),
        ]

        for name, context, expected_match in cases:
            with self.subTest(name=name), tempfile.TemporaryDirectory() as tmp:
                repo_root = Path(tmp)
                prepare_execplan_repo(repo_root)
                ref, sha, bundle = write_retained_execplan_bundle(module, repo_root, "run-1")

                output = self.run_prepare_revision(module, repo_root, context(ref, sha, bundle))

                self.assertIn("PREPARE_REVISION_STATUS=ok", output)
                self.assertIn("BUNDLE_DISCOVERY_STATUS=discovered", output)
                self.assertIn(f"BUNDLE_REF={ref}", output)
                self.assertIn(expected_match, output)

    def test_current_revision_issue_identifier_is_not_used_as_source_anchor(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            write_retained_execplan_bundle(module, repo_root, "run-1")
            stderr = io.StringIO()
            env = {
                "SCHERZO_ISSUE_CONTEXT": "Revision issue LIV-314 has feedback but no source anchor.",
                "SCHERZO_ISSUE_IDENTIFIER": "LIV-314",
            }

            with self.assertRaises(SystemExit) as raised:
                with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stderr(stderr):
                    module.command_prepare_revision([
                        "--from-issue-context",
                        "--write-bundle",
                        "tmp/execplan-previous-bundle.json",
                        "--write-review-doc-path",
                        "tmp/execplan-review-doc.path",
                        "--write-pack",
                        "tmp/execplan-previous-pack.json",
                    ])

            self.assertEqual(raised.exception.code, 2)
            error = stderr.getvalue()
            self.assertIn("SCHERZO_FAILURE_CODE=execplan_v2_missing_bundle_discovery", error)
            self.assertIn("No PR URL, source issue identifier", error)

    def test_discovery_does_not_require_current_checkout_review_doc_bytes(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            ref, _sha, _bundle = write_retained_execplan_bundle(module, repo_root, "run-1")
            (repo_root / "test" / "fixtures" / "execplan_v2" / "review-doc.valid.md").unlink()

            output = self.run_prepare_revision(
                module,
                repo_root,
                "Review PR: https://github.com/living-systems/scherzo/pull/314",
            )

            self.assertIn("PREPARE_REVISION_STATUS=ok", output)
            self.assertIn("BUNDLE_DISCOVERY_STATUS=discovered", output)
            self.assertIn(f"BUNDLE_REF={ref}", output)
            self.assertTrue((repo_root / "tmp" / "execplan-previous-pack.json").is_file())

    def test_discovers_manifestless_retained_bundle_file(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            ref, _sha, _bundle = write_retained_execplan_bundle(
                module,
                repo_root,
                "run-1",
                include_manifest=False,
            )

            output = self.run_prepare_revision(
                module,
                repo_root,
                "Review PR: https://github.com/living-systems/scherzo/pull/314",
            )

            self.assertIn("PREPARE_REVISION_STATUS=ok", output)
            self.assertIn("BUNDLE_DISCOVERY_STATUS=discovered", output)
            self.assertIn(f"BUNDLE_REF={ref}", output)

    def test_discovery_filters_candidates_before_full_retained_validation(self):
        module = load_module()

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            write_retained_execplan_bundle(module, repo_root, "run-1", pr_url="https://github.com/living-systems/scherzo/pull/313")
            matching_ref, _sha, _bundle = write_retained_execplan_bundle(module, repo_root, "run-2")
            validated_refs = []
            original = module.load_and_validate_retained_bundle

            def recording_load_and_validate_retained_bundle(bundle_path, *args, **kwargs):
                artifact_root = (repo_root / ".scherzo-state" / "artifacts").resolve()
                validated_refs.append(str(bundle_path.resolve().relative_to(artifact_root)))
                return original(bundle_path, *args, **kwargs)

            with patch.object(
                module,
                "load_and_validate_retained_bundle",
                side_effect=recording_load_and_validate_retained_bundle,
            ):
                output = self.run_prepare_revision(
                    module,
                    repo_root,
                    "Review PR: https://github.com/living-systems/scherzo/pull/314",
                )

            self.assertIn("PREPARE_REVISION_STATUS=ok", output)
            self.assertEqual(validated_refs, [matching_ref])


class RevisionPublicationTargetTests(unittest.TestCase):
    def test_prepare_revision_refreshes_previous_manifest_head(self):
        module = load_module()
        previous_head = "f" * 40
        main_head = "e" * 40
        manifest_branch = "scherzo/execplan/LIV-314/execplan_review_doc"
        pr_url = "https://github.com/living-systems/scherzo/pull/314"

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            ref, _sha, bundle = write_retained_execplan_bundle(module, repo_root, "run-1")
            bundle["review_surface"]["branch"] = "stale/review-surface-branch"
            bundle["review_surface"]["head_revision"] = main_head
            bundle_path = repo_root / ".scherzo-state" / "artifacts" / "runs" / "run-1" / "outputs" / "exec_plan_bundle.json"
            bundle_text = module.canonical_json(bundle)
            bundle_path.write_text(bundle_text, encoding="utf-8")
            bundle_sha = module.sha256_bytes(bundle_text.encode("utf-8"))
            write_retained_review_publication_manifest(
                module,
                repo_root,
                "run-1",
                branch=manifest_branch,
                head_revision=previous_head,
                pr_url=pr_url,
            )
            driver_path, driver_log = write_fake_execplan_driver(
                repo_root,
                changed_path="test/fixtures/execplan_v2/review-doc.valid.md",
            )
            stdout = io.StringIO()
            env = {
                "PATH": os.environ.get("PATH", ""),
                "SCHERZO_ISSUE_CONTEXT": f"Bundle ref: {ref}\nBundle sha256: {bundle_sha}\n",
                "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE": "origin",
                "SCHERZO_WORKSPACE_DRIVER": str(driver_path),
            }

            with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
                module.command_prepare_revision([
                    "--from-issue-context",
                    "--write-bundle",
                    "tmp/execplan-previous-bundle.json",
                    "--write-review-doc-path",
                    "tmp/execplan-review-doc.path",
                    "--write-pack",
                    "tmp/execplan-previous-pack.json",
                ])

            self.assertIn("PREPARE_REVISION_STATUS=ok", stdout.getvalue())
            driver_records = [
                json.loads(line)
                for line in driver_log.read_text(encoding="utf-8").splitlines()
            ]
            refresh_calls = [
                record["argv"]
                for record in driver_records
                if record["argv"][:1] == ["refresh-base"]
            ]
            self.assertEqual(
                refresh_calls,
                [["refresh-base", "--stage", "prepare_revision", "--target", previous_head, "--json"]],
            )

    def test_materialize_commit_stack_uses_previous_manifest_head_for_existing_pr_target_and_base(self):
        module = load_module()
        previous_head = "f" * 40
        main_head = "e" * 40
        new_head = "8" * 40
        head_tree = "7" * 40
        manifest_branch = "scherzo/execplan/LIV-314/execplan_review_doc"
        review_path = "test/fixtures/execplan_v2/review-doc.valid.md"
        pr_url = "https://github.com/living-systems/scherzo/pull/314"

        with tempfile.TemporaryDirectory() as tmp:
            repo_root = Path(tmp)
            prepare_execplan_repo(repo_root)
            _ref, _sha, bundle = write_retained_execplan_bundle(module, repo_root, "run-1")
            bundle["review_surface"]["branch"] = "stale/review-surface-branch"
            bundle["review_surface"]["head_revision"] = main_head
            write_retained_review_publication_manifest(
                module,
                repo_root,
                "run-1",
                branch=manifest_branch,
                head_revision=previous_head,
                pr_url=pr_url,
            )
            previous_bundle_path = repo_root / "tmp" / "execplan-previous-bundle.json"
            review_path_file = repo_root / "tmp" / "execplan-review-doc.path"
            previous_bundle_path.parent.mkdir(parents=True, exist_ok=True)
            previous_bundle_path.write_text(module.canonical_json(bundle), encoding="utf-8")
            review_path_file.write_text(review_path + "\n", encoding="utf-8")
            driver_path, driver_log = write_fake_execplan_driver(repo_root, changed_path=review_path)
            captured_kwargs = {}

            def fake_materialize_commit_stack_artifact(**kwargs):
                captured_kwargs.update(kwargs)
                refresh_seen = False
                if driver_log.exists():
                    for line in driver_log.read_text(encoding="utf-8").splitlines():
                        record = json.loads(line)
                        argv = record.get("argv") or []
                        if argv[:5] == ["refresh-base", "--stage", "materialize_commit_stack", "--target", previous_head]:
                            refresh_seen = True
                base_sha = previous_head if refresh_seen else main_head
                artifact = {
                    "schema_version": 1,
                    "artifact_type": module.commit_stack_helper.COMMIT_STACK_ARTIFACT_TYPE,
                    "repository": {"repo": kwargs["repository"]},
                    "base": {"ref": kwargs["base_ref"], "sha": base_sha},
                    "head": {"sha": new_head, "tree": head_tree},
                    "carrier": {
                        "ref": "runs/revision-run/outputs/execplan-commit-stack.bundle",
                        "sha256": "0" * 64,
                        "bytes": 1,
                        "media_type": module.commit_stack_helper.BUNDLE_MEDIA_TYPE,
                    },
                }
                module.write_json(kwargs["output_path"], artifact)
                return artifact

            stdout = io.StringIO()
            env = {
                "PATH": os.environ.get("PATH", ""),
                "SCHERZO_GITHUB_REPO": "living-systems/scherzo",
                "SCHERZO_JJ_WORKSPACE_BASE_BRANCH": "main",
                "SCHERZO_JJ_WORKSPACE_PUBLISH_REMOTE": "origin",
                "SCHERZO_WORKSPACE_DRIVER": str(driver_path),
                "SCHERZO_RUN_ID": "revision-run",
            }
            with patch.dict(os.environ, env, clear=True), chdir(repo_root), redirect_stdout(stdout):
                with patch.object(
                    module.commit_stack_helper,
                    "materialize_commit_stack_artifact",
                    side_effect=fake_materialize_commit_stack_artifact,
                ):
                    module.command_materialize_commit_stack([
                        "--review-doc-path-file",
                        "tmp/execplan-review-doc.path",
                        "--previous-bundle",
                        "tmp/execplan-previous-bundle.json",
                        "--target-output",
                        "tmp/execplan-publication-target.json",
                        "--output",
                        "tmp/execplan-commit-stack.json",
                    ])

            target = json.loads((repo_root / "tmp" / "execplan-publication-target.json").read_text(encoding="utf-8"))
            stack = json.loads((repo_root / "tmp" / "execplan-commit-stack.json").read_text(encoding="utf-8"))
            existing = target["existing_pr_branch"]
            self.assertEqual(target["kind"], "existing_pr_branch")
            self.assertEqual(existing["head"]["branch"], manifest_branch)
            self.assertEqual(existing["head"]["sha"], previous_head)
            self.assertNotEqual(existing["head"]["sha"], main_head)
            self.assertEqual(stack["base"]["sha"], previous_head)
            self.assertEqual(stack["base"]["sha"], existing["head"]["sha"])
            self.assertEqual(captured_kwargs["base_revision"], "@-")
            driver_records = [
                json.loads(line)
                for line in driver_log.read_text(encoding="utf-8").splitlines()
            ]
            refresh_calls = [
                record["argv"]
                for record in driver_records
                if record["argv"][:1] == ["refresh-base"]
            ]
            self.assertEqual(
                refresh_calls,
                [["refresh-base", "--stage", "materialize_commit_stack", "--target", previous_head, "--json"]],
            )


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
            for name in ["exec_plan_bundle.json", "implementation_pack.json", "plan.md"]:
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
            self.assertEqual(metadata["plan_path"], "tmp/execplan-review-doc.md")
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
