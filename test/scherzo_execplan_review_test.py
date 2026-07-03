from __future__ import annotations

import base64
import contextlib
import importlib.machinery
import importlib.util
import io
import json
import os
import re
import subprocess
import sys
import tempfile
import threading
import time
import unittest
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"
sys.path.insert(0, str(SCRIPTS))

import scherzo_execplan_review as review  # noqa: E402


FIXTURE_HTML = "\n".join(
    [
        "<!doctype html>",
        "<html><body>",
        '<h1 class="commentable" data-comment-id="heading-purpose" data-source-line="3">Purpose</h1>',
        '<p class="commentable" data-comment-id="paragraph-risk" data-source-line="4">Risk text</p>',
        "</body></html>",
    ]
)
FIXTURE_MARKDOWN = "# Example\n\nThis paragraph maps to line three.\n"
MARKDOWN_PATCH = "@@ -0,0 +1,3 @@\n+# Example\n+\n+This paragraph maps to line three."
BRIEF_HELPER_PATH = ROOT / "workflows" / "dogfood" / "scripts" / "scherzo-execplan-brief"


def load_brief_helper() -> Any:
    loader = importlib.machinery.SourceFileLoader(
        "scherzo_execplan_brief_helper", str(BRIEF_HELPER_PATH)
    )
    spec = importlib.util.spec_from_loader(loader.name, loader)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load brief helper from {BRIEF_HELPER_PATH}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class FakeGh:
    def __init__(self, root: Path, *, plan_path: str = "docs/plans/example.html", content: str = FIXTURE_HTML, files: list[dict[str, Any]] | None = None) -> None:
        self.root = root
        self.bin_dir = root / "bin"
        self.fixtures_dir = root / "fixtures"
        self.log_path = root / "gh-invocations.jsonl"
        self.plan_path = plan_path
        self.content = content
        self.files = files or [
            {
                "filename": plan_path,
                "status": "added",
                "patch": "@@ -0,0 +1,5 @@\n+<!doctype html>\n+<html><body>\n+<h1>Purpose</h1>\n+<p>Risk text</p>\n+</body></html>",
            }
        ]
        self.bin_dir.mkdir(parents=True)
        self.fixtures_dir.mkdir(parents=True)
        self._write_fixtures()
        self._write_script()

    def _write_fixtures(self) -> None:
        (self.fixtures_dir / "pull.json").write_text(
            json.dumps(
                {
                    "html_url": "https://github.com/owner/repo/pull/123",
                    "head": {"sha": "abc123", "repo": {"full_name": "owner/repo"}},
                }
            ),
            encoding="utf-8",
        )
        (self.fixtures_dir / "files.json").write_text(json.dumps(self.files), encoding="utf-8")
        content_json = {
            "encoding": "base64",
            "content": base64.b64encode(self.content.encode("utf-8")).decode("ascii"),
        }
        (self.fixtures_dir / "content.json").write_text(json.dumps(content_json), encoding="utf-8")

    def _write_script(self) -> None:
        script = f"""#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

fixtures = Path({str(self.fixtures_dir)!r})
log_path = Path({str(self.log_path)!r})
argv = sys.argv[1:]
method = "GET"
endpoint = ""
input_path = None
payload = None
if argv[:1] == ["repo"]:
    print("owner/repo")
    sys.exit(0)
if argv[:1] != ["api"]:
    print("unsupported gh invocation", file=sys.stderr)
    sys.exit(2)
i = 1
while i < len(argv):
    arg = argv[i]
    if arg == "--method":
        method = argv[i + 1].upper()
        i += 2
    elif arg == "--input":
        input_path = argv[i + 1]
        try:
            payload = json.loads(Path(input_path).read_text())
        except Exception as exc:
            payload = {{"error": str(exc)}}
        i += 2
    elif arg.startswith("-"):
        i += 2 if i + 1 < len(argv) and not argv[i + 1].startswith("-") else 1
    else:
        endpoint = arg
        i += 1
kind = "mutation" if method in {{"POST", "PUT", "PATCH", "DELETE"}} else "read"
record = {{"argv": argv, "method": method, "endpoint": endpoint, "input_payload_path": input_path, "payload": payload, "kind": kind}}
with log_path.open("a", encoding="utf-8") as handle:
    handle.write(json.dumps(record, sort_keys=True) + "\\n")
if method == "GET" and endpoint == "repos/owner/repo/pulls/123":
    print((fixtures / "pull.json").read_text())
elif method == "GET" and endpoint.startswith("repos/owner/repo/pulls/123/files"):
    print((fixtures / "files.json").read_text())
elif method == "GET" and "/contents/" in endpoint:
    print((fixtures / "content.json").read_text())
elif method == "POST" and endpoint in {{"repos/owner/repo/pulls/123/reviews", "repos/owner/repo/issues/123/comments"}}:
    print(json.dumps({{"ok": True, "endpoint": endpoint}}))
else:
    print(json.dumps({{"ok": True, "endpoint": endpoint}}))
"""
        path = self.bin_dir / "gh"
        path.write_text(script, encoding="utf-8")
        path.chmod(0o755)

    @contextlib.contextmanager
    def on_path(self):
        old_path = os.environ.get("PATH", "")
        os.environ["PATH"] = str(self.bin_dir) + os.pathsep + old_path
        try:
            yield
        finally:
            os.environ["PATH"] = old_path

    def invocations(self) -> list[dict[str, Any]]:
        if not self.log_path.exists():
            return []
        return [json.loads(line) for line in self.log_path.read_text(encoding="utf-8").splitlines() if line]

    def runner(self, args: list[str], **_kwargs: Any) -> subprocess.CompletedProcess[str]:
        method = "GET"
        endpoint = ""
        input_path = None
        payload = None
        index = 2 if args[:2] == ["gh", "api"] else 0
        while index < len(args):
            arg = args[index]
            if arg == "--method":
                method = args[index + 1].upper()
                index += 2
            elif arg == "--input":
                input_path = args[index + 1]
                payload = json.loads(Path(input_path).read_text(encoding="utf-8"))
                index += 2
            elif arg.startswith("-"):
                index += 1
            else:
                endpoint = arg
                index += 1
        record = {
            "argv": args,
            "method": method,
            "endpoint": endpoint,
            "input_payload_path": input_path,
            "payload": payload,
            "kind": "mutation" if method in {"POST", "PUT", "PATCH", "DELETE"} else "read",
        }
        with self.log_path.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps(record, sort_keys=True) + "\n")
        return subprocess.CompletedProcess(args, 0, json.dumps({"ok": True, "endpoint": endpoint}), "")


class ServerHarness:
    def __init__(
        self,
        session: review.PlanSession,
        fake: FakeGh | None = None,
        runner: Any | None = None,
    ) -> None:
        command_runner = runner or (fake.runner if fake else subprocess.run)
        self.server, self.token, self.nonce = review.start_review_server(
            session,
            token="test-token",
            nonce="test-nonce",
            port=0,
            run_command=command_runner,
        )
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.base_url = f"http://127.0.0.1:{self.server.server_address[1]}"

    def close(self) -> None:
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=2)

    def request(self, path: str, *, method: str = "GET", token: bool = True, data: Any = None, raw_data: bytes | None = None) -> tuple[int, dict[str, str], bytes]:
        headers: dict[str, str] = {}
        body = None
        if token:
            headers["X-Scherzo-Review-Token"] = self.token
        if raw_data is not None:
            body = raw_data
            headers["Content-Type"] = "application/json"
        elif data is not None:
            body = json.dumps(data).encode("utf-8")
            headers["Content-Type"] = "application/json"
        req = urllib.request.Request(self.base_url + path, data=body, headers=headers, method=method)
        try:
            with urllib.request.urlopen(req, timeout=5) as response:
                return response.status, dict(response.headers), response.read()
        except urllib.error.HTTPError as exc:
            return exc.code, dict(exc.headers), exc.read()


class ExecPlanReviewTest(unittest.TestCase):
    def make_session(self, tmp: Path, *, plan_path: str = "docs/plans/example.html", review_mode: str = "interactive-html", source: str = FIXTURE_HTML, patch: str | None = "@@ -0,0 +1,5 @@\n+<!doctype html>\n+<html><body>\n+<h1>Purpose</h1>\n+<p>Risk text</p>\n+</body></html>") -> review.PlanSession:
        source_hash = review.source_hash_bytes(source.encode("utf-8"))
        session = review.make_plan_session(
            repo="owner/repo",
            pr_number=123,
            pr_url="https://github.com/owner/repo/pull/123",
            plan_path=plan_path,
            head_repo="owner/repo",
            head_sha="abc123",
            output_dir=tmp / "tmp" / "scherzo-execplan-review",
            review_mode=review_mode,
            source_hash=source_hash,
        )
        session.source_path.parent.mkdir(parents=True, exist_ok=True)
        session.source_path.write_text(source, encoding="utf-8")
        files = [{"filename": plan_path, "status": "modified"}]
        if patch is not None:
            files[0]["patch"] = patch
        review.write_pr_files(session, files)
        review.write_session_json(session)
        if review_mode == "interactive-html":
            review.prepare_review_preview(session, token="test-token", nonce="test-nonce")
        return session

    def test_parse_args_preserves_preview_and_makes_review_explicit(self) -> None:
        preview = review.parse_args(["123"])
        self.assertEqual(preview.mode, "preview")
        self.assertEqual(preview.pr_number, "123")

        alias = review.parse_args(["open", "123", "--no-open"])
        self.assertEqual(alias.mode, "preview")
        self.assertTrue(alias.no_open)

        interactive = review.parse_args(["review", "123", "--no-open", "--port", "0"])
        self.assertEqual(interactive.mode, "review")
        self.assertTrue(interactive.no_open)
        self.assertEqual(interactive.port, 0)

    def test_help_mentions_markdown_source_and_temporary_html_preview(self) -> None:
        stdout = io.StringIO()
        with contextlib.redirect_stdout(stdout), self.assertRaises(SystemExit) as raised:
            review.parse_args(["--help"])
        self.assertEqual(raised.exception.code, 0)
        help_text = stdout.getvalue()
        self.assertIn("Markdown is the source of truth", help_text)
        self.assertIn("temporary HTML previews", help_text)
        self.assertIn("Legacy HTML plan PRs remain supported", help_text)

    def test_session_paths_include_contract_and_submit_artifacts(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp)
            expected_root = tmp / "tmp" / "scherzo-execplan-review" / "owner__repo" / "pr-123"
            self.assertEqual(session.session_root, expected_root)
            self.assertEqual(session.source_path, expected_root / "docs" / "plans" / "example.html")
            self.assertEqual(session.preview_path, expected_root / "preview" / "index.html")
            self.assertEqual(session.api_contract_probe_payload_path, expected_root / "api-contract-probe-payload.json")
            self.assertEqual(session.submit_payload_path, expected_root / "submit-payload.json")

    def test_preview_mode_with_fake_gh_is_read_only(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            fake = FakeGh(tmp)
            args = review.parse_args(["preview", "123", "--repo", "owner/repo", "--no-open", "--output-dir", str(tmp / "out")])
            stdout = io.StringIO()
            with fake.on_path(), contextlib.redirect_stdout(stdout):
                result = review.preview_pr(args)
            self.assertEqual(result, 0)
            output = stdout.getvalue()
            self.assertIn("PLAN_PR_PATH=docs/plans/example.html", output)
            self.assertIn("PLAN_PREVIEW_KIND=html", output)
            self.assertIn("PLAN_REMOTE_MUTATIONS=none", output)
            invocations = fake.invocations()
            self.assertGreaterEqual(len(invocations), 3)
            self.assertTrue(all(item["kind"] == "read" for item in invocations))

    def test_markdown_preview_mode_downloads_renders_and_reports_source(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            fake = FakeGh(
                tmp,
                plan_path="docs/plans/example.md",
                content=FIXTURE_MARKDOWN,
                files=[{"filename": "docs/plans/example.md", "status": "modified", "patch": MARKDOWN_PATCH}],
            )
            args = review.parse_args(["preview", "123", "--repo", "owner/repo", "--no-open", "--output-dir", str(tmp / "out")])
            stdout = io.StringIO()
            with fake.on_path(), contextlib.redirect_stdout(stdout):
                result = review.preview_pr(args)
            self.assertEqual(result, 0)
            output = stdout.getvalue()
            self.assertIn("PLAN_PR_PATH=docs/plans/example.md", output)
            self.assertIn("PLAN_PREVIEW_KIND=rendered-markdown", output)
            self.assertIn("PLAN_REMOTE_MUTATIONS=none", output)
            source_path = Path(next(line.split("=", 1)[1] for line in output.splitlines() if line.startswith("PLAN_SOURCE_PATH=")))
            preview_path = Path(next(line.split("=", 1)[1] for line in output.splitlines() if line.startswith("PLAN_PREVIEW_PATH=")))
            self.assertEqual(source_path, (tmp / "out" / "owner__repo" / "pr-123" / "docs" / "plans" / "example.md").resolve())
            self.assertEqual(preview_path.suffix, ".html")
            self.assertTrue(str(preview_path).startswith(str((tmp / "out").resolve())))
            self.assertTrue(preview_path.exists())
            invocations = fake.invocations()
            self.assertGreaterEqual(len(invocations), 3)
            self.assertTrue(all(item["kind"] == "read" for item in invocations))

    def test_find_plan_file_accepts_custom_markdown_path(self) -> None:
        self.assertEqual(
            review.find_plan_file([
                {
                    "filename": "doobar/docs/plans/example.md",
                    "status": "modified",
                    "patch": MARKDOWN_PATCH,
                }
            ]),
            "doobar/docs/plans/example.md",
        )

    def test_find_plan_file_rejects_unrelated_markdown_path(self) -> None:
        with self.assertRaises(SystemExit):
            review.find_plan_file([
                {
                    "filename": "README.md",
                    "status": "modified",
                    "patch": MARKDOWN_PATCH,
                }
            ])

    def test_markdown_renderer_emits_source_line_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(
                Path(directory),
                plan_path="docs/plans/example.md",
                review_mode="preview",
                source=FIXTURE_MARKDOWN,
                patch=MARKDOWN_PATCH,
            )
            self.assertEqual(review.render_preview_html(session), "rendered-markdown")
            rendered = session.preview_path.read_text(encoding="utf-8")
            self.assertRegex(rendered, r'<h1\b[^>]*class="commentable[^"]*plan-heading"[^>]*data-source-line="1"')
            self.assertRegex(rendered, r'<p\b[^>]*class="commentable[^"]*plan-paragraph"[^>]*data-source-line="3"')

    def test_markdown_renderer_keeps_list_items_commentable_by_source_line(self) -> None:
        markdown = "\n".join(
            [
                "# Plan",
                "",
                "## Concrete Steps",
                "",
                "- [ ] First item with `code`",
                "- [x] Second item with **bold**",
                "",
                "Paragraph with <unsafe> text.",
            ]
        )
        rendered = review.render_markdown_to_html(markdown, "docs/plans/example.md")
        self.assertRegex(
            rendered,
            r'<li\b[^>]*data-comment-id="li-0005-first-item-with-code"[^>]*data-source-line="5"',
        )
        self.assertRegex(
            rendered,
            r'<li\b[^>]*data-comment-id="li-0006-second-item-with-bold"[^>]*data-source-line="6"',
        )
        self.assertIn("<code>code</code>", rendered)
        self.assertIn("<strong>bold</strong>", rendered)
        self.assertIn("Paragraph with &lt;unsafe&gt; text.", rendered)
        self.assertNotIn("<unsafe>", rendered)

    def test_markdown_preview_and_brief_helper_share_heading_contract(self) -> None:
        markdown = "\n".join(
            [
                "# Sample ExecPlan",
                "",
                "## Outcomes & Retrospective",
                "",
                "Record outcomes.",
                "",
                "### Nested Follow-up",
                "",
                "- Keep preview anchors and brief slugs aligned.",
            ]
        )
        brief_helper = load_brief_helper()
        document = brief_helper.parse_markdown_document(
            "docs/plans/sample.md", "fixture-sha", markdown
        )
        rendered = review.render_markdown_to_html(markdown, "docs/plans/sample.md")
        self.assertEqual([section.slug for section in document.sections], [
            "sample-execplan",
            "outcomes-and-retrospective",
            "nested-follow-up",
        ])
        for section in document.sections:
            prefix = "title" if section.level == 1 else f"h{section.level}"
            self.assertIn(f'data-comment-id="{prefix}-{section.slug}"', rendered)

    def test_contract_probe_payload_and_submit_runner_use_review_endpoint(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp)
            fake = FakeGh(tmp)
            result = review.run_contract_probe(session, line=3, body="Probe body", run_command=fake.runner)
            self.assertTrue(result["ok"])
            payload = json.loads(session.api_contract_probe_payload_path.read_text(encoding="utf-8"))
            self.assertEqual(payload["commit_id"], "abc123")
            self.assertEqual(payload["event"], "COMMENT")
            self.assertEqual(payload["comments"], [{"path": "docs/plans/example.html", "body": "Probe body", "side": "RIGHT", "line": 3}])
            invocation = fake.invocations()[-1]
            self.assertEqual(invocation["endpoint"], "repos/owner/repo/pulls/123/reviews")
            self.assertEqual(invocation["kind"], "mutation")

    def test_sanitizer_strips_active_and_remote_content_but_keeps_comment_metadata(self) -> None:
        hostile = """<!doctype html><html><head><base href="https://evil.test/"><link rel="stylesheet" href="https://evil.test/a.css"><meta http-equiv="refresh" content="0; url=https://evil.test/"></head><body><script>alert(1)</script><embed src="https://evil.test/embed"><h1 class="commentable" data-comment-id="safe" data-source-line="7" onclick="steal()">Safe</h1><a href="javascript:alert(1)" ping="https://evil.test/p">link</a><a href="#local">local</a><img src="https://evil.test/i.png"><img src="data:image/png;base64,AAAA"><iframe srcdoc="<script>x</script>"></iframe><form action="https://evil.test/post"><button formaction="https://evil.test/x">Go</button></form></body></html>"""
        sanitized = review.sanitize_pr_html(hostile, "nonce")
        self.assertIn('data-comment-id="safe"', sanitized)
        self.assertIn('data-source-line="7"', sanitized)
        self.assertNotIn("<script", sanitized.lower())
        self.assertNotIn("onclick", sanitized.lower())
        self.assertNotIn("javascript:", sanitized.lower())
        self.assertNotIn("<base", sanitized.lower())
        self.assertNotIn("<link", sanitized.lower())
        self.assertNotIn("http-equiv", sanitized.lower())
        self.assertNotIn("https://evil.test", sanitized)
        self.assertNotIn("srcdoc", sanitized.lower())
        self.assertNotIn("ping=", sanitized.lower())
        self.assertIn('href="#"', sanitized)
        self.assertIn('href="#local"', sanitized)
        self.assertIn('src="data:image/png;base64,AAAA"', sanitized)

    def test_injected_preview_contains_drawer_api_header_and_no_environment_token(self) -> None:
        old = os.environ.get("GITHUB_TOKEN")
        os.environ["GITHUB_TOKEN"] = "SENTINEL_SECRET_TOKEN"
        try:
            injected = review.inject_review_ui(
                "<html><body><p>Plan</p></body></html>",
                {"apiBase": "/api", "token": "session-token"},
                "nonce-value",
            )
        finally:
            if old is None:
                os.environ.pop("GITHUB_TOKEN", None)
            else:
                os.environ["GITHUB_TOKEN"] = old
        self.assertIn('id="scherzo-review-drawer"', injected)
        self.assertIn('"apiBase": "/api"', injected)
        self.assertIn("X-Scherzo-Review-Token", injected)
        self.assertIn('nonce="nonce-value"', injected)
        self.assertNotIn("SENTINEL_SECRET_TOKEN", injected)

    def test_server_requires_token_and_serves_exact_security_headers(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(Path(directory))
            harness = ServerHarness(session)
            try:
                status, _headers, _body = harness.request("/api/drafts", token=False)
                self.assertEqual(status, 403)

                status, _headers, body = harness.request("/api/drafts")
                self.assertEqual(status, 200)
                self.assertEqual(json.loads(body), review.load_drafts_document(session))

                status, headers, body = harness.request("/?token=test-token", token=False)
                self.assertEqual(status, 200)
                self.assertIn(b"scherzo-review-drawer", body)
                expected = review.preview_security_headers("test-nonce")
                for key, value in expected.items():
                    self.assertEqual(headers[key], value)
            finally:
                harness.close()

    def test_server_draft_create_update_delete_and_bad_json(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(Path(directory))
            harness = ServerHarness(session)
            try:
                create = {
                    "data_comment_id": "heading-purpose",
                    "dom_tag": "h1",
                    "dom_source_line": 3,
                    "text_excerpt": "Purpose",
                    "body": "Please clarify purpose.",
                }
                status, _headers, body = harness.request("/api/drafts", method="POST", data=create)
                self.assertEqual(status, 201)
                draft = json.loads(body)["draft"]
                self.assertTrue(session.drafts_path.exists())

                status, _headers, _body = harness.request("/api/drafts", method="POST", raw_data=b"{")
                self.assertEqual(status, 400)
                self.assertEqual(len(review.load_drafts(session)), 1)

                status, _headers, body = harness.request(f"/api/drafts/{draft['id']}", method="PUT", data={"body": "Updated body"})
                self.assertEqual(status, 200)
                updated = json.loads(body)["draft"]
                self.assertEqual(updated["body"], "Updated body")
                self.assertGreaterEqual(updated["updated_at"], draft["updated_at"])

                status, _headers, _body = harness.request(f"/api/drafts/{draft['id']}", method="DELETE")
                self.assertEqual(status, 200)
                self.assertEqual(review.load_drafts(session), [])
            finally:
                harness.close()

    def test_line_mapping_returns_unique_source_line_only(self) -> None:
        self.assertEqual(review.line_for_comment_id(FIXTURE_HTML, "heading-purpose"), 3)
        self.assertIsNone(review.line_for_comment_id(FIXTURE_HTML, "missing"))
        duplicate = FIXTURE_HTML + '\n<p data-comment-id="heading-purpose">Duplicate</p>'
        self.assertIsNone(review.line_for_comment_id(duplicate, "heading-purpose"))

    def test_diff_parser_includes_right_side_context_and_additions_only(self) -> None:
        patch = "@@ -1,2 +10,4 @@\n context\n-old\n+new\n context2\n+added"
        self.assertEqual(review.right_side_commentable_lines(patch), {10, 11, 12, 13})

    def test_build_review_submission_mixes_inline_and_fallback(self) -> None:
        patch = "@@ -1,2 +3,1 @@\n+<h1>Purpose</h1>"
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(Path(directory), patch=patch)
            drafts = [
                review.DraftComment("1", "heading-purpose", "h1", 3, "Purpose", "Inline body", "now", "now"),
                review.DraftComment("2", "paragraph-risk", "p", 4, "Risk text", "Fallback body", "now", "now"),
            ]
            pr_file = review.load_pr_file_record(session)
            submission = review.build_review_submission(session, drafts, pr_file)
            self.assertFalse(submission.summary_only)
            self.assertEqual(
                submission.inline_comments,
                [
                    {
                        "path": "docs/plans/example.html",
                        "body": "Inline body\n\nSelected block: heading-purpose",
                        "side": "RIGHT",
                        "line": 3,
                    }
                ],
            )
            self.assertEqual(len(submission.fallback_entries), 1)
            self.assertIn("Fallback body", submission.payload["body"])

    def test_markdown_review_drafts_submit_against_markdown_line(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(
                tmp,
                plan_path="docs/plans/example.md",
                source=FIXTURE_MARKDOWN,
                patch=MARKDOWN_PATCH,
            )
            harness = ServerHarness(session)
            try:
                status, _headers, body = harness.request("/?token=test-token", token=False)
                self.assertEqual(status, 200)
                html_text = body.decode("utf-8")
                match = re.search(r'<h1\b[^>]*data-comment-id="([^"]+)"[^>]*data-source-line="(\d+)"', html_text)
                self.assertIsNotNone(match)
                assert match is not None
                comment_id, source_line = match.group(1), int(match.group(2))
                status, _headers, _body = harness.request(
                    "/api/drafts",
                    method="POST",
                    data={
                        "data_comment_id": comment_id,
                        "dom_tag": "h1",
                        "dom_source_line": source_line,
                        "text_excerpt": "Example",
                        "body": "Please clarify the title.",
                    },
                )
                self.assertEqual(status, 201)

                drafts = review.load_drafts(session)
                submission = review.build_review_submission(session, drafts, review.load_pr_file_record(session))
                self.assertFalse(submission.summary_only)
                self.assertEqual(
                    submission.inline_comments[0],
                    {
                        "path": "docs/plans/example.md",
                        "body": f"Please clarify the title.\n\nSelected block: {comment_id}",
                        "side": "RIGHT",
                        "line": 1,
                    },
                )
            finally:
                harness.close()

    def test_markdown_missing_or_non_diff_source_line_uses_summary_fallback(self) -> None:
        patch = "@@ -0,0 +1,1 @@\n+# Example"
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(
                Path(directory),
                plan_path="docs/plans/example.md",
                source=FIXTURE_MARKDOWN,
                patch=patch,
            )
            drafts = [
                review.DraftComment("1", "missing-line", "h1", None, "Example", "Missing line", "now", "now"),
                review.DraftComment("2", "zero-line", "h1", 0, "Example", "Zero line", "now", "now"),
                review.DraftComment("3", "past-end", "p", 99, "Paragraph", "Past end", "now", "now"),
                review.DraftComment("4", "not-in-diff", "p", 3, "Paragraph", "Not in diff", "now", "now"),
            ]
            submission = review.build_review_submission(session, drafts, review.load_pr_file_record(session))
            self.assertTrue(submission.summary_only)
            self.assertEqual(submission.inline_comments, [])
            self.assertEqual(len(submission.fallback_entries), 4)
            self.assertIsNone(drafts[0].source_line)
            self.assertIsNone(drafts[1].source_line)
            self.assertIsNone(drafts[2].source_line)
            self.assertEqual(drafts[3].source_line, 3)
            self.assertFalse(drafts[3].diff_eligible)

    def test_legacy_html_mapping_ignores_markdown_dom_line_fallback(self) -> None:
        patch = "@@ -0,0 +1,5 @@\n+<!doctype html>\n+<html><body>\n+<h1>Purpose</h1>\n+<p>Risk text</p>\n+</body></html>"
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(Path(directory), patch=patch)
            drafts = [
                review.DraftComment(
                    "1",
                    "heading-purpose",
                    "h1",
                    99,
                    "Purpose",
                    "Use the HTML source line.",
                    "now",
                    "now",
                )
            ]
            submission = review.build_review_submission(session, drafts, review.load_pr_file_record(session))
            self.assertFalse(submission.summary_only)
            self.assertEqual(submission.inline_comments[0]["path"], "docs/plans/example.html")
            self.assertEqual(submission.inline_comments[0]["line"], 3)

    def test_submit_review_uses_fake_gh_only_when_submit_is_called(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp)
            fake = FakeGh(tmp)
            self.assertEqual(fake.invocations(), [])
            submission = review.ReviewSubmission(
                inline_comments=[{"path": session.plan_path, "body": "Body", "side": "RIGHT", "line": 3}],
                fallback_entries=[],
                payload={"commit_id": session.head_sha, "event": "COMMENT", "body": "Body", "comments": [{"path": session.plan_path, "body": "Body", "side": "RIGHT", "line": 3}]},
            )
            result = review.submit_review(session, submission, fake.runner)
            self.assertTrue(result["ok"])
            invocations = fake.invocations()
            self.assertEqual(len(invocations), 1)
            self.assertEqual(invocations[0]["endpoint"], "repos/owner/repo/pulls/123/reviews")
            payload = json.loads(session.submit_payload_path.read_text(encoding="utf-8"))
            self.assertEqual(payload["commit_id"], "abc123")
            self.assertEqual(payload["event"], "COMMENT")
            self.assertEqual(payload["comments"], submission.inline_comments)

    def test_summary_only_submit_requires_confirmation_then_posts_issue_comment(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp, patch=None)
            fake = FakeGh(tmp)
            harness = ServerHarness(session, fake)
            try:
                status, _headers, _body = harness.request(
                    "/api/drafts",
                    method="POST",
                    data={
                        "data_comment_id": "heading-purpose",
                        "dom_tag": "h1",
                        "dom_source_line": 3,
                        "text_excerpt": "Purpose",
                        "body": "Summary only body",
                    },
                )
                self.assertEqual(status, 201)

                status, _headers, body = harness.request("/api/submit", method="POST", data={})
                self.assertEqual(status, 409)
                self.assertTrue(json.loads(body)["requires_summary_confirmation"])
                self.assertFalse(any(item["kind"] == "mutation" for item in fake.invocations()))

                status, _headers, _body = harness.request(
                    "/api/submit-summary",
                    method="POST",
                    data={"confirm_summary_only": True},
                )
                self.assertEqual(status, 200)
                mutation = [item for item in fake.invocations() if item["kind"] == "mutation"]
                self.assertEqual(len(mutation), 1)
                self.assertEqual(mutation[0]["endpoint"], "repos/owner/repo/issues/123/comments")
            finally:
                harness.close()

    def test_failed_inline_submit_offers_explicit_summary_retry(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp)
            review.create_draft(
                session,
                {
                    "data_comment_id": "heading-purpose",
                    "dom_tag": "h1",
                    "dom_source_line": 3,
                    "text_excerpt": "Purpose",
                    "body": "Inline body",
                },
            )
            endpoints: list[str] = []

            def runner(args: list[str], **_kwargs: Any) -> subprocess.CompletedProcess[str]:
                endpoint = next(arg for arg in args if arg.startswith("repos/"))
                endpoints.append(endpoint)
                if endpoint.endswith("/reviews"):
                    return subprocess.CompletedProcess(args, 1, "", "review rejected")
                return subprocess.CompletedProcess(
                    args,
                    0,
                    json.dumps({"ok": True, "endpoint": endpoint}),
                    "",
                )

            harness = ServerHarness(session, runner=runner)
            try:
                status, _headers, body = harness.request("/api/submit", method="POST", data={})
                self.assertEqual(status, 502)
                response = json.loads(body)
                self.assertTrue(response["requires_summary_confirmation"])
                self.assertIn("GitHub rejected", response["summary_retry_reason"])
                self.assertEqual(endpoints, ["repos/owner/repo/pulls/123/reviews"])
                self.assertIsNone(review.load_drafts(session)[0].submitted_at)

                status, _headers, _body = harness.request(
                    "/api/submit-summary",
                    method="POST",
                    data={"confirm_summary_only": True},
                )
                self.assertEqual(status, 200)
                self.assertEqual(
                    endpoints,
                    [
                        "repos/owner/repo/pulls/123/reviews",
                        "repos/owner/repo/issues/123/comments",
                    ],
                )
                self.assertIsNotNone(review.load_drafts(session)[0].submitted_at)
            finally:
                harness.close()

    def test_submit_rejects_when_all_drafts_are_already_submitted(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            session = self.make_session(tmp)
            fake = FakeGh(tmp)
            review.create_draft(
                session,
                {
                    "data_comment_id": "heading-purpose",
                    "dom_tag": "h1",
                    "dom_source_line": 3,
                    "text_excerpt": "Purpose",
                    "body": "Already submitted body",
                },
            )
            review.mark_drafts_submitted(session, submitted_at="2026-05-10T21:00:00Z")
            harness = ServerHarness(session, fake)
            try:
                status, _headers, body = harness.request("/api/submit", method="POST", data={})
                self.assertEqual(status, 400)
                self.assertEqual(json.loads(body)["error"], "no unsubmitted draft comments")

                status, _headers, body = harness.request(
                    "/api/submit-summary",
                    method="POST",
                    data={"confirm_summary_only": True},
                )
                self.assertEqual(status, 400)
                self.assertEqual(json.loads(body)["error"], "no unsubmitted draft comments")
                self.assertFalse(any(item["kind"] == "mutation" for item in fake.invocations()))
            finally:
                harness.close()

    def test_markdown_review_mode_serves_interactive_rendered_viewer(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            session = self.make_session(
                Path(directory),
                plan_path="docs/plans/example.md",
                source=FIXTURE_MARKDOWN,
                patch=MARKDOWN_PATCH,
            )
            self.assertEqual(session.review_mode, "interactive-html")
            harness = ServerHarness(session)
            try:
                status, _headers, _body = harness.request("/?token=test-token", token=False)
                self.assertEqual(status, 200)

                status, headers, body = harness.request("/", token=False)
                self.assertEqual(status, 403)

                status, headers, body = harness.request("/?token=test-token", token=False)
                self.assertEqual(status, 200)
                html_text = body.decode("utf-8")
                self.assertIn("scherzo-review-drawer", html_text)
                self.assertIn("Example", html_text)
                self.assertIn("This paragraph maps to line three.", html_text)
                self.assertRegex(html_text, r'<h1\b[^>]*data-comment-id="[^"]+"[^>]*data-source-line="1"')
                self.assertRegex(html_text, r'<p\b[^>]*data-comment-id="[^"]+"[^>]*data-source-line="3"')
                expected = review.preview_security_headers("test-nonce")
                for key, value in expected.items():
                    self.assertEqual(headers[key], value)
            finally:
                harness.close()

    def test_non_execplan_pr_fails_before_mutation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            fake = FakeGh(tmp, files=[{"filename": "README.md", "status": "modified"}])
            args = review.parse_args(["preview", "123", "--repo", "owner/repo", "--no-open", "--output-dir", str(tmp / "out")])
            with fake.on_path(), self.assertRaises(SystemExit):
                review.preview_pr(args)
            self.assertTrue(all(item["kind"] == "read" for item in fake.invocations()))


if __name__ == "__main__":
    unittest.main()
