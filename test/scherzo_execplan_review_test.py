from __future__ import annotations

import base64
import contextlib
import io
import json
import os
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

    def test_markdown_review_mode_remains_preview_only(self) -> None:
        markdown = "# Example\n\nThis remains preview-only.\n"
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            fake = FakeGh(
                tmp,
                plan_path="docs/plans/example.md",
                content=markdown,
                files=[{"filename": "docs/plans/example.md", "status": "modified", "patch": "@@ -0,0 +1,3 @@\n+# Example"}],
            )
            args = review.parse_args(["review", "123", "--repo", "owner/repo", "--no-open", "--output-dir", str(tmp / "out")])
            stdout = io.StringIO()
            with fake.on_path(), contextlib.redirect_stdout(stdout):
                result = review.review_pr(args)
            self.assertEqual(result, 0)
            output = stdout.getvalue()
            self.assertIn("PLAN_PREVIEW_KIND=rendered-markdown", output)
            self.assertIn("PLAN_REMOTE_MUTATIONS=none", output)
            self.assertNotIn("PLAN_REVIEW_MODE=interactive-html", output)
            self.assertTrue(all(item["kind"] == "read" for item in fake.invocations()))

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
