#!/usr/bin/env python3
"""Local helpers for reviewing Scherzo ExecPlan PR artifacts.

The default command is intentionally read-only: given a PR number for the
current GitHub repository, find the changed ExecPlan file under docs/plans/,
download the PR-head version into tmp/, render Markdown plans to the standard
ExecPlan HTML shell when needed, and open the preview in a browser.

The explicit ``review`` subcommand adds a loopback-only interactive HTML review
server. It keeps GitHub mutation on the Python side of the localhost boundary
and only submits feedback after the browser asks the server to do so.
"""

from __future__ import annotations

import argparse
import base64
import hashlib
import html
import json
import os
import re
import secrets
import shutil
import subprocess
import sys
import tempfile
import threading
import urllib.parse
import uuid
import webbrowser
from dataclasses import asdict, dataclass, replace
from datetime import datetime, timezone
from html.parser import HTMLParser
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path, PurePosixPath
from typing import Any, Callable

PLAN_RE = re.compile(r"^docs/plans/[^/]+\.(?:html|md)$", re.IGNORECASE)
DEFAULT_OUTPUT_DIR = Path("tmp") / "scherzo-execplan-review"
SERVER_HOST = "127.0.0.1"
MAX_DRAFT_BODY_CHARS = 16_000
MAX_DRAFTS = 200

CommandRunner = Callable[..., subprocess.CompletedProcess[str]]


@dataclass(frozen=True)
class PlanSession:
    repo: str
    pr_number: int
    pr_url: str
    plan_path: str
    head_repo: str
    head_sha: str
    session_root: Path
    source_path: Path
    preview_path: Path
    drafts_path: Path
    pr_files_path: Path
    api_contract_probe_payload_path: Path
    api_contract_probe_result_path: Path
    api_contract_probe_error_path: Path
    submit_payload_path: Path
    submit_result_path: Path
    submit_error_path: Path
    session_json_path: Path
    review_mode: str = "preview"
    source_hash: str = ""


@dataclass
class DraftComment:
    id: str
    data_comment_id: str
    dom_tag: str
    dom_source_line: int | None
    text_excerpt: str
    body: str
    created_at: str
    updated_at: str
    source_line: int | None = None
    diff_eligible: bool | None = None
    submitted_at: str | None = None

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "DraftComment":
        return cls(
            id=str(data.get("id") or ""),
            data_comment_id=str(data.get("data_comment_id") or ""),
            dom_tag=str(data.get("dom_tag") or ""),
            dom_source_line=_optional_int(data.get("dom_source_line")),
            text_excerpt=str(data.get("text_excerpt") or ""),
            body=str(data.get("body") or ""),
            created_at=str(data.get("created_at") or ""),
            updated_at=str(data.get("updated_at") or ""),
            source_line=_optional_int(data.get("source_line")),
            diff_eligible=_optional_bool(data.get("diff_eligible")),
            submitted_at=_optional_str(data.get("submitted_at")),
        )

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


@dataclass(frozen=True)
class InlineCommentCoordinate:
    path: str
    side: str
    line: int


@dataclass(frozen=True)
class ReviewSubmission:
    inline_comments: list[dict[str, object]]
    fallback_entries: list[dict[str, object]]
    payload: dict[str, object]
    summary_only: bool = False


@dataclass(frozen=True)
class ReviewServerState:
    session: PlanSession
    token: str
    nonce: str
    run_command: CommandRunner


class ReviewSubmitError(RuntimeError):
    def __init__(self, error: dict[str, Any]) -> None:
        super().__init__(str(error.get("message") or "GitHub submit failed"))
        self.error = error


def fail(message: str) -> None:
    print(f"scherzo-execplan-review: {message}", file=sys.stderr)
    sys.exit(1)


def run(args: list[str], *, check: bool = True) -> str:
    proc = subprocess.run(
        args,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if check and proc.returncode != 0:
        stderr = proc.stderr.strip()
        stdout = proc.stdout.strip()
        details = stderr or stdout or f"exit code {proc.returncode}"
        fail(f"command failed: {' '.join(args)}\n{details}")
    return proc.stdout


def run_gh(args: list[str]) -> str:
    if shutil.which("gh") is None:
        fail("required command not found: gh")
    return run(["gh", *args])


def gh_json(endpoint: str) -> Any:
    output = run_gh(["api", endpoint])
    try:
        return json.loads(output)
    except json.JSONDecodeError as exc:
        fail(f"gh api returned malformed JSON for {endpoint}: {exc}")


def infer_current_repo() -> str:
    output = run_gh(["repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner"])
    repo = output.strip()
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repo):
        fail(f"could not infer GitHub owner/repo from current directory: {repo!r}")
    return repo


def validate_repo(repo: str) -> str:
    repo = repo.strip()
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repo):
        fail(f"invalid GitHub owner/repo: {repo!r}")
    return repo


def parse_pr_number(value: str) -> int:
    try:
        number = int(value, 10)
    except ValueError:
        fail(f"invalid PR number: {value!r}")
    if number <= 0:
        fail(f"invalid PR number: {value!r}")
    return number


def paginated_pr_files(repo: str, pr_number: int) -> list[dict[str, Any]]:
    files: list[dict[str, Any]] = []
    page = 1
    while True:
        endpoint = f"repos/{repo}/pulls/{pr_number}/files?per_page=100&page={page}"
        page_items = gh_json(endpoint)
        if not isinstance(page_items, list):
            fail(f"unexpected GitHub response for PR files: {type(page_items).__name__}")
        files.extend(item for item in page_items if isinstance(item, dict))
        if len(page_items) < 100:
            return files
        page += 1


def find_plan_file(files: list[dict[str, Any]]) -> str:
    candidates: list[str] = []
    removed_candidates: list[str] = []
    for item in files:
        path = str(item.get("filename") or "")
        if not PLAN_RE.fullmatch(path):
            continue
        if item.get("status") == "removed":
            removed_candidates.append(path)
        else:
            candidates.append(path)

    unique_candidates = sorted(set(candidates))
    if len(unique_candidates) == 1:
        return unique_candidates[0]

    if not unique_candidates:
        details = ""
        if removed_candidates:
            details = "; removed plan files are not previewable: " + ", ".join(
                sorted(set(removed_candidates))
            )
        fail(f"expected exactly one changed docs/plans/*.html or *.md plan file, found 0{details}")

    fail(
        "expected exactly one changed docs/plans/*.html or *.md plan file, found "
        f"{len(unique_candidates)}: " + ", ".join(unique_candidates)
    )


def pr_metadata(repo: str, pr_number: int) -> dict[str, Any]:
    data = gh_json(f"repos/{repo}/pulls/{pr_number}")
    if not isinstance(data, dict):
        fail("unexpected GitHub response for PR metadata")
    return data


def head_repo_and_sha(pr: dict[str, Any]) -> tuple[str, str]:
    head = pr.get("head")
    if not isinstance(head, dict):
        fail("PR metadata did not include head information")
    sha = str(head.get("sha") or "").strip()
    repo = head.get("repo")
    if not isinstance(repo, dict):
        fail("PR head repository is unavailable; the source branch may have been deleted")
    full_name = str(repo.get("full_name") or "").strip()
    if not sha:
        fail("PR metadata did not include a head SHA")
    return validate_repo(full_name), sha


def repo_file_bytes(repo: str, ref: str, path: str) -> bytes:
    encoded_path = urllib.parse.quote(path, safe="/")
    encoded_ref = urllib.parse.quote(ref, safe="")
    data = gh_json(f"repos/{repo}/contents/{encoded_path}?ref={encoded_ref}")
    if isinstance(data, list):
        fail(f"expected a file but GitHub returned a directory: {path}")
    if not isinstance(data, dict):
        fail(f"unexpected GitHub response while downloading {path}")
    encoding = data.get("encoding")
    content = data.get("content")
    if encoding != "base64" or not isinstance(content, str):
        fail(f"GitHub contents response for {path} did not include base64 file content")
    try:
        return base64.b64decode(content)
    except Exception as exc:  # pragma: no cover - defensive CLI boundary
        fail(f"could not decode {path}: {exc}")


def safe_local_path(root: Path, repo_path: str) -> Path:
    pure_path = PurePosixPath(repo_path)
    parts = pure_path.parts
    if pure_path.is_absolute() or not parts or any(part in {"", ".", ".."} for part in parts):
        fail(f"unsafe repository path from GitHub: {repo_path!r}")
    return root.joinpath(*parts)


def render_markdown(source_path: Path, preview_path: Path, display_path: str) -> None:
    renderer = Path(__file__).resolve().with_name("scherzo-execplan-html")
    if not renderer.exists():
        fail(f"missing ExecPlan HTML renderer: {renderer}")

    proc = subprocess.run(
        [sys.executable, str(renderer), "render", str(source_path), str(preview_path), display_path],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if proc.returncode != 0:
        details = proc.stderr.strip() or proc.stdout.strip() or f"exit code {proc.returncode}"
        fail(f"failed to render Markdown plan {source_path}:\n{details}")
    if proc.stderr:
        print(proc.stderr, file=sys.stderr, end="")


def browser_open(url: str) -> bool:
    try:
        return bool(webbrowser.open(url, new=2))
    except webbrowser.Error:
        return False


def normalize_args(argv: list[str]) -> list[str]:
    if argv and argv[0] in {"open", "preview"}:
        return argv[1:]
    return argv


def parse_args(argv: list[str]) -> argparse.Namespace:
    remaining = list(argv)
    command = "preview"
    mode = "preview"
    if remaining and remaining[0] in {"open", "preview", "review"}:
        command = remaining.pop(0)
        mode = "review" if command == "review" else "preview"

    parser = argparse.ArgumentParser(
        prog="scherzo-execplan-review",
        usage=(
            "%(prog)s [open|preview] PR_NUMBER [--repo OWNER/REPO] "
            "[--output-dir DIR] [--no-open]\n"
            "       %(prog)s review PR_NUMBER [--repo OWNER/REPO] "
            "[--output-dir DIR] [--no-open] [--port PORT]"
        ),
        description=(
            "Download the single docs/plans ExecPlan changed by a GitHub PR "
            "for this repository and open a local browser preview. Use the "
            "explicit review subcommand for interactive HTML feedback capture."
        ),
        epilog=(
            "Examples:\n"
            "  scripts/scherzo-execplan-review 123\n"
            "  scripts/scherzo-execplan-review open 123 --no-open\n"
            "  scripts/scherzo-execplan-review review 123 --no-open\n"
            "  scripts/scherzo-execplan-review 123 --repo bromanko/scherzo"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("pr_number", help="GitHub pull request number")
    parser.add_argument(
        "--repo",
        help="GitHub owner/repo. Defaults to the repository for the current directory.",
    )
    parser.add_argument(
        "--output-dir",
        default=str(DEFAULT_OUTPUT_DIR),
        help=f"directory for local preview files (default: {DEFAULT_OUTPUT_DIR})",
    )
    parser.add_argument(
        "--no-open",
        action="store_true",
        help="download/render the preview but do not launch a browser",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=0,
        help="localhost port for review mode (default: 0, choose an available port)",
    )
    args = parser.parse_args(remaining)
    args.command = command
    args.mode = mode
    return args


def utc_now() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def source_hash_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def make_plan_session(
    *,
    repo: str,
    pr_number: int,
    pr_url: str,
    plan_path: str,
    head_repo: str,
    head_sha: str,
    output_dir: Path,
    review_mode: str = "preview",
    source_hash: str = "",
) -> PlanSession:
    session_root = output_dir / repo.replace("/", "__") / f"pr-{pr_number}"
    source_path = safe_local_path(session_root, plan_path)
    if review_mode == "interactive-html" and plan_path.lower().endswith(".html"):
        preview_path = session_root / "preview" / "index.html"
    elif plan_path.lower().endswith(".md"):
        preview_path = source_path.with_suffix(".html")
    else:
        preview_path = source_path

    return PlanSession(
        repo=repo,
        pr_number=pr_number,
        pr_url=pr_url,
        plan_path=plan_path,
        head_repo=head_repo,
        head_sha=head_sha,
        session_root=session_root,
        source_path=source_path,
        preview_path=preview_path,
        drafts_path=session_root / "drafts.json",
        pr_files_path=session_root / "pr-files.json",
        api_contract_probe_payload_path=session_root / "api-contract-probe-payload.json",
        api_contract_probe_result_path=session_root / "api-contract-probe-result.json",
        api_contract_probe_error_path=session_root / "api-contract-probe-error.json",
        submit_payload_path=session_root / "submit-payload.json",
        submit_result_path=session_root / "submit-result.json",
        submit_error_path=session_root / "submit-error.json",
        session_json_path=session_root / "session.json",
        review_mode=review_mode,
        source_hash=source_hash,
    )


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_json_atomic(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temp_name = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp", dir=str(path.parent))
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            json.dump(value, handle, indent=2, sort_keys=True)
            handle.write("\n")
        Path(temp_name).replace(path)
    except Exception:
        try:
            Path(temp_name).unlink(missing_ok=True)
        finally:
            raise


def write_session_json(session: PlanSession) -> None:
    data = {
        "version": 1,
        "repo": session.repo,
        "pr_number": session.pr_number,
        "pr_url": session.pr_url,
        "plan_path": session.plan_path,
        "head_repo": session.head_repo,
        "head_sha": session.head_sha,
        "review_mode": session.review_mode,
        "source_hash": session.source_hash,
        "session_root": str(session.session_root),
        "source_path": str(session.source_path),
        "preview_path": str(session.preview_path),
        "drafts_path": str(session.drafts_path),
        "pr_files_path": str(session.pr_files_path),
    }
    write_json(session.session_json_path, data)


def write_pr_files(session: PlanSession, files: list[dict[str, Any]]) -> None:
    write_json(session.pr_files_path, files)


def build_session(args: argparse.Namespace) -> PlanSession:
    session, _pr, _files = prepare_plan_session(args, review_mode=args.mode)
    return session


def prepare_plan_session(
    args: argparse.Namespace, *, review_mode: str
) -> tuple[PlanSession, dict[str, Any], list[dict[str, Any]]]:
    pr_number = parse_pr_number(args.pr_number)
    repo = validate_repo(args.repo) if args.repo else infer_current_repo()

    pr = pr_metadata(repo, pr_number)
    head_repo, head_sha = head_repo_and_sha(pr)
    files = paginated_pr_files(repo, pr_number)
    plan_path = find_plan_file(files)

    effective_mode = "interactive-html" if review_mode == "review" and plan_path.lower().endswith(".html") else "preview"
    session = make_plan_session(
        repo=repo,
        pr_number=pr_number,
        pr_url=str(pr.get("html_url") or ""),
        plan_path=plan_path,
        head_repo=head_repo,
        head_sha=head_sha,
        output_dir=Path(args.output_dir),
        review_mode=effective_mode,
    )

    session.source_path.parent.mkdir(parents=True, exist_ok=True)
    source_bytes = repo_file_bytes(head_repo, head_sha, plan_path)
    session.source_path.write_bytes(source_bytes)
    session = replace(session, source_hash=source_hash_bytes(source_bytes))
    write_pr_files(session, files)
    write_session_json(session)
    return session, pr, files


def prepare_preview(session: PlanSession) -> str:
    if session.plan_path.lower().endswith(".md"):
        render_markdown(session.source_path, session.preview_path, session.plan_path)
        return "rendered-markdown"
    return "html"


def build_contract_probe_payload(session: PlanSession, line: int, body: str) -> dict[str, object]:
    return {
        "commit_id": session.head_sha,
        "event": "COMMENT",
        "body": "Scherzo ExecPlan review contract probe.",
        "comments": [
            {
                "path": session.plan_path,
                "body": body,
                "side": "RIGHT",
                "line": line,
            }
        ],
    }


def run_contract_probe(
    session: PlanSession,
    *,
    line: int,
    body: str,
    run_command: CommandRunner = subprocess.run,
) -> dict[str, Any]:
    payload = build_contract_probe_payload(session, line, body)
    write_json(session.api_contract_probe_payload_path, payload)
    endpoint = f"repos/{session.repo}/pulls/{session.pr_number}/reviews"
    args = [
        "gh",
        "api",
        "--method",
        "POST",
        endpoint,
        "--input",
        str(session.api_contract_probe_payload_path),
    ]
    proc = run_command(args, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        error = command_error_artifact(proc, endpoint, session.api_contract_probe_payload_path, args)
        write_json(session.api_contract_probe_error_path, error)
        raise ReviewSubmitError(error)
    result = parse_json_stdout(proc.stdout)
    write_json(session.api_contract_probe_result_path, result)
    return result


class _Sanitizer(HTMLParser):
    REMOVE_WITH_CONTENT = {
        "script",
        "iframe",
        "frameset",
        "object",
        "portal",
        "svg",
        "math",
        "template",
        "noscript",
    }
    REMOVE_ELEMENTS = {"base", "link", "embed", "frame"}
    STRIP_ATTRS = {
        "srcdoc",
        "srcset",
        "ping",
        "autofocus",
        "action",
        "formaction",
        "poster",
        "background",
        "xlink:href",
    }
    VOID_ELEMENTS = {
        "area",
        "base",
        "br",
        "col",
        "embed",
        "hr",
        "img",
        "input",
        "link",
        "meta",
        "param",
        "source",
        "track",
        "wbr",
    }

    def __init__(self) -> None:
        super().__init__(convert_charrefs=False)
        self.parts: list[str] = []
        self.skip_stack: list[str] = []

    def handle_decl(self, decl: str) -> None:
        if not self.skip_stack:
            self.parts.append(f"<!{decl}>")

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        lower = tag.lower()
        if self.skip_stack:
            if lower in self.REMOVE_WITH_CONTENT:
                self.skip_stack.append(lower)
            return
        if lower in self.REMOVE_WITH_CONTENT:
            self.skip_stack.append(lower)
            return
        if self._drop_element(lower, attrs):
            return
        cleaned = self._clean_attrs(lower, attrs)
        self.parts.append(f"<{lower}{cleaned}>")

    def handle_startendtag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        lower = tag.lower()
        if self.skip_stack or lower in self.REMOVE_WITH_CONTENT or self._drop_element(lower, attrs):
            return
        cleaned = self._clean_attrs(lower, attrs)
        self.parts.append(f"<{lower}{cleaned}>")

    def handle_endtag(self, tag: str) -> None:
        lower = tag.lower()
        if self.skip_stack:
            if lower == self.skip_stack[-1]:
                self.skip_stack.pop()
            return
        if lower in self.REMOVE_WITH_CONTENT or lower in self.REMOVE_ELEMENTS or lower in self.VOID_ELEMENTS:
            return
        self.parts.append(f"</{lower}>")

    def handle_data(self, data: str) -> None:
        if not self.skip_stack:
            self.parts.append(data)

    def handle_entityref(self, name: str) -> None:
        if not self.skip_stack:
            self.parts.append(f"&{name};")

    def handle_charref(self, name: str) -> None:
        if not self.skip_stack:
            self.parts.append(f"&#{name};")

    def handle_comment(self, data: str) -> None:
        if not self.skip_stack:
            self.parts.append(f"<!--{data}-->")

    def _drop_element(self, tag: str, attrs: list[tuple[str, str | None]]) -> bool:
        if tag in self.REMOVE_ELEMENTS:
            return True
        if tag == "meta":
            return any(name.lower() == "http-equiv" for name, _value in attrs)
        return False

    def _clean_attrs(self, tag: str, attrs: list[tuple[str, str | None]]) -> str:
        rendered: list[str] = []
        for raw_name, value in attrs:
            name = raw_name.lower()
            if name.startswith("on") or name in self.STRIP_ATTRS:
                continue
            if name == "href":
                kept = value if value and value.startswith("#") else "#"
                rendered.append(f' href="{html.escape(kept, quote=True)}"')
                continue
            if name == "src":
                if value and value.lower().startswith("data:image/"):
                    rendered.append(f' src="{html.escape(value, quote=True)}"')
                continue
            if value is None:
                rendered.append(f" {html.escape(raw_name, quote=True)}")
            else:
                rendered.append(
                    f' {html.escape(raw_name, quote=True)}="{html.escape(value, quote=True)}"'
                )
        return "".join(rendered)


def sanitize_pr_html(source_html: str, nonce: str) -> str:
    del nonce  # The sanitizer is nonce-independent; injection uses the nonce.
    sanitizer = _Sanitizer()
    sanitizer.feed(source_html)
    sanitizer.close()
    return "".join(sanitizer.parts)


REVIEW_CSS = """
#scherzo-review-drawer {
  position: fixed;
  top: 3rem;
  right: 0;
  bottom: 0;
  width: min(28rem, 94vw);
  background: #ffffff;
  color: #161616;
  border-left: 1px solid #c6c6c6;
  box-shadow: -0.25rem 0 1rem rgba(0, 0, 0, 0.15);
  z-index: 9999;
  padding: 1rem;
  overflow: auto;
  font: 14px/1.45 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
}
#scherzo-review-drawer h2 { margin-top: 0; font-size: 1.1rem; }
#scherzo-review-drawer button { margin: .25rem .25rem .25rem 0; }
#scherzo-review-drafts { list-style: none; padding: 0; margin: .75rem 0; }
#scherzo-review-drafts li { border: 1px solid #e0e0e0; padding: .5rem; margin: .5rem 0; }
#scherzo-review-popover {
  position: fixed;
  left: 1rem;
  bottom: 1rem;
  z-index: 10000;
  width: min(30rem, calc(100vw - 2rem));
  background: #ffffff;
  color: #161616;
  border: 2px solid #0f62fe;
  box-shadow: 0 .25rem 1rem rgba(0, 0, 0, .2);
  padding: .75rem;
  font: 14px/1.45 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
}
#scherzo-review-popover[hidden] { display: none; }
#scherzo-review-popover textarea { width: 100%; min-height: 7rem; display: block; margin: .5rem 0; }
.commentable.scherzo-review-selected { outline: 3px solid #0f62fe; outline-offset: 3px; }
""".strip()


REVIEW_JS = r"""
(() => {
  const CONFIG = window.__SCHERZO_REVIEW_CONFIG__;
  const drawer = document.getElementById('scherzo-review-drawer');
  const list = document.getElementById('scherzo-review-drafts');
  const status = document.getElementById('scherzo-review-status');
  const submit = document.getElementById('scherzo-review-submit');
  const popover = document.getElementById('scherzo-review-popover');
  const textarea = document.getElementById('scherzo-review-body');
  const targetLabel = document.getElementById('scherzo-review-target');
  let selected = null;
  let editingId = null;

  function setStatus(message) { status.textContent = message || ''; }
  function textOf(element) { return (element.textContent || '').replace(/\s+/g, ' ').trim().slice(0, 240); }
  function api(path, options = {}) {
    const headers = Object.assign({}, options.headers || {}, { 'X-Scherzo-Review-Token': CONFIG.token });
    if (options.body && !headers['Content-Type']) headers['Content-Type'] = 'application/json';
    return fetch(CONFIG.apiBase + path, Object.assign({}, options, { headers })).then(async response => {
      const data = await response.json().catch(() => ({}));
      if (!response.ok) {
        const err = new Error(data.error || response.statusText);
        err.response = data;
        err.status = response.status;
        throw err;
      }
      return data;
    });
  }
  function draftPayload(element, body) {
    const line = element.getAttribute('data-source-line');
    return {
      data_comment_id: element.getAttribute('data-comment-id'),
      dom_tag: element.tagName.toLowerCase(),
      dom_source_line: line ? Number(line) : null,
      text_excerpt: textOf(element),
      body
    };
  }
  function renderDrafts(data) {
    const comments = data.comments || [];
    const pending = comments.filter(draft => !draft.submitted_at);
    list.textContent = '';
    submit.disabled = pending.length === 0;
    for (const draft of comments) {
      const item = document.createElement('li');
      const title = document.createElement('strong');
      title.textContent = draft.data_comment_id + (draft.dom_source_line ? ` (DOM line ${draft.dom_source_line})` : '') + (draft.submitted_at ? ' — submitted' : '');
      const excerpt = document.createElement('p');
      excerpt.textContent = draft.text_excerpt || '';
      const body = document.createElement('pre');
      body.textContent = draft.body || '';
      const edit = document.createElement('button');
      edit.type = 'button';
      edit.textContent = 'Edit';
      edit.addEventListener('click', () => {
        editingId = draft.id;
        selected = null;
        targetLabel.textContent = `Editing ${draft.data_comment_id}`;
        textarea.value = draft.body || '';
        popover.hidden = false;
        textarea.focus();
      });
      const remove = document.createElement('button');
      remove.type = 'button';
      remove.textContent = 'Delete';
      remove.addEventListener('click', () => {
        api(`/drafts/${encodeURIComponent(draft.id)}`, { method: 'DELETE' })
          .then(refresh)
          .catch(err => setStatus(err.message));
      });
      item.append(title, excerpt, body, edit, remove);
      list.appendChild(item);
    }
  }
  function refresh() { return api('/drafts').then(renderDrafts).catch(err => setStatus(err.message)); }
  document.addEventListener('click', event => {
    const target = event.target.closest('.commentable');
    if (!target || drawer.contains(event.target) || popover.contains(event.target)) return;
    event.preventDefault();
    if (selected) selected.classList.remove('scherzo-review-selected');
    selected = target;
    editingId = null;
    selected.classList.add('scherzo-review-selected');
    targetLabel.textContent = target.getAttribute('data-comment-id') || '(missing data-comment-id)';
    textarea.value = '';
    popover.hidden = false;
    textarea.focus();
  });
  document.getElementById('scherzo-review-cancel').addEventListener('click', () => {
    if (selected) selected.classList.remove('scherzo-review-selected');
    selected = null;
    editingId = null;
    popover.hidden = true;
  });
  document.getElementById('scherzo-review-save').addEventListener('click', () => {
    const body = textarea.value.trim();
    if (!body) { setStatus('Comment body is required.'); return; }
    const request = editingId
      ? api(`/drafts/${encodeURIComponent(editingId)}`, { method: 'PUT', body: JSON.stringify({ body }) })
      : api('/drafts', { method: 'POST', body: JSON.stringify(draftPayload(selected, body)) });
    request.then(() => {
      if (selected) selected.classList.remove('scherzo-review-selected');
      selected = null;
      editingId = null;
      popover.hidden = true;
      setStatus('Draft saved.');
      refresh();
    }).catch(err => setStatus(err.message));
  });
  submit.addEventListener('click', () => {
    if (!confirm('Submit these ExecPlan review comments to GitHub?')) return;
    api('/submit', { method: 'POST', body: JSON.stringify({}) })
      .then(data => { setStatus(data.message || 'Submitted.'); return refresh(); })
      .catch(err => {
        if (err.response && err.response.requires_summary_confirmation) {
          const retryReason = err.response.summary_retry_reason || 'No inline-safe diff lines were available.';
          if (!confirm(`${retryReason} Submit a PR summary comment instead?`)) return;
          api('/submit-summary', { method: 'POST', body: JSON.stringify({ confirm_summary_only: true }) })
            .then(data => { setStatus(data.message || 'Summary submitted.'); return refresh(); })
            .catch(summaryErr => setStatus(summaryErr.message));
          return;
        }
        setStatus(err.message);
      });
  });
  refresh();
})();
""".strip()


def inject_review_ui(html_text: str, config: dict[str, object], nonce: str) -> str:
    safe_config = json.dumps(config, sort_keys=True).replace("</", "<\\/")
    injection = f"""
<style id="scherzo-review-style">{REVIEW_CSS}</style>
<div id="scherzo-review-drawer" role="complementary" aria-label="ExecPlan review drafts">
  <h2>ExecPlan review drafts</h2>
  <p id="scherzo-review-status" role="status" aria-live="polite"></p>
  <ul id="scherzo-review-drafts"></ul>
  <button id="scherzo-review-submit" type="button" disabled>Submit to GitHub</button>
</div>
<div id="scherzo-review-popover" role="dialog" aria-label="Draft ExecPlan review comment" hidden>
  <strong id="scherzo-review-target"></strong>
  <textarea id="scherzo-review-body" aria-label="Review comment body"></textarea>
  <button id="scherzo-review-save" type="button">Save draft</button>
  <button id="scherzo-review-cancel" type="button">Cancel</button>
</div>
<script nonce="{html.escape(nonce, quote=True)}">window.__SCHERZO_REVIEW_CONFIG__ = {safe_config};</script>
<script nonce="{html.escape(nonce, quote=True)}">{REVIEW_JS}</script>
""".strip()
    if re.search(r"</body\s*>", html_text, flags=re.IGNORECASE):
        return re.sub(
            r"</body\s*>",
            lambda _match: injection + "\n</body>",
            html_text,
            count=1,
            flags=re.IGNORECASE,
        )
    return html_text + "\n" + injection


def preview_security_headers(nonce: str) -> dict[str, str]:
    csp = (
        "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'; "
        "object-src 'none'; media-src 'none'; font-src 'none'; "
        f"script-src 'nonce-{nonce}'; connect-src 'self'; img-src 'self' data:; "
        "style-src 'unsafe-inline'"
    )
    return {
        "Content-Security-Policy": csp,
        "Referrer-Policy": "no-referrer",
        "Cache-Control": "no-store",
        "X-Content-Type-Options": "nosniff",
    }


def json_security_headers() -> dict[str, str]:
    return {
        "Cache-Control": "no-store",
        "X-Content-Type-Options": "nosniff",
    }


def prepare_review_preview(session: PlanSession, *, token: str, nonce: str) -> None:
    source_html = session.source_path.read_text(encoding="utf-8", errors="replace")
    sanitized = sanitize_pr_html(source_html, nonce)
    config = {
        "apiBase": "/api",
        "token": token,
        "repo": session.repo,
        "prNumber": session.pr_number,
        "planPath": session.plan_path,
        "headSha": session.head_sha,
    }
    injected = inject_review_ui(sanitized, config, nonce)
    session.preview_path.parent.mkdir(parents=True, exist_ok=True)
    session.preview_path.write_text(injected, encoding="utf-8")


def _optional_int(value: Any) -> int | None:
    if value is None or value == "":
        return None
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def _optional_bool(value: Any) -> bool | None:
    if value is None:
        return None
    if isinstance(value, bool):
        return value
    return None


def _optional_str(value: Any) -> str | None:
    if value is None:
        return None
    text = str(value)
    return text if text else None


def empty_drafts_document(session: PlanSession) -> dict[str, Any]:
    return {
        "version": 1,
        "repo": session.repo,
        "pr_number": session.pr_number,
        "plan_path": session.plan_path,
        "head_sha": session.head_sha,
        "source_hash": session.source_hash,
        "updated_at": utc_now(),
        "comments": [],
    }


def load_drafts_document(session: PlanSession) -> dict[str, Any]:
    if not session.drafts_path.exists():
        return empty_drafts_document(session)
    try:
        data = json.loads(session.drafts_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return empty_drafts_document(session)
    if not isinstance(data, dict):
        return empty_drafts_document(session)
    if (
        data.get("repo") != session.repo
        or data.get("pr_number") != session.pr_number
        or data.get("plan_path") != session.plan_path
        or data.get("head_sha") != session.head_sha
        or data.get("source_hash") != session.source_hash
    ):
        backup = session.drafts_path.with_name(f"drafts-stale-{int(datetime.now().timestamp())}.json")
        try:
            session.drafts_path.replace(backup)
        except OSError:
            pass
        return empty_drafts_document(session)
    comments = data.get("comments")
    if not isinstance(comments, list):
        data["comments"] = []
    return data


def load_drafts(session: PlanSession) -> list[DraftComment]:
    document = load_drafts_document(session)
    comments = document.get("comments", [])
    return [DraftComment.from_dict(item) for item in comments if isinstance(item, dict)]


def save_drafts(session: PlanSession, drafts: list[DraftComment]) -> dict[str, Any]:
    document = empty_drafts_document(session)
    document["updated_at"] = utc_now()
    document["comments"] = [draft.to_dict() for draft in drafts]
    write_json_atomic(session.drafts_path, document)
    return document


def validate_draft_body(value: Any) -> str:
    body = str(value or "").strip()
    if not body:
        raise ValueError("comment body is required")
    if len(body) > MAX_DRAFT_BODY_CHARS:
        raise ValueError(f"comment body must be at most {MAX_DRAFT_BODY_CHARS} characters")
    return body


def create_draft(session: PlanSession, data: dict[str, Any]) -> DraftComment:
    drafts = load_drafts(session)
    if len(drafts) >= MAX_DRAFTS:
        raise ValueError(f"cannot store more than {MAX_DRAFTS} draft comments")
    data_comment_id = str(data.get("data_comment_id") or "").strip()
    if not data_comment_id:
        raise ValueError("data_comment_id is required")
    now = utc_now()
    draft = DraftComment(
        id=uuid.uuid4().hex,
        data_comment_id=data_comment_id,
        dom_tag=str(data.get("dom_tag") or "").strip(),
        dom_source_line=_optional_int(data.get("dom_source_line")),
        text_excerpt=str(data.get("text_excerpt") or "").strip()[:500],
        body=validate_draft_body(data.get("body")),
        created_at=now,
        updated_at=now,
    )
    drafts.append(draft)
    save_drafts(session, drafts)
    return draft


def update_draft(session: PlanSession, draft_id: str, data: dict[str, Any]) -> DraftComment:
    drafts = load_drafts(session)
    body = validate_draft_body(data.get("body"))
    for draft in drafts:
        if draft.id == draft_id:
            draft.body = body
            draft.updated_at = utc_now()
            draft.submitted_at = None
            save_drafts(session, drafts)
            return draft
    raise KeyError(draft_id)


def delete_draft(session: PlanSession, draft_id: str) -> None:
    drafts = load_drafts(session)
    kept = [draft for draft in drafts if draft.id != draft_id]
    if len(kept) == len(drafts):
        raise KeyError(draft_id)
    save_drafts(session, kept)


def mark_drafts_submitted(session: PlanSession, submitted_at: str | None = None) -> None:
    timestamp = submitted_at or utc_now()
    drafts = load_drafts(session)
    for draft in drafts:
        draft.submitted_at = timestamp
        draft.updated_at = timestamp
    save_drafts(session, drafts)


def line_for_comment_id(source_text: str, comment_id: str) -> int | None:
    pattern = re.compile(
        r"\bdata-comment-id\s*=\s*([\"'])" + re.escape(comment_id) + r"\1"
    )
    matches: list[int] = []
    for index, line in enumerate(source_text.splitlines(), start=1):
        if pattern.search(line):
            matches.append(index)
    if len(matches) == 1:
        return matches[0]
    return None


def right_side_commentable_lines(patch_text: str) -> set[int]:
    lines: set[int] = set()
    new_line: int | None = None
    for line in patch_text.splitlines():
        header = re.match(r"@@\s+-\d+(?:,\d+)?\s+\+(\d+)(?:,\d+)?\s+@@", line)
        if header:
            new_line = int(header.group(1))
            continue
        if new_line is None:
            continue
        if line.startswith("\\"):
            continue
        if line.startswith("+"):
            lines.add(new_line)
            new_line += 1
        elif line.startswith("-"):
            continue
        else:
            # Context lines usually start with a space, but an empty context line
            # may be represented as an empty string after splitlines().
            lines.add(new_line)
            new_line += 1
    return lines


def pr_file_for_path(files: list[dict[str, Any]], path: str) -> dict[str, Any] | None:
    for item in files:
        if item.get("filename") == path:
            return item
    return None


def load_pr_file_record(session: PlanSession) -> dict[str, Any] | None:
    if not session.pr_files_path.exists():
        return None
    try:
        files = json.loads(session.pr_files_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(files, list):
        return None
    dicts = [item for item in files if isinstance(item, dict)]
    return pr_file_for_path(dicts, session.plan_path)


def comment_body_with_footer(draft: DraftComment) -> str:
    return f"{draft.body.strip()}\n\nSelected block: {draft.data_comment_id}"


def fallback_entry_for_draft(draft: DraftComment, source_line: int | None) -> dict[str, object]:
    return {
        "id": draft.id,
        "data_comment_id": draft.data_comment_id,
        "source_line": source_line,
        "dom_source_line": draft.dom_source_line,
        "text_excerpt": draft.text_excerpt,
        "body": draft.body.strip(),
    }


def fallback_body(session: PlanSession, entries: list[dict[str, object]]) -> str:
    if not entries:
        return "Scherzo ExecPlan review feedback."
    lines = [
        f"Scherzo ExecPlan review feedback for {session.plan_path} at {session.head_sha}.",
        "",
        "Fallback comments that could not be anchored inline:",
    ]
    for entry in entries:
        source_line = entry.get("source_line") or entry.get("dom_source_line") or "unknown"
        excerpt = str(entry.get("text_excerpt") or "").strip()
        excerpt_text = f" — {excerpt}" if excerpt else ""
        lines.append(
            f"- {entry.get('data_comment_id')}, source line {source_line}: "
            f"{entry.get('body')}{excerpt_text}"
        )
    return "\n".join(lines)


def build_review_submission(
    session: PlanSession, drafts: list[DraftComment], pr_file: dict[str, object] | None
) -> ReviewSubmission:
    try:
        source_text = session.source_path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        source_text = ""
    patch = pr_file.get("patch") if isinstance(pr_file, dict) else None
    eligible_lines = right_side_commentable_lines(patch) if isinstance(patch, str) else set()
    has_patch = isinstance(patch, str)

    inline_comments: list[dict[str, object]] = []
    fallback_entries: list[dict[str, object]] = []
    for draft in drafts:
        if draft.submitted_at:
            continue
        source_line = draft.source_line
        if source_line is None:
            source_line = line_for_comment_id(source_text, draft.data_comment_id)
        diff_eligible = bool(has_patch and source_line is not None and source_line in eligible_lines)
        draft.source_line = source_line
        draft.diff_eligible = diff_eligible
        if diff_eligible and source_line is not None:
            inline_comments.append(
                {
                    "path": session.plan_path,
                    "body": comment_body_with_footer(draft),
                    "side": "RIGHT",
                    "line": source_line,
                }
            )
        else:
            fallback_entries.append(fallback_entry_for_draft(draft, source_line))

    if inline_comments:
        payload: dict[str, object] = {
            "commit_id": session.head_sha,
            "event": "COMMENT",
            "body": fallback_body(session, fallback_entries),
            "comments": inline_comments,
        }
        return ReviewSubmission(inline_comments, fallback_entries, payload, summary_only=False)

    summary_payload: dict[str, object] = {
        "body": fallback_body(session, fallback_entries),
    }
    return ReviewSubmission(inline_comments, fallback_entries, summary_payload, summary_only=True)


def build_summary_issue_submission(
    session: PlanSession, drafts: list[DraftComment]
) -> ReviewSubmission:
    entries = [fallback_entry_for_draft(draft, draft.source_line) for draft in drafts if not draft.submitted_at]
    return ReviewSubmission([], entries, {"body": fallback_body(session, entries)}, summary_only=True)


def parse_json_stdout(stdout: str) -> dict[str, Any]:
    if not stdout.strip():
        return {}
    try:
        data = json.loads(stdout)
    except json.JSONDecodeError:
        return {"raw_stdout": stdout}
    if isinstance(data, dict):
        return data
    return {"value": data}


def command_error_artifact(
    proc: subprocess.CompletedProcess[str], endpoint: str, payload_path: Path, args: list[str]
) -> dict[str, Any]:
    return {
        "message": "GitHub API command failed",
        "command": args,
        "exit_code": proc.returncode,
        "stdout": proc.stdout,
        "stderr": proc.stderr,
        "endpoint": endpoint,
        "payload_path": str(payload_path),
    }


def submit_review(
    session: PlanSession,
    submission: ReviewSubmission,
    run_command: CommandRunner = subprocess.run,
) -> dict[str, object]:
    write_json(session.submit_payload_path, submission.payload)
    if submission.summary_only:
        endpoint = f"repos/{session.repo}/issues/{session.pr_number}/comments"
    else:
        endpoint = f"repos/{session.repo}/pulls/{session.pr_number}/reviews"
    args = [
        "gh",
        "api",
        "--method",
        "POST",
        endpoint,
        "--input",
        str(session.submit_payload_path),
    ]
    proc = run_command(args, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        error = command_error_artifact(proc, endpoint, session.submit_payload_path, args)
        write_json(session.submit_error_path, error)
        raise ReviewSubmitError(error)
    result = parse_json_stdout(proc.stdout)
    write_json(session.submit_result_path, result)
    return result


def _load_json_body(handler: BaseHTTPRequestHandler) -> dict[str, Any]:
    length_header = handler.headers.get("Content-Length") or "0"
    try:
        length = int(length_header)
    except ValueError:
        raise ValueError("invalid Content-Length")
    body = handler.rfile.read(length) if length else b"{}"
    try:
        data = json.loads(body.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValueError(f"malformed JSON body: {exc}")
    if not isinstance(data, dict):
        raise ValueError("JSON body must be an object")
    return data


def make_review_handler(state: ReviewServerState) -> type[BaseHTTPRequestHandler]:
    class ReviewHandler(BaseHTTPRequestHandler):
        server_version = "ScherzoExecPlanReview/1"

        def log_message(self, format: str, *args: Any) -> None:  # noqa: A002 - stdlib signature
            return

        def do_GET(self) -> None:
            parsed = urllib.parse.urlparse(self.path)
            if parsed.path in {"/", "/index.html"}:
                if not self._token_ok(parsed):
                    self._json({"error": "forbidden"}, HTTPStatus.FORBIDDEN)
                    return
                self._send_preview()
                return
            if parsed.path == "/api/drafts":
                if not self._token_ok(parsed):
                    self._json({"error": "forbidden"}, HTTPStatus.FORBIDDEN)
                    return
                document = load_drafts_document(state.session)
                self._json(document)
                return
            self._json({"error": "not found"}, HTTPStatus.NOT_FOUND)

        def do_POST(self) -> None:
            parsed = urllib.parse.urlparse(self.path)
            if not self._token_ok(parsed):
                self._json({"error": "forbidden"}, HTTPStatus.FORBIDDEN)
                return
            try:
                data = _load_json_body(self)
            except ValueError as exc:
                self._json({"error": str(exc)}, HTTPStatus.BAD_REQUEST)
                return
            if parsed.path == "/api/drafts":
                self._create_draft(data)
                return
            if parsed.path == "/api/submit":
                self._submit(data)
                return
            if parsed.path == "/api/submit-summary":
                self._submit_summary(data)
                return
            self._json({"error": "not found"}, HTTPStatus.NOT_FOUND)

        def do_PUT(self) -> None:
            parsed = urllib.parse.urlparse(self.path)
            if not self._token_ok(parsed):
                self._json({"error": "forbidden"}, HTTPStatus.FORBIDDEN)
                return
            match = re.fullmatch(r"/api/drafts/([^/]+)", parsed.path)
            if not match:
                self._json({"error": "not found"}, HTTPStatus.NOT_FOUND)
                return
            try:
                data = _load_json_body(self)
                draft = update_draft(state.session, urllib.parse.unquote(match.group(1)), data)
            except ValueError as exc:
                self._json({"error": str(exc)}, HTTPStatus.BAD_REQUEST)
            except KeyError:
                self._json({"error": "draft not found"}, HTTPStatus.NOT_FOUND)
            else:
                self._json({"draft": draft.to_dict()})

        def do_DELETE(self) -> None:
            parsed = urllib.parse.urlparse(self.path)
            if not self._token_ok(parsed):
                self._json({"error": "forbidden"}, HTTPStatus.FORBIDDEN)
                return
            match = re.fullmatch(r"/api/drafts/([^/]+)", parsed.path)
            if not match:
                self._json({"error": "not found"}, HTTPStatus.NOT_FOUND)
                return
            try:
                delete_draft(state.session, urllib.parse.unquote(match.group(1)))
            except KeyError:
                self._json({"error": "draft not found"}, HTTPStatus.NOT_FOUND)
            else:
                self._json({"ok": True})

        def _token_ok(self, parsed: urllib.parse.ParseResult) -> bool:
            header = self.headers.get("X-Scherzo-Review-Token")
            if header and secrets.compare_digest(header, state.token):
                return True
            query = urllib.parse.parse_qs(parsed.query).get("token", [])
            return bool(query and secrets.compare_digest(query[0], state.token))

        def _send_preview(self) -> None:
            try:
                body = state.session.preview_path.read_bytes()
            except OSError:
                self._json({"error": "preview not found"}, HTTPStatus.NOT_FOUND)
                return
            self.send_response(HTTPStatus.OK)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            for key, value in preview_security_headers(state.nonce).items():
                self.send_header(key, value)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _json(self, data: dict[str, Any], status: HTTPStatus = HTTPStatus.OK) -> None:
            body = json.dumps(data, sort_keys=True).encode("utf-8")
            self.send_response(status)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            for key, value in json_security_headers().items():
                self.send_header(key, value)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _create_draft(self, data: dict[str, Any]) -> None:
            try:
                draft = create_draft(state.session, data)
            except ValueError as exc:
                self._json({"error": str(exc)}, HTTPStatus.BAD_REQUEST)
            else:
                self._json({"draft": draft.to_dict()}, HTTPStatus.CREATED)

        def _submit(self, data: dict[str, Any]) -> None:
            drafts = load_drafts(state.session)
            if not any(not draft.submitted_at for draft in drafts):
                self._json({"error": "no unsubmitted draft comments"}, HTTPStatus.BAD_REQUEST)
                return
            pr_file = load_pr_file_record(state.session)
            submission = build_review_submission(state.session, drafts, pr_file)
            save_drafts(state.session, drafts)
            if submission.summary_only and not data.get("confirm_summary_only"):
                self._json(
                    {
                        "error": "no inline-safe diff lines were available",
                        "requires_summary_confirmation": True,
                        "fallback_entries": submission.fallback_entries,
                    },
                    HTTPStatus.CONFLICT,
                )
                return
            self._submit_submission(submission)

        def _submit_summary(self, data: dict[str, Any]) -> None:
            if not data.get("confirm_summary_only"):
                self._json({"error": "summary confirmation is required"}, HTTPStatus.BAD_REQUEST)
                return
            drafts = load_drafts(state.session)
            if not any(not draft.submitted_at for draft in drafts):
                self._json({"error": "no unsubmitted draft comments"}, HTTPStatus.BAD_REQUEST)
                return
            submission = build_summary_issue_submission(state.session, drafts)
            self._submit_submission(submission)

        def _submit_submission(self, submission: ReviewSubmission) -> None:
            try:
                result = submit_review(state.session, submission, state.run_command)
            except ReviewSubmitError as exc:
                response: dict[str, Any] = {"error": str(exc), "details": exc.error}
                if not submission.summary_only:
                    response["requires_summary_confirmation"] = True
                    response["summary_retry_reason"] = "GitHub rejected the inline review request."
                self._json(response, HTTPStatus.BAD_GATEWAY)
                return
            mark_drafts_submitted(state.session)
            self._json(
                {
                    "ok": True,
                    "message": "Submitted review feedback to GitHub.",
                    "summary_only": submission.summary_only,
                    "result": result,
                }
            )

    return ReviewHandler


def start_review_server(
    session: PlanSession,
    *,
    token: str | None = None,
    nonce: str | None = None,
    port: int = 0,
    run_command: CommandRunner = subprocess.run,
) -> tuple[ThreadingHTTPServer, str, str]:
    token = token or secrets.token_urlsafe(32)
    nonce = nonce or secrets.token_urlsafe(24)
    if not session.preview_path.exists():
        prepare_review_preview(session, token=token, nonce=nonce)
    state = ReviewServerState(session=session, token=token, nonce=nonce, run_command=run_command)
    server = ThreadingHTTPServer((SERVER_HOST, port), make_review_handler(state))
    return server, token, nonce


def run_review_server(session: PlanSession, *, port: int, open_browser: bool) -> int:
    server, token, _nonce = start_review_server(session, port=port)
    actual_port = server.server_address[1]
    url = f"http://{SERVER_HOST}:{actual_port}/?token={urllib.parse.quote(token)}"
    if open_browser:
        browser_open(url)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("scherzo-execplan-review: stopping review server", file=sys.stderr)
    finally:
        server.server_close()
    return 0


def print_preview_metadata(
    *,
    session: PlanSession,
    preview_kind: str,
    preview_url: str,
    opened: bool,
    remote_mutations: str,
    review_server_url: str | None = None,
) -> None:
    print(f"PR_NUMBER={session.pr_number}")
    print(f"PR_URL={session.pr_url}")
    print(f"REPO={session.repo}")
    print(f"PLAN_PR_PATH={session.plan_path}")
    print(f"PLAN_HEAD_REPO={session.head_repo}")
    print(f"PLAN_HEAD_SHA={session.head_sha}")
    print(f"PLAN_SOURCE_PATH={session.source_path.resolve()}")
    print(f"PLAN_PREVIEW_KIND={preview_kind}")
    print(f"PLAN_PREVIEW_PATH={session.preview_path.resolve()}")
    if session.review_mode == "interactive-html" and review_server_url:
        print("PLAN_REVIEW_MODE=interactive-html")
        print(f"PLAN_REVIEW_DRAFTS_PATH={session.drafts_path.resolve()}")
        print(f"PLAN_REVIEW_SERVER={review_server_url}")
    print(f"PLAN_PREVIEW_URL={preview_url}")
    print(f"PLAN_REMOTE_MUTATIONS={remote_mutations}")
    print(f"OPENED={'true' if opened else 'false'}")


def preview_pr(args: argparse.Namespace) -> int:
    session, _pr, _files = prepare_plan_session(args, review_mode="preview")
    preview_kind = prepare_preview(session)
    preview_url = session.preview_path.resolve().as_uri()
    opened = False
    if not args.no_open:
        print(
            "scherzo-execplan-review: warning: opening HTML content from a PR locally; "
            "only preview PRs you trust.",
            file=sys.stderr,
        )
        opened = browser_open(preview_url)
        if not opened:
            print(
                "scherzo-execplan-review: warning: could not launch a browser; "
                "open PLAN_PREVIEW_URL manually.",
                file=sys.stderr,
            )

    print_preview_metadata(
        session=session,
        preview_kind=preview_kind,
        preview_url=preview_url,
        opened=opened,
        remote_mutations="none",
    )
    return 0


def review_pr(args: argparse.Namespace) -> int:
    session, _pr, _files = prepare_plan_session(args, review_mode="review")
    if session.review_mode != "interactive-html":
        preview_kind = prepare_preview(session)
        preview_url = session.preview_path.resolve().as_uri()
        opened = False if args.no_open else browser_open(preview_url)
        print_preview_metadata(
            session=session,
            preview_kind=preview_kind,
            preview_url=preview_url,
            opened=opened,
            remote_mutations="none",
        )
        return 0

    token = secrets.token_urlsafe(32)
    nonce = secrets.token_urlsafe(24)
    prepare_review_preview(session, token=token, nonce=nonce)
    server, _token, _nonce = start_review_server(session, token=token, nonce=nonce, port=args.port)
    actual_port = server.server_address[1]
    server_url = f"http://{SERVER_HOST}:{actual_port}/"
    preview_url = f"{server_url}?token={urllib.parse.quote(token)}"
    opened = False
    if not args.no_open:
        opened = browser_open(preview_url)
        if not opened:
            print(
                "scherzo-execplan-review: warning: could not launch a browser; "
                "open PLAN_PREVIEW_URL manually.",
                file=sys.stderr,
            )

    print_preview_metadata(
        session=session,
        preview_kind="interactive-html",
        preview_url=preview_url,
        opened=opened,
        remote_mutations="available-after-browser-submit",
        review_server_url=server_url,
    )
    print("scherzo-execplan-review: review server is running; press Ctrl-C to stop", file=sys.stderr)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("scherzo-execplan-review: stopping review server", file=sys.stderr)
    finally:
        server.server_close()
    return 0


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    if args.mode == "review":
        return review_pr(args)
    return preview_pr(args)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
