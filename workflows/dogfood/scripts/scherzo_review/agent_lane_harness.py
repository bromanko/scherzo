"""Agent-lane harness for .scherzo/workflows/scripts/scherzo-review.

The executable entrypoint lives in ``.scherzo/workflows/scripts/scherzo-review`` so
manual validation commands follow the dogfood workflow bundle boundary. This module contains the helper pieces
used by legacy script-level lanes: input bundle retention, fixture and external
backend execution, response normalization helpers, harness-owned evidence, and
cutover-readiness checks. Production implementation review uses native Scherzo
agent lane steps instead of these backends.
"""

from __future__ import annotations

import datetime as _datetime
import hashlib
import json
import os
import re
import shlex
import subprocess
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
AGENT_BACKENDS = {"heuristic", "fixture", "external"}
EXECUTABLE_CORRECTNESS_EVIDENCE_TYPES = {"test", "runtime", "reproduction"}
MUTATION_CAPABLE_ENV_VARS = {
    "GITHUB_TOKEN",
    "GH_TOKEN",
    "LINEAR_API_KEY",
    "SCHERZO_AGENT_LINEAR_API_KEY",
    "SSH_AUTH_SOCK",
    "GIT_ASKPASS",
    "GIT_SSH_COMMAND",
}
ENV_ALLOWLIST = {
    "PATH",
    "HOME",
    "LANG",
    "LC_ALL",
    "LC_CTYPE",
    "TMPDIR",
    "PYTHONPATH",
}
DEFAULT_EXTERNAL_TIMEOUT_SECONDS = 120
MAX_CONTEXT_SNAPSHOT_BYTES = 40_000


class HarnessError(Exception):
    """Raised when an agent lane cannot complete safely."""


def now_utc() -> str:
    return (
        _datetime.datetime.now(_datetime.timezone.utc)
        .replace(microsecond=0)
        .isoformat()
        .replace("+00:00", "Z")
    )


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def parse_agent_backend(value: str | None) -> str:
    backend = value or "heuristic"
    if backend not in AGENT_BACKENDS:
        raise HarnessError(
            "agent backend must be one of " + ", ".join(sorted(AGENT_BACKENDS))
        )
    return backend


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(65536), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def _is_path_escape(path: Path) -> bool:
    return any(part == ".." for part in path.parts)


def repo_relative_path(path: str | Path, repo_root: Path | None = None) -> str:
    repo_root = (repo_root or Path.cwd()).resolve()
    candidate = Path(path)
    if candidate.is_absolute():
        try:
            return candidate.resolve().relative_to(repo_root).as_posix()
        except ValueError as exc:
            raise HarnessError("path is outside the repository") from exc
    normalized = Path(*candidate.parts)
    if _is_path_escape(normalized):
        raise HarnessError(f"path escapes the repository: {path}")
    return normalized.as_posix()


def validate_retained_artifact_path(output_dir: Path, path: str | Path) -> str:
    candidate = Path(path)
    if candidate.is_absolute():
        raise HarnessError("retained artifact paths must be repository-relative or lane-output-relative")
    if _is_path_escape(candidate):
        raise HarnessError(f"retained artifact path escapes the lane output directory: {path}")
    full_path = output_dir / candidate
    try:
        full_path.resolve().relative_to(output_dir.resolve())
    except ValueError as exc:
        raise HarnessError(f"retained artifact path escapes the lane output directory: {path}") from exc
    return candidate.as_posix()


def retained_artifact_reference(output_dir: Path, path: Path, artifact_type: str) -> dict[str, Any]:
    relative = validate_retained_artifact_path(output_dir, path.relative_to(output_dir))
    return {
        "artifact_type": artifact_type,
        "path": relative,
        "sha256": sha256_file(path),
    }


def _artifact_type_for_path(path: Path) -> str:
    name = path.name
    if name.endswith(".log") or name.endswith(".txt"):
        return "log"
    if name == "prompt.md":
        return "lane_prompt"
    if name == "raw-agent-output.json":
        return "raw_agent_output"
    if name == "evidence-ledger.v1.json":
        return "evidence_ledger"
    if name.startswith("review-lane-") and name.endswith("-analysis.v1.json"):
        return "review_lane_analysis"
    if path.parts and path.parts[0] == "input":
        return "lane_input_bundle"
    if path.parts and path.parts[0] == "repro":
        return "reproduction_artifact"
    return "retained_artifact"


def collect_existing_agent_artifacts(output_dir: Path) -> list[dict[str, Any]]:
    if not output_dir.exists():
        return []
    artifacts: list[dict[str, Any]] = []
    preferred = [
        "input/review-brief.v1.json",
        "input/diff.patch",
        "input/source-metadata.v1.json",
        "input/changed-files.v1.json",
        "input/validation-status.v1.json",
        "input/context-manifest.v1.json",
        "prompt.md",
        "transcript.stdout.txt",
        "transcript.stderr.txt",
        "raw-agent-output.json",
        "evidence-ledger.v1.json",
    ]
    seen: set[str] = set()
    for relative in preferred:
        path = output_dir / relative
        if path.exists() and path.is_file():
            seen.add(relative)
            artifacts.append(retained_artifact_reference(output_dir, path, _artifact_type_for_path(Path(relative))))
    for path in sorted(output_dir.rglob("*")):
        if not path.is_file():
            continue
        relative = path.relative_to(output_dir).as_posix()
        if relative in seen or relative.startswith("review-lane-"):
            continue
        artifacts.append(retained_artifact_reference(output_dir, path, _artifact_type_for_path(Path(relative))))
    return artifacts


def _safe_context_name(repo_path: str) -> str:
    safe = re.sub(r"[^A-Za-z0-9_.-]+", "__", repo_path.strip("/"))
    if not safe:
        safe = "repository-root"
    return safe[:180] + ".txt"


def _file_attr(file: Any, name: str, default: Any = None) -> Any:
    return getattr(file, name, default)


def changed_file_entry(file: Any) -> dict[str, Any]:
    path = str(_file_attr(file, "path", ""))
    return {
        "path": path,
        "previous_path": _file_attr(file, "old_path", None),
        "change_kind": str(_file_attr(file, "change_kind", "modified")),
        "language": _language_for_path(path),
        "subsystem": _subsystem_for_path(path),
        "additions": int(_file_attr(file, "additions", 0) or 0),
        "deletions": int(_file_attr(file, "deletions", 0) or 0),
        "hunks": int(_file_attr(file, "hunks", 0) or 0),
        "hunk_headers": list(_file_attr(file, "hunk_headers", []) or []),
        "added_line_samples": list(_file_attr(file, "added_lines", []) or [])[:20],
        "deleted_line_samples": list(_file_attr(file, "deleted_lines", []) or [])[:20],
    }


def _language_for_path(path: str) -> str:
    lower = path.lower()
    suffixes = {
        ".gleam": "gleam",
        ".erl": "erlang",
        ".hrl": "erlang",
        ".py": "python",
        ".ts": "typescript",
        ".tsx": "typescript",
        ".js": "javascript",
        ".jsx": "javascript",
        ".rs": "rust",
        ".go": "go",
        ".fs": "fsharp",
        ".fsx": "fsharp",
        ".elm": "elm",
        ".md": "markdown",
        ".yaml": "yaml",
        ".yml": "yaml",
        ".json": "json",
        ".toml": "toml",
        ".nix": "nix",
        ".sh": "shell",
    }
    for suffix, language in suffixes.items():
        if lower.endswith(suffix):
            return language
    if path.startswith("scripts/"):
        return "shell-or-script"
    return "unknown"


def _subsystem_for_path(path: str) -> str:
    lower = path.lower()
    if lower.startswith(".scherzo/workflows/schemas/"):
        return "artifact schema contract"
    if lower.startswith("src/"):
        return "scherzo runtime source"
    if lower.startswith("test/"):
        return "tests"
    if lower.startswith("scripts/"):
        return "workflow helper scripts"
    if lower.startswith("docs/"):
        return "documentation"
    if lower.startswith("examples/"):
        return "examples"
    if lower.startswith(".scherzo/workflows/"):
        return "dogfood workflow"
    return "repository root"


def _current_file_snapshot(repo_root: Path, repo_path: str, output_dir: Path) -> dict[str, Any]:
    path = Path(repo_path)
    if path.is_absolute() or _is_path_escape(path):
        return {"path": repo_path, "available": False, "reason": "invalid repository-relative path"}
    source = repo_root / path
    if not source.exists() or not source.is_file():
        return {"path": repo_path, "available": False, "reason": "file is absent in current checkout"}
    try:
        data = source.read_bytes()
    except OSError as exc:
        return {"path": repo_path, "available": False, "reason": f"could not read file: {exc}"}
    truncated = len(data) > MAX_CONTEXT_SNAPSHOT_BYTES
    snippet = data[:MAX_CONTEXT_SNAPSHOT_BYTES]
    try:
        text = snippet.decode("utf-8")
    except UnicodeDecodeError:
        return {"path": repo_path, "available": False, "reason": "file is not UTF-8 text"}
    context_dir = output_dir / "input" / "context"
    context_dir.mkdir(parents=True, exist_ok=True)
    snapshot_path = context_dir / _safe_context_name(repo_path)
    snapshot_path.write_text(text)
    return {
        "path": repo_path,
        "available": True,
        "snapshot_path": validate_retained_artifact_path(output_dir, snapshot_path.relative_to(output_dir)),
        "sha256": sha256_file(snapshot_path),
        "truncated": truncated,
        "byte_count": len(data),
    }


def prompt_file_for_lane(lane_id: str) -> Path | None:
    filename = lane_id.replace("-", "_") + ".md"
    path = Path(__file__).resolve().parent / "prompts" / filename
    if path.exists():
        return path
    return None


def build_prompt(lane_id: str, backend: str, scenario_id: str | None) -> str:
    specific_path = prompt_file_for_lane(lane_id)
    specific = specific_path.read_text() if specific_path else ""
    return (
        f"# Scherzo staged review lane: {lane_id}\n\n"
        f"Backend: {backend}\n"
        f"Scenario id: {scenario_id or 'none'}\n\n"
        "Inspect the actual unified diff in `input/diff.patch`. Treat the "
        "ReviewBrief as orientation only; it is not a substitute for direct diff "
        "inspection. Use `input/changed-files.v1.json`, `input/context-manifest.v1.json`, "
        "and available context snapshots before making claims. Cite only "
        "repository-relative locations from the diff or retained context.\n\n"
        "Do not mutate remote state, post PR comments, update Linear, push branches, "
        "exfiltrate credentials, or edit the working tree. Emit JSON only. Keep "
        "findings distinct from non-blocking review notes. Blocking correctness "
        "findings require a harness-issued executable evidence id.\n\n"
        + specific
    )


def build_lane_input_bundle(
    lane_id: str,
    brief_path: Path,
    brief: dict[str, Any],
    diff: str,
    source: dict[str, Any],
    files: list[Any],
    output_dir: Path,
    prompt_path: Path,
    backend: str,
    scenario_id: str | None = None,
) -> dict[str, Any]:
    output_dir.mkdir(parents=True, exist_ok=True)
    input_dir = output_dir / "input"
    input_dir.mkdir(parents=True, exist_ok=True)
    repo_root = Path.cwd()

    changed_files = [changed_file_entry(file) for file in files]
    context_entries = [
        _current_file_snapshot(repo_root, entry["path"], output_dir)
        for entry in changed_files
    ]

    review_brief_path = input_dir / "review-brief.v1.json"
    diff_path = input_dir / "diff.patch"
    source_metadata_path = input_dir / "source-metadata.v1.json"
    changed_files_path = input_dir / "changed-files.v1.json"
    validation_status_path = input_dir / "validation-status.v1.json"
    context_manifest_path = input_dir / "context-manifest.v1.json"

    write_json(review_brief_path, brief)
    diff_path.write_text(diff)
    write_json(source_metadata_path, {
        **source,
        "diff_sha256": sha256_text(diff),
        "changed_file_count": len(files),
    })
    write_json(changed_files_path, {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "changed_files",
        "files": changed_files,
    })
    write_json(validation_status_path, {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "validation_status",
        "test_build_status": brief.get("test_build_status", []),
    })
    write_json(context_manifest_path, {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "context_manifest",
        "context_policy": {
            "max_snapshot_bytes": MAX_CONTEXT_SNAPSHOT_BYTES,
            "absolute_paths_serialized": False,
        },
        "files": context_entries,
    })
    prompt_path.write_text(build_prompt(lane_id, backend, scenario_id))

    bundle_artifacts: list[dict[str, Any]] = []
    for path in [
        review_brief_path,
        diff_path,
        source_metadata_path,
        changed_files_path,
        validation_status_path,
        context_manifest_path,
        prompt_path,
    ]:
        bundle_artifacts.append(retained_artifact_reference(output_dir, path, _artifact_type_for_path(path.relative_to(output_dir))))
    for context_path in sorted((input_dir / "context").glob("*")) if (input_dir / "context").exists() else []:
        if context_path.is_file():
            bundle_artifacts.append(retained_artifact_reference(output_dir, context_path, "context_snapshot"))

    return {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "lane_input_bundle",
        "lane_id": lane_id,
        "backend": backend,
        "scenario_id": scenario_id,
        "bundle_dir": "input",
        "prompt_path": validate_retained_artifact_path(output_dir, prompt_path.relative_to(output_dir)),
        "review_brief_path": validate_retained_artifact_path(output_dir, review_brief_path.relative_to(output_dir)),
        "diff_path": validate_retained_artifact_path(output_dir, diff_path.relative_to(output_dir)),
        "source": {**source, "diff_sha256": sha256_text(diff), "changed_file_count": len(files)},
        "changed_files": changed_files,
        "validation_status": brief.get("test_build_status", []),
        "artifacts": bundle_artifacts,
    }


def _first_location(bundle: dict[str, Any], preferred: str | None = None, concern: str | None = None) -> list[dict[str, Any]]:
    files = bundle.get("changed_files", [])
    if preferred:
        return [{"path": preferred}]
    if not isinstance(files, list) or not files:
        return [{"path": "<unknown>"}]
    if concern == "security-performance":
        for file in files:
            if isinstance(file, dict):
                path = str(file.get("path", ""))
                if path and not path.startswith("docs/"):
                    return [{"path": path}]
    for file in files:
        if isinstance(file, dict) and file.get("path"):
            return [{"path": str(file["path"])}]
    return [{"path": "<unknown>"}]


def _source_files(bundle: dict[str, Any]) -> list[dict[str, Any]]:
    files = bundle.get("changed_files", [])
    if not isinstance(files, list):
        return []
    return [file for file in files if isinstance(file, dict) and str(file.get("path", "")).startswith(("src/", "scripts/"))]


def _test_files(bundle: dict[str, Any]) -> list[dict[str, Any]]:
    files = bundle.get("changed_files", [])
    if not isinstance(files, list):
        return []
    return [file for file in files if isinstance(file, dict) and str(file.get("path", "")).startswith("test/")]


def _added_samples(bundle: dict[str, Any]) -> str:
    samples: list[str] = []
    for file in bundle.get("changed_files", []):
        if not isinstance(file, dict):
            continue
        for line in file.get("added_line_samples", []) or []:
            if isinstance(line, str):
                samples.append(line)
    return "\n".join(samples)


def _generic_fixture_response(lane_id: str, bundle: dict[str, Any]) -> dict[str, Any]:
    findings: list[dict[str, Any]] = []
    notes: list[dict[str, Any]] = []
    added = _added_samples(bundle)
    source_files = _source_files(bundle)
    test_files = _test_files(bundle)

    if lane_id == "test-quality":
        if source_files and not test_files:
            findings.append({
                "category": "testing",
                "severity": "medium",
                "evidence_type": "static",
                "verified": True,
                "blocking": False,
                "locations": _first_location(bundle),
                "summary": "Changed behavior has no focused test coverage in the diff.",
                "details": "The fixture backend saw implementation changes but no changed test file exercising the behavior.",
                "suggested_fix": "Add an assertion-bearing regression test for the changed branch or document the existing coverage.",
            })
        elif test_files:
            notes.append({
                "kind": "coverage_note",
                "category": "testing",
                "severity": "info",
                "locations": _first_location(bundle),
                "summary": "Fixture backend retained changed-test coverage context.",
                "details": "The diff includes test changes; this note records that the agent harness inspected them without treating them as a blocker.",
                "suggested_action": "Confirm the assertions exercise the behavior changed by the implementation.",
            })
    elif lane_id == "idioms-maintainability":
        if any(token in added for token in ["let assert", "panic(", "todo"]):
            findings.append({
                "category": "maintainability",
                "severity": "high",
                "evidence_type": "static",
                "verified": True,
                "blocking": True,
                "locations": _first_location(bundle),
                "summary": "Production diff adds a construct forbidden by the Scherzo lint policy.",
                "details": "The fixture backend found `let assert`, `panic`, or `todo` in added production context.",
                "suggested_fix": "Replace the construct with explicit error handling or add a narrow lint suppression that documents the invariant.",
            })
        else:
            notes.append({
                "kind": "review_note",
                "category": "maintainability",
                "severity": "info",
                "locations": _first_location(bundle),
                "summary": "Fixture maintainability pass completed without must-fix findings.",
                "details": "The agent harness retained the prompt and input bundle for maintainability review.",
                "suggested_action": "Review the retained prompt and bundle if a human needs to audit the lane output.",
            })
    elif lane_id == "security-performance":
        lower = added.lower()
        if "supersecret" in lower or "password" in lower or "api_key" in lower:
            findings.append({
                "category": "security",
                "severity": "high",
                "evidence_type": "static",
                "verified": True,
                "blocking": True,
                "locations": _first_location(bundle, concern="security-performance"),
                "summary": "Concrete hard-coded credential appears in the diff.",
                "details": "The fixture backend found a credential-like literal in changed runtime code.",
                "suggested_fix": "Remove the secret from source and read it from the existing secret-management path.",
            })
        elif "shell=true" in lower or "os.system" in lower:
            findings.append({
                "category": "security",
                "severity": "high",
                "evidence_type": "static",
                "verified": True,
                "blocking": True,
                "locations": _first_location(bundle, concern="security-performance"),
                "summary": "Concrete shell execution hazard appears in the diff.",
                "details": "The fixture backend found shell execution on changed code.",
                "suggested_fix": "Use argv-style subprocess execution with validated inputs.",
            })
        else:
            notes.append({
                "kind": "risk_note",
                "category": "security",
                "severity": "info",
                "locations": _first_location(bundle, concern="security-performance"),
                "summary": "Fixture security/performance lane retained boundary context.",
                "details": "The agent harness inspected the diff and kept broad boundary sensitivity as a note rather than a blocker.",
                "suggested_action": "Check authorization, credentials, command execution, sleeps, and unbounded growth if they are relevant to the changed path.",
            })
    else:
        notes.append({
            "kind": "review_note",
            "category": "correctness",
            "severity": "info",
            "locations": _first_location(bundle),
            "summary": "Fixture correctness lane completed without executable bug evidence.",
            "details": "The fixture backend exercised the agent harness path and did not find a harness-owned correctness blocker.",
            "suggested_action": "If the behavior is risky, add a trusted reproduction or test evidence request.",
        })

    return {
        "lane_id": lane_id,
        "analysis_summary": "Deterministic fixture agent response generated by the shared harness.",
        "findings": findings,
        "review_notes": notes,
        "evidence_requests": [],
        "evidence_references": [],
    }


def _write_inverted_auth_repro(output_dir: Path) -> None:
    repro_dir = output_dir / "repro"
    repro_dir.mkdir(parents=True, exist_ok=True)
    script = repro_dir / "inverted_auth_control_condition_repro.py"
    script.write_text(
        "#!/usr/bin/env python3\n"
        "from pathlib import Path\n"
        "import sys\n"
        "diff = Path('input/diff.patch').read_text()\n"
        "normalized = ''.join(diff.split())\n"
        "if '+User->Ok(\"deleted\")' in normalized:\n"
        "    print('REPRODUCED: unauthorized User received Ok(\"deleted\")')\n"
        "    sys.exit(0)\n"
        "print('NOT_REPRODUCED: unauthorized User was rejected')\n"
        "sys.exit(1)\n"
    )


def run_fixture_agent(
    bundle: dict[str, Any],
    scenario_id: str | None,
    output_dir: Path,
    raw_output_path: Path,
) -> dict[str, Any]:
    lane_id = str(bundle.get("lane_id", ""))
    if scenario_id == "malformed-agent-output" and lane_id == "correctness":
        raw_output_path.write_text("{ this is not valid JSON\n")
        raise HarnessError("fixture backend returned malformed JSON")

    if scenario_id == "inverted-auth-control-condition" and lane_id == "correctness":
        _write_inverted_auth_repro(output_dir)
        response = {
            "lane_id": lane_id,
            "analysis_summary": "Detected an authorization control condition inversion and requested the trusted reproduction.",
            "findings": [
                {
                    "category": "correctness",
                    "severity": "high",
                    "evidence_type": "reproduction",
                    "verified": True,
                    "blocking": True,
                    "locations": [{"path": "src/liv_152_fixture/project_authorization.gleam"}],
                    "summary": "Unauthorized users can delete a project after the control branch inversion.",
                    "details": "The changed `User` branch now returns `Ok(\"deleted\")`, allowing a non-admin role to perform the delete action.",
                    "suggested_fix": "Restore the unauthorized branch to return `Error(\"forbidden\")` and keep a regression test for the User role.",
                    "evidence_key": "inverted_auth_repro",
                }
            ],
            "review_notes": [],
            "evidence_requests": [
                {
                    "evidence_key": "inverted_auth_repro",
                    "evidence_type": "reproduction",
                    "command": "python3 repro/inverted_auth_control_condition_repro.py",
                    "cwd": ".",
                    "expected_exit_code": 0,
                    "description": "Reproduce that unauthorized User receives Ok(\"deleted\").",
                    "timeout_seconds": 20,
                    "expected_stdout_contains": "REPRODUCED: unauthorized User received Ok(\"deleted\")",
                    "trusted": True,
                }
            ],
            "evidence_references": ["inverted_auth_repro"],
        }
    elif scenario_id == "auth-control-static-suspicion-without-repro" and lane_id == "correctness":
        response = {
            "lane_id": lane_id,
            "analysis_summary": "Static control-flow suspicion emitted without trusted executable evidence.",
            "findings": [
                {
                    "category": "correctness",
                    "severity": "high",
                    "evidence_type": "static",
                    "verified": False,
                    "blocking": True,
                    "locations": [{"path": "src/liv_152_fixture/workflow_gate.gleam"}],
                    "summary": "Workflow gate condition may have been inverted.",
                    "details": "The changed guard looks authorization-sensitive, but the fixture intentionally provides no trusted reproduction.",
                    "suggested_fix": "Add a focused executable check for the allowed and denied gate cases before treating this as a blocker.",
                    "evidence_key": "missing_workflow_gate_repro",
                }
            ],
            "review_notes": [],
            "evidence_requests": [],
            "evidence_references": ["missing_workflow_gate_repro"],
        }
    else:
        response = _generic_fixture_response(lane_id, bundle)

    write_json(raw_output_path, response)
    return response


def sanitize_agent_environment(env: dict[str, str]) -> dict[str, str]:
    sanitized: dict[str, str] = {}
    for key, value in env.items():
        if key in MUTATION_CAPABLE_ENV_VARS:
            continue
        if key in ENV_ALLOWLIST or key.startswith("SCHERZO_REVIEW_AGENT_READONLY_"):
            sanitized[key] = value
    return sanitized


def capture_repo_state(repo_root: Path) -> dict[str, Any]:
    commands = [["jj", "status", "--color=never"], ["git", "status", "--short"]]
    for command in commands:
        try:
            proc = subprocess.run(
                command,
                cwd=repo_root,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=20,
            )
        except (OSError, subprocess.TimeoutExpired):
            continue
        if proc.returncode == 0:
            output = proc.stdout.strip()
            return {
                "command": " ".join(command),
                "state": "clean" if not output or output.startswith("The working copy has no changes.") else "dirty",
                "status_sha256": sha256_text(output),
                "summary": output.splitlines()[0] if output else "clean",
            }
    return {
        "command": "unavailable",
        "state": "unknown",
        "status_sha256": sha256_text("unknown"),
        "summary": "no jj or git status command was available",
    }


def _parse_timeout(value: str | None) -> int:
    if not value:
        return DEFAULT_EXTERNAL_TIMEOUT_SECONDS
    try:
        parsed = int(value)
    except ValueError:
        return DEFAULT_EXTERNAL_TIMEOUT_SECONDS
    if parsed <= 0:
        return DEFAULT_EXTERNAL_TIMEOUT_SECONDS
    return parsed


def _expand_command_template(
    command_template: str,
    lane_id: str,
    output_dir: Path,
    raw_output_path: Path,
) -> str:
    replacements = {
        "lane_id": lane_id,
        "prompt_path": repo_relative_path(output_dir / "prompt.md"),
        "bundle_dir": repo_relative_path(output_dir / "input"),
        "output_dir": repo_relative_path(output_dir),
        "raw_output_path": repo_relative_path(raw_output_path),
    }
    try:
        return command_template.format(**replacements)
    except KeyError as exc:
        raise HarnessError(f"unknown placeholder in SCHERZO_REVIEW_AGENT_COMMAND: {exc}") from exc


def run_external_agent(
    bundle: dict[str, Any],
    command_template: str | None,
    timeout_seconds: int | None,
    output_dir: Path,
    raw_output_path: Path,
) -> dict[str, Any]:
    lane_id = str(bundle.get("lane_id", ""))
    if not command_template:
        raise HarnessError("missing external backend configuration: SCHERZO_REVIEW_AGENT_COMMAND is required")

    repo_root = Path.cwd()
    before = capture_repo_state(repo_root)
    timeout = timeout_seconds or DEFAULT_EXTERNAL_TIMEOUT_SECONDS
    command = _expand_command_template(command_template, lane_id, output_dir, raw_output_path)
    stdout_path = output_dir / "transcript.stdout.txt"
    stderr_path = output_dir / "transcript.stderr.txt"
    stdout_path.write_text("")
    stderr_path.write_text("")
    try:
        proc = subprocess.run(
            shlex.split(command),
            cwd=repo_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            env=sanitize_agent_environment(dict(os.environ)),
        )
    except subprocess.TimeoutExpired as exc:
        stdout_path.write_text(exc.stdout or "")
        stderr_path.write_text(exc.stderr or "")
        raise HarnessError(f"external agent timed out after {timeout}s") from exc
    stdout_path.write_text(proc.stdout)
    stderr_path.write_text(proc.stderr)
    after = capture_repo_state(repo_root)
    if before.get("status_sha256") != after.get("status_sha256"):
        raise HarnessError("external agent changed the working tree")
    if proc.returncode != 0:
        raise HarnessError(f"external agent exited nonzero: {proc.returncode}")
    if not raw_output_path.exists():
        raise HarnessError("external agent did not write raw output JSON")
    try:
        raw = json.loads(raw_output_path.read_text())
    except json.JSONDecodeError as exc:
        raise HarnessError(f"external agent returned malformed JSON: {exc}") from exc
    if not isinstance(raw, dict):
        raise HarnessError("external agent raw output must be a JSON object")
    raw.setdefault("external_execution", {})
    raw["external_execution"].update({
        "command_template_used": True,
        "timeout_seconds": timeout,
        "working_tree_before": before,
        "working_tree_after": after,
        "stdout_artifact": "transcript.stdout.txt",
        "stderr_artifact": "transcript.stderr.txt",
    })
    return raw


def _normalize_locations(locations: Any, bundle: dict[str, Any]) -> list[dict[str, Any]]:
    if not isinstance(locations, list) or not locations:
        return _first_location(bundle)
    normalized: list[dict[str, Any]] = []
    for location in locations:
        if not isinstance(location, dict):
            continue
        path = location.get("path")
        if not isinstance(path, str) or not path:
            continue
        if Path(path).is_absolute() or _is_path_escape(Path(path)):
            raise HarnessError(f"agent output used an invalid location path: {path}")
        normalized_location = dict(location)
        normalized_location["path"] = Path(path).as_posix()
        normalized.append(normalized_location)
    return normalized or _first_location(bundle)


def _normalize_finding(raw: dict[str, Any], lane_id: str, index: int, bundle: dict[str, Any]) -> dict[str, Any]:
    category = str(raw.get("category") or ("security" if lane_id == "security-performance" else "other"))
    finding = {
        "id": str(raw.get("id") or f"{lane_id}-{index:03d}"),
        "category": category,
        "severity": str(raw.get("severity") or "medium"),
        "evidence_type": str(raw.get("evidence_type") or "static"),
        "verified": bool(raw.get("verified")),
        "blocking": bool(raw.get("blocking")),
        "locations": _normalize_locations(raw.get("locations"), bundle),
        "summary": str(raw.get("summary") or "Agent-backed lane proposed a finding."),
        "details": str(raw.get("details") or "No additional details were provided by the backend."),
        "suggested_fix": str(raw.get("suggested_fix") or raw.get("suggested_action") or "Inspect the retained agent output and address the issue."),
    }
    for key in ["evidence_key", "evidence_id", "evidence_ids", "finding_type", "review_priority"]:
        if key in raw:
            finding[key] = raw[key]
    return finding


def _normalize_note(raw: dict[str, Any], lane_id: str, index: int, bundle: dict[str, Any]) -> dict[str, Any]:
    default_category = "testing" if lane_id == "test-quality" else "maintainability" if lane_id == "idioms-maintainability" else "security" if lane_id == "security-performance" else "correctness"
    return {
        "id": str(raw.get("id") or f"{lane_id}-note-{index:03d}"),
        "kind": str(raw.get("kind") or "review_note"),
        "category": str(raw.get("category") or default_category),
        "severity": str(raw.get("severity") or "info"),
        "locations": _normalize_locations(raw.get("locations"), bundle),
        "summary": str(raw.get("summary") or "Agent-backed lane retained a non-blocking note."),
        "details": str(raw.get("details") or "The backend did not provide additional details."),
        "suggested_action": str(raw.get("suggested_action") or raw.get("suggested_fix") or "Inspect the retained prompt and raw output if follow-up is needed."),
    }


def run_evidence_command(evidence_request: dict[str, Any], lane_output_dir: Path, timeout_seconds: int | None = None) -> dict[str, Any]:
    key = str(evidence_request.get("evidence_key") or "evidence")
    scenario_id = str(evidence_request.get("scenario_id") or "scenario")
    evidence_type = str(evidence_request.get("evidence_type") or "unknown")
    command = str(evidence_request.get("command") or "")
    if not command:
        raise HarnessError(f"evidence request {key} did not include a command")
    timeout = int(evidence_request.get("timeout_seconds") or timeout_seconds or 30)
    expected_exit_code = int(evidence_request.get("expected_exit_code", 0))
    repro_dir = lane_output_dir / "repro"
    repro_dir.mkdir(parents=True, exist_ok=True)
    safe_key = re.sub(r"[^A-Za-z0-9_.-]+", "_", key)
    command_log_path = repro_dir / f"{safe_key}.command.log"
    stdout_path = repro_dir / f"{safe_key}.stdout.txt"
    stderr_path = repro_dir / f"{safe_key}.stderr.txt"
    before = capture_repo_state(Path.cwd())
    timed_out = False
    exit_code: int | None = None
    stdout = ""
    stderr = ""
    try:
        proc = subprocess.run(
            shlex.split(command),
            cwd=lane_output_dir,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            env=sanitize_agent_environment(dict(os.environ)),
        )
        exit_code = proc.returncode
        stdout = proc.stdout
        stderr = proc.stderr
    except subprocess.TimeoutExpired as exc:
        timed_out = True
        stdout = exc.stdout or ""
        stderr = exc.stderr or ""
    after = capture_repo_state(Path.cwd())
    stdout_path.write_text(stdout)
    stderr_path.write_text(stderr)
    command_log_path.write_text(
        f"command={command}\n"
        f"cwd={lane_output_dir}\n"
        f"expected_exit_code={expected_exit_code}\n"
        f"exit_code={exit_code}\n"
        f"timed_out={str(timed_out).lower()}\n"
    )
    unchanged = before.get("status_sha256") == after.get("status_sha256")
    expected_stdout = evidence_request.get("expected_stdout_contains")
    entry = {
        "evidence_id": f"{scenario_id}/{key}",
        "evidence_key": key,
        "evidence_type": evidence_type,
        "description": str(evidence_request.get("description") or "Harness-owned evidence command."),
        "command": command,
        "cwd": lane_output_dir.as_posix(),
        "expected_exit_code": expected_exit_code,
        "exit_code": exit_code,
        "timed_out": timed_out,
        "stdout_artifact": validate_retained_artifact_path(lane_output_dir, stdout_path.relative_to(lane_output_dir)),
        "stderr_artifact": validate_retained_artifact_path(lane_output_dir, stderr_path.relative_to(lane_output_dir)),
        "command_log_artifact": validate_retained_artifact_path(lane_output_dir, command_log_path.relative_to(lane_output_dir)),
        "stdout_sha256": sha256_file(stdout_path),
        "stderr_sha256": sha256_file(stderr_path),
        "command_log_sha256": sha256_file(command_log_path),
        "stdout_contains": expected_stdout,
        "working_tree_before": "clean" if unchanged else before.get("state", "unknown"),
        "working_tree_after": "clean" if unchanged else after.get("state", "unknown"),
        "working_tree_changed": not unchanged,
        "valid": (
            not timed_out
            and exit_code == expected_exit_code
            and not (expected_stdout and expected_stdout not in stdout)
            and unchanged
            and evidence_type in EXECUTABLE_CORRECTNESS_EVIDENCE_TYPES
        ),
    }
    return entry


def write_evidence_ledger(output_dir: Path, entries: list[dict[str, Any]]) -> str:
    path = output_dir / "evidence-ledger.v1.json"
    write_json(path, {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "evidence_ledger",
        "generated_at_utc": now_utc(),
        "evidence": entries,
    })
    return path.name


def load_evidence_ledger(output_dir: Path) -> dict[str, Any]:
    path = output_dir / "evidence-ledger.v1.json"
    if not path.exists():
        return {"evidence": []}
    value = json.loads(path.read_text())
    return value if isinstance(value, dict) else {"evidence": []}


def _ledger_by_key_and_id(evidence_ledger: dict[str, Any]) -> dict[str, dict[str, Any]]:
    indexed: dict[str, dict[str, Any]] = {}
    entries = evidence_ledger.get("evidence", [])
    if not isinstance(entries, list):
        return indexed
    for entry in entries:
        if not isinstance(entry, dict):
            continue
        for field in ["evidence_key", "evidence_id"]:
            value = entry.get(field)
            if isinstance(value, str) and value:
                indexed[value] = entry
    return indexed


def enforce_correctness_evidence_policy(lane_result: dict[str, Any], evidence_ledger: dict[str, Any], output_dir: Path) -> dict[str, Any]:
    if lane_result.get("lane", {}).get("id") != "correctness":
        return lane_result
    indexed = _ledger_by_key_and_id(evidence_ledger)
    findings = lane_result.get("findings", [])
    if not isinstance(findings, list):
        return lane_result
    kept_findings: list[dict[str, Any]] = []
    notes = lane_result.setdefault("review_notes", [])
    actions = lane_result.setdefault("harness_actions", [])
    for finding in findings:
        if not isinstance(finding, dict):
            continue
        if finding.get("category") == "correctness" and finding.get("blocking"):
            keys: list[str] = []
            for field in ["evidence_id", "evidence_key"]:
                value = finding.get(field)
                if isinstance(value, str) and value:
                    keys.append(value)
            ids = finding.get("evidence_ids")
            if isinstance(ids, list):
                keys.extend([str(value) for value in ids if isinstance(value, str)])
            valid_entry = None
            for key in keys:
                entry = indexed.get(key)
                if entry and entry.get("valid") is True:
                    valid_entry = entry
                    break
            if valid_entry is not None:
                evidence_id = str(valid_entry.get("evidence_id"))
                finding["evidence_id"] = evidence_id
                finding["evidence_ids"] = [evidence_id]
                finding["evidence_type"] = str(valid_entry.get("evidence_type"))
                finding["verified"] = True
                finding["blocking"] = True
                kept_findings.append(finding)
            else:
                note = {
                    "id": f"correctness-note-{len(notes) + 1:03d}",
                    "kind": "risk_note",
                    "category": "correctness",
                    "severity": "medium",
                    "locations": finding.get("locations") or [{"path": "<unknown>"}],
                    "summary": str(finding.get("summary") or "Unverified correctness concern needs executable evidence."),
                    "details": (
                        str(finding.get("details") or "")
                        + "\n\nDowngraded by the agent harness because blocking correctness findings require a valid harness-issued test, runtime, or reproduction evidence id."
                    ),
                    "suggested_action": "Add or allow a deterministic executable reproduction before treating this correctness concern as blocking.",
                }
                notes.append(note)
                actions.append({
                    "action": "downgraded_unverified_correctness_claim",
                    "finding_id": finding.get("id"),
                    "reason": "no valid harness-owned executable evidence entry was present",
                })
        else:
            kept_findings.append(finding)
    lane_result["findings"] = kept_findings
    return lane_result


def enforce_lane_contract(lane_id: str, lane_result: dict[str, Any], evidence_ledger: dict[str, Any], output_dir: Path) -> dict[str, Any]:
    artifacts = lane_result.get("artifacts", [])
    if not isinstance(artifacts, list):
        raise HarnessError("lane result artifacts must be a list")
    for artifact in artifacts:
        if not isinstance(artifact, dict):
            raise HarnessError("lane result artifact references must be objects")
        path = artifact.get("path")
        if not isinstance(path, str) or not path:
            raise HarnessError("lane result artifact references need a path")
        validate_retained_artifact_path(output_dir, path)
    return lane_result


def _analysis_from_lane_result(lane_id: str, raw_output: dict[str, Any], lane_result: dict[str, Any], bundle: dict[str, Any]) -> dict[str, Any]:
    findings = lane_result.get("findings", []) if isinstance(lane_result.get("findings"), list) else []
    notes = lane_result.get("review_notes", []) if isinstance(lane_result.get("review_notes"), list) else []
    return {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "review_lane_analysis",
        "lane_id": lane_id,
        "review_depth": "agent-fixture" if lane_result.get("agent_backend") == "fixture" else "agent-external",
        "agent_backend": lane_result.get("agent_backend"),
        "scenario_id": bundle.get("scenario_id"),
        "changed_file_count": len(bundle.get("changed_files", []) or []),
        "changed_files": [file.get("path") for file in bundle.get("changed_files", []) if isinstance(file, dict)],
        "checks": [
            "built lane input bundle",
            "retained prompt and raw backend output",
            "normalized agent response into ReviewLaneResult",
            "enforced harness-owned correctness evidence policy",
        ],
        "analysis_summary": str(raw_output.get("analysis_summary") or "Agent lane completed."),
        "finding_count": len(findings),
        "review_note_count": len(notes),
        "blocking_finding_count": len([finding for finding in findings if isinstance(finding, dict) and finding.get("blocking")]),
        "empty_findings_reason": "Agent backend produced no findings." if not findings else None,
        "harness_actions": lane_result.get("harness_actions", []),
    }


def normalize_agent_response(
    lane_id: str,
    brief_path: Path,
    brief: dict[str, Any],
    source: dict[str, Any],
    diff: str,
    raw_output: dict[str, Any],
    bundle: dict[str, Any],
    output_dir: Path,
    *,
    lane_metadata: dict[str, str],
    started_at: str,
    completed_at: str,
    schema_ref: str,
    backend: str,
    evidence_ledger: dict[str, Any],
) -> tuple[dict[str, Any], dict[str, Any]]:
    if raw_output.get("lane_id") not in {None, lane_id}:
        raise HarnessError(f"agent output lane_id did not match requested lane: {raw_output.get('lane_id')}")
    raw_findings = raw_output.get("findings", [])
    raw_notes = raw_output.get("review_notes", [])
    if not isinstance(raw_findings, list):
        raise HarnessError("agent output findings must be a list")
    if not isinstance(raw_notes, list):
        raise HarnessError("agent output review_notes must be a list")
    findings = [
        _normalize_finding(raw, lane_id, index + 1, bundle)
        for index, raw in enumerate(raw_findings)
        if isinstance(raw, dict)
    ]
    notes = [
        _normalize_note(raw, lane_id, index + 1, bundle)
        for index, raw in enumerate(raw_notes)
        if isinstance(raw, dict)
    ]

    raw_output_path = output_dir / "raw-agent-output.json"
    log_path = output_dir / f"review-lane-{lane_id}.log"
    analysis_path = output_dir / f"review-lane-{lane_id}-analysis.v1.json"
    ledger_path = output_dir / "evidence-ledger.v1.json"
    artifacts = collect_existing_agent_artifacts(output_dir)
    lane_result = {
        "$schema": schema_ref,
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "review_lane_result",
        "generated_at_utc": completed_at,
        "producer": {"name": ".scherzo/workflows/scripts/scherzo-review", "version": "1", "mode": "run-lane"},
        "lane": lane_metadata,
        "execution_status": {
            "state": "succeeded",
            "started_at_utc": started_at,
            "completed_at_utc": completed_at,
            "summary": f"{lane_metadata['name']} completed with {len(findings)} proposed finding(s) using {backend} backend.",
        },
        "agent_backend": backend,
        "input_brief_ref": {"path": str(brief_path), "sha256": sha256_file(brief_path)},
        "input_bundle_ref": {"path": "input", "artifacts": bundle.get("artifacts", [])},
        "findings": findings,
        "review_notes": notes,
        "artifacts": artifacts,
        "source": {**source, "diff_sha256": sha256_text(diff), "changed_file_count": len(bundle.get("changed_files", []) or [])},
        "review_depth": "agent-fixture" if backend == "fixture" else "agent-external",
        "raw_agent_output_ref": retained_artifact_reference(output_dir, raw_output_path, "raw_agent_output") if raw_output_path.exists() else None,
        "evidence_ledger_ref": retained_artifact_reference(output_dir, ledger_path, "evidence_ledger") if ledger_path.exists() else None,
        "harness_actions": [],
    }
    enforce_correctness_evidence_policy(lane_result, evidence_ledger, output_dir)
    analysis = _analysis_from_lane_result(lane_id, raw_output, lane_result, bundle)
    write_json(analysis_path, analysis)
    log_path.write_text(
        "scherzo-review run-lane\n"
        f"lane={lane_id}\n"
        f"agent_backend={backend}\n"
        f"scenario_id={bundle.get('scenario_id') or ''}\n"
        f"started_at_utc={started_at}\n"
        f"completed_at_utc={completed_at}\n"
        f"input_bundle=input\n"
        f"raw_output=raw-agent-output.json\n"
        f"evidence_ledger=evidence-ledger.v1.json\n"
        f"finding_count={analysis.get('finding_count')}\n"
        f"review_note_count={analysis.get('review_note_count')}\n"
        f"blocking_finding_count={analysis.get('blocking_finding_count')}\n"
    )
    lane_result["artifacts"] = collect_existing_agent_artifacts(output_dir)
    lane_result["artifacts"].append(retained_artifact_reference(output_dir, analysis_path, "review_lane_analysis"))
    lane_result["artifacts"].append(retained_artifact_reference(output_dir, log_path, "log"))
    enforce_lane_contract(lane_id, lane_result, evidence_ledger, output_dir)
    return lane_result, analysis


def run_agent_lane(
    lane_id: str,
    lane_metadata: dict[str, str],
    brief_path: Path,
    brief: dict[str, Any],
    diff: str,
    source: dict[str, Any],
    files: list[Any],
    output_dir: Path,
    backend: str,
    scenario_id: str | None,
    schema_ref: str,
) -> tuple[dict[str, Any], dict[str, Any], Path, Path, Path]:
    started_at = now_utc()
    output_dir.mkdir(parents=True, exist_ok=True)
    prompt_path = output_dir / "prompt.md"
    raw_output_path = output_dir / "raw-agent-output.json"
    bundle = build_lane_input_bundle(
        lane_id,
        brief_path,
        brief,
        diff,
        source,
        files,
        output_dir,
        prompt_path,
        backend,
        scenario_id,
    )

    if backend == "fixture":
        raw_output = run_fixture_agent(bundle, scenario_id, output_dir, raw_output_path)
    elif backend == "external":
        raw_output = run_external_agent(
            bundle,
            os.environ.get("SCHERZO_REVIEW_AGENT_COMMAND"),
            _parse_timeout(os.environ.get("SCHERZO_REVIEW_AGENT_TIMEOUT_SECONDS")),
            output_dir,
            raw_output_path,
        )
    else:
        raise HarnessError(f"unsupported agent backend for harness path: {backend}")

    # Re-read retained raw output so fixture and external backends exercise the same JSON path.
    try:
        raw_output = json.loads(raw_output_path.read_text())
    except json.JSONDecodeError as exc:
        raise HarnessError(f"agent backend returned malformed JSON: {exc}") from exc
    if not isinstance(raw_output, dict):
        raise HarnessError("agent backend raw output must be a JSON object")

    evidence_entries: list[dict[str, Any]] = []
    evidence_requests = raw_output.get("evidence_requests", [])
    if isinstance(evidence_requests, list):
        for request in evidence_requests:
            if not isinstance(request, dict):
                continue
            if request.get("trusted") is True or backend == "fixture":
                request = dict(request)
                request.setdefault("scenario_id", scenario_id or "manual")
                evidence_entries.append(run_evidence_command(request, output_dir, None))
    write_evidence_ledger(output_dir, evidence_entries)
    evidence_ledger = load_evidence_ledger(output_dir)

    completed_at = now_utc()
    lane_result, analysis = normalize_agent_response(
        lane_id,
        brief_path,
        brief,
        source,
        diff,
        raw_output,
        bundle,
        output_dir,
        lane_metadata=lane_metadata,
        started_at=started_at,
        completed_at=completed_at,
        schema_ref=schema_ref,
        backend=backend,
        evidence_ledger=evidence_ledger,
    )
    lane_result_path = output_dir / f"review-lane-{lane_id}.v1.json"
    write_json(lane_result_path, lane_result)
    analysis_path = output_dir / f"review-lane-{lane_id}-analysis.v1.json"
    log_path = output_dir / f"review-lane-{lane_id}.log"
    return lane_result, analysis, lane_result_path, log_path, analysis_path


def write_agent_failure_result(lane_id: str, brief_path: Path, output_dir: Path, message: str, bundle: dict[str, Any] | None = None, backend: str | None = None) -> str:
    # The executable script owns the canonical failed-lane shape because it has
    # the lane metadata constants. This helper is intentionally small but kept as
    # a module-level hook for callers that want to retain failure diagnostics.
    log_path = output_dir / f"review-lane-{lane_id}.log"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("a") as handle:
        handle.write(f"agent_backend={backend or 'unknown'}\nerror={message}\n")
    return log_path.name


def _manifest_lane_runs(manifest: dict[str, Any]) -> list[dict[str, Any]]:
    lane_runs = manifest.get("lane_runs", [])
    if isinstance(lane_runs, list):
        return [run for run in lane_runs if isinstance(run, dict)]
    return []


def evaluate_cutover_readiness(preflight_manifest: dict[str, Any], required_backend: str = "fixture") -> dict[str, Any]:
    blocking_reasons: list[str] = []
    backend = preflight_manifest.get("agent_backend")
    accepted_backends = {required_backend, "external"} if required_backend == "fixture" else {required_backend}
    if backend not in accepted_backends:
        blocking_reasons.append(f"agent_backend must be {required_backend} or external")
    if preflight_manifest.get("remote_mutations") != "none":
        blocking_reasons.append("remote_mutations must be none")
    if preflight_manifest.get("status") != "passed":
        blocking_reasons.append("preflight status must be passed")

    scenarios = preflight_manifest.get("scenarios", [])
    scenario_by_id = {
        str(scenario.get("id")): scenario
        for scenario in scenarios
        if isinstance(scenario, dict) and scenario.get("id") is not None
    } if isinstance(scenarios, list) else {}
    required_semantic = [
        "inverted-auth-control-condition",
        "auth-control-static-suspicion-without-repro",
    ]
    for scenario_id in required_semantic:
        scenario = scenario_by_id.get(scenario_id)
        if not scenario:
            blocking_reasons.append(f"required semantic scenario did not run: {scenario_id}")
        elif scenario.get("status") != "passed":
            blocking_reasons.append(f"required semantic scenario failed: {scenario_id}")
        else:
            assertions = scenario.get("assertions", {})
            if not isinstance(assertions, dict) or assertions.get("semantic_fixture_passed") is not True:
                blocking_reasons.append(f"required semantic assertions did not pass: {scenario_id}")

    required_lanes = {"correctness", "test-quality", "idioms-maintainability", "security-performance"}
    lane_runs = _manifest_lane_runs(preflight_manifest)
    for scenario in scenarios if isinstance(scenarios, list) else []:
        if not isinstance(scenario, dict) or scenario.get("readiness_required") is not True:
            continue
        scenario_id = str(scenario.get("id"))
        runs = [run for run in lane_runs if run.get("scenario_id") == scenario_id]
        lanes_seen = {str(run.get("lane_id")) for run in runs}
        missing = sorted(required_lanes - lanes_seen)
        if missing:
            blocking_reasons.append(f"scenario {scenario_id} missing lane run(s): {', '.join(missing)}")
        for run in runs:
            if run.get("backend") != backend:
                blocking_reasons.append(f"scenario {scenario_id} lane {run.get('lane_id')} recorded backend {run.get('backend')}")
            if run.get("execution_status") != "succeeded":
                blocking_reasons.append(f"scenario {scenario_id} lane {run.get('lane_id')} did not succeed")

    return {
        "ready": not blocking_reasons,
        "required_backend": required_backend,
        "accepted_backends": sorted(accepted_backends),
        "required_lanes": sorted(required_lanes),
        "blocking_reasons": blocking_reasons,
    }


def validate_cutover_readiness(preflight_manifest: dict[str, Any]) -> dict[str, Any]:
    if preflight_manifest.get("artifact_type") not in {"preflight_manifest", "review_preflight_manifest"}:
        raise HarnessError("--require-cutover-ready can only validate a preflight manifest")
    computed = evaluate_cutover_readiness(preflight_manifest)
    recorded = preflight_manifest.get("cutover_readiness")
    recorded_ready = isinstance(recorded, dict) and recorded.get("ready") is True
    if computed.get("ready") is not True or not recorded_ready:
        reasons = computed.get("blocking_reasons", [])
        if isinstance(reasons, list) and reasons:
            raise HarnessError(str(reasons[0]))
        raise HarnessError("cutover readiness is false")
    return computed
