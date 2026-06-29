"""Shared commit_stack publication invariants for dogfood workflow helpers.

The helpers in .scherzo/workflows/scripts are copied into retained workflow
bundles and must remain self-contained.  This module intentionally depends only
on Python's standard library plus the local `jj` and `git` commands already
required by commit_stack publication.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Sequence

COMMIT_STACK_ARTIFACT_TYPE = "scherzo.git_commit_stack.v1"
BUNDLE_MEDIA_TYPE = "application/vnd.git.bundle"
DEFAULT_MAX_CARRIER_BYTES = 104_857_600
DEFAULT_HASH_CHUNK_BYTES = 1024 * 1024
ARTIFACT_RUN_ID_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")
GIT_OID_RE = re.compile(r"^[0-9a-f]{40}$")
TREE_ID_RE = re.compile(r'TreeId\(\s*"([0-9a-f]{40})"\s*,?\s*\)')
COMMAND_DIAGNOSTIC_MAX_CHARS = 3500


@dataclass(frozen=True)
class CommitStackError(Exception):
    """A targeted, user-facing commit_stack materialization failure."""

    code: str
    message: str

    def __str__(self) -> str:
        return self.message


def fail(code: str, message: str) -> None:
    raise CommitStackError(code, message)


def diagnostic_excerpt(value: str, max_chars: int = COMMAND_DIAGNOSTIC_MAX_CHARS) -> str:
    value = value.strip()
    if len(value) <= max_chars:
        return value
    marker = "\n... truncated ...\n"
    available = max(0, max_chars - len(marker))
    omitted = len(value) - available
    marker = f"\n... truncated {omitted} chars ...\n"
    available = max(0, max_chars - len(marker))
    head_chars = available // 2
    tail_chars = available - head_chars
    return value[:head_chars] + marker + value[-tail_chars:]


def subprocess_failure_details(stdout: str, stderr: str, returncode: int) -> str:
    parts = [f"exit_code: {returncode}"]
    if stderr.strip():
        parts.append("stderr:\n" + diagnostic_excerpt(stderr))
    if stdout.strip():
        parts.append("stdout:\n" + diagnostic_excerpt(stdout))
    return "\n\n".join(parts)


def shell_display(value: str) -> str:
    if re.match(r"^[A-Za-z0-9_./:=+@^-]+$", value):
        return value
    return "'" + value.replace("'", "'\\''") + "'"


def command_display(args: Sequence[str]) -> str:
    return " ".join(shell_display(arg) for arg in args)


def run_proc(args: Sequence[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        list(args),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def run_checked(
    args: Sequence[str],
    *,
    code: str = "invalid_commit_stack_artifact",
    message: str | None = None,
) -> str:
    proc = run_proc(args)
    if proc.returncode != 0:
        details = subprocess_failure_details(proc.stdout, proc.stderr, proc.returncode)
        fail(
            code,
            (message or f"command failed while materializing commit_stack: {command_display(args)}")
            + "\n"
            + details,
        )
    return proc.stdout


def require_command(name: str) -> None:
    if shutil.which(name) is None:
        fail("required_command_missing", f"required command not found: {name}")


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, indent=2, sort_keys=True, separators=(",", ": ")) + "\n",
        encoding="utf-8",
    )


def require_artifact_run_id(value: str) -> str:
    run_id = value.strip() or "local-run"
    if ARTIFACT_RUN_ID_RE.fullmatch(run_id):
        return run_id
    fail(
        "invalid_configuration",
        "SCHERZO_RUN_ID must be a safe artifact path segment containing only "
        "letters, digits, '.', '_' and '-'",
    )


def run_id_for_artifacts() -> str:
    return require_artifact_run_id(os.environ.get("SCHERZO_RUN_ID", "local-run"))


def artifact_output_ref(run_id: str, name: str) -> str:
    return f"runs/{run_id}/outputs/{name}"


def append_artifact_dir_candidate(
    candidates: list[Path], seen: set[str], root: Path, run_id: str
) -> None:
    candidate = root / ".scherzo-state" / "artifacts" / "runs" / run_id
    key = str(candidate)
    if key not in seen:
        seen.add(key)
        candidates.append(candidate)


def state_root_search_paths() -> list[Path]:
    roots: list[Path] = []
    seen: set[str] = set()
    starts = [Path.cwd()]
    run_root = os.environ.get("SCHERZO_RUN_ROOT", "").strip()
    if run_root:
        starts.append(Path(run_root))

    for start in starts:
        for root in [start, *start.parents]:
            key = str(root)
            if key in seen:
                continue
            seen.add(key)
            if (root / ".scherzo-state").is_dir():
                roots.append(root)
    return roots


def run_artifact_dir_candidates(run_id: str) -> list[Path]:
    candidates: list[Path] = []
    seen: set[str] = set()

    run_root = os.environ.get("SCHERZO_RUN_ROOT", "").strip()
    repo_root = os.environ.get("SCHERZO_REPO_ROOT", "").strip()

    if not run_root and not repo_root:
        # Scrubbed local invocations may run inside a retained Scherzo workspace;
        # without workflow env, only honor an artifact store rooted at cwd.
        cwd = Path.cwd()
        if (cwd / ".scherzo-state").is_dir():
            append_artifact_dir_candidate(candidates, seen, cwd, run_id)
        return candidates

    if run_root:
        run_root_path = Path(run_root)
        append_artifact_dir_candidate(candidates, seen, run_root_path, run_id)
        append_artifact_dir_candidate(
            candidates, seen, run_root_path.parent.parent.parent, run_id
        )

    if repo_root:
        append_artifact_dir_candidate(candidates, seen, Path(repo_root), run_id)

    for root in state_root_search_paths():
        append_artifact_dir_candidate(candidates, seen, root, run_id)

    return candidates


def artifact_store_exists(candidate: Path) -> bool:
    state_root = candidate.parent.parent.parent
    return candidate.exists() or candidate.parent.is_dir() or state_root.is_dir()


def run_artifact_dir(run_id: str) -> Path | None:
    explicit = os.environ.get("SCHERZO_RUN_ARTIFACT_DIR", "").strip()
    if explicit:
        return Path(explicit)
    candidates = run_artifact_dir_candidates(run_id)
    run_root = os.environ.get("SCHERZO_RUN_ROOT", "").strip()
    if run_root and candidates:
        # SCHERZO_RUN_ROOT=$PWD is used by local helper fixtures to request an
        # isolated run root, even when an outer Scherzo artifact store exists.
        try:
            if Path(run_root).resolve() == Path.cwd().resolve():
                return candidates[0]
        except OSError:
            pass
    for candidate in candidates:
        if candidate.exists():
            return candidate
    for candidate in candidates:
        if artifact_store_exists(candidate):
            return candidate
    if candidates:
        if run_root and len(candidates) > 1:
            return candidates[1]
        return candidates[0]
    return None


def carrier_max_bytes(default_max: int = DEFAULT_MAX_CARRIER_BYTES) -> int:
    override = os.environ.get("SCHERZO_COMMIT_STACK_MAX_CARRIER_BYTES", "").strip()
    if not override:
        return default_max
    try:
        override_bytes = int(override)
    except ValueError:
        fail(
            "invalid_configuration",
            "SCHERZO_COMMIT_STACK_MAX_CARRIER_BYTES must be a positive integer",
        )
    if override_bytes <= 0:
        fail(
            "invalid_configuration",
            "SCHERZO_COMMIT_STACK_MAX_CARRIER_BYTES must be a positive integer",
        )
    return min(override_bytes, default_max)


def file_sha256(path: Path, chunk_bytes: int = DEFAULT_HASH_CHUNK_BYTES) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(chunk_bytes), b""):
            digest.update(chunk)
    return digest.hexdigest()


def require_git_oid(value: str, *, label: str) -> str:
    oid = value.strip().lower()
    if GIT_OID_RE.fullmatch(oid):
        return oid
    fail(
        "invalid_commit_stack_artifact",
        f"{label} did not resolve to a 40-character Git object ID",
    )


def unique_git_oids(values: Iterable[str]) -> list[str]:
    oids: list[str] = []
    seen: set[str] = set()
    for value in values:
        oid = require_git_oid(value, label="commit_stack bundle prerequisite")
        if oid not in seen:
            seen.add(oid)
            oids.append(oid)
    return oids


def jj_log_value(revision: str, template: str, *, code: str, label: str) -> str:
    proc = run_proc([
        "jj",
        "log",
        "-r",
        revision,
        "--no-graph",
        "-T",
        template,
        "--color=never",
    ])
    if proc.returncode != 0:
        details = subprocess_failure_details(proc.stdout, proc.stderr, proc.returncode)
        fail(code, f"could not read {label} for jj revision {revision}\n{details}")
    return proc.stdout.strip()


def jj_commit_oid(revision: str) -> str:
    return require_git_oid(
        jj_log_value(
            revision,
            "commit_id",
            code="invalid_commit_stack_artifact",
            label="Git commit ID",
        ),
        label=f"jj revision {revision}",
    )


def jj_description(revision: str) -> str:
    return jj_log_value(
        revision,
        "description",
        code="commit_stack_description_failed",
        label="description",
    ).strip()


def ensure_publication_commit_description(title: str | None) -> None:
    if jj_description("@").strip():
        return
    description = (title or "").strip() or "Scherzo publication"
    describe = run_proc(["jj", "describe", "-m", description])
    if describe.returncode != 0:
        details = subprocess_failure_details(
            describe.stdout,
            describe.stderr,
            describe.returncode,
        )
        fail(
            "commit_stack_description_failed",
            "could not set a jj description before materializing commit_stack publication\n"
            + details,
        )
    if not jj_description("@").strip():
        fail(
            "commit_stack_description_failed",
            "jj describe completed but the selected commit still has an empty description",
        )


def jj_tree_oid_for_commit(commit_oid: str, *, label: str) -> str:
    commit = require_git_oid(commit_oid, label=label)
    proc = run_proc(["jj", "debug", "object", "commit", commit])
    if proc.returncode != 0:
        details = subprocess_failure_details(proc.stdout, proc.stderr, proc.returncode)
        fail(
            "invalid_commit_stack_artifact",
            f"could not inspect Git tree for {label}\n{details}",
        )
    match = TREE_ID_RE.search(proc.stdout)
    if not match:
        fail(
            "invalid_commit_stack_artifact",
            f"{label} did not expose a 40-character Git tree ID",
        )
    return require_git_oid(match.group(1), label=f"tree for {label}")


def commit_stack_bundle_ref(run_id: str, head_oid: str, namespace: str) -> str:
    run_hash = hashlib.sha256(run_id.encode("utf-8")).hexdigest()
    safe_namespace = namespace.strip().strip("/") or "commit-stack"
    return f"refs/scherzo/{safe_namespace}/{run_hash}/{head_oid}"


def write_commit_stack_carrier(
    *,
    run_id: str,
    carrier_name: str,
    head_oid: str,
    prerequisite_oids: Sequence[str],
    bundle_ref_namespace: str = "commit-stack",
    tmp_dir: Path = Path("tmp"),
    max_carrier_bytes: int = DEFAULT_MAX_CARRIER_BYTES,
) -> tuple[str, str, int]:
    require_command("git")
    artifact_dir = run_artifact_dir(run_id)
    if artifact_dir is None:
        carrier_path = tmp_dir / carrier_name
        carrier_ref = str(carrier_path)
    else:
        carrier_path = artifact_dir / "outputs" / carrier_name
        carrier_ref = artifact_output_ref(run_id, carrier_name)
    carrier_path.parent.mkdir(parents=True, exist_ok=True)
    if carrier_path.exists():
        carrier_path.unlink()

    prerequisites = [f"^{oid}" for oid in unique_git_oids(prerequisite_oids)]
    bundle_ref = commit_stack_bundle_ref(run_id, head_oid, bundle_ref_namespace)
    try:
        run_checked(
            ["git", "update-ref", bundle_ref, head_oid],
            code="invalid_commit_stack_artifact",
        )
        run_checked(
            ["git", "bundle", "create", str(carrier_path), bundle_ref, *prerequisites],
            code="invalid_commit_stack_artifact",
        )
        run_checked(
            ["git", "bundle", "verify", str(carrier_path)],
            code="invalid_commit_stack_artifact",
        )
    finally:
        run_proc(["git", "update-ref", "-d", bundle_ref])

    try:
        carrier_bytes = carrier_path.stat().st_size
    except OSError as exc:
        fail(
            "invalid_commit_stack_artifact",
            f"commit_stack carrier bundle was not created at {carrier_path}: {exc}",
        )
    max_bytes = carrier_max_bytes(max_carrier_bytes)
    if carrier_bytes > max_bytes:
        try:
            carrier_path.unlink()
        except FileNotFoundError:
            pass
        fail(
            "commit_stack_carrier_too_large",
            "commit_stack carrier bundle exceeds maximum supported size "
            f"({carrier_bytes} bytes > {max_bytes} bytes)",
        )
    return carrier_ref, file_sha256(carrier_path), carrier_bytes


def materialize_commit_stack_artifact(
    *,
    repository: str,
    base_ref: str,
    output_path: Path,
    carrier_name: str,
    description_title: str | None,
    base_revision: str | None = None,
    base_oid: str | None = None,
    head_revision: str = "@",
    prerequisite_oids: Sequence[str] | None = None,
    bundle_ref_namespace: str = "commit-stack",
    tmp_dir: Path = Path("tmp"),
    max_carrier_bytes: int = DEFAULT_MAX_CARRIER_BYTES,
) -> dict[str, Any]:
    """Materialize a validated scherzo.git_commit_stack.v1 artifact.

    The head commit description is repaired before reading head/tree IDs.  This
    prevents a `jj describe` rewrite from invalidating a previously captured
    head SHA and gives every commit-stack-producing helper the same empty-
    description behavior.
    """
    ensure_publication_commit_description(description_title)
    run_id = run_id_for_artifacts()
    resolved_base_oid = require_git_oid(base_oid, label="commit_stack base") if base_oid else jj_commit_oid(base_revision or "@-")
    head_oid = jj_commit_oid(head_revision)
    head_tree_oid = jj_tree_oid_for_commit(head_oid, label=f"jj revision {head_revision}")
    resolved_prerequisites = list(prerequisite_oids) if prerequisite_oids is not None else [resolved_base_oid]
    carrier_ref, carrier_sha256, carrier_bytes = write_commit_stack_carrier(
        run_id=run_id,
        carrier_name=carrier_name,
        head_oid=head_oid,
        prerequisite_oids=resolved_prerequisites,
        bundle_ref_namespace=bundle_ref_namespace,
        tmp_dir=tmp_dir,
        max_carrier_bytes=max_carrier_bytes,
    )
    artifact = {
        "schema_version": 1,
        "artifact_type": COMMIT_STACK_ARTIFACT_TYPE,
        "repository": {"repo": repository},
        "base": {"ref": base_ref, "sha": resolved_base_oid},
        "head": {"sha": head_oid, "tree": head_tree_oid},
        "carrier": {
            "ref": carrier_ref,
            "sha256": carrier_sha256,
            "bytes": carrier_bytes,
            "media_type": BUNDLE_MEDIA_TYPE,
        },
    }
    write_json(output_path, artifact)
    return artifact
