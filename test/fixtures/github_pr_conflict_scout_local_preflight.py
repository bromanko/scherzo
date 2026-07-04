#!/usr/bin/env python3
"""Hermetic regression checks for the GitHub PR conflict scout helper.

This script is invoked from the Gleam test suite so the production Python helper
can be exercised without requiring network GitHub/Linear access.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import sys
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
SCOUT_PATH = ROOT / "workflows/dogfood/scripts/scherzo-github-pr-conflict-scout"
TMP_ROOT = ROOT / "test/tmp/github-pr-conflict-scout-local-preflight-real"


def load_scout() -> Any:
    loader = importlib.machinery.SourceFileLoader("scherzo_conflict_scout", str(SCOUT_PATH))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    if spec is None:
        raise AssertionError("failed to load scout module spec")
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


def run(args: list[str], cwd: Path | None = None, check: bool = True) -> subprocess.CompletedProcess[str]:
    env = {
        **os.environ,
        "GIT_AUTHOR_NAME": "Scherzo Test",
        "GIT_AUTHOR_EMAIL": "scherzo@example.test",
        "GIT_COMMITTER_NAME": "Scherzo Test",
        "GIT_COMMITTER_EMAIL": "scherzo@example.test",
    }
    proc = subprocess.run(
        args,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if check and proc.returncode != 0:
        raise AssertionError(
            f"command failed ({proc.returncode}): {' '.join(args)}\n"
            f"stdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        )
    return proc


def git(cwd: Path | None, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return run(["git", *args], cwd=cwd, check=check)


def commit_file(work: Path, path: str, body: str, message: str) -> None:
    target = work / path
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(body, encoding="utf-8")
    git(work, "add", path)
    git(work, "commit", "-m", message)


def build_remote_fixture() -> tuple[Path, Path]:
    shutil.rmtree(TMP_ROOT, ignore_errors=True)
    TMP_ROOT.mkdir(parents=True)
    remote = TMP_ROOT / "remote.git"
    work = TMP_ROOT / "work"
    cache = TMP_ROOT / "cache.git"

    git(None, "init", "--bare", str(remote))
    git(None, "init", str(work))
    git(work, "config", "user.name", "Scherzo Test")
    git(work, "config", "user.email", "scherzo@example.test")
    git(work, "config", "commit.gpgsign", "false")
    commit_file(work, "shared.txt", "common\n", "initial")
    git(work, "branch", "-M", "main")
    git(work, "remote", "add", "origin", str(remote))
    git(work, "push", "origin", "main")

    git(work, "checkout", "-b", "feature/clean")
    commit_file(work, "clean.txt", "clean\n", "clean branch")
    git(work, "push", "origin", "feature/clean")

    git(work, "checkout", "main")
    git(work, "checkout", "-b", "feature/conflict")
    commit_file(work, "shared.txt", "head change\n", "conflicting branch")
    git(work, "push", "origin", "feature/conflict")

    git(work, "checkout", "main")
    commit_file(work, "shared.txt", "base change\n", "base change")
    git(work, "push", "origin", "main")

    git(None, "init", "--bare", str(cache))
    return remote, cache


def assert_no_conflict_refs(cache: Path) -> None:
    refs = git(
        None,
        "--git-dir",
        str(cache),
        "for-each-ref",
        "--format=%(refname)",
        "refs/scherzo/conflict-scout",
    )
    if refs.stdout.strip():
        raise AssertionError(f"conflict scout refs were not cleaned up: {refs.stdout}")


def safe_pr(scout: Any, number: int, head_branch: str) -> Any:
    return scout.SafePullRequest(
        repo="scherzo-systems/scherzo",
        number=number,
        url=f"https://github.com/scherzo-systems/scherzo/pull/{number}",
        base_branch="main",
        head_branch=head_branch,
        base_sha=None,
        head_sha=None,
    )


def exercise_real_git_preflight(scout: Any) -> None:
    remote, cache = build_remote_fixture()
    git_command = ["git", "--git-dir", str(cache)]

    clean = scout.local_merge_tree_preflight(
        safe_pr(scout, 101, "feature/clean"), str(remote), git_command
    )
    if clean.get("status") != "clean" or not clean.get("base_sha") or not clean.get("head_sha"):
        raise AssertionError(f"expected clean merge-tree result with SHAs, got {clean}")
    assert_no_conflict_refs(cache)

    conflicted = scout.local_merge_tree_preflight(
        safe_pr(scout, 102, "feature/conflict"), str(remote), git_command
    )
    if conflicted.get("status") != "conflicted":
        raise AssertionError(f"expected conflicted merge-tree result, got {conflicted}")
    if "shared.txt" not in conflicted.get("paths", []):
        raise AssertionError(f"expected shared.txt conflicted path, got {conflicted}")
    assert_no_conflict_refs(cache)

    missing = scout.local_merge_tree_preflight(
        safe_pr(scout, 103, "feature/missing"), str(remote), git_command
    )
    if missing != {"status": "unavailable", "reason": "head_ref_fetch_failed"}:
        raise AssertionError(f"expected head ref fetch failure, got {missing}")
    assert_no_conflict_refs(cache)


def exercise_merge_tree_unavailable_skip(scout: Any) -> None:
    original_run_proc = scout.run_proc

    def fake_run_proc(args: list[str], cwd: str | None = None) -> subprocess.CompletedProcess[str]:
        if args == ["git", "rev-parse", "--git-dir"]:
            return subprocess.CompletedProcess(args, 0, ".git\n", "")
        if args == ["git", "merge-tree", "-h"]:
            return subprocess.CompletedProcess(args, 0, "usage: git merge-tree\n", "")
        raise AssertionError(f"unexpected command for unavailable probe: {args}")

    scout.run_proc = fake_run_proc
    try:
        result = scout.make_local_merge_tree_preflight("origin")(
            safe_pr(scout, 104, "feature/conflict")
        )
    finally:
        scout.run_proc = original_run_proc
    if result != {"status": "unavailable", "reason": "merge_tree_unavailable"}:
        raise AssertionError(f"expected merge-tree unavailable skip, got {result}")


class FixtureGithub:
    def __init__(self, pulls: list[dict[str, Any]]) -> None:
        self.pulls = pulls

    def list_open_pulls(self) -> list[dict[str, Any]]:
        return self.pulls


class RecordingLinear:
    def __init__(self, scout: Any) -> None:
        self.scout = scout
        self.created: list[dict[str, Any]] = []

    def fetch_project(self, project_slug: str) -> dict[str, Any]:
        return {
            "id": "project-id",
            "teams": [
                {
                    "id": "team-id",
                    "states": [{"id": "todo-id", "name": "Todo", "type": "unstarted"}],
                    "labels": [
                        {"id": "workflow-label-id", "name": self.scout.DEFAULT_WORKFLOW_LABEL}
                    ],
                }
            ],
        }

    def fetch_candidate_issues(self, project_slug: str, workflow_label: str) -> list[dict[str, Any]]:
        return []

    def create_issue(self, input_payload: dict[str, Any]) -> dict[str, Any]:
        self.created.append(input_payload)
        return {
            "id": "created-id",
            "identifier": "LIV-SCOUT-1",
            "url": "https://linear.example/LIV-SCOUT-1",
            "description": input_payload.get("description"),
        }

    def update_issue_description(self, issue_id: str, description: str) -> dict[str, Any]:
        raise AssertionError("unexpected update in path bound test")


def exercise_path_bounds_in_summary_and_description(scout: Any) -> None:
    raw_paths = ["dir/bad\nname.txt"] + [
        f"conflicts/path-{index}.txt" for index in range(scout.MAX_CONFLICTED_PATHS + 3)
    ]
    pull = {
        "number": 105,
        "html_url": "https://github.com/scherzo-systems/scherzo/pull/105",
        "draft": False,
        "base": {
            "repo": {"full_name": "scherzo-systems/scherzo"},
            "ref": "main",
            "sha": "base-sha",
        },
        "head": {
            "repo": {"full_name": "scherzo-systems/scherzo"},
            "ref": "feature/conflict",
            "sha": "head-sha",
        },
    }
    options = scout.Options(
        repo="scherzo-systems/scherzo",
        linear_project_slug="test-project",
        create_state="Todo",
        workflow_label=scout.DEFAULT_WORKFLOW_LABEL,
        json_summary=True,
        git_remote="origin",
        max_open_prs=None,
        observed_at="2026-05-09T20:00:00Z",
    )
    linear = RecordingLinear(scout)
    summary = scout.scan_with_clients(
        options,
        FixtureGithub([pull]),
        lambda: linear,
        lambda pr: {
            "status": "conflicted",
            "paths": raw_paths,
            "base_sha": "base-sha",
            "head_sha": "head-sha",
        },
    )
    conflicted = summary["conflicted_prs"][0]
    if len(conflicted["paths"]) != scout.MAX_CONFLICTED_PATHS:
        raise AssertionError(f"expected bounded path list, got {conflicted}")
    if conflicted.get("paths_omitted") != 4:
        raise AssertionError(f"expected omitted path count, got {conflicted}")
    if conflicted["paths"][0] != "dir/bad?name.txt":
        raise AssertionError(f"expected control-character sanitization, got {conflicted}")
    description = linear.created[0]["description"]
    if "dir/bad\nname.txt" in description:
        raise AssertionError(f"description contains unsanitized newline path: {description}")
    if "4 more conflicted path(s) omitted" not in description:
        raise AssertionError(f"description is missing truncation marker: {description}")


def main() -> int:
    scout = load_scout()
    exercise_real_git_preflight(scout)
    exercise_merge_tree_unavailable_skip(scout)
    exercise_path_bounds_in_summary_and_description(scout)
    print("github_pr_conflict_scout_local_preflight: ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
