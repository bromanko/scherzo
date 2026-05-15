"""Review-lane provider contract helpers.

The helpers in this module are intentionally local-only. They validate the
provider-facing review-lane tool argument schemas, materialize captured model
submissions into canonical ReviewLaneDraft artifacts by injecting runner-owned
metadata, and run the offline fixture suite used by Scherzo's native review
workflows.
"""

from __future__ import annotations

import datetime as _datetime
import hashlib
import json
import os
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any

try:  # Reuse the dependency already required by scripts/scherzo-json-schema-validate.
    import jsonschema
    from jsonschema import Draft202012Validator
except Exception as exc:  # pragma: no cover - only exercised in broken envs
    jsonschema = None  # type: ignore[assignment]
    Draft202012Validator = None  # type: ignore[assignment]
    JSONSCHEMA_IMPORT_ERROR = exc
else:
    JSONSCHEMA_IMPORT_ERROR = None

SCHEMA_VERSION = 1
CONTRACT_SCHEMA_REF = ".scherzo/workflows/schemas/review-artifacts.v1.schema.json"
CANONICAL_DRAFT_SCHEMA = Path(
    ".scherzo/workflows/schemas/review-lane-draft.v1.schema.json"
)
CHECKER_VERSION = "review-lane-contract-v1"

RUNNER_METADATA_FIELDS = {
    "schema_version",
    "artifact_type",
    "generated_at_utc",
    "producer",
    "lane",
    "input_refs",
    "remote_mutations",
}

PROVIDER_SCHEMA_ALLOWED_KEYWORDS = {
    "type",
    "description",
    "properties",
    "required",
    "additionalProperties",
    "items",
    "minLength",
    "maxLength",
    "minimum",
    "maximum",
    "minItems",
    "maxItems",
    "pattern",
}

REQUIRED_SUBMISSION_FIELDS = [
    "draft_findings",
    "review_notes",
    "evidence_requests",
    "self_check",
]

VALID_SEVERITIES = {"info", "low", "medium", "high", "critical"}
VALID_REVIEW_NOTE_CATEGORIES = {
    "correctness",
    "maintainability",
    "security",
    "performance",
    "testing",
    "workflow",
    "documentation",
    "artifact_contract",
    "other",
}

LANES: dict[str, dict[str, str]] = {
    "correctness": {
        "id": "correctness",
        "step_id": "lane_correctness",
        "artifact_name": "correctness_submission",
        "name": "Correctness reviewer",
        "category": "correctness",
        "version": "1",
        "provider_schema": "docs/schemas/provider/review-lane-draft.correctness.v1.schema.json",
    },
    "test-quality": {
        "id": "test-quality",
        "step_id": "lane_test_quality",
        "artifact_name": "test_quality_submission",
        "name": "Test-quality reviewer",
        "category": "testing",
        "version": "1",
        "provider_schema": "docs/schemas/provider/review-lane-draft.test-quality.v1.schema.json",
    },
    "idioms-maintainability": {
        "id": "idioms-maintainability",
        "step_id": "lane_idioms_maintainability",
        "artifact_name": "idioms_maintainability_submission",
        "name": "Idioms / maintainability reviewer",
        "category": "maintainability",
        "version": "1",
        "provider_schema": "docs/schemas/provider/review-lane-draft.idioms-maintainability.v1.schema.json",
    },
    "security-performance": {
        "id": "security-performance",
        "step_id": "lane_security_performance",
        "artifact_name": "security_performance_submission",
        "name": "Security / performance risk reviewer",
        "category": "security-performance",
        "version": "1",
        "provider_schema": "docs/schemas/provider/review-lane-draft.security-performance.v1.schema.json",
    },
}

PREPARED_REVIEW_ARTIFACTS = [
    ("review-brief.v1.json", "review_brief"),
    ("diff.patch", "diff"),
    ("changed-files.v1.json", "changed_files"),
    ("validation-status.v1.json", "validation_status"),
    ("context-manifest.v1.json", "context_manifest"),
]

FORBIDDEN_RAW_VALIDATOR_NAMES = {
    "review_lane_draft_schema",
    "review_lane_semantics",
    "review_lane_draft",
}


class ContractError(Exception):
    """Classified review-lane contract failure."""

    def __init__(self, code: str, message: str) -> None:
        super().__init__(message)
        self.code = code
        self.message = message


@dataclass
class FixtureOutcome:
    lane: str
    fixture: str
    expected: str
    status: str
    code: str
    message: str
    output_path: str | None = None


def now_utc() -> str:
    return (
        _datetime.datetime.now(_datetime.timezone.utc)
        .replace(microsecond=0)
        .isoformat()
        .replace("+00:00", "Z")
    )


def load_json(path: Path) -> Any:
    try:
        with path.open("r", encoding="utf-8") as handle:
            return json.load(handle)
    except json.JSONDecodeError as exc:
        raise ContractError("review_lane_contract_invalid_json", f"invalid JSON in {path}: {exc}") from exc
    except OSError as exc:
        raise ContractError("review_lane_contract_read_failed", f"could not read {path}: {exc}") from exc


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(65536), b""):
            digest.update(chunk)
    return digest.hexdigest()


def lane_ids() -> list[str]:
    return list(LANES.keys())


def provider_schema_path(lane_id: str) -> Path:
    return Path(lane_metadata(lane_id)["provider_schema"])


def lane_metadata(lane_id: str) -> dict[str, str]:
    try:
        return LANES[lane_id]
    except KeyError as exc:
        raise ContractError("review_lane_contract_unknown_lane", f"unknown review lane: {lane_id}") from exc


def lane_runner_metadata(lane_id: str) -> dict[str, str]:
    metadata = lane_metadata(lane_id)
    return {
        "id": metadata["id"],
        "name": metadata["name"],
        "category": metadata["category"],
        "version": metadata["version"],
    }


def check_jsonschema_available() -> None:
    if JSONSCHEMA_IMPORT_ERROR is not None:
        raise ContractError(
            "review_lane_contract_jsonschema_unavailable",
            f"jsonschema import failed: {JSONSCHEMA_IMPORT_ERROR}",
        )


def load_provider_schema(schema_path: Path) -> dict[str, Any]:
    value = load_json(schema_path)
    if not isinstance(value, dict):
        raise ContractError(
            "structured_output_tool_spec_schema_not_object",
            f"provider schema must be a JSON object: {schema_path}",
        )
    return value


def check_provider_schema(schema_path: Path) -> dict[str, Any]:
    schema = load_provider_schema(schema_path)
    validate_provider_schema_keywords(schema, str(schema_path))
    if schema.get("type") != "object":
        raise ContractError(
            "structured_output_tool_spec_provider_incompatible_schema",
            f"provider schema {schema_path} must have top-level type object",
        )
    check_jsonschema_available()
    try:
        Draft202012Validator.check_schema(schema)  # type: ignore[union-attr]
    except Exception as exc:
        raise ContractError(
            "review_lane_provider_schema_invalid_json_schema",
            f"provider schema {schema_path} is not a valid JSON Schema: {exc}",
        ) from exc
    return schema


def validate_provider_schema_keywords(schema: Any, schema_path: str) -> None:
    def walk_schema(value: Any, location: str) -> None:
        if isinstance(value, dict):
            for key, child in value.items():
                if key not in PROVIDER_SCHEMA_ALLOWED_KEYWORDS:
                    raise ContractError(
                        "structured_output_tool_spec_provider_incompatible_schema",
                        f"provider schema {schema_path} contains disallowed keyword {key} at {join_location(location, key)}",
                    )
                if key == "type" and isinstance(child, list):
                    raise ContractError(
                        "structured_output_tool_spec_provider_incompatible_schema",
                        f"provider schema {schema_path} contains disallowed keyword type at {join_location(location, key)}",
                    )
                child_location = join_location(location, key)
                if key == "properties":
                    if isinstance(child, dict):
                        for property_name, property_schema in child.items():
                            walk_schema(property_schema, join_location(child_location, property_name))
                elif key in {"items", "additionalProperties"}:
                    walk_schema(child, child_location)
        elif isinstance(value, list):
            for index, item in enumerate(value):
                walk_schema(item, f"{location}[{index}]")

    walk_schema(schema, "")


def join_location(location: str, key: str) -> str:
    return key if not location else f"{location}.{key}"


def validate_submission_against_provider_schema(submission: dict[str, Any], lane_id: str) -> None:
    schema_path = provider_schema_path(lane_id)
    schema = check_provider_schema(schema_path)
    validator = Draft202012Validator(schema)  # type: ignore[operator]
    errors = sorted(validator.iter_errors(submission), key=lambda err: list(err.absolute_path))
    if errors:
        first = errors[0]
        path = json_pointer(first.absolute_path)
        raise ContractError(
            "review_lane_submission_shape_invalid",
            f"provider submission failed {schema_path} at {path or '/'}: {first.message}",
        )


def json_pointer(parts: Any) -> str:
    rendered = [str(part).replace("~", "~0").replace("/", "~1") for part in parts]
    return "/" + "/".join(rendered) if rendered else ""


def load_submission(path: Path) -> dict[str, Any]:
    value = load_json(path)
    if not isinstance(value, dict):
        raise ContractError("review_lane_submission_shape_invalid", "provider submission must be a JSON object")
    return value


def validate_model_owned_submission(submission: dict[str, Any], lane_id: str) -> None:
    unexpected = sorted(RUNNER_METADATA_FIELDS.intersection(submission.keys()))
    if unexpected:
        raise ContractError(
            "review_lane_submission_unexpected_runner_metadata",
            "provider submission must not include runner-owned metadata fields: " + ", ".join(unexpected),
        )
    validate_submission_against_provider_schema(submission, lane_id)


def prepared_review_ref_path(prepare_dir: Path, filename: str) -> str:
    raw = str(prepare_dir / filename)
    parts = Path(raw).parts
    if "artifacts" in parts:
        index = parts.index("artifacts")
        return str(Path(*parts[index:]))
    return raw


def input_refs_from_prepare_dir(prepare_dir: Path) -> list[dict[str, Any]]:
    refs: list[dict[str, Any]] = []
    for filename, artifact_type in PREPARED_REVIEW_ARTIFACTS:
        path = prepare_dir / filename
        ref: dict[str, Any] = {
            "artifact_type": artifact_type,
            "path": prepared_review_ref_path(prepare_dir, filename),
        }
        if path.exists() and path.is_file():
            ref["sha256"] = sha256_file(path)
        refs.append(ref)
    return refs


def materialize_submission(
    *,
    lane_id: str,
    submission_path: Path,
    prepare_dir: Path,
    output_path: Path,
    generated_at: str | None = None,
) -> dict[str, Any]:
    submission = load_submission(submission_path)
    validate_model_owned_submission(submission, lane_id)
    artifact = {
        "$schema": CONTRACT_SCHEMA_REF,
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "review_lane_draft",
        "generated_at_utc": generated_at or now_utc(),
        "producer": {"name": "scherzo", "version": "1", "mode": "native-review-lane"},
        "lane": lane_runner_metadata(lane_id),
        "input_refs": input_refs_from_prepare_dir(prepare_dir),
        "draft_findings": submission.get("draft_findings", []),
        "review_notes": submission.get("review_notes", []),
        "evidence_requests": submission.get("evidence_requests", []),
        "self_check": submission.get("self_check", {}),
        "remote_mutations": "none",
    }
    validate_canonical_artifact(artifact)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    write_json(output_path, artifact)
    return artifact


def validate_canonical_artifact(artifact: dict[str, Any]) -> None:
    run_json_schema_validation(artifact)
    run_semantic_validation(artifact)
    run_local_semantic_guardrails(artifact)


def run_json_schema_validation(artifact: dict[str, Any]) -> None:
    proc = subprocess.run(
        ["python3", "scripts/scherzo-json-schema-validate", "--schema", str(CANONICAL_DRAFT_SCHEMA)],
        input=json.dumps(artifact, separators=(",", ":")),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if proc.returncode != 0:
        message = proc.stdout.strip() or proc.stderr.strip() or f"exit {proc.returncode}"
        raise ContractError("review_lane_submission_canonical_validation_failed", message)


def run_semantic_validation(artifact: dict[str, Any]) -> None:
    proc = subprocess.run(
        ["python3", "scripts/scherzo-review", "validate-structured-output", "--validator", "review_lane_draft"],
        input=json.dumps(artifact, separators=(",", ":")) + "\n",
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if proc.returncode != 0:
        message = proc.stderr.strip() or proc.stdout.strip() or f"exit {proc.returncode}"
        raise ContractError("review_lane_submission_canonical_validation_failed", message)


def run_local_semantic_guardrails(artifact: dict[str, Any]) -> None:
    """Keep the contract command strict even if older script validators are looser."""
    findings = artifact.get("draft_findings") if isinstance(artifact.get("draft_findings"), list) else []
    for finding in findings:
        if not isinstance(finding, dict):
            continue
        severity = finding.get("severity")
        if severity not in VALID_SEVERITIES:
            raise ContractError("review_lane_submission_canonical_validation_failed", f"invalid severity: {severity}")
        for location in finding.get("locations", []) if isinstance(finding.get("locations"), list) else []:
            validate_location_line_order(location)
    notes = artifact.get("review_notes") if isinstance(artifact.get("review_notes"), list) else []
    for note in notes:
        if not isinstance(note, dict):
            continue
        severity = note.get("severity")
        if severity not in VALID_SEVERITIES:
            raise ContractError("review_lane_submission_canonical_validation_failed", f"invalid review note severity: {severity}")
        category = note.get("category")
        if category not in VALID_REVIEW_NOTE_CATEGORIES:
            raise ContractError("review_lane_submission_canonical_validation_failed", f"invalid review note category: {category}")
        for location in note.get("locations", []) if isinstance(note.get("locations"), list) else []:
            validate_location_line_order(location)


def validate_location_line_order(location: Any) -> None:
    if not isinstance(location, dict):
        return
    start_line = location.get("start_line")
    end_line = location.get("end_line")
    if isinstance(start_line, int) and isinstance(end_line, int) and start_line > end_line:
        raise ContractError(
            "review_lane_submission_canonical_validation_failed",
            "location start_line must not be greater than end_line",
        )


def load_manifest(lane_dir: Path) -> list[dict[str, Any]]:
    manifest_path = lane_dir / "manifest.v1.json"
    manifest = load_json(manifest_path)
    if not isinstance(manifest, dict):
        raise ContractError("review_lane_contract_manifest_invalid", f"manifest must be an object: {manifest_path}")
    fixtures = manifest.get("fixtures")
    if not isinstance(fixtures, list):
        raise ContractError("review_lane_contract_manifest_invalid", f"manifest fixtures must be a list: {manifest_path}")
    for entry in fixtures:
        if not isinstance(entry, dict) or not isinstance(entry.get("fixture"), str) or entry.get("expect") not in {"pass", "fail"}:
            raise ContractError("review_lane_contract_manifest_invalid", f"invalid fixture manifest entry in {manifest_path}")
    return fixtures


def run_fixture_suite(fixtures_dir: Path, output_dir: Path) -> list[FixtureOutcome]:
    outcomes: list[FixtureOutcome] = []
    prepare_dir = fixtures_dir / "prepared-review"
    for lane_id in lane_ids():
        lane_dir = fixtures_dir / lane_id
        entries = load_manifest(lane_dir)
        for entry in entries:
            fixture_name = str(entry["fixture"])
            expected = str(entry["expect"])
            output_path = output_dir / "lanes" / lane_id / fixture_name.replace(".arguments.json", "") / "review-lane-draft.v1.json"
            try:
                materialize_submission(
                    lane_id=lane_id,
                    submission_path=lane_dir / fixture_name,
                    prepare_dir=prepare_dir,
                    output_path=output_path,
                    generated_at="2026-01-01T00:00:00Z",
                )
            except ContractError as exc:
                status = "passed" if expected == "fail" and expected_failure_matches(entry, exc) else "failed"
                outcomes.append(FixtureOutcome(
                    lane=lane_id,
                    fixture=fixture_name,
                    expected=expected,
                    status=status,
                    code=exc.code,
                    message=exc.message,
                ))
            else:
                status = "passed" if expected == "pass" else "failed"
                outcomes.append(FixtureOutcome(
                    lane=lane_id,
                    fixture=fixture_name,
                    expected=expected,
                    status=status,
                    code="ok",
                    message="materialized and validated" if status == "passed" else "fixture unexpectedly passed",
                    output_path=str(output_path),
                ))
    return outcomes


def expected_failure_matches(entry: dict[str, Any], exc: ContractError) -> bool:
    expected_code = entry.get("code")
    contains = entry.get("contains")
    if isinstance(expected_code, str) and expected_code and exc.code != expected_code:
        return False
    if isinstance(contains, str) and contains and contains not in exc.message:
        return False
    return True


def workflow_step_block(workflow_text: str, step_id: str) -> str:
    pattern = re.compile(rf"(?ms)^  - id: {re.escape(step_id)}\n.*?(?=^  - id: |\Z)")
    match = pattern.search(workflow_text)
    if match is None:
        raise ContractError("review_lane_workflow_missing_lane_step", f"workflow is missing step {step_id}")
    return match.group(0)


def check_workflow_migration(workflow_path: Path) -> dict[str, Any]:
    text = workflow_path.read_text(encoding="utf-8")
    lane_results: dict[str, Any] = {}
    errors: list[dict[str, str]] = []
    for lane_id, metadata in LANES.items():
        try:
            block = workflow_step_block(text, metadata["step_id"])
            expected_schema = metadata["provider_schema"]
            if f"parameters_schema_path: {expected_schema}" not in block:
                raise ContractError(
                    "review_lane_workflow_uses_non_provider_schema",
                    f"{metadata['step_id']} must use provider schema {expected_schema}",
                )
            if "tool_name: submit_review_lane_draft" not in block:
                raise ContractError(
                    "review_lane_workflow_tool_name_invalid",
                    f"{metadata['step_id']} must use submit_review_lane_draft",
                )
            forbidden = [name for name in FORBIDDEN_RAW_VALIDATOR_NAMES if re.search(rf"name:\s*{re.escape(name)}\b", block)]
            if forbidden or re.search(r"--validator\s*\n\s*- review_lane_draft", block):
                raise ContractError(
                    "review_lane_workflow_raw_validator_targets_canonical_draft",
                    f"{metadata['step_id']} still validates raw provider submission as canonical ReviewLaneDraft",
                )
            materialize_id = "materialize_" + lane_id.replace("-", "_")
            materialize_block = workflow_step_block(text, materialize_id)
            if f"--lane {lane_id}" not in materialize_block and f"- {lane_id}" not in materialize_block:
                raise ContractError(
                    "review_lane_workflow_missing_materialization",
                    f"{materialize_id} must call materialize for lane {lane_id}",
                )
            if "review-lane-draft.v1.json" not in materialize_block:
                raise ContractError(
                    "review_lane_workflow_missing_materialization",
                    f"{materialize_id} must write review-lane-draft.v1.json",
                )
            lane_dir_text = f"artifacts/review/lanes/{lane_id}"
            if lane_dir_text not in text:
                raise ContractError(
                    "review_lane_workflow_materialized_draft_not_consumed",
                    f"workflow must consume materialized lane dir {lane_dir_text}",
                )
            lane_results[lane_id] = {"status": "passed", "provider_schema": expected_schema, "materialize_step": materialize_id}
        except ContractError as exc:
            lane_results[lane_id] = {"status": "failed", "code": exc.code, "message": exc.message}
            errors.append({"lane": lane_id, "code": exc.code, "message": exc.message})
    status = "passed" if not errors else "failed"
    return {"status": status, "lanes": lane_results, "errors": errors}


def check_all_provider_schemas() -> dict[str, Any]:
    lanes: dict[str, Any] = {}
    errors: list[dict[str, str]] = []
    for lane_id in lane_ids():
        schema_path = provider_schema_path(lane_id)
        try:
            check_provider_schema(schema_path)
        except ContractError as exc:
            lanes[lane_id] = {"status": "failed", "path": str(schema_path), "code": exc.code, "message": exc.message}
            errors.append({"lane": lane_id, "code": exc.code, "message": exc.message})
        else:
            lanes[lane_id] = {"status": "passed", "path": str(schema_path), "sha256": sha256_file(schema_path)}
    return {"status": "passed" if not errors else "failed", "lanes": lanes, "errors": errors}


def offline_report(workflow_path: Path, fixtures_dir: Path, output_dir: Path) -> dict[str, Any]:
    output_dir.mkdir(parents=True, exist_ok=True)
    schema_status = check_all_provider_schemas()
    workflow_status = check_workflow_migration(workflow_path)
    fixture_outcomes = run_fixture_suite(fixtures_dir, output_dir)
    fixture_status = "passed" if all(outcome.status == "passed" for outcome in fixture_outcomes) else "failed"
    report = {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "review_lane_contract_report",
        "generated_at_utc": now_utc(),
        "checker_version": CHECKER_VERSION,
        "workflow": str(workflow_path),
        "fixtures": str(fixtures_dir),
        "remote_mutations": "none",
        "schema_status": schema_status,
        "workflow_status": workflow_status,
        "validator_status": workflow_status,
        "fixture_status": fixture_status,
        "materialization_status": fixture_status,
        "canonical_validation_status": fixture_status,
        "lanes": {
            lane_id: {
                "provider_schema": lane_metadata(lane_id)["provider_schema"],
                "fixtures": [outcome.__dict__ for outcome in fixture_outcomes if outcome.lane == lane_id],
            }
            for lane_id in lane_ids()
        },
    }
    report["status"] = "passed" if schema_status["status"] == "passed" and workflow_status["status"] == "passed" and fixture_status == "passed" else "failed"
    report_path = output_dir / "contract-report.v1.json"
    write_json(report_path, report)
    return report


def live_report(workflow_path: Path, output_dir: Path, skip_if_missing_credentials: bool) -> dict[str, Any]:
    output_dir.mkdir(parents=True, exist_ok=True)
    credential_present = any(os.environ.get(name) for name in ["ANTHROPIC_API_KEY", "OPENAI_API_KEY", "GEMINI_API_KEY", "GOOGLE_API_KEY"])
    if not credential_present and skip_if_missing_credentials:
        report = {
            "schema_version": SCHEMA_VERSION,
            "artifact_type": "review_lane_live_probe_report",
            "generated_at_utc": now_utc(),
            "workflow": str(workflow_path),
            "status": "skipped",
            "code": "skipped_missing_credentials",
            "remote_mutations": "none",
        }
        write_json(output_dir / "live-probe-report.v1.json", report)
        return report

    command = [
        "direnv",
        "exec",
        ".",
        "gleam",
        "run",
        "-m",
        "scherzo/review_lane_live_probe",
        "--",
        "--workflow",
        str(workflow_path),
        "--output-dir",
        str(output_dir),
    ]
    if skip_if_missing_credentials:
        command.append("--skip-if-missing-credentials")
    proc = subprocess.run(command, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    report_path = output_dir / "live-probe-report.v1.json"
    if report_path.exists():
        report = load_json(report_path)
        if isinstance(report, dict):
            return report
    report = {
        "schema_version": SCHEMA_VERSION,
        "artifact_type": "review_lane_live_probe_report",
        "generated_at_utc": now_utc(),
        "workflow": str(workflow_path),
        "status": "failed",
        "code": "provider_tool_registration_failed" if credential_present else "review_lane_live_credentials_missing",
        "message": (proc.stderr.strip() or proc.stdout.strip() or f"live probe exited {proc.returncode}"),
        "remote_mutations": "none",
    }
    write_json(report_path, report)
    return report


def preflight_cache_key(workflow_path: Path) -> dict[str, Any]:
    workflow_text = workflow_path.read_text(encoding="utf-8")
    schema_entries = []
    for lane_id in lane_ids():
        schema_path = provider_schema_path(lane_id)
        schema_entries.append({"lane": lane_id, "path": str(schema_path), "sha256": sha256_file(schema_path)})
    key_material = {
        "checker_version": CHECKER_VERSION,
        "workflow": str(workflow_path),
        "workflow_sha256": sha256_bytes(workflow_text.encode("utf-8")),
        "provider_schemas": schema_entries,
    }
    encoded = json.dumps(key_material, sort_keys=True, separators=(",", ":"))
    return {"schema_version": SCHEMA_VERSION, "artifact_type": "review_lane_preflight_cache_key", "key": sha256_bytes(encoded.encode("utf-8")), "material": key_material}
