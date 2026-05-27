# Workflow Artifact Taxonomy

Status: Draft design for refactor. This document describes the target contract shape for a future workflow-output refactor. The current runtime still uses the legacy `ContractType` enum in `src/scherzo/workflow_contract.gleam`.

Purpose: Define the boundary between Scherzo-core artifact carrier semantics and workflow-owned artifact semantics so workflows can introduce domain artifacts such as ExecPlan bundles without adding new Scherzo daemon enum cases.

## 1. Problem

Scherzo workflow contracts currently use a fixed core output type vocabulary that mixes generic carrier concepts with workflow-specific domains. Generic concepts include text, retained artifacts, URLs, and git refs. Workflow-specific concepts include `exec_plan`, `exec_plan_bundle`, `implementation_pack`, and `code_change_bundle`.

That mixture makes Scherzo core aware of particular workflows. It also makes future workflow domains look like they need daemon changes, even when the daemon only needs to retain bytes, record hashes, validate JSON, or pass named outputs to later workflows.

The refactor target is a smaller generic taxonomy. Scherzo core should answer: how is this output carried, addressed, hashed, and validated? Workflows and validators should answer: what does this output mean?

## 2. Core principles

Scherzo core owns the generic artifact descriptor shape, the small `kind` taxonomy, artifact-store refs, hash and byte accounting for retained bytes, inline value encoding, external reference validation for built-in reference types, and manifest recording.

Workflow definitions own output names, semantic artifact types, schemas, command validators, JSON payload shape, and the choice of which artifacts are meaningful to humans or machines.

A semantic artifact type is an opaque string such as `scherzo.exec_plan_bundle.v2` or `acme.release_notes.v1`. Scherzo core may store it, show it, and pass it to validators, but core should not branch on it for domain behavior.

A retained artifact ref is a Scherzo artifact-store ref, not a filesystem path. Consumers must not assume refs are local paths. A ref such as `runs/run-1/outputs/plan.md` must remain valid if the artifact store later moves from the local filesystem to a database, object store, or remote API.

## 3. Common artifact descriptor fields

Every named output or artifact-set entry should be represented by an artifact descriptor.

Required common fields:

- `name`: stable role name used by downstream consumers, mappings, and humans. This is not a filename and not a display title.
- `kind`: one of the core carrier kinds defined below.

Optional common fields:

- `artifact_type`: workflow- or domain-owned semantic type string. Use this when the artifact has a formal contract or schema.
- `description`: human-readable explanation of the output role.
- `source`: provenance metadata describing the step, field, path, structured-output artifact, or literal source that produced the value.
- `validation`: summaries of validators that checked the payload or reference.
- `metadata`: small JSON object for domain-neutral annotations. Domain-critical fields should live in the artifact payload or be enforced by validators, not hidden in metadata.

`name` is required because it is the stable contract key. `artifact_type` is optional because not every retained file or inline value has formal domain semantics.

## 4. Core kinds

The target core taxonomy has four carrier kinds: `file`, `value`, `ref`, and `artifact_set`.

### 4.1 `file`

A `file` is a leaf artifact whose exact bytes are retained by Scherzo and independently addressable by artifact-store ref.

Required fields for `kind: file`:

- `name`
- `kind: "file"`
- `ref`: backend-neutral Scherzo artifact-store ref.
- `sha256`: SHA-256 of the exact retained bytes.
- `bytes`: byte count of the exact retained bytes.
- `media_type`: payload media type, for example `text/plain`, `text/markdown`, `application/json`, or `text/x-patch`.

Optional fields include `artifact_type`, `source`, `validation`, `description`, and `metadata`.

A `file` is a leaf. It does not contain named children as far as core is concerned. A JSON file with children is still a `file` unless the output is intentionally an aggregate contract; in that case use `artifact_set`.

Example:

```json
{
  "name": "plan",
  "kind": "file",
  "media_type": "text/markdown",
  "artifact_type": "scherzo.exec_plan.v1",
  "ref": "runs/run-1/outputs/plan.md",
  "sha256": "abc123...",
  "bytes": 18422
}
```

### 4.2 `value`

A `value` is an inline JSON-compatible value embedded directly in the contract manifest.

Required fields for `kind: value`:

- `name`
- `kind: "value"`
- `value`: any valid JSON value, including object, array, string, number, boolean, or null.

Optional fields:

- `media_type`: defaults to `application/json`. Version 1 should only use `application/json`; the field exists for forward compatibility.
- `artifact_type`, `source`, `validation`, `description`, and `metadata`.

A `value` is always inline, always JSON-encoded, and has no independent artifact ref, hash, or byte count. Use `value` for small status values, parameters, verdicts, or other data that does not need independent retention.

Example:

```json
{
  "name": "verdict",
  "kind": "value",
  "media_type": "application/json",
  "value": {
    "status": "passed",
    "blocking_findings": 0
  }
}
```

### 4.3 `ref`

A `ref` is a durable pointer to something not stored as a Scherzo artifact. Scherzo owns the reference string only as an identifier; it does not own the target bytes.

Required fields for `kind: ref`:

- `name`
- `kind: "ref"`
- `ref_type`: reference namespace or protocol.
- `ref`: the reference string.

Initial built-in `ref_type` values:

- `url`: an HTTP or HTTPS URL.
- `git_ref`: a git ref string such as a branch or tag name.

Core should validate built-in reference types. Unknown or custom `ref_type` values are domain-owned and should require explicit validators before they are used for durable workflow chaining.

A `ref` does not normally have `media_type`, because the descriptor is not the payload. If exact bytes matter, copy or materialize the target into the artifact store and use `file` instead.

Examples:

```json
{
  "name": "pull_request",
  "kind": "ref",
  "ref_type": "url",
  "ref": "https://github.com/org/repo/pull/123"
}
```

```json
{
  "name": "branch",
  "kind": "ref",
  "ref_type": "git_ref",
  "ref": "feature/liv-123"
}
```

### 4.4 `artifact_set`

An `artifact_set` is an aggregate of named artifact descriptors. It is the generic replacement for workflow-specific bundle enum cases.

An artifact set is used when the output is not one leaf payload but a named collection of files, values, refs, or nested artifact sets. The aggregate may have its own `artifact_type`, schema, validators, and retained manifest bytes.

Required logical fields for `kind: artifact_set`:

- `name`
- `kind: "artifact_set"`
- `entries`: named artifact descriptors, each with its own `name` and `kind`.

When an artifact set is itself a durable workflow output or cross-workflow handoff, it should also be retained as an exact JSON manifest and include the same retained-byte fields as `file`:

- `ref`
- `sha256`
- `bytes`
- `media_type: "application/json"`

This lets downstream workflows depend on the exact aggregate membership and hashes rather than on a local path or mutable workspace state.

Example:

```json
{
  "name": "exec_plan_bundle",
  "kind": "artifact_set",
  "media_type": "application/json",
  "artifact_type": "scherzo.exec_plan_bundle.v2",
  "ref": "runs/run-1/outputs/exec_plan_bundle.json",
  "sha256": "def456...",
  "bytes": 4096,
  "entries": [
    {
      "name": "plan",
      "kind": "file",
      "media_type": "text/markdown",
      "artifact_type": "scherzo.exec_plan.v1",
      "ref": "runs/run-1/outputs/plan.md",
      "sha256": "abc123...",
      "bytes": 18422
    },
    {
      "name": "implementation_pack",
      "kind": "file",
      "media_type": "application/json",
      "artifact_type": "scherzo.implementation_pack.v1",
      "ref": "runs/run-1/outputs/implementation_pack.json",
      "sha256": "789abc...",
      "bytes": 9211
    }
  ]
}
```

## 5. Remote-store compatibility

The durable contract must use `ref`, `sha256`, `bytes`, `media_type`, and `kind`, not filesystem paths.

Scherzo may expose operator convenience fields such as `uri`, `display_path`, or `local_path`. These fields are not durable identity and must not be required for workflow chaining. A database-backed or remote artifact API may set `local_path` to null while still supporting artifact reads by `ref`.

For retained bytes, `sha256` and `bytes` are calculated over the exact bytes returned by the artifact store for `ref`. This remains true regardless of whether the backend is a filesystem directory, database row, object-store object, or remote API response.

## 6. Legacy type mapping

The current `ContractType` enum can be treated as legacy aliases during migration.

| Current type | Target representation |
| --- | --- |
| `text` | `kind: file`, `media_type: text/plain` |
| `document.markdown` | `kind: file`, `media_type: text/markdown` |
| `exec_plan` | `kind: file`, `media_type: text/markdown`, workflow-owned `artifact_type` such as `scherzo.exec_plan.v1` |
| `exec_plan_bundle` | `kind: artifact_set`, `media_type: application/json`, workflow-owned `artifact_type` such as `scherzo.exec_plan_bundle.v2` |
| `implementation_pack` | `kind: file`, `media_type: application/json`, workflow-owned `artifact_type` such as `scherzo.implementation_pack.v1` |
| `code_change_bundle` | `kind: artifact_set`, `media_type: application/json`, workflow-owned `artifact_type` |
| `code_change` | domain-specific value, file, ref, or artifact set depending on the workflow contract; core should not hard-code code-change semantics in the target model |
| `url` | `kind: ref`, `ref_type: url` |
| `git_ref` | `kind: ref`, `ref_type: git_ref` |
| `artifact[]` | `kind: artifact_set` with entries |

The current runtime has one hard-coded semantic check for inline `code_change`: it must contain one of `pr_url`, `branch`, `merge_commit`, or `patch_ref`. In the target model, that rule should move to a workflow-owned schema or command validator.

## 7. Core-versus-workflow ownership boundary

Scherzo core should own:

- parsing and validating the generic descriptor shape;
- the `kind` enum: `file`, `value`, `ref`, `artifact_set`;
- required fields for each kind;
- artifact-store write/read by backend-neutral `ref`;
- `sha256` and `bytes` for retained bytes;
- built-in validation for `ref_type: url` and `ref_type: git_ref`;
- source/provenance capture from step fields, step files, structured output, inline values, static refs, and future mapped outputs;
- manifest persistence and idempotent recovery behavior.

Workflows, schemas, and validators should own:

- semantic `artifact_type` strings;
- JSON schemas for domain artifacts;
- command validators for domain semantics;
- human-versus-machine consumption intent;
- bundle membership rules beyond generic descriptor validity;
- code-review, ExecPlan, implementation-pack, proof-bundle, release-note, or other domain meanings.

## 8. Refactoring direction

A safe refactor should be additive first.

Introduce the generic descriptor model alongside the legacy `ContractType` enum. Teach output materialization to produce generic descriptors while preserving legacy fields or compatibility aliases. Add tests that prove legacy workflow contracts map to the new descriptors without changing observable run behavior. Then migrate dogfood workflows from workflow-specific core types to generic kinds plus workflow-owned `artifact_type` and validators. Finally, remove or deprecate daemon branches that know about `exec_plan_bundle`, `implementation_pack`, `code_change_bundle`, and similar workflow-specific concepts.

The migration is successful when a new workflow can define a formal domain artifact using only generic `kind`, `media_type`, `artifact_type`, and validators, without changing Scherzo core.
