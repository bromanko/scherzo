# Additive workflow artifact descriptor compatibility

## Purpose / Big Picture

After this change, Scherzo workflow output manifests can describe each retained file, inline value, external reference, or bundle with a generic artifact descriptor using `kind: file`, `kind: value`, `kind: ref`, or `kind: artifact_set`. Operators and follow-up workflows still see the legacy manifest fields they rely on today, but a new workflow-domain artifact can be identified with an opaque `artifact_type` string instead of another daemon-owned `ContractType` enum case. The observable result is a manifest that keeps fields such as `type`, `ref_kind`, `ref`, `sha256`, `bytes`, `media_type`, and `value`, while also carrying a validated descriptor shape matching `docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md`.

## Problem Framing and Constraints

The current contract layer mixes carrier semantics with workflow semantics: `src/scherzo/workflow_contract.gleam` has legacy enum cases for generic carriers such as `url` and `git_ref` and also for domain concepts such as `exec_plan_bundle`, `implementation_pack`, and `code_change_bundle`. `src/scherzo/workflow_contract_manifest.gleam` and `src/scherzo/workflow_run/contract_io.gleam` then record legacy manifest fields instead of the generic descriptor model defined in the taxonomy spec. The implementation must be additive because dogfood workflow YAML, retained artifacts, handoff emission, and tests already depend on those legacy names. This phase must not migrate `.scherzo/workflows/*.yaml`, remove legacy semantic types, delete the hard-coded `code_change` compatibility check, or rewrite historical retained artifacts.

## Strategy Overview

The right-sized strategy is to add a generic descriptor module and manifest compatibility layer beside the existing contract model. Core validation should know only the carrier `kind`, required fields for each carrier, built-in `ref_type` checks for `url` and `git_ref`, retained-byte metadata, inline JSON value rules, and recursive `artifact_set.entries` validation. Legacy `ContractType` values remain accepted and are mapped to descriptors when manifests are written, so existing YAML declarations continue to materialize outputs exactly as before. The descriptor should treat `artifact_type` as opaque workflow-owned metadata, allowing fixtures and future workflows to name new domain artifacts without expanding the daemon enum.

## Alternatives Considered

A full migration from `ContractType` to generic descriptors in workflow YAML was rejected because it would turn a compatibility phase into a dogfood workflow migration and would increase blast radius. Adding new enum cases for each domain artifact was rejected because it preserves the coupling the taxonomy is meant to remove. Writing only a documentation fixture was rejected because acceptance requires runtime manifests to produce descriptors. Rewriting old retained manifests was rejected because descriptor compatibility can be achieved for new writes while old manifests remain decodable through the legacy fields.

## Risks and Countermeasures

The main risk is breaking existing consumers that decode `workflow_contract_outputs` or inspect legacy manifest fields. The countermeasure is to keep those fields unchanged, add descriptor data as an optional additive field, and test old-manifest decoding explicitly. A second risk is accepting descriptors that look generic but are not durable, such as file descriptors without valid hash and byte metadata or URL refs with non-HTTP schemes; targeted negative tests must cover those paths. A third risk is incorrectly mapping domain bundle types to carrier kinds; mapping tests must cover `exec_plan_bundle`, `implementation_pack`, `code_change_bundle`, `code_change`, `artifact[]`, `document.markdown`, `url`, and `git_ref`. A fourth risk is scope creep into YAML migration or historical artifact rewriting; validation must include a pre-publish diff check showing dogfood workflow YAML was not migrated. A fifth risk is accepting descriptor output that works only on the first materialization; validation must include an idempotency check that repeated manifest serialization or repeated output recording for the same run does not duplicate outputs, change artifact bytes, change refs, or mutate dogfood workflow YAML. A sixth risk is hiding docs/helper migration, provider-live behavior, or cache behavior changes inside this compatibility slice; those surfaces stay out of scope unless a future plan explicitly splits and accepts them.

## Scope Boundaries

In scope are a generic artifact descriptor type and JSON codec, recursive descriptor validation, legacy-to-generic descriptor mapping for manifests, output materialization support for file/value/ref/artifact-set descriptors, compatibility aliases for existing legacy fields, tests for old and new manifest shapes, and at least one checked-in fixture demonstrating the target descriptor shape with nested entries. Out of scope are migrating dogfood workflow YAML, removing legacy `ContractType` cases, removing the legacy `code_change` inline-object check, changing retained historical artifacts, browser UI work, docs/helper migrations, provider-live/cache behavior changes, manual browser or live dogfood checks as pre-publish gates, and inventing bundle refs or sha values in this review document.

## Milestones

Milestone 1 establishes the generic descriptor model with focused tests. At the end of this milestone, a new module can encode, decode, and validate descriptors for `file`, `value`, `ref`, and nested `artifact_set` values, and a fixture under `test/fixtures/` demonstrates a target `artifact_set` with a workflow-owned `artifact_type` and nested file/value/ref entries.

Milestone 2 adds legacy manifest compatibility. At the end of this milestone, manifests written by `workflow_contract_manifest` include a generic descriptor for each present named value while preserving all legacy fields, and decoders continue to accept historical manifests that do not have descriptors.

Milestone 3 wires output materialization through the compatibility layer. At the end of this milestone, step-field and step-file retained outputs are tested as separate file-output paths, and structured-output, inline-json, static-url, and static-git-ref outputs all produce the expected descriptor kind while their existing refs, hashes, bytes, media types, diagnostics, and missing-output behavior remain unchanged.

Milestone 4 proves compatibility and closes the gates. At the end of this milestone, legacy workflow YAML declarations still parse unchanged, targeted workflow-run tests exercise contracted output materialization without behavior changes, a custom descriptor with a previously unknown `artifact_type` validates without adding a `ContractType` case, the dogfood workflow YAML files remain unmigrated, idempotency evidence shows repeated manifest work does not duplicate outputs or change retained bytes, and the full Gleam test, format, glinter, and `scherzo_lint` gates pass. No manual browser or live dogfood run is a pre-publish requirement for this additive backend compatibility phase; any operator dogfood run is deferred post-implementation evidence.

## Progress

2026-05-28: Reviewed `docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md`, `src/scherzo/workflow_contract.gleam`, `src/scherzo/workflow_contract_manifest.gleam`, `src/scherzo/workflow_run/contract_io.gleam`, relevant workflow YAML, and existing contract tests; drafted this review document. No implementation code has been changed.

2026-05-28: Incorporated review feedback by adding explicit idempotency and recovery evidence, clarifying that manual browser/live dogfood checks are not pre-publish gates, requiring both step-field and step-file materialization tests, requiring legacy `code_change` compatibility-check evidence, and mirroring docs/helper plus provider-live/cache guardrails in the implementation pack.

## Decision Log

2026-05-28: The implementation should add descriptors beside legacy manifest fields rather than replacing `ManifestValue`, because retained consumers already depend on the legacy shape.

2026-05-28: `artifact_type` should remain an opaque string owned by workflows and validators; Scherzo core should validate carrier shape and built-in reference syntax only.

2026-05-28: Dogfood workflow YAML migration is deferred, so compatibility tests must use the existing legacy declarations and a separate descriptor fixture for the target shape.

2026-05-28: Historical retained artifacts should remain untouched; old manifests without descriptors must keep decoding successfully.

2026-05-28: Manual browser and live dogfood checks are deferred operator evidence, not pre-publish requirements, because automated descriptor, manifest, workflow-run, parser, diff, full-test, format, and lint gates can prove this additive backend compatibility phase without live provider dependencies.

2026-05-28: The legacy `code_change` inline-object compatibility check must remain tested until a future workflow-owned schema migration replaces it; mapping `code_change` to a descriptor kind is not enough acceptance evidence.

2026-05-28: Idempotency and recovery need concrete automated evidence: repeated manifest serialization or repeated output recording for the same run must not duplicate outputs, alter refs, change artifact bytes, or mutate dogfood workflow YAML.

2026-05-28: Docs/helper migrations, provider-live behavior, and cache behavior stay out of scope for this implementation pack unless future implementation discoveries force a separate accepted plan.

## Validation and Acceptance

Pre-publish evidence must include targeted descriptor tests proving that valid `file`, `value`, `ref`, and nested `artifact_set` descriptors decode and validate; invalid descriptors fail for missing required fields, invalid `sha256`, negative `bytes`, invalid or missing `media_type`, bad `url` refs, bad `git_ref` refs, and duplicate nested entry names. The fixture evidence is a checked-in JSON descriptor containing `kind: "artifact_set"`, `artifact_type: "scherzo.exec_plan_bundle.v2"`, retained-byte metadata, and nested entries with at least one file, one value, and one ref.

Pre-publish evidence must include manifest tests proving that legacy fields are still present and unchanged, old manifests without descriptors decode successfully, new manifests with descriptors decode successfully, and the legacy names `exec_plan_bundle`, `implementation_pack`, `code_change_bundle`, `code_change`, `artifact[]`, `document.markdown`, `url`, and `git_ref` map to the expected descriptor kinds or compatibility aliases. Acceptance must also include a legacy `code_change` compatibility-check test on the current contract/materialization path: inline `code_change` JSON containing one of `pr_url`, `branch`, `merge_commit`, or `patch_ref` remains accepted, and inline `code_change` JSON lacking all of those fields remains rejected with the existing failure class.

Pre-publish evidence must include output-materialization tests proving that retained files keep their artifact-store `ref`, `sha256`, `bytes`, and `media_type`; both step-field/final-response markdown and step-file retained outputs produce `kind: file`; inline JSON outputs produce `kind: value`; static URL and git-ref outputs produce `kind: ref`; and required-output failure diagnostics remain the same when a source is absent, failed, truncated, or invalid JSON.

Pre-publish evidence must include an idempotency and recovery test proving that repeated manifest serialization or repeated output recording for the same run leaves the manifest's output names, refs, hashes, byte counts, media types, descriptors, and legacy fields stable; it must not duplicate outputs, change retained artifact bytes, or mutate `.scherzo/workflows/*.yaml`. This evidence may be an automated workflow-run test or a narrower manifest round-trip test if it proves the same invariants.

Pre-publish evidence must include a repository diff check showing `.scherzo/workflows/*.yaml` were not migrated in this phase and a docs/helper inventory showing no `.scherzo/workflows/scripts/*`, provider-facing structured-output schema/helper, or cache/live-provider helper changed. No manual browser or live dogfood run is required before publish; a real operator dogfood check may be performed after implementation as deferred evidence. Final validation must run from the repository root with `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting all commands to exit successfully.

## Rollout, Recovery, and Idempotence

The rollout is additive: new manifests gain descriptors, old manifests remain decodable, and existing legacy fields stay available for rollback or downstream compatibility. If descriptor emission causes trouble, reverting the implementation returns runtime behavior to the legacy manifest shape without requiring retained artifact repair. Re-running output materialization for a recovered run should be idempotent in the same way it is today: it may rewrite that run's current output manifest through the existing checkpoint path, but it must not duplicate outputs, change artifact bytes, alter refs, or mutate dogfood workflow YAML, and the automated acceptance evidence must prove that before publish. No browser, docs/helper, provider-live, cache, or manual live dogfood rollout is required for this phase. A deferred operator dogfood check after implementation may inspect a real retained run manifest, but it is not a pre-publish gate.

## Open Questions and Clarifications Needed

No blocking clarification is needed. Future work must decide when to let workflow YAML declare generic `kind` and `artifact_type` directly, but this additive compatibility phase can prove the descriptor model through runtime manifests and fixtures while keeping legacy declarations unchanged.
