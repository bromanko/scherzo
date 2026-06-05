# LIV-872 robust cleanup ownership and delegation

## Purpose / Big Picture

Operators should be able to ask Scherzo what it can clean, see a dry-run inventory by default, and apply cleanup only when Scherzo can prove the state is owned by the daemon or by a subsystem that exposes an explicit cleanup capability. After implementation, `scherzoctl cleanup --json` should be a safe maintenance inventory, `scherzoctl cleanup --yes --json` should mutate only owned local state or delegated workspace resources, and the scheduled `workspace-cleanup` job should either remove eligible owned workspaces or report actionable retained/failure reasons instead of silently succeeding while leaving them behind.

## Problem Framing and Constraints

Current cleanup is split and misleading. `src/scherzo/ctl.gleam` maps `scherzoctl cleanup` to `src/scherzo/state/local_artifacts.gleam`, which only inventories and applies bounded cleanup for local `.scherzo-state` ledger archives and cleanup tombstones, with transcript cleanup currently reported as unavailable. The dogfood `workflows/dogfood/workspace-cleanup.yaml` job invokes that narrow path with `--yes` against a workspace root, so it can report success without removing eligible retained run workspaces. Separately, `scripts/scherzo-workspace-cleanup` contains a Python directory-scanning helper for local noop/jj runs, but it is not the cleanup authority and should not become a generic filesystem sweeper.

The redesign must keep cleanup ownership-aware. The daemon/core may directly delete only daemon-owned local state after retention, containment, symlink, and tombstone checks. Workspace cleanup must be based on Scherzo run/workspace ownership evidence and delegated through the workspace driver or lifecycle layer. Artifact repositories and task stores must not be mutated by generic cleanup; only local daemon-owned caches or explicit artifact-store/repository capabilities may participate.

## Strategy Overview

Introduce a cleanup-provider model with a common dry-run inventory and apply result contract. The daemon acts as coordinator, not universal deleter: `src/scherzo/cleanup.gleam` should coordinate providers, `src/scherzo/cleanup/local_state.gleam` should adapt verified `.scherzo-state` cleanup, and `src/scherzo/cleanup/workspaces.gleam` should discover eligible run roots from Scherzo ledger/manifest evidence, protect active runs, and delegate removal through existing workspace lifecycle/driver code. Artifact and task providers remain read-only or unavailable unless their owning subsystem exposes a proof-bearing cleanup capability.

The CLI should remain conservative: no mutation without explicit confirmation, JSON output should explain provider, item identity, ownership evidence, safety checks, intended action, result, warnings, and idempotency status, and text output should summarize the same facts for humans. The scheduled workspace cleanup should move to the same provider-backed surface so local dogfood cleanup works reliably without embedding noop/jj-specific assumptions in daemon code. Provider-live, remote-provider cache, browser, and remote tracker behavior should remain unchanged by this plan.

## Alternatives Considered

Expanding `local_artifacts.gleam` to recursively sweep workspace directories is rejected because it would make daemon cleanup responsible for paths it does not own. Keeping only `scripts/scherzo-workspace-cleanup` as the scheduled path is rejected because it is driver-specific, Python-only helper logic that can drift from daemon ownership records. Teaching the daemon about jj/noop internals is rejected because driver assumptions would leak into core. Mutating remote artifact repositories or remote task trackers from `cleanup` is rejected because those systems need explicit owner-provided cleanup capabilities, audit rules, and recovery semantics.

## Risks and Countermeasures

The highest risk is accidental deletion outside owned roots. Countermeasures are path containment, symlink rejection, manifest/run evidence validation, driver delegation, and tests with outside-root sentinels. Active-run deletion is countered by ledger/session active-run protection and negative tests. Provider abstraction drift is countered by small data contracts, fake-provider tests, and requiring every apply result to map back to a dry-run item. Scheduled false success is countered by failing or warning explicitly when a requested provider is unavailable, unsafe, or leaves eligible owned workspaces retained. Remote mutation risk is countered by non-goal tests proving remote task and artifact repositories are not called by generic cleanup. Provider-live and cache-regression risk is countered by treating live providers, remote caches, and browser/UI behavior as unchanged, with tests limited to provider-boundary assertions for local daemon-owned cache/outbox cleanup.

## Scope Boundaries

In scope are `scherzoctl cleanup`, provider coordination in new `src/scherzo/cleanup.gleam`, local-state adaptation around `src/scherzo/state/local_artifacts.gleam` and new `src/scherzo/cleanup/local_state.gleam`, workspace cleanup in new `src/scherzo/cleanup/workspaces.gleam`, workspace lifecycle delegation around `src/scherzo/workspace_run.gleam`, `src/scherzo/workspace_driver_lifecycle.gleam`, and `src/scherzo/workspace_manifest.gleam`, CLI parsing/rendering in `src/scherzo/ctl.gleam`, `src/scherzo/ctl/parser.gleam`, and `src/scherzo/ctl/usage.gleam`, tests under `test/`, the scheduled `workflows/dogfood/workspace-cleanup.yaml` workflow, and the legacy/helper role of `scripts/scherzo-workspace-cleanup`. The plan should preserve current post-run cleanup behavior while making retained-run maintenance use the same ownership evidence and lifecycle delegation.

The helper migration is in scope only as documentation, compatibility, and narrowing: `scripts/scherzo-workspace-cleanup` may remain as a local diagnostic/helper shim, but scheduled dogfood cleanup must stop depending on its noop/jj directory-scan rules as the authoritative cleanup model. Out of scope are generic deletion of arbitrary directories, direct daemon knowledge of jj/noop cleanup rules, remote task-store mutation, remote artifact-repository mutation, browser/UI work, and changing provider-live/cache behavior. Future remote cleanup may be added only by an owning subsystem with an explicit cleanup capability and safety proof.

## Milestones

Milestone 1 inventories and normalizes the current cleanup surfaces. The outcome is a checked-in provider domain model in `src/scherzo/cleanup.gleam` with JSON rendering support and a compatibility path where `scherzoctl cleanup --json` still reports the existing owned `.scherzo-state` decisions through a provider-shaped result. Completion is proven by parser/renderer tests in `test/ctl_test.gleam` and local-state compatibility tests in `test/state_local_artifacts_test.gleam` that show dry-run is the default and `--yes --dry-run` is rejected.

Milestone 2 hardens daemon-owned local state cleanup. The outcome is bounded retention, containment, symlink safety, tombstone/audit behavior, and idempotent apply for local artifacts in the local-state provider, with no workspace or remote repository mutation in this provider. Completion is proven by tests that create eligible ledger archives, retained current ledgers, tombstone-write failures, symlink escapes, already-deleted artifacts, and repeated apply attempts, then assert only verified local artifacts are deleted.

Milestone 3 adds workspace cleanup delegation. The outcome is dry-run inventory of eligible retained run roots from Scherzo ledger and managed-workspace manifest evidence, active-run protection, no directory-sweep fallback, and apply that delegates removals through `workspace_run.cleanup_run` and the workspace driver lifecycle. Completion is proven by tests in `test/workspace_run_test.gleam` and new provider tests that include a manifest-backed eligible run, an active run, an unmanifested directory, a retained marker, a driver remove failure, and an outside-root symlink sentinel that survives cleanup.

Milestone 4 encodes artifact, task-store, provider-live, and cache boundaries. The outcome is explicit unavailable/read-only provider results for artifact repositories and task stores unless an owning local provider exposes cleanup, plus unchanged provider-live/cache behavior. Completion is proven by fake-provider tests showing generic cleanup does not call remote artifact-repository or task-store mutation paths and can only report local daemon-owned cache/outbox cleanup through an owning provider result.

Milestone 5 migrates scheduled dogfood cleanup and helper documentation. The outcome is `workflows/dogfood/workspace-cleanup.yaml` using the provider-backed cleanup surface, surfacing non-zero failure or explicit retained reasons when eligible owned workspaces cannot be removed, and `scripts/scherzo-workspace-cleanup` narrowed to a documented local helper rather than the authoritative cleanup model. Completion is proven by workflow/config tests that inspect the scheduled command and helper tests that either cover the shim behavior or document its deprecation boundary.

Milestone 6 completes validation and operator evidence. The outcome is passing targeted tests, full Gleam tests, format and production lint gates, and pre-publish local dogfood evidence that dry-run, apply, active-run protection, outside-root safety, and idempotent re-run behavior are observable. Completion is proven by saved command output from the full validation gates plus a local temporary-workspace transcript; browser and remote-provider live checks are not pre-publish requirements and may be deferred to a human/operator after implementation if desired.

## Progress

- [x] 2026-06-05: Verified the current cleanup inventory across `scherzoctl cleanup`, `local_artifacts.gleam`, `workflows/dogfood/workspace-cleanup.yaml`, and `scripts/scherzo-workspace-cleanup`.
- [x] 2026-06-05: Authored this concise review document for the LIV-872 implementation handoff.
- [x] 2026-06-05: Incorporated review feedback by making milestones file-specific and evidence-backed, clarifying helper migration, provider-live/cache boundaries, full validation, linting, and pre-publish dogfood evidence.

## Decision Log

- 2026-06-05: Cleanup will be provider/capability based. Rationale: ownership and safety differ between daemon state, workspace drivers, artifact repositories, and task stores.
- 2026-06-05: The daemon/core may directly delete only verified local `.scherzo-state` artifacts. Rationale: this is the state it owns and can audit with path safety and tombstones.
- 2026-06-05: Workspace cleanup must delegate through workspace lifecycle/driver contracts using Scherzo ownership evidence. Rationale: core should not encode noop, jj, or future driver deletion rules.
- 2026-06-05: Remote task and artifact repositories are explicit non-goals for generic cleanup. Rationale: remote mutation requires owner-specific capabilities, authorization, and recovery semantics.
- 2026-06-05: The implementation should use concrete cleanup modules `src/scherzo/cleanup.gleam`, `src/scherzo/cleanup/local_state.gleam`, and `src/scherzo/cleanup/workspaces.gleam`. Rationale: review feedback asked for milestone specificity and implementation-pack alignment rather than leaving module boundaries implicit.
- 2026-06-05: Local dogfood cleanup evidence is pre-publish, while browser and remote-provider live checks are deferred human/operator checks only if desired. Rationale: the change is a local maintenance surface and must not require unrelated live remote or browser infrastructure to ship.

## Validation and Acceptance

Acceptance must be backed by concrete evidence, not just code review. Unit tests should prove `cleanup` defaults to dry-run, `--yes` is required for mutation, `--yes` plus `--dry-run` is rejected, local-state retention and tombstone behavior remain safe, active runs are retained, unsafe paths and symlinks are rejected, provider apply delegates only previously inventoried items, and repeated apply is idempotent. Workspace tests must include an unmanifested directory and an outside-root symlink sentinel that survive cleanup.

The required targeted evidence is: `direnv exec . gleam test test/ctl_test.gleam`, `direnv exec . gleam test test/state_local_artifacts_test.gleam`, `direnv exec . gleam test test/workspace_run_test.gleam`, and the new cleanup-provider test file selected by the implementer, all passing after first demonstrating at least one red/failing assertion for the new behavior. Provider-boundary tests should prove generic cleanup does not call remote artifact-repository or task-store mutation paths, while local daemon-owned cache/outbox cleanup can only run through an owning provider. Scheduled workflow tests should prove `workflows/dogfood/workspace-cleanup.yaml` invokes the provider-backed cleanup path and no longer treats unavailable or unsafe workspace cleanup as an unqualified success. Helper migration tests must prove `scripts/scherzo-workspace-cleanup` is no longer the scheduled authority or is explicitly documented as a local helper shim.

Before publish, run from the repository root: `direnv exec . gleam test`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting zero exits. Also collect pre-publish local dogfood evidence with a temporary workspace root: dry-run shows an eligible owned workspace without deleting it, apply removes it through delegation, active and unsafe candidates remain, an outside sentinel remains, and a second apply reports no harmful duplicate deletion. No browser or remote-provider live evidence is required before publish; if an operator later wants extra live reassurance, that check is a deferred human/operator dogfood check and not an implementation-completion gate.

## Rollout, Recovery, and Idempotence

Roll out additively by keeping cleanup dry-run by default and preserving the existing local-state behavior while adding providers behind explicit inventory/apply contracts. If the workspace provider cannot prove ownership or delegate safely, it must retain the run root and report why. Reverting the implementation should restore the previous narrow cleanup path without requiring data migration; any added provider inventory metadata or tombstones are local audit artifacts.

The scheduled workflow migration should be treated as a normal code change, not a data migration: update `workflows/dogfood/workspace-cleanup.yaml` only after provider-backed CLI behavior is tested, keep `scripts/scherzo-workspace-cleanup` available as a compatibility helper until tests and docs confirm its narrowed role, and fail loudly when workspace cleanup is unavailable rather than reporting success with retained eligible workspaces. Provider-live, cache, browser, remote artifact repository, and remote tracker behavior should remain untouched so rollback does not require remote state repair.

Apply operations must be safely repeatable. Missing already-deleted local artifacts should be reported as idempotent retained/deleted results rather than fatal surprises, workspace delegation should tolerate already-removed paths, and scheduled cleanup should be safe to retry after partial failure because every mutation is scoped to an owned item from a dry-run inventory.

## Open Questions and Clarifications Needed

No open questions. The concrete module names, ownership boundaries, provider dry-run/apply contract, workspace delegation requirement, helper migration boundary, validation gates, and remote mutation non-goals are specified above.
