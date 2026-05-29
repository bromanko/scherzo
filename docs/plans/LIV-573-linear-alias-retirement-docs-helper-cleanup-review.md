# LIV-573 Linear Alias Retirement and Docs Helper Cleanup

## Purpose / Big Picture

This review plan prepares a safe, evidence-backed cleanup of Scherzo's remaining Linear-named operator docs, workflow helpers, and compatibility aliases after the tracker-neutral runtime migration is materially complete. The intended outcome is that new operators learn task/tracker-neutral commands first, while existing Linear deployments, prompt templates, helper invocations, and retained ledgers continue to work until each alias has an explicit retirement path and observable acceptance evidence.

## Problem Framing and Constraints

Scherzo now describes runtime work as tracker tasks, but Linear remains the only production adapter and many public surfaces still use Linear or issue vocabulary. The cleanup must not remove a Linear name merely because a neutral model exists: old ledgers, old run artifacts, checked-in prompts, operator scripts, and Linear-only helpers still need readable compatibility. Review feedback tightened the bar for this plan: milestones must name the files and evidence they produce, manual and live dogfood checks must say whether they are pre-publish or deferred operator evidence, and the implementation pack must carry the mechanical validation commands rather than leaving them implicit in this human-readable review document.

## Strategy Overview

Treat the cleanup as an inventory and policy change before code removal. Classify names as follows: `linear-smoke`, `linear-contract`, `--linear-smoke`, and `--linear-contract-check` are temporary aliases for `tracker-smoke` and `tracker-contract`; the broader `--linear-*` family splits into those temporary aliases plus Linear-only helper options such as `--linear-attach-comment-file`; `linear_commands` and external `linear_contract` config are retirement candidates already rejected by simplified config diagnostics; internal `LinearContractConfig` and Linear GraphQL helper code are Linear-adapter internals until generic readiness/check setup fully replaces them; `--linear-project-slug`, Linear CLI calls, and helper commands that fetch Linear issues are Linear-only helper surfaces, not generic contracts; `issue.*`, `SCHERZO_ISSUE_*`, `issue_id`, `issue_identifier`, legacy ledger fields, and `linear_command_*` ledger/event names are permanent compatibility surfaces for reads that must stay readable, with new writes moving to task names only after dual-read tests exist.

The review document states intent, scope, risk, rollout, and acceptance. The paired implementation pack supplies the step-by-step mechanics: inventory commands, targeted docs/helper edits, focused tests, full validation, lint gates, review-lane cache/provider guardrails, and the deferred operator/live dogfood checklist. Documentation should prefer task/tracker wording and explicitly label any retained Linear name by class.

## Alternatives Considered

One alternative is to rename every Linear- or issue-shaped surface immediately. That would make the tree look cleaner but risks breaking existing deployments, prompt templates, old ledgers, recovery paths, and helper scripts. Another alternative is to leave the names indefinitely without policy. That is safer short term but keeps confusing new adapter work. A third alternative is to refactor the Linear helper fetch/publish paths into a generic adapter API in this cleanup. That is too large for this slice because the adapter-backed task-context fetch does not exist yet. The chosen approach is a staged compatibility policy with greppable inventories, tests, and explicit removal gates.

## Risks and Countermeasures

The main risk is accidental breakage of live Linear dogfood deployments. Countermeasure: keep Linear aliases accepted until validation proves neutral names are documented, aliases are tested, and old ledgers decode. A second risk is hiding true Linear-only behavior behind generic docs. Countermeasure: label Linear-only helper options as adapter-internal or adapter-specific instead of renaming them prematurely.

A third risk is helper churn affecting review-lane provider-live preflight or cache behavior. Countermeasure: if workflow helpers or review helpers are touched, run the offline review-lane contract before publish, record `remote_mutations: "none"`, and either run the live provider canary with credentials or record an explicit skipped/operator-deferred reason. A fourth risk is acceptance drift between this review document and the implementation pack. Countermeasure: keep this document free of mechanical implementation sections, but mirror every acceptance obligation in the structured implementation pack's concrete steps and testing text before materializing the bundle.

## Scope Boundaries

In scope are `README.md`, `.scherzo/README.md`, `docs/GETTING_STARTED.md`, `docs/runbooks/tracker-adapters.md`, `docs/specs/TRACKER_ADAPTER_SPEC.md`, dogfood prompt wording under `workflows/dogfood/prompts/`, `workflows/dogfood/scripts/scherzo-execplan`, `workflows/dogfood/scripts/scherzo-implementation`, related helper tests, and config/CLI tests that preserve alias behavior. The docs/helper migration in this slice is behavior-preserving: generic operator instructions move to task/tracker wording, retained Linear-only helper commands are labeled as Linear-adapter-specific, and helper scripts change only when a neutral wrapper, clearer diagnostic, or test fixture update can preserve old environment variables and invocation shapes.

Out of scope are adding Jira or Trello production support, removing Linear as the production adapter, changing the Linear board contract itself, deleting retained ledger compatibility readers, replacing `linear` CLI use inside helpers before an adapter-backed task-context fetch exists, or making live provider credentials a mandatory local pre-publish requirement for every documentation-only edit.

## Milestones

Milestone 1 produces a checked, greppable alias inventory. The implementer uses repository grep over `README.md`, `.scherzo/README.md`, `docs/GETTING_STARTED.md`, `docs/runbooks/`, `docs/specs/`, `workflows/dogfood/prompts/`, `workflows/dogfood/scripts/`, `src/`, and `test/` to classify `linear-smoke`, `linear-contract`, `--linear-smoke`, `--linear-contract-check`, `linear_contract`, `linear_commands`, `linear_command_*`, `SCHERZO_ISSUE_*`, `issue_id`, `issue_identifier`, and Linear-only helper options. The milestone is accepted when `docs/runbooks/tracker-adapters.md` or an equivalent current-docs location records one class and one retirement condition for each retained surface.

Milestone 2 updates operator-facing docs and dogfood prompt wording. At the end, `README.md`, `.scherzo/README.md`, `docs/GETTING_STARTED.md`, tracker-adapter runbooks/specs, and touched `workflows/dogfood/prompts/` examples prefer `task`, `tracker-smoke`, `tracker-contract`, `--tracker-smoke`, and `--tracker-contract-check`; any retained Linear command or environment variable is described as a compatibility alias or Linear-adapter-specific helper. The observable evidence is a grep transcript showing that generic instructions no longer present Linear names as the preferred path.

Milestone 3 applies only behavior-preserving helper migration. At the end, any edits to `workflows/dogfood/scripts/scherzo-execplan` or `workflows/dogfood/scripts/scherzo-implementation` preserve `SCHERZO_ISSUE_*`, Linear issue-context fallback, retained-run recovery, and existing fixture paths while improving neutral wording or diagnostics. If no helper code needs to change after the inventory, this milestone records that no helper migration was necessary and the helper tests remain unchanged.

Milestone 4 adds focused compatibility and negative-path tests. At the end, tests cover preferred tracker command names, Linear alias acceptance, simplified-config diagnostics for removed `linear_contract` or `linear_commands` inputs, old-ledger and `linear_command_*` decoding, and helper compatibility for Linear-backed issue context. The evidence is deterministic test output from `direnv exec . gleam test` plus `direnv exec . scherzo-test-contract` when helper scripts, workflow YAML, review-lane helpers, or shell-heavy fixtures changed.

Milestone 5 performs pre-publish validation and records deferred operator evidence. Pre-publish evidence includes review-doc validation, the alias inventory grep, focused docs/helper evidence, `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If review-lane provider-live or cache behavior is touched, pre-publish evidence also includes the offline review-lane contract report with `remote_mutations: "none"`; the live provider canary and real Linear dogfood checks are deferred human/operator checks unless credentials are available and the PR explicitly opts into running them before publish.

## Progress

- [x] 2026-05-29 Drafted the LIV-573 review plan and implementation-pack handoff guidance.
- [x] 2026-05-29 Incorporated review feedback by making milestones evidence-specific, separating pre-publish checks from deferred live dogfood/operator checks, and requiring the implementation pack to mirror validation, lint, helper migration, provider/cache, and manual-evidence obligations.

## Decision Log

- Decision: Do not retire `issue.*`, `SCHERZO_ISSUE_*`, or legacy ledger/event fields in the first cleanup slice.
  Rationale: They are durable compatibility surfaces for old prompts, workspace drivers, recovery, and retained ledgers; neutral replacements need dual-read evidence first.
  Date: 2026-05-29.
- Decision: Treat `--linear-smoke` and `--linear-contract-check` as temporary aliases, but treat Linear issue-fetching helper behavior as Linear-adapter-specific until adapter-backed helper APIs exist.
  Rationale: Smoke/contract have neutral equivalents today; helper fetch/publish paths still perform real Linear operations.
  Date: 2026-05-29.
- Decision: Keep live Linear dogfood and live provider canaries as explicit human/operator evidence unless the implementation environment has credentials and the PR opts into pre-publish execution.
  Rationale: The cleanup must be locally verifiable without credentials while still documenting how operators prove the change against live services. Offline review-lane contract evidence remains pre-publish when review-lane provider/cache code changes.
  Date: 2026-05-29.
- Decision: Put mechanical implementation commands in the structured implementation pack, not as level-2 sections in this review document.
  Rationale: The workflow validator reserves detailed steps, tests, interfaces, and artifact notes for `implementation_pack.sections`; this document should stay human-reviewable while the pack remains machine-validated.
  Date: 2026-05-29.

## Validation and Acceptance

Acceptance evidence must include `direnv exec . .scherzo/workflows/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-573-linear-alias-retirement-docs-helper-cleanup-review.md` exiting zero after every revision. It must include a repository grep transcript proving every required Linear or issue-shaped name is either removed from generic instructions or classified where retained. The grep scope should cover current operator docs, tracker specs/runbooks, dogfood prompts, dogfood helpers, `src/`, and `test/`, while excluding historical plans except this review document.

Implementation acceptance also requires deterministic tests and lint gates: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` must pass before publish. If helper scripts, workflow YAML, review-lane helper code, or shell-heavy fixtures changed, `direnv exec . scherzo-test-contract` must pass before publish. If review-lane provider-live or cache behavior changed, the offline command `direnv exec . .scherzo/workflows/scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/execplan.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/review-lane-contract` must pass and its report must show `remote_mutations: "none"`.

Manual and dogfood checks are not silent TODOs. Real Linear smoke/contract runs and live provider canaries are deferred post-implementation human/operator checks unless credentials are available during implementation and the PR explicitly records them as pre-publish evidence. When deferred, the PR must record the skipped reason and the exact commands an operator should run, including `LINEAR_API_KEY=... direnv exec . scherzo doctor --check tracker-contract --check tracker-smoke .scherzo/scherzo.yaml` for live Linear dogfood and the review-lane live canary command when provider credentials are available.

## Rollout, Recovery, and Idempotence

Rollout should be additive first: publish neutral docs and tests while leaving existing Linear aliases and legacy readers in place. Any later retirement must have a release note, a diagnostic that names the neutral replacement, and a rollback path that restores alias acceptance without rewriting old ledgers. Re-running the cleanup is idempotent because the inventory is grep-based, docs edits are textual, helper changes preserve old environment variables, and validation commands can be repeated safely after each review revision.

If a docs-only edit causes confusion, recovery is a follow-up docs patch restoring the compatibility note. If a helper migration breaks retained-run recovery or Linear issue-context fallback, revert the helper change while keeping the docs classification and tests that exposed the break. If provider-live/cache preflight evidence changes unexpectedly, stop before publish, retain the offline contract report, and either repair the cache/provider change or remove it from this cleanup slice.

## Open Questions and Clarifications Needed

No blocking questions. Before actual alias removal, choose the minimum compatibility window after tracker-neutral task context is fully available and decide whether CLI aliases should warn for one release before becoming errors.
