# LIV-1442 daemon shared capabilities review

This review defines a behavior-preserving implementation plan for replacing daemon-local closure context builders with shared capability records. It is intentionally concise; concrete edit steps, test details, interfaces, and artifact notes are carried in the structured implementation pack for this issue.

## Purpose / Big Picture

Scherzo's daemon should read as the public actor shell instead of a factory for dozens of one-off closure records. After the follow-up implementation, extracted orchestrator subsystems should receive a small set of stable capabilities for time, logging, session events, ledger appends, side-effect enqueueing, and timers, while operators observe the same dispatch, worker, scheduled-run, YAML workflow, recovery, and control-command behavior. The visible payoff is a lower daemon line-count ratchet, fewer daemon-boundary exceptions, and tests that still prove the daemon works through its public APIs.

## Problem Framing and Constraints

`src/scherzo/orchestrator/daemon.gleam` currently has more than ten thousand lines and remains difficult to shrink because extracted modules are parameterized over daemon `State` and are fed bespoke records of closures. Gleam has no interface feature, so records of functions remain the right tool; the problem is that the records are too granular and rebuilt at call sites. The implementation must keep `RuntimeDependencies` as the public test and composition-root seam, must not require a durable ledger, protocol, workflow YAML, EventHub, Linear, or UI behavior change, and must keep the repository compiling and passing tests at every migration milestone. This root-cause seam work should land before any plan that clusters daemon `State` into owned sub-records; whichever branch lands second must rebase over the capability names and updated daemon-boundary guardrails.

## Strategy Overview

Add `src/scherzo/orchestrator/daemon_capabilities.gleam` and build one `DaemonCapabilities(State, Message, TimerHandle)` value during daemon startup after the workflow, event hub, control plane, and effect runner are available. The first records are `Clock`, `Logger`, `EventPublisher`, `LedgerWriter(State)`, `EffectQueue(State)`, and `Timers(Message, TimerHandle)`. The `State` parameter is allowed only for shared functions that return an updated daemon state, such as ledger projection updates and side-effect enqueueing; it must not become a subsystem-specific escape hatch. `RuntimeDependencies` remains where tests and production startup provide raw functions, but subsystem boundaries receive the shared capabilities rather than fresh closures derived from `state.dependencies`.

Migration then proceeds one owner module at a time. Each milestone removes a family of daemon `*_context`, `*_dependencies`, or `*_handlers` builders, replaces it with capability-threaded functions or explicit input/output records, updates tests, and lowers the daemon-boundary ratchet only after validation is green. The plan is intentionally adapter-first: capabilities are introduced without changing behavior, then individual subsystems stop depending on bespoke contexts.

## Alternatives Considered

Leaving the current closure builders in place and only moving more behavior out of the daemon was rejected because every extraction would continue to add adapter code back into `daemon.gleam`. Passing `RuntimeDependencies` directly into subsystems was rejected because it is a service locator with startup-only concerns mixed with runtime capabilities. Rewriting the daemon `State` shape first was rejected because that work depends on stable subsystem seams and would make conflicts larger. Replacing records of functions with a class-like abstraction is not available in Gleam and would not fit the repository.

## Risks and Countermeasures

The main risk is accidentally changing runtime behavior while moving wiring. The countermeasure is one subsystem per commit, characterization tests before removing builders, and full daemon tests after each milestone. Those characterization tests must cover both success and negative/error paths before each migrated builder is deleted: worker spawn failures, worker DOWN handling, scheduled failure-report retry and exhaustion, operator rejection or not-found responses, YAML step errors, effect-runner errors, and duplicate or idempotent timer, ledger, and side-effect paths where a migrated subsystem can be invoked repeatedly.

A second risk is replacing many small records with one new god record. The countermeasure is to keep capabilities narrow by effect family, not by subsystem, and to reject fields that are only used by one call path unless they are daemon-owned shell actions. A third risk is losing testability. The countermeasure is to keep `RuntimeDependencies` as the public fake seam and add helper constructors so tests can override capabilities through existing dependency overrides. A fourth risk is documentation and guardrails drifting. The countermeasure is to update `docs/architecture/daemon-boundary.md`, `test/orchestrator_daemon_boundary_test.gleam`, and `test/source_guardrail_test.gleam` in the same milestone that removes an exception or lowers a ratchet.

## Scope Boundaries

For this planning issue, scope is exactly this Markdown review document and the structured implementation-pack submission. No production capability module or generated canonical bundle should be written by this issue.

For the follow-up implementation, in scope are daemon runtime capability records, migration of `worker_lifecycle`, `daemon_transition_shell`, `scheduled_runtime`, `yaml_workflow_lifecycle`, `operator_runtime`, and effect completion handling away from bespoke closure contexts, test updates for every daemon-importing test module, and daemon-boundary documentation/ratchet updates. Out of scope are public protocol changes, ledger schema changes, workflow YAML semantics, Linear adapter behavior changes, UI/browser work, provider-live behavior, cache behavior, cluster `State` sub-record ownership, and transition-state unification beyond mechanical conflict resolution.

## Milestones

Milestone 1 introduces the shared capability module and stores a startup-built capability value in daemon state. Expected daemon line-count change is neutral to plus 80 lines because this milestone adds the seam before deleting builders.

Milestone 2 migrates `worker_lifecycle` spawn, update, finish, down, and scheduled-worker helpers off their bespoke context records. Expected daemon reduction is 350 to 500 lines, primarily from deleting `worker_spawn_context`, scheduled worker context builders, and worker update/down builders.

Milestone 3 migrates `daemon_transition_shell` to shared capabilities and a smaller stable shell boundary. Expected daemon reduction is 120 to 220 lines by deleting the current large `transition_shell_handlers` factory and moving common now/log/ledger/effect/timer wiring behind capabilities.

Milestone 4 migrates scheduled runtime timer, retry, and failure-report wiring. Expected daemon reduction is 150 to 300 lines as scheduled retry/resumption and failure-report builders shrink or move out of the daemon.

Milestone 5 migrates YAML workflow lifecycle and operator-runtime seams. Expected daemon reduction is 80 to 160 lines by replacing YAML callback/dependency builders and operator command closure plumbing with shared capabilities plus explicit command inputs.

Milestone 6 migrates effect completion handling and removes remaining capability-shaped closure builders, then updates daemon-boundary docs and ratchets. Expected daemon reduction is 80 to 160 lines, with final target reduction of roughly 800 to 1,300 lines from the current ratchet while preserving behavior.

## Progress

- [x] (2026-07-08) Read the repo-local ExecPlan workflow guidance and the prepared output target.
- [x] (2026-07-08) Re-inventoried the implementation branch and confirmed the prepared review doc still validates.
- [x] (2026-07-08) Introduced `src/scherzo/orchestrator/daemon_capabilities.gleam` plus daemon `State.capabilities` wiring as the Milestone 1 shared seam.
- [x] (2026-07-08) Added `test/orchestrator_daemon_capabilities_test.gleam` coverage for fake clock, logger, timers, ledger, and effect-queue capabilities.
- [x] (2026-07-08) Migrated worker and scheduled-worker lifecycle contexts to carry shared `DaemonCapabilities` instead of individual clock/logger/events/ledger fields.
- [x] (2026-07-08) Migrated transition-shell now/log/ledger wiring to shared capabilities and reduced daemon transition-shell assembly to daemon-owned callbacks.
- [x] (2026-07-08) Migrated YAML workflow lifecycle dependency construction to shared capabilities while leaving only daemon-owned step routing in the actor shell.
- [x] (2026-07-08) Migrated scheduled retry timers and scheduled failure ledger appends through shared timer/ledger capabilities.
- [x] (2026-07-08) Migrated effect-completion crash logging through shared logger capabilities and kept only explicit daemon result routes.
- [x] (2026-07-08) Renamed operator runtime route construction to reflect daemon-owned command routes rather than generic shell capability plumbing.
- [x] (2026-07-08) Lowered the daemon ratchet to the validated `10_694` line count and updated boundary/source guardrails.

## Surprises & Discoveries

The target branch had already drifted from the planning baseline before edits: `python workflows/dogfood/scripts/scherzo-execplan validate-review-doc --path docs/plans/LIV-1442-daemon-shared-capabilities-review.md` still reports `REVIEW_DOC_VALID=ok`, but `wc -l src/scherzo/orchestrator/daemon.gleam` initially reported 10,889 lines rather than the 10,869 lines captured in the handoff, and direct daemon test imports also differed from the earlier inventory.

After the seam-first milestone, fake-adapter daemon tests exposed a startup-recovery readiness race. The tests now wait for startup recovery readiness after `daemon.start`, preserving behavior while making the adapter assertions deterministic.

The transition-shell capability migration moved generic now/log/ledger mechanics out of the daemon, but it necessarily raised the `daemon_transition_shell.gleam` source-guardrail baseline because the capability adapter now owns ledger-record conversion and capability-backed interpreter callbacks.

Final local validation for the implementation workspace is green: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` completed successfully.

## Decision Log

- Decision: Name the new shared module `src/scherzo/orchestrator/daemon_capabilities.gleam`. Rationale: the capabilities are daemon-owned runtime seams, not domain state models, and the name avoids colliding with existing `event_publisher.gleam` or effect modules. Date: 2026-07-08.
- Decision: Keep `RuntimeDependencies` as the public composition-root and fake seam while preventing it from crossing subsystem boundaries. Rationale: current daemon tests already override `RuntimeDependencies`, and preserving that API avoids a large unrelated test rewrite. Date: 2026-07-08.
- Decision: Migrate one owner module per milestone and lower ratchets only after validation. Rationale: this is a behavior-preserving refactor with high merge-conflict risk, so small green commits are safer than a single broad rewrite. Date: 2026-07-08.
- Decision: Treat negative/error-path behavior and duplicate/idempotent timer, ledger, and side-effect handling as acceptance-critical for each migrated seam. Rationale: shared capabilities centralize effect wiring, so a green happy-path refactor could still regress crash, retry, rejection, or duplicate suppression behavior. Date: 2026-07-08.
- Decision: Parameterize `DaemonCapabilities` by daemon `State`, `Message`, and `TimerHandle`, while allowing `State` only on effect-family functions that already return an updated state. Rationale: ledger projection updates and side-effect enqueueing currently preserve stateful daemon invariants, and a constrained shared state parameter is safer than inventing parallel projection ownership during this refactor. Date: 2026-07-08.

## Outcomes & Retrospective

The behavior-preserving capability refactor now has a green local validation story in the retained LIV-1454 implementation workspace. `RuntimeDependencies` remains the public composition-root/test seam, while daemon-owned runtime effects flow through `DaemonCapabilities(State, Message, TimerHandle)` at migrated subsystem boundaries. Worker lifecycle, transition shell, scheduled timer/ledger paths, YAML workflow dependency construction, operator command route construction, and effect-completion crash logging no longer depend on bespoke daemon capability records for clock/logger/events/ledger/timer plumbing.

The validated daemon line count is 10,694 lines, and the daemon-boundary/source guardrails were lowered to that value. The transition-shell module grew because it now owns the capability-backed interpreter adapter for now/log/ledger behavior; this is recorded in the source guardrail as the extracted owner of that wiring.

## Validation and Acceptance

This planning issue is accepted when this document exists at `docs/plans/LIV-1442-daemon-shared-capabilities-review.md`, every required section is present and non-empty, and Scherzo captures the structured implementation-pack submission for LIV-1442.

The follow-up implementation is accepted only when the capability records exist, the named subsystem migrations are complete, deleted daemon builders are reflected in boundary guardrails, expected line-count reductions are recorded, and characterization coverage proves parity for success paths, negative/error paths, and duplicate or idempotent timer, ledger, and side-effect behavior in each migrated subsystem. The required validation commands from the repository root are `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands through direnv.

## Rollout, Recovery, and Idempotence

Rollout for this planning issue is limited to adding this review document and submitting the implementation pack; Scherzo owns bundle materialization. The follow-up implementation should roll out as green commits by milestone, with no data migration and no operator-visible rollout switch. Recovery is to revert the most recent milestone while retaining characterization tests that still describe current behavior; do not lower line-count ratchets or remove boundary exceptions until the migrated milestone is green. Re-running tests, capability construction, and doc validation is idempotent because the work changes code structure rather than stored data, and migrated timer, ledger, and side-effect paths must explicitly prove repeated or duplicate invocations do not create extra observable effects.

## Open Questions and Clarifications Needed

No open question blocks implementation handoff. If the follow-up discovers that eliminating a builder requires a public protocol change, durable schema change, or a much broader `State` ownership refactor, stop and split that work into a separate plan instead of expanding this migration.
