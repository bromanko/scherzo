# Implement Scherzo control timeout policy

This ExecPlan review is the concise human-facing plan for LIV-1341. The structured implementation pack captured with this plan carries the mechanical file-by-file steps; this document keeps the problem, strategy, risks, milestones, and acceptance evidence reviewable.

## Purpose / Big Picture

Scherzo operators should be able to tell whether a control command reached the daemon, which phase consumed the timeout budget, and what safe command to run next. After the later implementation, `scherzoctl` and daemon-backed `scherzo ctl` commands report phase-labeled, retry-aware, operator-actionable timeouts; read commands prefer bounded stale-marked data when useful data exists; and long-running mutating commands return accepted operation ids promptly unless the operator explicitly asks to wait.

## Problem Framing and Constraints

Local operator history shows that `scherzoctl` failures are not dominant per call, but they affect a large fraction of operator sessions. The recurring pain is ambiguity: connection failures, daemon startup races, opaque `ping`/`ps`/`resume`/`operation-status` timeouts, fragile JSON piping, stale CLI spellings, and recovery-command confusion leave operators unable to distinguish not accepted, accepted and still running, and failed work.

The implementation must flow from `docs/specs/SCHERZO_CTL_TIMEOUT_POLICY.md`, not from isolated timeout bumps. It must preserve mutating recovery safety checks, retained-state guards, active-run checks, retry-step artifact checks, control-file and token redaction, and JSON secrecy. It must treat stdout/stderr and response-shape changes as compatibility-sensitive protocol changes, while avoiding stored-data migration.

## Strategy Overview

Add a shared control-timeout vocabulary and renderer, then migrate command surfaces in risk order. The stable timeout phases are `cli_bootstrap`, `control_file_discovery`, `daemon_connect`, `request_round_trip`, `daemon_actor_query`, `operation_admission`, `operation_wait`, `command_step_watchdog`, and `external_api`. Every JSON timeout response must include `phase`, `timeout_ms`, `retryable`, and an acceptance value of `true`, `false`, or `unknown`, plus a safe suggested next command when Scherzo can provide one.

The sequence starts by inventorying current behavior and adding tests that expose broken JSON or opaque timeout paths. It then introduces reusable duration parsing, effective timeout settings from `--timeout`, `SCHERZO_CTL_TIMEOUT`, and `SCHERZO_CTL_WAIT_TIMEOUT`, JSON-only stdout helpers, and timeout error types. Read-only commands receive the first behavior change because they are safe to repeat and give operators reliable diagnostics. Mutating commands then adopt admission-versus-completion semantics, explicit `--wait`, and unknown-acceptance guidance without weakening existing guards.

## Alternatives Considered

Raising hard-coded timeouts was rejected because it hides daemon startup races and makes ambiguous mutation receipt last longer instead of safer. Fixing only `operation-status` was rejected because JSON pollution, daemon connection failures, and mutation acknowledgement ambiguity are part of the same operator contract. Replacing the control protocol wholesale was rejected as disproportionate; the existing protocol, command-result operation ids, query service, and duration parser can be extended additively.

## Risks and Countermeasures

The main risk is a breaking protocol surprise for scripts that parse `--json` output. The countermeasure is additive fields where possible, direct stdout parse tests, clear documentation, and rollout notes that call out stdout/stderr changes. A second risk is making mutating commands look retryable after the daemon may have received them; the countermeasure is explicit `accepted: "unknown"`, `retryable: false`, stable operation or client request ids where needed, and suggested next commands that are safe reads rather than blind retries. A third risk is hiding real daemon failures behind aggressive connect retry; the countermeasure is bounded backoff, phase-specific errors for missing control files, refusal, bad responses, and timeouts, and tests for each branch. A fourth risk is leaking control tokens or prompts in diagnostics; the countermeasure is redaction tests for JSON, stderr, logs, and suggested commands. A fifth risk is letting review feedback live only in this prose while the implementation pack omits acceptance evidence, milestone-specific test obligations, dogfood timing, docs/helper boundaries, provider-live/cache boundaries, full validation, or lint gates; the countermeasure is to mirror each of those obligations in the structured implementation pack before Scherzo materializes the follow-up implementation artifacts.

## Scope Boundaries

In scope are `scripts/scherzoctl`, top-level `scherzo ctl ...`, daemon-backed read commands including `ping`, `ps`, `session`, `events`, and `query operation-status`, mutating controls including pause, resume, retry, retry-step, recollect, publication retry, run finalize, cleanup, abort, prompt, UI response, park/unpark, and schedule/work-item controls where practical, plus command help, docs, runbooks, helper text, tests, and timeout observability. The plan should report workflow command-step watchdog and external API timeouts through the shared phase vocabulary when those timeouts surface through Scherzo operator outputs.

Docs/helper migration scope is limited to the operator-facing docs, runbooks, CLI help, and test helpers needed to prove this timeout policy. The implementation should not migrate unrelated workflow helpers, workflow schemas, provider-facing structured-output helpers, or review-lane contract files. If a directly required change touches `.scherzo/workflows/scripts/*`, `.scherzo/workflows/schemas/*`, `workflows/dogfood/scripts/*`, or provider-facing helper contracts, the implementation must add a helper inventory, run the relevant helper/schema tests, and record why provider-live/cache behavior stayed compatible.

Out of scope are unrelated workflow DAG redesign, tracker/provider redesign, browser UI redesign, changing retained-state safety policy, relaxing recovery validation, adding new provider-live requirements, adding new remote-provider cache semantics, changing cache TTL or invalidation policy, and splitting the implementation into multiple follow-up tickets. Existing local projections or caches may be used only for bounded stale-marked read results when the inventory proves that data is safe; if no safe cached data exists, the command should return the policy timeout shape instead of inventing stale data. Stored data migration is not expected; response shapes and stdout/stderr behavior are the compatibility-sensitive parts.

## Milestones

Milestone 1 creates an evidence-backed inventory of current timeout and JSON behavior. Reviewers should see a map of hard-coded waits, daemon actor waits, command-result waits, `--json` stdout surfaces, docs/helper surfaces, and existing projection/cache candidates for stale reads, with baseline tests or transcripts that prove the current failure modes before behavior changes land.

Milestone 2 introduces central timeout policy primitives. Reviewers should see shared phase names, accepted-state values, duration parsing and effective timeout settings, client validation errors for invalid durations, one JSON timeout renderer used by control CLI and daemon responses, and focused tests for valid, invalid, env-default, and override cases.

Milestone 3 makes `--json` stdout reliable. Reviewers should see tests that parse stdout as exactly one JSON document for Scherzo-controlled non-streaming JSON commands, with logs, warnings, progress, and deprecated-alias hints on stderr, plus redaction assertions for token-bearing paths and prompts.

Milestone 4 applies consistent `--timeout` and daemon-connect behavior first to safe reads. Reviewers should see bounded connect retry/backoff, phase-specific missing-control/refused/bad-response/timeout failures, and policy-compliant JSON/human output for `ping`, `ps`, `session`, `events`, and `query operation-status`.

Milestone 5 fixes read-query timeout behavior. Reviewers should see `daemon_actor_query` responses instead of opaque query timeouts, `operation-status --wait --timeout <duration>` for long waits, and stale-marked successful data only where existing projections or caches can safely provide it; reviewers should also see explicit evidence for read surfaces that cannot safely return stale data.

Milestone 6 completes admission-versus-completion semantics for mutating controls. Reviewers should see long-running mutations return queued or applied admission responses with `operation_id` when applicable, `--wait` use `operation_wait`, unknown-acceptance timeout responses guide operators to rediscovery through safe reads, and duplicate/idempotency tests prove that unsafe blind retries are not introduced.

Milestone 7 adds observability, docs, helper inventory, and operator documentation. Reviewers should see timeout counts and durations by phase where Scherzo already records operational metrics, updated help and runbooks that teach accepted/not-accepted/unknown semantics, examples that avoid fragile JSON piping assumptions, a docs/helper migration inventory, and a provider-live/cache note that either proves no behavior changed or names the targeted tests that cover a directly required change.

Milestone 8 completes full validation and rollout evidence. Reviewers should see targeted tests for every timeout phase touched, negative/error-path tests, idempotency and duplicate-conflict checks, JSON-only stdout tests, documentation/helper evidence, a pre-publish local or fake-daemon dogfood transcript for one read timeout and one accepted operation wait timeout, any explicitly deferred human/operator live-daemon check with commands and expected redacted output, full formatting, full tests, and production lint gates passing before publish.

## Progress

- [x] (2026-07-02 01:47Z) Read `.scherzo/workflows/guidance/exec-plan.md`, `docs/specs/SCHERZO_CTL_TIMEOUT_POLICY.md`, current control CLI/client/server/query files, existing timeout-related tests, and nearby plans.
- [x] (2026-07-02 01:47Z) Authored this concise review document for `docs/plans/LIV-1341-control-timeout-policy-implementation.md` and prepared the structured implementation pack for handoff.
- [x] (2026-07-02 02:04Z) Incorporated review feedback by making acceptance evidence, milestone specificity, test obligations, pre-publish local/fake-daemon dogfood, deferred human/operator live-daemon checks, docs/helper inventory, provider-live/cache boundaries, full validation, and linting explicit in this document and in the updated structured implementation-pack submission.

## Surprises & Discoveries

The repository already has a reusable duration parser in `src/scherzo/duration.gleam`, so CLI timeout parsing should reuse that behavior rather than inventing a second grammar. The current control client uses a hard-coded 5000 ms transport timeout for connect/send/read paths, while operator command and query requests derive longer waits from `control.command_timeout_ms` plus a grace period. The query runtime currently has an especially small projection snapshot wait for `operation-status`, which matches the policy concern about too-tight opaque read-query timeouts.

The current command-result type already carries an optional `operation_id`, and retry-step, recollect, publication retry, and run-finalize documentation already describe queued operations. The policy implementation can build on that work instead of designing a new operation lifecycle.

## Decision Log

- Decision: Implement one central policy layer before changing individual commands. Rationale: The operator problem is ambiguity across phases, JSON shape, retryability, and acceptance state; isolated timeout increases would not answer the three required questions. Date: 2026-07-02.
- Decision: Migrate read commands before mutating commands. Rationale: Reads are safe to repeat, expose connection and daemon actor failures clearly, and provide the rediscovery path needed before mutating unknown-acceptance responses can be safe. Date: 2026-07-02.
- Decision: Treat `--json` stdout purity as part of timeout correctness. Rationale: Operators and agents cannot distinguish timeout phases or accepted operation ids if stdout contains non-JSON diagnostics. Date: 2026-07-02.
- Decision: Preserve recovery safety checks and prefer safe read suggestions after unknown acceptance. Rationale: Timeout UX must not weaken retained-run, active-run, artifact, or idempotency guards. Date: 2026-07-02.
- Decision: Treat review feedback about acceptance evidence, tests, dogfood timing, docs/helper migration, provider-live/cache boundaries, full validation, and linting as implementation-pack obligations rather than prose-only guidance. Rationale: The follow-up implementation workflow consumes the structured pack for mechanical steps, so the pack must carry every required validation obligation that this review document promises. Date: 2026-07-02.

## Outcomes & Retrospective

Not yet implemented. The expected outcome is a control CLI whose timeout responses reliably answer accepted/not accepted/unknown, name the timeout phase, state retryability, and suggest a safe next command without leaking secrets or polluting JSON stdout.

## Validation and Acceptance

Acceptance requires automated and operator-visible evidence. Targeted tests must prove duration parsing for `500ms`, `5s`, `2m`, invalid strings, env defaults, and command-line override precedence; structured JSON timeout errors for each implemented phase; daemon connect timeout/refusal/missing-control/bad-response branches; JSON-only stdout for non-streaming `--json` commands; invalid duration failure before daemon contact; stale-marked read data where implemented; `operation-status --wait --timeout` wait timeout returning accepted still-running metadata; mutating unknown acceptance returning `accepted: "unknown"`, `retryable: false`, and a safe read suggestion; and redaction of tokens, credentials, prompts, and secret-bearing paths from JSON, stderr, logs, and suggested commands.

Before publish, run from the repository root `direnv exec . gleam format --check src test`, focused targeted `gleam test` commands for the changed ctl/client/server/query suites, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`, expecting zero exits. Documentation/helper acceptance requires updated CLI help, `docs/specs/SCHERZO_CTL_TIMEOUT_POLICY.md` if implementation discoveries change the normative contract, relevant runbooks or operator examples, and a helper inventory stating whether `.scherzo/workflows/scripts/*`, `.scherzo/workflows/schemas/*`, `workflows/dogfood/scripts/*`, or provider-facing helper contracts changed; if any changed, the implementation must run the relevant helper/schema tests and record results. Provider-live/cache acceptance requires either an explicit note that no provider-live behavior, remote-provider cache behavior, TTL, or invalidation policy changed, or targeted tests for the directly required change before publish.

Manual or dogfood evidence should include at least one pre-publish local or fake-daemon transcript for a read timeout and one accepted operation wait timeout. No browser UI manual check is required because browser UI redesign is out of scope; if implementation unexpectedly changes a browser-visible control surface, that check becomes pre-publish unless the implementer records a safe deferral. Live daemon dogfood against real retained work may be deferred to a human/operator after handoff only if the implementation task records the exact command, expected redacted output, and reason it could not be safely completed before publish.

## Rollout, Recovery, and Idempotence

Rollout should be additive where possible: add timeout fields and accepted-state metadata without removing existing `ok`, `error.code`, `error.message`, `data.status`, or `operation_id` fields. Compatibility-sensitive stdout/stderr changes must be documented in help and runbooks. No stored-data migration is expected; rollback is to return command routing to the previous client/server timeout handling while leaving harmless historical operation records intact.

Mutating timeout recovery must be idempotency-safe. When Scherzo can prove a request was not accepted, exact retry may be suggested only if it is safe. When acceptance is unknown, suggested next commands must be safe reads such as `operation-status`, `ps`, `session`, or `events`, and the implementation should use existing or new stable operation/client request ids to rediscover daemon receipt. Re-running validation, documentation updates, and cleanup scripts should be safe and should not expose control tokens.

If implementation discovers that a workflow helper, schema, provider-facing structured-output helper, review-lane contract, provider-live call path, or remote-provider cache behavior must change, the safe rollout choice is to split or roll back that extra scope unless it is directly necessary for the timeout policy and has targeted helper/schema/provider/cache tests. New stale-read behavior must be additive, visibly marked as stale in JSON and human output, and safe to disable by falling back to the policy timeout error when the cached or projected data cannot be trusted.

## Open Questions and Clarifications Needed

No blocking clarification is needed before implementation. The implementation inventory should record which read surfaces have usable cached or projected data for stale-marked responses; if a read surface lacks safe cached data, it should return the policy JSON timeout shape instead of inventing stale data.
