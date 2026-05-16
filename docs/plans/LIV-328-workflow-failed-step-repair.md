# Add operator-driven failed-step workflow repair

This ExecPlan v2 review document frames the design for LIV-328. Mechanical implementation details, file-by-file edits, commands, and test instructions are intentionally kept out of this human-reviewable document and supplied through the structured implementation pack.

## Purpose / Big Picture

After this change, an operator can repair a workflow run that reached a terminal `failed_fatal` outcome without redispatching the whole issue from the beginning. The new operator action resumes from a safe step boundary: successful upstream artifacts and workspaces are reused, the failed attempt is explicitly superseded, and the failed step receives a new attempt before downstream DAG work continues.

The visible operator outcome is a distinct `scherzoctl retry-step ...` path that preserves the existing full `scherzoctl retry ...` behavior. A repaired run should make it obvious that upstream steps were not rerun, the failed step got a new attempt index, and downstream steps ran only after that retry succeeded.

## Problem Framing and Constraints

Scherzo already records durable workflow checkpoints, step artifacts, workspace locations, attempt indexes, workflow fingerprints, issue fingerprints, and resume inputs. Startup recovery can use these records when a workflow is active but interrupted. The gap is an operator-approved repair path for runs that have already been marked terminal failed.

The hard constraint is safety. Reusing prior work is only acceptable when the workflow definition, prompt contents, model settings, workspace profile or hooks, workspace root, and issue fingerprint still match the failed run. Missing or corrupt artifacts, missing source workspaces, ambiguous failed branches, active issues, and drift must produce clear operator-visible rejections or parking-for-inspection rather than silent continuation.

Another constraint is separation of intent. Full issue retry remains a fresh dispatch path with new run behavior. Failed-step retry is a repair action against a specific failed workflow run and must not become automatic startup behavior, especially for command steps.

## Strategy Overview

Introduce a narrow repair planner that is invoked only by an explicit operator command. The planner loads the local durable projection, selects either the exact run id requested by the operator or the latest terminal failed run for an issue reference, validates that the current workflow and issue still match the recorded fingerprints, identifies one failed fatal step, and builds a resume state that excludes the selected failed attempt and any dependent downstream artifacts. If an issue reference matches multiple indistinguishable latest failed runs, Scherzo rejects the command as ambiguous and requires a run id.

The MVP should reuse the existing `execute_with_resume` machinery and keep the original run id by reopening the durable run with an explicit repair-request record before spawning the resumed worker. This keeps retained artifacts, workspaces, session naming, and run history tied to one operator-understandable run while allowing the projection to become active again if the daemon restarts mid-repair. The failed attempt is marked superseded with the next attempt index before execution resumes.

The command result should be explicit and boring: accepted repairs say which run and step were selected and which attempt will be started; rejected repairs use stable reason codes for drift, ambiguity, missing artifacts, unsafe command retry in non-operator paths, missing workspaces, no failed terminal run, and already-active issues. Drift, corrupt artifacts, missing upstream artifacts, and missing source workspaces are rejected before acceptance; after acceptance, crash recovery repeats the same checks and parks the reopened run for manual inspection if previously valid context has disappeared or become corrupt.

## Alternatives Considered

One alternative is to overload `scherzoctl retry` so it sometimes performs partial repair. That was rejected because operators need a clear distinction between discarding a run and resuming from a failed step.

A second alternative is to create a brand-new linked repair run id for every partial retry. That is attractive for append-only purity, but it complicates workspace validation because recovered workspace summaries currently bind to the original run id and run root. The MVP should keep the stable run id and make the reopen explicit in the ledger. A later migration can add linked repair run ids if the projection model grows first-class support for copied upstream artifacts.

A third alternative is to make startup recovery automatically reopen terminal failed workflows when all artifacts look valid. That was rejected because command steps and failed agent prompts are not safe to rerun without an operator's explicit intent.

## Risks and Countermeasures

The main risk is replaying stale context after the issue, workflow YAML, prompts, model settings, hooks, or workspace root changed. The countermeasure is strict fingerprint and workspace-root validation before any repair record is appended. Drift rejects by default in the MVP; there is no override flag.

A second risk is treating a partial failed workspace as clean. The countermeasure is to prepare the retried step from the last successful logical source workspace, not from the failed attempt workspace, unless the existing pi-session continuation checks explicitly allow true continuation.

A third risk is ambiguity in parallel DAGs. If more than one failed or interrupted branch could be the repair boundary, the operator must provide `--step`; Scherzo must not guess.

A fourth risk is losing crash safety between accepting the repair and starting the new attempt. The countermeasure is to append repair-request, run-reopen, and step-superseded records before spawning the resumed worker, so normal recovery can either resume or park the reopened run.

## Scope Boundaries

In scope for the MVP is an operator command for failed-step repair of issue-dispatched workflow runs that ended `failed_fatal`, selection by issue reference or run id, optional step selection, durable repair records, artifact and workspace validation, resume-state construction, daemon spawning of the resumed workflow, control protocol support, CLI support, and operator-visible rejection codes. Command steps may be retried only through this explicit operator command; the separate `retry-step` action is the MVP confirmation signal.

Out of scope is changing full retry semantics, adding automatic startup repair of terminal failed runs, repairing scheduled workflows, inventing a new workspace driver protocol, performing semantic merge repair inside Scherzo, adding an extra command-step confirmation flag, or allowing drift overrides. True pi-session continuation for an interrupted failed attempt may be supported only when existing continuation validation already proves it safe; otherwise the retried step starts as a fresh attempt.

## Milestones

First, define the repair contract in durable state terms: how failed runs are selected, how failed attempts are superseded, what drift means, and what resume state is safe to build. This milestone retires the largest ambiguity before any operator surface is added.

Second, add the pure planning and recovery adapter behavior. At the end of this milestone, Scherzo can identify repairable terminal failed runs, reject unsafe candidates, recover hash-valid upstream artifacts and workspaces, and compute the next attempt index without spawning workers.

Third, add the operator control path. At the end, `retry-step` travels through `scherzoctl`, the control protocol, and the daemon command handler, returning stable applied or rejected statuses.

Fourth, wire execution through the existing resumed workflow worker path. At the end, a repaired run reuses completed upstream context, reruns only the selected step, and continues downstream DAG execution.

Fifth, harden the behavior with sequential, parallel, drift, corruption, ambiguity, and command-result tests before dogfooding on retained failed runs.

## Progress

- [x] (2026-05-16 00:00Z) Drafted the human-reviewable ExecPlan v2 review document for LIV-328.
- [ ] Implementation pack to be consumed by Scherzo's canonical bundle generator.
- [ ] Code implementation and validation not yet started.

## Decision Log

- Decision: Keep `scherzoctl retry` and failed-step repair as separate commands.
  Rationale: Operators must know whether they are discarding prior progress or preserving it.
  Date: 2026-05-16

- Decision: Use the original run id for the MVP repair execution, with explicit durable repair and reopen records.
  Rationale: Existing recovered workspace validation is keyed to the original run id and run root; preserving that identity avoids a broader artifact-copying model while keeping the ledger auditable.
  Date: 2026-05-16

- Decision: Reject drift by default and omit an MVP override flag.
  Rationale: The value of partial retry depends on reusing exactly the context that produced the successful upstream artifacts; any override should be a later, separately audited feature.
  Date: 2026-05-16

- Decision: Treat `retry-step` itself as sufficient command-step confirmation for the MVP.
  Rationale: The command is already explicit operator intent, while startup recovery and other non-operator paths remain unable to retry command steps automatically.
  Date: 2026-05-16

## Validation and Acceptance

Acceptance is behavioral. In a sequential DAG where A completed, B failed fatal, and C is pending, `retry-step` must preserve A, supersede B's failed attempt, start B at the next attempt index, and run C only after B succeeds. In a parallel DAG, Scherzo must preserve independent completed branches but require `--step` when multiple failed or interrupted repair boundaries exist.

Validation must also prove negative cases deterministically: before acceptance, drift, missing or corrupt upstream artifacts, missing source workspaces, ambiguous issue selection, and already-active issues are rejected. After an accepted repair, crash recovery either resumes from the accepted repair records when the same checks still pass or parks the reopened run for manual inspection when the accepted context is no longer valid. Full `retry` must still perform a full dispatch rather than partial repair.

## Rollout, Recovery, and Idempotence

The rollout should be additive. Existing workflow execution, startup recovery, and full retry behavior should remain unchanged unless the new operator command is invoked. New durable repair records are version-gated: a daemon that does not understand them must fail closed by rejecting or parking affected repaired runs rather than ignoring the records and continuing. Downgrade across an accepted repair is unsupported until the repaired run completes or is manually parked.

The repair command should be idempotent after acceptance: a second invocation against the same superseded failed attempt should not allocate another attempt unless the first repair attempt itself has reached a new terminal failed state. If the daemon crashes after accepting repair but before completion, the reopened run should either resume through existing recovery or park for manual inspection using the same safety checks.

## Open Questions and Clarifications Needed

No unresolved MVP safety questions remain. Linked repair run ids can be revisited after the MVP once the state model can represent preserved upstream artifacts independently from one run id.
