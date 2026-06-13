# Fixture ExecPlan v2 Review Doc

## Purpose / Big Picture

This fixture describes a tiny documentation-only change so the v2 helper tests can validate review document handling without relying on Linear or GitHub.

## Problem Framing and Constraints

The fixture keeps human-reviewable intent in Markdown and mechanical implementation detail in the retained implementation pack. It must remain repository-relative and deterministic.

## Strategy Overview

Add one small documentation note and validate it with helper-level tests. The implementation pack contains the exact commands and file list.

## Alternatives Considered

A full ExecPlan fixture would duplicate the legacy v1 format and obscure the v2 contract. A concise review document is sufficient for the helper tests.

## Risks and Countermeasures

The main risk is accepting stale retained artifacts. The bundle helper recomputes hashes for this review document and its implementation pack.

## Scope Boundaries

Only fixture files under `test/fixtures/execplan_v2/` are in scope for this sample. No production workflow is exercised by the fixture itself.

## Milestones

The single milestone is to validate the review document, validate the pack, and prove the bundle links the two by hash.

## Progress

- [x] 2026-05-15: Created the fixture review document.

## Surprises & Discoveries

None.

## Decision Log

- 2026-05-15: Keep fixture review docs concise so helper tests focus on v2 artifact contracts.

## Outcomes & Retrospective

Pending implementation.

## Validation and Acceptance

Run `scripts/scherzo-execplan validate-review-doc --path test/fixtures/execplan_v2/review-doc.valid.md` and expect a zero exit code.

## Rollout, Recovery, and Idempotence

The fixture is static. Rerunning validation is read-only and deterministic.

## Open Questions and Clarifications Needed

None.
