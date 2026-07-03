# ExecPlan completion preflight

Scherzo keeps the `workflow:execplan` preflight cheap and structural. The helper validates that the review document can be safely retained and published, but it does not interpret prose or compare Markdown cues against implementation-pack steps.

## Blocking helper checks

`validate-review-doc` fails closed for structural defects:

- no level-1 title;
- missing required level-2 review-doc sections;
- required sections with no meaningful content, unless they contain an explicit sentinel such as `None.` or `No open questions.`;
- mechanical implementation sections in the human review doc (`Concrete Steps`, `Testing and Falsifiability`, `Interfaces and Dependencies`, or `Artifacts and Notes`);
- generated HTML, absolute local path shapes, non-Markdown review-doc paths, target mismatches, or ambiguous changed-review-doc discovery.

If a required-section failure occurs in `validate_review_doc_after_review`, retry or rerun the incorporate-review step only when the agent can regenerate the section from known context. Otherwise repair the review document before materializing the bundle, or move the issue back to Todo when the missing content is unknown and needs human input.

## Semantic alignment checks

Semantic alignment is intentionally not enforced by deterministic keyword matching. Review-doc claims about negative/error-path tests, idempotency, manual/browser/dogfood evidence, docs/helper migration, provider-live/cache behavior, full validation, linting, and similar obligations are handled by agent-comprehension checkpoints instead:

- the `review_plan` / `incorporate_review` agent loop while the ExecPlan is authored, which must keep required acceptance, milestone, rollout/safety, and validation obligations represented in the implementation pack;
- the execplan-implementation completion verifier after the actual implementation diff and command evidence exist, which must fail completion when the canonical plan or pack requires those obligations and the implementation run does not provide observable evidence.

The final execplan-implementation plan-completion gate remains responsible for failures that require comprehension or implementation evidence, including whether code changes truly implement the promised behavior, whether referenced files/tests/docs were updated, whether validation commands or pre-publish manual checks ran and passed, and whether review feedback or base refresh introduced new gaps after bundle materialization.
