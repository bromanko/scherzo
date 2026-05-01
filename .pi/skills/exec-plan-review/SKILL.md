---
name: exec-plan-review
description: >
  Review execution plans (ExecPlans) adversarially for soundness, risk, clarity,
  and implementability. Use this skill when the user asks to "review a plan",
  "review this exec plan", "critique this plan", "is this plan good enough",
  "check this plan", "audit this plan", "review my PLAN.md", "tear this
  plan apart", "poke holes in this plan", "stress-test this plan", or wants
  feedback on whether an execution plan is both executable and a good idea. Also
  trigger when the user mentions "plan review", "plan quality", asks whether a
  plan is "implementable" or "self-contained enough", or wants a skeptical,
  adversarial review of a proposed implementation approach or design direction.
---

# Adversarial ExecPlan Review

Review an execution plan with two goals:

1. Determine whether a skilled developer or coding agent could implement it without getting stuck.
2. Determine whether the plan is actually good — meaning it solves the right problem, uses a proportionate design, addresses likely failure modes, and is worth implementing as written.

Do not review the plan as a friendly editor. Review it as a skeptical technical lead performing a pre-mortem. Assume that if the plan is vague, overcomplicated, poorly sequenced, unsafe, or based on a weak premise, a future implementer will faithfully build the wrong thing. Your job is to find those problems before implementation begins.

Read the exec-plan skill at `../exec-plan/SKILL.md` to understand the full authoring standard — particularly the "Who This Plan Is For" section, which defines the developer profile. This review skill evaluates plans against that standard plus the adversarial criteria below.


## Review Process

1. **Read the entire plan** from top to bottom before writing any feedback.
2. **Read the exec-plan skill** at `../exec-plan/SKILL.md` to refresh the authoring standard.
3. **Identify the claimed outcome**: what user-visible or operator-visible result the plan says it will produce.
4. **Pressure-test the approach before the prose**:
   - Is this the right problem to solve?
   - Is the proposed design proportionate to the problem?
   - Is there a simpler, safer, or more reversible path?
   - What assumptions could be false?
   - What happens if the implementer follows the plan exactly and the plan is wrong?
5. **Then evaluate executability**: whether a developer could carry it out safely, concretely, and with enough validation to catch failure.
6. **Output findings** in the format specified at the end of this document.

Prefer findings that would change whether the team should implement the plan, how it should be sequenced, or what design it should use. Cosmetic comments are low priority.


## Adversarial Lens

Use these questions throughout the review:

- **Wrong problem**: Does the plan optimize or rebuild something without proving that the user or business problem is real and current?
- **Wrong level of ambition**: Is the plan too big, too abstract, or too invasive for the outcome it promises?
- **Unproven assumptions**: What must be true for this plan to work? Does the plan verify those assumptions early?
- **Simpler alternative ignored**: Is there a smaller or lower-risk change that would achieve most of the value?
- **Hidden blast radius**: What existing behaviors, interfaces, migrations, operators, or downstream systems could break?
- **Poor failure handling**: If the change partially works, fails mid-flight, or needs rollback, does the plan keep the system safe?
- **Non-falsifiable success**: Could the plan "succeed" on paper while still delivering little or no real user value?
- **Cargo-cult complexity**: Does the plan introduce abstractions, layers, or infrastructure that sound sophisticated but are not justified by the current requirement?

If the answer to any of these is "yes" or "maybe," raise it explicitly.

Do not spend review energy on writing style unless it creates real ambiguity, hides risk, or makes the plan harder to execute safely.


## Review Checklist

### Problem Framing and User Value

The plan must be worth doing, not merely possible to execute.

- The plan states the concrete problem in user or operator terms, not just desired code changes.
- The promised outcome is observable. A reader can tell what becomes easier, safer, faster, or newly possible after implementation.
- The scope matches the problem. No gold-plating, platform-building, or speculative extensibility for future use cases that are not required now.
- If the plan claims performance, reliability, security, or usability improvement, it states how that claim will be measured or observed.
- The plan distinguishes between the real need and an implementation preference. "Use technology X" is not itself a valid purpose.
- If a materially simpler approach could deliver most of the value, the plan either chooses it or explains why it is insufficient.
- If the plan solves only part of the stated problem, it says so plainly and defines the boundary.

### Strategy and Architecture

The overall approach should be technically sound and proportionate.

- The design is likely to work in this repository as described. It is not hand-wavy about key interactions, interfaces, migrations, or compatibility concerns.
- The plan reuses existing modules, conventions, and infrastructure where reasonable instead of inventing parallel systems.
- Irreversible or high-blast-radius changes are justified and, where possible, deferred until lower-risk validation happens first.
- Unknowns are de-risked early. If feasibility is unclear, the plan includes a prototype, spike, or narrow validation milestone before committing to a broad refactor.
- Sequencing is sensible. The plan does not front-load major rewrites before proving necessity.
- The architecture is proportionate to the problem. No extra service, abstraction layer, configuration surface, or generic framework unless the current work genuinely requires it.
- Existing compatibility contracts are preserved, migrated safely, or intentionally broken with explicit migration steps and validation.
- Risky decisions remain reversible where practical.

### Risks, Failure Modes, and Safety

A good plan anticipates what can go wrong.

- The plan identifies likely failure modes: bad input, partial rollout, data migration issues, race conditions, duplicate work, stale state, compatibility breaks, operational confusion, or user-visible regressions.
- Destructive operations, schema changes, backfills, or removals include rollback or containment steps.
- The plan says how to detect failure in practice — through tests, logs, metrics, UI behavior, API responses, or command output.
- Edge cases are treated as first-class work, not as an afterthought.
- If the change affects security, privacy, access control, or trust boundaries, the plan includes explicit validation for those concerns.
- If the change affects performance or scalability, the plan includes a way to verify that it does not regress unacceptable paths.
- The plan does not assume a perfect operator or a perfectly clean environment.

### Executability

The plan must be concrete enough for a developer with only the plan and the working tree to execute it safely.

- Every term of art is defined in plain language where it first appears. No jargon is assumed understood.
- No references to "the architecture doc", "the wiki", "the prior plan", external blog posts, or anything outside the plan and the repository. If knowledge is required, it is embedded in the plan.
- The repository layout relevant to this work is described explicitly: which directories matter, what lives where, how the pieces connect.
- Steps are small, unambiguous actions that take roughly 2–5 minutes and do not require the implementer to invent design details.
- Where applicable, steps follow the TDD cycle: write failing test, run it, make it pass, rerun tests, commit.
- Each step names the exact file(s) to touch, with full repository-relative paths, and states what to add, change, or remove.
- No absolute paths appear anywhere in the plan. The plan may be implemented in any workspace, worktree, or checkout location; all paths must be relative to the repository root.
- Build, test, and run commands are stated in full — the working directory, the exact command, and the expected output.
- Environment assumptions (OS, language version, installed tools, environment variables) are stated or can be inferred from the repo.
- Commit points are called out explicitly, and the system remains in a working state at each commit point.

### Testing and Falsifiability

Assume the developer will not invent good tests on their own. The plan must specify exactly what to test and how. It must also make it possible to prove the plan wrong.

- Every new behavior has at least one test specified in the plan — not "write tests for this" but "write a test that calls `parse("")` and asserts it returns `Err(EmptyInput)`."
- The tests cover the core claim of the plan, not just helper functions or happy paths.
- Edge cases and negative cases are called out explicitly. The developer should not have to think about what the edge cases are.
- Test file locations are named with full repository-relative paths.
- The test runner command is stated, including how to run a single test or a subset.
- Expected test output (pass/fail counts, specific assertion messages) is described so the developer knows what "working" looks like.
- When a test should fail before implementation (red phase of TDD), the plan says so and describes what the failure looks like.
- Integration or end-to-end validation steps are included where appropriate — not just unit tests.
- If the plan makes non-functional claims (performance, resilience, safety), it includes a concrete way to falsify those claims.

### Validation and Rollout

The plan must show how to prove success in reality, not only in unit tests.

- Each milestone has a clear, observable outcome.
- Milestones are independently verifiable and build incrementally.
- Validation steps are concrete: the exact command, the expected output, and what to do if it does not match.
- The sequence of milestones reduces risk early instead of postponing the hardest unknowns until the end.
- If the work changes an existing interface, deployment path, data format, or operator workflow, the plan explains how to roll it out safely.
- If rollback, fallback, coexistence, feature-flagging, or migration windows matter, the plan specifies them.
- Success criteria are tied to real behavior, not merely "code added" or "tests passed."

### Format Compliance

Check compliance with the ExecPlan format, but do not let mechanical formatting comments crowd out strategic critique.

- The four required living-document sections are present and meaningful:
  - **Progress** — A checklist of granular steps with timestamps.
  - **Surprises & Discoveries** — Space to record unexpected findings during implementation.
  - **Decision Log** — Space to record design decisions with rationale and dates.
  - **Outcomes & Retrospective** — Space to capture lessons learned at milestones and completion.
- Progress appears current rather than stale or obviously aspirational.
- Readability issues are worth mentioning only when they create ambiguity, hide assumptions, or make execution materially harder.


## Output Format

Present findings grouped by checklist section. Lead with the issues most likely to make the plan a bad investment, unsafe, or technically misguided.

Use severity levels to prioritize:

- **BLOCKING**: The plan should not be implemented as written. It is likely to fail, create avoidable risk, solve the wrong problem, or force the implementer into guesswork that materially changes the outcome.
- **GAP**: Something important is missing or underspecified. A skilled developer might recover, but the plan is not yet trustworthy or complete.
- **SUGGESTION**: An improvement that would make the plan clearer, safer, more proportionate, or easier to follow.

```markdown
## Plan Review: <plan title or filename>

### Summary

<2-4 sentence overall assessment: Is this plan merely executable, or actually good? What are the biggest strategic or technical risks? Is there a simpler or safer direction?>

### Problem Framing and User Value

<findings or "No issues found.">

### Strategy and Architecture

<findings or "No issues found.">

### Risks, Failure Modes, and Safety

<findings or "No issues found.">

### Executability

<findings or "No issues found.">

### Testing and Falsifiability

<findings or "No issues found.">

### Validation and Rollout

<findings or "No issues found.">

### Format Compliance

<findings or "No issues found.">

### Verdict

**READY** | **REVISE** | **REWRITE**

- **READY**: The plan is both sound and implementable as-is.
- **REVISE**: The plan has real issues, but the core direction is plausible. Address the BLOCKING and GAP items before implementation.
- **REWRITE**: The plan has fundamental problems — wrong framing, disproportionate design, missing safety story, or missing core sections. Start over with the exec-plan skill.

### Priority Fixes

<Numbered list of the top 3-5 changes that would most improve the plan, starting with the issues that change whether the plan should be executed at all.>
```

Within each section, format individual findings as:

```markdown
**[SEVERITY] Finding title**
<Description of the issue — what is missing, what seems wrong, why it matters, and what bad outcome it could produce if left unchanged.>
<Concrete suggestion for how to fix it, with examples where helpful.>
```

When appropriate, explicitly name the safer or simpler alternative you think the plan should consider.


## Handling Edge Cases

- **Plan is not an ExecPlan**: If the document does not follow the ExecPlan format at all (no milestones, no progress section, no concrete steps), note this upfront and evaluate it against the spirit of the standard. Recommend converting to ExecPlan format.
- **Plan is partially implemented**: Review the remaining work. Check that the Progress section accurately reflects what is done and what remains. Verify that completed milestones still make sense in context.
- **Plan references prior plans**: Flag this as an executability issue. The relevant context from prior plans must be incorporated.
- **Plan is very short**: Short is fine if the task is small. But even a small task needs file paths, test commands, expected outputs, and commit points. Brevity is not an excuse for vagueness.
- **Plan may be executable but still poor**: Say so plainly. A plan can be detailed enough to follow and still be overengineered, risky, mis-scoped, or aimed at the wrong outcome. In that case, prioritize the strategic critique over the mechanical one.
