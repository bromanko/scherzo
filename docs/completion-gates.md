# Completion Gates

Before a task enters the merge queue, it passes through configurable completion gates. The primary gate is a parallel code review inspired by gastown's convoy pattern.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Review execution | Parallel (convoy pattern) | Faster than sequential, catches more issues |
| Review dimensions | Configurable per-repo | Flexibility; default to 7 dimensions |
| Synthesis | Required after parallel review | Dedupe findings, prioritize, unified feedback |
| Rule of Five | Keep sequential | Iterative refinement works best in order |
| Output format | Prioritized (P0/P1/P2) | Clear severity for task agent to address |

## Architecture

```
Task Done → Parallel Review Agents → Synthesis Agent → Merge Queue
              ├─ Correctness         (combines findings,
              ├─ Performance          prioritizes,
              ├─ Security             dedupes)
              ├─ Elegance                ↓
              ├─ Resilience       If issues found:
              ├─ Style            → Feedback to task agent
              └─ Smells           → Re-review after fixes
```

## Gate Types

### ParallelReviewGate

Spawns multiple review agents in parallel, each focused on a specific dimension. All reviewers see the same `jj diff` and produce structured findings. A synthesis agent then combines all findings.

```gleam
pub type ParallelReviewGate {
  ParallelReviewGate(
    name: String,
    dimensions: List(ReviewDimension),
    synthesis_prompt: String,
    agent_provider: AgentProvider,
  )
}

pub type ReviewDimension {
  ReviewDimension(
    id: String,           // "correctness", "security", etc.
    focus: String,        // What to look for
    prompt: String,       // Review prompt template
  )
}
```

### MultiPassReview (Rule of Five)

Sequential iterative refinement passes. Each pass focuses on a different quality dimension.

```gleam
pub type MultiPassReview {
  MultiPassReview(
    name: String,
    passes: List(ReviewPass),
    require_convergence: Bool,
    agent_provider: AgentProvider,
  )
}
```

Passes:
1. Draft (breadth-first)
2. Correctness
3. Clarity
4. Edge Cases
5. Excellence

### HumanGate

Blocks the pipeline until a human approves.

```gleam
pub type HumanGate {
  HumanGate(
    name: String,
    prompt: String,
  )
}
```

## Default Review Dimensions

Seven parallel reviewers (inspired by gastown):

| Dimension | Focus |
|-----------|-------|
| **Correctness** | Logic errors, edge cases, race conditions |
| **Performance** | Algorithmic efficiency, bottlenecks, scaling |
| **Security** | Vulnerabilities, injection, OWASP |
| **Elegance** | Design quality, abstractions, SOLID |
| **Resilience** | Error handling, failure recovery, timeouts |
| **Style** | Naming, formatting, documentation |
| **Smells** | Anti-patterns, tech debt, organization |

## Configuration

Gates are configured per-repo in `.scherzo/config.toml`:

```toml
[[completion_gates]]
name = "code_review"
type = "parallel_review"
agent = "claude"

[[completion_gates.dimensions]]
id = "correctness"
focus = "Logic errors, edge cases, race conditions"
prompt = """
Review this code change for correctness:
- Logic errors and bugs
- Edge cases not handled
- Race conditions or concurrency issues
- Off-by-one errors
- Null/undefined handling

For each issue found, specify:
- Priority: P0 (critical), P1 (major), P2 (minor)
- Location: file:line if applicable
- Issue: Clear description
- Suggestion: How to fix
"""

[[completion_gates.dimensions]]
id = "performance"
focus = "Algorithmic efficiency, bottlenecks, scaling"
prompt = "Review for performance issues..."

[[completion_gates.dimensions]]
id = "security"
focus = "Vulnerabilities, injection, OWASP"
prompt = "Review for security vulnerabilities..."

[[completion_gates.dimensions]]
id = "elegance"
focus = "Design quality, abstractions, SOLID"
prompt = "Review for design elegance..."

[[completion_gates.dimensions]]
id = "resilience"
focus = "Error handling, failure recovery, timeouts"
prompt = "Review for resilience..."

[[completion_gates.dimensions]]
id = "style"
focus = "Naming, formatting, documentation"
prompt = "Review for style consistency..."

[[completion_gates.dimensions]]
id = "smells"
focus = "Anti-patterns, tech debt, organization"
prompt = "Review for code smells..."

[completion_gates.synthesis]
prompt = """
Synthesize the findings from all reviewers:
1. Deduplicate overlapping issues
2. Prioritize: P0 (critical), P1 (major), P2 (minor)
3. Provide clear, actionable feedback

Output format:
- PASS if no P0 or P1 issues
- FAIL with prioritized list of issues to fix
"""
```

## Execution Flow

```
1. Task agent completes work
2. Stop hook runs (tests, lint, format)
3. If Stop hook passes, trigger completion gates:

   a. Spawn N review agents in parallel (one per dimension)
      - Each sees only `jj diff`
      - Each produces structured findings

   b. Wait for all reviewers to complete

   c. Spawn synthesis agent with all findings
      - Deduplicates
      - Prioritizes (P0 > P1 > P2)
      - Produces unified review

   d. If P0 or P1 issues exist:
      - Feed synthesis back to task agent
      - Task agent fixes issues
      - Re-run parallel review (up to max_iterations)

   e. If only P2 or no issues:
      - Pass to next gate or merge queue
```

## Review Output Types

```gleam
pub type ReviewFindings {
  ReviewFindings(
    dimension: String,
    findings: List(Finding),
  )
}

pub type Finding {
  Finding(
    priority: Priority,
    location: Option(String),  // file:line
    issue: String,
    suggestion: String,
  )
}

pub type Priority {
  P0Critical  // Must fix before merge
  P1Major     // Should fix before merge
  P2Minor     // Nice to fix, won't block
}

pub type SynthesizedReview {
  SynthesizedReview(
    pass: Bool,
    p0_count: Int,
    p1_count: Int,
    p2_count: Int,
    findings: List(Finding),  // Deduplicated, sorted by priority
    summary: String,
  )
}
```

## Embedded Default Formulas

Scherzo ships with default review configurations:

| Formula | Dimensions | Use Case |
|---------|------------|----------|
| `code-review` | All 7 | Comprehensive review |
| `security-audit` | Security, Resilience, Correctness | Security-focused |
| `quick-review` | Correctness, Security, Style | Fast feedback |

## Files

| File | Purpose |
|------|---------|
| `src/scherzo/gates/types.gleam` | Gate and review types |
| `src/scherzo/gates/parallel_review.gleam` | Parallel executor |
| `src/scherzo/gates/synthesis.gleam` | Synthesis logic |
| `src/scherzo/gates/pipeline.gleam` | Gate pipeline execution |
| `src/scherzo/gates/formulas/*.toml` | Embedded default configs |
