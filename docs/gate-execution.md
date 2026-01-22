# Gate Execution Design

This document details the end-to-end flow from task completion through gate evaluation to merge-ready status.

## Overview

```
Task Assigned → Agent Works → Stop Hook → Gate Evaluation → Pass/Fail
                    ↑                           │
                    └───── Feedback Loop ───────┘ (on failure)
```

When an agent signals completion via the Stop hook, Scherzo evaluates completion gates. If gates fail, feedback is provided and the agent (or a fresh agent) continues working. This loop repeats until all gates pass or iteration limits are reached.

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Trigger mechanism | Stop hook | Agent explicitly signals "I think I'm done" |
| Same vs new agent on failure | Same agent by default | Preserves context, faster iteration |
| When to use new agent | Context exhaustion or config | Fresh context if agent is stuck |
| Gate execution order | Configurable per-gate | Some gates are prerequisites for others |
| Parallel vs serial | Per-gate config | Parallel for independent dimensions, serial for dependencies |
| Iteration limit | Configurable, default 3 | Prevent infinite loops |
| Failure escalation | After max iterations → human review or fail | Clear exit condition |

## Task Lifecycle States

```gleam
pub type TaskPhase {
  // Agent is working on the task
  Working(agent_id: Id, iteration: Int)

  // Agent signaled done, gates are being evaluated
  Evaluating(agent_id: Id, iteration: Int, gate_index: Int)

  // Gates failed, feedback sent back to agent
  NeedsFixes(agent_id: Id, iteration: Int, feedback: GateFeedback)

  // All gates passed, ready for merge queue
  GatesPassed(agent_id: Id, completed_at: Timestamp)

  // Max iterations exceeded, needs human intervention
  Stuck(agent_id: Id, iterations: Int, last_feedback: GateFeedback)

  // Terminal failure (unrecoverable)
  Failed(reason: String)
}
```

## End-to-End Flow

### 1. Task Assignment

```
Coordinator assigns task to agent
  → Create workspace (.scherzo/workspaces/<task-id>/)
  → Write task.json with task metadata
  → Generate .claude/settings.json with hooks
  → Spawn agent process in workspace
  → Task enters Working(agent_id, iteration=1)
```

### 2. Agent Works

Agent works autonomously in its workspace. The Stop hook is configured to call `scherzo checkpoint --done` when the agent believes work is complete.

```toml
# .claude/settings.json (generated)
[[hooks]]
event = "Stop"
command = "scherzo checkpoint --task-id={{task_id}} --done"
```

### 3. Stop Hook Triggers Evaluation

When agent stops and signals done:

```
Agent stops with --done flag
  → scherzo checkpoint writes final checkpoint
  → Checkpoint includes: files_modified, work_summary, agent's confidence
  → AgentProcess detects exit
  → Notifies Coordinator: TaskCompletionSignal(task_id, checkpoint)
  → Task enters Evaluating(agent_id, iteration, gate_index=0)
```

### 4. Gate Evaluation

The GateExecutor runs configured gates in order:

```
GateExecutor receives TaskCompletionSignal
  → Load gate config from .scherzo/config.toml (or defaults)
  → For each gate in order:
      → Execute gate (may spawn review agents)
      → Collect results
      → If FAIL: stop evaluation, prepare feedback
      → If PASS: continue to next gate
  → All gates pass: Task enters GatesPassed
  → Any gate fails: Task enters NeedsFixes with feedback
```

### 5. Gate Types & Execution

#### Serial Gates
Run one at a time, in order. Later gates can depend on earlier gates passing.

```toml
[[gates]]
name = "tests"
type = "command"
command = "gleam test"
serial = true  # Must pass before subsequent gates run
```

#### Parallel Review Gate
Spawn multiple review agents concurrently:

```
ParallelReviewGate executes:
  → Get jj diff for the task's changes
  → Spawn N review agents (one per dimension)
  → Each reviewer sees: diff + dimension-specific prompt
  → Wait for all reviewers to complete
  → Spawn synthesis agent with all findings
  → Synthesis produces: pass/fail + prioritized findings
```

#### Command Gate
Run a shell command (tests, lint, typecheck):

```toml
[[gates]]
name = "typecheck"
type = "command"
command = "gleam check"
```

#### Human Gate
Block until human approves:

```
HumanGate executes:
  → Notify control pane: "Task X awaiting approval"
  → Show diff summary and any previous gate feedback
  → Block until human responds: approve/reject/comment
  → If rejected with comment: treat as gate failure with feedback
```

### 6. Feedback Loop (On Failure)

When a gate fails:

```
Gate fails with findings
  → GateExecutor builds GateFeedback:
      - Which gate failed
      - Structured findings (P0/P1/P2 issues)
      - Suggested fixes
  → Check iteration count:
      - If iteration < max_iterations:
          → Decide: same agent or new agent
          → Send feedback to agent
          → Task enters NeedsFixes, then back to Working
      - If iteration >= max_iterations:
          → Task enters Stuck
          → Notify human via control pane
```

#### Same Agent vs New Agent Decision

```gleam
pub type RetryStrategy {
  // Re-trigger same agent in same workspace (default)
  SameAgent

  // Spawn fresh agent (new context, same workspace)
  FreshAgent

  // Let config decide based on failure type
  Auto
}
```

**Same Agent (default):**
- Agent still has context from previous work
- Faster - no context rebuild needed
- Works well for small fixes
- Use when: gate feedback is specific and actionable

**Fresh Agent:**
- Clean context, fresh perspective
- Avoids agent getting "stuck" on same approach
- Use when:
  - Same agent failed multiple times on same issue
  - Context is exhausted
  - Config specifies fresh agent after N failures

```toml
# .scherzo/config.toml
[retry]
strategy = "auto"           # "same", "fresh", or "auto"
fresh_after_failures = 2    # Switch to fresh agent after 2 same-agent failures
max_iterations = 3          # Total attempts before marking stuck
```

### 7. Feeding Feedback to Agent

When re-triggering an agent (same or fresh):

**Same Agent:**
```
Send message to running agent process:
  "Gate evaluation found issues that need fixing:

  ## Failed Gate: code_review

  ### P0 Critical
  - src/auth.gleam:42 - SQL injection vulnerability in query construction

  ### P1 Major
  - src/user.gleam:15 - Missing error handling for database timeout

  Please fix these issues. The workspace is unchanged - continue where you left off."
```

**Fresh Agent:**
```
Spawn new agent with continuation prompt:
  "Continue task: <title>

  Previous work summary:
  <work_summary from checkpoint>

  Gate evaluation found issues:
  <structured feedback>

  Files modified so far:
  <list of files>

  Please fix the issues identified above.
  Review the current state with: jj diff"
```

### 8. Gates Pass → Merge Ready

When all gates pass:

```
All gates pass
  → Task enters GatesPassed(agent_id, completed_at)
  → Emit event: TaskGatesPassed(task_id, change_id)
  → MergeQueue receives event
  → Task enters merge queue for integration

  (Agent workspace kept until merge completes, then cleaned up)
```

## Gate Configuration

### Full Config Example

```toml
# .scherzo/config.toml

[gates]
# Use a preset formula as base, then override
formula = "code-review"     # "code-review", "security-audit", "quick-review", or "none"

[retry]
strategy = "auto"
fresh_after_failures = 2
max_iterations = 3

# Override or add gates
[[gates.list]]
name = "tests"
type = "command"
command = "gleam test"
serial = true               # Must pass before review gates
fail_fast = true            # Stop task immediately if tests fail

[[gates.list]]
name = "typecheck"
type = "command"
command = "gleam check"
serial = true

[[gates.list]]
name = "code_review"
type = "parallel_review"
# Uses dimensions from formula, or specify custom:
# dimensions = ["correctness", "security", "performance"]

[[gates.list]]
name = "human_approval"
type = "human"
prompt = "Review and approve this change"
# Only for certain task types:
when = "task.priority == 1"   # P1 tasks need human approval
```

### Preset Formulas

**code-review** (default):
- Tests (serial)
- Typecheck (serial)
- Parallel review: all 7 dimensions
- Synthesis

**security-audit**:
- Tests (serial)
- Parallel review: security, resilience, correctness only
- Human approval required
- Synthesis

**quick-review**:
- Tests (serial)
- Parallel review: correctness, security only
- Synthesis

**none**:
- No gates (for development/testing)

## State Machine

```
                                    ┌─────────────────┐
                                    │   Task Created  │
                                    └────────┬────────┘
                                             │
                                             ▼
                               ┌─────────────────────────┐
                               │  Working(agent, iter=1) │◄─────────────┐
                               └────────────┬────────────┘              │
                                            │                           │
                                   Stop hook (--done)                   │
                                            │                           │
                                            ▼                           │
                               ┌─────────────────────────┐              │
                               │  Evaluating(gate_idx)   │              │
                               └────────────┬────────────┘              │
                                            │                           │
                          ┌─────────────────┴─────────────────┐         │
                          │                                   │         │
                     All pass                            Gate fails     │
                          │                                   │         │
                          ▼                                   ▼         │
               ┌──────────────────┐              ┌────────────────────┐ │
               │   GatesPassed    │              │ NeedsFixes(feedback)│ │
               └────────┬─────────┘              └──────────┬─────────┘ │
                        │                                   │           │
                        ▼                        iter < max?│           │
               ┌──────────────────┐                         │           │
               │   Merge Queue    │              ┌──────────┴──────────┐│
               └──────────────────┘              │                     ││
                                            Yes: feed back         No: │
                                            to agent               stuck│
                                                 │                     │
                                                 │                     ▼
                                                 │          ┌──────────────────┐
                                                 └─────────►│      Stuck       │
                                                            │ (human review)   │
                                                            └──────────────────┘
```

## Components

### GateExecutor

```gleam
// gates/executor.gleam

pub type GateExecutor {
  GateExecutor(
    config: GateConfig,
    task_id: Id,
    workspace_path: String,
  )
}

pub fn execute(executor: GateExecutor) -> Result(GateResult, GateError)

pub type GateResult {
  AllPassed(gate_results: List(SingleGateResult))
  Failed(
    failed_gate: String,
    feedback: GateFeedback,
    passed_gates: List(SingleGateResult),
  )
}
```

### GateFeedback

```gleam
// gates/types.gleam

pub type GateFeedback {
  GateFeedback(
    gate_name: String,
    findings: List(Finding),
    summary: String,
    suggested_action: String,
  )
}

pub type Finding {
  Finding(
    priority: Priority,
    location: Option(Location),
    issue: String,
    suggestion: Option(String),
  )
}

pub type Location {
  Location(file: String, line: Option(Int), column: Option(Int))
}

pub type Priority {
  P0Critical    // Must fix
  P1Major       // Should fix
  P2Minor       // Nice to fix
  P3Suggestion  // Optional improvement
}
```

### RetryController

```gleam
// gates/retry.gleam

pub type RetryController {
  RetryController(
    task_id: Id,
    config: RetryConfig,
    history: List(IterationRecord),
  )
}

pub type IterationRecord {
  IterationRecord(
    iteration: Int,
    agent_id: Id,
    feedback: Option(GateFeedback),
    outcome: IterationOutcome,
  )
}

pub type IterationOutcome {
  Passed
  FailedWithRetry
  FailedMaxIterations
}

pub fn should_retry(ctrl: RetryController) -> Bool
pub fn should_use_fresh_agent(ctrl: RetryController) -> Bool
pub fn record_iteration(ctrl: RetryController, record: IterationRecord) -> RetryController
```

## Files

| File | Purpose |
|------|---------|
| `src/scherzo/gates/executor.gleam` | Main gate execution loop |
| `src/scherzo/gates/types.gleam` | GateFeedback, Finding, Priority, etc. |
| `src/scherzo/gates/retry.gleam` | Retry logic and same/fresh agent decision |
| `src/scherzo/gates/feedback.gleam` | Format findings for agent consumption |
| `src/scherzo/gates/command.gleam` | Command gate (tests, lint, etc.) |
| `src/scherzo/gates/parallel_review.gleam` | Parallel review executor |
| `src/scherzo/gates/synthesis.gleam` | Combine review findings |
| `src/scherzo/gates/multipass.gleam` | Sequential multi-pass review |
| `src/scherzo/gates/human.gleam` | Human approval gate |
| `src/scherzo/config/types.gleam` | ScherzoConfig, GateConfig, etc. |
| `src/scherzo/config/parser.gleam` | Parse .scherzo/config.toml |
| `src/scherzo/config/loader.gleam` | Load formula TOML files, merge with user config |
| `src/scherzo/config/formulas/*.toml` | Default gate formula configurations |

## Events

```gleam
// Extend core/event.gleam

pub type GateEvent {
  // Task completion signal received
  TaskCompletionSignaled(task_id: Id, checkpoint: Checkpoint)

  // Gate evaluation started
  GateEvaluationStarted(task_id: Id, iteration: Int)

  // Individual gate completed
  GateCompleted(task_id: Id, gate_name: String, result: SingleGateResult)

  // All gates passed
  AllGatesPassed(task_id: Id, iteration: Int)

  // Gate failed, retry triggered
  GateFailedRetrying(task_id: Id, gate_name: String, feedback: GateFeedback, iteration: Int)

  // Max iterations reached
  GateFailedMaxIterations(task_id: Id, feedback: GateFeedback, iterations: Int)

  // Human review requested
  HumanReviewRequested(task_id: Id, prompt: String)

  // Human approved/rejected
  HumanReviewCompleted(task_id: Id, approved: Bool, comment: Option(String))
}
```

## Integration Points

### With AgentProcess

AgentProcess needs to:
1. Detect when agent exits with "done" signal
2. Send `TaskCompletionSignaled` event
3. Handle feedback messages (for same-agent retry)
4. Support "continue working" command

### With Coordinator

Coordinator needs to:
1. Listen for `TaskCompletionSignaled`
2. Trigger GateExecutor
3. Handle gate results (pass → merge queue, fail → retry/stuck)
4. Manage task phase transitions

### With MergeQueue

When `AllGatesPassed`:
1. Emit event for merge queue to pick up
2. Task's jj change is ready for merge
3. Workspace cleanup happens after merge completes

## Design Decisions (Resolved)

1. **Partial gate re-evaluation**: Yes, restart from beginning. If tests pass but review fails, re-run all gates after fixes to catch regressions introduced while addressing feedback.

2. **Parallel gate failures**: Aggregate all findings, then synthesize. Wait for all reviewers to complete before providing combined feedback - more complete per iteration.

3. **Human gate timeout**: Block indefinitely. Task waits until human responds - simplest approach for critical approvals.

4. **Cross-task gate state**: No caching, each task isolated. Simpler implementation, no stale cache concerns.
