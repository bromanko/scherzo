# Scherzo: AI Agent Orchestrator

## Overview

Scherzo is a Gleam-based AI agent orchestrator - "Kubernetes for AI agents". It takes tasks from external sources and coordinates CLI-based AI agents (Claude, Codex, Gemini) to execute them autonomously.

## Technology Choices

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Language | Gleam | Strong typing, clean syntax, runs on BEAM for actor model |
| Runtime | Erlang/OTP | Battle-tested supervision trees, fault tolerance |
| CLI Agents | claude, codex, gemini | Spawn as OS processes, not API calls |
| Distribution | Burrito | Single binary distribution |
| State | jj-backed JSON files | Durable by default, full history, crash recovery via `jj status`, debuggable via `jj log/diff` |

## Architecture

### Supervision Tree

```
ScherzoSupervisor (OneForAll)
├── EventBus           # Central pub/sub
├── TaskSourceSupervisor (OneForOne)
│   └── TicketSource   # .tickets/ integration via tk CLI (single source of truth)
├── TaskQueue          # Priority queue of ready tasks
├── Coordinator        # Matches tasks to agents
└── AgentPoolSupervisor (Factory)
    ├── AgentProcess_1 # Wraps OS process
    ├── AgentProcess_2
    └── ...
```

**Note:** Task state is stored in `.tickets/` files and queried directly via the ticket source adapter. This avoids state duplication and keeps tickets as the single source of truth. Runtime agent state (which agents are running, what tasks they're working on) is managed in-memory by the coordinator.

### Core Types

```gleam
// Agent providers
pub type AgentProvider { Claude | Codex | Gemini }

// Task lifecycle
pub type TaskStatus {
  Pending
  Ready           // Dependencies resolved
  Assigned(agent_id: Id)
  InProgress(agent_id: Id, started_at: Timestamp)
  Completed(agent_id: Id, completed_at: Timestamp)
  Failed(agent_id: Id, reason: String, failed_at: Timestamp)
  Blocked(reason: String)
}

// Agent status
pub type AgentStatus {
  Idle
  Running(task_id: Id, started_at: Timestamp)
  Failed(reason: String)
}
```

### Agent Driver Interface

Each CLI agent has a driver that knows how to:
1. Build the command with appropriate flags (--yolo, --output-format, etc.)
2. Parse streaming output
3. Detect success/failure
4. Generate hook configurations for checkpointing

```gleam
pub type Driver {
  Driver(
    name: String,
    build_command: fn(Task, AgentConfig) -> Command,
    parse_output: fn(String) -> ParsedOutput,
    detect_failure: fn(String, Int) -> Option(String),
    generate_hooks: fn(Task, CheckpointConfig) -> Result(HookConfig, String),
  )
}
```

### Hook-Based Checkpointing

Scherzo leverages CLI agent hook systems (Claude Code hooks, Codex hooks) to enable agents to survive context exhaustion and seamlessly hand off work to fresh agents.

#### Available Hooks by Provider

| Hook | Claude Code | Codex | Purpose |
|------|-------------|-------|---------|
| Post-action | `PostToolUse` | `post_action` | Checkpoint after each file edit |
| Stop | `Stop` | `on_exit` | Capture final state on agent stop |
| Pre-compact | `PreCompact` | - | Capture state before context compaction |
| Notification | `Notification` | - | Detect context limit warnings |

#### Checkpoint Types

```gleam
pub type Checkpoint {
  Checkpoint(
    task_id: Id,
    agent_id: Id,
    sequence: Int,                    // Monotonic counter for ordering
    timestamp: Timestamp,
    checkpoint_type: CheckpointType,
    files_modified: List(FileChange),
    tool_calls_since_last: List(ToolCall),
    work_summary: String,             // AI-generated via hook prompt
    next_steps: Option(String),       // Remaining work (on Stop only)
    jj_change_id: String,
  )
}

pub type CheckpointType {
  Incremental   // PostToolUse - periodic saves
  PreCompact    // Before context compaction
  Final         // Stop hook - agent finished or exhausted
}

pub type FileChange {
  FileChange(
    path: String,
    change_type: FileChangeType,      // Added | Modified | Deleted
    summary: Option(String),          // Brief description of change
  )
}
```

#### State & Checkpoint Storage (jj-backed)

All orchestrator state lives in `.scherzo/` and is tracked by jj, giving automatic durability, history, and crash recovery.

```
project/
├── .scherzo/                      # All jj-tracked (gitignored)
│   ├── state/
│   │   ├── tasks.json             # Task status and metadata
│   │   ├── agents.json            # Agent pool status
│   │   └── queue.json             # Current task queue
│   ├── checkpoints/
│   │   ├── <task-id>/
│   │   │   ├── 001.json           # Incremental checkpoint
│   │   │   ├── 002.json           # Incremental checkpoint
│   │   │   └── final.json         # Final checkpoint on stop
│   │   └── ...
│   ├── workspaces/                # Per-agent isolated workspaces (jj workspaces)
│   │   └── <task-id>/             # Each agent gets its own jj workspace
│   │       ├── .claude/
│   │       │   └── settings.json  # Agent-specific hooks (scherzo prime/checkpoint)
│   │       ├── .scherzo/
│   │       │   └── task.json      # Task metadata for hooks to read
│   │       └── <project files>    # Full working copy from jj workspace
│   ├── events/
│   │   └── <date>.jsonl           # Event log (append-only)
│   └── config.toml                # Orchestrator config
```

**Benefits of jj-backed state:**
- **Crash recovery**: `jj status` shows uncommitted state, nothing lost
- **History**: `jj log -p .scherzo/` shows all state changes
- **Debugging**: `jj diff -r <change>` to see what changed when
- **Undo**: `jj undo` or `jj restore` to revert bad state
- **Conflicts**: jj handles concurrent writes gracefully

#### Context Exhaustion Flow

```
1. Agent working on task
   ↓
2. PostToolUse hook fires after each tool → writes incremental checkpoint
   ↓
3. Context nears limit:
   - Notification hook detects warning, OR
   - PreCompact hook fires, OR
   - Agent stops with context_exhausted signal
   ↓
4. Stop hook fires → writes final checkpoint with:
   - Summary of all work done
   - Explicit "next steps" for continuation
   - List of files modified with descriptions
   ↓
5. Scherzo's AgentProcess detects agent stopped
   ↓
6. Coordinator checks: crash vs context exhaustion vs completion
   - If context exhaustion: trigger handoff
   - If completion: mark task done
   - If crash: retry or fail based on policy
   ↓
7. Handoff: Coordinator spawns fresh agent with continuation prompt:

   "Continue task <task-id>: <title>

   Previous agent completed the following work:
   <work_summary from checkpoint>

   Files modified:
   <list of files with change summaries>

   Remaining work:
   <next_steps from checkpoint>

   Review changes with: jj diff -r <change-id>
   Continue working in jj change: <change-id>"

   ↓
8. New agent continues, inheriting the jj change and checkpoint history
```

#### Hook Generation

Scherzo generates provider-specific hook configs. Example for Claude Code:

```toml
# .claude/hooks/scherzo.toml (generated)
[[hooks]]
event = "PostToolUse"
match_tools = ["Edit", "Write", "Bash"]
command = "scherzo checkpoint incremental --task-id={{task_id}} --agent-id={{agent_id}}"

[[hooks]]
event = "PreCompact"
command = "scherzo checkpoint pre-compact --task-id={{task_id}} --agent-id={{agent_id}}"

[[hooks]]
event = "Stop"
command = "scherzo checkpoint final --task-id={{task_id}} --agent-id={{agent_id}}"

[[hooks]]
event = "Notification"
match_message = "context"
command = "scherzo checkpoint notify --task-id={{task_id}} --type=context-warning"
```

The `scherzo checkpoint` CLI commands are lightweight - they write JSON to `.scherzo/checkpoints/` and optionally prompt the agent for a summary.

### Completion Gates (Quality Pipeline)

Before a task is marked complete, it passes through a configurable pipeline of validation gates. This ensures quality without needing a full workflow DSL.

#### Boundary: Agent Hooks vs Orchestrator Gates

| Mechanism | Responsibility | Examples |
|-----------|---------------|----------|
| **Agent hooks** (Claude/Codex native) | Command-based validation | Tests, lint, format, type-check, build |
| **Orchestrator gates** (Scherzo) | Multi-agent coordination | Review agents, human approval, Rule of Five |

Tests and lint are handled by the agent's native `Stop` hook - if the hook fails, the agent continues working. Scherzo only adds orchestrator-level gates that require spawning additional agents or human interaction.

#### Gate Types

```gleam
pub type Gate {
  // Spawn a review agent with specific focus
  ReviewGate(
    name: String,
    agent_provider: AgentProvider,
    review_prompt: String,     // What to focus on
    pass_criteria: String,     // How agent signals "pass"
  )

  // Multiple review passes (Rule of Five)
  MultiPassReview(
    name: String,
    agent_provider: AgentProvider,
    passes: List(ReviewPass),  // Each pass has different focus
    require_convergence: Bool, // Stop when agent says "converged"
  )

  // Human approval checkpoint
  HumanGate(
    name: String,
    prompt: String,            // What to show human
  )
}

pub type ReviewPass {
  ReviewPass(
    focus: String,             // "correctness", "security", "performance", etc.
    prompt: String,
  )
}
```

#### Example Configuration

**Agent hooks** (handled by Claude/Codex natively):
```toml
# .claude/settings.toml (or injected by Scherzo)
[[hooks]]
event = "Stop"
command = "npm test && npm run lint && npm run typecheck"
# Agent continues working if this fails
```

**Orchestrator gates** (Scherzo):
```toml
# .scherzo/config.toml

[[completion_gates]]
name = "code_review"
type = "review"
agent = "claude"
prompt = """
Review this change for:
- Correctness: Does it do what the task asked?
- Code quality: Is it clean, readable, maintainable?
- Edge cases: Are error cases handled?

Respond with PASS or FAIL with explanation.
"""
pass_criteria = "PASS"

[[completion_gates]]
name = "security_review"
type = "review"
agent = "claude"
prompt = """
Security review this change for:
- Input validation
- Injection vulnerabilities (SQL, XSS, command)
- Authentication/authorization issues
- Sensitive data exposure

Respond with PASS or FAIL with explanation.
"""
pass_criteria = "PASS"

[[completion_gates]]
name = "rule_of_five"
type = "multi_pass"
agent = "claude"
require_convergence = true

[[completion_gates.passes]]
focus = "correctness"
prompt = "Review for logical correctness and bugs"

[[completion_gates.passes]]
focus = "completeness"
prompt = "Review for missing functionality or incomplete implementation"

[[completion_gates.passes]]
focus = "edge_cases"
prompt = "Review for unhandled edge cases and error conditions"

[[completion_gates.passes]]
focus = "clarity"
prompt = "Review for code clarity and maintainability"

[[completion_gates.passes]]
focus = "final"
prompt = "Final review - is this ready to merge? Say CONVERGED if satisfied."
```

#### Gate Execution Flow

```
Task Agent working...
         ↓
    ┌─────────────────┐
    │  Agent's Stop   │ ←──── fail: agent continues working
    │  hook (tests,   │       (native Claude/Codex behavior)
    │  lint, etc.)    │
    └────────┬────────┘
             ↓ pass (agent signals done)
             ↓
    ┌─────────────────────────────────────────┐
    │         Scherzo Orchestrator Gates       │
    ├─────────────────┬───────────────────────┤
    │  code_review    │ ←── fail: feedback to │
    │  (review agent) │     task agent, retry │
    ├─────────────────┼───────────────────────┤
    │ security_review │ ←── fail: feedback to │
    │  (review agent) │     task agent, retry │
    ├─────────────────┼───────────────────────┤
    │  rule_of_five   │ ←── iterate until     │
    │  (multi-pass)   │     CONVERGED         │
    └────────┬────────┴───────────────────────┘
             ↓ all gates pass
      Task Complete ✓
```

#### Retry Behavior

| Gate Type | On Failure |
|-----------|------------|
| ReviewGate | Feed review feedback to task agent, agent fixes, restart pipeline |
| MultiPassReview | Continue passes until convergence or max passes reached |
| HumanGate | Block until human approves/rejects |

#### Review Agents vs Task Agents

Review agents are **separate** from the task agent:
- They see only the jj diff (not full context)
- They can't modify code - only provide feedback
- Feedback goes back to the original task agent for fixes

This separation ensures reviews are unbiased by the task agent's reasoning.

## Project Structure

```
scherzo/
├── gleam.toml
├── src/
│   ├── scherzo.gleam              # Entry point, CLI (run, status, prime, checkpoint)
│   └── scherzo/
│       ├── core/
│       │   ├── types.gleam        # Core domain types
│       │   ├── task.gleam         # Task type
│       │   └── event.gleam        # Event types
│       ├── agent/
│       │   ├── driver.gleam       # Driver interface
│       │   ├── process.gleam      # Agent process actor (uses workspaces)
│       │   ├── workspace.gleam    # jj workspace management per agent ✓
│       │   ├── claude_settings.gleam # Generate .claude/settings.json for agents ✓
│       │   ├── pool.gleam         # Agent pool supervisor
│       │   ├── checkpoint.gleam   # Checkpoint types and persistence
│       │   ├── hooks.gleam        # Hook config generation
│       │   ├── handoff.gleam      # Context exhaustion handoff logic
│       │   └── drivers/
│       │       ├── claude.gleam   # Claude driver (sets SCHERZO_TASK_ID env) ✓
│       │       ├── codex.gleam    # Includes Codex hook generation
│       │       └── gemini.gleam
│       ├── task/
│       │   ├── source.gleam       # Task source interface
│       │   ├── queue.gleam        # Task queue actor
│       │   └── sources/
│       │       └── ticket.gleam   # Ticket (tk) integration
│       ├── vcs/
│       │   └── jj.gleam           # jujutsu operations
│       ├── orchestrator/
│       │   ├── supervisor.gleam   # Main supervision tree
│       │   └── coordinator.gleam  # Work coordination, gate triggering
│       ├── config/
│       │   ├── types.gleam        # ScherzoConfig, GateConfig, ReviewDimension
│       │   ├── parser.gleam       # Parse .scherzo/config.toml
│       │   ├── loader.gleam       # Merge formula defaults with user overrides
│       │   └── formulas/          # Default gate configurations (TOML)
│       │       ├── code-review.toml
│       │       ├── security-audit.toml
│       │       └── quick-review.toml
│       ├── gates/
│       │   ├── types.gleam        # Finding, Priority, GateFeedback, GateResult
│       │   ├── executor.gleam     # Main gate execution loop
│       │   ├── retry.gleam        # Retry logic, same/fresh agent decision
│       │   ├── feedback.gleam     # Format findings for agent consumption
│       │   ├── command.gleam      # Command gate (tests, lint, typecheck)
│       │   ├── parallel_review.gleam  # Parallel review executor
│       │   ├── synthesis.gleam    # Combine review findings
│       │   ├── multipass.gleam    # Sequential multi-pass review (Rule of Five)
│       │   └── human.gleam        # Human approval gate
│       ├── state/
│       │   └── store.gleam        # Persistent state management
│       ├── event/
│       │   └── bus.gleam          # Event bus
│       └── ui/
│           ├── tmux.gleam         # tmux session/pane management
│           ├── control.gleam      # Control pane REPL
│           └── layout.gleam       # Pane layout calculations
└── test/
```

## CLI Commands

```bash
# Start the console (creates tmux session with REPL, or attaches if exists)
scherzo console --workdir ./project

# Run tasks from tickets (headless, no tmux UI)
scherzo run --from-tickets --max-tasks 5 --workdir ./project

# Run a single task (headless)
scherzo run "task title" "task description" --workdir ./project

# Attach to existing tmux session (if you detached)
scherzo attach

# Standalone REPL (no tmux session management, for testing/debugging)
scherzo repl --workdir ./project

# Standalone commands (outside tmux session)
scherzo status
scherzo tasks list --status pending
scherzo squash  # Interactive jj squash of completed changes

# Hook commands (called by .claude/settings.json hooks in agent workspaces)
scherzo prime [task-id]       # Output task context for SessionStart hook ✓
scherzo checkpoint [task-id]  # Save state for Stop hook ✓
```

### Console vs REPL vs Attach

| Command | Creates Session | Has REPL | Use Case |
|---------|----------------|----------|----------|
| `console` | Yes (or attaches) | Yes | Primary user interface |
| `repl` | No | Yes | Testing, debugging, no tmux |
| `attach` | No | No | Re-attach after detaching |

## tmux UI

`scherzo console` creates a dedicated tmux session with the control REPL. This is the primary user interface for interacting with scherzo.

### Session Layout

```
┌─────────────────────────────────────────────────────────────┐
│ scherzo> status                              [Control Pane] │
│ Active: 3/4 agents | Pending: 12 | Completed: 5 | Failed: 1 │
│                                                             │
│ scherzo> _                                                  │
├─────────────────────────────────────────────────────────────┤
│ [Agent 1: abc-123] Feature: Add auth        │ [Agent 2: def│
│ Reading src/auth.gleam...                   │ Running tests │
│ ▌                                           │ ✓ 12 passed   │
├─────────────────────────────────────────────┼───────────────┤
│ [Agent 3: ghi-789] Bug: Fix login           │ [Agent 4]     │
│ Editing user_service.gleam                  │ (idle)        │
│ ...                                         │               │
└─────────────────────────────────────────────┴───────────────┘
```

### Control Pane Commands

```
status              # Show overall status
tasks               # List all tasks with status
agents              # Show agent status
pause <id>          # Pause an agent
resume <id>         # Resume a paused agent
retry <id>          # Retry a failed task
kill <id>           # Kill a running agent
focus <id>          # Expand agent pane to full screen
quit                # Graceful shutdown (finish current, don't start new)
abort               # Immediate shutdown (kill all agents)
```

### Implementation Notes

- Use `shellout` to call `tmux` commands from Gleam
- Session lifecycle: `tmux new-session -d -s scherzo`, `tmux kill-session -t scherzo`
- Pane creation: `tmux split-window`, `tmux select-layout tiled`
- Output routing: Each agent process writes to a named pipe, tmux pane tails the pipe
- Control pane: Gleam process reading stdin, parsing commands, sending messages to coordinator actor
- Dynamic layout: Recalculate pane layout when agents join/leave

## Key Dependencies

```toml
[dependencies]
gleam_stdlib = ">= 0.44.0"
gleam_otp = ">= 0.14.0"        # Actors, supervisors
gleam_erlang = ">= 1.3.0"      # Ports, processes
glint = ">= 1.0.0"             # CLI framework
simplifile = ">= 2.0.0"        # File operations (read/write .scherzo/ JSON)
gleam_json = ">= 2.0.0"        # JSON parsing for state files
shellout = ">= 1.6.0"          # Shell commands (jj, tmux, agent CLIs)
tom = ">= 1.0.0"               # TOML parsing for config
```

## Implementation Phases

### Phase 0: Development Environment
1. Nix flake: `flake.nix` with gleam, erlang, rebar3, jj, tmux
2. direnv: `.envrc` with `use flake`
3. GitHub Actions: `.github/workflows/ci.yml` for test/lint/build on PR
4. GitHub Actions: `.github/workflows/release.yml` for Burrito builds on tag
5. `CONTRIBUTING.md`: Dev setup instructions, workflow guidelines
6. **Milestone: `nix develop` provides complete dev environment, CI runs on push**

### Phase 1: Foundation ✅
7. Project setup: `gleam new scherzo`, gleam.toml with deps
8. Core types: `types.gleam`, `task.gleam`, `event.gleam`
9. CLI skeleton: `scherzo run`, `scherzo status` via glint
10. Event bus actor: Simple pub/sub with gleam_otp
11. Core task types and status definitions

### Phase 2: Single Agent E2E ✅
12. Agent driver interface: `driver.gleam`
13. Claude driver: Build command with --print, parse output
14. Agent process actor: Spawn via shellout, capture output
15. jj integration: Create change before agent, describe after
16. **Milestone: Run single task through single Claude agent**

### Phase 2.5: Agent Continuity (Checkpointing) ✅

**Workspace Isolation:**
- ✅ jj workspace per agent: `workspace.gleam` creates isolated workspaces in `.scherzo/workspaces/<task-id>/`
- ✅ Per-agent .claude/settings.json: `claude_settings.gleam` generates hooks for each agent
- ✅ Hook injection: SessionStart runs `scherzo prime`, Stop runs `scherzo checkpoint`
- ✅ Task context: Written to workspace `.scherzo/task.json`, read by `scherzo prime`
- ✅ CLI commands: `scherzo prime` and `scherzo checkpoint` implemented
- ✅ Environment variables: `SCHERZO_TASK_ID` set for agent processes

**Checkpointing & Handoff:**
- ✅ Checkpoint types: `checkpoint.gleam` with Checkpoint, FileChange types
- ✅ Checkpoint CLI: `scherzo checkpoint` writes state to `.scherzo/checkpoints/<task-id>/`
- ✅ Stop detection: AgentProcess detects exit reason (completion vs exhaustion vs crash)
- ✅ Handoff logic: `handoff.gleam` builds continuation prompt from checkpoint
- ✅ Continuation flow: Orchestrator spawns fresh agent with handoff context
- ✅ **Milestone: Agent survives context exhaustion, fresh agent continues task**

### Phase 3: Ticket Integration ✅
- ✅ Task source interface: `source.gleam`
- ✅ Ticket source: Parse `.tickets/*.md`, call `tk` CLI
- ✅ Task queue: Priority queue respecting dependencies (`tk ready`)
- ✅ Status sync: Update ticket status on completion/failure
- ✅ **Milestone: Process all ready tickets from a project**

### Phase 4: tmux UI
31. ✅ tmux session management: Create, attach, destroy (`ui/tmux.gleam`)
32. ✅ Pane layout: Control pane + agent panes (`ui/layout.gleam`)
33. ✅ Output routing: Named pipes for agent output (`ui/pipes.gleam`)
34. ✅ Session manager: Coordinates layout + pipes (`ui/session_manager.gleam`)
35. ✅ Control REPL: Parse commands, command registry (`ui/repl.gleam`, `ui/commands.gleam`)
36. ✅ Runner: Wires REPL + session together (`ui/runner.gleam`)
37. ✅ `scherzo console` CLI command: Entry point for tmux UI
38. ✅ **Milestone: Run with tmux UI, interactive control**

### Phase 5: Completion Gates

Quality gates that evaluate task completion before merge. When an agent signals "done" via Stop hook, gates run automatically. Failed gates feed back to the agent for fixes, looping until pass or max iterations.

See `docs/gate-execution.md` for detailed design and `docs/completion-gates.md` for review dimensions.

**5a. Orchestrator Refactor**
39. Split orchestrator: Refactor `orchestrator.gleam` into `orchestrator/supervisor.gleam` (supervision tree) and `orchestrator/coordinator.gleam` (work coordination, gate triggering)

**5b. Configuration Infrastructure**
40. Config types: `config/types.gleam` - ScherzoConfig, GateConfig, RetryConfig, ReviewDimension
41. Config parser: `config/parser.gleam` - load `.scherzo/config.toml` using `tom` library
42. Formula loader: `config/loader.gleam` - load TOML formula files from `config/formulas/`, merge with user overrides
43. Config validation: Ensure gates valid, prompts non-empty, no circular dependencies

**5c. Core Gate Types & Execution**
44. Task phases: Extend task.gleam with Working, Evaluating, NeedsFixes, GatesPassed, Stuck
45. Gate types: `gates/types.gleam` - Finding, Priority (P0-P3), Location, GateFeedback, GateResult
46. Gate events: Extend event.gleam with GateEvent variants (started, completed, failed, etc.)
47. Gate executor: `gates/executor.gleam` - main loop: run gates in order, collect results, handle pass/fail (always restarts from beginning on retry)

**5d. Retry & Feedback Loop**
48. Retry controller: `gates/retry.gleam` - track iterations, decide same-agent vs fresh-agent
49. Feedback builder: `gates/feedback.gleam` - format findings for agent consumption
50. Agent re-trigger: Extend AgentProcess to receive feedback and continue working
51. Coordinator integration: Wire gate completion signals into task lifecycle

**5e. Gate Implementations**
52. Command gate: `gates/command.gleam` - run shell commands (tests, lint, typecheck)
53. Parallel review: `gates/parallel_review.gleam` - spawn N review agents, one per dimension, aggregate all findings
54. Synthesis: `gates/synthesis.gleam` - combine reviewer findings, dedupe, prioritize (P0/P1/P2)
55. Multi-pass review: `gates/multipass.gleam` - sequential refinement (Rule of Five)
56. Human gate: `gates/human.gleam` - block indefinitely for human approval via control pane

**5f. UI & Testing**
57. UI integration: Show gate evaluation status in control pane (evaluating, needs fixes, stuck states)
58. Gate tests: Unit tests for executor, retry logic, feedback formatting, and individual gate types

59. **Milestone: Task completion triggers gate evaluation; failures loop back to agent; passes proceed to merge queue**

### Phase 6: Merge Queue

After completion gates pass, task changes enter a merge queue for integration into main. Works with single agent.

60. jj extensions: `jj.gleam` with rebase, conflict detection, squash, bookmark operations
61. Merge types: `merge/types.gleam` with MergeStatus, MergeRequest, MergeQueueConfig
62. Merge events: Extend `event.gleam` with merge queue events
63. CI runner: `merge/ci.gleam` for pre-merge verification
64. MergeQueue actor: `merge/queue.gleam` - sequential processing with rebase, CI, merge
65. Conflict resolution: `merge/resolution.gleam` - spawn agents for conflict resolution
66. Orchestrator integration: Wire merge queue into task completion flow
67. **Milestone: Completed tasks rebase, pass CI, squash, and merge to main automatically**

See `docs/merge-queue.md` for detailed design.

### Phase 7: Multi-Agent Orchestration

Scale from single agent to parallel execution. Builds on completion gates and merge queue.

68. Agent pool supervisor: Factory supervisor for N agents
69. Coordinator: Match tasks to available agents
70. Parallel execution: Multiple agents, multiple jj changes
71. Dynamic panes: Add/remove panes as agents start/stop
72. **Milestone: 4 agents in parallel with tmux visibility**

### Phase 8: Resilience & Recovery
73. Checkpoint recovery: Load checkpoints from `.scherzo/checkpoints/` on startup
74. Ticket state sync: Ensure ticket status reflects interrupted work
75. Crash detection: Compare checkpoints vs jj working copy on restart
76. Retry logic: Exponential backoff for failures
77. Resume flow: Detect interrupted tasks, rebuild from checkpoints + jj diff
78. Handoff metrics: Track continuation count per task, detect infinite loops
79. Supervision tree: Wire everything together

### Phase 9: Distribution
80. Additional drivers: Codex, Gemini (with provider-specific hooks)
81. Burrito build: Single binary for macOS/Linux
82. Polish: Better status display, colors, keybindings

## Failure Handling

1. **Agent crashes mid-task**: Captured by supervisor, recover from latest checkpoint if available, otherwise retry from start
2. **Context exhaustion**: Not a failure - trigger handoff to fresh agent with continuation prompt built from checkpoint
3. **Retryable failures**: Network errors, timeouts → exponential backoff, re-queue
4. **Non-retryable failures**: Bad task definition, code errors → mark terminal failure
5. **Orchestrator crash**: Task state survives in `.tickets/` files, checkpoints survive in `.scherzo/checkpoints/`. On restart: query tickets, load checkpoints, detect interrupted tasks, resume or retry
6. **Infinite handoff loop**: If task exceeds max continuation count (configurable), mark as failed with "task too complex" reason

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Task Source | Ticket (tk) via adapter | Start with Ticket, adapter pattern for future Beans support |
| Output Mode | Stream to terminal | Real-time visibility into agent work |
| Parallelism | Multiple parallel agents | Different agents work on different tasks concurrently |
| Version Control | jujutsu (jj) | Each agent creates a jj change, easier parallel work |
| Agent Continuity | Hook-based checkpointing | Leverage CLI agent hooks (Claude/Codex) for state persistence, enables handoff on context exhaustion |
| State Storage | Tickets as source of truth | Task state lives in `.tickets/` files, queried via ticket adapter. Runtime agent state in-memory. Checkpoints in `.scherzo/checkpoints/` for handoff. |
| Handoff Strategy | Continuation prompt | Build context from checkpoint + jj diff, let fresh agent continue naturally |
| Quality Assurance | Completion gates pipeline | Parallel code review with synthesis, configurable per-repo. See `docs/completion-gates.md`. |
| Review Strategy | Parallel review + synthesis | 7 dimensions run concurrently (correctness, performance, security, elegance, resilience, style, smells), then synthesis dedupes and prioritizes (P0/P1/P2). |
| Merge Strategy | Merge queue with CI gating | Sequential rebase, squash, CI verify, then merge to main. Conflicts spawn resolution agents. See `docs/merge-queue.md`. |

## Development

| Tool | Purpose | Rationale |
|------|---------|-----------|
| Nix + direnv | Dev environment | Reproducible, auto-activates on `cd`, pins Gleam/Erlang/OTP versions |
| GitHub Actions | CI pipeline | Test, lint, format check on PR; release builds on tag |
| Burrito | Release builds | Single binary distribution for macOS/Linux |
| jujutsu (jj) | Version control | Used for both Scherzo development and as runtime dependency |

### CI Pipeline

```yaml
# .github/workflows/ci.yml (conceptual)
on: [push, pull_request]
jobs:
  check:
    - gleam format --check
    - gleam test
    - gleam build
  release:
    if: startsWith(github.ref, 'refs/tags/')
    - burrito build (macos-arm64, macos-x64, linux-x64)
    - upload artifacts to GitHub release
```

### Dev Environment

```nix
# flake.nix provides:
# - gleam, erlang, rebar3
# - jj, tmux (runtime deps for testing)
# - claude CLI (optional, for integration tests)
```

Detailed setup instructions in `CONTRIBUTING.md` (created when project scaffolded).

## Task Source Adapter

Abstract interface for task backends:

```gleam
pub type TaskSource {
  TaskSource(
    name: String,
    fetch_tasks: fn() -> Result(List(Task), String),
    update_status: fn(Id, TaskStatus) -> Result(Nil, String),
    get_ready_tasks: fn() -> Result(List(Task), String),  // Deps resolved
  )
}
```

### Ticket Integration (Primary)

- Reads from `.tickets/*.md` (YAML frontmatter + markdown body)
- Uses `tk ready` to get tasks with resolved dependencies
- Uses `tk start <id>` / `tk close <id>` for status updates
- Uses `tk query --json` for structured data

### Beans Integration (Future)

- Reads from `.beans/` directory
- Uses `beans prime` for AI context
- GraphQL queries for structured access

## Version Control Strategy (jujutsu)

Each agent works in its own **jj workspace** (similar to git worktrees):

```bash
# Scherzo creates workspace for agent
jj workspace add .scherzo/workspaces/<task-id>

# Agent works in isolated workspace with its own working copy
# Workspace has its own .claude/settings.json with hooks

# After agent completes, workspace is cleaned up
jj workspace forget <task-id>
rm -rf .scherzo/workspaces/<task-id>
```

Benefits:
- **Full isolation**: Each agent has its own directory with its own `.claude` config
- **Hook injection**: Per-agent `SessionStart`/`Stop` hooks via workspace's `.claude/settings.json`
- **Context injection**: `scherzo prime` outputs task context, read from workspace's `.scherzo/task.json`
- Multiple agents can work in parallel without conflicts
- Easy to squash/reorder changes later
- No branch management overhead

## Conflict Handling for Parallel Agents

With jj workspaces, parallel work is fully isolated:

1. Each agent works in its own **workspace directory** (`.scherzo/workspaces/<task-id>/`)
2. Each workspace has its own working copy of all files
3. No coordination needed - agents can't interfere with each other
4. If agents touch same files: jj handles conflict markers at squash time
5. Coordinator can optionally prevent assigning tasks that touch same files

```bash
# Scherzo creates workspace for agent
jj workspace add .scherzo/workspaces/<task-id>

# Write agent-specific config
# .scherzo/workspaces/<task-id>/.claude/settings.json (hooks)
# .scherzo/workspaces/<task-id>/.scherzo/task.json (task metadata)

# Agent runs in workspace directory
cd .scherzo/workspaces/<task-id> && claude --print "..."

# SessionStart hook fires -> scherzo prime outputs context
# Agent does work...
# Stop hook fires -> scherzo checkpoint saves state

# After completion, cleanup workspace
jj workspace forget <task-id>
rm -rf .scherzo/workspaces/<task-id>
```
