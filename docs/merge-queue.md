# Merge Queue

After completion gates pass, task changes enter a merge queue for integration into the main branch. Changes are rebased, verified via CI, squashed, and merged sequentially. Conflicts spawn resolution agents.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Queue entry | Automatic after gates pass | Seamless flow from completion |
| Conflicts | Spawn resolution agent | Autonomous handling |
| Pre-merge checks | Full CI | Ensure main stays green |
| Architecture | Separate MergeQueue actor | Single responsibility, follows TaskQueue pattern |
| Squashing | Squash to single commit | Clean main history, one commit per task |
| CI parallelism | Sequential | Simpler, avoids wasted compute on conflicts |

## State Machine

```
TaskCompleted → [Gates Pass] → ReadyForMerge → InMergeQueue
                                                    ↓
                                               Rebasing
                                              /        \
                                      (success)     (conflict)
                                         ↓              ↓
                                    CIRunning      Conflicted
                                    /      \           ↓
                               (pass)    (fail)   Resolving
                                  ↓         ↓         ↓
                              Merged    CIFailed  [re-queue]
                                          ↓
                                       Blocked
```

## Types

### MergeStatus

```gleam
pub type MergeStatus {
  ReadyForMerge
  InMergeQueue(position: Int)
  Rebasing
  Conflicted(files: List(String))
  Resolving(agent_id: Id)
  CIRunning(started_at: Timestamp)
  Merged(merged_at: Timestamp)
  CIFailed(reason: String, attempts: Int)
  Blocked(reason: String)
}
```

### MergeRequest

```gleam
pub type MergeRequest {
  MergeRequest(
    task_id: Id,
    change_id: String,
    status: MergeStatus,
    enqueued_at: Timestamp,
    priority: Priority,
  )
}
```

### MergeQueueConfig

```gleam
pub type MergeQueueConfig {
  MergeQueueConfig(
    main_bookmark: String,        // Default: "main"
    ci_command: Option(String),   // e.g., "gleam test"
    ci_timeout_ms: Int,           // Default: 300000 (5 min)
    max_ci_retries: Int,          // Default: 2
  )
}
```

## Events

```gleam
MergeEnqueued(task_id: Id, change_id: String, timestamp: Timestamp)
MergeStarted(task_id: Id, change_id: String, timestamp: Timestamp)
RebaseCompleted(task_id: Id, had_conflicts: Bool, timestamp: Timestamp)
ConflictDetected(task_id: Id, files: List(String), timestamp: Timestamp)
ResolutionSpawned(task_id: Id, resolution_agent_id: Id, timestamp: Timestamp)
ConflictResolved(task_id: Id, timestamp: Timestamp)
CIStarted(task_id: Id, timestamp: Timestamp)
CICompleted(task_id: Id, passed: Bool, timestamp: Timestamp)
MergeCompleted(task_id: Id, change_id: String, timestamp: Timestamp)
MergeBlocked(task_id: Id, reason: String, timestamp: Timestamp)
```

## MergeQueue Actor

### Messages

```gleam
pub type Message {
  Enqueue(request: MergeRequest)
  ProcessNext
  RebaseResult(task_id: Id, result: Result(Nil, RebaseError))
  CIResult(task_id: Id, result: ci.CIResult)
  ResolutionComplete(task_id: Id, result: Result(Nil, String))
  GetStatus(reply_to: Subject(QueueStatus))
  Shutdown
}
```

### State

```gleam
pub type State {
  State(
    config: MergeQueueConfig,
    queue: List(MergeRequest),
    processing: Option(MergeRequest),
    blocked: Dict(Id, MergeRequest),
    event_bus: Subject(bus.Message),
    repo_dir: String,
  )
}
```

### Processing Logic

1. `ProcessNext` dequeues highest priority request
2. Fetch latest: `jj git fetch`
3. Rebase onto main: `jj rebase -r <change_id> -d main`
4. Check conflicts: `jj log -r <change_id> -T "conflict"`
5. If conflicts: spawn resolution agent, move to `Conflicted`
6. If clean: squash commits, run CI command
7. If CI passes: `jj bookmark set main -r <change_id>`, then `jj git push --bookmark main`
8. Publish events at each state transition

## CI Runner

```gleam
pub type CIResult {
  CIPassed(output: String, duration_ms: Int)
  CIFailed(output: String, exit_code: Int, duration_ms: Int)
  CITimeout
}

/// Run CI command in working directory
pub fn run(command: String, working_dir: String, timeout_ms: Int) -> CIResult

/// Run with retries for flaky tests
pub fn run_with_retries(command: String, working_dir: String, timeout_ms: Int, max_retries: Int) -> CIResult
```

## Conflict Resolution

### ResolutionContext

```gleam
pub type ResolutionContext {
  ResolutionContext(
    original_task: Task,
    change_id: String,
    conflicting_files: List(String),
    main_bookmark: String,
  )
}
```

### Resolution Flow

1. Detect conflicts after rebase (`jj log -r <change_id> -T "conflict"` returns `true`)
2. List conflicting files: `jj resolve --list -r <change_id>`
3. Create isolated workspace for resolution agent
4. Build resolution prompt with:
   - Original task description
   - List of conflicting files
   - Instructions to resolve and verify tests pass
   - Guidance to run `jj resolve` to mark resolution
5. Spawn resolution agent
6. On completion: re-queue the merge request (will re-rebase, re-run CI)

## jj Command Sequences

### Successful Merge

```bash
jj git fetch
jj rebase -r <change_id> -d main
# Check: jj log -r <change_id> -T "conflict" --no-graph → "false"

# Squash all task commits into one
jj squash -r <change_id>

# Run CI
<ci_command>

# Update main bookmark and push
jj bookmark set main -r <change_id>
jj git push --bookmark main
```

### Conflict Resolution

```bash
jj git fetch
jj rebase -r <change_id> -d main
# Check: jj log -r <change_id> -T "conflict" --no-graph → "true"
jj resolve --list -r <change_id>  # Get conflicting files

# Spawn resolution agent in new workspace
# Agent resolves conflicts, runs jj resolve for each file
# On success: re-queue → re-rebase → squash → CI → merge
```

## jj Module Extensions

Add to `src/scherzo/vcs/jj.gleam`:

```gleam
/// Rebase a change onto a destination
pub fn rebase(working_dir: String, change_id: ChangeId, dest: String) -> Result(Nil, String)
// Command: jj rebase -r <change_id> -d <dest>

/// Check if a change has conflicts
pub fn has_conflicts(working_dir: String, change_id: ChangeId) -> Result(Bool, String)
// Command: jj log -r <change_id> -T "conflict" --no-graph

/// List conflicting files for a change
pub fn list_conflicts(working_dir: String, change_id: ChangeId) -> Result(List(String), String)
// Command: jj resolve --list -r <change_id>

/// Squash a change into its parent
pub fn squash(working_dir: String, change_id: ChangeId) -> Result(Nil, String)
// Command: jj squash -r <change_id>

/// Set a bookmark to point at a change
pub fn bookmark_set(working_dir: String, name: String, change_id: ChangeId) -> Result(Nil, String)
// Command: jj bookmark set <name> -r <change_id>

/// Get the change ID a bookmark points to
pub fn bookmark_get(working_dir: String, name: String) -> Result(ChangeId, String)
// Command: jj log -r <name> -T "change_id" --no-graph

/// Fetch from git remote
pub fn git_fetch(working_dir: String) -> Result(Nil, String)
// Command: jj git fetch

/// Push a bookmark to remote
pub fn git_push_bookmark(working_dir: String, bookmark: String) -> Result(Nil, String)
// Command: jj git push --bookmark <bookmark>
```

## Files

| File | Purpose |
|------|---------|
| `src/scherzo/vcs/jj.gleam` | Extend with rebase, conflicts, bookmarks |
| `src/scherzo/merge/types.gleam` | MergeStatus, MergeRequest, MergeQueueConfig |
| `src/scherzo/merge/queue.gleam` | MergeQueue actor |
| `src/scherzo/merge/ci.gleam` | CI runner |
| `src/scherzo/merge/resolution.gleam` | Conflict resolution agent spawning |
| `src/scherzo/core/event.gleam` | Add merge events |
| `src/scherzo/orchestrator.gleam` | Wire merge queue into completion flow |
