---
name: gleam-performance-review
description: This skill should be used when the user asks to "review performance", "optimize gleam", "BEAM performance", "performance audit", "find bottlenecks", "improve efficiency", or wants analysis of Gleam code performance focusing on BEAM runtime patterns, process architecture, and data structure choices.
---

# Gleam Performance Review

**Action required:** Run `/review gleam performance` to start an interactive performance review. Do not perform the review manually.

---

<!-- The content below is used by the /review command as review instructions -->

Analyze Gleam code for performance issues, focusing on BEAM-specific patterns, process architecture, and efficient data handling.

## Scope Determination

First, determine what code to review:

1. **If the user specifies files/directories**: Review those paths
2. **If no scope specified**: Review working changes
   - Check for `.jj` directory first, use `jj diff` if present
   - Otherwise use `git diff` to identify changed `.gleam` files
   - If no changes, ask the user what to review

## Review Process

1. **Identify hot paths**: Find frequently executed code (request handlers, loops, recursive functions)
2. **Analyze data flow**: Track how data moves through the system
3. **Check BEAM-specific patterns** below
4. **Review process architecture** if using gleam_otp
5. **Output findings** in the standard format

## Performance Checklist

### Process Architecture (gleam_otp)
- Processes used for isolation, not just concurrency
- GenServer state kept minimal (large state = slow GC)
- Avoid single-process bottlenecks for high-throughput operations
- Process spawning bounded (no unbounded spawn from user input)
- Supervision trees appropriate for failure domains
- Consider process pooling for resource-intensive operations

### Message Passing
- Messages are small (large messages copied between process heaps)
- Avoid sending large binaries in messages (use references instead)
- No synchronous calls in hot paths when async would work
- Message queues monitored for buildup
- Consider selective receive implications

### Recursion & Iteration
- Tail-call optimization used (accumulator pattern)
- List operations efficient:
  - `list.reverse` at end of tail-recursive build, not repeated
  - Avoid `list.append` in loops (O(n) each time)
  - Use `list.fold` over manual recursion when appropriate
- Recursion depth bounded for untrusted input

### Data Structures
- Lists appropriate for sequential access, not random
- Consider `dict` for frequent key lookups
- Large collections: consider whether ETS would help
- Avoid repeated list traversals (cache intermediate results)
- String concatenation uses `string_builder` for multiple operations

### Binary & String Handling
- Large binaries handled efficiently (reference counted on BEAM)
- String operations minimize copying
- Binary pattern matching used where appropriate
- Avoid repeated binary-to-string conversions

### ETS Usage (if applicable)
- ETS tables for shared read-heavy data
- Appropriate table type (set, ordered_set, bag)
- Read concurrency enabled for read-heavy tables
- Write concurrency for write-heavy tables
- Avoid ETS for small, process-local data

### Hot Path Optimization
- Expensive operations not in request/response path
- Caching for repeated expensive computations
- Lazy evaluation where beneficial
- Avoid unnecessary allocations in loops
- Pattern match ordering: most common cases first

### I/O & External Calls
- External calls don't block critical paths
- Batch operations where possible
- Connection pooling for databases/services
- Timeouts prevent hanging on slow externals
- Consider async patterns for I/O-heavy operations

### Memory Considerations
- Large data structures don't live longer than needed
- Process heap size appropriate (consider hibernate for idle)
- No memory leaks from growing state
- Binary references released appropriately

## Output Format

Present findings as:

```markdown
## Findings

### [SEVERITY] Issue Title
**File:** `path/to/file.gleam:LINE`
**Category:** performance

**Issue:** Description of the performance concern and its impact.

**Suggestion:** How to optimize, with code example if helpful.

**Effort:** trivial|small|medium|large

---
```

Use severity indicators:
- HIGH: Significant bottleneck, unbounded growth, blocking issues
- MEDIUM: Suboptimal patterns, unnecessary overhead
- LOW: Minor optimizations, micro-improvements

## Summary

After all findings, provide:
- Total count by severity
- Top bottlenecks to address
- Hot paths identified
- Architecture recommendations (if applicable)
- Overall performance assessment (1-2 sentences)
