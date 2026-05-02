---
name: gleam-code-review
description: This skill should be used when the user asks to "review gleam code", "code quality review", "gleam idioms check", "review my gleam", "check code quality", or wants feedback on Gleam code patterns, idioms, Result handling, or module organization.
---

# Gleam Code Review

**Action required:** Run `/review gleam code` to start an interactive code quality review. Do not perform the review manually.

---

<!-- The content below is used by the /review command as review instructions -->

Perform a thorough code quality review of Gleam code, focusing on idiomatic patterns, proper error handling, and clean architecture.

## Scope Determination

First, determine what code to review:

1. **If the user specifies files/directories**: Review those paths
2. **If no scope specified**: Review working changes
   - Check for `.jj` directory first, use `jj diff` if present
   - Otherwise use `git diff` to identify changed `.gleam` files
   - If no changes, ask the user what to review

## Review Process

1. **Read the target files** using the Read tool
2. **Analyze against the checklist** below
3. **Output findings** in the standard format

## Review Checklist

### Idiomatic Patterns
- Uses pipeline operator (`|>`) for data transformations
- Pattern matching is exhaustive (no catch-all `_` when specific cases should be handled)
- Prefers pattern matching over nested conditionals
- Uses `use` expressions appropriately for callbacks and resource management
- Avoids deeply nested expressions

### Result/Option Handling
- No silent error swallowing (e.g., `result.unwrap` without good reason)
- Errors propagate with meaningful context
- Uses `result.try` or `use` for chaining fallible operations
- Option types used for optional values, not Result
- Early returns via pattern matching for error cases

### Module Organization
- Public functions (`pub fn`) form a clear, minimal API
- Private helper functions are truly internal
- Related functionality grouped in same module
- Module names reflect their purpose
- Avoids circular dependencies

### Naming Conventions
- Functions use `snake_case`
- Types use `PascalCase`
- Module names use `snake_case`
- Names are descriptive but not overly verbose
- Boolean functions/fields prefixed appropriately (e.g., `is_valid`, `has_items`)

### Type Design
- Custom types preferred over tuples for domain concepts
- Variant types model states explicitly (avoid stringly-typed code)
- Opaque types used to hide implementation details when appropriate
- Type aliases used sparingly and meaningfully

### Code Clarity
- Functions are focused (single responsibility)
- Complex logic has explanatory comments
- No dead code or unused imports
- Consistent formatting

## Output Format

Present findings as:

```markdown
## Findings

### [SEVERITY] Issue Title
**File:** `path/to/file.gleam:LINE`
**Category:** quality

**Issue:** Description of what's wrong and why it matters.

**Suggestion:** How to fix, with code example if helpful.

**Effort:** trivial|small|medium|large

---
```

Use severity indicators:
- HIGH: Bugs, incorrect behavior, serious maintainability issues
- MEDIUM: Non-idiomatic code, unclear patterns, missing error handling
- LOW: Style issues, minor improvements, optional enhancements

## Summary

After all findings, provide:
- Total count by severity
- Top 2-3 priority items to address
- Overall code quality assessment (1-2 sentences)
