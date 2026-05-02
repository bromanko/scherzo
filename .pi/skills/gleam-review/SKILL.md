---
name: gleam-review
description: This skill should be used when the user asks to "review Gleam", "full Gleam review", "review all Gleam", "comprehensive Gleam review", or wants a complete review covering code quality, security, performance, and testing for Gleam code.
---

# Gleam Full Review

**Action required:** Run the `/review gleam` command. Do not perform the review manually.

The `/review` command provides an interactive TUI that:
- Runs code, security, performance, and test review skills
- Presents findings one at a time
- Lets the user choose to fix, skip, or stop for each finding

If the user wants only specific review types, run `/review gleam <types>` where types can be: `code`, `security`, `performance`, `test`

Examples:
- Full review: `/review gleam`
- Code and security only: `/review gleam code security`
- Tests only: `/review gleam test`
