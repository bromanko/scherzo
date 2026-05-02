---
name: gleam-security-review
description: This skill should be used when the user asks for "security review", "vulnerability scan", "audit gleam security", "security audit", "find vulnerabilities", "check for security issues", or wants a deep security analysis of Gleam code including FFI safety, input validation, and dependency concerns.
---

# Gleam Security Review

**Action required:** Run `/review gleam security` to start an interactive security review. Do not perform the review manually.

---

<!-- The content below is used by the /review command as review instructions -->

Perform a comprehensive security audit of Gleam code, examining input validation, FFI boundaries, secrets handling, and potential vulnerabilities.

**Before starting the review, read the shared security review guidelines** at
[`../security-review-base/guidelines.md`](../security-review-base/guidelines.md)
for false-positive filtering rules, confidence scoring, and output format
requirements. Apply those rules throughout this review.

## Scope Determination

First, determine what code to review:

1. **If the user specifies files/directories**: Review those paths
2. **If no scope specified**: Review working changes
   - Check for `.jj` directory first, use `jj diff` if present
   - Otherwise use `git diff` to identify changed `.gleam` files
   - If no changes, ask the user what to review

## Review Process

1. **Understand existing security posture**: Identify security patterns,
   validation helpers, FFI wrappers, and established conventions already in the
   codebase before reviewing changes. Look for how the project currently handles
   input validation, FFI boundaries, and error propagation.
2. **Map the attack surface**: Identify entry points (HTTP handlers, CLI args,
   file inputs, external service calls)
3. **Trace data flow**: Follow untrusted input through the codebase
4. **Check each security domain** below
5. **Review dependencies**: Check `gleam.toml` for known issues
6. **Apply false-positive filtering** from the shared guidelines and the
   Gleam-specific rules below
7. **Output findings** in the format specified by the shared guidelines

## Gleam-Specific False-Positive Rules

In addition to the shared guidelines:

- Gleam is a memory-safe language on the BEAM — **do not report memory safety
  issues** in pure Gleam code; only flag unsafe patterns at FFI boundaries
  (Erlang NIFs, JavaScript FFI)
- BEAM process isolation provides strong safety guarantees — focus on FFI
  boundaries and data validation rather than process-level isolation concerns
- `panic` and `todo` in non-production code paths (tests, development utilities)
  are not security issues — only flag them in code reachable by production
  request handling
- Gleam's type system prevents many injection classes at compile time — focus on
  boundaries where data enters or exits the Gleam runtime (HTTP, FFI, file I/O,
  database queries)
- Missing `Result` handling in internal helper functions is a code quality issue,
  not a security issue, unless the unhandled error leads to a concrete bypass or
  data exposure

## Security Checklist

### Input Validation
- All external input validated at system boundaries
- String inputs checked for length limits
- Numeric inputs bounded appropriately
- File paths sanitized (no path traversal)
- URLs validated before use
- No assumption that input matches expected format

### FFI Safety (Erlang/JavaScript Interop)
- External functions wrapped with proper error handling
- Erlang NIFs reviewed for memory safety
- JavaScript FFI inputs sanitized
- Return types from FFI properly validated
- No blind trust of external function results
- Crashes in FFI code handled gracefully

### Secrets Handling
- No hardcoded secrets, API keys, or credentials
- Environment variables used for sensitive config
- Secrets not logged or included in error messages
- Config files with secrets not committed to repo
- Database connection strings properly secured

### Dependency Security
- Check `gleam.toml` dependencies:
  - Are packages from trusted sources?
  - Are there known vulnerabilities? (check hex.pm advisories)
  - Are dependencies pinned to specific versions?
  - Any unnecessary dependencies that increase attack surface?

### External Service Calls
- HTTP requests use HTTPS
- API responses validated before use
- Timeouts configured for external calls
- No SQL/command injection in queries
- Proper escaping for any interpolated values

### Process Isolation (BEAM-specific)
- Sensitive operations isolated in separate processes
- Process crashes don't leak sensitive data
- Supervision trees handle failures gracefully
- Message passing doesn't expose internal state inappropriately

### Unsafe Patterns
- `panic` and `todo` not used in production request-handling paths
- No `assert` on untrusted input
- Error messages don't leak internal details to end users
- Debug/development code not in production paths

### Authentication & Authorization
- Auth checks at appropriate boundaries
- No auth bypass through alternate code paths
- Session/token handling secure
- Privilege escalation paths reviewed
- Consistent auth enforcement

### Data Exposure
- Sensitive fields not serialized unintentionally
- Logs don't contain PII or secrets
- Error responses don't leak implementation details
- Debug endpoints disabled in production
