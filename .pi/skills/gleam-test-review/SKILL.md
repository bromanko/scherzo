---
name: gleam-test-review
description: This skill should be used when the user asks for "test review", "test coverage", "improve gleam tests", "review tests", "test quality", "testing audit", or wants analysis of Gleam test suites for coverage gaps, edge cases, and testing best practices.
---

# Gleam Test Review

**Action required:** Run `/review gleam test` to start an interactive test review. Do not perform the review manually.

---

<!-- The content below is used by the /review command as review instructions -->

Analyze Gleam test code for coverage gaps, edge case handling, test quality, and testing best practices.

## Scope Determination

First, determine what to review:

1. **If the user specifies test files**: Review those paths
2. **If the user specifies source files**: Find corresponding tests and review coverage
3. **If no scope specified**: Review working changes
   - Check for `.jj` directory first, use `jj diff` if present
   - Otherwise use `git diff` to identify changed files
   - Look at both changed source and test files

Test files are typically in `test/` directory with `_test.gleam` suffix.

## Review Process

1. **Map source to tests**: Identify which source modules have test coverage
2. **Analyze test coverage**: Check if key functionality is tested
3. **Review test quality** using checklist below
4. **Identify missing edge cases**
5. **Output findings** in the standard format

## Test Review Checklist

### Coverage Gaps
- Public functions have corresponding tests
- Error paths tested (Result.Error cases)
- Edge cases covered:
  - Empty collections ([], dict.new())
  - Zero/negative numbers where applicable
  - Empty strings
  - Boundary conditions
  - None/Option.None cases
- Integration points tested (module interactions)

### Test Quality
- Tests are focused (one concept per test)
- Test names describe behavior, not implementation
- Assertions are specific (not just "returns Ok")
- Tests are deterministic (no flaky tests)
- Setup/teardown handled appropriately
- Tests run independently (no order dependencies)

### Skipped & Disabled Tests
- Gleam has no built-in skip mechanism, so watch for workarounds: commented-out tests, renamed test functions (e.g., `_test` prefix to prevent discovery), or emptied test bodies
- Look for tests with trivially passing assertions replacing what should be real checks (a sign the test was gutted to pass)
- Watch for `TODO`/`FIXME` comments suggesting the test was too hard to fix and was bypassed instead
- These patterns are **HIGH severity** when they appear to be workarounds (e.g., an LLM disabling a test it couldn't fix rather than addressing the underlying failure)

### Test Organization
- Test files mirror source structure
- Related tests grouped logically
- Helper functions extracted for common setup
- Test data defined clearly
- No duplicate test logic

### Assertion Patterns
- Use `should.equal` for exact matches
- Use `should.be_true`/`should.be_false` for booleans
- Use `should.be_ok`/`should.be_error` for Results
- Error messages helpful for debugging
- Avoid overly broad assertions

### Edge Case Coverage
For each function, consider:
- What happens with empty input?
- What happens at boundaries (0, max_int, etc.)?
- What happens with invalid input?
- What happens with very large input?
- Are all pattern match branches exercised?

### Property-Based Testing Opportunities
Identify functions that would benefit from property tests:
- Pure functions with clear invariants
- Serialization/deserialization (roundtrip property)
- Mathematical operations (commutativity, associativity)
- Collection operations (length preservation, etc.)
- Parsers (valid input always parses)

Suggest using `gleeunit` with property testing libraries if applicable.

### Test Isolation
- External services mocked/stubbed
- File system interactions isolated
- Time-dependent tests use controlled time
- Random values seeded for reproducibility
- No global state pollution between tests

### Error Scenario Testing
- Invalid input handled gracefully
- External failures handled (network, file, etc.)
- Concurrent access scenarios (if applicable)
- Resource exhaustion considered
- Timeout behavior tested

## Output Format

Present findings as:

```markdown
## Findings

### [SEVERITY] Issue Title
**File:** `path/to/file_test.gleam:LINE` (or source file if missing tests)
**Category:** testing

**Issue:** Description of the testing gap or quality issue.

**Suggestion:** What to test or how to improve, with example if helpful.

**Effort:** trivial|small|medium|large

---
```

Use severity indicators:
- HIGH: Critical untested paths, missing error handling tests
- MEDIUM: Missing edge cases, unclear test intent
- LOW: Test organization, minor improvements

## Summary

After all findings, provide:
- Total count by severity
- Coverage summary (modules with/without tests)
- Top testing priorities
- Property testing candidates
- Overall test suite assessment (1-2 sentences)
