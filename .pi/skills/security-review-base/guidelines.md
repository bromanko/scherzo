# Shared Security Review Guidelines

These guidelines apply to all language-specific security reviews. They define
false-positive filtering, confidence scoring, output format, and general
precedent rules that reduce noise and focus findings on real, exploitable
vulnerabilities.

## Objective

Identify **high-confidence, exploitable security vulnerabilities** with real
impact. This is not a general code quality review — focus only on security
implications. Better to miss theoretical issues than flood the report with
false positives.

## Confidence Requirements

- Only report findings where you are **>80% confident** of actual exploitability
- Assign each finding a confidence score (1–10)
- **Discard findings scoring below 7**
- Include the confidence score in the output

Confidence scale:
- **9–10:** Certain exploit path identified
- **8:** Clear vulnerability pattern with known exploitation methods
- **7:** Suspicious pattern requiring specific conditions to exploit
- **Below 7:** Do not report (too speculative)

## False-Positive Filtering

### Hard Exclusions — Do NOT Report

1. Denial of Service (DoS) vulnerabilities or resource exhaustion attacks
2. Secrets or credentials stored on disk if they are otherwise secured
3. Rate limiting concerns or service overload scenarios
4. Memory consumption or CPU exhaustion issues
5. Missing input validation on non-security-critical fields without a proven
   security impact
6. Input sanitization concerns for CI/CD workflows unless clearly triggerable
   via untrusted input
7. A lack of hardening measures — code is not expected to implement all security
   best practices; only flag concrete vulnerabilities
8. Race conditions or timing attacks that are theoretical rather than practical;
   only report a race condition if it is concretely problematic
9. Vulnerabilities related to outdated third-party libraries (managed separately)
10. Memory safety issues in memory-safe languages (e.g., Rust, Gleam, Elm)
11. Files that are only unit tests or only used as part of running tests
12. Log spoofing — outputting unsanitized user input to logs is not a vulnerability
13. SSRF vulnerabilities that only control the path; SSRF is only a concern if
    it can control the host or protocol
14. Including user-controlled content in AI system prompts is not a vulnerability
15. Regex injection — injecting untrusted content into a regex is not a vulnerability
16. Regex DoS concerns
17. Insecure documentation — do not report findings in markdown or doc files
18. A lack of audit logs is not a vulnerability

### General Precedent Rules

1. Logging high-value secrets in plaintext is a vulnerability. Logging URLs is
   assumed safe.
2. UUIDs can be assumed unguessable and do not need extra validation.
3. Environment variables and CLI flags are trusted values. Attacks relying on
   controlling an environment variable are invalid.
4. Resource management issues such as memory or file descriptor leaks are not
   valid security findings.
5. Subtle or low-impact web vulnerabilities such as tabnabbing, XS-Leaks,
   prototype pollution, and open redirects should not be reported unless
   extremely high confidence.
6. Most CI/CD workflow vulnerabilities are not exploitable in practice. Before
   reporting, ensure a concrete and specific attack path where untrusted input
   triggers the vulnerability.
7. Only include MEDIUM findings if they are obvious and concrete issues.
8. Logging non-PII data is not a vulnerability even if the data may be
   sensitive. Only report logging vulnerabilities if they expose secrets,
   passwords, or personally identifiable information (PII).
9. Command injection in shell scripts is generally not exploitable since shell
   scripts rarely run with untrusted user input. Only report if there is a
   concrete untrusted-input path.
10. Vulnerabilities in notebooks (`.ipynb`) are rarely exploitable. Only report
    with a concrete attack path where untrusted input triggers the issue.

### Signal Quality Criteria

For each remaining finding, verify:

1. Is there a **concrete, exploitable vulnerability** with a clear attack path?
2. Does this represent a **real security risk** vs. a theoretical best practice?
3. Are there **specific code locations** and a plausible exploitation scenario?
4. Would this finding be **actionable** for a security team?

## Severity Guidelines

Focus on **HIGH and MEDIUM** findings only. Only include LOW findings if
specifically requested by the user.

- **HIGH:** Directly exploitable vulnerabilities leading to RCE, data breach,
  authentication bypass, or unauthorized access
- **MEDIUM:** Vulnerabilities requiring specific conditions but with significant
  impact if exploited

## Output Format

Present findings as:

````markdown
## Findings

### [SEVERITY] Issue Title (Confidence: N/10)
**File:** `path/to/file:LINE`
**Category:** security

**Issue:** Description of the vulnerability and potential impact.

**Exploit Scenario:** Concrete description of how an attacker would exploit
this — include specific inputs, request paths, or data flows.

**Suggestion:** How to remediate, with code example if helpful.

**Effort:** trivial|small|medium|large

---
````

## Summary

After all findings, provide:
- Total count by severity
- Critical items requiring immediate attention
- Attack surface summary (entry points identified)
- Dependency risk assessment
- Overall security posture (1–2 sentences)
