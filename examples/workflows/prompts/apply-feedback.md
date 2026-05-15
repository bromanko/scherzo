Apply the feedback from the completed validation and review steps for task {{ issue.identifier }}.

The first test command exited with {{ steps.test_after_implement.exit_code }}.

Test stdout:
{{ steps.test_after_implement.stdout }}

Test stderr:
{{ steps.test_after_implement.stderr }}

The code review said:
{{ steps.code_review.final_response }}

The security review said:
{{ steps.security_review.final_response }}

The performance review said:
{{ steps.performance_review.final_response }}

Fix blocking findings, keep the change focused, and summarize what you changed.
