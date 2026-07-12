#!/usr/bin/env python3
import sys

sys.stdin.read()
print("SCHERZO_FAILURE_CODE=implementation_noop", file=sys.stderr)
print("implementation produced no workflow-baseline changes", file=sys.stderr)
sys.exit(1)
