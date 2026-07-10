#!/usr/bin/env python3
import sys

sys.stdin.read()
print("SCHERZO_FAILURE_CODE=implementation_incomplete_noop", file=sys.stderr)
print("implementation completion rejected", file=sys.stderr)
sys.exit(1)
