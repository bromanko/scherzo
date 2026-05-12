#!/usr/bin/env python3
import sys
payload = sys.stdin.read()
if "TOPSECRET" in payload:
    print("secret leaked to stdin", file=sys.stderr)
print("reject secret=TOPSECRET", file=sys.stderr)
sys.exit(1)
