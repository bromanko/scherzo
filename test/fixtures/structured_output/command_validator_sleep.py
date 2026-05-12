#!/usr/bin/env python3
import sys
import time
print("starting sleep", file=sys.stderr)
sys.stderr.flush()
time.sleep(2)
sys.stdin.read()
sys.exit(0)
