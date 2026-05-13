#!/usr/bin/env python3
import sys
sys.stdin.read()
sys.stdout.write("O" * 9000)
sys.stdout.flush()
sys.stderr.write("E" * 9000)
sys.stderr.flush()
sys.exit(1)
