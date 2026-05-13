#!/usr/bin/env python3
import json
import sys

print(json.dumps({
    "status": "error",
    "code": "json_schema_config_error",
    "message": "jsonschema import failed: No module named jsonschema",
    "instance_path": "",
    "schema_path": "",
    "schema_file": "test/fixtures/structured_output/review_lane_draft.schema.json",
    "draft": "2020-12",
}, separators=(",", ":"), sort_keys=True))
sys.exit(2)
