from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scherzo_review import review_lane_contract as contract


class ProviderSchemaContractTest(unittest.TestCase):
    def write_schema(self, schema: dict) -> Path:
        tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(tmpdir.cleanup)
        path = Path(tmpdir.name) / "schema.json"
        path.write_text(json.dumps(schema), encoding="utf-8")
        return path

    def test_check_provider_schema_is_pure_python(self) -> None:
        schema_path = self.write_schema(
            {
                "type": "object",
                "required": ["summary"],
                "properties": {
                    "summary": {"type": "string", "minLength": 1},
                    "blocking": {"type": "boolean"},
                },
                "additionalProperties": False,
            }
        )

        with mock.patch.object(contract.subprocess, "run", side_effect=AssertionError("no subprocess")):
            schema = contract.check_provider_schema(schema_path)

        self.assertEqual(schema["type"], "object")

    def test_check_provider_schema_rejects_gleam_only_schema_features(self) -> None:
        schema_path = self.write_schema(
            {
                "type": "object",
                "$defs": {"x": {"type": "string"}},
                "properties": {"summary": {"$ref": "#/$defs/x"}},
            }
        )

        with self.assertRaises(contract.ContractError) as raised:
            contract.check_provider_schema(schema_path)

        self.assertEqual(raised.exception.code, "structured_output_tool_spec_provider_incompatible_schema")
        self.assertIn("disallowed keyword $defs", raised.exception.message)


if __name__ == "__main__":
    unittest.main()
