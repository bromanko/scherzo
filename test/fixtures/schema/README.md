# Durable and protocol schema fixtures

These fixtures back `test/schema_guardrail_test.gleam`. They are intentionally checked in as small golden examples for Scherzo's durable ledger, projection snapshot, control protocol v1, workflow DAG YAML, and orchestrator config YAML schemas.

When adding or changing a durable/protocol variant or persisted field, update the matching constructor manifest/example in `schema_guardrail_test.gleam` and refresh the fixture in this directory in the same change. A stale manifest or golden fixture should fail unit tests so partial schema wiring is obvious to future agents.
