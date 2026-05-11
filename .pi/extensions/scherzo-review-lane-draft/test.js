import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";

const here = path.dirname(new URL(import.meta.url).pathname);
const repoRoot = path.resolve(here, "../../..");
const schemaSource = fs.readFileSync(path.join(here, "review_lane_draft_schema.ts"), "utf8");
const indexSource = fs.readFileSync(path.join(here, "index.ts"), "utf8");
const documentedSchema = JSON.parse(
  fs.readFileSync(path.join(repoRoot, "docs/schemas/review-artifacts.v1.schema.json"), "utf8"),
);
const validFixture = JSON.parse(
  fs.readFileSync(path.join(repoRoot, "test/fixtures/review_lane_draft/valid-minimal.json"), "utf8"),
);
const remoteMutationFixture = JSON.parse(
  fs.readFileSync(path.join(repoRoot, "test/fixtures/review_lane_draft/invalid-remote-mutations.json"), "utf8"),
);

const reviewLaneDraft = documentedSchema.definitions?.ReviewLaneDraft ?? documentedSchema.$defs?.ReviewLaneDraft;
const reviewLaneDraftBody = reviewLaneDraft?.allOf?.find((entry) => Array.isArray(entry.required)) ?? reviewLaneDraft;
assert.ok(reviewLaneDraftBody, "documented ReviewLaneDraft schema body must exist");
assert.deepEqual(
  [...reviewLaneDraftBody.required].sort(),
  [
    "artifact_type",
    "draft_findings",
    "evidence_requests",
    "generated_at_utc",
    "input_refs",
    "lane",
    "producer",
    "remote_mutations",
    "review_notes",
    "schema_version",
    "self_check",
  ].sort(),
);

for (const key of reviewLaneDraftBody.required) {
  assert.ok(schemaSource.includes(`"${key}"`), `TypeBox schema source should mention ${key}`);
  assert.ok(Object.hasOwn(validFixture, key), `valid fixture should contain ${key}`);
}

assert.match(schemaSource, /artifact_type:\s*StringEnum\(\["review_lane_draft"\] as const\)/);
assert.match(schemaSource, /remote_mutations:\s*StringEnum\(\["none"\] as const\)/);
assert.doesNotMatch(schemaSource, /artifact_type:\s*Type\.Literal\("review_lane_draft"\)/);
assert.doesNotMatch(schemaSource, /remote_mutations:\s*Type\.Literal\("none"\)/);
assert.equal(validFixture.artifact_type, "review_lane_draft");
assert.equal(validFixture.remote_mutations, "none");
assert.notEqual(remoteMutationFixture.remote_mutations, "none");
assert.match(indexSource, /name:\s*"submit_review_lane_draft"/);
assert.match(indexSource, /parameters:\s*reviewLaneDraftParameters/);
assert.match(indexSource, /terminate:\s*true/);
assert.match(indexSource, /pi\.registerTool\(submitReviewLaneDraft\)/);

console.log("SCHERZO_REVIEW_LANE_DRAFT_EXTENSION_TEST=ok");
