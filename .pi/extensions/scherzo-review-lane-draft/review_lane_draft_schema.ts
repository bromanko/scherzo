import { StringEnum } from "@mariozechner/pi-ai";
import { Type } from "typebox";

export const REVIEW_LANE_DRAFT_REQUIRED_KEYS = [
  "schema_version",
  "artifact_type",
  "generated_at_utc",
  "producer",
  "lane",
  "input_refs",
  "draft_findings",
  "review_notes",
  "evidence_requests",
  "self_check",
  "remote_mutations",
] as const;

const looseObject = Type.Object({}, { additionalProperties: true });
const artifactRef = Type.Object(
  {
    artifact_type: Type.String(),
    path: Type.String(),
  },
  { additionalProperties: true },
);
const location = Type.Object(
  {
    path: Type.String(),
  },
  { additionalProperties: true },
);

export const reviewLaneDraftParameters = Type.Object(
  {
    schema_version: Type.Literal(1),
    artifact_type: StringEnum(["review_lane_draft"] as const),
    generated_at_utc: Type.String(),
    producer: Type.Object(
      {
        name: Type.String(),
        version: Type.String(),
        mode: Type.String(),
      },
      { additionalProperties: true },
    ),
    lane: Type.Object(
      {
        id: Type.String(),
        name: Type.String(),
        category: Type.String(),
        version: Type.String(),
      },
      { additionalProperties: true },
    ),
    input_refs: Type.Array(artifactRef),
    draft_findings: Type.Array(
      Type.Object(
        {
          draft_finding_id: Type.String(),
          title: Type.String(),
          claim: Type.String(),
          severity: Type.String(),
          proposed_blocking: Type.Boolean(),
          locations: Type.Array(location),
          evidence_request_ids: Type.Array(Type.String()),
        },
        { additionalProperties: true },
      ),
    ),
    review_notes: Type.Array(looseObject),
    evidence_requests: Type.Array(
      Type.Object(
        {
          request_id: Type.String(),
          draft_finding_id: Type.String(),
          evidence_key: Type.String(),
          claim: Type.String(),
          expected_observation: Type.String(),
          target: looseObject,
        },
        { additionalProperties: true },
      ),
    ),
    self_check: looseObject,
    remote_mutations: StringEnum(["none"] as const),
  },
  { additionalProperties: false },
);

export type ReviewLaneDraftParameters = typeof reviewLaneDraftParameters;
