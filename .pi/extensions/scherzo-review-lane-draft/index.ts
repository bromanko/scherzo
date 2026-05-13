import { defineTool, type ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Type, type Static } from "typebox";

export const SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME = "submit_review_lane_draft";
export const REVIEW_LANE_DRAFT_WORKFLOW_IDS = ["implementation", "execplan-implementation", "review-native", "review-native-contract-spike"] as const;
const nativeReviewLaneStepIds = ["lane_correctness", "lane_test_quality", "lane_idioms_maintainability", "lane_security_performance"] as const;
const contractSpikeStepIds = ["valid_lane", "malformed_lane", "failed_lane"] as const;
const allowedLaneIds = ["correctness", "test-quality", "idioms-maintainability", "security-performance"] as const;

const artifactRefSchema = Type.Object({
	artifact_type: Type.String({ description: "Non-empty retained artifact type, for example review_brief, diff, changed_files, validation_status, or context_manifest." }),
	path: Type.String({ description: "Repository- or run-root-relative path to the retained input artifact, for example artifacts/review/prepare_review/diff.patch; never use $SCHERZO_RUN_ROOT, /Users/..., /tmp/..., or drive-letter absolute paths." }),
	sha256: Type.Optional(Type.String()),
}, { additionalProperties: true });

const reviewLocationSchema = Type.Object({
	path: Type.String({ description: "Repository-relative file path." }),
	start_line: Type.Number({ description: "1-indexed start line." }),
	end_line: Type.Optional(Type.Number({ description: "1-indexed end line." })),
	symbol: Type.Optional(Type.String()),
}, { additionalProperties: true });

const draftFindingSchema = Type.Object({
	draft_finding_id: Type.String({ description: "Non-empty lane-local finding id." }),
	title: Type.String(),
	claim: Type.String(),
	severity: Type.String({ description: "critical, high, medium, low, or info." }),
	proposed_blocking: Type.Boolean(),
	locations: Type.Array(reviewLocationSchema),
	evidence_request_ids: Type.Array(Type.String()),
	category: Type.Optional(Type.String()),
	details: Type.Optional(Type.String()),
	suggested_fix: Type.Optional(Type.String()),
}, { additionalProperties: true });

const reviewNoteSchema = Type.Object({
	id: Type.String({ description: "Non-empty note id." }),
	kind: Type.String(),
	category: Type.String(),
	severity: Type.String({ description: "critical, high, medium, low, or info." }),
	summary: Type.String(),
	details: Type.String(),
	suggested_action: Type.String(),
	locations: Type.Array(reviewLocationSchema),
}, { additionalProperties: true });

const evidenceRequestSchema = Type.Object({
	request_id: Type.String({ description: "Non-empty evidence request id." }),
	draft_finding_id: Type.String({ description: "Matching draft_finding_id." }),
	evidence_key: Type.String(),
	claim: Type.String(),
	expected_observation: Type.String(),
	target: Type.Object({}, { additionalProperties: true, description: "Evidence target object, optionally including artifact_path or changed_file_path." }),
}, { additionalProperties: true });

function allowedStepIdsForWorkflow(workflowId: string): readonly string[] {
	switch (workflowId) {
		case "implementation":
		case "execplan-implementation":
		case "review-native":
			return nativeReviewLaneStepIds;
		case "review-native-contract-spike":
			return contractSpikeStepIds;
		default:
			return [];
	}
}

export function shouldRegisterReviewLaneDraftTool(
	workflowId = process.env.SCHERZO_WORKFLOW_ID || "",
	stepId = process.env.SCHERZO_STEP_ID || "",
): boolean {
	if (!REVIEW_LANE_DRAFT_WORKFLOW_IDS.includes(workflowId as typeof REVIEW_LANE_DRAFT_WORKFLOW_IDS[number])) {
		return false;
	}
	return allowedStepIdsForWorkflow(workflowId).includes(stepId);
}

export const submitReviewLaneDraftParameters = Type.Object({
	schema_version: Type.Number({ description: "Must be 1." }),
	artifact_type: StringEnum(["review_lane_draft"] as const),
	generated_at_utc: Type.String(),
	producer: Type.Object({}, { additionalProperties: true }),
	lane: Type.Object({
		id: StringEnum(allowedLaneIds),
		name: Type.String(),
		category: Type.String(),
		version: Type.String(),
	}, { additionalProperties: true }),
	input_refs: Type.Array(artifactRefSchema),
	draft_findings: Type.Array(draftFindingSchema),
	review_notes: Type.Array(reviewNoteSchema),
	evidence_requests: Type.Array(evidenceRequestSchema),
	self_check: Type.Object({}, { additionalProperties: true }),
	remote_mutations: StringEnum(["none"] as const),
}, { additionalProperties: true });

export type SubmitReviewLaneDraftInput = Static<typeof submitReviewLaneDraftParameters>;

export function shallowValidateReviewLaneDraft(params: SubmitReviewLaneDraftInput): string[] {
	const errors: string[] = [];

	// Keep these extension checks deliberately shallow. The extension only
	// confirms the top-level contract and duplicated defense-in-depth constants;
	// scripts/scherzo-review owns path portability, evidence linkage, finding
	// policy, duplicate id checks, and synthesis-readiness validation.
	if (!params || typeof params !== "object" || Array.isArray(params)) {
		return ["arguments must be a JSON object"];
	}
	if (params.schema_version !== 1) errors.push("schema_version must be 1");
	if (params.artifact_type !== "review_lane_draft") errors.push("artifact_type must be review_lane_draft");
	if (params.remote_mutations !== "none") errors.push("remote_mutations must be none");
	if (!params.lane || typeof params.lane !== "object" || !allowedLaneIds.includes(params.lane.id as typeof allowedLaneIds[number])) {
		errors.push("lane.id must be one of the native review lane ids");
	}
	return errors;
}

export const submitReviewLaneDraftTool = defineTool({
	name: SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME,
	label: "Submit Review Lane Draft",
	description: "Submit the final review_lane_draft object for a native Scherzo review lane. This tool has no side effects.",
	promptSnippet: "Submit a native review_lane_draft object as the final terminating review-lane artifact",
	promptGuidelines: [
		"Use submit_review_lane_draft exactly once as the final action for native Scherzo review lanes.",
		"Do not print review_lane_draft as final assistant JSON; pass the object as submit_review_lane_draft arguments instead.",
		"Do not call sibling tools in the same tool-call batch as submit_review_lane_draft.",
		"For input_refs[].path, use run-root-relative paths such as artifacts/review/prepare_review/diff.patch; never use $SCHERZO_RUN_ROOT or absolute local paths such as /Users/... or /tmp/...",
	],
	parameters: submitReviewLaneDraftParameters,
	async execute(_toolCallId, params) {
		const errors = shallowValidateReviewLaneDraft(params);
		if (errors.length > 0) {
			throw new Error(`invalid review_lane_draft: ${errors.join("; ")}`);
		}
		return {
			content: [{ type: "text", text: `Accepted review_lane_draft for ${params.lane.id}` }],
			details: {
				artifact_type: "review_lane_draft_tool_receipt",
				tool_name: SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME,
				lane_id: params.lane.id,
				remote_mutations: "none",
			},
			terminate: true,
		};
	},
});

export default function scherzoReviewLaneDraftExtension(pi: ExtensionAPI) {
	const workflowId = process.env.SCHERZO_WORKFLOW_ID || "";
	const stepId = process.env.SCHERZO_STEP_ID || "";
	const workflowInScope = REVIEW_LANE_DRAFT_WORKFLOW_IDS.includes(workflowId as typeof REVIEW_LANE_DRAFT_WORKFLOW_IDS[number]);
	const stepInScope = allowedStepIdsForWorkflow(workflowId).includes(stepId);
	const enabledForStep = shouldRegisterReviewLaneDraftTool(workflowId, stepId);
	if (enabledForStep) {
		pi.registerTool(submitReviewLaneDraftTool);
	}

	pi.registerCommand("review-lane-draft-tool-info", {
		description: "Print whether submit_review_lane_draft is active in this Pi session.",
		handler: async () => {
			const activeToolNames = new Set(pi.getActiveTools());
			const tool = pi.getAllTools().find((candidate) => candidate.name === SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME);
			const active = activeToolNames.has(SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME);
			const status = active
				? "active"
				: enabledForStep
					? "inactive"
					: workflowInScope && !stepInScope
						? "disabled_step_scope"
						: "disabled_workflow_scope";
			console.log(`REVIEW_LANE_DRAFT_TOOL_ADVERTISED=${JSON.stringify({
				status,
				tool_name: SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME,
				workflow_id: workflowId || null,
				step_id: stepId || null,
				source: tool?.sourceInfo?.source || null,
				path: tool?.sourceInfo?.path || null,
			})}`);
			if (enabledForStep && !active) {
				throw new Error("submit_review_lane_draft was registered for this native review lane step but is not active");
			}
			if (!enabledForStep && active) {
				throw new Error("submit_review_lane_draft is active outside native review lane step scope");
			}
		},
	});
}
