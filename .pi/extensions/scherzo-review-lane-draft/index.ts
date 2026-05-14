import { defineTool, type ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { StringEnum } from "@mariozechner/pi-ai";
import { Type, type Static } from "typebox";

export const SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME = "submit_review_lane_draft";
export const REVIEW_LANE_DRAFT_WORKFLOW_IDS = ["implementation", "execplan-implementation", "review-native", "review-native-contract-spike"] as const;
const nativeReviewLaneStepIds = ["lane_correctness", "lane_test_quality", "lane_idioms_maintainability", "lane_security_performance"] as const;
const contractSpikeStepIds = ["valid_lane", "malformed_lane", "failed_lane"] as const;
const allowedLaneIds = ["correctness", "test-quality", "idioms-maintainability", "security-performance"] as const;
const allowedReviewNoteKinds = ["risk_note", "coverage_note", "review_note", "follow_up_test"] as const;
const allowedReviewNoteCategories = [
	"correctness",
	"maintainability",
	"security",
	"performance",
	"testing",
	"workflow",
	"documentation",
	"artifact_contract",
	"other",
] as const;
const allowedSeverities = ["info", "low", "medium", "high", "critical"] as const;
const allowedEvidenceTargetFields = ["test_name", "fixture_id", "artifact_path", "changed_file_path", "static_scan_rule"] as const;
const repoRelativePathPattern = "^(?!/)(?!.*(^|/)\\.\\.(/|$))(?![A-Za-z]:[\\\\/]).+";

const repoRelativePathSchema = Type.String({
	minLength: 1,
	pattern: repoRelativePathPattern,
	description: "Repository- or run-root-relative path. Do not use $SCHERZO_RUN_ROOT, /Users/..., /tmp/..., a drive-letter path, or .. segments.",
});

const artifactRefSchema = Type.Object({
	artifact_type: Type.String({ minLength: 1, description: "Non-empty retained artifact type, for example review_brief, diff, changed_files, validation_status, or context_manifest." }),
	path: repoRelativePathSchema,
	sha256: Type.Optional(Type.String({ pattern: "^[0-9a-f]{64}$" })),
}, { additionalProperties: true });

const locationReferenceSchema = Type.Object({
	path: Type.String({ minLength: 1, description: "Repository-relative file path." }),
	start_line: Type.Optional(Type.Number({ description: "1-indexed start line." })),
	end_line: Type.Optional(Type.Number({ description: "1-indexed end line." })),
	symbol: Type.Optional(Type.String()),
	diff_hunk: Type.Optional(Type.String()),
	url: Type.Optional(Type.String()),
}, { additionalProperties: true });

const safeLocationReferenceSchema = Type.Object({
	path: repoRelativePathSchema,
	start_line: Type.Optional(Type.Number({ description: "1-indexed start line." })),
	end_line: Type.Optional(Type.Number({ description: "1-indexed end line." })),
	symbol: Type.Optional(Type.String()),
	diff_hunk: Type.Optional(Type.String()),
	url: Type.Optional(Type.String()),
}, { additionalProperties: true });

const draftFindingSchema = Type.Object({
	draft_finding_id: Type.String({ minLength: 1, description: "Non-empty lane-local finding id." }),
	title: Type.String({ minLength: 1 }),
	claim: Type.String({ minLength: 1 }),
	severity: StringEnum(allowedSeverities),
	proposed_blocking: Type.Boolean(),
	locations: Type.Array(safeLocationReferenceSchema),
	evidence_request_ids: Type.Array(Type.String({ minLength: 1 })),
	category: Type.Optional(Type.String()),
	details: Type.Optional(Type.String()),
	suggested_fix: Type.Optional(Type.String()),
}, { additionalProperties: true });

const reviewNoteSchema = Type.Object({
	id: Type.String({ minLength: 1, description: "Non-empty note id." }),
	kind: StringEnum(allowedReviewNoteKinds),
	category: StringEnum(allowedReviewNoteCategories),
	severity: StringEnum(allowedSeverities),
	summary: Type.String({ minLength: 1 }),
	details: Type.String({ minLength: 1 }),
	suggested_action: Type.String({ minLength: 1 }),
	locations: Type.Array(locationReferenceSchema),
}, { additionalProperties: true });

const evidenceTargetSchema = Type.Object({
	test_name: Type.Optional(Type.String({ minLength: 1 })),
	fixture_id: Type.Optional(Type.String({ minLength: 1 })),
	artifact_path: Type.Optional(repoRelativePathSchema),
	changed_file_path: Type.Optional(repoRelativePathSchema),
	static_scan_rule: Type.Optional(Type.String({ minLength: 1 })),
}, {
	additionalProperties: false,
	description: "Evidence target. Allowed keys are test_name, fixture_id, artifact_path, changed_file_path, and static_scan_rule only.",
});

const evidenceRequestSchema = Type.Object({
	request_id: Type.String({ minLength: 1, description: "Non-empty evidence request id." }),
	draft_finding_id: Type.String({ minLength: 1, description: "Matching draft_finding_id." }),
	evidence_key: Type.String({ minLength: 1 }),
	claim: Type.String({ minLength: 1 }),
	expected_observation: Type.String({ minLength: 1 }),
	target: evidenceTargetSchema,
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

export function expectedLaneIdForStepId(stepId = process.env.SCHERZO_STEP_ID || ""): string | null {
	switch (stepId) {
		case "lane_correctness":
			return "correctness";
		case "lane_test_quality":
			return "test-quality";
		case "lane_idioms_maintainability":
			return "idioms-maintainability";
		case "lane_security_performance":
			return "security-performance";
		case "valid_lane":
		case "malformed_lane":
		case "failed_lane":
			return null;
		default:
			return null;
	}
}

export function shouldRegisterReviewLaneDraftTool(
	workflowId = process.env.SCHERZO_WORKFLOW_ID || "",
	stepId = process.env.SCHERZO_STEP_ID || "",
): boolean {
	if (process.env.SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH) {
		return false;
	}
	if (!REVIEW_LANE_DRAFT_WORKFLOW_IDS.includes(workflowId as typeof REVIEW_LANE_DRAFT_WORKFLOW_IDS[number])) {
		return false;
	}
	return allowedStepIdsForWorkflow(workflowId).includes(stepId);
}

export const submitReviewLaneDraftParameters = Type.Object({
	schema_version: Type.Number({ description: "Must be 1." }),
	artifact_type: StringEnum(["review_lane_draft"] as const),
	generated_at_utc: Type.String({ minLength: 1 }),
	producer: Type.Object({}, { additionalProperties: true }),
	lane: Type.Object({
		id: StringEnum(allowedLaneIds),
		name: Type.String({ minLength: 1 }),
		category: Type.String({ minLength: 1 }),
		version: Type.String({ minLength: 1 }),
	}, { additionalProperties: true }),
	input_refs: Type.Array(artifactRefSchema),
	draft_findings: Type.Array(draftFindingSchema),
	review_notes: Type.Array(reviewNoteSchema),
	evidence_requests: Type.Array(evidenceRequestSchema),
	self_check: Type.Object({}, { additionalProperties: true }),
	remote_mutations: StringEnum(["none"] as const),
}, { additionalProperties: true });

export type SubmitReviewLaneDraftInput = Static<typeof submitReviewLaneDraftParameters>;

function isJsonObject(value: unknown): value is Record<string, unknown> {
	return !!value && typeof value === "object" && !Array.isArray(value);
}

function stringValue(value: Record<string, unknown>, key: string): string | null {
	const candidate = value[key];
	return typeof candidate === "string" ? candidate : null;
}

function pathIsUnsafe(path: string): boolean {
	return path.startsWith("/")
		|| path.startsWith("<absolute-local-path>")
		|| /(^|[\\/])\.\.([\\/]|$)/.test(path)
		|| /^[A-Za-z]:[\\/]/.test(path);
}

function validateRepoRelativePath(errors: string[], path: unknown, field: string) {
	if (typeof path !== "string" || path.length === 0) {
		errors.push(`${field} must be a non-empty repository-relative string`);
		return;
	}
	if (pathIsUnsafe(path)) {
		errors.push(`${field} must be repository- or run-root-relative; use artifacts/review/prepare_review/... or repository-relative paths, not ${path}`);
	}
}

function validateStringEnum(errors: string[], value: unknown, field: string, allowed: readonly string[]) {
	if (typeof value !== "string" || !allowed.includes(value)) {
		errors.push(`${field} must be one of: ${allowed.join(", ")}`);
	}
}

function validateInputRefs(errors: string[], params: Record<string, unknown>) {
	const refs = params.input_refs;
	if (!Array.isArray(refs)) {
		errors.push("input_refs must be a list");
		return;
	}
	for (const [index, ref] of refs.entries()) {
		if (!isJsonObject(ref)) {
			errors.push(`input_refs[${index}] must be an object`);
			continue;
		}
		validateRepoRelativePath(errors, ref.path, `input_refs[${index}].path`);
	}
}

function validateDraftFindings(errors: string[], params: Record<string, unknown>) {
	const findings = params.draft_findings;
	if (!Array.isArray(findings)) {
		errors.push("draft_findings must be a list");
		return;
	}
	for (const [findingIndex, finding] of findings.entries()) {
		if (!isJsonObject(finding)) {
			errors.push(`draft_findings[${findingIndex}] must be an object`);
			continue;
		}
		validateStringEnum(errors, finding.severity, `draft_findings[${findingIndex}].severity`, allowedSeverities);
		const locations = finding.locations;
		if (!Array.isArray(locations)) {
			errors.push(`draft_findings[${findingIndex}].locations must be a list`);
			continue;
		}
		for (const [locationIndex, location] of locations.entries()) {
			if (!isJsonObject(location)) {
				errors.push(`draft_findings[${findingIndex}].locations[${locationIndex}] must be an object`);
				continue;
			}
			validateRepoRelativePath(errors, location.path, `draft_findings[${findingIndex}].locations[${locationIndex}].path`);
		}
	}
}

function validateReviewNotes(errors: string[], params: Record<string, unknown>) {
	const notes = params.review_notes;
	if (!Array.isArray(notes)) {
		errors.push("review_notes must be a list");
		return;
	}
	for (const [index, note] of notes.entries()) {
		if (!isJsonObject(note)) {
			errors.push(`review_notes[${index}] must be an object`);
			continue;
		}
		validateStringEnum(errors, note.kind, `review_notes[${index}].kind`, allowedReviewNoteKinds);
		validateStringEnum(errors, note.category, `review_notes[${index}].category`, allowedReviewNoteCategories);
		validateStringEnum(errors, note.severity, `review_notes[${index}].severity`, allowedSeverities);
	}
}

function validateEvidenceRequests(errors: string[], params: Record<string, unknown>) {
	const requests = params.evidence_requests;
	if (!Array.isArray(requests)) {
		errors.push("evidence_requests must be a list");
		return;
	}
	const allowedTargetKeys = new Set<string>(allowedEvidenceTargetFields);
	for (const [index, request] of requests.entries()) {
		if (!isJsonObject(request)) {
			errors.push(`evidence_requests[${index}] must be an object`);
			continue;
		}
		const target = request.target;
		if (!isJsonObject(target)) {
			errors.push(`evidence_requests[${index}].target must be an object`);
			continue;
		}
		for (const key of Object.keys(target)) {
			if (!allowedTargetKeys.has(key)) {
				errors.push(`evidence_requests[${index}].target.${key} is not allowed; allowed target keys are ${allowedEvidenceTargetFields.join(", ")}`);
			}
		}
		if ("artifact_path" in target) {
			validateRepoRelativePath(errors, target.artifact_path, `evidence_requests[${index}].target.artifact_path`);
		}
		if ("changed_file_path" in target) {
			validateRepoRelativePath(errors, target.changed_file_path, `evidence_requests[${index}].target.changed_file_path`);
		}
	}
}

export function shallowValidateReviewLaneDraft(params: unknown): string[] {
	const errors: string[] = [];

	// These checks intentionally mirror the high-risk parts of the durable JSON
	// Schema so bad terminating tool calls fail inside Pi and can be corrected by
	// the model before Scherzo spends the structured-output retry budget.
	if (!isJsonObject(params)) {
		return ["arguments must be a JSON object"];
	}
	if (params.schema_version !== 1) errors.push("schema_version must be 1");
	if (params.artifact_type !== "review_lane_draft") errors.push("artifact_type must be review_lane_draft");
	if (params.remote_mutations !== "none") errors.push("remote_mutations must be none");
	if (!isJsonObject(params.lane) || !allowedLaneIds.includes(stringValue(params.lane, "id") as typeof allowedLaneIds[number])) {
		errors.push("lane.id must be one of the native review lane ids");
	} else {
		const expectedLaneId = expectedLaneIdForStepId();
		if (expectedLaneId && params.lane.id !== expectedLaneId) {
			errors.push(`lane.id must be ${expectedLaneId} for workflow step ${process.env.SCHERZO_STEP_ID}`);
		}
	}
	validateInputRefs(errors, params);
	validateDraftFindings(errors, params);
	validateReviewNotes(errors, params);
	validateEvidenceRequests(errors, params);
	if (!isJsonObject(params.producer)) errors.push("producer must be an object");
	if (!isJsonObject(params.self_check)) errors.push("self_check must be an object");
	return errors;
}

export const submitReviewLaneDraftTool = defineTool({
	name: SUBMIT_REVIEW_LANE_DRAFT_TOOL_NAME,
	label: "Submit Review Lane Draft",
	description: "Submit the final review_lane_draft object for a native Scherzo review lane. This tool has no side effects and rejects non-portable paths or schema-invalid evidence targets before terminating.",
	promptSnippet: "Submit a native review_lane_draft object as the final terminating review-lane artifact",
	promptGuidelines: [
		"Use submit_review_lane_draft exactly once as the final action for native Scherzo review lanes.",
		"Do not print review_lane_draft as final assistant JSON; pass the object as submit_review_lane_draft arguments instead.",
		"Use repository- or run-root-relative paths in submit_review_lane_draft, for example artifacts/review/prepare_review/diff.patch; never use $SCHERZO_RUN_ROOT, /Users/..., /tmp/..., drive-letter paths, or .. segments.",
		"For submit_review_lane_draft review_notes.category, use one of: correctness, maintainability, security, performance, testing, workflow, documentation, artifact_contract, other. Use testing for the test-quality lane, not test-quality.",
		"For submit_review_lane_draft evidence_requests[].target, use only test_name, fixture_id, artifact_path, changed_file_path, or static_scan_rule; do not include command, suggested_test_file, or suggested_test_name.",
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
	const genericStructuredOutputActive = !!process.env.SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH;
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
				: genericStructuredOutputActive
					? "disabled_generic_structured_output_active"
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
