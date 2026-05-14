import { existsSync, readFileSync } from "node:fs";
import { defineTool, type ExtensionAPI } from "@mariozechner/pi-coding-agent";

type JsonObject = Record<string, unknown>;

export interface StructuredOutputToolSpec {
	schema_version: 1;
	artifact_type: "scherzo_structured_output_tool_spec";
	workflow_id: string;
	run_id: string;
	step_id: string;
	attempt_index: number;
	artifact_name: string;
	tool_name: string;
	label: string;
	description: string;
	prompt_snippet: string;
	prompt_guidelines: string[];
	parameters_schema_path: string;
	parameters_schema_sha256: string;
	parameters_schema: JsonObject;
	require_single: true;
	reject_sibling_tool_calls: true;
	terminate: true;
}

type StartupToolInfo =
	| { status: "disabled_missing_spec_env" }
	| { status: "missing_spec_file"; spec_path: string }
	| { status: "invalid_spec"; spec_path: string; error: string }
	| { status: "loaded"; spec_path: string; spec: StructuredOutputToolSpec };

type ToolInfo =
	| { status: "disabled_missing_spec_env" }
	| { status: "missing_spec_file"; spec_path: string }
	| { status: "invalid_spec"; spec_path: string; error: string }
	| { status: "duplicate_tool_name"; spec_path: string; tool_name: string; active_structured_output_tool_count: number }
	| { status: "registration_failed"; spec_path: string; tool_name: string; error: string }
	| { status: "inactive"; spec_path: string; tool_name: string; artifact_name: string; schema_sha256: string; active_structured_output_tool_count: number }
	| { status: "active"; spec_path: string; tool_name: string; artifact_name: string; schema_sha256: string; active_structured_output_tool_count: number };

const SPEC_ENV = "SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH";
const SPEC_ARTIFACT_TYPE = "scherzo_structured_output_tool_spec";

function isJsonObject(value: unknown): value is JsonObject {
	return !!value && typeof value === "object" && !Array.isArray(value);
}

function requireString(value: JsonObject, field: string): string {
	const candidate = value[field];
	if (typeof candidate !== "string" || candidate.trim() === "") {
		throw new Error(`${field} must be a non-empty string`);
	}
	return candidate;
}

function requireBooleanTrue(value: JsonObject, field: string): true {
	if (value[field] !== true) throw new Error(`${field} must be true`);
	return true;
}

function requireNumber(value: JsonObject, field: string): number {
	const candidate = value[field];
	if (typeof candidate !== "number" || !Number.isInteger(candidate) || candidate < 0) {
		throw new Error(`${field} must be a non-negative integer`);
	}
	return candidate;
}

function requireStringList(value: JsonObject, field: string): string[] {
	const candidate = value[field];
	if (!Array.isArray(candidate) || !candidate.every((item) => typeof item === "string")) {
		throw new Error(`${field} must be a list of strings`);
	}
	return candidate;
}

function validToolName(name: string): boolean {
	return /^[a-z][a-z0-9_-]*$/.test(name);
}

function validSchemaPath(schemaPath: string): boolean {
	return schemaPath.length > 0
		&& !schemaPath.startsWith("/")
		&& !schemaPath.startsWith("$")
		&& !schemaPath.startsWith("<absolute-local-path>")
		&& !/(^|[/\\])\.\.([/\\]|$)/.test(schemaPath)
		&& !/^[A-Za-z]:[/\\]/.test(schemaPath);
}

function normalizeProviderParametersSchema(schema: JsonObject): JsonObject {
	const rootType = schema.type;
	if (rootType === undefined) return { ...schema, type: "object" };
	if (rootType === "object") return schema;
	throw new Error(
		`parameters_schema top-level type must be "object" for provider tool registration; got ${JSON.stringify(rootType)}`,
	);
}

export function validateSpec(value: unknown): StructuredOutputToolSpec {
	if (!isJsonObject(value)) throw new Error("spec must be a JSON object");
	if (value.schema_version !== 1) throw new Error("schema_version must be 1");
	if (value.artifact_type !== SPEC_ARTIFACT_TYPE) {
		throw new Error(`artifact_type must be ${SPEC_ARTIFACT_TYPE}`);
	}
	const toolName = requireString(value, "tool_name");
	if (!validToolName(toolName)) throw new Error(`tool_name is invalid: ${toolName}`);
	const schemaPath = requireString(value, "parameters_schema_path");
	if (!validSchemaPath(schemaPath)) throw new Error(`parameters_schema_path is invalid: ${schemaPath}`);
	const schemaValue = value.parameters_schema;
	if (!isJsonObject(schemaValue)) throw new Error("parameters_schema must be a JSON object");
	const schema = normalizeProviderParametersSchema(schemaValue);
	return {
		schema_version: 1,
		artifact_type: SPEC_ARTIFACT_TYPE,
		workflow_id: requireString(value, "workflow_id"),
		run_id: requireString(value, "run_id"),
		step_id: requireString(value, "step_id"),
		attempt_index: requireNumber(value, "attempt_index"),
		artifact_name: requireString(value, "artifact_name"),
		tool_name: toolName,
		label: requireString(value, "label"),
		description: requireString(value, "description"),
		prompt_snippet: requireString(value, "prompt_snippet"),
		prompt_guidelines: requireStringList(value, "prompt_guidelines"),
		parameters_schema_path: schemaPath,
		parameters_schema_sha256: requireString(value, "parameters_schema_sha256"),
		parameters_schema: schema,
		require_single: requireBooleanTrue(value, "require_single"),
		reject_sibling_tool_calls: requireBooleanTrue(value, "reject_sibling_tool_calls"),
		terminate: requireBooleanTrue(value, "terminate"),
	};
}

export function loadSpecFromPath(specPath: string): StructuredOutputToolSpec {
	const parsed = JSON.parse(readFileSync(specPath, "utf8"));
	return validateSpec(parsed);
}

export function createStructuredOutputTool(spec: StructuredOutputToolSpec) {
	return defineTool({
		name: spec.tool_name,
		label: spec.label,
		description: spec.description,
		promptSnippet: spec.prompt_snippet,
		promptGuidelines: spec.prompt_guidelines,
		parameters: spec.parameters_schema as any,
		async execute(_toolCallId: string, params: unknown) {
			if (!isJsonObject(params)) {
				throw new Error(`${spec.tool_name} arguments must be a JSON object`);
			}
			return {
				content: [{ type: "text" as const, text: `Accepted ${spec.artifact_name} via ${spec.tool_name}` }],
				details: {
					artifact_type: "scherzo_structured_output_tool_receipt",
					tool_name: spec.tool_name,
					artifact_name: spec.artifact_name,
					workflow_id: spec.workflow_id,
					run_id: spec.run_id,
					step_id: spec.step_id,
					attempt_index: spec.attempt_index,
					parameters_schema_path: spec.parameters_schema_path,
					parameters_schema_sha256: spec.parameters_schema_sha256,
					remote_mutations: "none",
				},
				terminate: true,
			};
		},
	});
}

function loadStartupToolInfo(): StartupToolInfo {
	const specPath = process.env[SPEC_ENV];
	if (!specPath) return { status: "disabled_missing_spec_env" };
	if (!existsSync(specPath)) return { status: "missing_spec_file", spec_path: specPath };
	try {
		return { status: "loaded", spec_path: specPath, spec: loadSpecFromPath(specPath) };
	} catch (error) {
		return {
			status: "invalid_spec",
			spec_path: specPath,
			error: error instanceof Error ? error.message : String(error),
		};
	}
}

function startupToolInfoToCommandInfo(info: StartupToolInfo): ToolInfo {
	switch (info.status) {
		case "disabled_missing_spec_env":
			return info;
		case "missing_spec_file":
			return info;
		case "invalid_spec":
			return info;
		case "loaded":
			return {
				status: "inactive",
				spec_path: info.spec_path,
				tool_name: info.spec.tool_name,
				artifact_name: info.spec.artifact_name,
				schema_sha256: info.spec.parameters_schema_sha256,
				active_structured_output_tool_count: 0,
			};
	}
}

function activeToolCount(pi: ExtensionAPI, toolName: string): number {
	return pi.getActiveTools().filter((name) => name === toolName).length;
}

function activeToolInfo(pi: ExtensionAPI, info: Extract<StartupToolInfo, { status: "loaded" }>): ToolInfo {
	const activeCount = activeToolCount(pi, info.spec.tool_name);
	const common = {
		spec_path: info.spec_path,
		tool_name: info.spec.tool_name,
		artifact_name: info.spec.artifact_name,
		schema_sha256: info.spec.parameters_schema_sha256,
		active_structured_output_tool_count: activeCount,
	};
	return {
		...common,
		status: activeCount > 0 ? "active" : "inactive",
	};
}

function duplicateToolInfo(pi: ExtensionAPI, info: Extract<StartupToolInfo, { status: "loaded" }>): ToolInfo {
	return {
		status: "duplicate_tool_name",
		spec_path: info.spec_path,
		tool_name: info.spec.tool_name,
		active_structured_output_tool_count: activeToolCount(pi, info.spec.tool_name),
	};
}

function registrationFailedToolInfo(
	info: Extract<StartupToolInfo, { status: "loaded" }>,
	error: unknown,
): ToolInfo {
	return {
		status: "registration_failed",
		spec_path: info.spec_path,
		tool_name: info.spec.tool_name,
		error: error instanceof Error ? error.message : String(error),
	};
}

export default function scherzoStructuredOutputExtension(pi: ExtensionAPI) {
	const startupInfo = loadStartupToolInfo();
	let info = startupToolInfoToCommandInfo(startupInfo);

	pi.on("session_start", () => {
		if (startupInfo.status !== "loaded") return;
		if (activeToolCount(pi, startupInfo.spec.tool_name) > 0) {
			info = duplicateToolInfo(pi, startupInfo);
			return;
		}
		try {
			pi.registerTool(createStructuredOutputTool(startupInfo.spec));
			info = activeToolInfo(pi, startupInfo);
		} catch (error) {
			info = registrationFailedToolInfo(startupInfo, error);
		}
	});

	pi.registerCommand("structured-output-tool-info", {
		description: "Print whether Scherzo's generic structured-output tool is active in this Pi session.",
		handler: async () => {
			if (startupInfo.status === "loaded" && info.status !== "duplicate_tool_name" && info.status !== "registration_failed") {
				info = activeToolInfo(pi, startupInfo);
			}
			console.log(`SCHERZO_STRUCTURED_OUTPUT_TOOL_ADVERTISED=${JSON.stringify(info)}`);
			if (info.status !== "disabled_missing_spec_env" && info.status !== "active") {
				throw new Error(`Scherzo structured-output tool is not active: ${info.status}`);
			}
		},
	});
}
