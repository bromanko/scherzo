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
	validation_schema: JsonObject;
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
const MAX_SCHEMA_VALIDATION_ERRORS = 20;

type SchemaValidationFailure = { path: string; message: string };

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

const unsupportedProviderSchemaKeywords = new Set(["$schema", "$id", "$defs", "$ref", "oneOf", "anyOf", "allOf", "enum", "not", "const"]);

function resolveLocalDefinition(ref: string, rootDefs: JsonObject): unknown {
	const prefix = "#/$defs/";
	if (!ref.startsWith(prefix)) return undefined;
	return rootDefs[ref.slice(prefix.length)];
}

function inferredJsonType(value: unknown): string | undefined {
	if (typeof value === "string") return "string";
	if (typeof value === "boolean") return "boolean";
	if (typeof value === "number") return Number.isInteger(value) ? "integer" : "number";
	if (Array.isArray(value)) return "array";
	if (isJsonObject(value)) return "object";
	return undefined;
}

function inferredTypeFromSchema(schema: JsonObject): string | undefined {
	if ("const" in schema) return inferredJsonType(schema.const);
	const enumValues = schema.enum;
	if (Array.isArray(enumValues) && enumValues.length > 0) return inferredJsonType(enumValues[0]);
	return undefined;
}

function normalizeSchemaType(typeValue: unknown): unknown {
	if (!Array.isArray(typeValue)) return typeValue;
	const nonNull = typeValue.find((candidate) => candidate !== "null");
	return typeof nonNull === "string" ? nonNull : undefined;
}

function sanitizeProviderProperties(value: unknown, rootDefs: JsonObject): unknown {
	if (!isJsonObject(value)) return value;
	return Object.fromEntries(
		Object.entries(value).map(([key, child]) => [key, sanitizeProviderSchemaNode(child, rootDefs)]),
	);
}

function sanitizeProviderSchemaNode(value: unknown, rootDefs: JsonObject): unknown {
	if (Array.isArray(value)) return value.map((child) => sanitizeProviderSchemaNode(child, rootDefs));
	if (!isJsonObject(value)) return value;

	const ref = value.$ref;
	if (typeof ref === "string") {
		const resolved = resolveLocalDefinition(ref, rootDefs);
		return resolved === undefined ? {} : sanitizeProviderSchemaNode(resolved, rootDefs);
	}

	const sanitized: JsonObject = {};
	for (const [key, child] of Object.entries(value)) {
		if (unsupportedProviderSchemaKeywords.has(key)) continue;
		if (key === "type") {
			const normalizedType = normalizeSchemaType(child);
			if (normalizedType !== undefined) sanitized.type = normalizedType;
			continue;
		}
		if (key === "properties") {
			sanitized.properties = sanitizeProviderProperties(child, rootDefs);
			continue;
		}
		if (key === "items") {
			sanitized.items = sanitizeProviderSchemaNode(child, rootDefs);
			continue;
		}
		if (key === "additionalProperties" && isJsonObject(child)) {
			sanitized.additionalProperties = sanitizeProviderSchemaNode(child, rootDefs);
			continue;
		}
		sanitized[key] = sanitizeProviderSchemaNode(child, rootDefs);
	}

	if (sanitized.type === undefined) {
		const inferred = inferredTypeFromSchema(value);
		if (inferred !== undefined) sanitized.type = inferred;
	}
	return sanitized;
}

function normalizeProviderParametersSchema(schema: JsonObject): JsonObject {
	const rootDefs = isJsonObject(schema.$defs) ? schema.$defs : {};
	const sanitizedValue = sanitizeProviderSchemaNode(schema, rootDefs);
	if (!isJsonObject(sanitizedValue)) throw new Error("parameters_schema must normalize to a JSON object");
	const rootType = sanitizedValue.type;
	if (rootType === undefined) return { ...sanitizedValue, type: "object" };
	if (rootType === "object") return sanitizedValue;
	throw new Error(
		`parameters_schema top-level type must be "object" for provider tool registration; got ${JSON.stringify(rootType)}`,
	);
}

// Codex rejects several JSON Schema keywords that Scherzo's durable schemas use.
// The advertised provider schema is normalized above, so execute mirrors the
// supported contract subset locally and returns a tool error the model can fix
// within the same Pi session before Scherzo spends its retry budget.
function pointerToken(value: string): string {
	return value.replace(/~/g, "~0").replace(/\//g, "~1");
}

function childPath(path: string, key: string | number): string {
	return `${path}/${pointerToken(String(key))}`;
}

function resolveJsonPointer(ref: string, rootSchema: JsonObject): unknown {
	if (ref === "#") return rootSchema;
	if (!ref.startsWith("#/")) return undefined;
	let current: unknown = rootSchema;
	for (const token of ref.slice(2).split("/")) {
		const key = token.replace(/~1/g, "/").replace(/~0/g, "~");
		if (!isJsonObject(current) && !Array.isArray(current)) return undefined;
		current = (current as Record<string, unknown>)[key];
	}
	return current;
}

function jsonType(value: unknown): string {
	if (value === null) return "null";
	if (Array.isArray(value)) return "array";
	if (Number.isInteger(value)) return "integer";
	if (typeof value === "number") return "number";
	return typeof value;
}

function jsonTypeMatches(value: unknown, expected: string): boolean {
	switch (expected) {
		case "null":
			return value === null;
		case "array":
			return Array.isArray(value);
		case "object":
			return isJsonObject(value);
		case "integer":
			return typeof value === "number" && Number.isInteger(value);
		case "number":
			return typeof value === "number" && Number.isFinite(value);
		case "string":
			return typeof value === "string";
		case "boolean":
			return typeof value === "boolean";
		default:
			return true;
	}
}

function schemaTypeList(typeValue: unknown): string[] {
	if (typeof typeValue === "string") return [typeValue];
	if (Array.isArray(typeValue)) return typeValue.filter((item): item is string => typeof item === "string");
	return [];
}

function jsonEquals(left: unknown, right: unknown): boolean {
	if (Object.is(left, right)) return true;
	if (Array.isArray(left) && Array.isArray(right)) {
		return left.length === right.length && left.every((item, index) => jsonEquals(item, right[index]));
	}
	if (isJsonObject(left) && isJsonObject(right)) {
		const leftKeys = Object.keys(left).sort();
		const rightKeys = Object.keys(right).sort();
		return leftKeys.length === rightKeys.length
			&& leftKeys.every((key, index) => key === rightKeys[index] && jsonEquals(left[key], right[key]));
	}
	return false;
}

function pushSchemaError(errors: SchemaValidationFailure[], path: string, message: string) {
	if (errors.length < MAX_SCHEMA_VALIDATION_ERRORS) {
		errors.push({ path: path || "/", message });
	}
}

function validateSchemaNode(
	value: unknown,
	schemaValue: unknown,
	rootSchema: JsonObject,
	path: string,
	errors: SchemaValidationFailure[],
	seenRefs: Set<string>,
) {
	if (errors.length >= MAX_SCHEMA_VALIDATION_ERRORS) return;
	if (!isJsonObject(schemaValue)) return;

	const ref = schemaValue.$ref;
	if (typeof ref === "string") {
		const resolved = resolveJsonPointer(ref, rootSchema);
		if (resolved === undefined) {
			pushSchemaError(errors, path, `references unsupported schema ${ref}`);
			return;
		}
		const refKey = `${path}:${ref}`;
		if (seenRefs.has(refKey)) return;
		const nextSeen = new Set(seenRefs);
		nextSeen.add(refKey);
		validateSchemaNode(value, resolved, rootSchema, path, errors, nextSeen);
		return;
	}

	if (Array.isArray(schemaValue.allOf)) {
		for (const child of schemaValue.allOf) {
			validateSchemaNode(value, child, rootSchema, path, errors, seenRefs);
		}
	}

	if (Array.isArray(schemaValue.anyOf)) {
		const matched = schemaValue.anyOf.some((child) => {
			const childErrors: SchemaValidationFailure[] = [];
			validateSchemaNode(value, child, rootSchema, path, childErrors, seenRefs);
			return childErrors.length === 0;
		});
		if (!matched) pushSchemaError(errors, path, "must match at least one allowed schema");
	}

	if (Array.isArray(schemaValue.oneOf)) {
		const matches = schemaValue.oneOf.filter((child) => {
			const childErrors: SchemaValidationFailure[] = [];
			validateSchemaNode(value, child, rootSchema, path, childErrors, seenRefs);
			return childErrors.length === 0;
		}).length;
		if (matches !== 1) pushSchemaError(errors, path, "must match exactly one allowed schema");
	}

	if ("not" in schemaValue) {
		const childErrors: SchemaValidationFailure[] = [];
		validateSchemaNode(value, schemaValue.not, rootSchema, path, childErrors, seenRefs);
		if (childErrors.length === 0) pushSchemaError(errors, path, "must not match a disallowed schema");
	}

	const expectedTypes = schemaTypeList(schemaValue.type);
	if (expectedTypes.length > 0 && !expectedTypes.some((expected) => jsonTypeMatches(value, expected))) {
		pushSchemaError(errors, path, `must be ${expectedTypes.join(" or ")}; got ${jsonType(value)}`);
	}

	if ("const" in schemaValue && !jsonEquals(value, schemaValue.const)) {
		pushSchemaError(errors, path, `must equal ${JSON.stringify(schemaValue.const)}`);
	}

	if (Array.isArray(schemaValue.enum) && !schemaValue.enum.some((candidate) => jsonEquals(value, candidate))) {
		pushSchemaError(errors, path, `must be one of ${schemaValue.enum.map((item) => JSON.stringify(item)).join(", ")}`);
	}

	if (typeof value === "string") {
		if (typeof schemaValue.minLength === "number" && value.length < schemaValue.minLength) {
			pushSchemaError(errors, path, `must be at least ${schemaValue.minLength} characters`);
		}
		if (typeof schemaValue.pattern === "string") {
			try {
				if (!new RegExp(schemaValue.pattern).test(value)) {
					pushSchemaError(errors, path, `must match pattern ${schemaValue.pattern}`);
				}
			} catch (_error) {
				pushSchemaError(errors, path, `has invalid schema pattern ${schemaValue.pattern}`);
			}
		}
	}

	if (typeof value === "number" && typeof schemaValue.minimum === "number" && value < schemaValue.minimum) {
		pushSchemaError(errors, path, `must be at least ${schemaValue.minimum}`);
	}

	if (Array.isArray(value) && isJsonObject(schemaValue.items)) {
		for (const [index, item] of value.entries()) {
			validateSchemaNode(item, schemaValue.items, rootSchema, childPath(path, index), errors, seenRefs);
		}
	}

	if (isJsonObject(value)) {
		const properties = isJsonObject(schemaValue.properties) ? schemaValue.properties : {};
		if (Array.isArray(schemaValue.required)) {
			for (const requiredKey of schemaValue.required) {
				if (typeof requiredKey === "string" && !Object.prototype.hasOwnProperty.call(value, requiredKey)) {
					pushSchemaError(errors, childPath(path, requiredKey), "is required");
				}
			}
		}
		for (const [key, child] of Object.entries(properties)) {
			if (Object.prototype.hasOwnProperty.call(value, key)) {
				validateSchemaNode(value[key], child, rootSchema, childPath(path, key), errors, seenRefs);
			}
		}
		if (schemaValue.additionalProperties === false) {
			for (const key of Object.keys(value)) {
				if (!Object.prototype.hasOwnProperty.call(properties, key)) {
					pushSchemaError(errors, childPath(path, key), "is not an allowed property");
				}
			}
		} else if (isJsonObject(schemaValue.additionalProperties)) {
			for (const key of Object.keys(value)) {
				if (!Object.prototype.hasOwnProperty.call(properties, key)) {
					validateSchemaNode(value[key], schemaValue.additionalProperties, rootSchema, childPath(path, key), errors, seenRefs);
				}
			}
		}
	}
}

function validateAgainstConfiguredSchema(value: unknown, schema: JsonObject): SchemaValidationFailure[] {
	const errors: SchemaValidationFailure[] = [];
	validateSchemaNode(value, schema, schema, "", errors, new Set());
	return errors;
}

function formatSchemaValidationFailures(errors: SchemaValidationFailure[]): string {
	const suffix = errors.length >= MAX_SCHEMA_VALIDATION_ERRORS ? "; additional errors omitted" : "";
	return errors.map((error) => `${error.path}: ${error.message}`).join("; ") + suffix;
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
		validation_schema: schemaValue,
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
			const schemaErrors = validateAgainstConfiguredSchema(params, spec.validation_schema);
			if (schemaErrors.length > 0) {
				throw new Error(
					`${spec.tool_name} arguments failed configured JSON Schema ${spec.parameters_schema_path}: ${formatSchemaValidationFailures(schemaErrors)}`,
				);
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
