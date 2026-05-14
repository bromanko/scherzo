import { readFileSync } from "node:fs";
import { join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
	createAssistantMessageEventStream,
	type AssistantMessage,
	type AssistantMessageEventStream,
	type Context,
	type Model,
	type SimpleStreamOptions,
} from "@mariozechner/pi-ai";

const providerName = "scherzo-raw-schema-smoke";
const modelName = "scherzo-raw-schema-smoke";
const fixtureDir = "test/fixtures/structured_output/pi_raw_schema";

function fixture(name: string): Record<string, unknown> {
	return JSON.parse(readFileSync(join(fixtureDir, name), "utf8")) as Record<string, unknown>;
}

function contextContainsToolError(context: Context): boolean {
	const serialized = JSON.stringify(context).toLowerCase();
	return serialized.includes("toolresult") && (serialized.includes("error") || serialized.includes("iserror"));
}

function emitToolCall(model: Model<any>, params: Record<string, unknown>, id: string): AssistantMessageEventStream {
	const stream = createAssistantMessageEventStream();
	queueMicrotask(() => {
		const output: AssistantMessage = {
			role: "assistant",
			content: [],
			api: model.api,
			provider: model.provider,
			model: model.id,
			usage: {
				input: 1,
				output: 1,
				cacheRead: 0,
				cacheWrite: 0,
				totalTokens: 2,
				cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
			},
			stopReason: "toolUse",
			timestamp: Date.now(),
		};
		stream.push({ type: "start", partial: output });
		const contentIndex = output.content.length;
		const toolCall = {
			type: "toolCall" as const,
			id,
			name: "submit_structured_output",
			arguments: params,
		};
		output.content.push(toolCall);
		stream.push({ type: "toolcall_start", contentIndex, partial: output });
		stream.push({ type: "toolcall_delta", contentIndex, delta: JSON.stringify(params), partial: output });
		stream.push({ type: "toolcall_end", contentIndex, toolCall, partial: output });
		stream.push({ type: "done", reason: "toolUse", message: output });
		stream.end();
	});
	return stream;
}

function emitInvalidReachedExecute(model: Model<any>): AssistantMessageEventStream {
	const stream = createAssistantMessageEventStream();
	queueMicrotask(() => {
		const output: AssistantMessage = {
			role: "assistant",
			content: [{ type: "text", text: "RAW_SCHEMA_SMOKE_INVALID_REACHED_EXECUTE" }],
			api: model.api,
			provider: model.provider,
			model: model.id,
			usage: { input: 1, output: 1, cacheRead: 0, cacheWrite: 0, totalTokens: 2, cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 } },
			stopReason: "stop",
			timestamp: Date.now(),
		};
		stream.push({ type: "start", partial: output });
		stream.push({ type: "text_start", contentIndex: 0, partial: output });
		stream.push({ type: "text_end", contentIndex: 0, content: "RAW_SCHEMA_SMOKE_INVALID_REACHED_EXECUTE", partial: output });
		stream.push({ type: "done", reason: "stop", message: output });
		stream.end();
	});
	return stream;
}

function schemaInvalidFixture(): Record<string, unknown> {
	const invalid = fixture("valid-review-lane.arguments.json");
	delete invalid.schema_version;
	return invalid;
}

function streamSimple(model: Model<any>, context: Context, _options?: SimpleStreamOptions): AssistantMessageEventStream {
	const serialized = JSON.stringify(context);
	if (serialized.includes("Accepted review_lane_draft") || serialized.includes("scherzo_structured_output_tool_receipt")) {
		return emitInvalidReachedExecute(model);
	}
	if (contextContainsToolError(context)) {
		return emitToolCall(model, fixture("valid-review-lane.arguments.json"), "raw-schema-smoke-valid");
	}
	return emitToolCall(model, schemaInvalidFixture(), "raw-schema-smoke-invalid");
}

export default function smokeProvider(pi: ExtensionAPI) {
	pi.registerProvider(providerName, {
		name: "Scherzo Raw Schema Smoke",
		baseUrl: "http://127.0.0.1/scherzo-raw-schema-smoke",
		apiKey: "SCHERZO_RAW_SCHEMA_SMOKE_API_KEY",
		api: "scherzo-raw-schema-smoke" as any,
		streamSimple,
		models: [
			{
				id: modelName,
				name: "Scherzo Raw Schema Smoke",
				reasoning: false,
				input: ["text"],
				cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
				contextWindow: 16_000,
				maxTokens: 1024,
			},
		],
	});
}
