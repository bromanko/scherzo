import { defineTool, type ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { reviewLaneDraftParameters } from "./review_lane_draft_schema";

export const submitReviewLaneDraft = defineTool({
  name: "submit_review_lane_draft",
  label: "Submit Review Lane Draft",
  description:
    "Submit the final native review lane draft. Remote mutations are forbidden; remote_mutations must be none.",
  promptSnippet: "Submit the final native review lane draft as a terminating tool call",
  promptGuidelines: [
    "Use submit_review_lane_draft exactly once as the final action for native review lane drafts.",
    "Do not print the review lane draft as final assistant JSON after calling submit_review_lane_draft.",
    "Do not batch submit_review_lane_draft with any other tool call.",
  ],
  parameters: reviewLaneDraftParameters,
  async execute(_toolCallId, params) {
    return {
      content: [{ type: "text" as const, text: "Review lane draft submitted." }],
      details: {
        artifact_type: params.artifact_type,
        remote_mutations: params.remote_mutations,
      },
      terminate: true,
    };
  },
});

export default function (pi: ExtensionAPI) {
  pi.registerTool(submitReviewLaneDraft);
}
