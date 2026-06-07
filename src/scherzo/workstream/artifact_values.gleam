import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/json_value
import scherzo/workstream/types

pub fn workstream_to_value(
  value: types.WorkstreamArtifact,
) -> json_value.JsonValue {
  json_value.JObject([
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.workstream_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("task_ref", task_ref_to_value(value.task_ref)),
    #("status", json_value.JString(value.status)),
    #("summary", json_value.JString(value.summary)),
    #(
      "produced_artifacts",
      json_value.JArray(list.map(value.produced_artifacts, snapshot_to_value)),
    ),
    #(
      "next_actions",
      json_value.JArray(list.map(value.next_actions, json_value.JString)),
    ),
  ])
}

pub fn handoff_to_value(value: types.HandoffArtifact) -> json_value.JsonValue {
  json_value.JObject([
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.handoff_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("phase_id", json_value.JString(value.phase_id)),
    #("summary", json_value.JString(value.summary)),
    #(
      "outputs",
      json_value.JArray(list.map(value.outputs, handoff_output_to_value)),
    ),
    #(
      "recommended_next_actions",
      json_value.JArray(list.map(
        value.recommended_next_actions,
        json_value.JString,
      )),
    ),
    #(
      "open_questions",
      json_value.JArray(list.map(value.open_questions, json_value.JString)),
    ),
  ])
}

pub fn decision_to_value(
  value: types.DecisionArtifact,
) -> json_value.JsonValue {
  json_value.JObject([
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.decision_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("action_id", json_value.JString(value.action_id)),
    #("gate_id", json_value.JString(value.gate_id)),
    #("kind", json_value.JString(value.kind)),
    #("decided_at_ms", json_value.JInt(value.decided_at_ms)),
    #("decided_by", json_value.JString(value.decided_by)),
    #("rationale", json_value.JString(value.rationale)),
    #(
      "inputs",
      json_value.JArray(list.map(value.inputs, decision_input_to_value)),
    ),
    #("summary", json_value.JString(value.summary)),
  ])
}

pub fn input_bundle_to_value(
  value: types.InputBundleArtifact,
) -> json_value.JsonValue {
  let base = [
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.input_bundle_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("source_handoff_ref", json_value.JString(value.source_handoff_ref)),
    #("workflow_id", json_value.JString(value.workflow_id)),
    #(
      "inputs",
      json_value.JArray(list.map(value.inputs, input_binding_to_value)),
    ),
  ]
  let base = case value.source_kind {
    Some(source_kind) ->
      list.append(base, [#("source_kind", json_value.JString(source_kind))])
    None -> base
  }
  let base = case value.source_reason {
    Some(source_reason) ->
      list.append(base, [#("source_reason", json_value.JString(source_reason))])
    None -> base
  }
  json_value.JObject(base)
}

pub fn assignment_to_value(
  value: types.AssignmentArtifact,
) -> json_value.JsonValue {
  let base = [
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.assignment_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("workflow_id", json_value.JString(value.workflow_id)),
    #("reason", json_value.JString(value.reason)),
  ]
  case value.playbook_id {
    Some(playbook_id) ->
      json_value.JObject(
        list.append(base, [#("playbook_id", json_value.JString(playbook_id))]),
      )
    None -> json_value.JObject(base)
  }
}

pub fn next_action_to_value(
  value: types.NextActionArtifact,
) -> json_value.JsonValue {
  let base = [
    #("schema_version", json_value.JInt(types.schema_version)),
    #("artifact_type", json_value.JString(types.next_action_artifact_type)),
    #("artifact_id", json_value.JString(value.artifact_id)),
    #("workstream_id", json_value.JString(value.workstream_id)),
    #("action_id", json_value.JString(value.action_id)),
    #("workflow_id", json_value.JString(value.workflow_id)),
    #("state", json_value.JString(value.state)),
    #("priority", json_value.JInt(value.priority)),
    #("inputs", json_value.JArray(list.map(value.inputs, json_value.JString))),
    #("auto_enqueue", json_value.JBool(value.auto_enqueue)),
  ]
  let base = case value.requires_gate {
    Some(gate) ->
      list.append(base, [#("requires_gate", json_value.JString(gate))])
    None -> base
  }
  json_value.JObject(base)
}

fn task_ref_to_value(value: types.TaskRef) -> json_value.JsonValue {
  let base = [
    #("backend_kind", json_value.JString(value.backend_kind)),
    #("remote_id", json_value.JString(value.remote_id)),
  ]
  let base = append_optional_string(base, "key", value.key)
  let base = append_optional_string(base, "url", value.url)
  json_value.JObject(base)
}

fn snapshot_to_value(value: types.ArtifactSnapshot) -> json_value.JsonValue {
  let base = [
    #("ref", json_value.JString(value.ref)),
    #("sha256", json_value.JString(value.sha256)),
    #("bytes", json_value.JInt(value.bytes)),
    #("media_type", json_value.JString(value.media_type)),
    #("original_path", json_value.JString(value.original_path)),
    #("descriptor", descriptor_to_value(value.descriptor)),
    #("producer", producer_to_value(value.producer)),
    #("validation", validation_to_value(value.validation)),
    #("summary", json_value.JString(value.summary)),
  ]
  let base = append_optional_string(base, "contract_type", value.contract_type)
  json_value.JObject(base)
}

fn handoff_output_to_value(value: types.HandoffOutput) -> json_value.JsonValue {
  json_value.JObject([
    #("name", json_value.JString(value.name)),
    #("snapshot", snapshot_to_value(value.snapshot)),
  ])
}

fn decision_input_to_value(
  value: types.DecisionInputRef,
) -> json_value.JsonValue {
  json_value.JObject([
    #("name", json_value.JString(value.name)),
    #("ref", json_value.JString(value.ref)),
    #("sha256", json_value.JString(value.sha256)),
  ])
}

fn input_binding_to_value(value: types.InputBinding) -> json_value.JsonValue {
  let base = [
    #("name", json_value.JString(value.name)),
    #("descriptor", descriptor_to_value(value.descriptor)),
    #("value_ref", json_value.JString(value.value_ref)),
  ]
  let base = append_optional_string(base, "contract_type", value.contract_type)
  let base = append_optional_string(base, "sha256", value.sha256)
  let base = case value.bytes {
    Some(bytes) -> list.append(base, [#("bytes", json_value.JInt(bytes))])
    None -> base
  }
  let base = append_optional_string(base, "media_type", value.media_type)
  let base = append_optional_string(base, "original_path", value.original_path)
  let base = append_optional_string(base, "source_kind", value.source_kind)
  json_value.JObject(base)
}

fn descriptor_to_value(
  value: types.ContractDescriptorRecord,
) -> json_value.JsonValue {
  let base = [#("kind", json_value.JString(value.kind))]
  let base = append_optional_string(base, "ref_type", value.ref_type)
  let base = append_optional_string(base, "media_type", value.media_type)
  let base = append_optional_string(base, "artifact_type", value.artifact_type)
  let base = append_optional_json(base, "source", value.source)
  let base = append_optional_json(base, "validation", value.validation)
  let base = append_optional_json(base, "metadata", value.metadata)
  json_value.JObject(base)
}

fn append_optional_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  value: Option(String),
) -> List(#(String, json_value.JsonValue)) {
  case value {
    Some(value) -> list.append(entries, [#(key, json_value.JString(value))])
    None -> entries
  }
}

fn append_optional_json(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
  value: Option(json_value.JsonValue),
) -> List(#(String, json_value.JsonValue)) {
  case value {
    Some(value) -> list.append(entries, [#(key, value)])
    None -> entries
  }
}

fn producer_to_value(value: types.ProducerRef) -> json_value.JsonValue {
  json_value.JObject([
    #("workflow_id", json_value.JString(value.workflow_id)),
    #("run_id", json_value.JString(value.run_id)),
    #("step_id", json_value.JString(value.step_id)),
  ])
}

fn validation_to_value(value: types.ValidationSummary) -> json_value.JsonValue {
  json_value.JObject([
    #("status", json_value.JString(value.status)),
    #("validator", json_value.JString(value.validator)),
    #("checked_at", json_value.JString(value.checked_at)),
  ])
}
