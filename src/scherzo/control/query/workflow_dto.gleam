import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json
import scherzo/control/query/types

pub fn workflow_list_to_json(workflows: types.WorkflowListDto) -> json.Json {
  json.object([
    #("schema_version", json.int(workflows.schema_version)),
    #("freshness", freshness_to_json(workflows.freshness)),
    #("diagnostics", json.array(workflows.diagnostics, of: diagnostic_to_json)),
    #("workflows", json.array(workflows.workflows, of: summary_to_json)),
  ])
}

pub fn workflow_detail_to_json(workflow: types.WorkflowDetailDto) -> json.Json {
  json.object([
    #("schema_version", json.int(workflow.schema_version)),
    #("summary", summary_to_json(workflow.summary)),
    #(
      "yaml_sources",
      json.array(workflow.yaml_sources, of: yaml_source_to_json),
    ),
    #("diagnostics", json.array(workflow.diagnostics, of: diagnostic_to_json)),
    #("freshness", freshness_to_json(workflow.freshness)),
    #("graph", graph_to_json(workflow.graph)),
  ])
}

pub fn decode_workflow_list_dynamic(
  value: Dynamic,
) -> Result(types.WorkflowListDto, types.QueryError) {
  case decode.run(value, workflow_list_decoder()) {
    Ok(workflows) -> validate_workflow_list_schema(workflows)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid workflow list query payload",
      ))
  }
}

pub fn decode_workflow_detail_dynamic(
  value: Dynamic,
) -> Result(types.WorkflowDetailDto, types.QueryError) {
  case decode.run(value, workflow_detail_decoder()) {
    Ok(workflow) -> validate_workflow_detail_schema(workflow)
    Error(_) ->
      Error(types.QueryError(
        types.QueryBackendFailed,
        "invalid workflow detail query payload",
      ))
  }
}

fn summary_to_json(summary: types.WorkflowSummaryDto) -> json.Json {
  json.object([
    #("id", json.string(summary.id)),
    #("name", json.string(summary.name)),
    #("route", json.nullable(summary.route, of: json.string)),
    #("label", json.nullable(summary.label, of: json.string)),
    #("yaml_paths", json.array(summary.yaml_paths, of: json.string)),
    #("step_count", json.int(summary.step_count)),
    #("status", json.string(summary.status)),
  ])
}

fn yaml_source_to_json(source: types.WorkflowYamlSourceDto) -> json.Json {
  json.object([
    #("path", json.string(source.path)),
    #("contents", json.string(source.contents)),
    #("contents_sha256", json.string(source.contents_sha256)),
    #("contents_truncated", json.bool(source.contents_truncated)),
  ])
}

fn diagnostic_to_json(diagnostic: types.WorkflowDiagnosticDto) -> json.Json {
  json.object([
    #("severity", json.string(diagnostic.severity)),
    #("code", json.string(diagnostic.code)),
    #("message", json.string(diagnostic.message)),
    #("path", json.nullable(diagnostic.path, of: json.string)),
  ])
}

fn freshness_to_json(freshness: types.WorkflowFreshnessDto) -> json.Json {
  json.object([
    #("source_hash", json.string(freshness.source_hash)),
    #("reload_status", json.string(freshness.reload_status)),
  ])
}

fn graph_to_json(graph: types.WorkflowGraphDto) -> json.Json {
  json.object([
    #("nodes", json.array(graph.nodes, of: graph_node_to_json)),
    #("edges", json.array(graph.edges, of: graph_edge_to_json)),
  ])
}

fn graph_node_to_json(node: types.WorkflowGraphNodeDto) -> json.Json {
  json.object([
    #("id", json.string(node.id)),
    #("label", json.string(node.label)),
    #("kind", json.string(node.kind)),
  ])
}

fn graph_edge_to_json(edge: types.WorkflowGraphEdgeDto) -> json.Json {
  json.object([
    #("from", json.string(edge.from)),
    #("to", json.string(edge.to)),
  ])
}

fn workflow_list_decoder() -> decode.Decoder(types.WorkflowListDto) {
  use schema_version <- decode.field("schema_version", decode.int)
  use freshness <- decode.field("freshness", freshness_decoder())
  use diagnostics <- decode.field(
    "diagnostics",
    decode.list(diagnostic_decoder()),
  )
  use workflows <- decode.field("workflows", decode.list(summary_decoder()))
  decode.success(types.WorkflowListDto(
    schema_version: schema_version,
    freshness: freshness,
    diagnostics: diagnostics,
    workflows: workflows,
  ))
}

fn workflow_detail_decoder() -> decode.Decoder(types.WorkflowDetailDto) {
  use schema_version <- decode.field("schema_version", decode.int)
  use summary <- decode.field("summary", summary_decoder())
  use yaml_sources <- decode.field(
    "yaml_sources",
    decode.list(yaml_source_decoder()),
  )
  use diagnostics <- decode.field(
    "diagnostics",
    decode.list(diagnostic_decoder()),
  )
  use freshness <- decode.field("freshness", freshness_decoder())
  use graph <- decode.field("graph", graph_decoder())
  decode.success(types.WorkflowDetailDto(
    schema_version: schema_version,
    summary: summary,
    yaml_sources: yaml_sources,
    diagnostics: diagnostics,
    freshness: freshness,
    graph: graph,
  ))
}

fn summary_decoder() -> decode.Decoder(types.WorkflowSummaryDto) {
  use id <- decode.field("id", decode.string)
  use name <- decode.field("name", decode.string)
  use route <- decode.field("route", decode.optional(decode.string))
  use label <- decode.field("label", decode.optional(decode.string))
  use yaml_paths <- decode.field("yaml_paths", decode.list(decode.string))
  use step_count <- decode.field("step_count", decode.int)
  use status <- decode.field("status", decode.string)
  decode.success(types.WorkflowSummaryDto(
    id: id,
    name: name,
    route: route,
    label: label,
    yaml_paths: yaml_paths,
    step_count: step_count,
    status: status,
  ))
}

fn yaml_source_decoder() -> decode.Decoder(types.WorkflowYamlSourceDto) {
  use path <- decode.field("path", decode.string)
  use contents <- decode.field("contents", decode.string)
  use contents_sha256 <- decode.field("contents_sha256", decode.string)
  use contents_truncated <- decode.field("contents_truncated", decode.bool)
  decode.success(types.WorkflowYamlSourceDto(
    path: path,
    contents: contents,
    contents_sha256: contents_sha256,
    contents_truncated: contents_truncated,
  ))
}

fn diagnostic_decoder() -> decode.Decoder(types.WorkflowDiagnosticDto) {
  use severity <- decode.field("severity", decode.string)
  use code <- decode.field("code", decode.string)
  use message <- decode.field("message", decode.string)
  use path <- decode.field("path", decode.optional(decode.string))
  decode.success(types.WorkflowDiagnosticDto(
    severity: severity,
    code: code,
    message: message,
    path: path,
  ))
}

fn freshness_decoder() -> decode.Decoder(types.WorkflowFreshnessDto) {
  use source_hash <- decode.field("source_hash", decode.string)
  use reload_status <- decode.field("reload_status", decode.string)
  decode.success(types.WorkflowFreshnessDto(
    source_hash: source_hash,
    reload_status: reload_status,
  ))
}

fn graph_decoder() -> decode.Decoder(types.WorkflowGraphDto) {
  use nodes <- decode.field("nodes", decode.list(graph_node_decoder()))
  use edges <- decode.field("edges", decode.list(graph_edge_decoder()))
  decode.success(types.WorkflowGraphDto(nodes: nodes, edges: edges))
}

fn graph_node_decoder() -> decode.Decoder(types.WorkflowGraphNodeDto) {
  use id <- decode.field("id", decode.string)
  use label <- decode.field("label", decode.string)
  use kind <- decode.field("kind", decode.string)
  decode.success(types.WorkflowGraphNodeDto(id: id, label: label, kind: kind))
}

fn graph_edge_decoder() -> decode.Decoder(types.WorkflowGraphEdgeDto) {
  use from <- decode.field("from", decode.string)
  use to <- decode.field("to", decode.string)
  decode.success(types.WorkflowGraphEdgeDto(from: from, to: to))
}

fn validate_workflow_list_schema(
  workflows: types.WorkflowListDto,
) -> Result(types.WorkflowListDto, types.QueryError) {
  case workflows.schema_version == types.workflow_query_schema_version {
    True -> Ok(workflows)
    False -> unsupported_workflow_schema_version()
  }
}

fn validate_workflow_detail_schema(
  workflow: types.WorkflowDetailDto,
) -> Result(types.WorkflowDetailDto, types.QueryError) {
  case workflow.schema_version == types.workflow_query_schema_version {
    True -> Ok(workflow)
    False -> unsupported_workflow_schema_version()
  }
}

fn unsupported_workflow_schema_version() -> Result(a, types.QueryError) {
  Error(types.QueryError(
    types.QueryBackendFailed,
    "unsupported workflow query schema version",
  ))
}
