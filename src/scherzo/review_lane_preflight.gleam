import birl
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/control/file as control_file
import scherzo/hash
import scherzo/json_value
import scherzo/path
import scherzo/review_lane_live_probe
import scherzo/review_lane_preflight_policy
import scherzo/review_lane_tools
import scherzo/structured_output_tool_spec
import scherzo/workflow_dag
import scherzo/workflow_fingerprint
import simplifile

pub const cache_filename = "review-lane-contract-cache.v1.json"

const checker_version = "review-lane-contract-v1"

const live_report_filename = "live-probe-report.v1.json"

pub type SchemaDigest {
  SchemaDigest(path: String, sha256: String)
}

type CacheMaterial {
  CacheMaterial(
    cache_key: String,
    workflow_id: String,
    workflow_fingerprint: String,
    provider_name: String,
    model_name: String,
    tool_names: List(String),
    schema_digests: List(SchemaDigest),
    mode: review_lane_preflight_policy.PreflightMode,
  )
}

type CacheWriteError {
  CacheWriteError(message: String)
}

pub type PreflightResult {
  PreflightPassed(cache_key: String, warnings: List(String))
  PreflightFailed(
    cache_key: String,
    code: String,
    message: String,
    blocking: Bool,
  )
}

pub fn policy_from_env() -> review_lane_preflight_policy.Policy {
  review_lane_preflight_policy.from_env()
}

pub fn for_workflow(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  workflow_path: String,
  state_root: String,
  effective: config_types.EffectiveConfig,
  policy: review_lane_preflight_policy.Policy,
  now_ms: Int,
) -> PreflightResult {
  let fingerprint = workflow_fingerprint.for_dag(workflow_id, dag)
  case review_lane_tools(workflow_id, dag, repository_root) {
    Error(err) ->
      failed(
        fallback_cache_key(workflow_id, fingerprint, effective, policy.mode),
        err.code,
        err.message,
        True,
      )
    Ok(tools) -> {
      let material =
        cache_material(workflow_id, fingerprint, effective, tools, policy.mode)
      case policy.mode {
        review_lane_preflight_policy.Off -> passed(material.cache_key)
        review_lane_preflight_policy.OfflineRequired ->
          cached_or_run(
            workflow_path,
            dag,
            repository_root,
            state_root,
            material,
            effective,
            policy,
            now_ms,
          )
        review_lane_preflight_policy.RequiredLive ->
          case live_credentials_present() {
            False ->
              failed(
                material.cache_key,
                "review_lane_live_credentials_missing",
                "review-lane live preflight is required but no supported provider credentials are present",
                True,
              )
            True ->
              cached_or_run(
                workflow_path,
                dag,
                repository_root,
                state_root,
                material,
                effective,
                policy,
                now_ms,
              )
          }
      }
    }
  }
}

fn cached_or_run(
  workflow_path: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  state_root: String,
  material: CacheMaterial,
  effective: config_types.EffectiveConfig,
  policy: review_lane_preflight_policy.Policy,
  now_ms: Int,
) -> PreflightResult {
  case read_cached_result(state_root, material.cache_key, now_ms) {
    Some(result) -> result
    None -> {
      let result =
        run_uncached_preflight(
          workflow_path,
          dag,
          repository_root,
          state_root,
          material,
          effective,
          policy,
        )
      persist_cache(state_root, material, result, now_ms, policy)
      result
    }
  }
}

pub fn review_lane_tools(
  workflow_id: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
) -> Result(
  List(review_lane_tools.ReviewLaneTool),
  structured_output_tool_spec.ToolSpecError,
) {
  review_lane_tools.for_workflow(workflow_id, dag, repository_root)
}

fn run_uncached_preflight(
  workflow_path: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  state_root: String,
  material: CacheMaterial,
  effective: config_types.EffectiveConfig,
  policy: review_lane_preflight_policy.Policy,
) -> PreflightResult {
  case policy.mode {
    review_lane_preflight_policy.Off -> passed(material.cache_key)
    review_lane_preflight_policy.OfflineRequired -> passed(material.cache_key)
    review_lane_preflight_policy.RequiredLive ->
      case live_credentials_present() {
        False ->
          failed(
            material.cache_key,
            "review_lane_live_credentials_missing",
            "review-lane live preflight is required but no supported provider credentials are present",
            True,
          )
        True ->
          run_required_live_probe(
            workflow_path,
            dag,
            repository_root,
            state_root,
            material,
            effective,
            policy,
          )
      }
  }
}

fn run_required_live_probe(
  workflow_path: String,
  dag: workflow_dag.WorkflowDag,
  repository_root: String,
  state_root: String,
  material: CacheMaterial,
  effective: config_types.EffectiveConfig,
  policy: review_lane_preflight_policy.Policy,
) -> PreflightResult {
  let output_dir =
    live_output_dir(state_root, material.workflow_id, material.cache_key)
  case
    review_lane_live_probe.probe_loaded_workflow(
      workflow_path,
      dag,
      repository_root,
      effective,
      output_dir,
      False,
    )
  {
    Error(err) -> failed(material.cache_key, err.code, err.message, True)
    Ok(report) -> live_report_to_preflight(material.cache_key, report, policy)
  }
}

fn live_report_to_preflight(
  cache_key: String,
  report: review_lane_live_probe.ProbeReport,
  policy: review_lane_preflight_policy.Policy,
) -> PreflightResult {
  case report.status {
    "passed" ->
      PreflightPassed(cache_key: cache_key, warnings: [
        "required-live mode provider-backed review-lane probe passed; report: "
        <> path.join(report.output_dir, live_report_filename),
      ])
    "skipped" ->
      failed(
        cache_key,
        optional_string(report.code, "review_lane_live_credentials_missing"),
        optional_string(report.message, "required-live provider probe skipped"),
        True,
      )
    _ -> {
      let code = optional_string(report.code, "review_lane_live_probe_failed")
      failed(
        cache_key,
        code,
        optional_string(report.message, "required-live provider probe failed"),
        live_failure_blocks(code, policy.strict_live_model_checks),
      )
    }
  }
}

fn optional_string(value: Option(String), fallback: String) -> String {
  case value {
    Some(text) -> text
    None -> fallback
  }
}

fn optional_bool(value: Option(Bool), fallback: Bool) -> Bool {
  case value {
    Some(bool) -> bool
    None -> fallback
  }
}

fn live_failure_blocks(code: String, strict_live_model_checks: Bool) -> Bool {
  case code {
    "model_payload_invalid" | "repair_loop_failed" -> strict_live_model_checks
    _ -> True
  }
}

pub fn cache_path(state_root: String) -> String {
  path.join(state_root, cache_filename)
}

pub fn read_cached_result(
  state_root: String,
  cache_key: String,
  now_ms: Int,
) -> Option(PreflightResult) {
  case simplifile.read(cache_path(state_root)) {
    Error(err) -> {
      let _reason = simplifile.describe_error(err)
      None
    }
    Ok(contents) -> decode_cached_result(contents, cache_key, now_ms)
  }
}

pub fn decode_cached_result(
  contents: String,
  cache_key: String,
  now_ms: Int,
) -> Option(PreflightResult) {
  case json_value.parse(contents) {
    Error(Nil) -> None
    Ok(json_value.JObject(entries)) ->
      case object_field(entries, "entries") {
        Some(json_value.JArray(values)) ->
          cached_result_loop(values, cache_key, now_ms)
        _ -> None
      }
    Ok(_) -> None
  }
}

fn cached_result_loop(
  values: List(json_value.JsonValue),
  cache_key: String,
  now_ms: Int,
) -> Option(PreflightResult) {
  case values {
    [] -> None
    [value, ..rest] ->
      case cache_entry_to_result(value, cache_key, now_ms) {
        Some(result) -> Some(result)
        None -> cached_result_loop(rest, cache_key, now_ms)
      }
  }
}

fn cache_entry_to_result(
  value: json_value.JsonValue,
  expected_cache_key: String,
  now_ms: Int,
) -> Option(PreflightResult) {
  case value {
    json_value.JObject(entries) -> {
      let key = object_string(entries, "cache_key")
      let expires_at_ms = object_int(entries, "expires_at_ms")
      let status = object_string(entries, "status")
      case key, expires_at_ms, status {
        Some(cache_key), Some(expires_at_ms), Some(status) ->
          case cache_key == expected_cache_key && expires_at_ms > now_ms {
            False -> None
            True -> decoded_cache_status(entries, cache_key, status)
          }
        _, _, _ -> None
      }
    }
    _ -> None
  }
}

fn decoded_cache_status(
  entries: List(#(String, json_value.JsonValue)),
  cache_key: String,
  status: String,
) -> Option(PreflightResult) {
  case status {
    "passed" ->
      Some(PreflightPassed(
        cache_key: cache_key,
        warnings: cache_warnings(entries),
      ))
    "failed" -> {
      let code =
        optional_string(
          object_string(entries, "code"),
          "review_lane_preflight_cached_failure",
        )
      let message =
        optional_string(
          object_string(entries, "message"),
          "cached review-lane preflight failure",
        )
      let blocking = optional_bool(object_bool(entries, "blocking"), True)
      Some(PreflightFailed(
        cache_key: cache_key,
        code: code,
        message: message,
        blocking: blocking,
      ))
    }
    _ -> None
  }
}

fn cache_warnings(
  entries: List(#(String, json_value.JsonValue)),
) -> List(String) {
  case object_field(entries, "warnings") {
    Some(json_value.JArray(values)) -> json_string_list(values, [])
    _ -> ["review-lane preflight cache hit"]
  }
}

fn json_string_list(
  values: List(json_value.JsonValue),
  acc: List(String),
) -> List(String) {
  case values {
    [] -> list.reverse(acc)
    [json_value.JString(value), ..rest] ->
      json_string_list(rest, [value, ..acc])
    [_, ..rest] -> json_string_list(rest, acc)
  }
}

fn persist_cache(
  state_root: String,
  material: CacheMaterial,
  preflight_result: PreflightResult,
  now_ms: Int,
  policy: review_lane_preflight_policy.Policy,
) -> Nil {
  let report_path =
    path.join(
      live_output_dir(state_root, material.workflow_id, material.cache_key),
      live_report_filename,
    )
  let _cache_write_result =
    write_cache_entry(
      state_root,
      material,
      preflight_result,
      now_ms,
      policy.cache_ttl_seconds,
      report_path,
    )
  Nil
}

fn write_cache_entry(
  state_root: String,
  material: CacheMaterial,
  preflight_result: PreflightResult,
  now_ms: Int,
  ttl_seconds: Int,
  report_path: String,
) -> Result(Nil, CacheWriteError) {
  let cache_path = cache_path(state_root)
  let old_entries = existing_cache_entries(cache_path, material.cache_key)
  let expires_at_ms = now_ms + ttl_to_ms(ttl_seconds)
  let entry =
    cache_entry_json(
      material,
      preflight_result,
      now_ms,
      expires_at_ms,
      report_path,
    )
  let document =
    json.object([
      #("schema_version", json.int(1)),
      #("artifact_type", json.string("review_lane_preflight_cache")),
      #("entries", json.array([entry, ..old_entries], of: identity_json)),
    ])
  use Nil <- result.try(ensure_parent_dir(cache_path))
  simplifile.write(cache_path, json.to_string(document) <> "\n")
  |> result.map_error(fn(err) {
    CacheWriteError(simplifile.describe_error(err))
  })
}

fn existing_cache_entries(
  cache_path: String,
  replaced_key: String,
) -> List(json.Json) {
  case simplifile.read(cache_path) {
    Error(err) -> {
      let _reason = simplifile.describe_error(err)
      []
    }
    Ok(contents) ->
      case json_value.parse(contents) {
        Error(Nil) -> []
        Ok(json_value.JObject(entries)) ->
          case object_field(entries, "entries") {
            Some(json_value.JArray(values)) ->
              retained_cache_entries(values, replaced_key, [])
            _ -> []
          }
        Ok(_) -> []
      }
  }
}

fn retained_cache_entries(
  values: List(json_value.JsonValue),
  replaced_key: String,
  acc: List(json.Json),
) -> List(json.Json) {
  case values {
    [] -> list.reverse(acc)
    [value, ..rest] ->
      case cache_value_key(value) == Some(replaced_key) {
        True -> retained_cache_entries(rest, replaced_key, acc)
        False ->
          retained_cache_entries(rest, replaced_key, [
            json_value.to_json(value),
            ..acc
          ])
      }
  }
}

fn cache_value_key(value: json_value.JsonValue) -> Option(String) {
  case value {
    json_value.JObject(entries) -> object_string(entries, "cache_key")
    _ -> None
  }
}

fn cache_entry_json(
  material: CacheMaterial,
  preflight_result: PreflightResult,
  checked_at_ms: Int,
  expires_at_ms: Int,
  report_path: String,
) -> json.Json {
  json.object([
    #("cache_key", json.string(material.cache_key)),
    #("workflow_id", json.string(material.workflow_id)),
    #("workflow_fingerprint", json.string(material.workflow_fingerprint)),
    #("provider_name", json.string(material.provider_name)),
    #("model_name", json.string(material.model_name)),
    #("tool_names", json.array(material.tool_names, of: json.string)),
    #(
      "schema_digests",
      json.array(material.schema_digests, of: schema_digest_json),
    ),
    #("checker_version", json.string(checker_version)),
    #(
      "mode",
      json.string(review_lane_preflight_policy.mode_to_string(material.mode)),
    ),
    #("status", json.string(result_status(preflight_result))),
    #("blocking", json.bool(blocking(preflight_result))),
    #("code", json.string(code(preflight_result))),
    #("message", json.string(message(preflight_result))),
    #("warnings", json.array(warnings(preflight_result), of: json.string)),
    #("report_path", json.string(report_path)),
    #("checked_at_ms", json.int(checked_at_ms)),
    #("expires_at_ms", json.int(expires_at_ms)),
    #("checked_at_utc", json.string(iso_utc(checked_at_ms))),
    #("expires_at_utc", json.string(iso_utc(expires_at_ms))),
  ])
}

fn schema_digest_json(schema: SchemaDigest) -> json.Json {
  json.object([
    #("path", json.string(schema.path)),
    #("sha256", json.string(schema.sha256)),
  ])
}

fn identity_json(value: json.Json) -> json.Json {
  value
}

fn ttl_to_ms(ttl_seconds: Int) -> Int {
  case ttl_seconds <= 0 {
    True -> 0
    False -> ttl_seconds * 1000
  }
}

fn ensure_parent_dir(file_path: String) -> Result(Nil, CacheWriteError) {
  let directory = case path.dirname(file_path) {
    Ok(value) -> value
    Error(Nil) -> "."
  }
  simplifile.create_directory_all(directory)
  |> result.map_error(fn(err) {
    CacheWriteError(simplifile.describe_error(err))
  })
}

fn iso_utc(at_ms: Int) -> String {
  birl.from_unix_milli(at_ms)
  |> birl.to_iso8601
  |> string.replace(".000", "")
  |> string.replace("+00:00", "Z")
}

fn live_output_dir(
  state_root: String,
  workflow_id: String,
  cache_key: String,
) -> String {
  path.join(
    path.join(path.join(state_root, "artifacts"), "review-lane-preflight"),
    workflow_id <> "-" <> string.slice(cache_key, 0, 12),
  )
}

fn cache_material(
  workflow_id: String,
  workflow_fingerprint: String,
  effective: config_types.EffectiveConfig,
  tools: List(review_lane_tools.ReviewLaneTool),
  mode: review_lane_preflight_policy.PreflightMode,
) -> CacheMaterial {
  let schemas =
    tools
    |> list.map(fn(tool) {
      SchemaDigest(
        path: tool.tool_spec.parameters_schema_path,
        sha256: tool.tool_spec.parameters_schema_sha256,
      )
    })
  let tool_names =
    tools
    |> list.map(fn(tool) { tool.tool_spec.tool_name })
    |> list.sort(by: string.compare)
  let key =
    cache_key(
      workflow_id,
      workflow_fingerprint,
      provider_name(effective),
      model_name(effective),
      string.join(tool_names, with: ","),
      schemas,
      checker_version,
      mode,
    )
  CacheMaterial(
    cache_key: key,
    workflow_id: workflow_id,
    workflow_fingerprint: workflow_fingerprint,
    provider_name: provider_name(effective),
    model_name: model_name(effective),
    tool_names: tool_names,
    schema_digests: schemas,
    mode: mode,
  )
}

pub fn cache_key(
  workflow_id: String,
  workflow_fingerprint: String,
  provider_name: String,
  model_name: String,
  tool_name: String,
  schemas: List(SchemaDigest),
  checker_version: String,
  mode: review_lane_preflight_policy.PreflightMode,
) -> String {
  let schema_material =
    schemas
    |> list.sort(by: fn(left, right) { string.compare(left.path, right.path) })
    |> list.map(fn(schema) { schema.path <> ":" <> schema.sha256 })
    |> string.join(with: ",")
  [
    workflow_id,
    workflow_fingerprint,
    provider_name,
    model_name,
    tool_name,
    schema_material,
    checker_version,
    review_lane_preflight_policy.mode_to_string(mode),
  ]
  |> string.join(with: "|")
  |> hash.sha256_hex
}

pub fn passed(cache_key: String) -> PreflightResult {
  PreflightPassed(cache_key: cache_key, warnings: [])
}

pub fn failed(
  cache_key: String,
  code: String,
  message: String,
  blocking: Bool,
) -> PreflightResult {
  PreflightFailed(
    cache_key: cache_key,
    code: code,
    message: message,
    blocking: blocking,
  )
}

pub fn blocking(result: PreflightResult) -> Bool {
  case result {
    PreflightPassed(..) -> False
    PreflightFailed(blocking: value, ..) -> value
  }
}

pub fn code(result: PreflightResult) -> String {
  case result {
    PreflightPassed(..) -> "ok"
    PreflightFailed(code: value, ..) -> value
  }
}

pub fn message(result: PreflightResult) -> String {
  case result {
    PreflightPassed(..) -> ""
    PreflightFailed(message: value, ..) -> value
  }
}

pub fn warnings(result: PreflightResult) -> List(String) {
  case result {
    PreflightPassed(warnings: value, ..) -> value
    PreflightFailed(..) -> []
  }
}

fn result_status(result: PreflightResult) -> String {
  case result {
    PreflightPassed(..) -> "passed"
    PreflightFailed(..) -> "failed"
  }
}

pub fn to_json(result: PreflightResult) -> json.Json {
  case result {
    PreflightPassed(cache_key, warnings) ->
      json.object([
        #("schema_version", json.int(1)),
        #("artifact_type", json.string("review_lane_preflight_result")),
        #("status", json.string("passed")),
        #("cache_key", json.string(cache_key)),
        #("blocking", json.bool(False)),
        #("warnings", json.array(warnings, of: json.string)),
      ])
    PreflightFailed(cache_key, code, message, blocking) ->
      json.object([
        #("schema_version", json.int(1)),
        #("artifact_type", json.string("review_lane_preflight_result")),
        #("status", json.string("failed")),
        #("cache_key", json.string(cache_key)),
        #("code", json.string(code)),
        #("message", json.string(message)),
        #("blocking", json.bool(blocking)),
      ])
  }
}

fn fallback_cache_key(
  workflow_id: String,
  workflow_fingerprint: String,
  effective: config_types.EffectiveConfig,
  mode: review_lane_preflight_policy.PreflightMode,
) -> String {
  cache_key(
    workflow_id,
    workflow_fingerprint,
    provider_name(effective),
    model_name(effective),
    "review-lane-tools",
    [],
    checker_version,
    mode,
  )
}

fn provider_name(effective: config_types.EffectiveConfig) -> String {
  case effective.pi.argv_command {
    Some(argv) -> argv.executable
    _ -> "pi-command"
  }
}

fn model_name(effective: config_types.EffectiveConfig) -> String {
  hash.sha256_hex(effective.pi.command)
}

fn live_credentials_present() -> Bool {
  control_file.get_env("ANTHROPIC_API_KEY") != None
  || control_file.get_env("OPENAI_API_KEY") != None
  || control_file.get_env("GEMINI_API_KEY") != None
  || control_file.get_env("GOOGLE_API_KEY") != None
}

fn object_field(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(json_value.JsonValue) {
  case entries {
    [] -> None
    [#(current, value), ..rest] ->
      case current == key {
        True -> Some(value)
        False -> object_field(rest, key)
      }
  }
}

fn object_string(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(String) {
  case object_field(entries, key) {
    Some(json_value.JString(value)) -> Some(value)
    _ -> None
  }
}

fn object_int(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(Int) {
  case object_field(entries, key) {
    Some(json_value.JInt(value)) -> Some(value)
    _ -> None
  }
}

fn object_bool(
  entries: List(#(String, json_value.JsonValue)),
  key: String,
) -> Option(Bool) {
  case object_field(entries, key) {
    Some(json_value.JBool(value)) -> Some(value)
    _ -> None
  }
}
