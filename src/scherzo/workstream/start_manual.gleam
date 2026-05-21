import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/state/artifact_store as state_artifact_store
import scherzo/workflow_contract
import scherzo/workstream/artifact_store
import scherzo/workstream/start_key

pub type ArtifactInput {
  ArtifactInput(
    name: String,
    artifact_type: String,
    original_path: String,
    contract_type: Option(String),
    media_type: Option(String),
  )
}

pub type ResolvedInput {
  ResolvedInput(
    name: String,
    artifact_type: String,
    contract_type: workflow_contract.ContractType,
    ref: String,
    sha256: String,
    bytes: Int,
    media_type: String,
    original_path: String,
    source_kind: String,
  )
}

pub type Error {
  ManualError(code: String, message: String)
}

pub fn inspect_inputs(
  inputs: List(ArtifactInput),
  contract: Option(workflow_contract.Contract),
  repo_root: String,
) -> Result(List(ResolvedInput), Error) {
  inspect_inputs_loop(inputs, contract, repo_root, [])
}

pub fn snapshot_inputs(
  inputs: List(ArtifactInput),
  contract: Option(workflow_contract.Contract),
  repo_root: String,
  store: state_artifact_store.Store,
) -> Result(List(ResolvedInput), Error) {
  snapshot_inputs_loop(inputs, contract, repo_root, store, [])
}

fn inspect_inputs_loop(
  inputs: List(ArtifactInput),
  contract: Option(workflow_contract.Contract),
  repo_root: String,
  acc: List(ResolvedInput),
) -> Result(List(ResolvedInput), Error) {
  case inputs {
    [] -> Ok(list.reverse(acc))
    [manual, ..rest] -> {
      use input <- result.try(inspect_input(manual, contract, repo_root))
      inspect_inputs_loop(rest, contract, repo_root, [input, ..acc])
    }
  }
}

fn inspect_input(
  manual: ArtifactInput,
  contract: Option(workflow_contract.Contract),
  repo_root: String,
) -> Result(ResolvedInput, Error) {
  use contract_type <- result.try(contract_type_for(manual, contract))
  let media_type = media_type_for(manual)
  use snapshot <- result.try(
    artifact_store.inspect_repository_path(
      repo_root,
      manual.original_path,
      media_type,
    )
    |> result.map_error(snapshot_error("manual_artifact_snapshot_failed")),
  )
  Ok(input_from_snapshot(manual, contract_type, snapshot))
}

fn snapshot_inputs_loop(
  inputs: List(ArtifactInput),
  contract: Option(workflow_contract.Contract),
  repo_root: String,
  store: state_artifact_store.Store,
  acc: List(ResolvedInput),
) -> Result(List(ResolvedInput), Error) {
  case inputs {
    [] -> Ok(list.reverse(acc))
    [manual, ..rest] -> {
      use input <- result.try(snapshot_input(manual, contract, repo_root, store))
      snapshot_inputs_loop(rest, contract, repo_root, store, [input, ..acc])
    }
  }
}

fn snapshot_input(
  manual: ArtifactInput,
  contract: Option(workflow_contract.Contract),
  repo_root: String,
  store: state_artifact_store.Store,
) -> Result(ResolvedInput, Error) {
  use contract_type <- result.try(contract_type_for(manual, contract))
  let media_type = media_type_for(manual)
  use snapshot <- result.try(
    artifact_store.snapshot_repository_path(
      store,
      repo_root,
      manual.original_path,
      media_type,
    )
    |> result.map_error(snapshot_error("manual_artifact_snapshot_failed")),
  )
  Ok(input_from_snapshot(manual, contract_type, snapshot))
}

fn input_from_snapshot(
  manual: ArtifactInput,
  contract_type: workflow_contract.ContractType,
  snapshot: artifact_store.Snapshot,
) -> ResolvedInput {
  ResolvedInput(
    name: manual.name,
    artifact_type: manual.artifact_type,
    contract_type: contract_type,
    ref: snapshot.ref,
    sha256: snapshot.sha256,
    bytes: snapshot.bytes,
    media_type: snapshot.media_type,
    original_path: snapshot.original_path,
    source_kind: "manual",
  )
}

fn media_type_for(manual: ArtifactInput) -> String {
  case manual.media_type {
    Some(value) -> value
    None -> start_key.media_type_for_path(manual.original_path)
  }
}

fn contract_type_for(
  manual: ArtifactInput,
  contract: Option(workflow_contract.Contract),
) -> Result(workflow_contract.ContractType, Error) {
  case manual.contract_type {
    Some(contract_type) ->
      workflow_contract.type_from_string(contract_type)
      |> result.map_error(contract_type_error(manual.name))
    None ->
      case contract {
        Some(contract) ->
          case mapped_input_spec_named(contract.inputs, manual.name) {
            Ok(spec) -> Ok(spec.type_)
            Error(Nil) -> contract_type_from_name(manual.name)
          }
        None -> contract_type_from_name(manual.name)
      }
  }
}

fn mapped_input_spec_named(
  specs: List(workflow_contract.InputSpec),
  name: String,
) -> Result(workflow_contract.InputSpec, Nil) {
  specs
  |> list.filter(fn(spec) {
    spec.name == name
    && spec.source == Some(workflow_contract.MappedOutputSource)
  })
  |> list.first
}

fn contract_type_from_name(
  name: String,
) -> Result(workflow_contract.ContractType, Error) {
  workflow_contract.type_from_string(name)
  |> result.map_error(contract_type_error(name))
}

fn contract_type_error(
  name: String,
) -> fn(workflow_contract.ContractError) -> Error {
  fn(err) {
    let workflow_contract.ContractError(_, message) = err
    ManualError(
      "contract_type_unknown",
      "unknown contract type for input " <> name <> ": " <> message,
    )
  }
}

fn snapshot_error(code: String) -> fn(artifact_store.SnapshotError) -> Error {
  fn(err) { ManualError(code, start_key.describe_snapshot_error(err)) }
}
