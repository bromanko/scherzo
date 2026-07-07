import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import scherzo/doctor
import scherzo/orchestrator/run_finalize_control
import scherzo/runtime_bundle
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/workspace_run
import simplifile

pub fn maybe_append(
  results: List(doctor.CheckResult),
  selected: List(doctor.CheckName),
  bundle: runtime_bundle.RuntimeBundle,
) -> List(doctor.CheckResult) {
  case doctor.contains_check(selected, doctor.RetainedPublications) {
    False -> results
    True -> list.append(results, [check_result(bundle)])
  }
}

fn check_result(bundle: runtime_bundle.RuntimeBundle) -> doctor.CheckResult {
  case retained_unpublished_runs(bundle) {
    Error(error) -> {
      let RetainedPublicationDoctorError(message) = error
      doctor.CheckResult(
        check: doctor.RetainedPublications,
        status: doctor.Warn,
        code: "retained_publication_inspection_failed",
        message: message,
        fields: [#("workspace_root", bundle.effective.workspace.root)],
      )
    }
    Ok([]) ->
      doctor.CheckResult(
        check: doctor.RetainedPublications,
        status: doctor.Pass,
        code: "ok",
        message: "no retained materialized runs have unpublished required publications",
        fields: [#("workspace_root", bundle.effective.workspace.root)],
      )
    Ok([first, ..rest]) -> {
      let unpublished = [first, ..rest]
      let RetainedUnpublishedRun(run_id, publication_id, status) = first
      doctor.CheckResult(
        check: doctor.RetainedPublications,
        status: doctor.Warn,
        code: "retained_publications_unpublished",
        message: "retained materialized runs have unpublished required publication routes: "
          <> retained_unpublished_summary(unpublished),
        fields: list.append(
          [
            #("workspace_root", bundle.effective.workspace.root),
            #(
              "unpublished_run_count",
              int.to_string(retained_unpublished_run_count(unpublished)),
            ),
            #(
              "unpublished_route_count",
              int.to_string(list.length(unpublished)),
            ),
            #("first_run_id", run_id),
            #("first_publication_id", publication_id),
            #("first_publication_status", status),
          ],
          retained_unpublished_fields(unpublished, 1),
        ),
      )
    }
  }
}

type RetainedUnpublishedRun {
  RetainedUnpublishedRun(run_id: String, publication_id: String, status: String)
}

type RetainedPublicationDoctorError {
  RetainedPublicationDoctorError(message: String)
}

fn retained_unpublished_summary(runs: List(RetainedUnpublishedRun)) -> String {
  runs
  |> list.map(fn(run) {
    let RetainedUnpublishedRun(run_id, publication_id, status) = run
    run_id <> " " <> publication_id <> "=" <> status
  })
  |> string.join(with: ", ")
}

fn retained_unpublished_run_count(runs: List(RetainedUnpublishedRun)) -> Int {
  runs
  |> list.fold([], fn(seen, run) {
    let RetainedUnpublishedRun(run_id, _, _) = run
    case list.contains(seen, run_id) {
      True -> seen
      False -> [run_id, ..seen]
    }
  })
  |> list.length
}

fn retained_unpublished_fields(
  runs: List(RetainedUnpublishedRun),
  index: Int,
) -> List(#(String, String)) {
  case runs {
    [] -> []
    [run, ..rest] -> {
      let RetainedUnpublishedRun(run_id, publication_id, status) = run
      let suffix = int.to_string(index)
      list.append(
        [
          #("unpublished_route_" <> suffix <> "_run_id", run_id),
          #("unpublished_route_" <> suffix <> "_publication_id", publication_id),
          #("unpublished_route_" <> suffix <> "_status", status),
        ],
        retained_unpublished_fields(rest, index + 1),
      )
    }
  }
}

fn retained_unpublished_runs(
  bundle: runtime_bundle.RuntimeBundle,
) -> Result(List(RetainedUnpublishedRun), RetainedPublicationDoctorError) {
  use ledger_path <- result.try(
    ledger.path_for_workspace_root(bundle.effective.workspace.root)
    |> result.map_error(ledger_error),
  )
  use replayed <- result.try(
    ledger.replay(ledger_path)
    |> result.map_error(ledger_error),
  )
  retained_unpublished_runs_from_projection(replayed.projection, bundle)
}

fn retained_unpublished_runs_from_projection(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
) -> Result(List(RetainedUnpublishedRun), RetainedPublicationDoctorError) {
  retained_unpublished_runs_loop(
    dict.to_list(projected.workflow_runs),
    projected,
    bundle,
    [],
  )
}

fn retained_unpublished_runs_loop(
  entries: List(#(String, projection.WorkflowRunStatus)),
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  acc: List(RetainedUnpublishedRun),
) -> Result(List(RetainedUnpublishedRun), RetainedPublicationDoctorError) {
  case entries {
    [] -> Ok(acc)
    [entry, ..rest] -> {
      let #(run_id, status) = entry
      case projection.workflow_output_manifest(projected, run_id) {
        None -> retained_unpublished_runs_loop(rest, projected, bundle, acc)
        Some(_) -> {
          use unpublished <- result.try(retained_unpublished_for_status(
            projected,
            bundle,
            run_id,
            status,
          ))
          retained_unpublished_runs_loop(
            rest,
            projected,
            bundle,
            list.append(acc, unpublished),
          )
        }
      }
    }
  }
}

fn retained_unpublished_for_status(
  projected: projection.Projection,
  bundle: runtime_bundle.RuntimeBundle,
  run_id: String,
  status: projection.WorkflowRunStatus,
) -> Result(List(RetainedUnpublishedRun), RetainedPublicationDoctorError) {
  use retained <- result.try(
    retained_marker_present(workflow_status_run_root(status)),
  )
  case retained {
    False -> Ok([])
    True -> {
      use publication_statuses <- result.try(
        run_finalize_control.publication_statuses_for_bundle(
          projected,
          bundle,
          bundle.effective.workspace.root,
          run_id,
        )
        |> result.map_error(fn(error) {
          publication_status_error(run_id, error)
        }),
      )
      Ok(
        list.map(
          run_finalize_control.required_unpublished(publication_statuses),
          fn(publication_status) {
            RetainedUnpublishedRun(
              run_id,
              publication_status.publication_id,
              publication_status.status,
            )
          },
        ),
      )
    }
  }
}

fn retained_marker_present(
  run_root: String,
) -> Result(Bool, RetainedPublicationDoctorError) {
  case simplifile.is_file(workspace_run.cleanup_retention_marker(run_root)) {
    Ok(found) -> Ok(found)
    Error(error) ->
      Error(RetainedPublicationDoctorError(
        "retention marker inspection failed for "
        <> run_root
        <> ": "
        <> simplifile.describe_error(error),
      ))
  }
}

fn publication_status_error(
  run_id: String,
  error: #(String, String),
) -> RetainedPublicationDoctorError {
  let #(_, message) = error
  RetainedPublicationDoctorError(
    "publication status inspection failed for " <> run_id <> ": " <> message,
  )
}

fn ledger_error(error: ledger.LedgerError) -> RetainedPublicationDoctorError {
  RetainedPublicationDoctorError(ledger.ledger_error_to_string(error))
}

fn workflow_status_run_root(status: projection.WorkflowRunStatus) -> String {
  case status {
    projection.WorkflowRunActive(run_root: run_root, ..)
    | projection.WorkflowRunFinished(run_root: run_root, ..)
    | projection.WorkflowRunInterrupted(run_root: run_root, ..)
    | projection.WorkflowRunSuperseded(run_root: run_root, ..) -> run_root
  }
}
