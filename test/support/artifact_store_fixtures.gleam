import gleam/option.{None}
import gleam/result
import scherzo/path
import scherzo/state/artifact_store
import simplifile

pub fn artifact_root(root: String) -> String {
  root <> "/.scherzo-state/artifacts"
}

pub fn hidden_local_path_store(root: String) -> artifact_store.Store {
  let store_root = artifact_root(root)
  artifact_store.custom(
    "hidden-local-path",
    artifact_store.StoreCallbacks(
      write: fn(ref, contents) {
        let final_path = store_root <> "/" <> ref
        let parent = path.dirname(final_path) |> result.unwrap(final_path)
        use Nil <- result.try(
          simplifile.create_directory_all(parent)
          |> result.map_error(fn(error) {
            artifact_store.ArtifactIo(simplifile.describe_error(error))
          }),
        )
        artifact_store.write_atomic(final_path, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read: fn(ref) {
        simplifile.read(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            simplifile.Enoent -> artifact_store.MissingStepArtifact(ref)
            _ -> artifact_store.ArtifactIo(simplifile.describe_error(error))
          }
        })
      },
      write_immutable_bytes: fn(ref, contents) {
        artifact_store.write_immutable(store_root <> "/" <> ref, contents)
        |> result.map_error(fn(error) {
          artifact_store.ArtifactWriteFailed(error)
        })
      },
      read_bytes: fn(ref) {
        artifact_store.read_file_bytes(store_root <> "/" <> ref)
        |> result.map_error(fn(error) {
          case error {
            artifact_store.MissingStepArtifact(_) ->
              artifact_store.MissingStepArtifact(ref)
            _ -> error
          }
        })
      },
      locate: fn(ref) {
        Ok(artifact_store.ArtifactLocation(
          ref: ref,
          uri: "artifact://hidden-local-path/" <> ref,
          display_path: "artifacts://" <> ref,
          local_path: None,
        ))
      },
    ),
  )
}
