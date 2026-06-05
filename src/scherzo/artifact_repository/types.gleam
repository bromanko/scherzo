import gleam/option.{type Option}
import scherzo/artifact_publication_planner

pub type SelectedArtifactBytes {
  SelectedArtifactBytes(
    file: artifact_publication_planner.PlannedPublicationFile,
    bytes: BitArray,
  )
}

pub type SelectedCommitStackBytes {
  SelectedCommitStackBytes(
    source: artifact_publication_planner.SelectedArtifact,
    bytes: BitArray,
  )
}

pub type PublicationExecutionInput {
  PublicationExecutionInput(
    manifest: artifact_publication_planner.DryRunPublicationManifest,
    selected_files: List(SelectedArtifactBytes),
    commit_stack: Option(SelectedCommitStackBytes),
  )
}

pub type GithubPullRequestMatch {
  GithubPullRequestMatch(number: Int, url: String, is_draft: Bool)
}

pub type LatestPublicationDetails {
  LatestPublicationDetails(
    status: String,
    version_id: Option(String),
    branch: Option(String),
    commit_sha: Option(String),
    pr_url: Option(String),
    selected_paths: List(String),
  )
}
