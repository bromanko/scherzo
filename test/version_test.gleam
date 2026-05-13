import gleam/option.{type Option, None, Some}
import scherzo/version

pub fn version_format_is_stable_and_scriptable_test() {
  let identity =
    version.SourceIdentity(
      revision: "abc123def456",
      date: "2026-05-05",
      dirty: Some(True),
    )

  assert version.format(identity)
    == "scherzo revision=abc123def456 date=2026-05-05 dirty=true"
}

pub fn version_fallback_is_deterministic_test() {
  let identity = version.detect_with(empty_env, fn(_) { Error(Nil) })

  assert identity
    == version.SourceIdentity(revision: "unknown", date: "unknown", dirty: None)
  assert version.format(identity)
    == "scherzo revision=unknown date=unknown dirty=unknown"
}

pub fn version_env_metadata_wins_without_vcs_test() {
  let identity = version.detect_with(source_env, fn(_) { Error(Nil) })

  assert identity
    == version.SourceIdentity(
      revision: "feedface1234",
      date: "2026-05-05",
      dirty: Some(False),
    )
}

pub fn version_prefers_jj_git_commit_id_when_available_test() {
  let identity = version.detect_with(empty_env, jj_source)

  assert identity
    == version.SourceIdentity(
      revision: "84f336a7e447",
      date: "2026-05-05",
      dirty: Some(False),
    )
}

pub fn version_falls_back_to_git_when_jj_is_unavailable_test() {
  let identity = version.detect_with(empty_env, git_source)

  assert identity
    == version.SourceIdentity(
      revision: "73755abd3bca",
      date: "2026-05-04",
      dirty: Some(True),
    )
}

fn empty_env(_name: String) -> Option(String) {
  None
}

fn source_env(name: String) -> Option(String) {
  case name {
    "SCHERZO_SOURCE_REVISION" -> Some("feedface1234")
    "SCHERZO_SOURCE_DATE" -> Some("2026-05-05")
    "SCHERZO_SOURCE_DIRTY" -> Some("false")
    _ -> None
  }
}

fn jj_source(query: version.VcsQuery) -> Result(String, Nil) {
  case query {
    version.JjRevision -> Ok("84f336a7e447")
    version.JjDate -> Ok("2026-05-05")
    version.JjDirty -> Ok("")
    version.GitRevision -> Ok("outer-git")
    version.GitDate -> Ok("2026-05-01")
    version.GitDirty -> Ok("M outer")
  }
}

fn git_source(query: version.VcsQuery) -> Result(String, Nil) {
  case query {
    version.JjRevision | version.JjDate | version.JjDirty -> Error(Nil)
    version.GitRevision -> Ok("73755abd3bca")
    version.GitDate -> Ok("2026-05-04")
    version.GitDirty -> Ok(" M src/scherzo.gleam")
  }
}
