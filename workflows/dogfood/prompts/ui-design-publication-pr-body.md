## Visual review

Local-only visual artifacts for this run are retained in the Scherzo artifact store. Run these commands from the repository root; they use `SCHERZO_ARTIFACT_STORE`/`SCHERZO_CONFIG_DIR` when set and otherwise detect the repo-local `.scherzo/workspaces/.scherzo-state/artifacts` store.

- Preview this branch: `pnpm control:preview branch scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }} --no-open`
- List retained artifacts from the published PR: `pnpm control:visual-artifacts pr <number> --list`
- Open the retained artifact folder: `pnpm control:visual-artifacts pr <number> --open`
- Run id: `{{ run.id }}`
- Artifact bundle ref: `runs/{{ run.id }}/outputs/visual_artifacts.json`

<!-- scherzo-ui-design-artifacts-v1
run_id={{ run.id }}
visual_bundle_ref=runs/{{ run.id }}/outputs/visual_artifacts.json
branch=scherzo/{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}
-->
