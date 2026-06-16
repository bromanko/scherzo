# LIV-720 legacy alias retirement inventory

This inventory is the Milestone 1 readiness checkpoint for `docs/plans/LIV-720-retire-legacy-workflow-artifact-aliases.md`. It records the future-start evidence available in the current tree, classifies the remaining legacy-alias grep surface, and states whether implementation may proceed. It is not a declaration that Milestone 1 readiness is complete.

## Readiness result

Do not start Milestone 2 yet.

The current repository proves that descriptor compatibility landed, checked-in dogfood workflows are descriptor-first, and the ExecPlan helper/tooling path understands descriptor-first bundles and manifests. The implementation handoff does not yet record the required owner dogfood-soak decision or explicit waiver, and it does not yet attach explicit owner approval evidence for the decode-only historical-readability policy. Per the plan, that is a stop condition.

## Future-start precondition status

Descriptor compatibility implementation: satisfied.
Evidence: `docs/plans/LIV-718-additive-workflow-artifact-descriptor-compatibility.md` records the additive compatibility phase as implemented; `src/scherzo/workflow_contract_manifest.gleam` and `src/scherzo/workflow_contract.gleam` contain descriptor fields and compatibility mappings.

Checked-in workflow migration to descriptor-first YAML: satisfied.
Evidence: `workflows/dogfood/execplan.yaml`, `workflows/dogfood/execplan-revision.yaml`, and `workflows/dogfood/execplan-implementation.yaml` declare `kind`, `media_type`, and `artifact_type`. `direnv exec . sh -c 'grep -RIn -- "type: *\(exec_plan_bundle\|implementation_pack\|code_change_bundle\|code_change\|artifact\[\]\)" workflows/dogfood .scherzo/workflows || true'` returned only `artifact_type: artifact[]` in `execplan.yaml`, not live legacy `type:` declarations.

Operator tooling reads descriptor-first manifests and bundles: satisfied.
Evidence: `workflows/dogfood/scripts/scherzo-execplan` validates descriptor-first `entries`, `plan`, and `implementation_pack` metadata, and `scripts/tests/test_scherzo_execplan.py` exercises retained bundle handling.

Owner dogfood-soak decision or explicit waiver: missing.
Evidence: the Scherzo-retained implementation handoff materialized for this LIV-937 run as `tmp/execplan-review-doc.md`, `tmp/execplan-implementation-pack.json`, and `tmp/execplan-bundle.json`; those retained handoff artifacts do not record that the owner has completed or waived the required soak.

Owner approval of decode-only historical readability policy: missing.
Evidence: the plan states the policy, but the handoff artifacts do not contain a separate owner-recorded approval artifact or note.

## Grep transcript

The raw workflow-local grep transcript was generated as `tmp/LIV-720-legacy-alias-inventory.grep`, but this checked document retains the reproducible command and classified results rather than relying on that temporary path.

Command used:

    direnv exec . sh -c 'grep -RIn -- "exec_plan_bundle\|implementation_pack\|code_change_bundle\|code_change\|artifact\[\]" src test workflows/dogfood scripts docs .scherzo/workflows/schemas | tee tmp/LIV-720-legacy-alias-inventory.grep'

## Classification of remaining matches

### Live runtime behavior to retire in later milestones

These are the remaining live core/helper branches that still interpret legacy semantic aliases and therefore are in-scope for later code cleanup once readiness evidence is complete.

- `src/scherzo/workflow_contract.gleam`
- `src/scherzo/workflow_contract_descriptor_compat.gleam`
- `src/scherzo/workflow_contract_manifest.gleam`
- `src/scherzo/artifact_publication_config.gleam`
- `workflows/dogfood/scripts/scherzo-execplan`

### Non-artifact false positive in live source

- `src/scherzo_signal_ffi.erl` matches `code_change` only because Erlang `gen_event` requires a `code_change/3` callback. This is not a workflow-artifact alias and is out of scope.

### Checked-in workflow and helper surfaces that are already descriptor-first but still contain legacy names as output/input names, opaque artifact-type strings, or migration-sensitive helper text

- `workflows/dogfood/execplan.yaml`
- `workflows/dogfood/execplan-revision.yaml`
- `workflows/dogfood/execplan-implementation.yaml`
- `workflows/dogfood/scripts/scherzo-implementation`
- `workflows/dogfood/prompts/execplan-draft.md`
- `workflows/dogfood/prompts/execplan-incorporate-review.md`
- `workflows/dogfood/prompts/execplan-revision.md`
- `workflows/dogfood/prompts/execplan-implementation-implement.md`
- `workflows/dogfood/prompts/execplan-implementation-review.md`
- `workflows/dogfood/prompts/execplan-implementation-apply-feedback.md`
- `workflows/dogfood/prompts/execplan-implementation-apply-plan-completion-feedback.md`
- `workflows/dogfood/prompts/execplan-implementation-apply-final-plan-completion-feedback.md`
- `workflows/dogfood/prompts/execplan-implementation-apply-late-plan-completion-feedback.md`
- `workflows/dogfood/prompts/execplan-implementation-verify-completion.md`
- `workflows/dogfood/prompts/execplan-implementation-verify-completion-after-feedback.md`
- `workflows/dogfood/prompts/execplan-implementation-verify-completion-after-final-repair.md`
- `workflows/dogfood/prompts/execplan-implementation-verify-completion-after-late-repair.md`
- `workflows/dogfood/prompts/execplan-implementation-verify-completion-before-final-validation.md`

### Schema fixtures and opaque artifact-type data that should remain readable but are not themselves live semantic branches

- `.scherzo/workflows/schemas/exec-plan-bundle.v2.schema.json`
- `.scherzo/workflows/schemas/implementation-pack.v2.schema.json`
- `.scherzo/workflows/schemas/code-change-bundle.v2.schema.json`
- `.scherzo/workflows/schemas/implementation-pack-submission.v2.schema.json`
- `.scherzo/workflows/schemas/provider/implementation-pack-submission.v2.schema.json`
- `workflows/dogfood/schemas/exec-plan-bundle.v2.schema.json`
- `workflows/dogfood/schemas/implementation-pack.v2.schema.json`
- `workflows/dogfood/schemas/code-change-bundle.v2.schema.json`
- `workflows/dogfood/schemas/implementation-pack-submission.v2.schema.json`
- `workflows/dogfood/schemas/provider/implementation-pack-submission.v2.schema.json`

### Tests and fixtures that currently prove compatibility, historical readability, or workflow naming surfaces

These are expected inventory hits today. They should shrink only as the live behavior changes and the test surface is intentionally updated.

- `test/runtime_bundle_test.gleam`
- `test/workflow_portability_test.gleam`
- `test/execplan_v2_bundle_test.gleam`
- `test/workstream_handoff_emitter_test.gleam`
- `test/workspace_driver_contract_test.gleam`
- `test/workflow_dag_test.gleam`
- `test/workflow_contract_test.gleam`
- `test/workflow_contract_manifest_test.gleam`
- `test/workstream_spec_test.gleam`
- `test/workstream_phase_metadata_test.gleam`
- `test/workstream_start_test.gleam`
- `test/workflow_dag_validator_parser_test.gleam`
- `test/workflow_fingerprint_test.gleam`
- `test/workstream_playbook_test.gleam`
- `test/workflow_checkpoint_repair_manifest_test.gleam`
- `test/execplan_implementation_helper_test.gleam`
- `test/workflow_artifact_descriptor_test.gleam`
- `test/ctl_test.gleam`
- `test/workflow_run_test.gleam`
- `test/public_yaml_schema_test.gleam`
- `test/artifact_publication_planner_test.gleam`
- `test/fixtures/workflow_artifacts/target_exec_plan_bundle_descriptor.json`
- `test/fixtures/workstream/specs/decision_invalid_unknown_kind.json`
- `test/fixtures/workstream/specs/decision_valid_approve.json`
- `test/fixtures/workstream/specs/handoff_invalid_absolute_original_path.json`
- `test/fixtures/workstream/specs/workstream_invalid_missing_artifact_type.json`
- `test/fixtures/workstream/specs/workstream_valid.json`
- `test/fixtures/workstream/specs/handoff_invalid_missing_artifact_type.json`
- `test/fixtures/workstream/specs/handoff_valid.json`
- `test/fixtures/workstream/specs/handoff_invalid_output_missing_ref.json`
- `test/fixtures/workstream/specs/workstream_invalid_missing_schema_version.json`
- `test/fixtures/workstream/specs/handoff_invalid_missing_schema_version.json`
- `test/fixtures/workstream/specs/input_bundle_invalid_missing_source_handoff_ref.json`
- `test/fixtures/workstream/specs/input_bundle_valid.json`
- `test/fixtures/workstream/specs/next_action_invalid_unknown_state.json`
- `test/fixtures/workstream/specs/next_action_valid.json`
- `test/fixtures/execplan_v2/exec-plan-bundle.valid.json`
- `test/fixtures/execplan_v2/implementation-pack.valid.json`
- `test/fixtures/execplan_v2/exec-plan-bundle.stale-pack.json`
- `test/fixtures/execplan_v2/code-change-bundle.valid.json`
- `test/fixtures/execplan_v2/exec-plan-bundle.absolute-path.json`
- `test/fixtures/execplan_v2/legacy/exec-plan-bundle.legacy.json`
- `test/fixtures/execplan_v2/legacy/implementation-pack.legacy.json`
- `test/fixtures/execplan_v2/legacy/code-change-bundle.legacy.json`
- `test/fixtures/execplan_v2/artifacts/runs/run-1/outputs/exec_plan_bundle.json`
- `test/fixtures/execplan_v2/artifacts/runs/run-1/outputs/implementation_pack.json`
- `test/fixtures/execplan_v2/artifacts/runs/run-legacy/outputs/exec_plan_bundle.json`
- `test/fixtures/execplan_v2/artifacts/runs/run-legacy/outputs/implementation_pack.json`

### Helper-test migration surface

- `scripts/tests/test_scherzo_execplan.py`

### Documentation, runbooks, and historical planning artifacts

These hits are documentation, historical plans, or spec text, not live runtime behavior.

- `docs/specs/WORKFLOW_ARTIFACT_TAXONOMY.md`
- `docs/specs/ARTIFACT_PUBLICATION_PRD.md`
- `docs/runbooks/artifact-store.md`
- `docs/runbooks/composable-workstreams.md`
- `docs/ffi.md`
- `docs/plans/LIV-241-composable-workstreams-uberplan.html`
- `docs/plans/LIV-292-workflow-contracts-v1.md`
- `docs/plans/LIV-313-design-v2-execplan-bundle-workflows.md`
- `docs/plans/LIV-405-composable-workstreams-ticket-3-handoff-artifacts.md`
- `docs/plans/LIV-495-daemon-outbound-remote-client-lifecycle.md`
- `docs/plans/LIV-496-remote-command-routing-result-streaming.md`
- `docs/plans/LIV-573-linear-alias-retirement-docs-helper-cleanup-review.md`
- `docs/plans/LIV-718-additive-workflow-artifact-descriptor-compatibility.md`
- `docs/plans/LIV-719-migrate-workflows-to-generic-artifact-taxonomy.md`
- `docs/plans/LIV-720-retire-legacy-workflow-artifact-aliases.md`
- `docs/plans/LIV-782-cleanup-inventory.md`
- `docs/plans/LIV-857-generic-custom-artifact-descriptors-binary-workflow-outputs.md`
- `docs/plans/LIV-910-execplan-publication-workspace-driver-commit-stack.md`
- `docs/plans/execplan-artifact-publication-migration.md`
- `docs/plans/LIV-96-harden-erlang-ffi-boundary-and-contracts.md`
- `docs/plans/hardening-01-graceful-daemon-lifecycle.md`

## Conclusion

The Milestone 1 inventory/classification step is complete, but Milestone 1 readiness is not complete. The repository now has a checked readiness inventory, reproducible grep command, and classified results, but the cleanup remains blocked on missing owner-recorded soak/waiver evidence and missing owner-recorded approval of the historical decode-only policy. No source cleanup should begin until those records are added to the implementation task artifacts.
