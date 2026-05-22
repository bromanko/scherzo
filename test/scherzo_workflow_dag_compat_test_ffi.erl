-module(scherzo_workflow_dag_compat_test_ffi).
-export([legacy_without_recover/1]).

legacy_without_recover({workflow_dag, Id, Description, WorkspaceProfile,
                        WorkspaceCapabilities, MaxParallelSteps, _Recover,
                        Steps, Contract, WorkstreamPhase}) ->
    {workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
     MaxParallelSteps, legacy_steps_without_recover(Steps), Contract,
     WorkstreamPhase}.

legacy_steps_without_recover(Steps) ->
    [legacy_step_without_recover(Step) || Step <- Steps].

legacy_step_without_recover({workflow_step, Id, Kind, DependsOn, Workspace,
                             OnFailure, ModelSettings, _Recover}) ->
    {workflow_step, Id, Kind, DependsOn, Workspace, OnFailure, ModelSettings};
legacy_step_without_recover(Step) ->
    Step.
