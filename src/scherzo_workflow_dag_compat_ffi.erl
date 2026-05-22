-module(scherzo_workflow_dag_compat_ffi).
-export([normalize/1]).

normalize({workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
           MaxParallelSteps, Recover, Steps, Contract, WorkstreamPhase})
    when is_list(Steps) ->
    {workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
     MaxParallelSteps, Recover, normalize_steps(Steps), Contract, WorkstreamPhase};
normalize({workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
           MaxParallelSteps, Steps, Contract, WorkstreamPhase})
    when is_list(Steps) ->
    {workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
     MaxParallelSteps, none, normalize_steps(Steps), Contract, WorkstreamPhase};
normalize({workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
           MaxParallelSteps, Steps, Contract})
    when is_list(Steps) ->
    {workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
     MaxParallelSteps, none, normalize_steps(Steps), Contract, none};
normalize({workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
           MaxParallelSteps, Steps})
    when is_list(Steps) ->
    {workflow_dag, Id, Description, WorkspaceProfile, WorkspaceCapabilities,
     MaxParallelSteps, none, normalize_steps(Steps), none, none};
normalize(Dag) ->
    Dag.

normalize_steps(Steps) ->
    [normalize_step(Step) || Step <- Steps].

normalize_step({workflow_step, Id, Kind, DependsOn, Workspace, OnFailure,
                ModelSettings, Recover}) ->
    {workflow_step, Id, Kind, DependsOn, Workspace, OnFailure, ModelSettings,
     Recover};
normalize_step({workflow_step, Id, Kind, DependsOn, Workspace, OnFailure,
                ModelSettings}) ->
    {workflow_step, Id, Kind, DependsOn, Workspace, OnFailure, ModelSettings,
     none};
normalize_step(Step) ->
    Step.
