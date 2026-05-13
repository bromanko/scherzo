-module(scherzo_control_command_handler_test_ffi).

-export([legacy_context/10]).

legacy_context(State, PendingClaimCount, SetPaused, ReloadWorkflow, RetryIssue, ParkIssue,
               UnparkIssue, AbortSession, RouteWorkerCommand, LogResult) ->
    {context,
     State,
     PendingClaimCount,
     SetPaused,
     ReloadWorkflow,
     RetryIssue,
     ParkIssue,
     UnparkIssue,
     AbortSession,
     RouteWorkerCommand,
     LogResult}.
