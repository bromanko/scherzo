# Scherzo

Scherzo is a Gleam service that polls Linear issues, prepares one workspace per issue, and runs a pi coding-agent session in that workspace using pi RPC mode.

This repository uses `devenv` and `direnv` for a reproducible development environment. Install Nix, `devenv`, and `direnv` on the host, then run:

    direnv allow
    direnv exec . gleam test

Run Scherzo with the default workflow path:

    direnv exec . gleam run --

or with an explicit workflow file:

    direnv exec . gleam run -- path/to/WORKFLOW.md

Runtime operation requires `LINEAR_API_KEY`, a Linear project slug in `WORKFLOW.md`, a `pi` executable that supports `pi --mode rpc`, and either `REPO_URL` for the example clone hook or another trusted workspace population or verification hook. Scherzo creates workspace directories, but the workflow owns how project code is placed there.

Deferred from the core implementation: HTTP dashboard, SSH workers, built-in Linear writes, distributed claiming, and the optional `linear_graphql` pi tool extension. Until durable claiming or Linear writes exist, run only one Scherzo instance per Linear project and workspace root.
