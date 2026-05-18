{ inputs, pkgs, ... }:

let
  llmAgentsPackages = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system};

  linearCli = pkgs.callPackage ./nix/linear-cli.nix { };

  linearCommand =
    binName:
    pkgs.writeShellScriptBin binName ''
      export SCHERZO_LINEAR_CLI="''${SCHERZO_LINEAR_CLI:-${linearCli}/bin/linear}"
      exec ${pkgs.bash}/bin/bash ${./scripts/scherzo-linear-cli-wrapper} "$@"
    '';

  projectScript = scriptName: ''
    exec ${pkgs.bash}/bin/bash "''${DEVENV_ROOT:-$PWD}/scripts/${scriptName}" "$@"
  '';
in
{
  env.LINEAR_DEFAULT_PROJECT = "Scherzo Core";
  env.SCHERZO_GITHUB_REPO = "scherzo-systems/scherzo";
  env.SCHERZO_LINEAR_PROJECT_SLUG = "scherzo-f6f4bc92d6d7";

  packages = [
    pkgs.gleam
    pkgs.erlang
    pkgs.rebar3
    pkgs.nodejs_22
    (pkgs.python3.withPackages (ps: [ ps.jsonschema ]))
    pkgs.git
    pkgs.jq
    pkgs.selfci
    pkgs.buildkite-cli
    llmAgentsPackages.pi
    (linearCommand "linear")
    (linearCommand "lc")
  ];

  scripts.check.exec = ''
    if [ -d src ] && [ -d test ]; then
      gleam format --check src test
      gleam test
    else
      echo "src/ and test/ do not exist yet; scaffold the Gleam project first."
    fi
  '';

  scripts."scherzo-start".exec = projectScript "scherzo-start";
  scripts."scherzo-test-unit".exec = projectScript "scherzo-test-unit";
  scripts."scherzo-test-contract".exec = projectScript "scherzo-test-contract";
  scripts."scherzo-test-local-integration".exec = projectScript "scherzo-test-local-integration";
  scripts."scherzo-test-real-pi-validation".exec = projectScript "scherzo-test-real-pi-validation";

  profiles."scherzo-agent".module =
    { pkgs, ... }:
    {
      packages = [
        pkgs.gh
        pkgs.openssh
        pkgs.jujutsu
        pkgs.curl
      ];

      scripts."scherzo-agent-env-check".exec = projectScript "scherzo-agent-env-check";
      scripts."scherzo-agent-whoami".exec = projectScript "scherzo-agent-whoami";
      scripts."scherzo-agent-run".exec = projectScript "scherzo-agent-run";
    };
}
