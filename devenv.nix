{ pkgs, ... }:

let
  linearctlPackage = "linearctl@0.1.10";

  linearctlCommand = binName: pkgs.writeShellScriptBin binName ''
    export NODE_NO_WARNINGS=1

    if [ -z "''${LINEAR_API_KEY:-}" ] && [ -n "''${SCHERZO_AGENT_LINEAR_API_KEY:-}" ]; then
      export LINEAR_API_KEY="$SCHERZO_AGENT_LINEAR_API_KEY"
    fi

    exec ${pkgs.nodejs_22}/bin/npx --yes --package ${linearctlPackage} ${binName} "$@"
  '';

  projectScript = scriptName: ''
    exec ${pkgs.bash}/bin/bash "''${DEVENV_ROOT:-$PWD}/scripts/${scriptName}" "$@"
  '';
in
{
  packages = [
    pkgs.gleam
    pkgs.erlang
    pkgs.rebar3
    pkgs.nodejs_22
    pkgs.git
    pkgs.jq
    pkgs.selfci
    (linearctlCommand "lc")
    (linearctlCommand "linearctl")
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

  profiles."scherzo-agent".module = { pkgs, ... }: {
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
