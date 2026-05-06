{ pkgs, ... }:

let
  linearCli = pkgs.callPackage ./nix/linear-cli.nix { };

  linearCommand =
    binName:
    pkgs.writeShellScriptBin binName ''
      if [ -z "''${LINEAR_API_KEY:-}" ] && [ -n "''${SCHERZO_AGENT_LINEAR_API_KEY:-}" ]; then
        export LINEAR_API_KEY="$SCHERZO_AGENT_LINEAR_API_KEY"
      fi

      exec ${linearCli}/bin/linear "$@"
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
