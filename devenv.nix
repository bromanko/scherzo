{ pkgs, ... }:

{
  packages = [
    pkgs.gleam
    pkgs.erlang
    pkgs.rebar3
    pkgs.nodejs_22
    pkgs.git
    pkgs.jq
  ];

  scripts.check.exec = ''
    if [ -d src ] && [ -d test ]; then
      gleam format --check src test
      gleam test
    else
      echo "src/ and test/ do not exist yet; scaffold the Gleam project first."
    fi
  '';
}
