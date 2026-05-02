{ pkgs, ... }:

let
  gleamVersion = "1.15.4";

  gleamTargets = {
    x86_64-linux = {
      target = "x86_64-unknown-linux-musl";
      hash = "1izl47jpqr1i1f11cbydqiy150yc5h8iwygirbzmkap4a7cvr4vw";
    };
    aarch64-linux = {
      target = "aarch64-unknown-linux-musl";
      hash = "07zcy6i8ihk4zwfr0zc7i3c2snj0dbgcxnn8y23x41h9mqkcqigi";
    };
    x86_64-darwin = {
      target = "x86_64-apple-darwin";
      hash = "1914wnln0a40d73vga8avn3bp5qxllmn3fhah3kinh2gxbvzl93n";
    };
    aarch64-darwin = {
      target = "aarch64-apple-darwin";
      hash = "13r1na7i79dydqjkiv03jg04b03bcfrrp45cfp74f0bdj51kvc3l";
    };
  };

  gleamTarget = gleamTargets.${pkgs.stdenv.hostPlatform.system}
    or (throw "Unsupported Gleam binary platform: ${pkgs.stdenv.hostPlatform.system}");

  gleamBinary = pkgs.stdenvNoCC.mkDerivation {
    pname = "gleam";
    version = gleamVersion;

    src = pkgs.fetchurl {
      url = "https://github.com/gleam-lang/gleam/releases/download/v${gleamVersion}/gleam-v${gleamVersion}-${gleamTarget.target}.tar.gz";
      sha256 = gleamTarget.hash;
    };

    dontUnpack = true;

    installPhase = ''
      runHook preInstall
      tar -xzf $src gleam
      install -Dm755 gleam $out/bin/gleam
      runHook postInstall
    '';
  };
in
{
  packages = [
    gleamBinary
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
