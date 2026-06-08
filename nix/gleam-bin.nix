{ lib
, stdenvNoCC
, fetchurl
,
}:

let
  version = "1.17.0";

  releases = {
    aarch64-darwin = {
      target = "aarch64-apple-darwin";
      hash = "sha256-IOWDm5uIR146sL6mTzU7TVqmvoyRIVuPkc9Gy1HzbV4=";
    };
    aarch64-linux = {
      target = "aarch64-unknown-linux-musl";
      hash = "sha256-LdwgF/jCESv2pYvGYtyMWadYFBZa5ZMjA2hQnokO6Mo=";
    };
    x86_64-darwin = {
      target = "x86_64-apple-darwin";
      hash = "sha256-N97zjYEFbIGwKdf8DSjJHdo09Vxcpc6CVisBtRi9tm0=";
    };
    x86_64-linux = {
      target = "x86_64-unknown-linux-musl";
      hash = "sha256-wNHqraxAyIrJPqRfwVD2Nj9M64ySW1rJDzcbFmVhPMQ=";
    };
  };

  release =
    releases.${stdenvNoCC.hostPlatform.system}
      or (throw "unsupported Gleam binary platform: ${stdenvNoCC.hostPlatform.system}");
in
stdenvNoCC.mkDerivation {
  pname = "gleam";
  inherit version;

  src = fetchurl {
    url = "https://github.com/gleam-lang/gleam/releases/download/v${version}/gleam-v${version}-${release.target}.tar.gz";
    inherit (release) hash;
  };

  sourceRoot = ".";

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 gleam "$out/bin/gleam"

    runHook postInstall
  '';

  meta = {
    description = "Statically typed language for the Erlang VM";
    homepage = "https://gleam.run/";
    license = lib.licenses.asl20;
    mainProgram = "gleam";
    platforms = builtins.attrNames releases;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
}
