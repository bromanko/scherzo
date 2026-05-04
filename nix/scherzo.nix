{ lib
, stdenvNoCC
, gleam
, erlang
, rebar3
, cacert
, makeWrapper
, src
}:

let
  manifest = builtins.fromTOML (builtins.readFile "${src}/gleam.toml");
  pname = manifest.name;
  version = manifest.version;

  deps = stdenvNoCC.mkDerivation {
    pname = "${pname}-gleam-deps";
    inherit version src;

    nativeBuildInputs = [ gleam rebar3 cacert ];

    # This fixed-output derivation lets Gleam fetch Hex packages once while the
    # main package build remains sandboxed and offline. If manifest.toml changes,
    # temporarily set this to lib.fakeSha256 and rebuild .#scherzo to get the new
    # recursive hash.
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-bo5IqZZ2dJI124LknMz4MsG8/TQ5T99vnF0YB01BCpY=";

    dontConfigure = true;

    buildPhase = ''
      runHook preBuild

      export HOME="$TMPDIR/home"
      export HEX_HOME="$TMPDIR/hex"
      export REBAR_CACHE_DIR="$TMPDIR/rebar-cache"
      export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
      mkdir -p "$HOME" "$HEX_HOME" "$REBAR_CACHE_DIR"

      gleam deps download

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p "$out"
      cp -R build/packages "$out/packages"
      cp build/gleam-*.lock "$out"/ 2>/dev/null || true

      runHook postInstall
    '';
  };
in
stdenvNoCC.mkDerivation {
  inherit pname version src;

  nativeBuildInputs = [ gleam erlang rebar3 makeWrapper ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    export HOME="$TMPDIR/home"
    export HEX_HOME="$TMPDIR/hex"
    export REBAR_CACHE_DIR="$TMPDIR/rebar-cache"
    mkdir -p "$HOME" "$HEX_HOME" "$REBAR_CACHE_DIR"

    rm -rf build
    mkdir -p build
    cp -R ${deps}/packages build/packages
    cp ${deps}/gleam-*.lock build/ 2>/dev/null || true
    chmod -R u+w build

    gleam export erlang-shipment

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/${pname}" "$out/libexec/${pname}" "$out/bin"
    cp -R build/erlang-shipment/. "$out/lib/${pname}/"
    cp scripts/scherzo-start-runner "$out/libexec/${pname}/scherzo-start-runner"

    patchShebangs "$out/lib/${pname}/entrypoint.sh"
    patchShebangs "$out/libexec/${pname}/scherzo-start-runner"
    makeWrapper "$out/lib/${pname}/entrypoint.sh" "$out/bin/scherzo" \
      --add-flags run \
      --prefix PATH : ${lib.makeBinPath [ erlang ]}
    makeWrapper "$out/bin/scherzo" "$out/bin/scherzoctl" \
      --add-flags ctl

    cat > "$out/bin/scherzo-start" <<EOF
#!/usr/bin/env bash
exec "$out/libexec/${pname}/scherzo-start-runner" -- "$out/bin/scherzo" "\$@"
EOF
    chmod +x "$out/bin/scherzo-start"
    patchShebangs "$out/bin/scherzo-start"

    runHook postInstall
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    runHook preInstallCheck

    "$out/bin/scherzo" --help >/dev/null
    "$out/bin/scherzo-start" --help >/dev/null
    "$out/bin/scherzoctl" --help >/dev/null

    runHook postInstallCheck
  '';

  meta = {
    description = manifest.description or "Linear/pi orchestration daemon";
    homepage = "https://github.com/bromanko/scherzo";
    mainProgram = "scherzo";
    platforms = lib.platforms.unix;
  };
}
