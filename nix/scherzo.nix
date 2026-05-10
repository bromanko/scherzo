{
  lib,
  stdenvNoCC,
  gleam,
  erlang,
  rebar3,
  cacert,
  coreutils,
  makeWrapper,
  src,
  sourceRevision ? "unknown",
  sourceDate ? "unknown",
  sourceDirty ? "unknown",
}:

let
  manifest = builtins.fromTOML (builtins.readFile "${src}/gleam.toml");
  pname = manifest.name;
  version = manifest.version;
  runtimePath = lib.makeBinPath [
    erlang
    coreutils
  ];
  startRunnerPath = lib.makeBinPath [ coreutils ];

  deps = stdenvNoCC.mkDerivation {
    pname = "${pname}-gleam-deps";
    inherit version src;

    nativeBuildInputs = [
      gleam
      rebar3
      cacert
    ];

    # This fixed-output derivation lets Gleam fetch Hex packages once while the
    # main package build remains sandboxed and offline. If manifest.toml changes,
    # temporarily set this to lib.fakeSha256 and rebuild .#scherzo to get the new
    # recursive hash.
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-SZDaGZXvW5n1Er8sSaflVCmNt6B0j58GWj3soXvWOAs=";

    dontConfigure = true;

    buildPhase = ''
      runHook preBuild

      export HOME="$TMPDIR/home"
      export HEX_HOME="$TMPDIR/hex"
      export REBAR_CACHE_DIR="$TMPDIR/rebar-cache"
      export SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt"
      mkdir -p "$HOME" "$HEX_HOME" "$REBAR_CACHE_DIR"

      gleam deps download

      # Gleam writes build/packages/packages.toml from an unordered package map.
      # This derivation is fixed-output and recursively hashed, so normalize that
      # file to avoid semantically identical dependency downloads producing a
      # different Nix hash on each build.
      if [ -f build/packages/packages.toml ]; then
        {
          echo '[packages]'
          grep -v '^\[packages\]$' build/packages/packages.toml | LC_ALL=C sort
        } > build/packages/packages.toml.sorted
        mv build/packages/packages.toml.sorted build/packages/packages.toml
      fi

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

  nativeBuildInputs = [
    gleam
    erlang
    rebar3
    makeWrapper
  ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    export HOME="$TMPDIR/home"
    export HEX_HOME="$TMPDIR/hex"
    export REBAR_CACHE_DIR="$TMPDIR/rebar-cache"
    # Prevent BEAM compile_info chunks from embedding transient Nix build paths.
    export ERL_COMPILER_OPTIONS=deterministic
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
      --set-default SCHERZO_SOURCE_REVISION "${sourceRevision}" \
      --set-default SCHERZO_SOURCE_DATE "${sourceDate}" \
      --set-default SCHERZO_SOURCE_DIRTY "${sourceDirty}" \
      --prefix PATH : ${runtimePath}
    makeWrapper "$out/bin/scherzo" "$out/bin/scherzoctl" \
      --add-flags ctl
    makeWrapper "$out/libexec/${pname}/scherzo-start-runner" "$out/bin/scherzo-start" \
      --add-flags -- \
      --add-flags "$out/bin/scherzo" \
      --prefix PATH : ${startRunnerPath}

    runHook postInstall
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    runHook preInstallCheck

    mkdir -p "$TMPDIR/install-check-home"

    PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" "$out/bin/scherzo" --help > scherzo-help
    grep -q "Usage: scherzo" scherzo-help

    PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" "$out/bin/scherzo" --version > scherzo-version
    grep -q "^scherzo revision=" scherzo-version
    grep -q " date=" scherzo-version
    grep -q " dirty=" scherzo-version

    PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" "$out/bin/scherzo-start" --help > scherzo-start-help
    grep -q "Usage: scherzo" scherzo-start-help

    PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" "$out/bin/scherzoctl" --help > scherzoctl-help
    grep -q "Usage: scherzo ctl" scherzoctl-help

    runHook postInstallCheck
  '';

  meta = {
    description = manifest.description or "Linear/pi orchestration daemon";
    homepage = "https://github.com/bromanko/scherzo";
    mainProgram = "scherzo";
    platforms = lib.platforms.unix;
  };
}
