{
  lib,
  stdenvNoCC,
  gleam,
  erlang,
  rebar3,
  cacert,
  coreutils,
  python3,
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
  driverRuntimePath = lib.makeBinPath [
    python3
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
    install -m755 scripts/scherzo-workspace-noop "$out/libexec/${pname}/scherzo-workspace-noop"

    patchShebangs "$out/lib/${pname}/entrypoint.sh"
    patchShebangs "$out/libexec/${pname}/scherzo-start-runner"
    patchShebangs "$out/libexec/${pname}/scherzo-workspace-noop"
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
    makeWrapper "$out/libexec/${pname}/scherzo-workspace-noop" "$out/bin/scherzo-workspace-noop" \
      --prefix PATH : ${driverRuntimePath}

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

    PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" "$out/bin/scherzo-workspace-noop" describe --json > noop-describe
    test "$(cat noop-describe)" = '{"version":1,"capabilities":["status","changed-files","assert-only"]}'

    noop_run_root="$TMPDIR/noop-run"
    noop_workspace="$noop_run_root/workspaces/main"
    env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" lifecycle create
    test -f "$noop_workspace/.scherzo-workspace-driver-noop"

    env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" changed-files --json > noop-empty-changed
    test "$(cat noop-empty-changed)" = '{"version":1,"files":[]}'

    printf 'findings\n' > "$noop_workspace/research-findings.md"
    env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" changed-files --json > noop-changed
    test "$(cat noop-changed)" = '{"version":1,"files":[{"path":"research-findings.md","status":"modified"}]}'

    env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" assert-only --path research-findings.md

    if env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" diff --json > noop-unsupported.out 2> noop-unsupported.err; then
      echo "expected unsupported diff command to fail" >&2
      exit 1
    else
      status=$?
      test "$status" -eq 2
    fi

    env -i PATH=/path-that-does-not-exist HOME="$TMPDIR/install-check-home" \
      SCHERZO_WORKSPACE_PATH="$noop_workspace" SCHERZO_RUN_ROOT="$noop_run_root" \
      "$out/bin/scherzo-workspace-noop" lifecycle remove
    test ! -e "$noop_workspace"

    runHook postInstallCheck
  '';

  meta = {
    description = manifest.description or "Linear/pi orchestration daemon";
    homepage = "https://github.com/bromanko/scherzo";
    mainProgram = "scherzo";
    platforms = lib.platforms.unix;
  };
}
