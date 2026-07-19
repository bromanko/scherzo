{
  lib,
  stdenv,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_22,
  makeWrapper,
  fd,
  ripgrep,
}:

buildNpmPackage rec {
  pname = "pi";
  version = "0.80.10-rpc-progress";

  src = fetchFromGitHub {
    owner = "earendil-works";
    repo = "pi";
    rev = "v0.80.10";
    hash = "sha256-Vs/ndHYzFyfN4CjPV2zMYblLXe9IuM13UrPJI1VsZEQ=";
  };

  patches = [ ./patches/pi-rpc-message-progress.patch ];

  nodejs = nodejs_22;
  npmDepsHash = "sha256-Ro2ovgqH6EpFb20M5DvcP6KIxXZPHcjeEdo1Sh4JbDM=";
  npmDepsFetcherVersion = 2;
  makeCacheWritable = true;
  npmRebuildFlags = [ "--ignore-scripts" ];

  nativeBuildInputs = [ makeWrapper ];

  # Do not run the monorepo npm build script here: the ai package's build script
  # refreshes generated model metadata from network sources. The checked-in
  # generated files are sufficient for this pinned Scherzo fork build.
  buildPhase = ''
    runHook preBuild

    (cd packages/tui && ../../node_modules/.bin/tsgo -p tsconfig.build.json)
    (cd packages/ai && ../../node_modules/.bin/tsgo -p tsconfig.build.json)
    (cd packages/agent && ../../node_modules/.bin/tsgo -p tsconfig.build.json)
    (cd packages/coding-agent && ../../node_modules/.bin/tsgo -p tsconfig.build.json && chmod +x dist/cli.js && npm run copy-assets)
    (cd packages/orchestrator && ../../node_modules/.bin/tsgo -p tsconfig.build.json && chmod +x dist/cli.js)

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    packageRoot="$out/lib/pi-source"
    mkdir -p "$packageRoot" "$out/bin"
    cp package.json package-lock.json "$packageRoot/"
    cp -PR node_modules "$packageRoot/node_modules"
    mkdir -p "$packageRoot/packages"

    for package in ai agent tui coding-agent orchestrator; do
      mkdir -p "$packageRoot/packages/$package"
      cp -R "packages/$package/dist" "$packageRoot/packages/$package/dist"
      cp "packages/$package/package.json" "$packageRoot/packages/$package/package.json"
      if [ -f "packages/$package/README.md" ]; then
        cp "packages/$package/README.md" "$packageRoot/packages/$package/README.md"
      fi
      if [ -f "packages/$package/CHANGELOG.md" ]; then
        cp "packages/$package/CHANGELOG.md" "$packageRoot/packages/$package/CHANGELOG.md"
      fi
    done

    cp -R packages/coding-agent/docs "$packageRoot/packages/coding-agent/docs"
    cp -R packages/coding-agent/examples "$packageRoot/packages/coding-agent/examples"

    makeWrapper ${nodejs_22}/bin/node "$out/bin/pi" \
      --add-flags "$packageRoot/packages/coding-agent/dist/cli.js" \
      --prefix PATH : ${
        lib.makeBinPath [
          fd
          ripgrep
        ]
      } \
      --set-default PI_PACKAGE_DIR "$packageRoot/packages/coding-agent" \
      --set PI_SKIP_VERSION_CHECK 1 \
      --set PI_TELEMETRY 0

    runHook postInstall
  '';

  dontPatchELF = stdenv.isDarwin;

  meta = {
    description = "Scherzo-pinned pi with bounded RPC message progress";
    homepage = "https://github.com/earendil-works/pi";
    license = lib.licenses.mit;
    mainProgram = "pi";
    platforms = lib.platforms.all;
    sourceProvenance = with lib.sourceTypes; [ fromSource ];
  };
}
