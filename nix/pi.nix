{ lib
, stdenv
, buildNpmPackage
, fetchFromGitHub
, nodejs_22
, makeWrapper
, fd
, ripgrep
,
}:

buildNpmPackage rec {
  pname = "pi";
  version = "0.75.4-0759f68d";

  src = fetchFromGitHub {
    owner = "scherzo-systems";
    repo = "pi";
    rev = "0759f68d53627a0f32590696f58f39a7b2c9016f";
    hash = "sha256-FZrR4Zu/gbyEDUwXR2ppdsuZL1fHWyZQmh+0cbqzAA4=";
  };

  nodejs = nodejs_22;
  npmDepsHash = "sha256-E+8ygf0gQgoYXC4cLq45j2zCDQ5Bj1RIvauGIN19H7M=";
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

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    packageRoot="$out/lib/pi-source"
    mkdir -p "$packageRoot" "$out/bin"
    cp package.json package-lock.json "$packageRoot/"
    cp -PR node_modules "$packageRoot/node_modules"
    mkdir -p "$packageRoot/packages"

    for package in ai agent tui coding-agent; do
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
    description = "Scherzo-pinned pi fork with RPC message update suppression";
    homepage = "https://github.com/scherzo-systems/pi";
    license = lib.licenses.mit;
    mainProgram = "pi";
    platforms = lib.platforms.all;
    sourceProvenance = with lib.sourceTypes; [ fromSource ];
  };
}
