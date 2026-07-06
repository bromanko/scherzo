{
  lib,
  stdenvNoCC,
  bash,
  coreutils,
  direnv,
  git,
  gh,
  jujutsu,
  makeWrapper,
  python3,
  linearCli,
  src,
}:

let
  pythonWithJsonschema = python3.withPackages (ps: [ ps.jsonschema ]);
  runtimePath = lib.makeBinPath [
    bash
    coreutils
    direnv
    git
    gh
    jujutsu
    linearCli
    pythonWithJsonschema
  ];
in
stdenvNoCC.mkDerivation {
  pname = "scherzo-dogfood-workflows";
  version = "1";
  inherit src;

  nativeBuildInputs = [
    makeWrapper
    pythonWithJsonschema
  ];
  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out"
    cp -R workflows/dogfood/. "$out/"
    patchShebangs "$out/scripts"

    find "$out/scripts" -maxdepth 1 -type f | while read -r script; do
      wrapProgram "$script" \
        --prefix PATH : ${runtimePath} \
        --set-default PYTHONPATH "$out/scripts" \
        --set-default PYTHONDONTWRITEBYTECODE "1" \
        --set-default SCHERZO_WORKFLOW_BUNDLE_DIR "$out"
    done

    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck

    test -f "$out/implementation.yaml"
    test -f "$out/workspace-cleanup.yaml"
    test -x "$out/scripts/scherzo-review"
    test -x "$out/scripts/scherzo-execplan"
    PYTHONPATH="$out/scripts" python3 -c 'import os, sys; sys.path.insert(0, os.environ["PYTHONPATH"]); import scherzo_review'

    consumer="$TMPDIR/consumer"
    mkdir -p "$consumer/.scherzo"
    ln -s "$out" "$consumer/.scherzo/workflows"
    test -f "$consumer/.scherzo/workflows/implementation.yaml"
    test -x "$consumer/.scherzo/workflows/scripts/scherzo-review"

    runHook postInstallCheck
  '';
}
