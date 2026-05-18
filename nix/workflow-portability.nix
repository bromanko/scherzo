{
  pkgs,
  repoRoot,
  scherzo,
}:
let
  pythonWithJsonschema = pkgs.python3.withPackages (ps: [ ps.jsonschema ]);
  runtimePackages = [
    pkgs.bash
    pkgs.coreutils
    pkgs.git
    pkgs.gh
    pkgs.jq
    pkgs.jujutsu
    pkgs.python3
    pythonWithJsonschema
    scherzo
  ];
in
{
  check = pkgs.stdenvNoCC.mkDerivation {
    pname = "workflow-portability";
    version = "1";
    src = repoRoot;
    nativeBuildInputs = runtimePackages;
    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"

      output_dir="$TMPDIR/workflow-portability"
      python3 "$src/scripts/scherzo-workflow-portability" check \
        --repo-root "$src" \
        --scherzo "${scherzo}/bin/scherzo" \
        --output-dir "$output_dir"

      mkdir -p "$out"
      cp "$output_dir/workflow-portability-report.v1.json" "$out/"

      runHook postInstall
    '';
  };

  devShell = pkgs.mkShell {
    packages = runtimePackages;
    shellHook = ''
      echo "workflow portability debug shell"
      echo "Run: python3 scripts/scherzo-workflow-portability check --repo-root . --scherzo scherzo --output-dir tmp/scherzo-workflow-portability/manual"
    '';
  };
}
