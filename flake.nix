{
  description = "Scherzo Linear/pi orchestration daemon";

  # Trust Numtide's cache for shared development dependencies where available.
  nixConfig = {
    extra-substituters = [
      "https://cache.numtide.com"
    ];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };

  inputs = {
    # Linux CI depends on substituting aarch64-linux pkgs.gleam from
    # cache.nixos.org. Only bump this nixpkgs-unstable lock to revisions where
    # that output is cached; otherwise CI may compile deno/rusty-v8 and OOM
    # before Scherzo's derivation runs.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
        };

      sourceRevision = self.shortRev or (self.dirtyShortRev or "unknown");

      sourceDate =
        let
          raw = self.lastModifiedDate or "";
        in
        if builtins.stringLength raw >= 8 then
          "${builtins.substring 0 4 raw}-${builtins.substring 4 2 raw}-${builtins.substring 6 2 raw}"
        else
          "unknown";

      sourceDirty =
        if (self ? dirtyRev) || (self ? dirtyShortRev) then
          "true"
        else if (self ? rev) || (self ? shortRev) then
          "false"
        else
          "unknown";

      sourceFor =
        system:
        let
          pkgs = pkgsFor system;
          root = toString self;
        in
        pkgs.lib.cleanSourceWith {
          src = self;
          filter =
            path: type:
            let
              fullPath = toString path;
              relative = pkgs.lib.removePrefix (root + "/") fullPath;
            in
            relative != "test/tmp" && !(pkgs.lib.hasPrefix "test/tmp/" relative);
        };

      scherzoFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/scherzo.nix {
          src = sourceFor system;
          inherit sourceRevision sourceDate sourceDirty;
        };

      linearCliFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/linear-cli.nix { };

      piFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/pi.nix { };

      dogfoodWorkflowsFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/scherzo-dogfood-workflows.nix {
          src = sourceFor system;
          linearCli = linearCliFor system;
        };

      workflowPortabilityFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        import ./nix/workflow-portability.nix {
          inherit pkgs;
          repoRoot = sourceFor system;
          bundleRoot = self.packages.${system}.scherzo-dogfood-workflows;
          scherzo = self.packages.${system}.scherzo;
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          pi = piFor system;
          scherzo = scherzoFor system;
        in
        {
          default = scherzo;
          linear-cli = linearCliFor system;
          pi = pi;
          scherzo = scherzo;
          scherzo-dogfood-workflows = dogfoodWorkflowsFor system;
        }
      );

      apps = forAllSystems (system: {
        default = self.apps.${system}.scherzo;
        scherzo = {
          type = "app";
          program = "${self.packages.${system}.scherzo}/bin/scherzo";
          meta.description = "Run the Scherzo daemon or CLI";
        };
        scherzo-start = {
          type = "app";
          program = "${self.packages.${system}.scherzo}/bin/scherzo-start";
          meta.description = "Run Scherzo with graceful Ctrl-C handling";
        };
        scherzoctl = {
          type = "app";
          program = "${self.packages.${system}.scherzo}/bin/scherzoctl";
          meta.description = "Inspect and control a running Scherzo daemon";
        };
      });

      checks = forAllSystems (system: {
        default = self.packages.${system}.scherzo;
        scherzo = self.packages.${system}.scherzo;
        scherzo-dogfood-workflows = self.packages.${system}.scherzo-dogfood-workflows;
        workflow-portability = (workflowPortabilityFor system).check;
      });

      devShells = forAllSystems (system: {
        workflow-portability = (workflowPortabilityFor system).devShell;
      });

      overlays.default = final: _prev: {
        linear-cli = final.callPackage ./nix/linear-cli.nix { };
        scherzo = final.callPackage ./nix/scherzo.nix {
          src = self;
        };
      };

      formatter = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.nixpkgs-fmt
      );
    };
}
