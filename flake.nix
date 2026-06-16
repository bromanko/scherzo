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
    # Linux CI depends on cache.nixos.org substituting large aarch64-linux
    # dependencies from nixpkgs (for example deno/rusty-v8). Only bump this
    # nixpkgs-unstable lock to cache-safe revisions; Gleam itself is pinned
    # separately via nix/gleam-bin.nix.
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

      # Keep the nixpkgs pin on cache-safe revisions for CI; pin the Gleam
      # toolchain itself via official release binaries.
      gleamFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/gleam-bin.nix { };

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
          pi = piFor system;
          gleam = gleamFor system;
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
          gleam = gleamFor system;
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
          meta.description = "Deprecated compatibility alias for graceful Scherzo daemon startup";
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
        gleam = final.callPackage ./nix/gleam-bin.nix { };
        linear-cli = final.callPackage ./nix/linear-cli.nix { };
        pi = final.callPackage ./nix/pi.nix { };
        scherzo = final.callPackage ./nix/scherzo.nix {
          src = self;
          pi = final.pi;
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
