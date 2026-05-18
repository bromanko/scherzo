{
  description = "Scherzo Linear/pi orchestration daemon";

  # The devenv shell includes pi from numtide/llm-agents.nix. Trust Numtide's
  # cache so CI can substitute pi instead of source-building its npm package.
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

      scherzoFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/scherzo.nix {
          src = self;
          inherit sourceRevision sourceDate sourceDirty;
        };

      linearCliFor =
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/linear-cli.nix { };
    in
    {
      packages = forAllSystems (
        system:
        let
          scherzo = scherzoFor system;
        in
        {
          default = scherzo;
          linear-cli = linearCliFor system;
          scherzo = scherzo;
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
