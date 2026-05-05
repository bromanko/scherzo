{
  description = "Scherzo Linear/pi orchestration daemon";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      pkgsFor = system: import nixpkgs {
        inherit system;
      };

      scherzoFor = system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.callPackage ./nix/scherzo.nix {
          src = self;
        };
    in
    {
      packages = forAllSystems (system:
        let
          scherzo = scherzoFor system;
        in
        {
          default = scherzo;
          scherzo = scherzo;
        });

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
        scherzo = final.callPackage ./nix/scherzo.nix {
          src = self;
        };
      };

      formatter = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.nixpkgs-fmt);
    };
}
