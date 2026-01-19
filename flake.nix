{
  description = "Scherzo - AI Agent Orchestrator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Gleam and Erlang
            gleam
            erlang
            rebar3

            # Runtime dependencies
            jujutsu
            tmux

            # Development utilities
            just

            # Formatting and linting
            nixfmt-rfc-style
          ];

          shellHook = ''
            echo "Scherzo development environment"
            echo "Gleam: $(gleam --version)"
            echo "Erlang: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
            echo "jj: $(jj --version)"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "scherzo";
          version = "0.1.0";
          src = ./.;

          buildInputs = with pkgs; [
            gleam
            erlang
            rebar3
          ];

          buildPhase = ''
            gleam build
          '';

          installPhase = ''
            mkdir -p $out/bin
            # Burrito will handle actual binary creation
            cp -r build/prod/erlang-shipment/* $out/
          '';
        };
      }
    );
}
