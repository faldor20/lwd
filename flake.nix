
{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    nixpkgs.url = "nixpkgs-unstable"; # also valid: "nixpkgs"

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  # Flake outputs
  outputs = { self, nixpkgs, flake-parts, ocaml-overlay, ... }@inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
          inherit (pkgs) mkShell lib;
          # package=
          #      ocamlPackages.buildDunePackage {
          #       pname = "lwd";
          #       version = "0.1.0";
          #       duneVersion = "3";
          #       src = ./. ;
          #       buildInputs = with ocamlPackages; [

                

          #       ];

          #       strictDeps = true;
          #     };

        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ ocaml-overlay.overlays.default ];
          };
          # packages = {
          #   default = package;
          # };
          devShells = {
            default = mkShell.override { stdenv = pkgs.gccStdenv; } {
              buildInputs = with ocamlPackages; [
                dune
                utop
                ocaml
                ocamlformat
                notty
                bonsai
                ppx_jane
                ppx_pattern_bind



              ];
              inputsFrom = [ 
             # self'.packages.default
            # ocamlPackages.bonsai
               ];
              packages = builtins.attrValues {
                inherit (pkgs) gcc pkg-config;
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
              };
            };
          };

        };
    };
}
