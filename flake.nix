{
  description = "tree-sitter-haskell";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;

  };

  outputs = inputs:
    with inputs.flake-utils.lib;
    eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
      in
      rec {
        # nix develop
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [tree-sitter graphviz];
        };
      });
}

