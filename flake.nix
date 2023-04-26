{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        erlpkgs = pkgs.beam.packages.erlangR23;
      in
      rec {
        devShell = pkgs.mkShell {
          buildInputs = with erlpkgs; [
            rebar3
            erlang
            erlang-ls
          ];
        };
      }
    );
}
