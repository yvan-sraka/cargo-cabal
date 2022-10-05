{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    agg.url = "github:yvan-sraka/agg";
  };

  outputs = { self, nixpkgs, utils, agg }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = with pkgs;
          mkShell {
            buildInputs = [
              asciinema
              asciinema-scenario
              gifsicle
              agg.defaultPackage.${system}
            ];
          };
      });
}
