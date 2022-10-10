{
  inputs = {
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    cargo-sync-readme.url = "github:yvan-sraka/cargo-sync-readme";
  };

  outputs = { self, nixpkgs, utils, naersk, cargo-sync-readme }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk' = pkgs.callPackage naersk { };
        sync-readme = cargo-sync-readme.defaultPackage.${system};
      in {
        defaultPackage = naersk'.buildPackage ./.;
        devShell = with pkgs;
          mkShell {
            buildInputs = [
              cargo
              rust-analyzer
              rustc
              rustfmt
              rustPackages.clippy
              sync-readme
            ];
            RUST_SRC_PATH = rustPlatform.rustLibSrc;
          };
      });
}
