pub(crate) fn generate(name: &str) -> String {
    format!(
        "{{
  inputs = {{
    haskell-nix.url = \"github:input-output-hk/haskell.nix\";
    nixpkgs.follows = \"haskell-nix/nixpkgs-unstable\";
    utils.url = \"github:numtide/flake-utils\";
    naersk.url = \"github:nix-community/naersk/master\";
  }};

  outputs = {{ self, nixpkgs, utils, haskell-nix, naersk }}:
    utils.lib.eachDefaultSystem (system:
      let
        naersk' = pkgs.callPackage naersk {{ }};
        overlays = [
          haskell-nix.overlay
          (final: prev: {{
            # Add `extra-libraries` dependencies
            {name} = naersk'.buildPackage {{
              src = ./.;
              copyLibs = true;
            }};
            # This overlay adds our project to pkgs
            project = final.haskell-nix.project' {{
              src = ./.;
              compiler-nix-name = \"ghc924\";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {{
                cabal = \"latest\";
                hlint = \"latest\";
                haskell-language-server = \"latest\";
              }};
              # Non-Haskell shell tools go here
              shell.buildInputs = [ ];
            }};
          }})
        ];
        pkgs = import nixpkgs {{
          inherit system overlays;
          inherit (haskell-nix) config;
        }};
        flake = pkgs.project.flake {{ }};
      in flake // {{
        # Built by `nix build .`
        packages.default = flake.packages.\"{name}:lib:{name}\";
      }});
}}"
    )
}
