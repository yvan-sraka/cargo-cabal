{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    asciinema
    asciinema-scenario
    # FIXME: this `agg` dependency is OS-specific, rather use `nix develop`
    # and rewrite all this as a flake!
    darwin.apple_sdk.frameworks.Security
    gifsicle
  ];
  # FIXME: package `agg` with Nix rather tan rely on this impure hack
  shellHook = "cargo install --git https://github.com/asciinema/agg";
}
