#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cargo rustc
set -euxo pipefail
pushd greetings
cargo add hs-bindgen --features full
../../result/bin/cargo-cabal cabal init --overwrite --enable-nix
git add flake.nix
nix build
