#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cargo rustc cabal-install ghc
set -euxo pipefail
pushd greetings
cargo clean
cargo add hs-bindgen --features full
../../result/bin/cargo-cabal cabal init --overwrite
cargo build
popd
cabal clean
cabal run test
