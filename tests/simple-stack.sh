#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cargo rustc cabal-install ghc stack
set -euxo pipefail
pushd greetings
cargo clean
cargo add hs-bindgen --features full
../../result/bin/cargo-cabal cabal init --overwrite --enable-stack
cargo build
popd
stack clean
stack run test
