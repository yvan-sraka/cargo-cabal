#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cargo rustc cabal-install ghc
cd greetings
cargo add hs-bindgen --features full
../../result/bin/cargo-cabal cabal init
cargo build
cabal build
cd ..
cabal run test
