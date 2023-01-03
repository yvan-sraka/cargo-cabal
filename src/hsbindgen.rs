// FIXME: rather than living in this custom file, these options could be moved
// under an `[hs-bindgen]` manifest key directly in `Cargo.toml` (even if this
// would trigger a `unused manifest key` warning at `cargo build`)?

const VERSION: &str = "0.7.1";

/// Generate content of `hsbindgen.toml` file, a neat way to share config option
/// between `hs-bindgen` and `cargo-cabal`!
pub(crate) fn generate(module: &str) -> String {
    format!(
        "# Since the only `.cabal` format parser implementation and specification live
# in Cabal itself ... this deadly simple config file is used by `hs-bindgen`
# Rust crate to get needed data (like default exposed module name).

default = \"{module}\"

# There is an unlikely future where instead we have a Rust `.cabal` parser,
# that most likely would rely under the hood on a Haskell static lib wrapper
# of `Cabal.Parse` or https://hackage.haskell.org/package/Cabal-syntax library.
# But even in this case, it would be nice to know the `cargo-cabal` version that
# generated the `.cabal` file used.

version = \"{VERSION}\"",
    )
}
