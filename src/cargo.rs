//! This module defines data-structure into which user `Cargo.toml` is parsed

use serde::Deserialize;

#[derive(Clone, Deserialize)]
pub(crate) struct Root {
    pub(crate) package: Option<Package>,
    pub(crate) lib: Option<Lib>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct Package {
    pub(crate) name: Option<String>,
    pub(crate) version: Option<String>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct Lib {
    #[serde(alias = "crate-type")]
    pub(crate) crate_type: Option<Vec<String>>,
}

/// We allow `staticlib` and `cdylib` target only since `dylib` doesn't offer
/// the same ABI stability guarantees:
///
/// - https://users.rust-lang.org/t/what-is-the-difference-between-dylib-and-cdylib/28847
/// - https://users.rust-lang.org/t/abi-stability-guarantee-of-dylib-vs-cdylib/50879
#[derive(Debug, PartialEq)]
pub(crate) enum CrateType {
    /// `staticlib` target, which is what you want (really)
    StaticLib,
    /// `cdylib` target (overide `staticlib` target since `staticlib` require no
    /// `cargo-cabal` extra step that wouldn't require `cdylib`)
    DynLib,
}

/// From a list a targets return the one that represent the strategy used by
/// `cargo-cabal`, return `None` when there is no target usable by `cargo-cabal`
/// like `rlib` or `dylib`.
pub(crate) fn get_crate_type(cargo: Root) -> Option<CrateType> {
    let crate_type = cargo.lib?.crate_type?;
    crate_type
        .contains(&"cdylib".to_string())
        .then_some(CrateType::DynLib)
        .or_else(|| {
            crate_type
                .contains(&"staticlib".to_string())
                .then_some(CrateType::StaticLib)
        })
}
