const VERSION: &str = "0.1.0";

pub(crate) fn generate(module: &str) -> String {
    format!(
        "# Since the only `.cabal` format parser implementation and specification live
# in Cabal itself ... this deadly simple config file is used by `hsbindgen`
# Rust crate to get needed data (like default exposed module name).

default = \"{module}\"

# There is an unlikely future where instead we have a Rust `.cabal` parser,
# that most likely would rely under the hood on an Haskell static lib wrapper
# of `Cabal.Parse` library. But even in this case it would be nice to know the
# `hackage-pack` version that generated the `.cabal` file used.

version = \"{VERSION}\""
    )
}
