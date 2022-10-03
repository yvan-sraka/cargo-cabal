use displaydoc::Display;
use thiserror::Error;

#[derive(Display, Error, Debug)]
pub(crate) enum Error {
    /** Fail to read content of `Cargo.toml` file
     *  n.b. you have to run the command from the root folder of your Rust project
     */
    NoCargoToml,
    /// Fail to parse TOML content of `Cargo.toml` file
    WrongCargoToml,
    /** Your `Cargo.toml` file should contain a [package] section
     *  n.b. Cargo Workspace aren't currently supported by `cabal-pack`
     */
    NotCargoPackage,
    /// Your `Cargo.toml` [package] section should contain a `name` field
    NoCargoNameField,
    /** Your `Cargo.toml` file should contain a [lib] section with a `crate-type` field
     *  that contains `staticlib` value:
     *
     *  [lib]
     *  crate-type = ["staticlib"]
     */
    NoCargoStaticLibTarget,
    /// Fail to write `{0}` file
    FailedToWriteFile(String),
    /** `{0}.cabal`, `Setup.hs` or `.hsbindgen` file already exist,
     *  please back up it before re-running `cabal-pack` command
     */
    FileAlreadyExist(String),
}
