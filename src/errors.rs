use displaydoc::Display;
use thiserror::Error;

/// CLI errors displayed by `cargo-cabal` to help end-users to set up correctly
/// their Rust project!
#[derive(Display, Error, Debug)]
pub enum Error {
    /** Fail to read content of `Cargo.toml` file
     *  n.b. you have to run the command from the root folder of your Rust project
     */
    NoCargoToml,
    /// Fail to parse TOML content of `Cargo.toml` file
    WrongCargoToml,
    /** Your `Cargo.toml` file should contain a [package] section
     *  n.b. Cargo Workspace aren't currently supported by `cargo-cabal`
     */
    NotCargoPackage,
    /// Your `Cargo.toml` [package] section should contain a `name` field
    NoCargoNameField,
    /** Your `Cargo.toml` file should contain a [lib] section with a `crate-type` field
     *  that contains either `staticlib` or `cdylib` value, e.g.:
     *
     *  [lib]
     *  crate-type = ["staticlib"]
     */
    NoCargoLibTarget,
    /// Fail to write `{0}` file
    FailedToWriteFile(String),
    /** `{0}.cabal`, `hsbindgen.toml`, `Setup.hs` or `Setup.lhs` file already exist,
     * please back up it before re-running `cargo cabal init --overwrite` command
     */
    CabalFilesExist(String),
    /** `build.rs` file already exist, but `crates-type = [ "cdylib" ]` target
     *  need to generate one, please either remove this option or back up it
     *  before re-running `cargo cabal init --overwrite` command
     */
    BuildFileExist,
    /** `flake.nix` file already exist, but `--enable-nix` option need to
     * generate one, please either remove this CLI arg or back up it before
     * re-running `cargo cabal init --overwrite` command
     */
    FlakeFileExist,
}
