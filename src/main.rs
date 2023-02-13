//! # `cargo-cabal`
//!
//! A tool that helps you to turn in one command a Rust crate into a Haskell
//! Cabal library!
//!
//! To generate bindings, you need to annotate the Rust function you want to
//! expose with [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen) macro.
//!
//! ## Getting started
//!
//! Here a little screencast demonstrating how it works (commands walkthrough
//! are just pasted below):
//!
//! ![asciinema](extra/cargo-cabal-opt.gif)
//!
//! > **N.B.** You need in your `$PATH` a working Rust and Haskell environment,
//! > if you use [Nix](https://nixos.org) you can just enter:
//! > `nix-shell -p cabal-install ghc cargo rustc`
//!
//! ---
//!
//! Welcome in this little `cargo-cabal` / `hs-bindgen` demo 🙂
//!
//! Let's start by creating a dumb Rust library!
//!
//! ```text
//! $ cargo new --lib greetings
//!      Created library `greetings` package
//!
//! $ tree greetings
//! greetings
//! ├── Cargo.toml
//! └── src
//!     └── lib.rs
//!
//! 1 directory, 2 files
//!
//! $ cd greetings
//! ```
//!
//! Add `hs-bindgen` to the dependencies list:
//!
//! ```text
//! $ cargo add hs-bindgen --features full
//!     Updating crates.io index
//!       Adding hs-bindgen v0.8.0 to dependencies.
//!              Features:
//!              + antlion
//!              + full
//!              + std
//! ```
//!
//! And use it to decorate the function we want to expose:
//!
//! * `src/lib.rs`:
//!
//! ```rust
//! use hs_bindgen::*;
//!
//! #[hs_bindgen]
//! fn hello(name: &str) {
//!     println!("Hello, {name}!");
//! }
//! ```
//!
//! ```text
//! $ cargo build
//!    Compiling proc-macro2 v1.0.47
//!    Compiling quote v1.0.21
//!    Compiling unicode-ident v1.0.5
//!    Compiling syn v1.0.105
//!    Compiling serde_derive v1.0.149
//!    Compiling semver-parser v0.7.0
//!    Compiling serde v1.0.149
//!    Compiling thiserror v1.0.37
//!    Compiling antlion v0.3.1
//!    Compiling semver v0.9.0
//!    Compiling semver v1.0.14
//!    Compiling lazy_static v1.4.0
//!    Compiling hs-bindgen-traits v0.8.0
//!    Compiling rustc_version v0.2.3
//!    Compiling hs-bindgen-attribute v0.7.2
//!    Compiling thiserror-impl v1.0.37
//!    Compiling displaydoc v0.2.3
//!    Compiling hs-bindgen-types v0.8.0
//!    Compiling toml v0.5.9
//!    Compiling hs-bindgen v0.8.0
//!    Compiling greetings v0.1.0 (/Users/yvan/demo/greetings)
//! error: custom attribute panicked
//!  --> src/lib.rs:3:1
//!   |
//! 3 | #[hs_bindgen]
//!   | ^^^^^^^^^^^^^
//!   |
//!   = help: message: fail to read content of `hsbindgen.toml` configuration file
//!           n.b. you have to run the command `cargo-cabal` to generate it: Os { code: 2, kind: NotFound, message: "No such file or directory" }
//!
//! error: could not compile `greetings` due to previous error
//! ```
//!
//! So, we will use `cargo-cabal` to check our setup and generate Cabal files:
//!
//! ```text
//! $ cargo install cargo-cabal
//!     Updating crates.io index
//!      Ignored package `cargo-cabal v0.7.0` is already installed, use --force to override
//!
//! $ cargo cabal init
//! Error: Your `Cargo.toml` file should contain a [lib] section with a `crate-type` field
//! that contains either `staticlib` or `cdylib` value, e.g.:
//!
//! [lib]
//! crate-type = ["staticlib"]
//! ```
//!
//! > **N.B.** if you're a Nix user, rather than rely on impure `cargo install`,
//! > feel free to just `nix run github:yvan-sraka/cargo-cabal -- cabal init`
//!
//! Right, we edit the `Cargo.toml` accordingly:
//!
//! * `Cargo.toml`:
//!
//! ```toml
//! [package]
//! name = "greetings"
//! version = "0.1.0"
//! edition = "2021"
//!
//! # See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html
//!
//! [dependencies]
//! hs-bindgen = { version = "0.8.0", features = ["full"] }
//!
//! [lib]
//! crate-type = ["staticlib"]
//! ```
//!
//! ```text
//! $ cargo cabal init
//! Cabal files generated!
//! **********************
//! You should now be able to compile your library with `cabal build` and should
//! add `hs-bindgen` to your crate dependencies list and decorate the Rust function
//! you want to expose with `#[hs_bindgen]` attribute macro.
//!
//! $ ls
//! Cargo.lock  Cargo.toml  Setup.lhs  greetings.cabal  src  target
//! ```
//!
//! ```text
//! $ cargo build
//!    Compiling greetings v0.1.0 (/Users/yvan/demo/greetings)
//!     Finished dev [unoptimized + debuginfo] target(s) in 1.06s
//!
//! $ cabal build
//! Build profile: -w ghc-9.0.2 -O1
//! In order, the following will be built (use -v for more details):
//!  - greetings-0.1.0 (lib:greetings) (first run)
//! [1 of 1] Compiling Main             ( omitted ... )
//! Linking /Users/yvan/demo/dist-newstyle/build/aarch64-osx/ghc-9.0.2/greetings-0.1.0/setup/setup ...
//! Configuring greetings-0.1.0...
//! Preprocessing library for greetings-0.1.0..
//! Building library for greetings-0.1.0..
//! [1 of 1] Compiling Greetings        ( src/Greetings.hs, omitted ... )
//! ```
//!
//! It works! And so `cargo build` too if you just want to use the library in a
//! Rust project!
//!
//! ---
//!
//! Now let's try to use our freshly generated library in an Haskell app 😉
//!
//! ```text
//! $ cd ..
//! $ cabal init --non-interactive test
//! [Log] Guessing dependencies...
//! [Log] Using cabal specification: 3.8
//! [Warning] unknown license type, you must put a copy in LICENSE yourself.
//! [Log] Creating fresh file CHANGELOG.md...
//! [Log] Creating fresh directory ./app...
//! [Log] Creating fresh file app/Main.hs...
//! [Log] Creating fresh file test.cabal...
//! [Warning] No synopsis given. You should edit the .cabal file and add one.
//! [Info] You may want to edit the .cabal file and add a Description field.
//!
//! $ tree test
//! test
//! ├── app
//! │   └── Main.hs
//! ├── CHANGELOG.md
//! └── test.cabal
//!
//! 1 directory, 3 files
//! ```
//!
//! We create a `cabal.project` (equivalent to cargo workspace) to perform a
//! local test without having to upload `greetings` on hackage:
//!
//! * `cabal.project`:
//!
//! ```cabal
//! packages: ./greetings ./test
//! ```
//!
//! We edit `test.cabal` to make it depends on `greetings` library:
//!
//! * `test/test.cabal` (content partially omitted):
//!
//! ```cabal
//! executable test
//!     -- Other library packages from which modules are imported.
//!     build-depends:    base, greetings
//! ```
//!
//! We write a minimalist `main` function that will make call `hello` from
//! `Greetings` module
//!
//! * `test/app/Main.hs`:
//!
//! ```haskell
//! module Main where
//!
//! import Foreign.C.String
//! import Greetings
//!
//! main :: IO ()
//! main = withCString "Rust 🦀" hello
//! ```
//!
//! Let's check if everything works as expected:
//!
//! ```text
//! $ cabal run test
//! Build profile: -w ghc-9.0.2 -O1
//! In order, the following will be built (use -v for more details):
//!  - test-0.1.0.0 (exe:test) (first run)
//! Configuring executable 'test' for test-0.1.0.0..
//! Preprocessing executable 'test' for test-0.1.0.0..
//! Building executable 'test' for test-0.1.0.0..
//! [1 of 1] Compiling Main             ( app/Main.hs, omitted ... )
//! Linking /Users/yvan/demo/dist-newstyle/build/aarch64-osx/ghc-9.0.2/test-0.1.0.0/x/test/build/test/test ...
//! Hello, Rust 🦀!
//! ```
//!
//! That's all folks! Happy hacking 🙂
//!
//! ## Nix support
//!
//! The `--enable-nix` CLI arg makes `cargo-cabal` generate a
//! [haskell.nix](https://github.com/input-output-hk/haskell.nix) /
//! [naersk](https://github.com/nix-community/naersk) based `flake.nix` rather
//! than the `Setup.lhs`.
//!
//! > **N.B.** when first working with `hs-bindgen` and Nix flakes, checking if
//! > `Cargo.lock` isn't in `.gitignore` and running `cargo build` and
//! > `git add --all` before `nix build`, will save you a lot of pain 😉
//!
//! ## Acknowledgments
//!
//! ⚠️ This is still a working experiment, not yet production ready.
//!
//! `cargo-cabal` was heavily inspired by other interoperability initiatives, as
//! [`wasm-pack`](https://github.com/rustwasm/wasm-pack) and
//! [`Maturin`](https://github.com/PyO3/maturin).
//!
//! This project was part of a work assignment as an
//! [IOG](https://github.com/input-output-hk) contractor.
//!
//! ## License
//!
//! Licensed under either of [Apache License](LICENSE-APACHE), Version 2.0 or
//! [MIT license](LICENSE-MIT) at your option.
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in this project by you, as defined in the Apache-2.0 license,
//! shall be dual licensed as above, without any additional terms or conditions.

#![forbid(unsafe_code)]

mod cabal;
mod cargo;
#[macro_use]
mod errors;
mod flake;
mod hsbindgen;

use crate::cargo::{get_crate_type, CrateType};
use ansi_term::Colour;
use clap::{arg, Parser, Subcommand};
use errors::Error;
use std::{fs, path::Path};

/// A tool that helps you to turn in one command a Rust crate into a Haskell Cabal library
#[derive(Parser)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    cabal: Wrapper,
}

#[derive(Subcommand)]
enum Wrapper {
    #[command(subcommand)]
    Cabal(Commands),
}

#[derive(Subcommand)]
enum Commands {
    /// Initilize the ploject by generating custom Cabal files
    Init {
        /// Generate a haskell.nix / naersk based flake.nix
        #[arg(long)]
        enable_nix: bool,
        /// Run a clean before generating files
        #[arg(long)]
        overwrite: bool,
    },
    /// Remove files generated by cargo-cabal, except flake.nix
    Clean,
}

fn main() {
    if let Err(e) = routine() {
        println!("{}{}", Colour::Red.bold().paint("Error: "), e);
    }
}

fn routine() -> Result<(), Error> {
    // Parse `cargo-cabal` CLI arguments
    match Args::parse().cabal {
        Wrapper::Cabal(command) => {
            // Parse Cargo.toml file content ...
            let cargo = fs::read_to_string("Cargo.toml").or(Err(Error::NoCargoToml))?;
            let root: cargo::Root = toml::from_str(&cargo).or(Err(Error::WrongCargoToml))?;
            let package = root.clone().package.ok_or(Error::NotCargoPackage)?;
            let version = package.version.unwrap_or_else(|| "0.1.0.0".to_owned());
            let name = package.name.ok_or(Error::NoCargoNameField)?;
            let module = name
                .split(&['-', '_'])
                .map(|s| format!("{}{}", &s[..1].to_uppercase(), &s[1..]))
                .collect::<Vec<String>>()
                .join("");

            match command {
                Commands::Init {
                    enable_nix,
                    overwrite,
                } => init(root, &version, &name, &module, enable_nix, overwrite),
                Commands::Clean => clean(&name),
            }
        }
    }
}

fn init(
    root: cargo::Root,
    version: &str,
    name: &str,
    module: &str,
    enable_nix: bool,
    overwrite: bool,
) -> Result<(), Error> {
    // `cargo cabal init --overwrite` == `cargo cabal clean && cargo cabal init`
    if overwrite {
        clean(name)?;
    }

    // Check that project have a `crate-type` target ...
    let crate_type = get_crate_type(root).ok_or(Error::NoCargoLibTarget)?;

    // Check that `cargo cabal init` have not been already run ...
    let cabal = format!("{name}.cabal");
    (!(Path::new(&cabal).exists()
        || Path::new(".hsbindgen").exists()
        || Path::new("hsbindgen.toml").exists()
        || Path::new("Setup.hs").exists()
        || Path::new("Setup.lhs").exists()))
    .then_some(())
    .ok_or_else(|| Error::CabalFilesExist(name.to_owned()))?;
    // ... and that no existing file would conflict ...
    if crate_type == CrateType::DynLib {
        (!Path::new("build.rs").exists())
            .then_some(())
            .ok_or(Error::BuildFileExist)?;
    }
    if enable_nix {
        (!Path::new("flake.rs").exists())
            .then_some(())
            .ok_or(Error::FlakeFileExist)?;
    }

    // Generate wanted files from templates ... starting by a `.cabal` ...
    fs::write(
        cabal.clone(),
        cabal::generate(name, module, version, enable_nix),
    )
    .or(Err(Error::FailedToWriteFile(cabal)))?;

    // `hsbindgen.toml` is a config file readed by `#[hs_bindgen]` proc macro ...
    fs::write("hsbindgen.toml", hsbindgen::generate(module))
        .map_err(|_| Error::FailedToWriteFile("hsbindgen.toml".to_owned()))?;

    // If `crate-type = [ "cdylib" ]` then a custom `build.rs` is needed ...
    if crate_type == CrateType::DynLib {
        fs::write("build.rs", include_str!("build.rs"))
            .map_err(|_| Error::FailedToWriteFile("build.rs".to_owned()))?;
    }

    // `--enable-nix` CLI option generate a `flake.nix` rather than a `Setup.lhs`
    if enable_nix {
        fs::write("flake.nix", flake::generate(name))
            .map_err(|_| Error::FailedToWriteFile("flake.nix".to_owned()))?;
    } else {
        fs::write("Setup.lhs", include_str!("Setup.lhs"))
            .map_err(|_| Error::FailedToWriteFile("Setup.lhs".to_owned()))?;
    }

    println!(
        "\
Cabal files generated!
**********************
You should now be able to compile your library with `cabal build` and should
add `hs-bindgen` to your crate dependencies list and decorate the Rust function
you want to expose with `#[hs_bindgen]` attribute macro."
    );

    Ok(())
}

fn clean(name: &str) -> Result<(), Error> {
    let _ = fs::remove_file(format!("{name}.cabal"));
    let _ = fs::remove_file(".hsbindgen");
    let _ = fs::remove_file("hsbindgen.toml");
    let _ = fs::remove_file("Setup.hs");
    let _ = fs::remove_file("Setup.lhs");
    Ok(())
}
