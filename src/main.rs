//! # `cabal-pack`
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
//! [![asciinema](extra/cabal-pack-opt.gif)](https://asciinema.org/a/525919)
//!
//! > **N.B.** You need in your `$PATH` a working Rust and Haskell environment,
//! > if you use [Nix](https://nixos.org) you can just enter:
//! > `nix-shell -p cabal-install ghc cargo rustc`
//!
//! ---
//!
//! Welcome in this little `cabal-pack` / `hs-bindgen` demo 🙂
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
//! $ cargo add hs-bindgen
//!     Updating crates.io index
//!       Adding hs-bindgen v0.5.1 to dependencies.
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
//!    Compiling proc-macro2 v1.0.46
//!    Compiling unicode-ident v1.0.4
//!    Compiling quote v1.0.21
//!    Compiling syn v1.0.101
//!    Compiling serde_derive v1.0.145
//!    Compiling serde v1.0.145
//!    Compiling semver v1.0.14
//!    Compiling toml v0.5.9
//!    Compiling hs-bindgen v0.5.1
//!    Compiling greetings v0.1.0 (/Users/yvan/demo/greetings)
//! error: custom attribute panicked
//!  --> src/lib.rs:3:1
//!   |
//! 3 | #[hs_bindgen]
//!   | ^^^^^^^^^^^^^
//!   |
//!   = help: message: fail to read content of `.hsbindgen` configuration file
//!           n.b. you have to run the command `cabal-pack` to generate it: Os { code: 2, kind: NotFound, message: "No such file or directory" }
//!
//! error: could not compile `greetings` due to previous error
//! ```
//!
//! So, we will use `cabal-pack` to check our setup and generate Cabal files:
//!
//! ```text
//! $ cargo install cabal-pack
//!     Updating crates.io index
//!      Ignored package `cabal-pack v0.5.1` is already installed, use --force to override
//!
//! $ cabal-pack
//! Error: Your `Cargo.toml` file should contain a [lib] section with a `crate-type` field
//! that contains `staticlib` value:
//!
//! [lib]
//! crate-type = ["staticlib"]
//! ```
//!
//! > **N.B.** if you're a Nix user, rather than rely on impure `cargo install`
//! > fell free to just `nix run github:yvan-sraka/cabal-pack`
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
//! hs-bindgen = "0.5.1"
//!
//! [lib]
//! crate-type = ["staticlib"]
//! ```
//!
//! ```text
//! $ cabal-pack
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
//!    Compiling hs-bindgen v0.5.1
//!    Compiling greetings v0.1.0 (/Users/yvan/demo/greetings)
//!     Finished dev [unoptimized + debuginfo] target(s) in 0.55s
//!
//! $ cabal build
//! Build profile: -w ghc-9.0.2 -O1
//! In order, the following will be built (use -v for more details):
//!  - greetings-0.1.0 (lib:greetings) (first run)
//! Preprocessing library for greetings-0.1.0..
//! Building library for greetings-0.1.0..
//! [1 of 1] Compiling Greetings        ( src/Greetings.hs, /Users/yvan/demo/greetings/dist-newstyle/build/aarch64-osx/ghc-9.0.2/greetings-0.1.0/build/Greetings.o, /Users/yvan/demo/greetings/dist-newstyle/build/aarch64-osx/ghc-9.0.2/greetings-0.1.0/build/Greetings.dyn_o )
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
//! main = hello =<< newCString "Rust"
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
//! [1 of 1] Compiling Main             ( app/Main.hs, /Users/yvan/demo/dist-newstyle/build/aarch64-osx/ghc-9.0.2/test-0.1.0.0/x/test/build/test/test-tmp/Main.o )
//! Linking /Users/yvan/demo/dist-newstyle/build/aarch64-osx/ghc-9.0.2/test-0.1.0.0/x/test/build/test/test ...
//! Hello, Rust!
//! ```
//!
//! That's all folks! Happy hacking 🙂
//!
//! ## Nix support
//!
//! The `--enable-nix` CLI arg makes `cabal-pack` generate a
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
//! `cabal-pack` was heavily inspired by other interoperability initiatives, as
//! [`wasm-pack`](https://github.com/rustwasm/wasm-pack) and
//! [`Maturin`](https://github.com/PyO3/maturin).
//!
//! This project was part of a work assignment as an
//! [IOG](https://github.com/input-output-hk) contractor.

mod cabal;
mod cargo;
#[macro_use]
mod errors;
mod flake;
mod hsbindgen;

use ansi_term::Colour;
use clap::Parser;
use errors::Error;
use std::{fs, path::Path};

/// A tool that helps you to turn in one command a Rust crate into a Haskell Cabal library
#[derive(Parser)]
#[command(version)]
struct Args {
    /// generate a haskell.nix / naersk based flake.nix
    #[arg(long)]
    enable_nix: bool,
}

fn main() {
    if let Err(e) = routine() {
        println!("{}{}", Colour::Red.bold().paint("Error: "), e);
    } else {
        println!(
            "Cabal files generated!
**********************
You should now be able to compile your library with `cabal build` and should
add `hs-bindgen` to your crate dependencies list and decorate the Rust function
you want to expose with `#[hs_bindgen]` attribute macro."
        );
    }
}

fn routine() -> Result<(), Error> {
    // Parse `cabal-pack` CLI arguments
    let args = Args::parse();

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

    // Check that project have a `crate-type = ["staticlib"]` target ...
    have_staticlib_target(root).ok_or(Error::NoCargoStaticLibTarget)?;
    // FIXME: add support to dynamic library, this would require in user crate
    // a small `build.rs` script, that append GHC version to output filename,
    // e.g.: `libNAME-ghcVERSION.so`, `libNAME-ghcVERSION.dylib` or
    // `NAME-ghcVERSION.dll`

    // Check that `cabal-pack` have not been already run ...
    let cabal = format!("{name}.cabal");
    (!(Path::new(&cabal).exists()
        || Path::new("Setup.lhs").exists()
        || Path::new(".hsbindgen").exists()))
    .then_some(())
    .ok_or_else(|| Error::FileAlreadyExist(name.to_owned()))?;

    // Generate wanted Cabal files from templates ...
    fs::write(
        cabal.clone(),
        cabal::generate(&name, &module, &version, &args),
    )
    .or(Err(Error::FailedToWriteFile(cabal)))?;
    if args.enable_nix {
        fs::write("flake.nix", flake::generate(&name))
            .map_err(|_| Error::FailedToWriteFile("flake.nix".to_owned()))?;
    } else {
        fs::write("Setup.lhs", include_str!("Setup.lhs"))
            .map_err(|_| Error::FailedToWriteFile("Setup.lhs".to_owned()))?;
    }
    fs::write(".hsbindgen", hsbindgen::generate(&module))
        .map_err(|_| Error::FailedToWriteFile(".hsbindgen".to_owned()))?;

    Ok(())
}

fn have_staticlib_target(cargo: cargo::Root) -> Option<()> {
    cargo
        .lib?
        .crate_type?
        .contains(&"staticlib".to_owned())
        .then_some(())
}
