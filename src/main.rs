mod cabal;
mod cargo;
#[macro_use]
mod errors;
mod hsbindgen;

use ansi_term::Colour;
use errors::Error;
use std::{fs, path::Path};

fn main() {
    if let Err(e) = routine() {
        println!("{}{}", Colour::Red.bold().paint("Error: "), e);
    } else {
        println!(
            "✨ FIXME: write a little paragraph on what to do next / how to use `hs-bindgen`?!"
        );
    }
}

fn routine() -> Result<(), Error> {
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

    // Check that `hackage-pack` have not been already run ...
    let cabal = format!("{name}.cabal");
    (Path::new(&cabal).exists()
        || Path::new("Setup.hs").exists()
        || Path::new(".hsbindgen").exists())
    .then_some(())
    .ok_or_else(|| Error::FileAlreadyExist(name.to_owned()))?;

    // Generate wanted Cabal files from templates ...
    fs::write(cabal.clone(), cabal::generate(&name, &module, &version))
        .or(Err(Error::FailedToWriteFile(cabal)))?;
    fs::write("Setup.hs", include_str!("Setup.hs"))
        .map_err(|_| Error::FailedToWriteFile("Setup.hs".to_owned()))?;
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
