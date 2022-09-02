pub(crate) fn generate(name: &str, module: &str, version: &str) -> String {
    format!(
        "cabal-version:      2.4
-- The cabal-version field refers to the version of the .cabal specification,
-- and can be different from the cabal-install (the tool) version and the
-- Cabal (the library) version you are using. As such, the Cabal (the library)
-- version used must be equal or greater than the version stated in this field.
-- Starting from the specification version 2.2, the cabal-version field must be
-- the first thing in the cabal file.

-- Initial package description 'foobar' generated by
-- 'cabal init'. For further documentation, see:
--   http://haskell.org/cabal/users-guide/
--
-- The name of the package.
name:               {name}

-- The package version.
-- See the Haskell package versioning policy (PVP) for standards
-- guiding when and how versions should be incremented.
-- https://pvp.haskell.org
-- PVP summary:     +-+------- breaking API changes
--                  | | +----- non-breaking API additions
--                  | | | +--- code changes with no API change
version:            {version}

-- A short (one-line) description of the package.
-- synopsis:

-- A longer description of the package.
-- description:

-- The license under which the package is released.
-- license:

-- The package author(s).
-- author:

-- An email address to which users can send suggestions, bug reports, and
-- patches.
-- maintainer:

-- A copyright notice.
-- copyright:

-- This let us hook Cabal steps to Setup.hs script.
build-type:         Custom
custom-setup
    setup-depends:  Cabal, base, directory, process

-- Extra doc files to be distributed with the package, such as a CHANGELOG or a
-- README.
extra-doc-files:    CHANGELOG.md

-- Extra source files to be distributed with the package, such as examples, or
-- a tutorial module.
extra-source-files: Cargo.toml Cargo.lock **/*.rs
-- Warning: you should check with `cargo package --list` that there is no
-- source file left unmatched by this pattern (e.g., if you use
-- `include_str!` macro) ...

common warnings
    ghc-options: -Wall

library
    -- Import common warning flags.
    import:           warnings

    -- Modules exported by the library.
    exposed-modules:  {module}

    -- Modules included in this library but not exported.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:

    -- Other library packages from which modules are imported.
    build-depends:    base

    -- Directories containing source files.
    hs-source-dirs:   src

    -- Base language which the package is written in.
    default-language: Haskell2010

    -- Libraries that are bundled with the package.
    extra-bundled-libraries: {name}

-- This file was generated by `hackage-pack`, but feel free to edit it!

-- We would rather rely on `cabal init --non-interactive` to generate this file
-- but there is no CLI arg to set `build-type: Custom` on which it sadly
-- currently have to rely."
    )
}