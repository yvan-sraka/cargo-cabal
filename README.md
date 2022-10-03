<!-- cargo-sync-readme start -->

# `hs-pack`

`hs-pack` is a tool that helps you to turn in one command a Rust crate
into a Haskell library!

To generate bindings, you need to annotate the Rust function you want to
expose with [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen) macro.

## Getting started

Here a little screencast demonstrating how it works (commands walkthrough
are just pasted below):

[![asciinema](https://i.imgur.com/eMYmps7.gif)](https://asciinema.org/a/525350)

You need in your `$PATH` a working Rust and Haskell environment, if you use
[Nix](https://nixos.org) you can just enter:
`nix-shell -p cabal-install ghc cargo rustc`

---

Welcome in this little `hackage-pack` / `hs-bindgen` demo 🙂

Let's start by creating a dumb Rust library!

```text
$ cargo new --lib greetings
     Created library `greetings` package

$ tree greetings
greetings
├── Cargo.toml
└── src
    └── lib.rs

1 directory, 2 files

$ cd greetings
```

Add `hs-bindgen` to the dependencies list:

```text
$ cargo add hs-bindgen
    Updating crates.io index
      Adding hs-bindgen v0.2.0 to dependencies.
```

And use it to decorate the function we want to expose:

* `src/lib.rs`:

```rust
use hs_bindgen::hs_bindgen;

#[hs_bindgen]
fn hello(name: &str) {
    println!("Hello, {name}!");
}
```

```text
$ cargo build --release
    Updating crates.io index
   Compiling proc-macro2 v1.0.44
   Compiling quote v1.0.21
   Compiling unicode-ident v1.0.4
   Compiling syn v1.0.101
   Compiling serde_derive v1.0.145
   Compiling serde v1.0.145
   Compiling toml v0.5.9
   Compiling hs-bindgen v0.2.0
   Compiling greetings v0.1.0 (/home/yvan/demo/greetings)
error: custom attribute panicked
 --> src/lib.rs:3:1
  |
3 | #[hs_bindgen]
  | ^^^^^^^^^^^^^
  |
  = help: message: fail to read content of `.hsbindgen` configuration file
          n.b. you have to run the command `hackage-pack` to generate it: Os { code: 2, kind: NotFound, message: "No such file or directory" }

error: could not compile `greetings` due to previous error
```

So, we will use `hackage-pack` to check our setup and generate Cabal files:

```text
$ cargo install hackage-pack
    Updating crates.io index
     Ignored package `hackage-pack v0.2.0` is already installed, use --force to override

$ hackage-pack
Error: Your `Cargo.toml` file should contain a [lib] section with a `crate-type` field
that contains `staticlib` value:

[lib]
crate-type = ["staticlib"]
```

Right, we edit the `Cargo.toml` accordingly:

* `Cargo.toml`:

```toml
[package]
name = "greetings"
version = "0.1.0"
edition = "2021"


[dependencies]
hs-bindgen = "0.2.0"

[lib]
crate-type = ["staticlib"]
```

```text
$ hackage-pack
✨ Cabal files generated!
*************************
You should now be able to compile your library with `cabal build` and should
add `hs-bindgen` to your crate dependencies list and decorate the Rust function
you want to expose with `#[hs_bindgen]` attribute macro.

$ ls
Cargo.lock  Cargo.toml  Setup.hs  greetings.cabal  src  target
```

```text
$ cabal build
Resolving dependencies...
Build profile: -w ghc-9.0.2 -O1
In order, the following will be built (use -v for more details):
 - greetings-0.1.0 (lib:greetings) (first run)
[1 of 1] Compiling Main             ( /home/yvan/demo/greetings/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/setup.hs, /home/yvan/demo/greetings/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/Main.o )
Linking /home/yvan/demo/greetings/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/setup ...
Configuring greetings-0.1.0...
******************************************************************
Call `cargo build --release` to build a dependency written in Rust
   Compiling greetings v0.1.0 (/home/yvan/demo/greetings)
    Finished release [optimized] target(s) in 0.18s
... `rustc` compilation seems to succeed 🦀! Back to Cabal build:
******************************************************************
Back to Cabal build
Preprocessing library for greetings-0.1.0..
Building library for greetings-0.1.0..
[1 of 1] Compiling Greetings        ( src/Greetings.hs, /home/yvan/demo/greetings/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/build/Greetings.o, /home/yvan/demo/greetings/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/build/Greetings.dyn_o )
```

It works! And so `cargo build` too if you just want to use the library in a
Rust project!

---

Now let's try to use our freshly generated library in an Haskell app 😉

```text
$ cd ..
$ cabal init --non-interactive test
[Log] Guessing dependencies...
[Log] Using cabal specification: 3.8
[Warning] unknown license type, you must put a copy in LICENSE yourself.
[Log] Creating fresh file CHANGELOG.md...
[Log] Creating fresh directory ./app...
[Log] Creating fresh file app/Main.hs...
[Log] Creating fresh file test.cabal...
[Warning] No synopsis given. You should edit the .cabal file and add one.
[Info] You may want to edit the .cabal file and add a Description field.

$ tree test
test
├── app
│   └── Main.hs
├── CHANGELOG.md
└── test.cabal

1 directory, 3 files
```

We create a `cabal.project` (equivalent to cargo workspace) to perform a
local test without having to upload `greetings` on hackage:

* `cabal.project`:

```cabal
packages: ./greetings ./test
```

We edit `test.cabal` to make it depends on `greetings` library:

* `test/test.cabal` (content partially omitted):

```cabal
executable test
    -- Other library packages from which modules are imported.
    build-depends:    base, greetings
```

We write a minimalist `main` function that will make call `hello` from
`Greetings` module

* `test/app/Main.hs`:

```haskell
module Main where

import Greetings

main :: IO ()
main = hello "Rust"
```

Let's check if everything works as expected:

```text
$ cabal run test
Resolving dependencies...
Build profile: -w ghc-9.0.2 -O1
In order, the following will be built (use -v for more details):
 - greetings-0.1.0 (lib:greetings) (first run)
 - test-0.1.0.0 (exe:test) (configuration changed)
[1 of 1] Compiling Main             ( /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/setup.hs, /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/Main.o )
Linking /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/setup/setup ...
Configuring greetings-0.1.0...
******************************************************************
Call `cargo build --release` to build a dependency written in Rust
    Finished release [optimized] target(s) in 0.08s
... `rustc` compilation seems to succeed 🦀! Back to Cabal build:
******************************************************************
Back to Cabal build
Preprocessing library for greetings-0.1.0..
Building library for greetings-0.1.0..
[1 of 1] Compiling Greetings        ( src/Greetings.hs, /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/build/Greetings.o, /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/greetings-0.1.0/build/Greetings.dyn_o )
Configuring executable 'test' for test-0.1.0.0..
Preprocessing executable 'test' for test-0.1.0.0..
Building executable 'test' for test-0.1.0.0..
[1 of 1] Compiling Main             ( app/Main.hs, /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/test-0.1.0.0/x/test/build/test/test-tmp/Main.o )
Linking /home/yvan/demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/test-0.1.0.0/x/test/build/test/test ...
Hello, Rust!
```

That's all folks! Happy hacking 🙂

## Acknowledgments

:warning: This is still a working experiment, not yet production ready.

`hs-pack` was heavily inspired by other interoperability initiatives, as
[`wasm-pack`](https://github.com/rustwasm/wasm-pack) and
[`Maturin`](https://github.com/PyO3/maturin).

This project was part of a work assignment as an
[IOG](https://github.com/input-output-hk) contractor.

<!-- cargo-sync-readme end -->