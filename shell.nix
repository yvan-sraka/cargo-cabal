{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell { buildInputs = [ cargo rustc libiconv ]; }
