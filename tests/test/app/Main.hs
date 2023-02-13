module Main where

import Foreign.C.String
import Greetings

main :: IO ()
main = withCString "Rust 🦀" hello
