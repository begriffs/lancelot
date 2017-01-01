module Main where

import qualified TestBase64 as TB64

main :: IO Bool
main =
  TB64.runTests
