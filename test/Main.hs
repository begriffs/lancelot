module Main where

import qualified TestBase64 as TB64
import System.Exit

main :: IO ()
main = do
  good <- and <$> sequence [TB64.runTests]
  if good
     then exitSuccess
     else exitFailure
