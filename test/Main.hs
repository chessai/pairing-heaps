module Main (main) where

import Property qualified
import Unit qualified

main :: IO ()
main = do
  _ <- Property.tests
  Unit.tests
