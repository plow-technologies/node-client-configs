module Main where

import           Node.Client.ConfigsSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  configSpec
  serverCommSpec
