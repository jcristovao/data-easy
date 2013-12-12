{-# LANGUAGE CPP #-}

import Test.Hspec

import Data.Easy

import qualified DataEasyTest as DET

main :: IO ()
main = hspec $ do
  DET.specs

