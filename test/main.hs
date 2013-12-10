{-# LANGUAGE CPP #-}

import Test.Hspec

import Data.UniformUtils
import Control.Monad.Trans.Convert

import qualified CMTCTest as CMTCT

main :: IO ()
main = hspec $ do
  CMTCT.specs

