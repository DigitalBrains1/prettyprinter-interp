{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Prettyprinter.Interpolate

f :: Text
f = "world"

basicFunc :: Assertion
basicFunc = show [di|Hello #{f}!|] @?= "Hello world!"

tests :: TestTree
tests =
  testCase "Basic functionality" basicFunc

main :: IO ()
main = defaultMain tests
