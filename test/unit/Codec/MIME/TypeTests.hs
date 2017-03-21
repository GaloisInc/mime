{-# OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Codec.MIME.TypeTests where
import Test.Tasty
import Test.Tasty.HUnit
import Codec.MIME.Type

test :: TestTree
test = testGroup "Codec.MIME.Type" tests

tests :: [TestTree]
tests = 
  [ testGroup "isXmlType"
      [ testCase "examples" (do
          isXmlType nullType @?= False)
      , testCase "typical" (do
          pure ())
      , testCase "exceptions" (do
          pure ())
      ]
  ]
