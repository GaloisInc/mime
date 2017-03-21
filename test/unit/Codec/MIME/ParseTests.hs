{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Codec.MIME.ParseTests where
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Codec.MIME.Type as MMT
import Codec.MIME.Factory
import Codec.MIME.Parse as MMP

test :: TestTree
test = testGroup "Codec.MIME.Parse" tests

tests :: [TestTree]
tests = 
  [ testGroup "parseMIMEMessage"
      [ testCase "examples" (do
          runParseEmail singlePartPlain @?= Just "This is the body text\r\n\r\n")
      , testCase "typical" (do
          runParseEmail' multiPartTwoPlain 
            @?= Just ("this is the body\r\n","this is an attachment\n"))
      , testCase "exceptions" (do
          pure ())
{-
      , testCase "ics" (do
          runParseEmail ""
            @?= Nothing)
-}
      ]
  ]

runParseEmail :: T.Text -> Maybe T.Text
runParseEmail m =
  let mv = MMP.parseMIMEMessage m
      hs = MMT.mime_val_headers mv  -- to, from, subject
      content@(MMT.Single cont) = MMT.mime_val_content mv
  in Just cont

runParseEmail' :: T.Text -> Maybe (T.Text, T.Text)
runParseEmail' m =
  let mv = MMP.parseMIMEMessage m
      hs = MMT.mime_val_headers mv  -- to, from, subject
      multi@(MMT.Multi mvs) = MMT.mime_val_content mv
      mv1:mv2:_ = mvs
      msg1@(MMT.Single cont) = MMT.mime_val_content mv1
      msg2@(MMT.Single cont2) = MMT.mime_val_content mv2
  in Just (cont, cont2)
