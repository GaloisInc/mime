module Main where
import Test.Tasty
import qualified Codec.MIME.TypeTests as TYP
import qualified Codec.MIME.ParseTests as PRS

main :: IO ()
main = 
  defaultMain 
    (testGroup "-" 
      [ TYP.test
      , PRS.test
      ])
