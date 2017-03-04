module Main where
import Test.Tasty
import qualified Codec.MIME.TypeTests as TYP

main :: IO ()
main = 
  defaultMain 
    (testGroup "-" 
      [ TYP.test
      ])
