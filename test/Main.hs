{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import qualified Codec.MIME.QuotedPrintable as QP
import qualified Data.ByteString.Char8 as S8
import           Data.Text ()
import qualified Data.Text.Encoding as T
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "QuotedPrintable"
    (it
       "Decode regression test"
       (shouldBe
          (T.decodeUtf8
             (S8.pack
                (QP.decode
                   "/home/mlitchard/projects/git/reflex-todo/.stack-work/downloaded/Vheiln5kqwE=\n\
                   \0/src/JSDOM/Custom/XMLHttpRequest.hs:39:46: error:\n\
                   \    =E2=80=A2 Could not deduce (Control.Monad.Catch.MonadThrow\n\
                   \                          Language.Javascript.JSaddle.Types.JSM)\n\
                   \        arising from a use of =E2=80=98throwM=E2=80=99\n\
                   \      from the context: MonadDOM m\n\
                   \        bound by the type signature for:\n\
                   \                   throwXHRError :: MonadDOM m =3D&gt; Maybe XHRError -&gt;=\n\
                   \ m ()\n\
                   \        at src/JSDOM/Custom/XMLHttpRequest.hs:38:1-53\n\
                   \    =E2=80=A2 In the second argument of =E2=80=98(.)=E2=80=99, namely =E2=\n\
                   \=80=98throwM=E2=80=99\n\
                   \      In the second argument of =E2=80=98maybe=E2=80=99, namely =E2=80=98(l=\n\
                   \iftDOM . throwM)=E2=80=99\n\
                   \      In the expression: maybe (return ()) (liftDOM . throwM)")))
          "/home/mlitchard/projects/git/reflex-todo/.stack-work/downloaded/Vheiln5kqwE0/src/JSDOM/Custom/XMLHttpRequest.hs:39:46: error:\n\
          \    • Could not deduce (Control.Monad.Catch.MonadThrow\n\
          \                          Language.Javascript.JSaddle.Types.JSM)\n\
          \        arising from a use of ‘throwM’\n\
          \      from the context: MonadDOM m\n\
          \        bound by the type signature for:\n\
          \                   throwXHRError :: MonadDOM m =&gt; Maybe XHRError -&gt; m ()\n\
          \        at src/JSDOM/Custom/XMLHttpRequest.hs:38:1-53\n\
          \    • In the second argument of ‘(.)’, namely ‘throwM’\n\
          \      In the second argument of ‘maybe’, namely ‘(liftDOM . throwM)’\n\
          \      In the expression: maybe (return ()) (liftDOM . throwM)"))
