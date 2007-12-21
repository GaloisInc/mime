{- |
  Module      :  MIME.Utils
  Copyright   :  (c) 2007

  Maintainer      : tse-dev-team@galois.com
  Stability       : unstable
  Portability     : GHC

  Extracting content from MIME values and types.
-}
module MIME.Utils
  ( findMultipartNamed -- :: String -> MIMEValue -> Maybe MIMEValue
  )  where

import MIME.Type
import Data.List ( find )
import Control.Monad ( msum )

-- | Given a parameter name, locate it within a MIME value,
-- returning the corresponding (sub) MIME value.
findMultipartNamed :: String -> MIMEValue -> Maybe MIMEValue
findMultipartNamed nm mv =
 case mime_val_content mv of
   Multi ms  -> msum (map (findMultipartNamed nm) ms)
   Single {} -> do cd <- mime_val_disp mv
                   find (withDispName nm) (dispParams cd)
                   return mv
 where withDispName a (Name b) = a == b
       withDispName _ _ = False

