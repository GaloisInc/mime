--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.QuotedPrintable
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: 
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Codec.MIME.QuotedPrintable where

import Data.Char

decode :: String -> String
decode "" = ""
decode ('=':x1:x2:xs)
 | isHexDigit x1 && isHexDigit x2 =
    chr (digitToInt x1 * 16 + digitToInt x2) : decode xs
decode ('=':xs) = '=':decode xs
              -- make it explicit that we propagate other '=' occurrences.
decode (x1:xs) = x1:decode xs
