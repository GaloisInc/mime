module MIME.Decode where

import Data.Char
import MIME.QuotedPrintable as QP
import MIME.Base64 as Base64

decodeBody :: String -> String -> String
decodeBody enc body =
 case map toLower enc of
   "base64"           -> map (chr.fromIntegral) $ Base64.decode body
   "quoted-printable" -> QP.decode body
   _ -> body

-- Decoding of RFC 2047's "encoded-words' production
-- (as used in email-headers and some HTTP header cases
-- (AtomPub's Slug: header))
decodeWord :: String -> Maybe (String, String)
decodeWord str =
  case str of
   '=':'?':xs ->
     case dropLang $ break (\ch -> ch =='?' || ch == '*') xs of
       (cs,_:x:'?':bs)
         | isKnownCharset (map toLower cs) ->
           case toLower x of
             'q' -> decodeQ cs (break (=='?') bs)
             'b' -> decodeB cs (break (=='?') bs)
             _   -> Nothing
       _ -> Nothing
   _ -> Nothing
 where
  isKnownCharset cs = cs `elem` ["iso-8859-1", "us-ascii"]

   -- ignore RFC 2231 extension of permitting a language tag to be supplied
   -- after the charset.
  dropLang (as,'*':bs) = (as,dropWhile (/='?') bs)
  dropLang (as,bs) = (as,bs)

  decodeQ cset (fs,'?':'=':rs) = Just (fromCharset cset (QP.decode fs),rs)
  decodeQ _ _ = Nothing

  decodeB cset (fs,'?':'=':rs) =
    Just (fromCharset cset (Base64.decodeToString fs),rs)
  decodeB _ _ = Nothing

  fromCharset _cset cs = cs

decodeWords :: String -> String
decodeWords "" = ""
decodeWords (x:xs)
 | isSpace x = x : decodeWords xs
 | otherwise =
  case decodeWord (x:xs) of
    Nothing -> x : decodeWords xs
    Just (as,bs) -> as ++ decodeWords bs


