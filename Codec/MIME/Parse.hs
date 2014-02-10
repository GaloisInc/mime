{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Pare
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability : provisional
-- Portability: portable
--
-- Parsing MIME content.
-- 
--------------------------------------------------------------------
module Codec.MIME.Parse
  ( parseMIMEBody    -- :: [(T.Text,T.Text)] -> T.Text -> MIMEValue
  , parseMIMEType    -- :: T.Text -> Maybe Type
  , parseMIMEMessage -- :: T.Text -> MIMEValue

  , parseHeaders     -- :: T.Text -> ([(T.Text,T.Text)], T.Text)
  , parseMultipart   -- :: Type -> T.Text -> (MIMEValue, T.Text)
  , parseContentType -- :: T.Text -> Maybe Type
  , splitMulti       -- :: T.Text -> T.Text -> ([MIMEValue], T.Text)
  , normalizeCRLF
  ) where

import Codec.MIME.Type
import Codec.MIME.Decode
import Control.Arrow(second)

import Data.Char
import Data.Maybe
import qualified Data.List as L
import Debug.Trace ( trace )
import qualified Data.Text as T
import Data.Monoid(Monoid(..), (<>))

enableTrace :: Bool
enableTrace = False

doTrace :: String -> b -> b
doTrace | enableTrace = trace
        | otherwise   = \_ x -> x


parseMIMEBody :: [MIMEParam] -> T.Text -> MIMEValue
parseMIMEBody headers_in body = result { mime_val_headers = headers }
  where
  result = case mimeType mty of
    Multipart{} -> fst (parseMultipart mty body)
    Message{}   -> fst (parseMultipart mty body)
    _           -> nullMIMEValue { mime_val_type    = mty
                                 , mime_val_disp    = parseContentDisp headers
                                 , mime_val_content = Single (processBody headers body)
                                 }
  headers = [ MIMEParam (T.toLower k) v | (MIMEParam k v) <- headers_in ]
  mty = fromMaybe defaultType
                       (parseContentType =<< lookupField "content-type" (paramPairs headers))
defaultType :: Type
defaultType = Type { mimeType   = Text "plain"
                   , mimeParams = [MIMEParam "charset" "us-ascii"]
                   }

parseContentDisp :: [MIMEParam] -> Maybe Disposition
parseContentDisp headers =
    (processDisp . dropFoldingWSP) =<< lookupField "content-disposition" (paramPairs headers)
  where
    processDisp t | T.null t  = Nothing
                  | T.null bs = Just $ Disposition { dispType = toDispType (T.toLower as)
                                                   , dispParams = []
                                                   }
                  | otherwise = Just $ Disposition { dispType = toDispType (T.toLower as)
                                                   , dispParams = processParams (parseParams bs)
                                                   } 
      where (as,bs) = T.break (\ch -> isSpace ch || ch == ';') t

    processParams = map procP
      where
        procP (MIMEParam as val)
            | "name" == asl              = Name val
            | "filename" == asl          = Filename val
            | "creation-date" == asl     = CreationDate val
            | "modification-date" == asl = ModDate val
            | "read-date" == asl         = ReadDate val
            | "size" == asl              = Size val
            | otherwise                  = OtherParam asl val
          where asl = T.toLower as

    toDispType t = if t == "inline" then DispInline
                   else if t == "attachment" then DispAttachment
                   else if t == "form-data"  then DispFormData
                   else  DispOther t

paramPairs :: [MIMEParam] -> [(T.Text, T.Text)]
paramPairs = map paramPair
  where
    paramPair (MIMEParam a b) = (a,b)

processBody :: [MIMEParam] -> T.Text -> T.Text
processBody headers body =
  case lookupField "content-transfer-encoding" $ paramPairs headers of
    Nothing -> body
    Just v  -> T.pack $ decodeBody (T.unpack v) $ T.unpack body

normalizeCRLF :: T.Text -> T.Text
normalizeCRLF t
    | T.null t = ""
    | "\r\n" `T.isPrefixOf` t = "\r\n" <> normalizeCRLF (T.drop 2 t)
    | any (`T.isPrefixOf` t) ["\r", "\n"] = "\r\n" <> normalizeCRLF (T.drop 1 t)
    | otherwise = let (a,b) = T.break (`elem` "\r\n") t in a <> normalizeCRLF b
  
parseMIMEMessage :: T.Text -> MIMEValue
parseMIMEMessage entity =
  case parseHeaders (normalizeCRLF entity) of
   (as,bs) -> parseMIMEBody as bs

parseHeaders :: T.Text -> ([MIMEParam], T.Text)
parseHeaders str =
  case findFieldName "" str of
    Left (nm, rs) -> parseFieldValue nm (dropFoldingWSP rs)
    Right body    -> ([],body)
 where
  findFieldName acc t 
    | T.null t = Right ""
    | "\r\n" `T.isPrefixOf` t = Right $ T.drop 2 t
    | ":" `T.isPrefixOf` t = Left (T.reverse $ T.dropWhile isHSpace acc, T.drop 1 t)
    | otherwise = findFieldName (T.take 1 t <> acc) $ T.drop 1 t

  parseFieldValue nm xs 
      | T.null bs = ([MIMEParam nm as], "")
      | otherwise = let (zs,ys) = parseHeaders bs in (MIMEParam nm as :zs, ys)
    where 
      (as,bs) = takeUntilCRLF xs

parseMultipart :: Type -> T.Text -> (MIMEValue, T.Text)
parseMultipart mty body =
  case lookupField "boundary" (paramPairs $ mimeParams mty) of
    Nothing -> doTrace ("Multipart mime type, " ++ T.unpack (showType mty) ++
      ", has no required boundary parameter. Defaulting to text/plain") $
      (nullMIMEValue{ mime_val_type = defaultType
                    , mime_val_disp = Nothing
		    , mime_val_content = Single body
		    }, "")
    Just bnd -> (nullMIMEValue { mime_val_type = mty
                               , mime_val_disp = Nothing
			       , mime_val_content = Multi vals
			       }, rs)
      where (vals,rs) = splitMulti bnd body

splitMulti :: T.Text -> T.Text -> ([MIMEValue], T.Text)
splitMulti bnd body_in =
  -- Note: we insert a CRLF if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body | "--" `T.isPrefixOf` body_in = "\r\n" <> body_in
           | otherwise  = body_in
  in case untilMatch dashBoundary body of
       Nothing           -> mempty
       Just xs  | "--" `T.isPrefixOf` xs    -> ([], T.drop 2 xs)
                | otherwise                 -> splitMulti1 (dropTrailer xs)

 where
  dashBoundary = ("\r\n--" <> bnd)

  splitMulti1 xs 
      | T.null as && T.null bs = ([], "")
      | T.null bs = ([parseMIMEMessage as],"")
      | T.isPrefixOf "--" bs    =  ([parseMIMEMessage as], dropTrailer bs)
      | otherwise   = let (zs,ys) = splitMulti1 (dropTrailer bs)
                            in ((parseMIMEMessage as) : zs,ys)

    where
      (as,bs) = matchUntil dashBoundary xs

  dropTrailer xs 
      | "\r\n" `T.isPrefixOf` xs1 = T.drop 2 xs1
      | otherwise   = xs1 -- hmm, flag an error?
    where
       xs1 = T.dropWhile isHSpace xs 

parseMIMEType :: T.Text -> Maybe Type
parseMIMEType = parseContentType

parseContentType :: T.Text -> Maybe Type
parseContentType str
    | T.null minor0 = doTrace ("unable to parse content-type: " ++ show str) $ Nothing
    | otherwise     = Just Type  { mimeType = toType maj as
                                 , mimeParams = parseParams (T.dropWhile isHSpace bs)
                                 }
  where
    (maj, minor0) = T.break (=='/') (dropFoldingWSP str)
    minor = T.drop 1 minor0
    (as, bs) = T.break (\ ch -> isHSpace ch || isTSpecial ch) minor 
    toType a b = case lookupField (T.toLower a) mediaTypes of
         Just ctor -> ctor b
         _ -> Other a b

parseParams :: T.Text -> [MIMEParam]
parseParams t   | T.null t          = []
                | ';' == T.head t   = let (nm_raw, vs0) = T.break (=='=') (dropFoldingWSP $ T.tail t) 
                                          nm = T.toLower nm_raw in
                    if T.null vs0 
                        then []
                        else let vs = T.tail vs0 in
                            if not (T.null vs) && T.head vs == '"' 
                                then let vs1 = T.tail vs 
                                         (val, zs0) = T.break (=='"') vs1 in
                                    if T.null zs0 
                                        then [MIMEParam nm val]
                                        else MIMEParam nm val : parseParams (T.dropWhile isHSpace $ T.tail zs0)
                                else let (val, zs) = T.break (\ch -> isHSpace ch || isTSpecial ch) vs in
                                    MIMEParam nm val : parseParams (T.dropWhile isHSpace zs)
                | otherwise = doTrace ("Codec.MIME.Parse.parseParams: curious param value -- " ++ show t) []

mediaTypes :: [(T.Text, T.Text -> MIMEType)]
mediaTypes =
  [ ("multipart",   (Multipart . toMultipart))
  , ("application", Application)
  , ("audio",       Audio)
  , ("image",       Image)
  , ("message",     Message)
  , ("model",       Model)
  , ("text",        Text)
  , ("video",       Video)
  ]
 where toMultipart b = fromMaybe other (lookupField (T.toLower b) multipartTypes)
          where other | T.isPrefixOf "x-" b = Extension b
                      | otherwise           = OtherMulti b

multipartTypes :: [(T.Text, Multipart)]
multipartTypes =
  [ ("alternative", Alternative)
  , ("byteranges",  Byteranges)
  , ("digest",      Digest)
  , ("encrypted",   Encrypted)
  , ("form-data",   FormData)
  , ("mixed",       Mixed)
  , ("parallel",    Parallel)
  , ("related",     Related)
  , ("signed",      Signed)
  ]

untilMatch :: T.Text -> T.Text -> Maybe T.Text
untilMatch a b  | T.null a  = Just b
                | T.null b  = Nothing
                | a `T.isPrefixOf` b = Just $ T.drop (T.length a) b
                | otherwise = untilMatch a $ T.tail b

matchUntil :: T.Text -> T.Text -> (T.Text, T.Text)
-- searching str; returning parts before str and after str
matchUntil str = second (T.drop $ T.length str) . T.breakOn str

{-
matchUntil' :: T.Text -> T.Text -> (T.Text, T.Text)
matchUntil' _   "" = ("", "")
matchUntil' str xs
    | T.null xs = mempty
    -- slow, but it'll do for now.
    | str `T.isPrefixOf` xs = ("", T.drop (T.length str) xs)
    | otherwise = let (as,bs) = matchUntil' str $ T.tail xs in (T.take 1 xs <> as, bs)
-}

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isTSpecial :: Char -> Bool
isTSpecial x = x `elem` "()<>@,;:\\\"/[]?=" -- "

dropFoldingWSP :: T.Text -> T.Text
dropFoldingWSP t | T.null t   = ""
                 | isHSpace (T.head t) = dropFoldingWSP $ T.tail t
                 | "\r\n" `T.isPrefixOf` t && not (T.null $ T.drop 2 t) && isHSpace (T.head $ T.drop 2 t) 
                    = dropFoldingWSP $ T.drop 3 t
                 | otherwise    = t 

takeUntilCRLF :: T.Text -> (T.Text, T.Text)
takeUntilCRLF str = go "" str
 where
  go acc t  | T.null t  = (T.reverse (T.dropWhile isHSpace acc), "")
            | "\r\n" `T.isPrefixOf` t && not (T.null $ T.drop 2 t) && isHSpace (T.head $ T.drop 2 t)  
                        = go (" " <> acc) (T.drop 3 t)
            | "\r\n" `T.isPrefixOf` t && not (T.null $ T.drop 2 t)  
                        = (T.reverse (T.dropWhile isHSpace acc), T.drop 2 t)
            | otherwise = go (T.take 1 t <> acc) $ T.tail t

-- case in-sensitive lookup of field names or attributes\/parameters.
lookupField :: T.Text -> [(T.Text,a)] -> Maybe a
lookupField n ns = 
   -- assume that inputs have been mostly normalized already 
   -- (i.e., lower-cased), but should the lookup fail fall back
   -- to a second try where we do normalize before giving up.
  case lookup n ns of
    x@Just{} -> x
    Nothing  -> 
      let nl = T.toLower n in
      fmap snd $ L.find ((nl==) . T.toLower . fst) ns
      
