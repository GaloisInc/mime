{- |
  Module      :  Codec.MIME.Parse
  Copyright   :  (c) 2006-2008 Galois Inc.

  Maintainer      : Sigbjorn Finne <sof@galois.com>
  Stability       : unstable
  Portability     : GHC

  Parsing MIME content.
-}
module Codec.MIME.Parse
  ( parseMIMEBody
  , parseMIMEType
  ) where

import Codec.MIME.Type
import Codec.MIME.Decode

import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace ( trace )

parseMIMEBody :: [(String,String)] -> String -> MIMEValue
parseMIMEBody headers_in body =
  case mimeType mty of
    Multipart{} -> fst (parseMultipart mty body)
    Message{}   -> fst (parseMultipart mty body)
    _           -> MIMEValue mty (parseContentDisp headers)
                                 (Single (processBody headers body))

 where headers = [ (map toLower k,v) | (k,v) <- headers_in ]
       mty = fromMaybe defaultType
                       (parseContentType =<< lookupField "content-type" headers)
defaultType :: Type
defaultType = Type { mimeType   = Text "plain"
                   , mimeParams = [("charset", "us-ascii")]
                   }

parseContentDisp :: [(String,String)] -> Maybe Disposition
parseContentDisp headers =
  (processDisp . dropFoldingWSP) =<< lookupField "content-disposition" headers
  where
  processDisp "" = Nothing
  processDisp xs = Just $
    case break (\ch -> isSpace ch || ch == ';') xs of
      (as,"") -> Disposition { dispType = toDispType (map toLower as)
                             , dispParams = []
                             }
      (as,bs) -> Disposition { dispType = toDispType (map toLower as)
                             , dispParams = processParams (parseParams bs)
                             }

  processParams = map procP
    where
    procP (as,val)
      | "name" == asl              = Name val
      | "filename" == asl          = Filename val
      | "creation-date" == asl     = CreationDate val
      | "modification-date" == asl = ModDate val
      | "read-date" == asl         = ReadDate val
      | "size" == asl              = Size val
      | otherwise                  = OtherParam (map toLower as) val
      where asl = map toLower as

  toDispType t = case t of
                   "inline"     -> DispInline
                   "attachment" -> DispAttachment
                   "form-data"  -> DispFormData
                   _            -> DispOther t


processBody :: [(String,String)] -> String -> String
processBody headers body =
  case lookupField "content-transfer-encoding" headers of
    Nothing -> body
    Just v  -> decodeBody v body

parseMIMEMessage :: String -> MIMEValue
parseMIMEMessage entity =
  case parseHeaders entity of
   (as,bs) -> parseMIMEBody as bs

parseHeaders :: String -> ([(String,String)], String)
parseHeaders str =
  case findFieldName "" str of
    Left (nm, rs) -> parseFieldValue nm (dropFoldingWSP rs)
    Right body    -> ([],body)
 where
  findFieldName _acc "" = Right ""
  findFieldName _acc ('\r':'\n':xs) = Right xs
  findFieldName acc (':':xs) = Left (reverse (dropWhile isHSpace acc), xs)
  findFieldName acc (x:xs) = findFieldName (x:acc) xs

  parseFieldValue nm xs =
    case takeUntilCRLF xs of
      (as,"") -> ([(nm,as)],"")
      (as,bs) -> let (zs,ys) = parseHeaders bs in ((nm,as):zs,ys)

parseMultipart :: Type -> String -> (MIMEValue, String)
parseMultipart mty body =
  case lookupField "boundary" (mimeParams mty) of
    Nothing -> trace ("Multipart mime type, " ++ showType mty ++
      ", has no required boundary parameter. Defaulting to text/plain") $
      (MIMEValue defaultType Nothing (Single body), "")
    Just bnd -> (MIMEValue mty Nothing (Multi vals), rs)
      where (vals,rs) = splitMulti bnd body

splitMulti :: String -> String -> ([MIMEValue], String)
splitMulti bnd body_in =
  -- Note: we insert a CRLF if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body = case body_in of
                '-':'-':_ -> ('\r':'\n':body_in)
                _ -> body_in
  in case untilMatch dashBoundary body of
       Nothing           -> ([],"")
       Just ('-':'-':xs) -> ([],xs)
       Just xs           -> splitMulti1 (dropTrailer xs)

 where
  dashBoundary = ("\r\n--" ++ bnd)

  splitMulti1 xs =
    case matchUntil dashBoundary xs of
      ("","") -> ([],"")
      (as,"") -> ([parseMIMEMessage as],"")
      (as,'-':'-':bs) -> ([parseMIMEMessage as], dropTrailer bs)
      (as,bs) -> let (zs,ys) = splitMulti1 (dropTrailer bs)
                 in ((parseMIMEMessage as) : zs,ys)

  dropTrailer xs =
    case dropWhile isHSpace xs of
      '\r':'\n':xs1 -> xs1
      xs1 -> xs1 -- hmm, flag an error?

parseMIMEType :: String -> Maybe Type
parseMIMEType = parseContentType

parseContentType :: String -> Maybe Type
parseContentType str =
   case break (=='/') (dropFoldingWSP str) of
       (maj,_:minor) ->
          case break (\ ch -> isHSpace ch || isTSpecial ch) minor of
            (as,bs)  -> 
               Just Type { mimeType = toType maj as
                         , mimeParams = parseParams (dropWhile isHSpace bs)
                         }
       _ -> trace ("unable to parse content-type: " ++ show str) $ Nothing
 where
  toType a b = case lookupField (map toLower a) mediaTypes of
                 Just ctor -> ctor b
                 _ -> Other a b


parseParams :: String -> [(String,String)]
parseParams "" = []
parseParams (';':xs) =
    case break (=='=') (dropFoldingWSP xs) of
      (_,[]) -> []
      (nm_raw,_:vs) ->
        case vs of
          '"':vs1 ->
             case break (=='"') vs1 of
               (val,"") -> [(nm,val)]
               (val,_:zs) -> (nm,val):parseParams (dropWhile isHSpace zs)
          _ -> case break (\ ch -> isHSpace ch || isTSpecial ch) vs of
                 (val,zs) -> (nm,val):parseParams (dropWhile isHSpace zs)
       where
        nm = map toLower nm_raw

parseParams cs = trace ("Codec.MIME.Parse.parseParams: curious param value -- " ++ show cs) []

mediaTypes :: [(String, String -> MIMEType)]
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
 where toMultipart b = fromMaybe other (lookupField (map toLower b) multipartTypes)
          where other = case b of
                          'x':'-':_ -> Extension b
                          _ -> OtherMulti b


multipartTypes :: [(String, Multipart)]
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


untilMatch :: String -> String -> Maybe String
untilMatch str xs = go str xs
  where go ""     rs      = Just rs
        go _      ""      = Nothing
        go (a:as) (b:bs)  = if a == b then go as bs else go str bs

matchUntil :: String -> String -> (String, String)
matchUntil _   "" = ("", "")
matchUntil str xs
    -- slow, but it'll do for now.
  | str `isPrefixOf` xs = ("", drop (length str) xs)
matchUntil str (x:xs) = let (as,bs) = matchUntil str xs in (x:as,bs)



isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isTSpecial :: Char -> Bool
isTSpecial x = x `elem` "()<>@,;:\\\"/[]?="


dropFoldingWSP :: String -> String
dropFoldingWSP "" = ""
dropFoldingWSP (x:xs)
 | isHSpace x = dropFoldingWSP xs
dropFoldingWSP ('\r':'\n':x:xs)
 | isHSpace x = dropFoldingWSP xs
dropFoldingWSP (x:xs) = x:xs

takeUntilCRLF :: String -> (String, String)
takeUntilCRLF str = go "" str
 where
  go acc "" = (reverse (dropWhile isHSpace acc), "")
  go acc ('\r':'\n':x:xs)
   | isHSpace x = go (' ':acc) xs
   | otherwise  = (reverse (dropWhile isHSpace acc), x:xs)
  go acc (x:xs) = go (x:acc) xs

-- case in-sensitive lookup of field names or attributes\/parameters.
lookupField :: String -> [(String,a)] -> Maybe a
lookupField n ns = 
   -- assume that inputs have been mostly normalized already 
   -- (i.e., lower-cased), but should the lookup fail fall back
   -- to a second try where we do normalize before giving up.
  case lookup n ns of
    x@Just{} -> x
    Nothing  -> 
      let nl = map toLower n in
      case find (\ (y,_) -> nl == map toLower y) ns of
        Nothing -> Nothing
	Just (_,x)  -> Just x
      
