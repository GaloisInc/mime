{- |
 
  Module      :  Codec.MIME.Type
  Copyright   :  (c) 2006-2008, Galois Inc.
- License     : BSD3

  Maintainer      : Sigbjorn Finne <sof@galois.com>
  Stability       : unstable
  Portability     : GHC
  
  Representing MIME types and values.
-}

module Codec.MIME.Type where

import Data.List ( concatMap, isSuffixOf )

data Type
 = Type
    { mimeType   :: MIMEType
    , mimeParams :: [(String,String)]
    } deriving ( Show, Ord, Eq )

nullType :: Type
nullType = Type
    { mimeType   = Text "plain"
    , mimeParams = []
    }

showType :: Type -> String
showType t = showMIMEType (mimeType t) ++ showMIMEParams (mimeParams t)

showMIMEParams :: [(String,String)] -> String
showMIMEParams ps = concatMap showP ps
 where 
  showP (a,b) = ';':' ':a ++ '=':'"':b ++ "\""


data MIMEType
 = Application SubType
 | Audio       SubType
 | Image       SubType
 | Message     SubType
 | Model       SubType
 | Multipart   Multipart
 | Text        TextType
 | Video       SubType
 | Other       String SubType
   deriving ( Show, Ord, Eq )

showMIMEType :: MIMEType -> String
showMIMEType t = 
 case t of
   Application s -> "application/"++s
   Audio s       -> "audio/"++s
   Image s       -> "image/"++s
   Message s     -> "message/"++s
   Model s       -> "model/"++s
   Multipart s   -> "multipart/"++showMultipart s
   Text s        -> "text/"++s
   Video s       -> "video/"++s
   Other a b     -> a ++ '/':b

-- | a (type, subtype) MIME pair.
data MIMEPair
 = MIMEPair String SubType
   deriving ( Eq )

showMIMEPair :: MIMEPair -> String
showMIMEPair (MIMEPair a b) = a ++ '/':b

-- | default subtype representation.
type SubType = String

-- | subtype for text content; currently just a string.
type TextType = SubType

subTypeString :: Type -> String
subTypeString t = 
  case break (=='/') (showMIMEType (mimeType t)) of
   (_,"") -> ""
   (_,_:bs) -> bs

majTypeString :: Type -> String
majTypeString t = 
  case break (=='/') (showMIMEType (mimeType t)) of
   (as,_) -> as

data Multipart
 = Alternative
 | Byteranges
 | Digest
 | Encrypted
 | FormData
 | Mixed
 | Parallel
 | Related
 | Signed
 | Extension  String  -- ^ e.g., 'x-foo' (i.e., includes the 'x-' bit)
 | OtherMulti String  -- unrecognized\/uninterpreted.
                      -- (e.g., appledouble, voice-message, etc.)
   deriving ( Show, Ord, Eq )

isXmlBased :: Type -> Bool
isXmlBased t = 
  case mimeType t of
     Multipart{} -> False
     _ -> "+xml" `isSuffixOf` subTypeString t

isXmlType :: Type -> Bool
isXmlType t = isXmlBased t ||
  case mimeType t of
    Application s -> s `elem` xml_media_types
    Text s        -> s `elem` xml_media_types
    _             -> False
 where
    -- Note: xml-dtd isn't considered an XML type here.
  xml_media_types :: [String]
  xml_media_types = 
    [ "xml"
    , "xml-external-parsed-entity"
    ]
  

showMultipart :: Multipart -> String
showMultipart m = 
 case m of
   Alternative -> "alternative"
   Byteranges  -> "byteranges"
   Digest      -> "digest"
   Encrypted   -> "encrypted"
   FormData    -> "form-data"
   Mixed       -> "mixed"
   Parallel    -> "parallel"
   Related     -> "related"
   Signed      -> "signed"
   Extension e -> e
   OtherMulti e -> e
   
type Content = String

data MIMEValue = MIMEValue
      { mime_val_type     :: Type
      , mime_val_disp     :: Maybe Disposition
      , mime_val_content  :: MIMEContent
      , mime_val_headers  :: [(String,String)]
      , mime_val_inc_type :: Bool
      } deriving ( Show, Eq )

nullMIMEValue :: MIMEValue
nullMIMEValue = MIMEValue
      { mime_val_type     = nullType
      , mime_val_disp     = Nothing
      , mime_val_content  = Multi []
      , mime_val_headers  = []
      , mime_val_inc_type = True
      } 

data MIMEContent 
  = Single Content
  | Multi [MIMEValue]
    deriving (Eq,Show)
   
data Disposition
 = Disposition
     { dispType   :: DispType
     , dispParams :: [DispParam]
     } deriving ( Show, Eq )

data DispType
 = DispInline
 | DispAttachment
 | DispFormData
 | DispOther String
   deriving ( Show, Eq)

data DispParam
 = Name String
 | Filename String
 | CreationDate String
 | ModDate String
 | ReadDate String
 | Size String
 | OtherParam String String
   deriving ( Show, Eq)
