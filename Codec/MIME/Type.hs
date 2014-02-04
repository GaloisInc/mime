{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Type
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability : provisional
-- Portability: portable
--
-- 
-- Representing MIME types and values.
-- 
--------------------------------------------------------------------
module Codec.MIME.Type where

import qualified Data.Text as T
import Data.Monoid ((<>))

data MIMEParam = MIMEParam  { paramName     :: T.Text
                            , paramValue    :: T.Text }
    deriving (Show, Ord, Eq)

data Type = Type
    { mimeType   :: MIMEType
    , mimeParams :: [MIMEParam]
    } deriving ( Show, Ord, Eq )

-- | The @null@ MIME record type value; currently a @text/plain@.
nullType :: Type
nullType = Type
    { mimeType   = Text "plain"
    , mimeParams = []
    }

showType :: Type -> T.Text
showType t = showMIMEType (mimeType t) <> showMIMEParams (mimeParams t)

showMIMEParams :: [MIMEParam] -> T.Text
showMIMEParams ps = T.concat $ map showP ps
  where 
    showP (MIMEParam a b) = "; " <> a <> "=\"" <> b <> "\""


data MIMEType
 = Application SubType
 | Audio       SubType
 | Image       SubType
 | Message     SubType
 | Model       SubType
 | Multipart   Multipart
 | Text        TextType
 | Video       SubType
 | Other       {otherType :: T.Text, otherSubType :: SubType}
   deriving ( Show, Ord, Eq )

showMIMEType :: MIMEType -> T.Text
showMIMEType t = 
 case t of
   Application s -> "application/"<>s
   Audio s       -> "audio/"<>s
   Image s       -> "image/"<>s
   Message s     -> "message/"<>s
   Model s       -> "model/"<>s
   Multipart s   -> "multipart/"<>showMultipart s
   Text s        -> "text/"<>s
   Video s       -> "video/"<>s
   Other a b     -> a <> "/" <> b

-- | a (type, subtype) MIME pair.
data MIMEPair
 = MIMEPair T.Text SubType
   deriving ( Eq )

showMIMEPair :: MIMEPair -> T.Text
showMIMEPair (MIMEPair a b) = a <> "/" <> b

-- | default subtype representation.
type SubType = T.Text

-- | subtype for text content; currently just a string.
type TextType = SubType

subTypeString :: Type -> T.Text
subTypeString t = T.drop 1 $ snd $ T.break (=='/') (showMIMEType (mimeType t))

majTypeString :: Type -> T.Text
majTypeString t = fst $ T.break (=='/') (showMIMEType (mimeType t))

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
 | Extension  T.Text  -- ^ e.g., 'x-foo' (i.e., includes the 'x-' bit)
 | OtherMulti T.Text  -- unrecognized\/uninterpreted.
                      -- (e.g., appledouble, voice-message, etc.)
   deriving ( Show, Ord, Eq )

isXmlBased :: Type -> Bool
isXmlBased t = 
  case mimeType t of
     Multipart{} -> False
     _ -> "+xml" `T.isSuffixOf` subTypeString t

isXmlType :: Type -> Bool
isXmlType t = isXmlBased t ||
  case mimeType t of
    Application s -> s `elem` xml_media_types
    Text s        -> s `elem` xml_media_types
    _             -> False
 where
    -- Note: xml-dtd isn't considered an XML type here.
  xml_media_types :: [T.Text]
  xml_media_types = 
    [ "xml"
    , "xml-external-parsed-entity"
    ]
  

showMultipart :: Multipart -> T.Text
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
   
type Content = T.Text

data MIMEValue = MIMEValue
      { mime_val_type     :: Type
      , mime_val_disp     :: Maybe Disposition
      , mime_val_content  :: MIMEContent
      , mime_val_headers  :: [MIMEParam]
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
 | DispOther T.Text
   deriving ( Show, Eq)

data DispParam
 = Name T.Text
 | Filename T.Text
 | CreationDate T.Text
 | ModDate T.Text
 | ReadDate T.Text
 | Size T.Text
 | OtherParam T.Text T.Text
   deriving ( Show, Eq)
