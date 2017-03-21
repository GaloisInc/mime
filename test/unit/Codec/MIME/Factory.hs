{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, QuasiQuotes #-}
module Codec.MIME.Factory where
import Codec.MIME.Type
import Data.String.QQ
import qualified Data.Text as T

(singlePartPlain :: T.Text) = "\
\From: John Doe <example@example.com>\n\
\MIME-Version: 1.0\n\
\Content-Type: text/plain\n\
\\n\
\This is the body text\n\
\\n\
\"

(multiPartTwoPlain :: T.Text) = [s|
MIME-Version: 1.0
Received: by 10.157.73.149 with HTTP; Tue, 21 Mar 2017 12:15:04 -0700 (PDT)
From: Heath Ritchie <heath@example.com>
Date: Tue, 21 Mar 2017 15:15:04 -0400
Subject: sample multipart email
To: Kanishka Azimi <kanishka@example.com>
Content-Type: multipart/mixed; boundary=001a113cece61c9a9c054b4276d8

--001a113cece61c9a9c054b4276d8
Content-Type: text/plain; charset=UTF-8

this is the body

--001a113cece61c9a9c054b4276d8
Content-Type: text/plain; charset=US-ASCII; name="attachment1.txt"
Content-Disposition: attachment; filename="attachment1.txt"
Content-Transfer-Encoding: base64

dGhpcyBpcyBhbiBhdHRhY2htZW50Cg==
--001a113cece61c9a9c054b4276d8--|]


singlePartIcs = ""

multiPartIcsPlain = ""