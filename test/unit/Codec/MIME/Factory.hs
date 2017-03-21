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


-- singlePartIcs = ""

(multiPartIcsPlain :: T.Text) = [s|
MIME-Version: 1.0
Reply-To: Heath Ritchie <heath@example.com>
Sender: Google Calendar <calendar-notification@google.com>
Date: Tue, 21 Mar 2017 19:43:54 +0000
Subject: Invitation: test ics parser @ Tue Mar 21, 2017 4pm - 5pm (Kanishka Azimi)
From: Heath Ritchie <heath@example.com>
To: kanishka@example.com
Content-Type: multipart/mixed; boundary=94eb2c0550982f0f6b054b42dda0

--94eb2c0550982f0f6b054b42dda0
Content-Type: multipart/alternative; boundary=94eb2c0550982f0f67054b42dd9e

--94eb2c0550982f0f67054b42dd9e
Content-Type: text/plain; charset=UTF-8; format=flowed; delsp=yes
Content-Transfer-Encoding: base64

WW91IGhhdmUgYmVlbiBpbnZpdGVkIHRvIHRoZSBmb2xsb3dpbmcgZXZlbnQuDQoNClRpdGxlOiB0
ZXN0IGljcyBwYXJzZXINCmRlc2NyaXB0aW9uIGdvZXMgaGVyZQ0KV2hlbjogVHVlIE1hciAyMSwg
MjAxNyA0cG0g4oCTIDVwbSBFYXN0ZXJuIFRpbWUNCkNhbGVuZGFyOiBLYW5pc2hrYSBBemltaQ0K
V2hvOg0KICAgICAqIEhlYXRoIFJpdGNoaWUgLSBvcmdhbml6ZXINCiAgICAgKiBLYW5pc2hrYSBB
emltaQ0KDQpFdmVudCBkZXRhaWxzOiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFy
L2V2ZW50P2FjdGlvbj1WSUVXJmVpZD1OR0kyTVdveE0zRm9iRGRoWjJkbE4zSnJOV2d3WmpNNGJY
TWdhMkZ1YVhOb2EyRkFiV0ZqYTJWNVkyOXljQzVqYjIwJnRvaz1NakFqYUdWaGRHaEFiV0ZqYTJW
NVkyOXljQzVqYjIwME16STVNV0V4T0dVeFlUUXdZamMzWmpFME9URmhOVGc0WVRSbFkySXhPR0Uw
TldOak5qRmomY3R6PUFtZXJpY2EvTmV3X1lvcmsmaGw9ZW4NCg0KSW52aXRhdGlvbiBmcm9tIEdv
b2dsZSBDYWxlbmRhcjogaHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS9jYWxlbmRhci8NCg0KWW91IGFy
ZSByZWNlaXZpbmcgdGhpcyBlbWFpbCBhdCB0aGUgYWNjb3VudCBrYW5pc2hrYUBtYWNrZXljb3Jw
LmNvbSBiZWNhdXNlICANCnlvdSBhcmUgc3Vic2NyaWJlZCBmb3IgaW52aXRhdGlvbnMgb24gY2Fs
ZW5kYXIgS2FuaXNoa2EgQXppbWkuDQoNClRvIHN0b3AgcmVjZWl2aW5nIHRoZXNlIGVtYWlscywg
cGxlYXNlIGxvZyBpbiB0byAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyLyBhbmQg
Y2hhbmdlIHlvdXIgbm90aWZpY2F0aW9uIHNldHRpbmdzIGZvciAgDQp0aGlzIGNhbGVuZGFyLg0K
DQpGb3J3YXJkaW5nIHRoaXMgaW52aXRhdGlvbiBjb3VsZCBhbGxvdyBhbnkgcmVjaXBpZW50IHRv
IG1vZGlmeSB5b3VyIFJTVlAgIA0KcmVzcG9uc2UuIExlYXJuIG1vcmUgYXQgIA0KaHR0cHM6Ly9z
dXBwb3J0Lmdvb2dsZS5jb20vY2FsZW5kYXIvYW5zd2VyLzM3MTM1I2ZvcndhcmRpbmcNCg==
--94eb2c0550982f0f67054b42dd9e
Content-Type: text/html; charset=UTF-8
Content-Transfer-Encoding: quoted-printable

<span itemscope itemtype=3D"http://schema.org/InformAction"><span style=3D"=
display:none" itemprop=3D"about" itemscope itemtype=3D"http://schema.org/Pe=
rson"><meta itemprop=3D"description" content=3D"Invitation from Heath Ritch=
ie"/></span><span itemprop=3D"object" itemscope itemtype=3D"http://schema.o=
rg/Event"><div style=3D""><table cellspacing=3D"0" cellpadding=3D"8" border=
=3D"0" summary=3D"" style=3D"width:100%;font-family:Arial,Sans-serif;border=
:1px Solid #ccc;border-width:1px 2px 2px 1px;background-color:#fff;"><tr><t=
d><meta itemprop=3D"eventStatus" content=3D"http://schema.org/EventSchedule=
d"/><div style=3D"padding:2px"><span itemprop=3D"publisher" itemscope itemt=
ype=3D"http://schema.org/Organization"><meta itemprop=3D"name" content=3D"G=
oogle Calendar"/></span><meta itemprop=3D"eventId/googleCalendar" content=
=3D"4b61j13qhl7agge7rk5h0f38ms"/><div style=3D"float:right;font-weight:bold=
;font-size:13px"> <a href=3D"https://www.google.com/calendar/event?action=
=3DVIEW&amp;eid=3DNGI2MWoxM3FobDdhZ2dlN3JrNWgwZjM4bXMga2FuaXNoa2FAbWFja2V5Y=
29ycC5jb20&amp;tok=3DMjAjaGVhdGhAbWFja2V5Y29ycC5jb200MzI5MWExOGUxYTQwYjc3Zj=
E0OTFhNTg4YTRlY2IxOGE0NWNjNjFj&amp;ctz=3DAmerica/New_York&amp;hl=3Den" styl=
e=3D"color:#20c;white-space:nowrap" itemprop=3D"url">more details &raquo;</=
a><br></div><h3 style=3D"padding:0 0 6px 0;margin:0;font-family:Arial,Sans-=
serif;font-size:16px;font-weight:bold;color:#222"><span itemprop=3D"name">t=
est ics parser</span></h3><div style=3D"padding-bottom:15px;font-size:13px;=
color:#222;white-space:pre-wrap!important;white-space:-moz-pre-wrap!importa=
nt;white-space:-pre-wrap!important;white-space:-o-pre-wrap!important;white-=
space:pre;word-wrap:break-word"><span>description goes here</span><meta ite=
mprop=3D"description" content=3D"description goes here"/></div><table cellp=
adding=3D"0" cellspacing=3D"0" border=3D"0" summary=3D"Event details"><tr><=
td style=3D"padding:0 1em 10px 0;font-family:Arial,Sans-serif;font-size:13p=
x;color:#888;white-space:nowrap" valign=3D"top"><div><i style=3D"font-style=
:normal">When</i></div></td><td style=3D"padding-bottom:10px;font-family:Ar=
ial,Sans-serif;font-size:13px;color:#222" valign=3D"top"><time itemprop=3D"=
startDate" datetime=3D"20170321T200000Z"></time><time itemprop=3D"endDate" =
datetime=3D"20170321T210000Z"></time>Tue Mar 21, 2017 4pm =E2=80=93 5pm <sp=
an style=3D"color:#888">Eastern Time</span></td></tr><tr><td style=3D"paddi=
ng:0 1em 10px 0;font-family:Arial,Sans-serif;font-size:13px;color:#888;whit=
e-space:nowrap" valign=3D"top"><div><i style=3D"font-style:normal">Calendar=
</i></div></td><td style=3D"padding-bottom:10px;font-family:Arial,Sans-seri=
f;font-size:13px;color:#222" valign=3D"top">Kanishka Azimi</td></tr><tr><td=
 style=3D"padding:0 1em 10px 0;font-family:Arial,Sans-serif;font-size:13px;=
color:#888;white-space:nowrap" valign=3D"top"><div><i style=3D"font-style:n=
ormal">Who</i></div></td><td style=3D"padding-bottom:10px;font-family:Arial=
,Sans-serif;font-size:13px;color:#222" valign=3D"top"><table cellspacing=3D=
"0" cellpadding=3D"0"><tr><td style=3D"padding-right:10px;font-family:Arial=
,Sans-serif;font-size:13px;color:#222"><span style=3D"font-family:Courier N=
ew,monospace">&#x2022;</span></td><td style=3D"padding-right:10px;font-fami=
ly:Arial,Sans-serif;font-size:13px;color:#222"><div><div style=3D"margin:0 =
0 0.3em 0"><span itemprop=3D"attendee" itemscope itemtype=3D"http://schema.=
org/Person"><span itemprop=3D"name" class=3D"notranslate">Heath Ritchie</sp=
an><meta itemprop=3D"email" content=3D"heath@example.com"/></span><span =
itemprop=3D"organizer" itemscope itemtype=3D"http://schema.org/Person"><met=
a itemprop=3D"name" content=3D"Heath Ritchie"/><meta itemprop=3D"email" con=
tent=3D"heath@example.com"/></span><span style=3D"font-size:11px;color:#=
888"> - organizer</span></div></div></td></tr><tr><td style=3D"padding-righ=
t:10px;font-family:Arial,Sans-serif;font-size:13px;color:#222"><span style=
=3D"font-family:Courier New,monospace">&#x2022;</span></td><td style=3D"pad=
ding-right:10px;font-family:Arial,Sans-serif;font-size:13px;color:#222"><di=
v><div style=3D"margin:0 0 0.3em 0"><span itemprop=3D"attendee" itemscope i=
temtype=3D"http://schema.org/Person"><span itemprop=3D"name" class=3D"notra=
nslate">Kanishka Azimi</span><meta itemprop=3D"email" content=3D"kanishka@m=
ackeycorp.com"/></span></div></div></td></tr></table></td></tr></table></di=
v><p style=3D"color:#222;font-size:13px;margin:0"><span style=3D"color:#888=
">Going?&nbsp;&nbsp;&nbsp;</span><wbr><strong><span itemprop=3D"potentialac=
tion" itemscope itemtype=3D"http://schema.org/RsvpAction"><meta itemprop=3D=
"attendance" content=3D"http://schema.org/RsvpAttendance/Yes"/><span itempr=
op=3D"handler" itemscope itemtype=3D"http://schema.org/HttpActionHandler"><=
link itemprop=3D"method" href=3D"http://schema.org/HttpRequestMethod/GET"/>=
<a href=3D"https://www.google.com/calendar/event?action=3DRESPOND&amp;eid=
=3DNGI2MWoxM3FobDdhZ2dlN3JrNWgwZjM4bXMga2FuaXNoa2FAbWFja2V5Y29ycC5jb20&amp;=
rst=3D1&amp;tok=3DMjAjaGVhdGhAbWFja2V5Y29ycC5jb200MzI5MWExOGUxYTQwYjc3ZjE0O=
TFhNTg4YTRlY2IxOGE0NWNjNjFj&amp;ctz=3DAmerica/New_York&amp;hl=3Den" style=
=3D"color:#20c;white-space:nowrap" itemprop=3D"url">Yes</a></span></span><s=
pan style=3D"margin:0 0.4em;font-weight:normal"> - </span><span itemprop=3D=
"potentialaction" itemscope itemtype=3D"http://schema.org/RsvpAction"><meta=
 itemprop=3D"attendance" content=3D"http://schema.org/RsvpAttendance/Maybe"=
/><span itemprop=3D"handler" itemscope itemtype=3D"http://schema.org/HttpAc=
tionHandler"><link itemprop=3D"method" href=3D"http://schema.org/HttpReques=
tMethod/GET"/><a href=3D"https://www.google.com/calendar/event?action=3DRES=
POND&amp;eid=3DNGI2MWoxM3FobDdhZ2dlN3JrNWgwZjM4bXMga2FuaXNoa2FAbWFja2V5Y29y=
cC5jb20&amp;rst=3D3&amp;tok=3DMjAjaGVhdGhAbWFja2V5Y29ycC5jb200MzI5MWExOGUxY=
TQwYjc3ZjE0OTFhNTg4YTRlY2IxOGE0NWNjNjFj&amp;ctz=3DAmerica/New_York&amp;hl=
=3Den" style=3D"color:#20c;white-space:nowrap" itemprop=3D"url">Maybe</a></=
span></span><span style=3D"margin:0 0.4em;font-weight:normal"> - </span><sp=
an itemprop=3D"potentialaction" itemscope itemtype=3D"http://schema.org/Rsv=
pAction"><meta itemprop=3D"attendance" content=3D"http://schema.org/RsvpAtt=
endance/No"/><span itemprop=3D"handler" itemscope itemtype=3D"http://schema=
.org/HttpActionHandler"><link itemprop=3D"method" href=3D"http://schema.org=
/HttpRequestMethod/GET"/><a href=3D"https://www.google.com/calendar/event?a=
ction=3DRESPOND&amp;eid=3DNGI2MWoxM3FobDdhZ2dlN3JrNWgwZjM4bXMga2FuaXNoa2FAb=
WFja2V5Y29ycC5jb20&amp;rst=3D2&amp;tok=3DMjAjaGVhdGhAbWFja2V5Y29ycC5jb200Mz=
I5MWExOGUxYTQwYjc3ZjE0OTFhNTg4YTRlY2IxOGE0NWNjNjFj&amp;ctz=3DAmerica/New_Yo=
rk&amp;hl=3Den" style=3D"color:#20c;white-space:nowrap" itemprop=3D"url">No=
</a></span></span></strong>&nbsp;&nbsp;&nbsp;&nbsp;<wbr><a href=3D"https://=
www.google.com/calendar/event?action=3DVIEW&amp;eid=3DNGI2MWoxM3FobDdhZ2dlN=
3JrNWgwZjM4bXMga2FuaXNoa2FAbWFja2V5Y29ycC5jb20&amp;tok=3DMjAjaGVhdGhAbWFja2=
V5Y29ycC5jb200MzI5MWExOGUxYTQwYjc3ZjE0OTFhNTg4YTRlY2IxOGE0NWNjNjFj&amp;ctz=
=3DAmerica/New_York&amp;hl=3Den" style=3D"color:#20c;white-space:nowrap" it=
emprop=3D"url">more options &raquo;</a></p></td></tr><tr><td style=3D"backg=
round-color:#f6f6f6;color:#888;border-top:1px Solid #ccc;font-family:Arial,=
Sans-serif;font-size:11px"><p>Invitation from <a href=3D"https://www.google=
.com/calendar/" target=3D"_blank" style=3D"">Google Calendar</a></p><p>You =
are receiving this email at the account kanishka@example.com because you=
 are subscribed for invitations on calendar Kanishka Azimi.</p><p>To stop r=
eceiving these emails, please log in to https://www.google.com/calendar/ an=
d change your notification settings for this calendar.</p><p>Forwarding thi=
s invitation could allow any recipient to modify your RSVP response. <a hre=
f=3D"https://support.google.com/calendar/answer/37135#forwarding">Learn Mor=
e</a>.</p></td></tr></table></div></span></span>
--94eb2c0550982f0f67054b42dd9e
Content-Type: text/calendar; charset=UTF-8; method=REQUEST
Content-Transfer-Encoding: 7bit

BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VEVENT
DTSTART:20170321T200000Z
DTEND:20170321T210000Z
DTSTAMP:20170321T194354Z
ORGANIZER;CN=Heath Ritchie:mailto:heath@example.com
UID:4b61j13qhl7agge7rk5h0f38ms@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=
 TRUE;CN=Kanishka Azimi;X-NUM-GUESTS=0:mailto:kanishka@example.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=Heath Ritchie;X-NUM-GUESTS=0:mailto:heath@example.com
CREATED:20170321T194353Z
DESCRIPTION:description goes here\nView your event at https://www.google.co
 m/calendar/event?action=VIEW&eid=NGI2MWoxM3FobDdhZ2dlN3JrNWgwZjM4bXMga2FuaX
 Noa2FAbWFja2V5Y29ycC5jb20&tok=MjAjaGVhdGhAbWFja2V5Y29ycC5jb200MzI5MWExOGUxY
 TQwYjc3ZjE0OTFhNTg4YTRlY2IxOGE0NWNjNjFj&ctz=America/New_York&hl=en.
LAST-MODIFIED:20170321T194353Z
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:test ics parser
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR

--94eb2c0550982f0f67054b42dd9e--
--94eb2c0550982f0f6b054b42dda0
Content-Type: application/ics; name="invite.ics"
Content-Disposition: attachment; filename="invite.ics"
Content-Transfer-Encoding: base64


--94eb2c0550982f0f6b054b42dda0--
|]

-- New Email: Tue Mar 21 20:22:00 UTC 2017
-- emmett@example.com 4332 ubuntu@bbexample.com
-- From emmett@example.com Tue Mar 21 20:22:00 2017
(multipartWithJson :: T.Text) = [s|
Received: from mail-qk0-f181.google.com (mail-qk0-f181.google.com [209.85.220.181])
	  by ip-172-30-0-97.ec2.internal (Postfix) with ESMTPS id 53C8D69A
	  for <ubuntu@bbexample.com>; Tue, 21 Mar 2017 21:33:33 +0000 (UTC)
Received: by mail-qk0-f181.google.com with SMTP id y76so145513382qkb.0
        for <ubuntu@bbexample.com>; Tue, 21 Mar 2017 14:33:33 -0700 (PDT)
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=example-com.20150623.gappssmtp.com; s=20150623;
        h=from:to:subject:date:message-id:mime-version:thread-index
         :content-language;
        bh=XdyGRrfatWfHJpjbcVgwvXn/+KzqkH67agWITktrJT4=;
        b=TJ+arWX2qYHBm9CVui0BFpTb2z52aJoKPgaKmNVE2pR+Vsgk8fgV6OH94QLbI5nQeq
         O3SHDjoZc9uK9IRKu1kn0aJ0frtNtfRnnogMjcRFHVgdhtlO4A4z48VXElY9I72aHM+Z
         UZIDV6WxGwfZFeI++k7HEBNH07OnO8tas1RZwU3AgcHdJKD9tALczHtQdUPWMQjW9YY7
         mUK3aW0S6HgPwA9B3RMVYDz4p2ypRU2/GFtizdnfQB85MrZs096V2rze/2baqZoYA0Y8
         9ihXX750ZsIIiAm8KHF0Wk0iUi2ZoDyz/+oXQpep+9R1XnwpEYs2ew1bbasB0604SrG9
         L6BQ==
X-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=1e100.net; s=20161025;
        h=x-gm-message-state:from:to:subject:date:message-id:mime-version
         :thread-index:content-language;
        bh=XdyGRrfatWfHJpjbcVgwvXn/+KzqkH67agWITktrJT4=;
        b=tnDAeNU5SfRMHPsBjhxTx75rFs1JXy/LnUjG1yq8/2dJO+zZxwiVP4SNsA8PmZTCmz
         2i5qk4Yjc6NpHwULbzM4B3Oj2QQVrSIT5pdxx/I8+lR8Kqdjh9qi5j0WESxEUrjfvRhW
         3ELuNAq0wOTQ/6MZ4IOd+qHEDeLct7w/z7D/Sij+wn/EMl5C0iZjjBqXy1XaWcuZZLEv
         Ot2C9xdgGLsf9EFWJShvlbhA1g6hzb8SKb7rUss9f06Y5HXH0pdH/jAeeXBPa7hMLt0j
         t10Uyr8+FoczO8B82zFQ13EqVzfVxKoyZ5rYtBW1RpTOuCAE6GwMQbbRgykNAmmqMgiP
         uf9g==
X-Gm-Message-State: AFeK/H1O4btD2fkPfYMuKi5TOZ8WPqT/YKQt9DdyhEvaCMM1RFi7emadkHI1G85VdMBqGg==
X-Received: by 10.55.121.134 with SMTP id u128mr35292626qkc.12.1490132012864;
        Tue, 21 Mar 2017 14:33:32 -0700 (PDT)
Received: from MackeyRMSTHINK (50-245-11-246-static.hfc.comcastbusiness.net. [50.245.11.246])
        by smtp.gmail.com with ESMTPSA id a26sm15678770qtb.28.2017.03.21.14.33.31
        for <ubuntu@bbexample.com>
        (version=TLS1 cipher=ECDHE-RSA-AES128-SHA bits=128/128);
        Tue, 21 Mar 2017 14:33:32 -0700 (PDT)
From: "Emmett Manning" <emmett@example.com>
To: <ubuntu@bbexample.com>
Subject: BB Intimation
Date: Tue, 21 Mar 2017 17:33:30 -0400
Message-ID: <019f01d2a28a$c96212c0$5c263840$@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed;
	      boundary="----=_NextPart_000_01A0_01D2A269.425099D0"
X-Mailer: Microsoft Outlook 14.0
Thread-Index: AdKiisjDgOWAW8dST96tGWdFPmVDDQ==
Content-Language: en-us

This is a multipart message in MIME format.

------=_NextPart_000_01A0_01D2A269.425099D0
Content-Type: multipart/alternative;
	      boundary="----=_NextPart_001_01A1_01D2A269.425099D0"


------=_NextPart_001_01A1_01D2A269.425099D0
Content-Type: text/plain;
	      charset="us-ascii"
Content-Transfer-Encoding: 7bit

BB Intimation 


------=_NextPart_001_01A1_01D2A269.425099D0
Content-Type: text/html;
	      charset="us-ascii"
Content-Transfer-Encoding: quoted-printable

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML>
<HEAD>
<META HTTP-EQUIV=3D"Content-Type" CONTENT=3D"text/html; =
charset=3Dus-ascii">
<META NAME=3D"Generator" CONTENT=3D"MS Exchange Server version =
14.02.5004.000">
<TITLE></TITLE>
</HEAD>
<BODY>
<!-- Converted from text/rtf format -->

<P><FONT FACE=3D"Calibri">BB Intimation</FONT>
</P>

</BODY>
</HTML>
------=_NextPart_001_01A1_01D2A269.425099D0--

------=_NextPart_000_01A0_01D2A269.425099D0
Content-Type: text/plain;
	      name="BBOPDetails.txt"
Content-Transfer-Encoding: quoted-printable
Content-Disposition: attachment;
		     filename="BBOPDetails.txt"

{"GUID":"ec5309d0-6b74-477d-9170-7c346a3e0b93","Action":"New","BrokerElig=
ible":true,"Broker":18,"BrokerText":"Advanced Advisor =
Group,LLC","EventType":25,"EventTypeText":"Event type =
21","ooSubject":"This is the subject","ooLocation":"This is the location =
","ooBodyText":"Whats this =
field?","ooStart":"2017-04-01T01:00:00","ooEnd":"2017-04-02T01:30:00","oo=
Recepients":["kanishka@example.com"]}

------=_NextPart_000_01A0_01D2A269.425099D0--
|]

