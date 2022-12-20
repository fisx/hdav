{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Samples (sample) where

import Data.Text (Text)

sample :: Int -> Text
sample 0 =
  "BEGIN:VCARD\n\
  \VERSION:4.0\n\
  \UID:0c2581e0-b81d-4c9d-851e-20eba424e781\n\
  \REV;VALUE=DATE-AND-OR-TIME:20200328T200100Z\n\
  \FN:Vorname Nachname\n\
  \ADR;TYPE=HOME:;;Weg 1;Stadt;;12345;\n\
  \ADR;TYPE=WORK:;;Auchweg 1;Auchstadt;;94321;\n\
  \EMAIL;TYPE=HOME:email@example.com\n\
  \TEL;TYPE=HOME:123-4/51\n\
  \NOTE:Wohneinheit 9.12F\n\
  \CATEGORIES:Test,Test2\n\
  \END:VCARD\n"
sample 1 =
  "BEGIN:VCARD\n\
  \VERSION:3.0\n\
  \PRODID;VALUE=TEXT:-//Sabre//Sabre VObject 4.3.0//EN\n\
  \UID:3ea76966-d5eb-11eb-9564-6743f72e2c6d\n\
  \REV:20210309T121349Z\n\
  \FN;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:=F6=F6=F6\n\
  \=F6=F6=F6=F6=F6=F6=F6=F6=F6=F6=F6=F6=F6\n\
  \ITEM4.ADR;TYPE=HOME:;;;;;;\n\
  \EMAIL;TYPE=WORK:me@example.com\n\
  \TEL;TYPE=\"HOME,VOICE\";VALUE=UNKNOWN:030-1874/8751\n\
  \NOTE:\n\
  \CATEGORIES:Conplan,A,AG_Something_Or_\n\
  \ Other,WEG\n\
  \ORG:Evil Corp.\n\
  \END:VCARD\n\
  \"
sample 2 =
  "BEGIN:VCARD\n\
  \VERSION:4.0\n\
  \UID:0c2581e0-b81d-4c9d-851e-20eba424e781\n\
  \FN:Vorname Nachname\n\
  \EMAIL;TYPE=HOME:email@example.com\n\
  \ADR;TYPE=HOME:;;Weg 1;Stadt;;12345;\n\
  \ADR;TYPE=WORK:;;Auchweg 1;Auchstadt;;94321;\n\
  \CATEGORIES:Test\n\
  \END:VCARD\n"
sample 3 =
  "BEGIN:VCARD\n\
  \EMAIL;TYPE=HOME:email@example.com\n\
  \TEL;TYPE=HOME:123-4/51\n\
  \NOTE:Wohneinheit 9.12F\n\
  \CATEGORIES:Test,Test2\n\
  \END:VCARD\n"
