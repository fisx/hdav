{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Test.Data.DAV.ValueSpec (spec) where

import Control.Monad (forM_)
import Data.DAV.Util (Parser, nt)
import Data.DAV.Value
import Data.Either (isRight)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Samples
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

checkExample :: (HasCallStack, Eq a, Show a) => Parser a -> Text -> Either Text a -> Spec
checkExample prs txt val = it (show txt) $ nt (runParser prs mempty txt) `shouldBe` val

spec :: Spec
spec = do
  it "fieldMod" . property $ \(fldmod :: FieldMod) ->
    let rendered = renderFieldMod fldmod
     in counterexample (show rendered) $ do
          nt (runParser (fieldModParser) mempty rendered) `shouldBe` Right fldmod

  describe "field (examples)" $ do
    checkExample fieldParser "F:" $ Right (Field (FieldName "F") Nothing Nothing [] [])
    checkExample fieldParser "F:" $ Right (Field (FieldName "F") Nothing Nothing [] [])
    checkExample fieldParser "F:aef" $ Right (Field (FieldName "F") Nothing Nothing [] ["aef"])
    checkExample fieldParser "F:aef;wef" $ Right (Field (FieldName "F") Nothing Nothing [] ["aef", "wef"])
    checkExample fieldParser "F;wef=aef:aef;wef" $ Right (Field (FieldName "F") Nothing Nothing [FieldMod "wef" "aef"] ["aef", "wef"])
    checkExample fieldParser "ITEM0.a:" $ Right (Field (FieldName "a") (Just 0) Nothing [] [])
    checkExample fieldParser "ITEM0.a:wef" $ Right (Field (FieldName "a") (Just 0) Nothing [] ["wef"])
    checkExample fieldParser "ITEM0.a;xjm=cPnl;BT=h;Gv=xT:TFPO;gwoM;_G" $
      Right (Field (FieldName "a") (Just 0) Nothing [FieldMod "xjm" "cPnl", FieldMod "BT" "h", FieldMod "Gv" "xT"] ["TFPO", "gwoM", "_G"])

  it "field" . property $ \(fld :: Field) ->
    let rendered = renderField fld
     in counterexample (show rendered) $ do
          nt (runParser fieldParser mempty rendered) `shouldBe` Right fld

  describe "multi-lines" $ do
    checkExample multiLineParser "" $ Right [""]
    checkExample multiLineParser "wef" $ Right ["wef"]
    forM_ ["\n", "\r\n"] $ \sep -> do
      checkExample multiLineParser (T.intercalate sep ["wef", ""]) $ Right ["wef", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", " iou", ""]) $ Right ["wefiou", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", "=iou", ""]) $ Right ["wef=iou", ""] -- (for quoted-printable line breaks)
      checkExample multiLineParser (T.intercalate sep ["wef", "   iou", " gnnnn"]) $ Right ["wefiougnnnn", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", "iou", ""]) $ Right ["wef", "iou", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", "", "iou", ""]) $ Right ["wef", "", "iou", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", "", "", "iou", ""]) $ Right ["wef", "", "", "iou", ""]
      checkExample multiLineParser (T.intercalate sep ["wef", "", "", "", "iou", ""]) $ Right ["wef", "", "", "", "iou", ""]

  describe "samples" $ do
    describe "parseValue" $ do
      forM_ [0 .. 3] $ \i ->
        it (show i) $
          parseValue (Samples.sample i) `shouldSatisfy` isRight
    describe "parseValues" $ do
      forM_ [0 .. 3] $ \i ->
        it (show i) $
          parseValues (Samples.sample i) `shouldSatisfy` isRight

  it "value" . property $ \(val :: Value) ->
    let rendered = renderValue val
     in counterexample (cs rendered) $ do
          parseValue rendered `shouldBe` Right val

  it "values (0..3)" . property . forAll (scale (`div` 3) arbitrary) $ \(take 5 -> vals) -> do
    let rendered = renderValues vals
    counterexample (show rendered) $ do
      T.length rendered `shouldNotBe` -1
      parseValues rendered `shouldBe` Right vals

  it "monoid" $ do
    let prs :: Text -> Value
        prs t = case parseValue t of Right v -> v

        shw :: Value -> String
        shw = cs . renderValue

        want :: Value
        want =
          prs $
            "BEGIN:VCARD\n\
            \VERSION:4.0\n\
            \UID:0c2581e0-b81d-4c9d-851e-20eba424e781\n\
            \FN:Vorname Nachname\n\
            \EMAIL;TYPE=HOME:email@example.com\n\
            \ADR;TYPE=HOME:;;Weg 1;Stadt;;12345;\n\
            \ADR;TYPE=WORK:;;Auchweg 1;Auchstadt;;94321;\n\
            \TEL;TYPE=HOME:123-4/51\n\
            \NOTE:Wohneinheit 9.12F\n\
            \CATEGORIES:Test,Test2\n\
            \END:VCARD\n"

        have :: Value
        have = (prs $ Samples.sample 2) <> (prs $ Samples.sample 3)

    counterexample (shw have) (have `shouldBe` want)
