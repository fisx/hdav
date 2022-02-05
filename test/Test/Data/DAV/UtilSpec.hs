module Test.Data.DAV.UtilSpec (spec) where

import Data.DAV.Util
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "int (examples)" $ do
    checkExample int "" $ Left "1:1:\n  |\n1 | <empty line>\n  | ^\nunexpected end of input\nexpecting int\n"
    checkExample int "1" $ Right 1
    checkExample int "14" $ Right 14
    checkExample int "14478" $ Right 14478
    checkExample int "14478--39" $ Right 14478

  describe "segment (examples)" $ do
    checkExample segment "" $ Left "1:1:\n  |\n1 | <empty line>\n  | ^\nsegment\n"
    checkExample (many segment) "" $ Right []
    checkExample (many segment) "a" $ Right ["a"]
    checkExample (many segment) "awef;yt" $ Right ["awef", "yt"]
    checkExample (many segment) "awef;yt;" $ Right ["awef", "yt"]
    checkExample (many segment) "awef;yt\nwef" $ Right ["awef", "yt"]
  where
    checkExample :: (HasCallStack, Eq a, Show a) => Parser a -> Text -> Either Text a -> Spec
    checkExample prs txt val = it (show txt) $ nt (runParser prs mempty txt) `shouldBe` val
