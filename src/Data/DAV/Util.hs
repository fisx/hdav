module Data.DAV.Util
  ( Parser,
    chunk_,
    chunkCI,
    int,
    segment,
    nt,
  )
where

import Control.Lens ((<&>))
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.CaseInsensitive as CI
import Data.Char (isDigit, ord)
import Data.Functor (($>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec

----------------------------------------------------------------------

type Parser = Parsec Void Text

chunk_ :: (Stream u, Ord s) => Tokens u -> Parsec s u ()
chunk_ = void . chunk

chunkCI :: (Stream u, Ord s, Tokens u ~ Text) => Text -> Parsec s u (CI.CI Text)
chunkCI txt = do
  txt' <- CI.mk <$> takeP Nothing (T.length txt)
  if txt' == CI.mk txt then pure txt' else fail ""

int :: (Stream u, Ord s, Token u ~ Char, Tokens u ~ Text) => Parsec s u Int
int = (takeWhile1P Nothing isDigit <&> readInt 0 . cs) <?> "int"
  where
    readInt :: Int -> [Char] -> Int
    readInt i "" = i
    readInt i (x : xs)
      | x `elem` ['0' .. '9'] = readInt (i * 10 + (ord x - ord '0')) xs
      | otherwise = error "impossible"

-- See test cases for what this combinator does.
-- FUTUREWORK: parameterize over ';' and '\n'?
segment :: (Stream u, Ord s, Token u ~ Char, Tokens u ~ Text) => Parsec s u Text
segment = do
  let terminates = (`elem` (";\n" :: [Char]))
  word :: Text <- takeWhileP (Just "segment") (not . terminates)
  continue :: Bool <- try (single ';' $> True) <|> pure False
  if T.null word && not continue
    then fail "segment"
    else pure word

-- | Natural transformation from megaparsec error into more convenient error.
nt :: Either (ParseErrorBundle Text Void) a -> forall m. MonadError Text m => m a
nt = either (throwError . cs . errorBundlePretty) pure
