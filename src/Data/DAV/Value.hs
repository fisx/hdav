module Data.DAV.Value
  ( -- * types
    Value (..),
    fromValue,
    FieldName (..),
    fromFieldName,
    Field (..),
    fieldName,
    fieldItem,
    fieldGroup,
    fieldMods,
    fieldContents,
    FieldMod (..),
    fmodKey,
    fmodVal,

    -- * constructors
    mkValue,
    mkField,
    mkSimpleField,

    -- * canonicalize
    canonicalize,
    multiLineParser,

    -- * parse
    parseValue,
    parseValues,
    fieldParser,
    fieldModParser,

    -- * render
    renderValue,
    renderValues,
    renderField,
    renderFieldMod,
    renderFieldContents,
  )
where

import Control.Lens (makeLenses, view, (.~), (^.))
import Control.Monad (void, (>=>))
import Control.Monad.Except (MonadError)
import Data.CaseInsensitive (CI, mk, original)
import Data.Char (isSpace)
import Data.DAV.Util
import Data.List (foldl', nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Test.QuickCheck as QC
import Text.Megaparsec as MP
import Text.Megaparsec.Char (newline)

----------------------------------------------------------------------
-- types

newtype Value = Value {_fromValue :: Map FieldName (Set Field)}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Monoid)

instance Semigroup Value where
  (Value a) <> (Value b) = Value (b <> a) -- overwrite fields from first arg with fields from second.

newtype FieldName = FieldName {_fromFieldName :: CI Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

data Field = Field
  { _fieldName :: FieldName,
    _fieldItem :: Maybe Int,
    _fieldGroup :: Maybe Int,
    _fieldMods :: [FieldMod],
    _fieldContents :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)

data FieldMod = FieldMod
  { _fmodKey :: CI Text,
    _fmodVal :: CI Text
  }
  deriving stock (Eq, Ord, Show, Generic)

----------------------------------------------------------------------
-- lenses

makeLenses ''Value
makeLenses ''FieldName
makeLenses ''Field
makeLenses ''FieldMod

----------------------------------------------------------------------
-- constructors

mkValue :: [Field] -> Value
mkValue = Value . aggregate
  where
    aggregate :: [Field] -> Map FieldName (Set Field)
    aggregate = foldl' (flip step) mempty

    step :: Field -> Map FieldName (Set Field) -> Map FieldName (Set Field)
    step field = Map.alter (Just . maybe (Set.singleton field) (Set.insert field)) (field ^. fieldName)

mkField :: FieldName -> [FieldMod] -> [Text] -> Field
mkField fname fmods contents = Field fname Nothing Nothing fmods contents

mkSimpleField :: FieldName -> [Text] -> Field
mkSimpleField fname = mkField fname []

----------------------------------------------------------------------
-- arbitrary

instance Arbitrary Value where
  arbitrary = Value . sanitizeValue <$> arbitrary
  shrink (Value mp) = filter (`notElem` [Value mempty, Value mp]) . nub $ Value . sanitizeValue <$> shrink mp

sanitizeValue :: Map FieldName (Set Field) -> Map FieldName (Set Field)
sanitizeValue = Map.filter (not . null) . Map.mapWithKey (\n -> Set.map (fieldName .~ n))

instance Arbitrary FieldName where
  arbitrary = FieldName . mk . fromSimpleText <$> arbitrary

instance Arbitrary Field where
  arbitrary =
    Field
      <$> arbitrary
      <*> simpleMaybeInt
      <*> simpleMaybeInt
      <*> (vector =<< chooseInt (0, 5))
      <*> (fmap (cs . fromSimpleText) <$> (vector =<< chooseInt (0, 5)))
    where
      simpleMaybeInt = QC.oneof [pure Nothing, Just <$> QC.elements [0 .. 15]]
  shrink (Field n i g mods cont) =
    [ Field n i g mods [],
      Field n i g [] cont,
      Field n i Nothing mods cont,
      Field n Nothing g mods cont
    ]

instance Arbitrary FieldMod where
  arbitrary = FieldMod <$> (mk . fromSimpleText <$> arbitrary) <*> (mk . fromSimpleText <$> arbitrary)

newtype SimpleText = SimpleText {fromSimpleText :: Text}
  deriving stock (Eq, Ord, Show)

instance Arbitrary SimpleText where
  -- FUTUREWORK: this is just a guess; adjust as we collect evidence from the field.
  arbitrary = SimpleText . cs <$> listOf1 (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> "_-+"))
  shrink (SimpleText t) = SimpleText . cs <$> shrink (cs @Text @String t)

----------------------------------------------------------------------
-- canonicalize

-- Change "\r\n" to "\n"; remove line breaks from multi-line fields.
-- FUTUREWORK: remove quoted-printable and other encoding artifacts.
canonicalize :: forall m. MonadError Text m => Text -> m Text
canonicalize = fmap T.unlines . nt . runParser multiLineParser mempty

multiLineParser :: Parser [Text]
multiLineParser = many' parseMultiLine
  where
    many' :: Parser (a, Bool) -> Parser [a]
    many' prs = do
      (a, keepGoing) <- prs
      (a :) <$> if keepGoing then many' prs else pure []

    parseMultiLine :: Parser (Text, Bool)
    parseMultiLine = do
      (seg, keepGoing) <- parseSegment
      segs <- many $ continues >> (fst <$> parseSegment)
      pure (mconcat $ seg : segs, keepGoing)

    parseSegment :: Parser (Text, Bool)
    parseSegment = do
      seg <- takeWhileP Nothing (`notElem` ("\r\n" :: [Char]))
      keepGoing <- (const True <$> linebreak) <|> (const False <$> eof)
      pure (seg, keepGoing)

    linebreak :: Parser ()
    linebreak = (void . try . chunk $ "\n") <|> (void . try . chunk $ "\r\n")

    continues :: Parser ()
    continues = leadingSpace <|> leadingQuotedPrintableMark

    leadingSpace :: Parser ()
    leadingSpace = void $ takeWhile1P Nothing (`elem` (" \t\f\v" :: [Char]))

    leadingQuotedPrintableMark :: Parser ()
    leadingQuotedPrintableMark = void . lookAhead . chunk $ "="

----------------------------------------------------------------------
-- parser

parseValues :: forall m. MonadError Text m => Text -> m [Value]
parseValues = canonicalize >=> (nt . runParser (many valueParser) mempty)

parseValue :: forall m. MonadError Text m => Text -> m Value
parseValue = canonicalize >=> (nt . runParser valueParser mempty)

valueParser :: Parser Value
valueParser = try $ do
  many (satisfy isSpace)
  chunk_ "BEGIN:VCARD" >> newline
  mkValue <$> manyTill (fieldParser <* newline) (chunk_ "END:VCARD" >> newline)

fieldParser :: Parser Field
fieldParser = try $ do
  _fieldItem <- do
    (MP.try (Just <$> (chunkCI "ITEM" >> int <* single '.')) <|> pure Nothing)
      <?> "fieldItem"
  _fieldGroup <- do
    (MP.try (Just <$> (chunkCI "GROUP" >> int <* single '.')) <|> pure Nothing)
      <?> "fieldGroup"
  _fieldName <- do
    (FieldName . mk <$> takeWhile1P Nothing (`notElem` [':', ';']))
      <?> "fieldName"
  _fieldMods <- do
    (many (single ';' >> fieldModParser) <* single ':')
      <?> "fieldMods"
  _fieldContents <- do
    -- FUTUREWORK: `strip <$$>`, but that messes with the roundtrip property
    many segment
      <?> "fieldContents"
  pure Field {..}

fieldModParser :: Parser FieldMod
fieldModParser = do
  key :: Text <- takeWhile1P (Just "fieldmod key") (/= '=') <* single '='
  val :: Text <- takeWhile1P (Just "fieldmod value") (`notElem` [':', ';'])
  pure $ FieldMod (mk key) (mk val)

----------------------------------------------------------------------
-- render

renderValues :: [Value] -> Text
renderValues = T.unlines . fmap renderValue

renderValue :: Value -> Text
renderValue (Value fields) =
  T.unlines $
    ["BEGIN:VCARD"]
      <> (renderField <$> mconcat (Set.toAscList <$> Map.elems fields))
      <> ["END:VCARD"]

renderField :: Field -> Text
renderField field =
  mconcat . catMaybes $
    [ (("ITEM" <>) . (<> ".") . cs . show) <$> (field ^. fieldItem),
      (("GROUP" <>) . (<> ".") . cs . show) <$> (field ^. fieldGroup),
      Just . original . view fromFieldName $ field ^. fieldName,
      case renderFieldMod <$> field ^. fieldMods of
        [] -> Nothing
        mods@(_ : _) -> Just . (";" <>) . T.intercalate ";" $ mods,
      Just ":",
      Just . renderFieldContents $ field ^. fieldContents
    ]

renderFieldMod :: FieldMod -> Text
renderFieldMod (FieldMod key val) = original key <> "=" <> original val

renderFieldContents :: [Text] -> Text
renderFieldContents = T.intercalate ";"
