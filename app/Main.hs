{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (optional)
import Control.Exception
import Control.Lens (makeLenses, to, (%~), (&), (.~), (<&>), (?~), (^.))
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text as Text
import Data.Text.IO as Text
import GHC.Stack
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT
import Options.Applicative.Builder
  ( argument,
    auto,
    command,
    eitherReader,
    help,
    idm,
    info,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    showHelpOnError,
    str,
    strOption,
    subparser,
  )
import Options.Applicative.Extra (customExecParser)
import Options.Applicative.Types (Parser)
import System.Directory (getDirectoryContents)
import System.Environment
import System.FilePath
import System.IO (stderr)
import System.Process
import Text.Hamlet.XML as XML
import Text.XML as XML
import Text.XML.Cursor (($/), ($|), (&/), (&//), (&|))
import qualified Text.XML.Cursor as XML
import URI.ByteString (URI)
import qualified URI.ByteString as URI

----------------------------------------------------------------------
-- orphans

instance WT.Postable XML.Document where
  postPayload doc = WT.postPayload $ W.Raw "application/xml charset=utf-8" (H.RequestBodyLBS . cs $ XML.renderText XML.def doc)

----------------------------------------------------------------------
-- types

data Credentials = Credentials {_credLogin :: Text, _credPassword :: Text}

data CouldNotParseCredentials = CouldNotParseCredentials ([[Text]], [[Text]])
  deriving (Eq, Show)

instance Exception CouldNotParseCredentials

data VCard = VCard {_vcUrlPath :: Text, _vcFileName :: FilePath, _vcEtag :: Text, _vcCard :: Text}
  deriving (Eq, Ord, Show)

newtype AddressBook = AddressBook {unAddressBook :: Set.Set VCard}
  deriving (Eq, Show)

makeLenses ''Credentials
makeLenses ''VCard

----------------------------------------------------------------------
-- methods

getDefCredsFile :: IO FilePath
getDefCredsFile = getEnv "HOME" <&> (</> ".davfs2" </> "secrets")

defCredsEntryPrefix :: Text
defCredsEntryPrefix = "https://"

getCredentials :: CliCtx -> IO Credentials
getCredentials ctx = do
  raw :: [Text] <- Text.lines <$> (Text.readFile $ cliCredsFile ctx)
  let credentials = Text.words <$> Prelude.filter (cliCredsEntryPrefix ctx `Text.isPrefixOf`) raw
  case credentials of
    [[_, login, password]] -> pure $ Credentials login password
    bad -> throwIO $ CouldNotParseCredentials (credentials, bad)

getAddressBook :: Credentials -> URI -> IO AddressBook
getAddressBook creds url = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("Depth", "1")]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <- W.customPayloadMethodWith "REPORT" options (cs $ URI.serializeURIRef' url) getAddressBookQuery
  pure . parseAddressBook . XML.parseLBS_ XML.def . H.responseBody $ resp

getAddressBookQuery :: XML.Document
getAddressBookQuery = XML.Document (XML.Prologue [] Nothing []) root []
  where
    root =
      XML.Element
        "C:addressbook-query"
        (Map.fromList [("xmlns:D", "DAV:"), ("xmlns:C", "urn:ietf:params:xml:ns:carddav")])
        [xml|
<D:prop>
  <D:getetag>
  <C:address-data>
|]

parseAddressBook :: HasCallStack => XML.Document -> AddressBook
parseAddressBook doc = parseDoc $ XML.fromDocument doc
  where
    parseDoc :: XML.Cursor -> AddressBook
    parseDoc = AddressBook . Set.fromList . ($/ XML.element "{DAV:}response" &| parseResponse)

    parseResponse :: XML.Cursor -> VCard
    parseResponse cursor = VCard {..}
      where
        nonnull :: Text -> Text
        nonnull "" = error $ show cursor
        nonnull good = good

        _vcUrlPath = nonnull . mconcat $ (cursor $/ XML.element "{DAV:}href" &/ XML.content)
        _vcFileName = cs . nonnull . cs . takeFileName . cs $ _vcUrlPath
        _vcEtag =
          nonnull . mconcat $
            ( cursor $/ XML.element "{DAV:}propstat"
                &/ XML.element "{DAV:}prop"
                &/ XML.element "{DAV:}getetag"
                &/ XML.content
            )
        _vcCard =
          nonnull . mconcat $
            ( cursor $/ XML.element "{DAV:}propstat"
                &/ XML.element "{DAV:}prop"
                &/ XML.element "{urn:ietf:params:xml:ns:carddav}address-data"
                &/ XML.content
            )

putVCard :: HasCallStack => Credentials -> URI -> VCard -> IO ()
putVCard creds url vcard = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("If-Match", cs $ vcard ^. vcEtag)]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <-
    W.customPayloadMethodWith
      "PUT"
      options
      (cs $ URI.serializeURIRef' url)
      (W.Raw "text/vcard; charset=utf-8" (H.RequestBodyLBS (cs (vcard ^. vcCard))))
  unless (resp ^. W.responseStatus . W.statusCode < 400) $ do
    error $ "could not update vcard: " <> show resp

saveAddressBook :: HasCallStack => FilePath -> AddressBook -> IO ()
saveAddressBook dir = mapM_ (saveVCard dir) . Set.toList . unAddressBook

saveVCard :: HasCallStack => FilePath -> VCard -> IO ()
saveVCard dir card = do
  Text.writeFile (dir </> cs (card ^. vcFileName)) (card ^. vcCard)
  Text.writeFile (dir </> takeBaseName (cs (card ^. vcFileName)) <.> "etag") (card ^. vcEtag)

loadAddressBook :: HasCallStack => String -> FilePath -> IO AddressBook
loadAddressBook addrBookURL dir = do
  vcfs <- getDirectoryContents dir <&> List.filter ((== ".vcf") . takeExtension)
  AddressBook . Set.fromList <$> (loadVCard addrBookURL dir `mapM` vcfs)

loadVCard :: HasCallStack => String -> FilePath -> FilePath -> IO VCard
loadVCard addrBookURL addrBookDir _vcFileName = do
  let _vcUrlPath = cs $ addrBookURL </> _vcFileName
  _vcCard <- Text.readFile (addrBookDir </> _vcFileName)
  _vcEtag <- Text.readFile (addrBookDir </> takeBaseName _vcFileName <.> "etag")
  pure $ VCard {..}

----------------------------------------------------------------------
-- cli, main

data CliCtx = CliCtx
  { cliCredsFile :: FilePath,
    cliCredsEntryPrefix :: Text,
    cliCommand :: Command
  }
  deriving (Eq, Show)

data Command
  = GetContacts URI FilePath
  | PutContacts FilePath URI
  deriving (Eq, Show)

parseCliCtx :: FilePath -> Parser CliCtx
parseCliCtx defCredsFile = do
  cliCredsFile <-
    strOptionWithDefault
      "credentials-file"
      "PATH"
      "Config file that contains login and password"
      (cs defCredsFile)
  cliCredsEntryPrefix <-
    strOptionWithDefault
      "credentials-url"
      "String"
      "Unique prefix of the line in the credentials file that contains login and password"
      defCredsEntryPrefix
  cliCommand <- parseCmd
  pure CliCtx {..}

strOptionWithDefault :: (IsString a, Show a) => String -> String -> String -> a -> Parser a
strOptionWithDefault long_ metavar_ desc_ def_ =
  fromMaybe def_ <$> optional (strOption (long long_ <> metavar metavar_ <> help (desc_ <> " default: " <> show def_)))

parseCmd :: Parser Command
parseCmd =
  subparser $
    mconcat
      [ command
          "get-contacts"
          ( info
              (GetContacts <$> cliAddressBookUrl <*> cliAddressBookDir)
              ( progDesc
                  "Download an addressbook from a given URL to a given local directory. \
                  \The directory should be empty except for previously downloaded data from \
                  \the same address book.  It will be populated (or updated) with, for each \
                  \vCard in the address book, a file `<uuid>.vcf` and a vile `<uuid>.etag`. \
                  \The uuid is extracted from the vCard url.  The etag will be used when you \
                  \put contacts to decide whether a vcf file is already up to date on the \
                  \server."
              )
          ),
        command
          "put-contacts"
          ( info
              (PutContacts <$> cliAddressBookDir <*> cliAddressBookUrl)
              ( progDesc
                  "Upload all vCards in the local directory which are different on the server. \
                  \To be more specific: download contacts; go through all local vCards; update \
                  \those that do not exist on the server, or exist, but have different etags \
                  \locally and remotely."
              )
          )
      ]

cliAddressBookUrl :: Parser URI
cliAddressBookUrl = argument (eitherReader (\raw -> first (mkerr raw) $ prs raw)) (metavar "URI")
  where
    prs = URI.parseURI URI.strictURIParserOptions . cs
    mkerr raw err = "could not parse URI: " <> show (raw, err)

cliAddressBookDir :: Parser FilePath
cliAddressBookDir = argument str (metavar "FilePath")

main :: IO ()
main = do
  defCredsFile <- getDefCredsFile
  ctx <- customExecParser (prefs showHelpOnError) (info (parseCliCtx defCredsFile) idm)
  creds <- getCredentials ctx

  case cliCommand ctx of
    GetContacts url dir -> doGetContacts creds url dir
    PutContacts dir url -> doPutContacts creds dir url

doGetContacts :: Credentials -> URI -> FilePath -> IO ()
doGetContacts creds url dir = do
  system $ "mkdir -p " <> show dir
  vcardsFromUrl <- getAddressBook creds url
  saveAddressBook dir vcardsFromUrl
  vcardsFromDir <- loadAddressBook (url ^. URI.pathL . to cs) dir
  unless (vcardsFromUrl == vcardsFromDir) $
    error "saveAddressBook / loadAddressBook failed roundtrip check."

doPutContacts :: Credentials -> FilePath -> URI -> IO ()
doPutContacts creds dir url =
  do
    vcardsFromDir :: [VCard] <-
      loadAddressBook (url ^. URI.pathL . to cs) dir <&> (Set.toList . unAddressBook)

    let setToMap :: Ord k => (a -> k) -> Set a -> Map k a
        setToMap mkKey = Map.fromList . fmap (\a -> (mkKey a, a)) . Set.toList
    vcardsFromUrl :: Map Text VCard <-
      getAddressBook creds url <&> (setToMap (^. vcEtag) . unAddressBook)

    let go :: VCard -> IO ()
        go vcard = do
          Map.lookup (vcard ^. vcEtag) vcardsFromUrl & \case
            Nothing -> do
              hPutStrLn stderr $ "sending " <> cs (vcard ^. vcFileName) <> "."
              putVCard creds (url & URI.pathL .~ cs (vcard ^. vcUrlPath)) vcard
            Just _ -> pure ()
    go `mapM_` vcardsFromDir

    -- store data from server, including etags and possible mutations.
    doGetContacts creds url dir
