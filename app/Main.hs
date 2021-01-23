{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Main (main) where

import Control.Exception
import Control.Lens (makeLenses, (%~), (&), (.~), (<&>), (?~), (^.))
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text as Text
import Data.Text.IO as Text
import GHC.Stack
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT
import System.Directory (getDirectoryContents)
import System.Environment
import System.FilePath
import System.Process
import Text.Hamlet.XML as XML
import Text.XML as XML
import Text.XML.Cursor (($/), ($|), (&/), (&//), (&|))
import qualified Text.XML.Cursor as XML

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

newtype URL = URL {unURL :: Text}
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

data VCard = VCard {_vcUrl :: URL, _vcFileName :: FilePath, _vcEtag :: Text, _vcCard :: Text}
  deriving (Eq, Ord, Show)

newtype AddressBook = AddressBook {unAddressBook :: Set.Set VCard}
  deriving (Eq, Show)

makeLenses ''Credentials
makeLenses ''VCard

----------------------------------------------------------------------
-- methods

getCredentialsFile :: IO FilePath
getCredentialsFile = getEnv "HOME" <&> (</> ".davfs2" </> "secrets")

getCredentials :: URL -> IO Credentials
getCredentials url = do
  raw :: [Text] <- Text.lines <$> (Text.readFile =<< getCredentialsFile)
  let credentials = Prelude.filter ([unURL url] `List.isPrefixOf`) (Text.words <$> raw)
  case credentials of
    [[_, login, password]] -> pure $ Credentials login password
    bad -> throwIO $ CouldNotParseCredentials (credentials, bad)

getAddressBook :: Credentials -> URL -> IO AddressBook
getAddressBook creds url = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("Depth", "1")]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <- W.customPayloadMethodWith "REPORT" options (cs $ unURL url) getAddressBookQuery
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

        _vcUrl = URL . nonnull . mconcat $ (cursor $/ XML.element "{DAV:}href" &/ XML.content)
        _vcFileName = cs . nonnull . cs . takeFileName . cs . unURL $ _vcUrl
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

saveAddressBook :: HasCallStack => FilePath -> AddressBook -> IO ()
saveAddressBook dir = mapM_ (saveVCard dir) . Set.toList . unAddressBook

saveVCard :: HasCallStack => FilePath -> VCard -> IO ()
saveVCard dir card = do
  Text.writeFile (dir </> cs (card ^. vcFileName)) (card ^. vcCard)
  Text.writeFile (dir </> takeBaseName (cs (card ^. vcFileName)) <.> "etag") (card ^. vcEtag)

loadAddressBook :: HasCallStack => URL -> FilePath -> IO AddressBook
loadAddressBook addrBookURL dir = do
  vcfs <- getDirectoryContents dir <&> List.filter ((== ".vcf") . takeExtension)
  AddressBook . Set.fromList <$> (loadVCard addrBookURL dir `mapM` vcfs)

loadVCard :: HasCallStack => URL -> FilePath -> FilePath -> IO VCard
loadVCard addrBookURL addrBookDir _vcFileName = do
  let _vcUrl = URL . cs $ cs (unURL addrBookURL) </> _vcFileName
  _vcCard <- Text.readFile (addrBookDir </> _vcFileName)
  _vcEtag <- Text.readFile (addrBookDir </> takeBaseName _vcFileName <.> "etag")
  pure $ VCard {..}

----------------------------------------------------------------------
-- hacky stuff, main

getContext :: IO (URL, Text, Text, FilePath)
getContext = do
  args <- getArgs
  case args of
    [ credentialsHost, -- the line in the secrets file starts with this string
      addressbookURLHost, -- eg., https://something.other
      addressbookURLPath, -- eg., /remote.php/dav/addressbooks/users/me.self/contacts.me.self/
      -- you can get `addressbookURL*` from the nextcloud UI in the contact settings
      addressbookDir -- location in the local file system (should be an otherwise empty directory)
      ] -> do
        () <- assert (Prelude.last addressbookURLHost /= '/') $ pure ()
        () <- assert (Prelude.head addressbookURLPath == '/') $ pure ()
        system $ "mkdir -p " <> addressbookDir
        pure (URL $ cs credentialsHost, cs addressbookURLHost, cs addressbookURLPath, cs addressbookDir)
    bad -> error $ show bad

main :: IO ()
main = do
  (credentialsHost, addressbookURLHost, addressbookURLPath, addressbookDir) <- getContext
  creds <- getCredentials credentialsHost
  vcards <- getAddressBook creds (URL $ addressbookURLHost <> addressbookURLPath)
  saveAddressBook addressbookDir vcards
  vcards' <- loadAddressBook (URL addressbookURLPath) addressbookDir
  () <- assert (vcards == vcards') $ pure ()
  pure ()
