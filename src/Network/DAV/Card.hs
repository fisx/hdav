module Network.DAV.Card
  ( VCard (VCard),
    vcUrlPath,
    vcFileName,
    vcEtag,
    vcCard,
    AddressBook (AddressBook, unAddressBook),
    getAddressBook,
    getAddressBookQuery,
    parseAddressBook,
    putVCard,
    saveAddressBook,
    saveVCard,
    loadAddressBook,
    loadVCard,
    doGetContacts,
    doPutContacts,
  )
where

import Control.Lens (makeLenses, to, (%~), (&), (.~), (<&>), (?~), (^.))
import Control.Monad (unless)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text as Text
import Data.Text.IO as Text
import GHC.Stack (HasCallStack)
import Network.DAV.Util
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import System.Directory (getDirectoryContents)
import System.FilePath
import System.IO (stderr)
import System.Process
import Text.Hamlet.XML as XML
import Text.XML as XML
import Text.XML.Cursor (($/), (&/), (&|))
import qualified Text.XML.Cursor as XML
import URI.ByteString (URI)
import qualified URI.ByteString as URI

----------------------------------------------------------------------
-- types

data VCard = VCard {_vcUrlPath :: Text, _vcFileName :: FilePath, _vcEtag :: Text, _vcCard :: Text}
  deriving stock (Eq, Ord, Show, Read)

newtype AddressBook = AddressBook {unAddressBook :: Set.Set VCard}
  deriving stock (Eq, Show, Read)

makeLenses ''VCard

----------------------------------------------------------------------
-- getAddressBook

getAddressBook :: Credentials -> URI -> IO AddressBook
getAddressBook creds url = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("Depth", "1")]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <-
    W.customPayloadMethodWith
      "REPORT"
      options
      (cs $ URI.serializeURIRef' url)
      (payloadFromXML getAddressBookQuery)
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

----------------------------------------------------------------------
-- putVCard

-- | @setIfMatchHeader == True@ means update; @setIfMatchHeader == False@ means create.
putVCard :: HasCallStack => Bool -> Credentials -> URI -> VCard -> IO ()
putVCard setIfMatchHeader creds url vcard = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("If-Match", cs $ vcard ^. vcEtag) | setIfMatchHeader]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <-
    W.customPayloadMethodWith
      "PUT"
      options
      (cs $ URI.serializeURIRef' url)
      (W.Raw "text/vcard; charset=utf-8" (H.RequestBodyLBS (cs (vcard ^. vcCard))))
  unless (resp ^. W.responseStatus . W.statusCode < 400) $ do
    error $ "could not update vcard: " <> show resp

----------------------------------------------------------------------
-- local storage

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
-- cli primitives

doGetContacts :: Credentials -> URI -> FilePath -> IO ()
doGetContacts creds url dir = do
  system $ "rm -rf " <> show dir
  system $ "mkdir -p " <> show dir
  vcardsFromUrl <- getAddressBook creds url
  saveAddressBook dir vcardsFromUrl
  vcardsFromDir <- loadAddressBook (url ^. URI.pathL . to cs) dir
  unless (vcardsFromUrl == vcardsFromDir) $
    error "saveAddressBook / loadAddressBook failed roundtrip check."

doPutContacts :: Credentials -> FilePath -> URI -> IO ()
doPutContacts creds dir url = do
  -- local address data
  vcardsFromDir :: [VCard] <-
    loadAddressBook (url ^. URI.pathL . to cs) dir <&> (Set.toList . unAddressBook)

  -- remote address data
  let setToMap :: Ord k => (a -> k) -> Set a -> Map k a
      setToMap mkKey = Map.fromList . fmap (\a -> (mkKey a, a)) . Set.toList
  vcardsFromUrl :: Map Text VCard <-
    getAddressBook creds url <&> (setToMap (^. vcUrlPath) . unAddressBook)

  -- sync one entry
  let go :: VCard -> IO ()
      go vcard = do
        Map.lookup (vcard ^. vcUrlPath) vcardsFromUrl & \case
          Nothing -> upd "updating" True
          Just vcard' ->
            if vcard ^. vcEtag /= vcard' ^. vcEtag
              then upd "creating" False
              else pure ()
        where
          upd :: Text -> Bool -> IO ()
          upd typmsg typbool = do
            hPutStrLn stderr $ typmsg <> " remote " <> cs (vcard ^. vcFileName) <> "."
            putVCard typbool creds (url & URI.pathL .~ cs (vcard ^. vcUrlPath)) vcard

  -- iterate over all entries
  go `mapM_` vcardsFromDir

  -- download data from server, including etags and possible mutations.
  doGetContacts creds url dir
