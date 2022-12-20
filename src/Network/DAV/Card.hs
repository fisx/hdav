module Network.DAV.Card
  ( VCard (VCard),
    vcAddressbookUrlPath,
    vcFilename,
    vcEtag,
    vcCard,
    AddressBook (AddressBook, unAddressBook),
    getAddressBook,
    putAddressBook,
    getAddressBookQuery,
    parseAddressBook,
    getVCard,
    putVCard,
    deleteVCard,
    saveAddressBook,
    saveVCard,
    loadAddressBook,
    loadVCard,
    doGetContacts,
    doPutContacts,
    doTestContacts,
  )
where

import Control.Exception (SomeException, catch)
import Control.Lens (makeLenses, to, view, (%~), (&), (.~), (<&>), (?~), (^.))
import Control.Monad (forM, forM_, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (sort, (\\))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import GHC.Stack (HasCallStack)
import Network.DAV.Util
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Process
import Text.Hamlet.XML as XML
import Text.XML as XML
import Text.XML.Cursor (($/), (&/), (&|))
import qualified Text.XML.Cursor as XML
import URI.ByteString (URI)
import qualified URI.ByteString as URI

----------------------------------------------------------------------
-- types

-- unparsed vcard, with some metadata needed for syncing.
data VCard = VCard
  { _vcAddressbookUrlPath :: Text,
    _vcFilename :: FilePath,
    _vcEtag :: Text,
    _vcCard :: Text
  }
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

        _vcAddressbookUrlPath = nonnull . mconcat $ (cursor $/ XML.element "{DAV:}href" &/ XML.content)
        _vcFilename = cs . nonnull . cs . takeFileName . cs $ _vcAddressbookUrlPath
        _vcEtag =
          nonnull . mconcat $
            ( cursor
                $/ XML.element "{DAV:}propstat"
                &/ XML.element "{DAV:}prop"
                &/ XML.element "{DAV:}getetag"
                &/ XML.content
            )
        _vcCard =
          nonnull . mconcat $
            ( cursor
                $/ XML.element "{DAV:}propstat"
                &/ XML.element "{DAV:}prop"
                &/ XML.element "{urn:ietf:params:xml:ns:carddav}address-data"
                &/ XML.content
            )

----------------------------------------------------------------------
-- putAddressBook

putAddressBook :: Credentials -> URI -> AddressBook -> IO ()
putAddressBook creds url (AddressBook (Set.toList -> vcardsFromDir)) = do
  -- remote address data
  let setToMap :: Ord k => (a -> k) -> Set a -> Map k a
      setToMap mkKey = Map.fromList . fmap (\a -> (mkKey a, a)) . Set.toList
  vcardsFromUrl :: Map Text VCard <-
    getAddressBook creds url <&> (setToMap (^. vcAddressbookUrlPath) . unAddressBook)

  -- sync all local entries
  let updateOrCreateOne :: VCard -> IO ()
      updateOrCreateOne vcard = do
        let origin = Map.lookup (vcard ^. vcAddressbookUrlPath) vcardsFromUrl
        when (isNothing origin || origin /= Just vcard) $ do
          let ifMatch = if isJust origin then Just (vcard ^. vcEtag . to cs) else Nothing -- ?!
          hPutStrLn stderr $ "creating or overwriting remote " <> cs (vcard ^. vcFilename) <> "."
          -- FUTUREWORK: i think we could just compare etags here to avoid redundant puts.
          putVCard creds (url & URI.pathL .~ cs (vcard ^. vcAddressbookUrlPath)) vcard ifMatch >>= \case
            HdavSuccess (_card :: VCard) -> do
              -- FUTUREWORK: re-construct local address book from this instead of pulling it again after we're done here.
              pure ()
            HdavError i e -> do
              -- FUTUREWORK: report more nicely!
              error $ show (i, e)
          pure ()

  updateOrCreateOne `mapM_` vcardsFromDir

  -- delete all entries that are missing locally, but are present remotely
  let deleteOne :: URI -> IO ()
      deleteOne uri = do
        deleteVCard creds uri Nothing >>= \case
          HdavSuccess () -> pure ()
          HdavError i e -> error $ show (i, e)
        pure ()

      deletees :: [URI]
      deletees = (\path -> url & URI.pathL .~ cs path) <$> deleteePaths
        where
          allPaths = Map.keys vcardsFromUrl
          deleteePaths = allPaths \\ ((^. vcAddressbookUrlPath) <$> vcardsFromDir)

  deleteOne `mapM_` deletees

----------------------------------------------------------------------
-- CRUD VCard

data HdavResult result
  = HdavSuccess result
  | HdavError Int Text
  deriving stock (Eq, Ord, Show)

_postVCard :: HasCallStack => Credentials -> URI -> VCard -> IO ()
_postVCard = error "Not implemented (nextcloud does not implement this either, as of 18.0.3.0)."

getVCard :: HasCallStack => Credentials -> URI -> IO (HdavResult VCard)
getVCard creds vcardUrl = do
  let options =
        W.defaults
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
          & (W.checkResponse .~ Just (\_ _ -> pure ()))
  resp <- W.customMethodWith "GET" options (cs $ URI.serializeURIRef' vcardUrl)
  let status = resp ^. W.responseStatus . W.statusCode
      msg = cs $ "could not GET vcard: " <> show (resp ^. W.responseStatus, resp ^. W.responseBody)
      addressbookUrl = cs . takeDirectory . cs $ URI.serializeURIRef' vcardUrl
  pure $
    if resp ^. W.responseStatus . W.statusCode < 400
      then HdavSuccess $ VCard addressbookUrl "" "" (resp ^. W.responseBody . to cs)
      else HdavError status msg

-- | Take a vcard URI, a VCard, and an optional etag of the vcard version previously existing
-- in the target.  If the etag matches (or if it is 'Nothing' and the vcard does not exist in
-- the target), upload the vcard; otherwise fail.
putVCard :: HasCallStack => Credentials -> URI -> VCard -> Maybe ByteString -> IO (HdavResult VCard)
putVCard creds vcardUrl vcard mbEtag = do
  putOrDeleteVCard PUT creds vcardUrl (Just vcard) mbEtag >>= \case
    HdavSuccess _ -> pure ()
    HdavError i e -> do
      -- FUTUREWORK: report more nicely!
      error $ show (i, e)
  getVCard creds vcardUrl

deleteVCard :: HasCallStack => Credentials -> URI -> Maybe ByteString -> IO (HdavResult ())
deleteVCard creds vcardUrl mbEtag = putOrDeleteVCard DELETE creds vcardUrl Nothing mbEtag

data Verb = PUT | DELETE
  deriving stock (Eq, Show)

putOrDeleteVCard :: HasCallStack => Verb -> Credentials -> URI -> Maybe VCard -> Maybe ByteString -> IO (HdavResult ())
putOrDeleteVCard verb creds vcardUrl mbVCard mbEtag = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("If-Match", fromJust mbEtag) | isJust mbEtag]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
          & (W.checkResponse .~ Just (\_ _ -> pure ()))
  resp <-
    W.customPayloadMethodWith
      (show verb)
      options
      (cs $ URI.serializeURIRef' vcardUrl)
      (W.Raw "text/vcard; charset=utf-8" (maybe mempty (\vcard -> H.RequestBodyLBS (cs (vcard ^. vcCard))) mbVCard))

  let status = resp ^. W.responseStatus . W.statusCode
      msg = cs $ "could not " <> show verb <> " vcard: " <> show (resp ^. W.responseStatus, resp ^. W.responseBody)

  pure $
    if resp ^. W.responseStatus . W.statusCode < 400
      then HdavSuccess ()
      else HdavError status msg

----------------------------------------------------------------------
-- local storage

saveAddressBook :: HasCallStack => FilePath -> AddressBook -> IO ()
saveAddressBook dir = mapM_ (saveVCard dir) . Set.toList . unAddressBook

saveVCard :: HasCallStack => FilePath -> VCard -> IO ()
saveVCard dir card = do
  Text.writeFile (dir </> cs (card ^. vcFilename)) (card ^. vcCard)
  Text.writeFile (dir </> takeBaseName (cs (card ^. vcFilename)) <.> "etag") (card ^. vcEtag)

loadAddressBook :: HasCallStack => String -> FilePath -> IO AddressBook
loadAddressBook addrBookURL dir = do
  vcfs <- getDirectoryContents dir <&> List.filter ((== ".vcf") . takeExtension)
  AddressBook . Set.fromList <$> (loadVCard addrBookURL dir `mapM` vcfs)

loadVCard :: HasCallStack => String -> FilePath -> FilePath -> IO VCard
loadVCard addrBookURL addrBookDir _vcFilename = do
  let _vcAddressbookUrlPath = cs $ addrBookURL </> _vcFilename
  -- If the following seems ugly to you, you're right, I don't know why I am reading
  -- bytestring and hit it with 'cs' instead of calling Text.readFile.  But consider this:
  -- rumor has it that some VCF files have different encoding in different record fields.
  _vcCard <-
    cs
      <$> ByteString.readFile (addrBookDir </> _vcFilename)
      `catch` (error . (show @SomeException))
  _vcEtag <- do
    let etagFile = addrBookDir </> takeBaseName _vcFilename <.> "etag"
    doesFileExist etagFile >>= \case
      True ->
        cs
          <$> ByteString.readFile etagFile
          `catch` (error . (show @SomeException))
      False ->
        pure mempty
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
  addrbook <- loadAddressBook (url ^. URI.pathL . to cs) dir
  putAddressBook creds url addrbook
  -- download data from server, including etags and possible mutations.
  doGetContacts creds url dir

-- | Prerequisite: 'addressbookUrl' must point to an address book that will be used for
-- CRUDding VCards.  Any data in that address book will be destroyed.
doTestContacts :: Credentials -> URI -> IO ()
doTestContacts creds addressbookUrl = do
  initialCleanup
  -- happy path
  testGetPutDelete
  -- sad path
  testPostExists
  testPutNotExists
  testPutStaleEtag
  testDeleteNonExists
  testDeleteStaleEtag
  where
    -- download address book and remove all vcards that we find.
    initialCleanup :: IO ()
    initialCleanup = do
      vcardsFromUrl <- getAddressBook creds addressbookUrl
      forM_ (Set.toList (unAddressBook vcardsFromUrl)) $ \vcard -> do
        hPutStrLn stderr $ "initial cleanup: removing " <> show vcard
        deleteVCard creds (addressbookUrl & URI.pathL .~ cs (vcard ^. vcAddressbookUrlPath)) Nothing

    testGetPutDelete :: IO ()
    testGetPutDelete = do
      forM_ [False, True] $ \withEtag -> do
        hPutStrLn stderr $ "testGetPutDelete: " <> show withEtag
        do
          -- put new
          [] <- ensureAddressBookFlush []
          let vcard = vcard1 "asdf" withEtag
              vcardUrl = addressbookUrl & URI.pathL .~ cs (cs (vcard ^. vcAddressbookUrlPath) </> cs (vcard ^. vcFilename))
          vcard' <-
            putVCard creds vcardUrl vcard Nothing >>= \case
              HdavSuccess v -> pure v
              bad -> error $ "put new: " <> show bad
          void $ ensureAddressBookFlush [vcard']

        do
          -- put same
          [] <- ensureAddressBookFlush []
          let vcard = vcard1 "asdf" withEtag
              vcardUrl = addressbookUrl & URI.pathL .~ cs (cs (vcard ^. vcAddressbookUrlPath) </> cs (vcard ^. vcFilename))
          vcard' <-
            putVCard creds vcardUrl vcard Nothing >>= \case
              HdavSuccess v -> pure v
              bad -> error $ "put same(1): " <> show bad
          vcard'' <-
            putVCard creds vcardUrl vcard (Just . cs . view vcEtag $ vcard') >>= \case
              HdavSuccess v -> pure v
              bad -> error $ "put same(2): " <> show bad
          unless (vcard' == vcard'') $ do
            error $ "put same: " <> show (vcard', vcard'')
          void $ ensureAddressBookFlush [vcard'']

        do
          -- put changed
          [] <- ensureAddressBookFlush []
          let vcard = vcard1 "asdf" withEtag
              vcardChanged = vcard1 "woeifhoy" withEtag
              vcardUrl = addressbookUrl & URI.pathL .~ cs (cs (vcard ^. vcAddressbookUrlPath) </> cs (vcard ^. vcFilename))
          vcard' <-
            putVCard creds vcardUrl vcard Nothing >>= \case
              HdavSuccess v -> pure v
              bad -> error $ "put changed(1): " <> show bad
          vcard'' <-
            putVCard creds vcardUrl vcardChanged (Just . cs . view vcEtag $ vcard') >>= \case
              HdavSuccess v -> pure v
              bad -> error $ "put changed(2): " <> show bad
          unless (vcard' ^. vcCard == vcard ^. vcCard && vcard'' ^. vcCard == vcardChanged ^. vcCard) $ do
            -- FUTUREWORK: would be nice to also have vcAddressbookUrlPath align, but there is
            -- sometimes a hostname, and sometimes not.
            error $ "put changed: " <> show (vcard', vcard, vcard'', vcardChanged)
          void $ ensureAddressBookFlush [vcardChanged]

    -- post existing vcard => 4xx
    testPostExists :: IO ()
    testPostExists = error "FUTUREWORK"

    -- put a non-existing vcard => 4xx
    testPutNotExists :: IO ()
    testPutNotExists = error "FUTUREWORK"

    -- put a existing vcard with stale etag => 4xx
    testPutStaleEtag :: IO ()
    testPutStaleEtag = error "FUTUREWORK"

    -- delete non-existing vcard => 204
    testDeleteNonExists :: IO ()
    testDeleteNonExists = error "FUTUREWORK"

    -- delete vcard with mismatching etag => 4xx
    testDeleteStaleEtag :: IO ()
    testDeleteStaleEtag = error "FUTUREWORK"

    vcard1 :: Text -> Bool -> VCard
    vcard1 fullName withEtag =
      VCard
        { _vcAddressbookUrlPath = "/remote.php/dav/addressbooks/users/matthias.fischmann/test/",
          _vcFilename = "D55DBA88-A8EE-4FE7-ADEB-71522CD8F330.vcf",
          _vcEtag =
            if withEtag
              then case fullName of
                "asdf" -> "\"55ac246bc24e1b7a27488ffbfca631dc\""
                "woeifhoy" -> "\"f900b929731b3ee1f3ff3716d65096cd\""
                bad -> error $ "not implemented: " <> show bad
              else "",
          _vcCard =
            ( "BEGIN:VCARD\n\
              \VERSION:3.0\n\
              \PRODID:-//Sabre//Sabre VObject 4.3.5//EN\n\
              \UID:4d815a4b-2b22-472d-89e0-28e1612cdc14\n\
              \FN:"
                <> fullName
                <> "\n\
                   \ADR;TYPE=HOME:;;;;;;\n\
                   \EMAIL;TYPE=HOME:123@43\n\
                   \TEL;TYPE=HOME,VOICE:123\n\
                   \ORG:asdf\n\
                   \REV;VALUE=DATE-AND-OR-TIME:20220205T114727Z\n\
                   \END:VCARD\n"
            )
        }

    ensureAddressBookFlush :: HasCallStack => [VCard] -> IO [VCard]
    ensureAddressBookFlush (sort -> vcards) = do
      vcardsFromUrl <- Set.toAscList . unAddressBook <$> getAddressBook creds addressbookUrl
      unless (length vcards == length vcardsFromUrl) $ do
        error $ "didn't expect that: " <> show (vcards, vcardsFromUrl)
      let pairs = zip vcards vcardsFromUrl
      forM pairs $ \(should, have) -> do
        unless (have ^. vcCard == should ^. vcCard) $ do
          -- FUTUREWORK: would be nice to also have vcAddressbookUrlPath align, but there is
          -- sometimes a hostname, and sometimes not.
          error $ "mismatch: " <> show (should, have)
        deleteVCard creds (addressbookUrl & URI.pathL .~ cs (have ^. vcAddressbookUrlPath)) Nothing
        pure have
