{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (optional)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.String.Conversions (cs)
import Data.Text as Text
import Network.DAV.Cal
import Network.DAV.Card
import Network.DAV.Util
import Options.Applicative.Builder
  ( argument,
    command,
    eitherReader,
    help,
    idm,
    info,
    long,
    metavar,
    prefs,
    progDesc,
    showHelpOnError,
    str,
    strOption,
    subparser,
  )
import Options.Applicative.Extra (customExecParser)
import Options.Applicative.Types (Parser)
import URI.ByteString (URI)
import qualified URI.ByteString as URI

data CliCtx = CliCtx
  { cliCredsFile :: FilePath,
    cliCredsEntryPrefix :: Text,
    cliCommand :: Command
  }
  deriving stock (Eq, Show)

data Command
  = GetContacts URI FilePath
  | PutContacts FilePath URI
  | TestContacts URI
  | GetCalendar URI FilePath
  deriving stock (Eq, Show)

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
          ),
        command
          "test-contacts"
          ( info
              (TestContacts <$> cliAddressBookUrl)
              ( progDesc
                  "`addressbookUrl` must point to an address book that will be used for CRUDding \
                  \VCards.  Any data in that address book will be destroyed."
              )
          ),
        command
          "get-calendar"
          ( info
              (GetCalendar <$> cliAddressBookUrl <*> cliAddressBookDir)
              (progDesc "you know...")
          )
      ]

-- TODO: rename to `parseCliUrl`?
cliAddressBookUrl :: Parser URI
cliAddressBookUrl = argument (eitherReader (\raw -> first (mkerr raw) $ prs raw)) (metavar "URI")
  where
    prs = URI.parseURI URI.strictURIParserOptions . cs
    mkerr raw err = "could not parse URI: " <> show (raw, err)

-- TODO: rename to `parseFileOrDirPath`?
cliAddressBookDir :: Parser FilePath
cliAddressBookDir = argument str (metavar "FilePath")

main :: IO ()
main = do
  defCredsFile <- getDefCredsFile
  ctx <- customExecParser (prefs showHelpOnError) (info (parseCliCtx defCredsFile) idm)
  creds <- getCredentials (cliCredsFile ctx) (cliCredsEntryPrefix ctx)

  case cliCommand ctx of
    GetContacts url dir -> doGetContacts creds url dir
    PutContacts dir url -> doPutContacts creds dir url
    TestContacts url -> doTestContacts creds url
    GetCalendar url dir -> doGetCalendar creds url dir
