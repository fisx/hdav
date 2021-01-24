module Network.DAV.Util
  ( Credentials (Credentials),
    credLogin,
    credPassword,
    CouldNotParseCredentials (CouldNotParseCredentials),
    getDefCredsFile,
    defCredsEntryPrefix,
    getCredentials,
    payloadFromXML,
  )
where

import Control.Exception
import Control.Lens (makeLenses, (<&>))
import Data.String.Conversions (cs)
import Data.Text as Text
import Data.Text.IO as Text
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import System.Environment
import System.FilePath
import Text.XML as XML

----------------------------------------------------------------------
-- auth

data Credentials = Credentials {_credLogin :: Text, _credPassword :: Text}

makeLenses ''Credentials

data CouldNotParseCredentials = CouldNotParseCredentials ([[Text]], [[Text]])
  deriving stock (Eq, Show)

instance Exception CouldNotParseCredentials

getDefCredsFile :: IO FilePath
getDefCredsFile = getEnv "HOME" <&> (</> ".davfs2" </> "secrets")

defCredsEntryPrefix :: Text
defCredsEntryPrefix = "https://"

getCredentials :: FilePath -> Text -> IO Credentials
getCredentials credsFile credsEntryPrefix = do
  raw :: [Text] <- Text.lines <$> Text.readFile credsFile
  let credentials = Text.words <$> Prelude.filter (credsEntryPrefix `Text.isPrefixOf`) raw
  case credentials of
    [[_, login, password]] -> pure $ Credentials login password
    bad -> throwIO $ CouldNotParseCredentials (credentials, bad)

----------------------------------------------------------------------
-- misc

payloadFromXML :: XML.Document -> W.Payload
payloadFromXML doc = W.Raw "application/xml charset=utf-8" (H.RequestBodyLBS . cs $ XML.renderText XML.def doc)
