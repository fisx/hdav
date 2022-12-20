module Network.DAV.Cal
  ( getCalendar,
    doGetCalendar,
  )
where

import Control.Lens ((%~), (&), (?~), (^.))
import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Network.DAV.Util
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W
import URI.ByteString (URI)
import qualified URI.ByteString as URI

-- TODO: cli commadn for creating event
-- TODO: cli command get-all-calendars (same for "all address books")

getCalendar :: Credentials -> URI -> IO Text
getCalendar creds url = do
  let options =
        W.defaults
          & (W.headers %~ (<> [("Depth", "1")]))
          & (W.auth ?~ W.basicAuth (cs $ creds ^. credLogin) (cs $ creds ^. credPassword))
  resp <-
    W.customPayloadMethodWith
      "GET"
      options
      (cs $ URI.serializeURIRef' url)
      ("" :: ByteString)
  pure . cs . H.responseBody $ resp

doGetCalendar :: Credentials -> URI -> FilePath -> IO ()
doGetCalendar creds url fp = do
  Text.writeFile fp =<< getCalendar creds url
