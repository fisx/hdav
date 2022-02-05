module Data.DAV.Class
  ( decode,
    encode,
    FromDAV (fromDAV),
    ToDAV (toDAV),
  )
where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError)
import Data.DAV.Value
import Data.Text (Text)

----------------------------------------------------------------------

decode :: FromDAV a => Text -> MonadError Text m => m a
decode = parseValue >=> fromDAV

encode :: ToDAV a => a -> Text
encode = renderValue . toDAV

class FromDAV a where
  fromDAV :: Value -> MonadError Text m => m a

instance FromDAV Value where
  fromDAV = pure

class ToDAV a where
  toDAV :: a -> Value

instance ToDAV Value where
  toDAV = id
