module Pact.Core.Crypto.Base64
  ( encodeBase64UrlUnpadded
  , decodeBase64UrlUnpadded
  , decodeBase64UrlUnpadded'
  , fromB64UrlUnpaddedText
  , toB64UrlUnpaddedText
  ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Data.Bifunctor (first)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Base64.Types as B64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- TODO Double Check how the Left message could contaminate and being an unexpected forking change.
-- TODO Triple Check  ^
decodeBase64UrlUnpadded :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded = first T.unpack . B64URL.decodeBase64Untyped

-- Reject padded Base64
decodeBase64UrlUnpadded' :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded' = first (const "Base64URL decode failed: Invalid padding") . B64URL.decodeUnpadded

fromB64UrlUnpaddedText :: ByteString -> Either String Text
fromB64UrlUnpaddedText bs = case decodeBase64UrlUnpadded bs of
  Right bs' -> case T.decodeUtf8' bs' of
    Left _ -> Left "Base64URL decode failed: invalid unicode"
    Right t -> Right t
  Left _ -> Left $ "Base64URL decode failed"

encodeBase64UrlUnpadded :: ByteString -> ByteString
encodeBase64UrlUnpadded = B64URL.extractBase64 . B64URL.encodeBase64Unpadded'

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText  = B64URL.extractBase64 . B64URL.encodeBase64Unpadded
