{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Implementation of the `keccak256` pact native.
module Pact.Core.Crypto.Hash.Keccak256 (Keccak256Error(..), keccak256) where

import Data.ByteArray as BA
import Crypto.Hash
import Control.Exception (Exception(..), SomeException(..))
import Control.DeepSeq
import Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import qualified Data.Vector as V
import Pact.Core.Crypto.Base64 (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)
import GHC.Generics(Generic)

data Keccak256Error
  = Keccak256Base64Exception String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

instance NFData Keccak256Error

keccak256 :: V.Vector BS.ByteString -> Either Keccak256Error Text
keccak256 bytesArray =
  case BS.concat <$> (mapM decodeBase64UrlUnpadded $ V.toList bytesArray) of
    Left b64Err -> Left $ Keccak256Base64Exception b64Err
    Right bs -> Right $ (Text.decodeUtf8 .  encodeBase64UrlUnpadded . BA.convert . doHash) bs

  where
    doHash :: BS.ByteString -> Digest Keccak_256
    doHash = hash
