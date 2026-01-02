module Pact.Crypto.SlhDsa.ChainwebSlhDsa
(
  verifySig
) where

import Pact.Crypto.SlhDsa.Parameters
import Pact.Crypto.SlhDsa.Utils
import Pact.Crypto.SlhDsa.SlhDsa
import Pact.Core.Hash
import Pact.Core.Scheme

import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Pact.Core.Crypto.Base64 as B64

-- Pact / Chainweb SLH-DSA signatures Spectification

-- Constant context
chainwebContext:: SB.ShortByteString
chainwebContext = "CHAINWEB"

-- TODO: Rechek these OID before freezing the spec

-- 1.3.6.1.4.1.1722.12.2.1.8
blake2b256Oid:: SB.ShortByteString
blake2b256Oid = SB.toShort $ B16.decodeLenient "060A2B0601040186BA0C020108"

-- 1.3.6.1.4.1.1722.12.2.1.16
blake2b512Oid:: SB.ShortByteString
blake2b512Oid = SB.toShort $ B16.decodeLenient "060A2B0601040186BA0C020110"

-- Auto Select the set of pramaters depending on scheme
paramaterFromScheme:: PPKScheme -> Either String Parameter
paramaterFromScheme SlhDsaSha128s = Right slh_dsa_sha2_128s
paramaterFromScheme SlhDsaSha192s = Right slh_dsa_sha2_192s
paramaterFromScheme SlhDsaSha256s = Right slh_dsa_sha2_256s
paramaterFromScheme _ = Left "Unsupported Signature scheme"


-- Auto Select the Hash OID based ont the Hash length
oidFromhash:: Hash -> Either String OID
oidFromhash txHash
              | (SB.length . unHash) txHash == 32 = Right blake2b256Oid
              | (SB.length . unHash) txHash == 64 = Right blake2b512Oid
              | otherwise = Left "Unsupported TxHash"

-- Main entry point for verifying signatures
-- TODO Signature spec needs to be finalized
verifySig:: PPKScheme -> T.Text -> T.Text -> Hash -> Either String ()
verifySig pactScheme pkey sig txHash = do
    decodedPkey <- SB.toShort <$> (B16.decode $ TE.encodeUtf8 pkey)
    decodedSig  <- B64.decodeBase64UrlUnpadded $ TE.encodeUtf8 sig
    prm <- paramaterFromScheme pactScheme
    oid <- oidFromhash txHash
    verifySignaturePreHashedWithContext prm chainwebContext oid decodedPkey decodedSig $ unHash txHash
