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
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import qualified Pact.Core.Crypto.Base64 as B64
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding as ASN1
import qualified Data.ASN1.Types as ASN1

-- Pact / Chainweb SLH-DSA signatures Spectification

-- Constant context
chainwebContext:: SB.ShortByteString
chainwebContext = "CHAINWEB"

-- According to RFC-7693 Section 4
blake2Base :: ASN1.OID
blake2Base = [1, 3, 6, 1, 4, 1, 1722, 12, 2]

encodeBlake2WithSuffix :: ASN1.OID -> EncodedOID
encodeBlake2WithSuffix = SB.toShort . ASN1.encodeASN1' ASN1.DER . L.singleton . ASN1.OID . (++) blake2Base

-- 1.3.6.1.4.1.1722.12.2.1.8
blake2b256Asn1:: EncodedOID
blake2b256Asn1 = encodeBlake2WithSuffix [1, 8]

-- 1.3.6.1.4.1.1722.12.2.1.16
blake2b512Asn1:: EncodedOID
blake2b512Asn1 = encodeBlake2WithSuffix [1, 16]

-- Auto Select the set of pramaters depending on scheme
paramaterFromScheme:: PPKScheme -> Either String Parameter
paramaterFromScheme SlhDsaSha128s = Right slh_dsa_sha2_128s
paramaterFromScheme SlhDsaSha192s = Right slh_dsa_sha2_192s
paramaterFromScheme SlhDsaSha256s = Right slh_dsa_sha2_256s
paramaterFromScheme _ = Left "Unsupported Signature scheme"


-- Auto Select the Hash OID based ont the Hash length
oidFromhash:: Hash -> Either String EncodedOID
oidFromhash txHash
              | (SB.length . unHash) txHash == 32 = Right blake2b256Asn1
              | (SB.length . unHash) txHash == 64 = Right blake2b512Asn1
              | otherwise = Left "Unsupported TxHash"

-- Main entry point for verifying signatures
-- TODO Signature spec needs to be finalized
verifySig :: PPKScheme -> T.Text -> T.Text -> Hash -> Either String ()
verifySig pactScheme pkey sig txHash = do
    decodedPkey <- SB.toShort <$> (B16.decode $ TE.encodeUtf8 pkey)
    decodedSig  <- B64.decodeBase64UrlUnpaddedStrict $ TE.encodeUtf8 sig
    prm <- paramaterFromScheme pactScheme
    oid <- oidFromhash txHash
    verifySignaturePreHashedWithContext prm chainwebContext oid decodedPkey decodedSig $ unHash txHash
