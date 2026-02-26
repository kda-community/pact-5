module Pact.Crypto.SlhDsa.Signature
( RawSignature
, Signature
, toSignature
, toSignatureChecked
, ForsSignature
, HTSignature
, WotsSignature
, ForsAUTH
, ForsSK
, ForsAUTHValue
, XmmsSignature
, getR
, getHT
, getXmss
, getFors
, getXmssAuth
, getForsSK
, getForsAUTH
, getWots
) where

--- Some utils for signature parsing
import Pact.Crypto.SlhDsa.Parameters
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V

type RawSignature = B.ByteString

--- Top Level
type Signature = V.Vector SB.ShortByteString
------ > Second Level
type SignatureR = SB.ShortByteString
type ForsSignature = V.Vector SB.ShortByteString
type HTSignature = V.Vector SB.ShortByteString

----------- > Third Level
--------------- > FORS
type ForsSK = SB.ShortByteString
type ForsAUTH = V.Vector ForsAUTHValue
---
type ForsAUTHValue = SB.ShortByteString

--------------- > HT
type WotsSignature = V.Vector SB.ShortByteString
type XmmsSignature = V.Vector SB.ShortByteString
--
type XmmsAuthValue = SB.ShortByteString

-- Signatures are encoded in N-length ByteStrings in put in a Vector.
-- All algorithms use theses chunks, and must be able to quickly pickup elements

-- Convert a raw signature: big ByteString to a vector if N-length ByteStrings
toSignature :: Parameter -> RawSignature -> Signature
toSignature prm raw = V.unfoldr toSignature' $ SB.toShort raw
  where toSignature' raw'
          | SB.null raw' = Nothing
          | otherwise = Just $ SB.splitAt (n prm) raw'


checkSignature :: Parameter -> RawSignature -> Either String RawSignature
checkSignature prm sig
          | B.length sig == (sigLen prm * n prm) = Right sig
          | otherwise = Left "Invalid signature length"

toSignatureChecked :: Parameter -> RawSignature -> Either String Signature
toSignatureChecked prm sig = toSignature prm <$> checkSignature prm sig

-- Length of differents parts of the signature
-- FIPS-205 §9.2 Figure 17
rLen :: Parameter -> Int
rLen _ = 1

-- FIPS-205 §8 Figure 14
forsChunkLen :: Parameter -> Int
forsChunkLen prm = a prm + 1

-- FIPS-205 §8 Figure 14
forsLen :: Parameter -> Int
forsLen prm = k prm * forsChunkLen prm

wotsLen :: Parameter -> Int
wotsLen prm = n prm * 2 + 3

xmssLen :: Parameter -> Int
xmssLen prm =  wotsLen prm + h' prm

-- FIPS-205 §7.1 Figure 13
htLen :: Parameter -> Int
htLen prm = d prm * xmssLen prm

sigLen :: Parameter -> Int
sigLen prm = rLen prm + forsLen prm + htLen prm

-- Getter to pickup parts of the signature during the verification process
-- FIPS-205 §9.2 Figure 17
getR :: Signature -> SignatureR
getR = V.head

-- FIPS-205 §9.2 Figure 17
getFors :: Parameter -> Signature -> ForsSignature
getFors prm = V.slice (rLen prm) (forsLen prm)

-- FIPS-205 §9.2 Figure 17
getHT :: Parameter -> Signature -> HTSignature
getHT prm = V.slice (rLen prm + forsLen prm) (htLen prm)

-- FIPS-205 §8 Figure 14
getForsSK :: Parameter -> ForsSignature -> Int -> ForsSK
getForsSK prm sig i = sig V.! (forsChunkLen prm * i)

-- FIPS-205 §8 Figure 14
getForsAUTH :: Parameter -> ForsSignature -> Int -> ForsAUTH
getForsAUTH prm sig i = V.slice (forsChunkLen prm * i + 1) (a prm) sig

-- FIPS-205 §7.1 Figure 13
getXmss :: Parameter -> HTSignature -> Int -> XmmsSignature
getXmss prm sig i = V.slice (_len * i) _len sig
  where _len = xmssLen prm

getWots :: Parameter -> XmmsSignature ->  WotsSignature
getWots prm = V.slice 0 (wotsLen prm)

getXmssAuth :: Parameter -> XmmsSignature -> Int -> XmmsAuthValue
getXmssAuth prm sig i = sig V.! (wotsLen prm + i)
