{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Crypto.SlhDsa.Addresses
( Address(..)
, TreeHeight
, TreeIndex
, HashAddress
, ChainAddress
, encode
, encodeCompressed
, toWOTSPKAddress
, toForsRootAddress
, toWOTSHashAddress
, toHashTreeAddress
, updateMerkleTreeIndex
) where

import Data.ByteString.Builder
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Lazy as LB
type LayerAddress   = Word32
type TreeAddress    = Integer
type KeyPairAddress = Word32
type ChainAddress   = Word32
type HashAddress    = Word32
type TreeHeight     = Word32
type TreeIndex      = Word32


-- FIPS-205 §4.2 - Figures 2 to 9
-- Define all adresses types
data AdressType = WOTS_HASH | WOTS_PK | TREE | FORS_TREE | FORS_ROOT | WOTS_PRF | FORS_PRF
  deriving (Enum, Show)

data Address = BaseAddress     { la :: LayerAddress, ta :: TreeAddress }
             | WOTSHashAddress { la :: LayerAddress, ta :: TreeAddress, kpa :: KeyPairAddress, ca :: ChainAddress, ha :: HashAddress }
             | WOTSPKAddress   { la :: LayerAddress, ta :: TreeAddress, kpa :: KeyPairAddress }
             | HashTreeAddress { la :: LayerAddress, ta :: TreeAddress, th :: TreeHeight, ti :: TreeIndex }
             | ForsTreeAddress { la :: LayerAddress, ta :: TreeAddress, kpa :: KeyPairAddress, th :: TreeHeight, ti :: TreeIndex }
             | ForsRootAddress { la :: LayerAddress, ta :: TreeAddress, kpa :: KeyPairAddress }
  deriving (Show)

-- Serialization functions
encodeTreeAddress :: TreeAddress -> Builder
encodeTreeAddress n = word32BE (fromIntegral $ shiftR n 64) <> word64BE (fromIntegral n)

encodeType :: AdressType -> Builder
encodeType = word32BE . fromIntegral . fromEnum

padding32 :: Builder
padding32 = word32BE 0

padding64 :: Builder
padding64 = word64BE 0

encode' :: Address -> LB.LazyByteString
encode' BaseAddress{..}     = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> padding64 <> padding64 -- Not used
encode' WOTSHashAddress{..} = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> encodeType WOTS_HASH <> word32BE kpa <> word32BE ca <> word32BE ha
encode' WOTSPKAddress{..}   = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> encodeType WOTS_PK   <> word32BE kpa <> padding64
encode' HashTreeAddress{..} = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> encodeType TREE      <> padding32    <> word32BE th <> word32BE ti
encode' ForsTreeAddress{..} = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> encodeType FORS_TREE <> word32BE kpa <> word32BE th <> word32BE ti
encode' ForsRootAddress{..} = toLazyByteString $ word32BE la <> encodeTreeAddress ta <> encodeType FORS_ROOT <> word32BE kpa <> padding64

encode:: Address -> ShortByteString
encode = SB.toShort . B.toStrict . encode'


-- Compressed Adresses for SHA-2 function
-- FIPS-205 §11.2 - Figure 18
encodeTreeAddress' :: TreeAddress -> Builder
encodeTreeAddress' = word64BE . fromIntegral

encodeType' :: AdressType -> Builder
encodeType' = word8 . fromIntegral . fromEnum

encodeLA' :: LayerAddress -> Builder
encodeLA' = word8 . fromIntegral

encodeCompressed' :: Address -> LB.LazyByteString
encodeCompressed' BaseAddress{..}     = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> word8 0               <> padding32    <> padding64  -- Not used
encodeCompressed' WOTSPKAddress{..}   = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> encodeType' WOTS_PK   <> word32BE kpa <> padding64
encodeCompressed' HashTreeAddress{..} = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> encodeType' TREE      <> padding32    <> word32BE th <> word32BE ti
encodeCompressed' ForsTreeAddress{..} = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> encodeType' FORS_TREE <> word32BE kpa <> word32BE th <> word32BE ti
encodeCompressed' WOTSHashAddress{..} = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> encodeType' WOTS_HASH <> word32BE kpa <> word32BE ca <> word32BE ha
encodeCompressed' ForsRootAddress{..} = toLazyByteString $ encodeLA' la <> encodeTreeAddress' ta <> encodeType' FORS_ROOT <> word32BE kpa <> padding64

encodeCompressed :: Address -> ShortByteString
encodeCompressed = SB.toShort . B.toStrict . encodeCompressed'

-- Some addresses transformation functions
toWOTSHashAddress :: Address -> KeyPairAddress -> Address
toWOTSHashAddress BaseAddress{..} kpa = WOTSHashAddress {ca = 0, ha = 0, ..}
toWOTSHashAddress _ _ = error "Not a use case"

toHashTreeAddress :: Address -> TreeIndex -> Address
toHashTreeAddress BaseAddress{..} ti = HashTreeAddress {th = 0, ..}
toHashTreeAddress _ _ = error "Not a use case"

toWOTSPKAddress :: Address -> Address
toWOTSPKAddress WOTSHashAddress{..} = WOTSPKAddress {..}
toWOTSPKAddress _ = error "Not a use case"

toForsRootAddress :: Address -> Address
toForsRootAddress ForsTreeAddress{..} = ForsRootAddress {..}
toForsRootAddress _ = error "Not a use case"

--- Special case to walk through in a Merkle tree.
--- Increment the height and update the index: /2 in case of left and - 1 / 2 in case of right
--- Used by Alogrithms 11 and 18
updateMerkleTreeIndex :: Address -> Bool -> Address
updateMerkleTreeIndex addr isLeft = case addr of
    ForsTreeAddress{..} -> ForsTreeAddress {th=th + 1, ti=updateIdx ti, ..}
    HashTreeAddress{..} -> HashTreeAddress {th=th + 1, ti=updateIdx ti, ..}
    _ -> error "Only tree addresses are used to walk through Merkle trees"

  where updateIdx x
              | isLeft =  quot x 2
              | otherwise = quot (x - 1) 2
