{-# LANGUAGE TypeApplications #-}

module Pact.Crypto.SlhDsa.Utils
( PublickKey
, PublicKeySeed
, PublicKeyRoot
, Message
, MessageDigest
, Node
, SigContext
, OID
, toBase2N
, toInt
, ceilDiv
, mod2n
, sha256
, sha512
, mfg1Sha
, sphincsSha256
, sphincsSha512
  ) where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SB
import Data.Bits
import Pact.Crypto.SlhDsa.Addresses

import qualified Data.Hash.SHA2 as HS
import Data.Coerce

--- Some declarations related to Key management
type PublickKey = ShortByteString
type PublicKeySeed = ShortByteString
type PublicKeyRoot = ShortByteString

---- Related to data management
type Message = ShortByteString
type SigContext = ShortByteString
type OID = ShortByteString
type MessageDigest = ShortByteString
type Node = ShortByteString -- <-Merkle node

-- FIPS 205 §4.4 - Algorithm 4
-- For simplicty and performances, we only implement some special cases 4, 12 and 14

-- Base4096 : We group values by 3 bytes = 24 bits = Then output 2
-- We assume N mod 3 = 0
toBase4096':: [Int] -> [Int]
toBase4096' [] = []
toBase4096' (a:b:c:xs) = (a `shiftL` 4 .|. b `shiftR` 4) : ((b .&. 0xf) `shiftL` 8 .|. c) : toBase4096' xs
toBase4096' xs = toBase4096' $ take 3 (xs ++ repeat 0) -- Padding in case we have less than 3 remaining

-- Base16384 : We group values by 7 bytes = 56 bits = Then output 4
toBase16384':: [Int] -> [Int]
toBase16384' [] = []
toBase16384' (a:b:c:d:e:f:g:xs) = (a            `shiftL`  6 .|. b `shiftR` 2)
                                : ((b .&. 0x03) `shiftL` 12 .|. c `shiftL` 4  .|. d `shiftR` 4)
                                : ((d .&. 0x0f) `shiftL` 10 .|. e `shiftL` 2  .|. f `shiftR` 6)
                                : ((f .&. 0x3f) `shiftL`  8 .|. g)
                                : toBase16384' xs
toBase16384' xs = toBase16384' $ take 7 (xs ++ repeat 0) -- Padding in case we have less than 7 remaining

-- Base 16
toNibbles':: [Int] -> [Int]
toNibbles' [] = []
toNibbles' (a:xs) = (a `shiftR` 4) : (a .&. 0xf) : toNibbles' xs

toBase2N:: Int -> ShortByteString -> [Int]
toBase2N n = toBase2N' . map fromIntegral . SB.unpack
    where toBase2N'
            | n == 4 = toNibbles'
            | n == 12 = toBase4096'
            | n == 14 = toBase16384'
            | otherwise = error $ "Unsupported Base " ++ (show n)

---- Some Math Util functions
toInt:: ShortByteString -> Int
toInt = foldl' (\acc x -> shiftL acc 8  + x) 0 . map fromIntegral . SB.unpack

ceilDiv :: Int -> Int -> Int
ceilDiv n d = (n + d - 1) `div` d

mod2n:: Int -> Int -> Int
mod2n n = (.&.) ((bit $ n) - 1)


int32ToSB :: Int -> SB.ShortByteString
int32ToSB n = SB.pack [ fromIntegral $ (n `shiftR` 24) .&. 0xFF
                      , fromIntegral $ (n `shiftR` 16) .&. 0xFF
                      , fromIntegral $ (n `shiftR` 8)  .&. 0xFF
                      , fromIntegral $ n               .&. 0xFF]

----- Hash related functions
sha256:: [ShortByteString] -> ShortByteString
sha256 = coerce . HS.hashShortByteString_ @HS.Sha2_256 . SB.concat

sha512:: [ShortByteString] -> ShortByteString
sha512 = coerce . HS.hashShortByteString_ @HS.Sha2_512 . SB.concat

-- FIPS 205 11.2
-- SHA256 related functions
mfg1Sha:: ([ShortByteString] -> ShortByteString) -> Int -> [ShortByteString] -> ShortByteString
mfg1Sha hashF len dataIn = go SB.empty 0
  where go current cnt
          | SB.length current >= len = SB.take len current
          | otherwise = go (current <> hashF [seed, int32ToSB cnt]) (cnt +1)
        seed = SB.concat dataIn

-- Base SPHINCS hash function used to build F, H and Tl
sphincsSha:: ([ShortByteString] -> ShortByteString) -> Int -> Int -> ShortByteString -> Address -> [ShortByteString] -> ShortByteString
sphincsSha hashF padding len pks addr  = SB.take len . hashF . (++) [pks, SB.replicate (padding - len) 0, encodeCompressed addr]

sphincsSha256:: Int -> ShortByteString -> Address -> [ShortByteString] -> ShortByteString
sphincsSha256 = sphincsSha sha256 64

sphincsSha512:: Int -> ShortByteString -> Address -> [ShortByteString] -> ShortByteString
sphincsSha512 = sphincsSha sha512 128