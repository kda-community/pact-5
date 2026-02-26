{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pact.Crypto.SlhDsa.MessageDigest
( forsDigest
, treeIndex
, leafIndex
) where

import qualified Data.ByteString.Short as SB
import Pact.Crypto.SlhDsa.Parameters
import Pact.Crypto.SlhDsa.Utils

forsDigestLen :: Parameter -> Int
forsDigestLen prm = ceilDiv (k prm * a prm) 8

treeDigestLen :: Parameter -> Int
treeDigestLen prm = ceilDiv (h prm - h' prm) 8

leafDigestLen :: Parameter -> Int
leafDigestLen prm = ceilDiv (h' prm) 8

forsDigest :: Parameter -> MessageDigest -> MessageDigest
forsDigest prm  = SB.take $ forsDigestLen prm

treeDigest :: Parameter -> MessageDigest -> MessageDigest
treeDigest prm = SB.take (treeDigestLen prm) . SB.drop (forsDigestLen prm)

leafDigest :: Parameter -> MessageDigest -> MessageDigest
leafDigest prm = SB.take (treeDigestLen prm) . SB.drop (forsDigestLen prm +  treeDigestLen prm)

treeIndex :: Parameter -> MessageDigest -> Int
treeIndex prm = mod2n (h prm - h' prm) . toInt . treeDigest prm

leafIndex :: Parameter -> MessageDigest -> Int
leafIndex prm = mod2n (h' prm) . toInt . leafDigest prm