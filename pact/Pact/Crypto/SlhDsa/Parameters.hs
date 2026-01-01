module Pact.Crypto.SlhDsa.Parameters
( Parameter
, slh_dsa_sha2_128s
, slh_dsa_sha2_192s
, slh_dsa_sha2_256s
, n, h, d, h', a, k, m
, toSeed, toRoot
, fips205Hmsg, fips205F, fips205H, fips205Tl
) where

import Data.ByteString.Short as SB
import Pact.Crypto.SlhDsa.Addresses
import Pact.Crypto.SlhDsa.Utils

-------------------------------------------------------------------------------
-- FIPS - 205 §11 - Parameter Sets
-------------------------------------------------------------------------------

data Parameter = Parameter { n:: Int
                           , h:: Int
                           , d:: Int
                           , h':: Int
                           , a:: Int
                           , k:: Int
                           , m:: Int
                           , toSeed:: PublickKey -> PublicKeySeed
                           , toRoot:: PublickKey -> PublicKeyRoot
                           , fips205Hmsg:: ShortByteString -> PublickKey -> Message                         -> MessageDigest
                           , fips205F::    PublicKeySeed   -> Address -> ShortByteString                    -> ShortByteString
                           , fips205H::    PublicKeySeed   -> Address -> ShortByteString -> ShortByteString -> ShortByteString
                           , fips205Tl::   PublicKeySeed   -> Address -> [ShortByteString]                  -> ShortByteString
                           }

slh_dsa_sha2_128s:: Parameter
slh_dsa_sha2_128s = Parameter n' -- -> n
                              63 -- -> h
                              7  -- -> d
                              9  -- -> h'
                              12 -- -> a
                              14 -- -> k
                              m' -- -> m
                              toSeed' -- -> toSeed
                              toRoot' -- -> toRoot
                              (\r pk msg       -> mfg1Sha sha256 m' [r, toSeed' pk, sha256 [r, toSeed' pk, toRoot' pk, msg]]) -- -> Hmsg
                              (\pks addr mA    -> sphincsSha256 n' pks addr [mA]) -- -> F
                              (\pks addr mA mB -> sphincsSha256 n' pks addr [mA, mB]) -- -> H
                              (sphincsSha256 n')  -- -> Tl
                        where n' = 16
                              m' = 30
                              toSeed' = SB.take n'
                              toRoot' = SB.takeEnd n'

slh_dsa_sha2_192s:: Parameter
slh_dsa_sha2_192s = Parameter n' -- -> n
                              63 -- -> h
                              7  -- -> d
                              9  -- -> h'
                              14 -- -> a
                              17 -- -> k
                              m' -- -> m
                              toSeed' -- -> toSeed
                              toRoot' -- -> toRoot
                              (\r pk msg       -> mfg1Sha sha512 m' [r, toSeed' pk, sha512 [r, toSeed' pk, toRoot' pk, msg]]) -- -> Hmsg
                              (\pks addr mA    -> sphincsSha256 n' pks addr [mA]) -- -> F
                              (\pks addr mA mB -> sphincsSha512 n' pks addr [mA, mB]) -- -> H
                              (sphincsSha512 n')  -- -> Tl
                        where n' = 24
                              m' = 39
                              toSeed' = SB.take n'
                              toRoot' = SB.takeEnd n'

slh_dsa_sha2_256s:: Parameter
slh_dsa_sha2_256s = Parameter n' -- -> n
                              64 -- -> h
                              8  -- -> d
                              8  -- -> h'
                              14 -- -> a
                              22 -- -> k
                              m' -- -> m
                              toSeed' -- -> toSeed
                              toRoot' -- -> toRoot
                              (\r pk msg       -> mfg1Sha sha512 m' [r, toSeed' pk, sha512 [r, toSeed' pk, toRoot' pk, msg]]) -- -> Hmsg
                              (\pks addr mA    -> sphincsSha256 n' pks addr [mA]) -- -> F
                              (\pks addr mA mB -> sphincsSha512 n' pks addr [mA, mB]) -- -> H
                              (sphincsSha512 n')  -- -> Tl
                        where n' = 32
                              m' = 47
                              toSeed' = SB.take n'
                              toRoot' = SB.takeEnd n'
