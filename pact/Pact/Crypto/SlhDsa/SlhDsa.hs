{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Pact.Crypto.SlhDsa.SlhDsa
( verifySignatureRaw
, verifySignaturePureWithContext
, verifySignaturePreHashedWithContext
) where

import Prelude hiding (Foldable(..))
import Data.Foldable (Foldable(..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SB
import qualified Data.Vector as V
import Data.Bits
--import Data.Function (applyWhen)

import Pact.Crypto.SlhDsa.Parameters
import Pact.Crypto.SlhDsa.MessageDigest
import Pact.Crypto.SlhDsa.Utils
import Pact.Crypto.SlhDsa.Signature
import Pact.Crypto.SlhDsa.Addresses


keyChecked :: Parameter -> PublickKey -> Either String PublickKey
keyChecked prm key
    | SB.length key == (n prm *2) = Right key
    | otherwise = Left "Invalid key"

-------------------------------------------------------------------------------
-- FIPS - 205 §5 - Winternitz One-Time Signature Plus Scheme
-------------------------------------------------------------------------------
-- FIPS-205 Equations 5.1, 5.2, 5.3, 5.4
wotsLgw :: Int
wotsLgw = 4

wotsW :: Int
wotsW = 16

wotsLen1 :: Parameter -> Int
wotsLen1 = (*2) . n

wotsLen2 :: Int
wotsLen2 = 3

wotsLenT :: Parameter -> Int
wotsLenT = (+ wotsLen2) . wotsLen1

-- FIPS 205 §5.0 - Algorithm 5
wotsChain :: Parameter -> PublicKeySeed -> Address -> Int -> Int -> ShortByteString -> ShortByteString
wotsChain prm pks addr start count = go 0
    where go:: Int -> ShortByteString-> ShortByteString
          go j x
            | j == count = x
            | otherwise = go (j+1) $ fips205F prm pks addr{ha = fromIntegral $ start + j} x


-- FIPS-205 §5.3 - Algorithm 8

-- line 7 base2b (bytes x) = Simplified fro the specific checksum case
-- We take the 3 first nibbles from a Word16
checksumEncode :: Int -> [Int]
checksumEncode x = map ((.&. 0x0f) . shiftR x) [12, 8, 4]

computeCheckSum :: [Int] -> [Int]
computeCheckSum = checksumEncode . (\x -> shiftL x 4) . foldl' (\acc x -> acc + wotsW - 1 - x) 0

addCheckSum :: [Int] -> [Int]
addCheckSum x = x ++ computeCheckSum x

wotsPkFromSig :: Parameter -> PublicKeySeed -> Address -> WotsSignature -> ShortByteString -> ShortByteString
wotsPkFromSig prm pks addr sig = compressPubKey . zipWith computePubKey [0..] . encodeMsg
        where
            encodeMsg = addCheckSum . take (wotsLen1 prm) . toBase2N wotsLgw
            computePubKey i msg = wotsChain prm pks addr{ca=fromIntegral i} msg (wotsW - 1 - msg) $ sig V.! i
            compressPubKey = fips205Tl prm pks $ toWOTSPKAddress addr

-------------------------------------------------------------------------------
-- FIPS - 205 §6 - eXtended Merkle Signature Scheme
-------------------------------------------------------------------------------
-- FIPS-205 §6.3 - Algorithm 11
xmssComputeRoot :: Parameter -> PublicKeySeed -> XmmsSignature -> Address -> Int -> Node -> Int -> Node
xmssComputeRoot prm pks sig addr index node iK
  | h' prm == iK = node
  | otherwise = xmssComputeRoot prm pks sig addr' (shiftR index 1) node' $ iK + 1
        where
            addr' = updateMerkleTreeIndex addr $ even index
            auth = getXmssAuth prm sig iK
            node' | even index = fips205H prm pks addr' node auth
                  | otherwise = fips205H prm pks addr' auth node


xmssPkFromSig :: Parameter -> PublicKeySeed -> Address -> Int -> XmmsSignature -> ShortByteString -> Node
xmssPkFromSig prm pks addr index sig msg = xmssComputeRoot prm pks sig htAddr index initNode 0
        where
            wAddr = (toWOTSHashAddress addr $ fromIntegral index)
            htAddr = (toHashTreeAddress addr $ fromIntegral index)
            initNode = wotsPkFromSig prm pks wAddr sig msg


-------------------------------------------------------------------------------
-- FIPS - 205 §7 - The SLH-DSA Hypertree
-------------------------------------------------------------------------------
-- FIPS-205 §7.2 - Algorithm 13
hyperTreePkFromSig :: Parameter -> PublicKeySeed -> HTSignature ->  Int -> Int -> Node -> PublicKeyRoot
hyperTreePkFromSig prm pks sig = go 0
  where go j indexTree indexLeaf node
          | d prm == j = node
          | otherwise = go (j + 1) indexTree' indexLeaf' $ xmssPkFromSig prm pks addr indexLeaf' (getXmss prm sig j) node
              where addr = (BaseAddress (fromIntegral j) (fromIntegral indexTree'))
                    indexTree'
                        | j == 0 = indexTree
                        | otherwise = shiftR indexTree $ h' prm
                    indexLeaf'
                        | j == 0 = indexLeaf
                        | otherwise = mod2n (h' prm) indexTree

-------------------------------------------------------------------------------
-- FIPS - 205 §8 - Forest of Random Subsets
-------------------------------------------------------------------------------
-- FIPS-205 §8.4 - Algorithm 17
type MsgIndex = Int

-- ComputeRoot inner loop fuction
forsComputeRoot' :: Parameter -> PublicKeySeed -> ForsAUTH ->  Address -> MsgIndex -> Node -> Node
forsComputeRoot' prm pks forsAUTH = go 0
  where go:: Int -> Address -> MsgIndex -> Node  -> Node
        go j addr index node
            | a prm == j = node
            | otherwise = go (j + 1)  addr' (shiftR index 1) node'
                where
                    addr' = updateMerkleTreeIndex addr $ even index
                    auth = forsAUTH V.! j
                    node' | even index = fips205H prm pks addr' node auth
                          | otherwise  = fips205H prm pks addr' auth node

forsComputeRoot :: Parameter -> PublicKeySeed -> ForsSignature -> Address -> Int -> MsgIndex -> Node
forsComputeRoot prm pks sig addr i index = forsComputeRoot' prm pks (getForsAUTH prm sig i) addr' index initNode
    where
        addr' = addr{th=0, ti = fromIntegral $ shiftL i (a prm) + index}
        initNode = fips205F prm pks addr' $ getForsSK prm sig i

forsPkFromSig :: Parameter -> PublicKeySeed -> ForsSignature -> Address -> MessageDigest -> ShortByteString
forsPkFromSig prm pks sig address = fips205Tl prm pks (toForsRootAddress address) . zipWith (forsComputeRoot prm pks sig address) [0..] . take (k prm) . toBase2N (a prm)


-------------------------------------------------------------------------------
-- FIPS - 205 §9 - SLH-DSA Internal Functions
-------------------------------------------------------------------------------
-- FIPS-205 §9.2 - Algorithm 19
slhDsaPkFromSig :: Parameter -> PublickKey -> Signature  -> Message -> PublicKeyRoot
slhDsaPkFromSig prm pubkey sig msg = hyperTreePkFromSig' $ forsPkFromSig' $ forsDigest prm digest
    where
        digest = fips205Hmsg prm (getR sig) pubkey msg
        pks = toSeed prm pubkey
        idxTree = treeIndex prm digest
        idxLeaf = leafIndex prm digest
        forsAddr = ForsTreeAddress 0 (fromIntegral idxTree) (fromIntegral idxLeaf) 0 0
        hyperTreePkFromSig' = hyperTreePkFromSig prm pks (getHT prm sig) idxTree idxLeaf
        forsPkFromSig' = forsPkFromSig prm pks (getFors prm sig) forsAddr


-- Check that the recovers PublickeyRoot match with the known one
checkPubKeyMatch :: Parameter -> PublickKey -> PublicKeyRoot -> Either String ()
checkPubKeyMatch prm pubkey pkr
    | pkr == toRoot prm pubkey = Right ()
    | otherwise = Left "Signature verification failed"


-------------------------------------------------------------------------------
-- FIPS - 205 §10 - SLH-DSA External Functions
-------------------------------------------------------------------------------
-- FIPS-205 §10.2.1 and §10.2.2 - Algorithms 22 and 23
prepareMessage :: SigContext -> EncodedOID -> Message -> Either String Message
prepareMessage context oid msg
    | SB.length context > 255 = Left "Wrong context length"
    | otherwise = Right $ SB.concat [ SB.singleton tag,  SB.singleton contextLen, context, oid,  msg]
        where
          tag | oid == SB.empty = 0
              | otherwise = 1
          contextLen = fromIntegral $ SB.length context

preparePureMessage :: SigContext -> Message -> Either String Message
preparePureMessage context = prepareMessage context SB.empty


-------------------------------------------------------------------------------
-- External functions
-------------------------------------------------------------------------------
-- According to the old SPHINCS / FIPS-205 spec
verifySignatureRaw :: Parameter -> PublickKey -> RawSignature -> Message -> Either String ()
verifySignatureRaw prm pkey rawSig msg = do
    pkey' <- keyChecked prm pkey
    sig   <- toSignatureChecked prm rawSig
    checkPubKeyMatch prm pkey' $ slhDsaPkFromSig prm pkey' sig msg

-- Verify a signature with context and wrappping according to the FIP-25 in force
verifySignaturePureWithContext :: Parameter -> SigContext -> PublickKey -> RawSignature -> Message -> Either String ()
verifySignaturePureWithContext prm ctx pkey rawSig msg = do
    pkey' <- keyChecked prm pkey
    sig   <- toSignatureChecked prm rawSig
    msg'  <- preparePureMessage ctx msg
    checkPubKeyMatch prm pkey' $ slhDsaPkFromSig prm pkey' sig msg'

-- Verify a pre-hashed signature with context and wrappping according to the FIP-25 in force
-- We assume the msg comes pre-hashed
verifySignaturePreHashedWithContext :: Parameter -> SigContext -> EncodedOID -> PublickKey -> RawSignature -> Message -> Either String ()
verifySignaturePreHashedWithContext prm ctx oid pkey rawSig msg = do
    pkey' <- keyChecked prm pkey
    sig   <- toSignatureChecked prm rawSig
    msg'  <- prepareMessage ctx oid msg
    checkPubKeyMatch prm pkey' $ slhDsaPkFromSig prm pkey' sig msg'