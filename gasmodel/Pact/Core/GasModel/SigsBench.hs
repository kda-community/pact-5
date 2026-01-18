--{-# OPTIONS_GHC -Wno-unused-top-binds #-}
--{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pact.Core.GasModel.SigsBench
  (benchmarks) where

import qualified Criterion as C

import Data.Either (fromRight)
import Pact.Core.Command.Crypto
import Control.DeepSeq
import Pact.Core.Command.Types
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Base16 as B16
import Pact.Core.Hash
import System.Random
import Data.IORef
import Data.ByteString
import Data.Vector as V
import Data.Text.Encoding as TE

-- We want a reproducible random generator
rndSeed:: Int
rndSeed = 140605

benchmarks :: C.Benchmark
benchmarks = C.bgroup "Signatures" [ed25519Benchmark, slhDsaBenchmark]

data SigStore = SigStore
  { sigs :: V.Vector (UserSig, Hash)
  , idx :: IORef Int
  }

instance NFData SigStore where
  rnf (SigStore sigs' _) = rnf sigs'

-- Create a a set of 1000 random signatures and put it into a Vector
setupSigs :: PPKScheme -> Bool -> IO SigStore
setupSigs scheme goodSig= SigStore (V.unfoldrN 1000 buildSig $ mkStdGen rndSeed) <$> newIORef 0
  where
    buildSig:: StdGen -> Maybe ( (UserSig, Hash), StdGen)
    buildSig rnd = case (scheme, goodSig) of
                      (ED25519, True) -> let (h, rnd') =  uniformByteString size rnd
                                             h' = Hash $ SB.toShort h
                                             s = exportEd25519Signature $ signEd25519 (fst ed25519KeyPair) (snd ed25519KeyPair) h'
                                          in Just ( (encodeSig s, h'), rnd')

                      _ ->  let (s, rnd') =  uniformByteString size rnd
                                (h, rnd'') =  uniformByteString 32 rnd'
                             in Just ( (encodeSig s, Hash $ SB.toShort h ), rnd'')

    encodeSig:: ByteString -> UserSig
    encodeSig = case scheme of
        ED25519 -> PlainSig . TE.decodeUtf8Lenient . B16.encode
        SlhDsaSha128s -> PlainSig . toB64UrlUnpaddedText
        SlhDsaSha192s -> PlainSig . toB64UrlUnpaddedText
        SlhDsaSha256s -> PlainSig . toB64UrlUnpaddedText
        _ -> error "We only do SLH tests here"

    size = case scheme of
      ED25519 -> 64
      SlhDsaSha128s -> 7856
      SlhDsaSha192s -> 16224
      SlhDsaSha256s -> 29792
      _ -> error "We only do SLH tests here"


-- Get a signature fromt he store and increment pointer
getSigMaterial :: SigStore -> IO (UserSig, Hash)
getSigMaterial store = do
  currentIdx <- readIORef $ idx store
  writeIORef (idx store) $ (currentIdx + 1) `mod` (V.length $ sigs store)
  return $ sigs store ! currentIdx

slhTest:: SigStore -> PPKScheme -> IO ( Either String ())
slhTest store scheme = do
    (sig, testHash) <- getSigMaterial store
    return $ verifyUserSig  testHash sig key

  where
    key = case scheme of
      ED25519 -> ed25519Key
      SlhDsaSha128s -> slhKey0
      SlhDsaSha192s -> slhkey1
      SlhDsaSha256s -> slhkey2
      _ -> error "We only do SLH tests here"


ed25519Benchmark:: C.Benchmark
ed25519Benchmark =  C.bgroup "ED25519" [ C.env (setupSigs ED25519 True)       (\store ->  C.bench "ED25519 Good Sig"      $ C.nfIO (slhTest store ED25519))
                                       , C.env (setupSigs ED25519 False)       (\store ->  C.bench "ED25519 Bad Sig"      $ C.nfIO (slhTest store ED25519))
                                       ]

slhDsaBenchmark:: C.Benchmark
slhDsaBenchmark = C.bgroup "SLH DSA" [ C.env (setupSigs SlhDsaSha128s False ) (\store ->  C.bench "SLH-DSA-128s" $ C.nfIO (slhTest store SlhDsaSha128s))
                                     , C.env (setupSigs SlhDsaSha192s False ) (\store ->  C.bench "SLH-DSA-192s" $ C.nfIO (slhTest store SlhDsaSha192s))
                                     , C.env (setupSigs SlhDsaSha256s False) (\store ->  C.bench "SLH-DSA-256s" $ C.nfIO (slhTest store SlhDsaSha256s))
                                     ]

ed25519Key:: Signer
ed25519Key = Signer (Just ED25519) "b663516a3b3d953adc773cab734877fd49175fe4dd4e1003208f994017b8297c" Nothing []

ed25519KeyPair:: Ed25519KeyPair
ed25519KeyPair = fromRight (error "InvalidKey") $ importEd25519KeyPair Nothing $ PrivBS $ B16.decodeLenient "013f25be7fede27344ff45271dbce4d4fc8f8d429daae44101558cdb0cad3d32"


slhKey0:: Signer
slhKey0 = Signer (Just SlhDsaSha128s) "8e675391075de70e10ab6d5401c5a04dea29131e47c7c616e127103e03b54a74" Nothing []

slhkey1:: Signer
slhkey1 = Signer (Just SlhDsaSha192s) "e1fb904a38f84a71eaef029faf0d94cda83f3e784ee2874eecf12b96ba87af9eff5c887ff5eaddf10337c6ee7b8b5250" Nothing []

slhkey2:: Signer
slhkey2 = Signer (Just SlhDsaSha256s) "3cd2a457bdd846889bca6b48c1a9df3e38da37b1d9ded2b934a968abe498abdc2ae5477dafd5cda8383d6dba43d203a6e1f336793d35df9024e10be6bbd856c5" Nothing []
