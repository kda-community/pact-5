{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}


module Pact.Core.Scheme
  ( PPKScheme(..)
  , defPPKScheme
  ) where

import GHC.Generics
import Control.DeepSeq

import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J


--------- PPKSCHEME DATA TYPE ---------

data PPKScheme = ED25519 | WebAuthn | SlhDsaSha128s | SlhDsaSha192s | SlhDsaSha256s
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance NFData PPKScheme

instance J.Encode PPKScheme where
  build ED25519 = J.text "ED25519"
  build WebAuthn = J.text "WebAuthn"
  build SlhDsaSha128s = J.text "SLH-DSA-SHA2-128s"
  build SlhDsaSha192s = J.text "SLH-DSA-SHA2-192s"
  build SlhDsaSha256s = J.text "SLH-DSA-SHA2-256s"
  {-# INLINE build #-}

instance JD.FromJSON PPKScheme where
  parseJSON = JD.withText "PPKScheme" $ \case
    "ED25519" -> pure ED25519
    "WebAuthn" -> pure WebAuthn
    "SLH-DSA-SHA2-128s" -> pure SlhDsaSha128s
    "SLH-DSA-SHA2-192s" -> pure SlhDsaSha192s
    "SLH-DSA-SHA2-256s" -> pure SlhDsaSha256s
    _ -> fail "Invalid PPKScheme"

defPPKScheme :: PPKScheme
defPPKScheme = ED25519