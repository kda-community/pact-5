{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for the Pact 5 formal verification engine.
--
-- Translates @model annotations (PropertyExpr AST) into a form
-- that can be checked by an SMT solver (Z3 via SBV).
module Pact.Core.Verify.Types
  ( VerifyResult(..)
  , PropertyType(..)
  , VerifyError(..)
  , ModuleVerifyResult(..)
  , PropertyResult(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Result of verifying a single property.
data PropertyResult = PropertyResult
  { _prName       :: !Text       -- ^ Function or schema name
  , _prProperty   :: !Text       -- ^ Property expression as text
  , _prResult     :: !VerifyResult
  } deriving (Show, Eq, Generic)

-- | Outcome of a single property check.
data VerifyResult
  = Verified                      -- ^ Z3 proved the property holds
  | Falsified !Text               -- ^ Z3 found a counterexample
  | VerifyTimeout                 -- ^ Solver timed out
  | VerifyError !VerifyError      -- ^ Translation or solver error
  deriving (Show, Eq, Generic)

-- | Classification of property.
data PropertyType
  = SchemaInvariant               -- ^ @model invariant on defschema
  | FunctionProperty              -- ^ @model property on defun
  | StepProperty                  -- ^ @model property on defpact step
  deriving (Show, Eq, Generic)

-- | Errors that can occur during verification.
data VerifyError
  = UnsupportedProperty !Text     -- ^ Property uses unsupported syntax
  | TranslationError !Text        -- ^ Could not translate to SMT
  | SolverError !Text             -- ^ Z3 returned an error
  deriving (Show, Eq, Generic)

-- | Aggregated result for an entire module.
data ModuleVerifyResult = ModuleVerifyResult
  { _mvrModule     :: !Text
  , _mvrResults    :: ![PropertyResult]
  , _mvrAllPassed  :: !Bool
  } deriving (Show, Eq, Generic)
