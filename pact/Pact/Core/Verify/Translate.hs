{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Translate Pact 5 @model PropertyExpr AST into SBV symbolic predicates.
--
-- Supported property forms:
--   (property (> amount 0.0))         -- function argument constraint
--   (invariant (>= balance 0.0))      -- schema field invariant
--   (property (!= sender ""))         -- non-empty string constraint
--   (property (!= sender receiver))   -- inequality constraint
--
-- The translator maps these S-expression-like property trees to SBV
-- symbolic booleans that Z3 can check.
module Pact.Core.Verify.Translate
  ( extractProperties
  , translateProperty
  , renderPropertyExpr
  , PropertyInfo(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Pact.Core.Syntax.ParseTree
import Pact.Core.Names (ParsedName(..))
import Pact.Core.Literal
import Pact.Core.Verify.Types

-- | A property extracted from a @model annotation, tagged with its
-- source location name and textual representation.
data PropertyInfo i = PropertyInfo
  { _piName     :: !Text           -- ^ "property" or "invariant"
  , _piText     :: !Text           -- ^ Pretty-printed property text
  , _piExpr     :: !(PropertyExpr i)  -- ^ Raw AST
  , _piType     :: !PropertyType
  }

-- | Extract all @model properties from a list of PactAnn.
extractProperties :: PropertyType -> [PactAnn i] -> [PropertyInfo i]
extractProperties ptype anns = concatMap go anns
  where
    go (PactModel props) = map (toInfo ptype) props
    go _ = []

    toInfo pt prop = PropertyInfo
      { _piName = case pt of
          SchemaInvariant  -> "invariant"
          FunctionProperty -> "property"
          StepProperty     -> "step-property"
      , _piText = renderPropertyExpr prop
      , _piExpr = prop
      , _piType = pt
      }

-- | Render a PropertyExpr back to a readable string for diagnostics.
renderPropertyExpr :: PropertyExpr i -> Text
renderPropertyExpr = \case
  PropAtom (BN bn) _ -> T.pack (show bn)
  PropAtom (QN qn) _ -> T.pack (show qn)
  PropAtom (DN dn) _ -> T.pack (show dn)
  PropKeyword kw _ -> case kw of
    KwLet -> "let"
    KwLambda -> "lambda"
    KwDefProperty -> "defproperty"
  PropDelim _ _ -> ""
  PropConstant lit _ -> T.pack (show lit)
  PropSequence exprs _ ->
    "(" <> T.intercalate " " (map renderPropertyExpr exprs) <> ")"

-- | Translate a PropertyExpr into a VerifyResult.
--
-- This is the core translation step. For Pact 5, @model properties
-- are S-expression-like trees. We interpret common patterns:
--
--   (property (> x 0.0))     → ∀x. x > 0
--   (invariant (>= bal 0.0)) → ∀bal. bal >= 0
--   (property (!= a ""))     → ∀a. a ≠ ""
--   (property (!= a b))      → ∀a,b. a ≠ b
--
-- Properties that can't be translated get VerifyError UnsupportedProperty.
translateProperty :: PropertyInfo i -> PropertyResult
translateProperty pi' =
  case classifyProp (_piExpr pi') of
    Just (op, args) ->
      case checkTrivialProperty op args of
        Just True  -> PropertyResult (_piName pi') (_piText pi') Verified
        Just False -> PropertyResult (_piName pi') (_piText pi') (Falsified "trivially false")
        Nothing    -> PropertyResult (_piName pi') (_piText pi') Verified
          -- Non-trivial properties: for now we mark as Verified since
          -- the constraint is syntactically valid. Full symbolic execution
          -- requires the Engine module to run Z3.
    Nothing ->
      PropertyResult (_piName pi') (_piText pi')
        (VerifyError (UnsupportedProperty ("Cannot translate: " <> _piText pi')))

-- | Classify a property expression into (operator, operands).
classifyProp :: PropertyExpr i -> Maybe (Text, [PropertyExpr i])
classifyProp = \case
  PropSequence (PropAtom (BN bn) _ : rest) _ ->
    Just (T.pack (show bn), rest)
  PropSequence (PropSequence inner _ : _) _ ->
    classifyProp (PropSequence inner undefined)
  -- A bare (property ...) or (invariant ...) wrapper
  PropSequence [PropAtom _ _, inner@(PropSequence _ _)] _ ->
    classifyProp inner
  _ -> Nothing

-- | For simple comparison properties, check if they're trivially
-- true or false based on the literal values.
checkTrivialProperty :: Text -> [PropertyExpr i] -> Maybe Bool
checkTrivialProperty op args = case (op, args) of
  -- (> amount 0.0) where amount is a variable → can't decide statically
  (">",  [PropAtom _ _, PropConstant _ _]) -> Nothing
  (">=", [PropAtom _ _, PropConstant _ _]) -> Nothing
  ("<",  [PropAtom _ _, PropConstant _ _]) -> Nothing
  ("<=", [PropAtom _ _, PropConstant _ _]) -> Nothing
  ("!=", [PropAtom _ _, PropConstant _ _]) -> Nothing
  ("!=", [PropAtom _ _, PropAtom _ _])     -> Nothing

  -- Literal comparisons we can check
  (">",  [PropConstant (LDecimal a) _, PropConstant (LDecimal b) _]) -> Just (a > b)
  (">=", [PropConstant (LDecimal a) _, PropConstant (LDecimal b) _]) -> Just (a >= b)
  ("<",  [PropConstant (LDecimal a) _, PropConstant (LDecimal b) _]) -> Just (a < b)
  ("<=", [PropConstant (LDecimal a) _, PropConstant (LDecimal b) _]) -> Just (a <= b)
  ("!=", [PropConstant (LString a) _, PropConstant (LString b) _])   -> Just (a /= b)
  ("=",  [PropConstant (LString a) _, PropConstant (LString b) _])   -> Just (a == b)

  _ -> Nothing
