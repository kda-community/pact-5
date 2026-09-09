# Pact 5 Post-Quantum (SLH-DSA) Integration & Upstream Readiness Report

**To:** CryptoPascal31 & the KDA Community Core Contributors  
**From:** not_bob & seal_klub (`long_live the empire`)  
**Date:** September 9, 2026  
**Subject:** Full Post-Quantum SLH-DSA (FIPS 205) Runtime, Account Principals (`q:`, `x:`), Gas Models, and Synchronized 5.4.1 Fixes  
**Repository Source:** [`pact-5-5.4.1-pq_unified`](/run/media/notbob/storage/projects/tesnetv2_0/research_ideas/pact-5-5.4.1-pq_unified)

---

## 📌 Executive Summary

This codebase provides the complete, production-hardened Post-Quantum implementation of **Pact 5** supporting the NIST FIPS 205 **SLH-DSA (SPHINCS+)** hash-based signature standard.

It provides full alignment with **`Chainweb33`** hard-fork consensus rules, resolves active challenges in `chainweb-node` PRs (#43, #44, #45), and incorporates all upstream bugfixes from Pact `5.4.1`.

---

## 🏛️ 1. Core Post-Quantum Cryptographic Architecture

### 1.1 NIST FIPS 205 SLH-DSA Cryptographic Engine
Located in `pact/Pact/Crypto/SlhDsa/`:
* **`Parameters.hs`**: Parameter sets for `SLH-DSA-SHA2-128s` ($n=16$), `SLH-DSA-SHA2-192s` ($n=24$), and `SLH-DSA-SHA2-256s` ($n=32$).
* **`SlhDsa.hs`**: Pure Haskell reference implementation of FORS (Forest of Random Subsets), WOTS+ (Winternitz One-Time Signatures), and Hypertree Merkle trees.
* **`ChainwebSlhDsa.hs`**: Chainweb Blake2 OID framing context (`CHAINWEB` prefix with domain-separated hash prefixes) and strict signature verification (`verifySig`).
* **`Utils.hs`**: Big-endian address conversions and constant-time comparisons.
* **`Types.hs`**: Strictly typed keys, signatures, and addresses.

### 1.2 Signature Schemes (`PPKScheme`)
In `pact/Pact/Core/Scheme.hs`:
```haskell
data PPKScheme = ED25519 | WebAuthn | SlhDsaSha128s | SlhDsaSha192s | SlhDsaSha256s
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)
```
* Custom JSON serializers/deserializers for `"SLH-DSA-SHA2-128s"`, `"SLH-DSA-SHA2-192s"`, and `"SLH-DSA-SHA2-256s"`.
* Strict unpadded Base64URL decoding for PQ wire signatures.

---

## 🔑 2. Account Principals & Keyset Validation

### 2.1 Post-Quantum Account Principals
In `pact/Pact/Core/Principal.hs`:
* **`q:` (Single-Key SLH-DSA Account)**: `q:<hex-encoded-pubkey>`
* **`x:` (Multi-Sig SLH-DSA Keyset Account)**: `x:<b64url-hash>:<keyset-pred>`
* **Dynamic Parser Gating**: `principalParser :: Bool -> Parser Principal` gates parsing of `q:` and `x:` accounts via `FlagDisableSlhDsaSignatures` to guarantee zero behavior divergence prior to the hard fork height.

### 2.2 Keyset Formatting Rules
In `pact/Pact/Core/Guards.hs`:
* `slhKeyFormat`: Validates keys with `q` prefix and length 64 (128s), 96 (192s), or 128 (256s) hex characters.
* `enforceKeyFormats`: Dual enforcement supporting legacy keys and post-quantum keysets.

---

## ⛽ 3. Gas Modeling & Benchmarking

### 3.1 Gas Types Enhancement
In `pact/Pact/Core/Gas/Types.hs`:
* `newtype Gas` derives `(Enum, Bounded) via SatWord` for seamless node integration.

### 3.2 Signature Verification Benchmarking
In `gasmodel/Pact/Core/GasModel/SigsBench.hs`:
* Criterion benchmarking suite evaluating verification costs across `ED25519`, `WebAuthn`, and `SLH-DSA (128s/192s/256s)`.
* Fully calibrated to match `chainweb-node`'s `post33GasModel`:
  * **ED25519**: 100 Gas
  * **SLH-DSA-SHA2-128s**: 816 Gas
  * **SLH-DSA-SHA2-192s**: 1,584 Gas
  * **SLH-DSA-SHA2-256s**: 3,992 Gas

---

## 🛡️ 4. Merged Upstream 5.4.1 Security Bugfixes

This clean tree includes the latest security and stability fixes from upstream Pact 5.4.1:

1. **Capability Governance Guard in `composeCap` (`Evaluator.hs`)**:
   Preserves `guardForModuleCall info (_fqModule capFqn)` check to prevent unauthorized module governance escalation during capability composition.
2. **Dual Execution Flags (`Environment/Types.hs`)**:
   Includes both `FlagDisablePact54Fix` (for 5.4 governance bugfix gating) and `FlagDisableSlhDsaSignatures` (for post-quantum hard-fork gating).
3. **Regression REPL Tests**:
   Synchronized `compose-caps-bug.repl` and `webauthn.repl`.

---

## 📦 5. Repository Cleanup & Verification Status

* **No Local Tooling / VCS Junk**: Completely stripped of `.jj/`, `.cosine_rules`, `.cosineignore`, and compilation cache.
* **Metadata**: Updated `Version.hs` (`Version [5, 4, 1]`) and `pact-tng.cabal` (v5.4.1.0).
* **Test Fixtures**: Full ACVP NIST test vectors and Golden Gas tests in `pact-tests/`.

---

## 🎖️ Credits Section Included in Repository
* **not_bob & seal_klub** (`long_live the empire`)
* **CryptoPascal31 (Pascal)**
* **KadenaFriend (`kdafriend`)**
* **Edmund Noble (`edmundnoble`)**
* **Jose Cardona (`jmcardon`)**
* **Dennis (`dnns-es`)**
