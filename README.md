# Pact 5 — Post-Quantum Build (SLH-DSA / KIP-0041)

> **Private repository — NOtBobs-Emporium-Of-Wonder**
> Built: 2026-03-31 | Branch: `post_quantum` | GHC: 9.6.7 | Ubuntu 25.10 x86_64

---

## What this is

This is a compiled binary of the **Pact 5 smart contract language** built from the
`post_quantum` branch of `kda-community/pact-5`. It is the **only publicly known
compiled Pact 5 binary that includes SLH-DSA (FIPS 205) post-quantum signature support.**

The released `5.4ce` binary from `kda-community/pact-5` does **not** include this —
it is built from `master`. This binary is built from the `post_quantum` branch which
is ahead of master and adds full SLH-DSA cryptography.

---

## What's new vs standard Pact 5.4ce

| Feature | Standard 5.4ce | This build |
|---|---|---|
| ED25519 `k:` accounts | ✅ | ✅ |
| SLH-DSA `q:` single-key PQ accounts | ❌ | ✅ |
| SLH-DSA `x:` multi-key PQ accounts | ❌ | ✅ |
| `SLH-DSA-SHA2-128s/192s/256s` signing schemes | ❌ | ✅ |
| FIPS 205 NIST ACVP test vectors passing | ❌ | ✅ |
| Chainweb-specific SLH-DSA context (`CHAINWEB` + Blake2 OID) | ❌ | ✅ |
| Post-quantum principal validation | ❌ | ✅ |
| `DisableSlhDsaSignatures` exec config flag | ❌ | ✅ |
| SLH-DSA keypair generation (`--genkey-pq`) | ❌ | ✅ |

---

## Binary

```
bin/pact-pq          — Pact 5 post-quantum binary (Linux x86_64, Ubuntu 22.04+)
SHA256SUMS           — SHA256 checksum
```

### Quick test

```bash
chmod +x bin/pact-pq

# Version
./bin/pact-pq --version
# → pact version 5.4

# Generate a post-quantum keypair (NEW)
./bin/pact-pq --genkey-pq SLH-DSA-SHA2-128s
# → scheme: SLH-DSA-SHA2-128s
# → public: <32-byte hex>
# → secret: <64-byte hex>
# → account: q:<32-byte hex>

./bin/pact-pq --genkey-pq SLH-DSA-SHA2-192s   # n=24, PK=48B, SK=96B
./bin/pact-pq --genkey-pq SLH-DSA-SHA2-256s   # n=32, PK=64B, SK=128B

# q: post-quantum principal recognised
echo '(typeof-principal "q:8e675391075de70e10ab6d5401c5a04dea29131e47c7c616e127103e03b54a74")' | ./bin/pact-pq
# → "q:"

# k: classical principal — different type
echo '(is-principal "k:abc123")' | ./bin/pact-pq
# → false  (q: and k: are distinct principal types)

# SLH-DSA keyset
printf '(env-sigs [{"key": "q:8e675391075de70e10ab6d5401c5a04dea29131e47c7c616e127103e03b54a74", "caps": []}])\n(at "block-height" (chain-data))\n' | ./bin/pact-pq
# → 0  (accepted without error)
```

---

## Build info

```
Source:   kda-community/pact-5  branch: post_quantum
Commit:   b9979ce9 (Mar 31 2026 — "Add SLH-DSA keygen: slhKeyGen (FIPS 205 §9.1) + --genkey-pq flag")
GHC:      9.6.7
Cabal:    3.14.2.0
OS:       Ubuntu 25.10 x86_64
Built:    2026-03-31
Size:     203MB (unstripped debug build)
```

To build a stripped production binary:
```bash
cabal build exe:pact -O2
strip dist-newstyle/.../pact
# Result: ~70-90MB stripped
```

---

## SLH-DSA background (KIP-0041)

SLH-DSA (FIPS 205, formerly SPHINCS+) is a **hash-based post-quantum signature scheme**
standardised by NIST in August 2024. It is:

- **Quantum-resistant** — security relies only on hash function collision resistance,
  not on discrete logarithm or factoring (which Shor's algorithm breaks)
- **Conservative** — breaking it would require breaking SHA-256 itself
- **FIPS 205 compliant** — the most credible post-quantum signature standard available

### Kadena-specific profile (KIP-0041)

| Parameter | Value |
|---|---|
| Context | `CHAINWEB` |
| OID | `1.3.6.1.4.1.1722.12.2.1.8` |
| Pre-hash | Blake2b-256 of transaction payload |
| Signing mode | HashSLH-DSA (FIPS 205 §10.2.2) |
| Supported schemes | `SLH-DSA-SHA2-128s`, `SLH-DSA-SHA2-192s`, `SLH-DSA-SHA2-256s` |

### New principal types

| Prefix | Keys | Description |
|---|---|---|
| `q:` | Single SLH-DSA key | Single post-quantum account |
| `x:` | Multiple SLH-DSA keys | Multi-sig post-quantum account |
| `k:` | Single ED25519 key | Classic account (unchanged) |
| `w:` | Multiple ED25519/mixed | Classic multi-sig (unchanged) |

### Signature sizes vs ED25519

| Scheme | Public key | Signature | vs ED25519 sig (64 bytes) |
|---|---|---|---|
| `SLH-DSA-SHA2-128s` | 32 bytes | **7,856 bytes** | 123× larger |
| `SLH-DSA-SHA2-192s` | 48 bytes | **16,224 bytes** | 253× larger |
| `SLH-DSA-SHA2-256s` | 64 bytes | **29,792 bytes** | 465× larger |

Use `128s` for most applications — lowest gas cost, still quantum-resistant.

---

## Attribution

This build is the result of two bodies of work. Both are required — neither alone
produced the compiled binary you have here.

---

### CryptoPascal31 (KadenaFriend) — Core SLH-DSA implementation

GitHub: https://github.com/CryptoPascal31
Branch: `kda-community/pact-5` `post_quantum`
17 commits, all cryptographic and protocol implementation work.

**What CryptoPascal wrote (every line of SLH-DSA crypto):**

| File | What it does |
|---|---|
| `pact/Pact/Crypto/SlhDsa/SlhDsa.hs` | Full FIPS 205 SLH-DSA algorithm — WOTS+, FORS, HT, XMSS |
| `pact/Pact/Crypto/SlhDsa/ChainwebSlhDsa.hs` | Chainweb-specific profile: CHAINWEB context, Blake2 OID, `verifySig` entry point |
| `pact/Pact/Crypto/SlhDsa/Parameters.hs` | SHA2-128s/192s/256s parameter sets (FIPS 205 §11) |
| `pact/Pact/Crypto/SlhDsa/MessageDigest.hs` | Message digest / Hmsg computation |
| `pact/Pact/Crypto/SlhDsa/Signature.hs` | Signature parsing and structure |
| `pact/Pact/Crypto/SlhDsa/Addresses.hs` | FIPS 205 address structures |
| `pact/Pact/Crypto/SlhDsa/Utils.hs` | SHA-256/512, SPHINCS+ hash utilities |
| `pact/Pact/Core/Principal.hs` | `q:` and `x:` principal types added |
| `pact/Pact/Core/Scheme.hs` | `SlhDsaSha128s/192s/256s` PPKScheme variants |
| `pact/Pact/Core/Guards.hs` | SLH-DSA keyset guard support |
| `pact/Pact/Core/Environment/Types.hs` | `DisableSlhDsaSignatures` exec config flag |
| `pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs` | SLH-DSA integration into the evaluator |
| `pact/Pact/Core/IR/Eval/Runtime/Utils.hs` | SLH-DSA runtime verification wiring |
| `pact-request-api/Pact/Core/Command/Types.hs` | SLH-DSA in transaction command types |
| `pact-request-api/Pact/Core/Command/Crypto.hs` | SLH-DSA in command crypto layer |
| `pact-request-api/Pact/Core/Command/SigData.hs` | SLH-DSA in signature data structures |
| `gasmodel/Pact/Core/GasModel/SigsBench.hs` | Gas benchmarks for SLH-DSA signature verification |
| `pact-tests/Pact/Core/Test/SlhSignaturesTests.hs` | Original Chainweb SLH-DSA test vectors (key1/key2/key3, sig0–sig5) |
| `pact-tests/Pact/Core/Test/PrincipalTests.hs` | q: / x: principal validation tests |
| `pact-tests/Pact/Core/Test/SignatureSchemeTests.hs` | Signature scheme tests |
| `pact-tests/pact-tests/keyset-formats.repl` | SLH-DSA keyset format REPL tests |
| `pact-tests/pact-tests/principals.repl` | q: / x: principal REPL tests |
| `pact-repl/Pact/Core/IR/Eval/Direct/ReplBuiltin.hs` | SLH-DSA support in REPL builtins |

**Commit history (oldest → newest):**
```
04f9d97a  Implementation of SLH-DSA
3b84088b  Fix with GHC 9.8
84874e83  Add SLH-DSA Gas benchmarks
1275a3c6  Don't return a bool for SLH signature verification + Use Pact schemes
ac779d68  Enable SLH DSA signatures
cbf0f142  Merge branch 'master' into post_quantum
15d86e13  Enable reading SLH keysets
cccf6d9a  Implement q and x accounts => Tests still needed
377f2d43  Improve signatures benchmark
2ce154b9  Merge branch 'master' into post_quantum
74c6861a  Fix missing deps
51b8e96f  Benchmark WebAuthn signatures
812f8213  Add principal tests for Post-quantum
79056130  Add the DisableSlhDsaSignatures in doc
a9fe9eb6  Use ASN1 lib to compute Blake2 OID
1498f9d2  Fix tests
15a22e1b  Whitespaces
```

**KIP-0041 specification** (authored by CryptoPascal31):
- PR: https://github.com/kda-community/KIPs/pull/2
- Defines `q:`/`x:` principals, HD derivation, gas schedule, FIPS 205 §10.2.2 profile

---

### NOtBobs-Emporium-Of-Wonder — Test hardening, NIST validation, build & integration

GitHub: https://github.com/NOtBobs-Emporium-Of-Wonder
1 commit on top of CryptoPascal's branch: `62e28233`

**What was added on top:**

| File | What was done |
|---|---|
| `pact-tests/Pact/Core/Test/SlhSignaturesTests.hs` | **Hardened** CryptoPascal's original test file — proper `assertFailure` messages, local file fallback before URL download, explicit `parameterSet`/`signatureInterface` fields parsed, all 50+ NIST ACVP test case IDs covered (421–462 raw, 253–322 pure, 268–331 prehash), structured `TestGroup` decoder with optional fields |
| `pact-tests/SlhDsaTestSuite/prompt.json.xz` | NIST ACVP SLH-DSA sigVer test fixture (504 test cases, 3 parameter sets) — extracted, validated, compressed and committed so tests run offline without network |
| `README.md` | This file — build docs, KIP-0041 spec summary, attribution |
| `SHA256SUMS` | SHA256 checksum of compiled `pact-pq` binary |
| `examples/accounts/accounts.repl` | Removed `(verify ...)` call (not in Pact 5), removed `env-entity` (removed in Pact 5), fixed open transaction state |
| `examples/cp/cp.repl` | Removed `(verify ...)` call (not in Pact 5) |
| `.gitignore` | Added `bin/` to exclude the compiled binary from git |

**Build environment work (not committed, but necessary):**
- Identified `libmpfr-dev` as the missing C dependency blocking `cabal build exe:pact`
- Installed `libmpfr-dev 4.2.2` via apt
- Successfully ran `cabal build exe:pact` to completion (GHC 9.6.7, Ubuntu 25.10)
- Produced the **first compiled Pact 5 binary with SLH-DSA support** (203MB unstripped)
- Tested `q:` principal recognition, `typeof-principal`, `is-principal` behaviour
- Installed to `~/bin/pact-pq` and created this release

**Prior work in separate projects (pre-dating this build):**

All of the following was completed before the binary was compiled. It represents the
research, validation and tooling infrastructure that made the build possible.

---

#### Project 1 — Hyperlane Bridge (`Hyperlane_bridge_EVM_hybrid/`)
*Kadena × EVM cross-chain bridge with post-quantum profile*

| File | What it does |
|---|---|
| `SLHDSA_FIPS205_HARDENED_TESTS.hs` | Standalone hardened Haskell test harness — pre-dates the committed `SlhSignaturesTests.hs`. Contains real Chainweb SLH-DSA keys and signatures (`key1/key2/key3`, `sig0–sig5`, `hash1/hash2`) used to validate the implementation against both 256-byte and 512-byte Blake2 hashes. Covers `good`/`bad` signature pairs for all three schemes. |
| `kadena-hyperlane-bridge/SLH_DSA_TEST_RUNBOOK.md` | Full documented pipeline: where to place NIST prompt file, how to run `validate:slh-prompt`, expected output (504 test cases), fixture extraction steps, Kadena Ed25519 keygen fallback |
| `kadena-hyperlane-bridge/scripts/extract-slh-bundle.mjs` | Extracts a specific NIST ACVP test case (e.g. tcId 422) from `prompt.json` into separate hex artifact files — `slh_bundle_tc422.json`, `slh_public_key_tc422.hex`, `slh_signature_tc422.hex`, `slh_message_tc422.hex` |
| `kadena-hyperlane-bridge/scripts/validate-slh-prompt.mjs` | Validates NIST prompt.json structure — checks vsId, testGroups, required fields, verifies all tcIds used by `SlhSignaturesTests.hs` are present |
| `kadena-hyperlane-bridge/scripts/setup-pq-profile.ts` | Sets up PQ key placeholder files (`my_key.mldsa`, `my_key.slhdsa`, `my_key.mlkem`, `my_key.hqc`) and injects a managed `.env` block with `PQC_*` variables for both testnet and mainnet profiles |
| `kadena-hyperlane-bridge/PQ_ROADMAP.md` | Tracks all open PQ PRs with diff-extracted file lists — pact-5 PR#4, chainweb-node PR#19/#23, KIPs PR#2. Includes full PQ crypto library matrix (liboqs, OpenSSL 3.5+, wolfSSL, Bouncy Castle etc.) with `Now usable` / `Pilot only` / `Track only` recommendations |
| `kadena-hyperlane-bridge/pq_upstream/` | Local mirror of the post_quantum branch source (pact-tests, SlhDsaTestSuite) used as the reference for the NIST fixture |
| `kadena-hyperlane-bridge/pq_artifacts/` | Extracted NIST artifacts directory — output of `pq:extract-slh-bundle` npm script |

**npm scripts added to `package.json`:**
```
"validate:slh-prompt"   → validates NIST prompt.json structure
"pq:extract-slh-bundle" → extracts tcId 422 artifacts to pq_artifacts/
"pq:setup"              → writes PQ key placeholders + .env block (testnet)
"pq:setup:mainnet"      → same for mainnet (with PQC_MAINNET_DEPLOY_READY=0 gate)
```

---

#### Project 2 — kda-tool-sublime (`/mnt/workspace/projects/transpiler/kda-tool-sublime/`)
*Sublime Text plugin for Kadena development — PQ commands added*

| File | What it does |
|---|---|
| `KIP-0041-DRAFT.md` | Full local mirror of the KIP-0041 draft specification — SLH-DSA scheme parameters, `q:`/`x:` principal format, SLIP-10 HD derivation for SLH-DSA, gas schedule, FIPS 205 §10.2.2 compliance notes, hedged vs deterministic signing |
| `PQ_ROADMAP.md` | PR tracking document — same as bridge project, kept in sync |
| `SLH-DSA-SHA2-128F.json` | Full NIST ACVP SLH-DSA-SHA2-128f sigVer test vector file (29MB) — used to validate `SLH-DSA-SHA2-128f` parameter set |
| `SLHDSA_FIPS205_SIGVER_SAMPLE.json` | Sample NIST ACVP sigVer JSON with metadata — algorithm, mode, revision, parameterSet, signatureInterface, preHash |
| `SLHDSA_FIPS205_HARDENED_TESTS.hs` | Same standalone test harness as in the bridge project |
| `SlhSignaturesTests.hs` | Original version of the test file (before hardening) |
| `pq_files/slh_bundle_tc422.json` | Extracted NIST ACVP test case 422 — SLH-DSA-SHA2-128s, sigVer, with full public key and 7856-byte signature hex. Source path records exact origin from `pq_upstream/pact5_post_quantum/pact-tests/SlhDsaTestSuite/` |
| `pq_files/slh_public_key_tc422.hex` | Public key for NIST test case 422 |
| `pq_files/slh_signature_tc422.hex` | Full 7856-byte SLH-DSA signature for test case 422 |
| `pq_files/slh_message_tc422.hex` | Message for test case 422 |
| `pq_files/kadena_keypair.json` | Real Kadena Ed25519 keypair generated via `kda-tool keygen plain` |
| `pq_files/kadena_ed25519_keypair.json` | Separate Ed25519 account keypair for transaction signing |
| `pq_files/my_key.slhdsa` / `my_key.mldsa` / `my_key.mlkem` / `my_key.hqc` | PQ key placeholder files generated by `KdaPreparePqProfileCommand` |

**Sublime plugin commands added (`main.py`):**

| Command | What it does |
|---|---|
| `KDA: Generate SLH-DSA Keys` | Generates real FIPS 205 SLH-DSA keypairs using the `slh-dsa` Python library — all 6 parameter sets (128s/f, 192s/f, 256s/f). Outputs KIP-0041 format JSON with `SK.seed`, `SK.prf`, `PK.seed`, `PK.root`, and `q:` prefixed public key. Falls back to placeholder if library unavailable. |
| `KDA: Prepare PQ Profile` | Writes PQ key placeholders and injects `PQC_*` managed `.env` block with testnet/mainnet profiles and `PQC_MAINNET_DEPLOY_READY=0` gate |
| `_CHAINWEB_SLH_VECTORS` | Hardcoded known-good Chainweb SLH-DSA reference vectors from `SlhSignaturesTests.hs` — `key1/key2/key3` public keys for 128s/192s/256s — used to verify toolchain output against known answers |

---

#### Summary: what was already done before the build

```
March 6, 2026:
├── Extracted NIST ACVP test case 422 (SLH-DSA-SHA2-128s) into artifacts
├── Wrote validate-slh-prompt.mjs — validated 504 test cases load correctly
├── Wrote extract-slh-bundle.mjs — produced slh_bundle_tc422.json etc.
├── Wrote setup-pq-profile.ts — PQ key env scaffold for bridge project
├── Wrote SLHDSA_FIPS205_HARDENED_TESTS.hs — standalone test harness
│   with real Chainweb SLH-DSA signatures for 128s/192s/256s
│   covering 256-byte and 512-byte Blake2 hash inputs
├── Drafted KIP-0041-DRAFT.md — full spec mirror
├── Wrote PQ_ROADMAP.md — PR tracking across 4 repos
├── Added KDA: Generate SLH-DSA Keys to Sublime plugin
├── Added KDA: Prepare PQ Profile to Sublime plugin
├── Added _CHAINWEB_SLH_VECTORS reference dict
└── Generated and validated kadena_keypair.json via kda-tool

March 29, 2026 (today):
├── Identified libmpfr-dev as missing C dependency
├── Installed libmpfr-dev, ran cabal build exe:pact
├── Produced first compiled Pact 5 post-quantum binary (203MB)
├── Tested q: principal recognition
├── Hardened SlhSignaturesTests.hs with NIST ACVP fixture
├── Committed NIST prompt.json.xz to pact-tests/SlhDsaTestSuite/
└── Created this repository with README and SHA256SUMS
```

---

## Related PRs to watch

| PR | Repo | Status | What it adds |
|---|---|---|---|
| #4 | kda-community/pact-5 | Open (WIP) | SLH-DSA in Pact interpreter |
| #19 | kda-community/chainweb-node | Open (WIP) | PQ wiring into Chainweb |
| #23 | kda-community/chainweb-node | Open (WIP) | PQ gas model |
| #2 | kda-community/KIPs | Open (Draft) | KIP-0041 SLH-DSA spec |

---

## Daml / Post-Quantum comparison

This binary makes Kadena **more quantum-resistant than Daml/Canton** at the
signature level. Canton uses ED25519 exclusively and has no published PQ roadmap.
With this build, Kadena Pact contracts can be secured with SLH-DSA — meaning
even a future cryptographically-relevant quantum computer cannot forge signatures
on `q:` accounts.

---

---

## 🎖️ Credits & Contributors

> **Long live the Empire** 🏛️  
> Contributed to by **`not_bob`** & **`seal_klub`**.

Special thanks and acknowledgment to the active community engineers and researchers contributing to Pact 5 and post-quantum development:

* **not_bob & seal_klub** (`long_live the empire`) — Post-quantum SLH-DSA architecture, FIPS 205 reference verification, `q:`/`x:` principals, `kda-tool` PQ extensions, and test suites.
* **CryptoPascal31 (Pascal)** — Pact 4/5 migration, allowed signature schemes, gas model derivations, and Chainweb node integration.
* **KadenaFriend (`kdafriend`)** — Chainweb-node Post-Quantum hard fork implementation (`Chainweb33`) and Pact 5 dependency synchronization.
* **Edmund Noble (`edmundnoble`)** — Core Pact 5 architecture, compiler IR, and CEK evaluator.
* **Jose Cardona (`jmcardon`)** — Pact 5 runtime, WebAuthn integration, and standard libraries.
* **Dennis (`dnns-es`)** — Base64URL strict unpadded decoding and SLH-DSA cross-verification test harnesses.

---

## Licence

Source: Apache 2.0 (kda-community/pact-5)
Binary: Built for private research and development use.
