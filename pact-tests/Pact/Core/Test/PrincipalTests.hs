module Pact.Core.Test.PrincipalTests(tests) where

import Data.Attoparsec.Text
import Data.Text

import Pact.Core.Principal

import Test.Tasty
import Test.Tasty.HUnit
import Pact.Core.Guards
import Pact.Core.Names

tests :: TestTree
tests = testGroup "PrincipalTests"
  [ kSpec
  , wSpec
  , rSpec
  , mSpec
  , uSpec
  , pSpec
  , qSpec128
  , qSpec192
  , qSpec256
  , xSpec ]

-- | Default info is sufficient for this spec
--
kSpec :: TestTree
kSpec =
  testGroup "k:" [kRoundtrips, kCorrectIdents]
  where
    kRoundtrips =
      testCase "k: parser roundtrips" $ do
        -- principal -> text
        pk @?= Right k'
        -- text -> principal
        mkPrincipalIdent k' @?= k
    kCorrectIdents =
      testCase "k: has correct identifiers" $
        -- principal -> correct id
        fmap showPrincipalType pk @?= Right (showPrincipalType k')
    k = "k:584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68"
    k' = K (PublicKeyText "584deb6f81d8efe67767309d1732019cf6ad14f9f0007cff50c730ef62521c68")
    pk = parseOnly (principalParser True) k

wSpec :: TestTree
wSpec =
  testGroup "w:" [wRoundtrips, wCorrectIdents]
  where
    wRoundtrips =
      testCase "w: parser roundtrips" $ do
        pw @?= Right w'
        mkPrincipalIdent w' @?= w
    wCorrectIdents =
      testCase "w: has correct identifiers" $
        fmap showPrincipalType pw @?= Right (showPrincipalType w')
    w = "w:5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew:keys-all"
    w' = W "5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew" "keys-all"
    pw = parseOnly (principalParser True) w

rSpec :: TestTree
rSpec =
  testGroup "r:" [rRoundtrips, rCorrectIdents]
  where
    rRoundtrips =
      testCase "r: parser roundtrips" $ do
        pr @?= Right r'
        mkPrincipalIdent r' @?= r
    rCorrectIdents =
      testCase "r: has correct identifiers" $
        fmap showPrincipalType pr @?= Right (showPrincipalType r')
    r = "r:ks"
    r' = R (KeySetName "ks" Nothing)
    pr = parseOnly (principalParser True) r

mSpec :: TestTree
mSpec =
  testGroup "m:" [mRoundtrips, mCorrectIdents]
  where
    mRoundtrips =
      testCase "m: parser roundtrips" $ do
        pm @?= Right m'
        mkPrincipalIdent m' @?= m
    mCorrectIdents =
      testCase "m: has correct identifiers" $
        fmap showPrincipalType pm @?= Right (showPrincipalType m')
    m = "m:test-ns.tester:tester"
    m' = M (ModuleName "tester" (Just (NamespaceName "test-ns"))) "tester"
    pm = parseOnly (principalParser True) m

uSpec :: TestTree
uSpec =
  testGroup "u:" [uRoundtrips, uCorrectIdents]
  where
    uRoundtrips = testCase "u: parser roundtrips" $ do
      pu @?= Right u'
      mkPrincipalIdent u' @?= u
    uCorrectIdents =
       testCase "u: has correct identifiers" $
        fmap showPrincipalType pu @?= Right (showPrincipalType u')
    u :: Text
    u = "u:test-ns.tester.both-guard:aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    u' = U "test-ns.tester.both-guard" "aqukm-5Jj6ITLeQfhNYydmtDccinqdJylD9CMlLKQDI"
    pu = parseOnly (principalParser True) u

pSpec :: TestTree
pSpec = testGroup "p:"
  [ pRoundtrips
  , pCorrectIdents ]
  where
    pRoundtrips = testCase "p: parser roundtrips" $ do
      pp @?= Right p'
      mkPrincipalIdent p' @?= p
    pCorrectIdents = testCase "p: has correct identifiers" $
      fmap showPrincipalType pp @?= Right (showPrincipalType p')
    p = "p:DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g:pact-guard"
    p' = P (DefPactId "DldRwCblQ7Loqy6wYJnaodHl30d3j3eH-qtFzfEv46g") "pact-guard"
    pp = parseOnly (principalParser True) p

qSpec128 :: TestTree
qSpec128 =
  testGroup "q:128" [kRoundtrips, kCorrectIdents]
  where
    kRoundtrips =
      testCase "q: parser roundtrips" $ do
        -- principal -> text
        pq @?= Right q'
        -- text -> principal
        mkPrincipalIdent q' @?= q
    kCorrectIdents =
      testCase "q: has correct identifiers" $
        -- principal -> correct id
        fmap showPrincipalType pq @?= Right (showPrincipalType q')
    q = "q:ce46e14af2707dd21b6dc06b1e56cbf676ab222a443b0170075d581a90123c3e"
    q' = Q (PublicKeyText "qce46e14af2707dd21b6dc06b1e56cbf676ab222a443b0170075d581a90123c3e")
    pq = parseOnly (principalParser False) q

qSpec192 :: TestTree
qSpec192 =
  testGroup "q:192" [kRoundtrips, kCorrectIdents]
  where
    kRoundtrips =
      testCase "q: parser roundtrips" $ do
        -- principal -> text
        pq @?= Right q'
        -- text -> principal
        mkPrincipalIdent q' @?= q
    kCorrectIdents =
      testCase "q: has correct identifiers" $
        -- principal -> correct id
        fmap showPrincipalType pq @?= Right (showPrincipalType q')
    q = "q:2037a79e494cf0e11a83cf1958610d5c3e382b2499f7bdd3987ebb97b7c6107fb75e9d453ab27f522b29f9d6a985e297"
    q' = Q (PublicKeyText "q2037a79e494cf0e11a83cf1958610d5c3e382b2499f7bdd3987ebb97b7c6107fb75e9d453ab27f522b29f9d6a985e297")
    pq = parseOnly (principalParser False) q

qSpec256 :: TestTree
qSpec256 =
  testGroup "q:256" [kRoundtrips, kCorrectIdents]
  where
    kRoundtrips =
      testCase "q: parser roundtrips" $ do
        -- principal -> text
        pq @?= Right q'
        -- text -> principal
        mkPrincipalIdent q' @?= q
    kCorrectIdents =
      testCase "q: has correct identifiers" $
        -- principal -> correct id
        fmap showPrincipalType pq @?= Right (showPrincipalType q')
    q = "q:7023cd52cdcb8c36125e613ed47ea2c0602364c5e9f98dfca8d67e041b6efa504492828ddfd23108777c3ba7135e06054a2441a03a9784092be1d5356decc75f"
    q' = Q (PublicKeyText "q7023cd52cdcb8c36125e613ed47ea2c0602364c5e9f98dfca8d67e041b6efa504492828ddfd23108777c3ba7135e06054a2441a03a9784092be1d5356decc75f")
    pq = parseOnly (principalParser False) q

xSpec :: TestTree
xSpec =
  testGroup "x:" [wRoundtrips, wCorrectIdents]
  where
    wRoundtrips =
      testCase "x: parser roundtrips" $ do
        px @?= Right x'
        mkPrincipalIdent x' @?= x
    wCorrectIdents =
      testCase "x: has correct identifiers" $
        fmap showPrincipalType px @?= Right (showPrincipalType x')
    x = "x:5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew:keys-all"
    x' = X "5PhRgNM3oePrkfAKhk9dYmjRqOhEEhbR2eyFz8HU_ew" "keys-all"
    px = parseOnly (principalParser False) x
