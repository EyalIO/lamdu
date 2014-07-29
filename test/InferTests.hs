{-# LANGUAGE RankNTypes, OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module InferTests (allTests, factorialExpr, euler1Expr, solveDepressedQuarticExpr) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (void)
import Data.String (IsString(..))
import InferAssert
import InferCombinators
import InferWrappers
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import qualified Lamdu.Expr as E
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Infer.Error as InferErr

a, b, c, d :: TypeStream
a:b:c:d:_ = map (typeVar . fromString . (:[])) ['a'..]

simpleTests =
  [ testInfer "literal int" $ literalInteger 5
  , testInfer "simple apply" $
    holeWithInferredType (a ~> b) $$ holeWithInferredType a
  ]

applyIntToBoolFuncWithHole =
  testInfer "apply" $
  glob [] "IntToBoolFunc" $$ holeWithInferredType intType

inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  lambda "xs" listInts $ \xs ->
  glob [intType] ":" $$:
  [literalInteger 5, xs]
  where
    listInts = listOf intType

-- Depend on Rigidity:

applyOnVar =
  testInferAllowFail "No support for rigidity yet"
    "apply on var" $
  lambda "x" a $ \x ->
  glob [] "IntToBoolFunc" $$
  (holeWithInferredType (a ~> intType) $$ x)

monomorphRedex =
  testInfer "foo = f (\\b (x:{b}) -> (\\_1 -> x) ?) where f (_2:(a:Type -> ? -> a)) = ?" $
  whereItem "f" (lambda "_2" fArgType (\_ -> holeWithInferredType b)) $ \f ->
  f $$
  ( lambda "x" b $ \x ->
    lambda "_1" c (const x) $$ holeWithInferredType d )
  where
    -- (a:Type -> _[=a] -> a)
    fArgType = a ~> a

-- TODO: not sure what this used to test but I think I changed it..
idPreservesDependency =
  testInfer "5 + f _ where f x = id _{no inferred type}" $
  whereItem "f"
  ( lambda "x" (pure E.intType) (const (glob [intType] "id" $$
    holeWithInferredType intType))
  ) $ \f ->
  glob [] "+" $$:
  [literalInteger 5, f $$ holeWithInferredType intType]

idTest = testInfer "id test" $ glob [intType] "id" $$ literalInteger 5

inferFromOneArgToOther =
  testInfer "f = \\ (x:a) (y:a) -> if {True x y}" $
  lambda "x" a $ \x ->
  lambda "y" a $ \y ->
  glob [a] "if" $$:
  [glob [] "True", x, y]

inferFromOneArgToOtherList =
  testInfer "f = \\ x y -> if {_ (_:x) y}" $
  lambda "x" listType $ \x ->
  lambda "y" listType $ \y ->
  glob [listType] "if" $$:
  [holeWithInferredType boolType
  , glob [a] ":" $$: [holeWithInferredType a, x]
  , y
  ]
  where
    listType = listOf a

-- fOfXIsFOf5 =
--   testInfer "f x = f 5" $
--   lambda "" (asHole E.intType) $ \_ ->
--   recurse (E.intType ~> hole) $$ literalInteger 5

argTypeGoesToPi =
  testInfer "arg type goes to func type" $
  holeWithInferredType (intType ~> a) $$ literalInteger 5

idOnAnInt =
  testInfer "id on an int" $
  glob [intType] "id" $$ literalInteger 5

idOnHole = testInfer "id hole" $ glob [a] "id" $$ holeWithInferredType a

-- failResumptionAddsRules =
--   -- f     x    = x _ _
--   --   --------
--   --   (_ -> _)
--   --         ^ Set Bool here
--   testCase "Resumption that adds rules and fails" .
--   runContextAssertion $ do
--     -- TODO: Use standard resumption APIs for failed resumes too?
--     rootInferred <- inferDef $ infer =<< load expr
--     fmap verifyError . try . void $
--       loadInferInto (rootInferred ^?! resumptionPoint) resumptionValue
--   where
--     verifyError :: Either Error () -> M ()
--     verifyError (Left (InferError Infer.Mismatch {})) = return ()
--     verifyError _ = error "Resumption did not fail!"
--     expr = lambda "x" (hole ~> hole) $ \x -> x $$ hole $$ hole
--     resumptionValue = glob "Bool" -- <- anything but Pi
--     resumptionPoint =
--       lamParamType KVal . lamResult KType .
--       E.ePayload . Lens._1
-- lamParamType :: Kind -> Lens.Traversal' (E.Expr def par a) (E.Expr def par a)
-- lamParamType k = ExprLens.exprKindedLam k . Lens._2
-- lamResult :: Kind -> Lens.Traversal' (E.Expr def par a) (E.Expr def par a)
-- lamResult k = ExprLens.exprKindedLam k . Lens._3

-- testRecurseResumption =
--   testInfer "Resumption with recursion" $
--   lambda "a" (asHole oldFuncType `resumeHere` newFuncType) $ \a ->
--   a $$ (recurse (((hole ~> hole) `resumedTo` newFuncType) ~> (hole `resumedTo` E.intType)) $$ a)
--   where
--     oldFuncType = holeWithInferredType set ~> holeWithInferredType set
--     newFuncType = asHole E.intType ~> E.intType

resumptionTests =
  testGroup "type infer resume" $
  [ testInfer "{hole->5}" $
    holeWithInferredType a `resumeHere` literalInteger 5
  , testInfer "hole {hole->id}" $
    holeWithInferredType (a ~> b) $$
    (holeWithInferredType a `resumeHere` glob [c] "id")
  , testInfer "\\_ -> {hole->id}" $
    lambda "" (a `resumedToType` c ~> c) $
    \_ -> holeWithInferredType b `resumeHere` glob [c] "id"
  -- , failResumptionAddsRules
  -- , testRecurseResumption
  , testInfer "Resumption with list [_ => 1]" $
    nonEmptyList [holeWithInferredType a `resumeHere` literalInteger 1]
  , testInfer "Resumption with getfield" $
    ( holeWithInferredType
      (E.TRecord <$>
       compositeTypeExtend "x"
       (a `resumedToType` intType)
       (compositeTypeVar "r1"))
      `resumeHere` record [("x", literalInteger 5)]
    ) $. "x"
  , testInfer "apply of resumed-glob" $
    holeWithInferredType (boolType ~> a) `resumeHere` glob [] "not" $$
    glob [] "True"
  , testInfer "apply of resumed-lam" $
    holeWithInferredType (a ~> b)
    `resumeHere` lambda "x" c (const (holeWithInferredType d))
    $$ holeWithInferredType a
  ] ++
  [ testInfer ("\\x:_ -> \\y:_ -> {_->" ++ name ++ "}") $
    lambda "x" a $ \x ->
    lambda "y" b $ \y ->
    holeWithInferredType c `resumeHere` sel (x, y)
  | (name, sel) <- [("x", fst), ("y", snd)]
  ]

recordTest =
  testInfer "f x = {x" $
  lambda "x" a $ \x -> record [ ("field", x) ]

inferReplicateOfReplicate =
  testInfer "replicate (replicate 1 3) 2" $
  replicat (listOf intType)
  (replicat intType
   (literalInteger 1)
   (literalInteger 3))
  (literalInteger 2)
  where
    replicat typ x n =
      glob [typ] "replicate" $$: [ x, n ]

inferFailsDueToOccurs =
  inferFailsAssertion "InfiniteExpr" isExpectedError
  where
    isExpectedError InferErr.OccursCheckFail {} = True
    isExpectedError _ = False

fix3Lambdas =
  testCase "fix3Lambdas: fix (\\recu -> \\x -> \\y -> recu ?)" .
  inferFailsDueToOccurs $
  P.global "fix" `P.app`
  ( P.abs "recu" $ P.abs "x" $ P.abs "y" $
    P.var "recu" `P.app` P.hole
  )

infiniteTypeTests =
  testGroup "Infinite types"
  [ -- wrongRecurseMissingArg
    getFieldWasntAllowed
  , fix3Lambdas
  ]

getFieldWasntAllowed =
  testInfer "map (\\x. {_ => x}) ({}:_)" $
  glob [recType, a] "map" $$:
  [ glob [recType] ":" $$:
    [ eRecEmpty
    , holeWithInferredType $ listOf recType
    ]
  , lambda "x" recType $ \x -> holeWithInferredType a `resumeHere` x
  ]
  where
    recType = E.TRecord <$> emptyCompositeType

-- wrongRecurseMissingArg =
--   testCase "f x = f" .
--   inferFailsDueToOccurs $
--   lambda "x" hole . const $ recurse hole

mapIdTest =
  testInfer "map id (5:_)" $
  glob [intType, intType] "map" $$:
  [ glob [intType] ":" $$:
    [ literalInteger 5
    , holeWithInferredType $ listOf intType
    ]
  , glob [intType] "id"
  ]

factorialExpr =
  glob [facType] "fix" $$
  lambda "loop" (facType)
  ( \loop ->
    lambda "x" iInt $ \x ->
    glob [iInt] "if" $$:
    [ glob [iInt] "==" $$:
      [x, literalInteger 0]
    , literalInteger 1
    , glob [] "*" $$:
      [ x
      , loop $$ (glob [] "-" $$: [x, literalInteger 1])
      ]
    ]
  )
  where
    facType = intType ~> intType
    iInt = intType

-- recurseScopeTest =
--   testCase "recurse scope (f x = f ?<verify scope>)" $
--   assertEqual "scope must match" (fmap void scope) (Map.fromList [(xGuid, pureHole)])
--   where
--     scope = runSuccessfulM (derefWithPL =<< loadInferDef expr) ^?! scopeAtPoint
--     iset = holeWithInferredType set
--     -- TODO: Use proper infrastructure
--     expr = lambda "x" iset . const $ recurse (hole ~> hole) $$ hole
--     xGuid = expr ^?! ExprLens.exprKindedLam KVal . Lens._1
--     scopeAtPoint =
--       lamResult KVal .
--       ExprLens.exprApply . E.applyArg .
--       E.ePayload . Lens._1 . InferDeref.dScope

euler1Expr =
  glob [] "sum" $$
  ( glob [iInt] "filter" $$:
    [ glob [] ".." $$: [literalInteger 1, literalInteger 1000]
    , lambda "x" iInt $ \x ->
      glob [] "||" $$:
      [ glob [iInt] "==" $$:
        [ literalInteger 0, glob [] "%" $$: [x, literalInteger 3] ]
      , glob [iInt] "==" $$:
        [ literalInteger 0, glob [] "%" $$: [x, literalInteger 5] ]
      ]
    ]
  )
  where
    iInt = pure E.intType

-- Solve depressed quartic polynomial
solveDepressedQuarticExpr =
  lambdaRecord "params"
  [ ("e0", iInt)
  , ("d0", iInt)
  , ("c0", iInt)
  ] $ \[e0, d0, c0] ->
  whereItem "solvePoly" ( glob [iListInt] "id" )
  $ \solvePoly ->
  whereItem "sqrts"
  ( lambda "x" iInt $ \x ->
    whereItem "r"
    ( glob [] "sqrt" $$ x
    ) $ \r ->
    nonEmptyList [r, glob [] "negate" $$ r]
  )
  $ \sqrts ->
  glob [iListInt] "if" $$:
  [ glob [iInt] "==" $$: [d0, literalInteger 0]
  , glob [iInt] "concat" $$
    ( glob [iInt, iListInt] "map" $$:
      [ solvePoly $$ nonEmptyList [e0, c0, literalInteger 1]
      , sqrts
      ]
    )
  , glob [iInt] "concat" $$
    ( glob [iInt, iListInt] "map" $$:
      [ sqrts $$ (glob [iInt] "head" $$ (solvePoly $$ nonEmptyList
        [ glob [] "negate" $$ (d0 %* d0)
        , (c0 %* c0) %- (literalInteger 4 %* e0)
        , literalInteger 2 %* c0
        , literalInteger 1
        ]))
      , lambda "x" iInt $ \x ->
        solvePoly $$ nonEmptyList
        [ (c0 %+ (x %* x)) %- (d0 %/ x)
        , literalInteger 2 %* x
        , literalInteger 2
        ]
      ]
    )
  ]
  where
    iInt = intType
    iListInt = listOf intType
    x %+ y = glob [] "+" $$: [x, y]
    x %- y = glob [] "-" $$: [x, y]
    x %* y = glob [] "*" $$: [x, y]
    x %/ y = glob [] "/" $$: [x, y]

joinMaybe =
  testInfer "\\x:_ -> caseMaybe x (empty=Nothing, just=\\x->x)" $
  lambda "x" (maybeOf (maybeOf a)) $ \x ->
  glob [maybeOf a, maybeOf a] "caseMaybe"
  $$:
  [ x
  , glob [a] "Nothing"
  , lambda "item" (maybeOf a) id
  ]

getFieldTests =
  testGroup "GetField tests"
  [ testGroup "missing fields"
    [ let
        isExpectedError InferErr.TypesDoNotUnity {} = True
        isExpectedError _ = False
      in
        testCase name .
        inferFailsAssertion "GetMissingField" isExpectedError $
        reco `P.getField` getFieldTag
    | (name, getFieldTag, reco) <-
      [ ("GetField on empty record", "field", P.recEmpty)
      , ("GetField on one mismatching field", "bar"
        , P.recExtend "foo" P.hole P.recEmpty
        )
      , ("GetField on two mismatching fields", "bar"
        , P.recExtend "foo" P.hole $
          P.recExtend "baz" P.hole P.recEmpty
        )
      ]
    ]
  , let
      isExpectedError InferErr.TypesDoNotUnity {} = True
      isExpectedError _ = False
    in
      testCase "getField of non-record" $
      inferFailsAssertion "GetFieldRequiresRecord" isExpectedError $
      P.litInt 5 `P.getField` "test"
  , testGroup "allowed getFields"
    [ testInfer "GetField tag of record of 2" $
      ( eRecExtend "field1" (holeWithInferredType a) $
        eRecExtend "field2" (holeWithInferredType b) $
        holeWithInferredType (E.TRecord <$> (compositeTypeVar "r1"))
      ) $. "foo"
    ]
  , testInfer "GetField verified against (resumed record)" $
    ( holeWithInferredType (E.TRecord <$> (compositeTypeVar "r1"))
      `resumeHere`
      record
      [ ("x", literalInteger 5)
      , ("y", holeWithInferredType a)
      ]
    ) $. "x"
  ]

fromQuickCheck1 =
  testCase "fromQuickCheck1: \\a -> a (a a)" .
  inferFailsDueToOccurs $
  -- QuickCheck discovered: ? (\\a:?==>a (25 (a (a (a a)) ?)))
  -- simplified to:
  P.abs "x" $ x `P.app` (x `P.app` x)
  where
    x = P.var "x"

-- testUnificationCarriesOver =
--   testGroup "Unification carries over"
--   [ testInfer "(\\(a:Set) -> (+)) _ :: ({l:E.intType, r:_}->_)" $
--     typeAnnotate
--     (recType E.intType (asHole E.intType) ~> asHole E.intType) $
--     lambda "a" set (\_ -> glob "+") $$
--     holeWithInferredType set

  -- TODO: revive this test now. (+) was polymorphic, need a different poly func
  -- , testInfer "(\\(a:Set) -> ?{(+)} ?) ? :: ({l:E.intType, r:?}->?)" $
  --   typeAnnotate
  --   (recType E.intType (holeWithInferredType set `resumedTo` E.intType)
  --    ~> (holeWithInferredType set `resumedTo` E.intType)) $
  --   lambda "a" set
  --   ( \_ ->
  --     (holeWithInferredType (hole ~> hole) `resumeHere` glob "+") $$ hole `resumedToType` set
  --   ) $$ holeWithInferredType set
  --
  -- , testInfer
  --   "(\\(_1:Set) -> (? :: (a:?{Type}) -> {l:?{a}, r:?{a}} -> ?{a}) ?) ? :: {l:E.intType, r:?} -> ?" $
  --   typeAnnotate
  --   (recType E.intType (holeWithInferredType set `resumedTo` E.intType)
  --    ~> (holeWithInferredType set `resumedTo` E.intType)) $
  --   lambda "_1" set
  --   ( \_ ->
  --     typeAnnotate
  --     ( piType "a" set
  --       (\a ->
  --         recType
  --         (holeWithInferredType set `resumeHere` a)
  --         (holeWithInferredType set `resumeHere` a) ~>
  --         (holeWithInferredType set `resumeHere` a))
  --     )
  --     hole $$
  --     holeWithInferredType set
  --   ) $$ holeWithInferredType set
  -- ]
  -- where
  --   recType lType rType = record KType [("infixlarg", lType), ("infixrarg", rType)]

hunitTests =
  simpleTests
  ++
  [ fromQuickCheck1
  , mapIdTest
  , testInfer "factorial" factorialExpr
  -- , recurseScopeTest
  , testInfer "euler1" euler1Expr
  , testInfer "solveDepressedQuartic" solveDepressedQuarticExpr
  , applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnHole
  , inferFromOneArgToOther
  , inferFromOneArgToOtherList
  , idPreservesDependency
  -- , fOfXIsFOf5
  , monomorphRedex
  , inferPart
  , testInfer "val infer" $ record []
  , recordTest
  , inferReplicateOfReplicate
  , infiniteTypeTests
  , resumptionTests
  , joinMaybe
  , getFieldTests
  -- , testUnificationCarriesOver
  ]

inferPreservesShapeProp :: E.Val () -> Property
inferPreservesShapeProp expr =
  case runNewContext $ loadInferDef expr of
    Left _ -> property rejected
    Right inferred -> property (void inferred == expr)

qcProps =
  [ testProperty "infer preserves shape" inferPreservesShapeProp
  ]

allTests = hunitTests ++ qcProps
