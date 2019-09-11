{-# LANGUAGE TypeApplications, DisambiguateRecordFields, KindSignatures, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses, DataKinds #-}
module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import           AST
import           AST.Class.Recursive (foldMapRecursive)
import           AST.Knot.Ann (Ann(..), ann, val, annotations)
import qualified Control.Lens.Extended as Lens
import           Data.Constraint (Dict(..))
import qualified Data.Map as Map
import           Data.Monoid (Any(..))
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertParams, convertLamParams, cpParams, cpAddFirstParam, mkVarInfo)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeActions, subexprPayloads)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) -> T m ()
lamParamToHole (V.Lam param x) =
    SubExprs.getVarsToHole param (x & annotations %~ (^. Input.stored))

makeInline ::
    Monad m =>
    ValP m -> Redex (Input.Payload m a) -> EntityId -> BinderVarInline (T m)
makeInline stored redex useId
    | Lens.has traverse otherUses = CannotInlineDueToUses (drop 1 after ++ before)
    | otherwise =
        inlineLet stored (redex <&> (^. Input.stored) <&> Property.value)
        & InlineVar
    where
        otherUses = filter (/= useId) uses
        uses = redex ^. Redex.paramRefs
        (before, after) = break (== useId) uses

convertLet ::
    (Monad m, Monoid a) =>
    Input.Payload m a ->
    Redex (Input.Payload m a) ->
    ConvertM m
    (Tree (Ann (ConvertPayload m a)) (Binder InternalName (T m) (T m)))
convertLet pl redex =
    do
        float <- makeFloatLetToOuterScope (pl ^. Input.stored . Property.pSet) redex
        tag <- ConvertTag.taggedEntity param
        (value, letBody, actions) <-
            do
                (_pMode, value) <-
                    convertAssignment binderKind param (redex ^. Redex.arg)
                    <&> _2 . ann . pInput . Input.entityId .~
                        EntityId.ofValI (redex ^. Redex.arg . ann . Input.stored . Property.pVal)
                letBody <-
                    convertBinder bod
                    & ConvertM.local (scScopeInfo . siLetItems <>~
                        Map.singleton param (makeInline stored redex))
                actions <- makeActions pl
                pure (value, letBody, actions)
            & localNewExtractDestPos pl
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . ann . Input.stored . Property.pVal)
                        <&> EntityId.ofValI
                    )
        postProcess <- ConvertM.postProcessAssert
        let del =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.lamOut . ann . Input.stored
                        & replaceWith stored & void
                <* postProcess
        typS <-
            convertType (EntityId.ofTypeOf (argAnn ^. Input.entityId))
            (argAnn ^. Input.inferredType)
        pure Ann
            { _val =
                BinderLet Let
                { _lVarInfo = mkVarInfo typS
                , _lValue = value & ann . pActions %~ fixValueNodeActions
                , _lDelete = del
                , _lName = tag
                , _lBodyScope = redex ^. Redex.bodyScope
                , _lBody =
                    letBody
                    & ann . pActions . mReplaceParent ?~
                        (letBody ^. ann . pInput . Input.entityId <$ del)
                , _lUsages = redex ^. Redex.paramRefs
                }
            , _ann =
                ConvertPayload
                { _pInput =
                    pl
                    & Input.userData .~ redex ^. Redex.lamPl . Input.userData
                , _pActions = actions
                }
            }
    where
        argAnn = redex ^. Redex.arg . ann
        stored = pl ^. Input.stored
        binderKind =
            redex ^. Redex.lam
            & V.lamOut . annotations %~ (^. Input.stored)
            & BinderKindLet
        V.Lam param bod = redex ^. Redex.lam

convertBinder ::
    (Monad m, Monoid a) =>
    Val (Input.Payload m a) ->
    ConvertM m (Tree (Ann (ConvertPayload m a)) (Binder InternalName (T m) (T m)))
convertBinder expr@(Ann pl body) =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.letExpression) >>=
    \case
    False -> convertExpr
    True ->
        case Redex.check body of
        Nothing -> convertExpr
        Just redex -> convertLet pl redex
    where
        convertExpr =
            do
                convertSub <- Lens.view (Lens.to ConvertM.scConvertSubexpression)
                convertSub ConvertM.BinderPos expr
            & localNewExtractDestPos pl
            <&> \exprS ->
            exprS
            & val %~ BinderExpr
            & ann . pInput .~ pl -- TODO: <-- why is this necessary?
            & ann . pInput . Input.userData .~
                mconcat
                (subexprPayloads
                (body ^.. traverseK1)
                (exprS ^.. val . SugarLens.childPayloads))


localNewExtractDestPos :: Input.Payload m a -> ConvertM m b -> ConvertM m b
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. Input.stored
    , _osiScope = x ^. Input.inferResult . V.iScope
    }
    & ConvertM.local

makeFunction ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) ->
    ConvertM m
    (Tree (Function InternalName (T m) (T m)) (Ann (ConvertPayload m a)))
makeFunction chosenScopeProp params funcBody =
    convertBinder funcBody
    <&> mkRes
    & ConvertM.local (ConvertM.scScopeInfo %~ addParams)
    where
        mkRes assignmentBody =
            Function
            { _fParams =
                -- TODO: avoid partiality here
                params ^?! cpParams . Lens._Just
            , _fChosenScopeProp = chosenScopeProp ^. Property.mkProperty
            , _fBody = assignmentBody
            , _fBodyScopes = cpScopes params
            , _fAddFirstParam = params ^. cpAddFirstParam
            }
        addParams ctx =
            ctx
            & ConvertM.siTagParamInfos <>~ _cpParamInfos params
            & ConvertM.siNullParams <>~
            case params ^. cpParams of
            Just NullParam{} -> Set.fromList (cpMLamParam params ^.. Lens._Just . _2)
            _ -> Set.empty

makeAssignment ::
    (Monad m, Monoid a) =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Val (Input.Payload m a) -> Input.Payload m a ->
    ConvertM m
    (Tree (Ann (ConvertPayload m a)) (Assignment InternalName (T m) (T m)))
makeAssignment chosenScopeProp params funcBody pl =
    case params ^. cpParams of
    Nothing ->
        convertBinder funcBody
        <&> val %~ BodyPlain . AssignPlain (params ^. cpAddFirstParam)
    Just{} ->
        do
            funcS <- makeFunction chosenScopeProp params funcBody
            nodeActions <- makeActions pl & localNewExtractDestPos pl
            pure Ann
                { _ann =
                    ConvertPayload
                    { _pInput =
                        -- TODO: Why are redundant hidden entity ids
                        -- returned here?
                        pl & Input.userData .~ mempty
                    , _pActions = nodeActions
                    }
                , _val = BodyFunction funcS
                }

convertLam ::
    (Monad m, Monoid a) =>
    Tree (V.Lam V.Var V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam exprPl =
    do
        convParams <- convertLamParams lam exprPl
        func <-
            makeFunction
            (lam ^. V.lamIn & Anchors.assocScopeRef)
            convParams (lam ^. V.lamOut)
        let paramNames =
                func ^.. fParams . _Params . traverse . fpInfo . piTag . tagRefTag . tagName
                & Set.fromList
        lightLamSugar <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.lightLambda)
        let lambda
                | useNormalLambda paramNames func || not lightLamSugar =
                    Lambda NormalBinder UnlimitedFuncApply func
                | otherwise =
                    func
                    & fBody %~ markNodeLightParams paramNames
                    & Lambda LightLambda UnlimitedFuncApply
        BodyLam lambda
            & addActions (lam ^.. V.lamOut) exprPl
            <&> val %~
                mapK (const (ann . pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)))

useNormalLambda ::
    Set InternalName -> Tree (Function InternalName i o) (Ann a) -> Bool
useNormalLambda paramNames func
    | Set.size paramNames < 2 = True
    | otherwise =
        ( foldMapRecursive
            (Proxy @SugarLens.SugarExpr ##>>
                Any . SugarLens.isForbiddenInLightLam
            ) (func ^. fBody . val)
            ^. Lens._Wrapped
        ) || not (allParamsUsed paramNames func)

class GetParam (t :: Knot -> *) where
    getParam :: t f -> Maybe InternalName
    getParam _ = Nothing

    getParamRecursive ::
        Proxy t -> Dict (KNodesConstraint t GetParam)
    default getParamRecursive ::
        KNodesConstraint t GetParam =>
        Proxy t -> Dict (KNodesConstraint t GetParam)
    getParamRecursive _ = Dict

instance Recursive GetParam where
    recurse =
        getParamRecursive . p
        where
            p :: Proxy (GetParam k) -> Proxy k
            p _ = Proxy

instance GetParam (Const (BinderVarRef InternalName o)) where
instance GetParam (Const (NullaryVal InternalName i o))

instance GetParam (Else InternalName i o)

instance GetParam (Function InternalName i o) where

instance GetParam (Const (GetVar InternalName o)) where
    getParam = (^? Lens._Wrapped . _GetParam . pNameRef . nrName)

instance GetParam (Assignment InternalName i o) where
    getParam x = x ^? _BodyPlain . apBody >>= getParam

instance GetParam (Binder InternalName i o) where
    getParam x = x ^? _BinderExpr >>= getParam

instance GetParam (Body InternalName i o) where
    getParam x = x ^? _BodyGetVar <&> Const >>= getParam

allParamsUsed ::
    Set InternalName -> Tree (Function InternalName i o) (Ann a) -> Bool
allParamsUsed paramNames func =
    Set.null (paramNames `Set.difference` usedParams)
    where
        usedParams =
            foldMapRecursive
            ( Proxy @GetParam ##>>
                (^. Lens._Just . Lens.to Set.singleton) . getParam
            ) func

class MarkLightParams (t :: Knot -> *) where
    markLightParams :: Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)

    default markLightParams ::
        (KFunctor t, KNodesConstraint t MarkLightParams) =>
        Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)
    markLightParams = defaultMarkLightParams

defaultMarkLightParams ::
    (KFunctor t, KNodesConstraint t MarkLightParams) =>
    Set InternalName -> Tree t (Ann a) -> Tree t (Ann a)
defaultMarkLightParams paramNames =
    mapK (Proxy @MarkLightParams #> markNodeLightParams paramNames)

markNodeLightParams ::
    MarkLightParams t =>
    Set InternalName ->
    Tree (Ann a) t ->
    Tree (Ann a) t
markNodeLightParams paramNames =
    val %~ markLightParams paramNames

instance MarkLightParams (Lens.Const a)
instance MarkLightParams (Else InternalName i o)
instance MarkLightParams (Let InternalName i o)
instance MarkLightParams (Function InternalName i o)

instance MarkLightParams (Assignment InternalName i o) where
    markLightParams ps (BodyPlain x) = x & apBody %~ markLightParams ps & BodyPlain
    markLightParams ps (BodyFunction x) = markLightParams ps x & BodyFunction

instance MarkLightParams (Binder InternalName i o) where
    markLightParams ps (BinderExpr x) = markLightParams ps x & BinderExpr
    markLightParams ps (BinderLet x) = markLightParams ps x & BinderLet

instance MarkLightParams (Body InternalName i o) where
    markLightParams paramNames (BodyGetVar (GetParam n))
        | paramNames ^. Lens.contains (n ^. pNameRef . nrName) =
            n
            & pBinderMode .~ LightLambda
            & GetParam & BodyGetVar
    markLightParams paramNames bod = defaultMarkLightParams paramNames bod

-- Let-item or definition (form of <name> [params] = <body>)
convertAssignment ::
    (Monad m, Monoid a) =>
    BinderKind m -> V.Var -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Tree (Ann (ConvertPayload m a)) (Assignment InternalName (T m) (T m))
    )
convertAssignment binderKind defVar expr =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.assignmentParameters)
    >>=
    \case
    False ->
        convertBinder expr
        <&> val %~
            BodyPlain .
            AssignPlain (AddInitialParam (error "TODO: add param when assignment parameters not supported"))
        <&> (,) Nothing
    True ->
        do
            (mPresentationModeProp, convParams, funcBody) <-
                convertParams binderKind defVar expr
            makeAssignment (Anchors.assocScopeRef defVar) convParams
                funcBody (expr ^. ann)
                <&> (,) mPresentationModeProp

convertDefinitionBinder ::
    (Monad m, Monoid a) =>
    DefI m -> Val (Input.Payload m a) ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Tree (Ann (ConvertPayload m a)) (Assignment InternalName (T m) (T m))
    )
convertDefinitionBinder defI =
    convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI)
