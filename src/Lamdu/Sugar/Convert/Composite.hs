{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Composite
    ( convertCompositeItem, makeAddItem, convertEmptyComposite, convertOpenCompositeActions
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag, convertTagSelection)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Property as Property
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    ConvertM m (T m EntityId)
deleteItem stored restI =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> Lens.mapped %~ EntityId.ofValI

convertOpenCompositeActions ::
    Monad m => V.Leaf -> ExprIRef.ValIProperty m -> ConvertM m (OpenCompositeActions (T m))
convertOpenCompositeActions leaf stored =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal ->
    OpenCompositeActions
    { _openCompositeClose =
        ExprIRef.newValBody (V.BLeaf leaf)
        >>= protectedSetToVal stored
        <&> EntityId.ofValI
    }

convertEmptyComposite ::
    Monad m =>
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Input.Payload m a ->
    ConvertM m (Composite InternalName (T m) expr)
convertEmptyComposite extendOp exprPl =
    do
        actions <-
            ConvertM.postProcess
            <&>
            \postProcess ->
            ClosedCompositeActions
            { _closedCompositeOpen =
                DataOps.replaceWithHole (exprPl ^. Input.stored)
                <* postProcess
                <&> EntityId.ofValI
            }
        addItem <-
            exprPl ^. Input.stored & makeAddItem extendOp 0
            >>= convertTagSelection mempty (EntityId.ofTag (exprPl ^. Input.entityId))
            <&> Lens.mapped %~ (^. cairNewVal)
        pure Composite
            { _cItems = []
            , _cTail = ClosedComposite actions
            , _cAddItem = addItem
            }

convertCompositeItem ::
    (Monad m, Monoid a) =>
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CompositeItem InternalName (T m) (ExpressionU m a))
convertCompositeItem cons stored restI inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag (expr ^. Val.payload . Input.stored . Property.pVal) restI
                        & ExprIRef.writeValBody valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. Property.pVal
        tagS <- convertTag tag mempty (EntityId.ofTag inst) setTag
        pure CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }

makeAddItem :: Monad m =>
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Int -> ExprIRef.ValIProperty m ->
    ConvertM m (T.Tag -> T m CompositeAddItemResult)
makeAddItem addItem orderVal stored =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal tag ->
    do
        DataOps.CompositeExtendResult newValI resultI <- addItem tag (stored ^. Property.pVal)
        _ <- protectedSetToVal stored resultI
        let resultEntity = EntityId.ofValI resultI
        Transaction.setP (Anchors.assocTagOrder tag) orderVal
        pure
            CompositeAddItemResult
            { _cairNewTag = TagInfo (EntityId.ofTag resultEntity tag) tag
            , _cairNewVal = EntityId.ofValI newValI
            }
