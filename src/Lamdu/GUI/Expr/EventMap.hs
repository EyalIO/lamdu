module Lamdu.GUI.Expr.EventMap
    ( add
    , ExprInfo(..), addWith
    , extractCursor
    , addLetEventMap
    , makeLiteralEventMap, makeLiteralNumberEventMap
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (HasWidget(..), EventContext)
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import           Lamdu.Sugar.Parens (MinOpPrec)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data ExprInfo name i o = ExprInfo
    { exprInfoActions :: Sugar.NodeActions name i o
    , exprInfoMinOpPrec :: MinOpPrec
    , exprInfoIsSelected :: Bool
    }

exprInfoFromPl ::
    Monad i =>
    GuiM env i o
    ((Sugar.Payload v name i0 o0, ExprGui.GuiPayload) -> ExprInfo name i0 o0)
exprInfoFromPl =
    GuiState.isSubCursor
    <&> \isSubCursor pl ->
    let isSelected = WidgetIds.fromExprPayload (pl ^. _1) & isSubCursor in
    ExprInfo
    { exprInfoActions = pl ^. _1 . Sugar.plActions
    , exprInfoMinOpPrec =
        -- Expression with parentheses intercepts all operations from inside it,
        -- But if it is itself selected then we're out of the parentheses,
        -- and its parents may take some operators.
        if pl ^. _2 . ExprGui.plParenInfo . Sugar.piNeedParens && not isSelected
        then 0
        else pl ^. _2 . ExprGui.plParenInfo . Sugar.piMinOpPrec
    , exprInfoIsSelected = isSelected
    }

add ::
    ( HasWidget w, Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    (Sugar.Payload v name i o, ExprGui.GuiPayload) ->
    GuiM env i o (w o -> w o)
add pl = exprInfoFromPl ?? pl >>= addWith

addWith ::
    ( HasWidget w, Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    ExprInfo name i o -> GuiM env i o (w o -> w o)
addWith exprInfo =
    actionsEventMap exprInfo <&> Widget.weakerEventsWithContext

extractCursor :: Sugar.ExtractDestination -> Widget.Id
extractCursor (Sugar.ExtractToLet letId) = WidgetIds.fromEntityId letId
extractCursor (Sugar.ExtractToDef defId) = WidgetIds.fromEntityId defId

extractEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.Definitions Text) env
    , Functor o
    ) =>
    m (Sugar.NodeActions name i o -> EventMap (o GuiState.Update))
extractEventMap =
    Lens.view id
    <&>
    \env actions ->
    actions ^. Sugar.extract <&> extractCursor
    & E.keysEventMapMovesCursor (env ^. has . Config.extractKeys)
    (E.toDoc env
        [has . MomentuTexts.edit, has . Texts.extract])

addLetEventMap ::
    ( Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    o Sugar.EntityId -> GuiM env i o (EventMap (o GuiState.Update))
addLetEventMap addLet =
    do
        env <- Lens.view id
        savePos <- GuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor
            (env ^. has . Config.letAddItemKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.letClause
                    , has . Texts.add
                    ])
            & pure

actionsEventMap ::
    ( Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    ExprInfo name i o ->
    GuiM env i o (EventContext -> EventMap (o GuiState.Update))
actionsEventMap exprInfo =
    mconcat
    [ detachEventMap ?? exprInfo ?? actions ^. Sugar.detach
    , extractEventMap ?? actions
    , mkReplaceParent
    , actions ^. Sugar.delete & replaceEventMap
    , actions ^. Sugar.mNewLet & foldMap addLetEventMap
    , makeLiteralEventMap ?? actions
    ] <&> const -- throw away EventContext here
    where
        actions = exprInfoActions exprInfo
        mkReplaceParent =
            Lens.view id
            <&> \env ->
            let replaceKeys = env ^. has . Config.replaceParentKeys in
            actions ^. Sugar.mReplaceParent
            & foldMap
                (E.keysEventMapMovesCursor replaceKeys
                    (E.toDoc env
                        [has . MomentuTexts.edit, has . Texts.replaceParent])
                . fmap WidgetIds.fromEntityId)

detachEventMap ::
    ( MonadReader env m, Has Config env, Has Dir.Layout env
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    , Functor f
    ) =>
    m (ExprInfo name i o -> Sugar.DetachAction f -> EventMap (f GuiState.Update))
detachEventMap =
    Lens.view id
    <&>
    \env exprInfo ->
    \case
    Sugar.DetachAction act
        | exprInfoIsSelected exprInfo ->
            E.keysEventMapMovesCursor (env ^. has . Config.detachKeys)
            (E.toDoc env [has . MomentuTexts.edit, has . Texts.modify])
            (act <&> WidgetIds.fromEntityId)
            <>
            E.charGroup (Just "Open Paren")
            (E.toDoc env [has . MomentuTexts.edit, has . Texts.detach])
            parenKeys (const (mempty <$ act))
        where
            parenKeys =
                case env ^. has of
                Dir.LeftToRight -> "(["
                Dir.RightToLeft -> ")]"
    _ -> mempty


replaceEventMap ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    , Functor f
    ) =>
    Sugar.Delete f -> m (EventMap (f GuiState.Update))
replaceEventMap x =
    Lens.view id
    <&>
    \env ->
    let mk action =
            action <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.setToHole])
    in
    case x of
    Sugar.SetToHole action -> mk action
    Sugar.Delete action -> mk action
    Sugar.CannotDelete -> mempty

goToLiteral :: Sugar.EntityId -> GuiState.Update
goToLiteral = GuiState.updateCursor . WidgetIds.literalEditOf . WidgetIds.fromEntityId

makeLiteralNumberEventMap ::
    ( MonadReader env m, Monad o
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    ) =>
    String ->
    m ((Sugar.Literal Identity -> o Sugar.EntityId) -> EventMap (o GuiState.Update))
makeLiteralNumberEventMap prefix =
    Lens.view id <&> E.toDoc
    <&> \toDoc makeLiteral ->
    E.charGroup (Just "Digit")
    (toDoc [has . MomentuTexts.edit, has . Texts.literalNumber])
    Chars.digit
    (fmap goToLiteral . makeLiteral . Sugar.LiteralNum . Identity . read . (prefix <>) . (: []))

makeLiteralTextEventMap ::
    ( MonadReader env m, Monad o
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    ) =>
    m ((Sugar.Literal Identity -> o Sugar.EntityId) -> EventMap (o GuiState.Update))
makeLiteralTextEventMap =
    Lens.view id <&> E.toDoc <&>
    \toDoc makeLiteral ->
    E.charGroup Nothing
    (toDoc [has . MomentuTexts.edit, has . Texts.literalText]) "'\""
    (const (makeLiteral (Sugar.LiteralText (Identity "")) <&> goToLiteral))

makeRecordEventMap ::
    ( MonadReader env m, Monad o
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    , Has Dir.Layout env
    ) =>
    m (o Sugar.EntityId -> EventMap (o GuiState.Update))
makeRecordEventMap =
    Lens.view id <&>
    \env makeRec ->
    E.charGroup Nothing
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.record])
    ( case env ^. has of
        Dir.LeftToRight -> "{"
        Dir.RightToLeft -> "}"
    ) (const (makeRec <&> WidgetIds.fromEntityId <&> GuiState.updateCursor))

makeLiteralEventMap ::
    ( MonadReader env m, Monad o
    , Has (MomentuTexts.Texts Text) env, Has (Texts.CodeUI Text) env
    , Has Dir.Layout env
    ) =>
    m (Sugar.NodeActions name i o -> EventMap (o GuiState.Update))
makeLiteralEventMap =
    (<>)
    <$> ( (<>) <$> makeLiteralTextEventMap <*> makeLiteralNumberEventMap ""
            <&> (. (^. Sugar.setToLiteral))
        )
    <*> (makeRecordEventMap <&> (. (^. Sugar.setToEmptyRecord)))
