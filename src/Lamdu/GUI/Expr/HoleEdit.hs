module Lamdu.GUI.Expr.HoleEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    ( Monad i, Monad o
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Glue.HasTexts env
    ) =>
    ExprGui.Payload i o ->
    GuiM env i o (Responsive o)
make pl =
    do
        litEventMap <- ExprEventMap.makeLiteralEventMap ?? pl ^. _1 . Sugar.plActions
        (ExprEventMap.add pl <&> (Align.tValue %~))
            <*> ((Widget.makeFocusableView ?? WidgetIds.fromExprPayload (pl ^. _1) <&> fmap) <*> Label.make "_")
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (litEventMap <>)
            <&> Responsive.fromWithTextPos
