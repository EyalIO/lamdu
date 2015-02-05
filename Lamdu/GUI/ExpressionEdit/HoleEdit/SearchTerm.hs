{-# LANGUAGE RecordWildCards #-}
-- | A hole's search term component

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm
    ( make
    , hiOpenSearchTermId
    ) where

import           Control.Lens.Operators
import           Control.Monad (when)
import           Control.MonadA (MonadA)
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open.EventMap as OpenEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

hiOpenSearchTermId :: HoleInfo m -> Widget.Id
hiOpenSearchTermId = HoleWidgetIds.openSearchTermId . hiEntityId

textEditNoEmpty :: TextEdit.Style -> TextEdit.Style
textEditNoEmpty textEditStyle =
    textEditStyle
    & TextEdit.sEmptyFocusedString .~ "  "
    & TextEdit.sEmptyUnfocusedString .~ "  "

make ::
    MonadA m =>
    HoleInfo m -> Maybe (EditableHoleInfo m) ->
    ExprGuiM m (ExpressionGui m)
make holeInfo mEditableHoleInfo =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        makeTextEdit mSearchTermProp
            <&> Widget.eventMap %~ OpenEventMap.disallowChars searchTerm
            <&> addBackground (Widget.toAnimId openSearchTermId)
                (Config.layers config) holeSearchTermBGColor
            <&> ExpressionGui.fromValueWidget
            & WE.localEnv (WE.envTextStyle %~ textEditNoEmpty)
            & ExprGuiM.widgetEnv
    where
      openSearchTermId = hiOpenSearchTermId holeInfo
      mSearchTermProp = mEditableHoleInfo <&> HoleInfo.ehiSearchTermProperty
      searchTerm = maybe "" Property.value mSearchTermProp
      noEmpty "" = "  "
      noEmpty x = x
      resultsId = holeInfo & hiEntityId & HoleWidgetIds.resultsPrefixId
      makeTextEdit Nothing =
          BWidgets.makeTextViewWidget (noEmpty searchTerm)
          (Widget.toAnimId openSearchTermId)
      makeTextEdit (Just searchTermProp) =
          BWidgets.makeTextEdit searchTerm openSearchTermId
          <&> Widget.events %~ \(newSearchTerm, eventRes) ->
              do
                  when (newSearchTerm /= searchTerm) $
                      Property.set searchTermProp newSearchTerm
                  eventRes
                      -- When first letter is typed in search term, jump to the
                      -- results, which will go to first result:
                      & ( if null searchTerm && (not . null) newSearchTerm
                          then Widget.eCursor .~ Monoid.Last (Just resultsId)
                          else id
                        )
                      & return
