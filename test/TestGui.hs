{-# LANGUAGE ScopedTypeVariables #-}

module TestGui where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit(..))
import           Data.Functor.Identity (Identity(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (HasCursor(..), VirtualCursor(..))
import           GUI.Momentu.State (HasState)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (Event(..), KeyEvent(..))
import qualified Lamdu.Cache as Cache
import           Lamdu.Config (HasConfig)
import           Lamdu.Config.Theme (HasTheme)
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import           Lamdu.GUI.ExpressionEdit.BinderEdit (makeBinderBodyEdit)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import           Lamdu.Settings (HasSettings)
import           Lamdu.Style (HasStyle)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import qualified Test.Lamdu.GuiEnv as GuiEnv
import           Test.Lamdu.Instances ()
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)
import           Unsafe.Coerce (unsafeCoerce)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "gui-tests"
    [ testOpPrec
    , testFragmentSize
    ]

replExpr ::
    Lens.Traversal' (Sugar.WorkArea name i o a) (Sugar.Expression name i o a)
replExpr = Sugar.waRepl . Sugar.replExpr . Sugar.bbContent . Sugar._BinderExpr

wideFocused :: Lens.Traversal' (Responsive a) (Widget.Surrounding -> Widget.Focused a)
wideFocused = Responsive.rWide . Align.tValue . Widget.wState . Widget._StateFocused

makeReplGui ::
    ( HasState env, HasStdSpacing env, HasConfig env, HasTheme env
    , HasSettings env, HasStyle env
    ) =>
    Cache.Functions -> env -> T ViewM (ExpressionGui (T ViewM))
makeReplGui cache env =
    do
        workArea <- convertWorkArea cache
        gui <-
            workArea ^. Sugar.waRepl . Sugar.replExpr
            & makeBinderBodyEdit
            & ExprGuiM.run ExpressionEdit.make DbLayout.guiAnchors env id
        unless (Lens.has wideFocused gui) (fail "Red cursor!")
        pure gui

applyEvent ::
    ( HasState env, HasStdSpacing env, HasConfig env, HasTheme env
    , HasSettings env, HasStyle env
    ) =>
    Cache.Functions -> env -> Event -> T ViewM ()
applyEvent cache env event =
    do
        gui <- makeReplGui cache env
        let eventMap =
                ((gui ^?! wideFocused) (Widget.Surrounding 0 0 0 0) ^. Widget.fEventMap)
                Widget.EventContext
                { Widget._eVirtualCursor = VirtualCursor (Rect 0 0)
                , Widget._ePrevTextRemainder = ""
                }
        _ <- runIdentity (E.lookup (Identity Nothing) event eventMap) ^?! Lens._Just
        pure ()

fromWorkArea ::
    Cache.Functions ->
    Lens.ATraversal'
    (Sugar.WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload) a ->
    T ViewM a
fromWorkArea cache path =
    convertWorkArea cache <&> (^?! Lens.cloneTraversal path)

-- | Test for issue #410
-- https://trello.com/c/00mxkLRG/410-navigating-to-fragment-affects-layout
testFragmentSize :: Test
testFragmentSize =
    testCase "fragment-size" $
    GuiEnv.make >>=
    \baseEnv ->
    testProgram "simple-fragment.json" $
    \cache ->
    do
        frag <- fromWorkArea cache (replExpr . Sugar.annotation)
        guiCursorOnFrag <-
            baseEnv
            & cursor .~ WidgetIds.fromExprPayload frag
            & makeReplGui cache
        guiCursorElseWhere <- makeReplGui cache baseEnv
        unless (guiCursorOnFrag ^. sz == guiCursorElseWhere ^. sz) (fail "fragment size inconsistent")
    where
        sz = Responsive.rWide . Align.tValue . Element.size

-- | Test for issue #375
-- https://trello.com/c/KFLJPNmO/375-operator-precedence-crosses-lambda-boundaries-add-test
testOpPrec :: Test
testOpPrec =
    testCase "apply-operator" $
    GuiEnv.make >>=
    \baseEnv ->
    testProgram "simple-lambda.json" $
    \cache ->
    do
        holeId <-
            fromWorkArea cache
            (replExpr . Sugar.body . Sugar._BodyLam . Sugar.lamFunc .
             Sugar.fBody . Sugar.bbContent . Sugar._BinderExpr .
             Sugar.annotation . Sugar.plEntityId)
            <&> HoleWidgetIds.make
            <&> HoleWidgetIds.hidClosed
        workArea <- convertWorkArea cache
        EventKey KeyEvent
            { keKey = GLFW.Key'7
            , keScanCode = 0 -- dummy
            , keModKeys = GLFW.ModifierKeys True False False False
            , keState = GLFW.KeyState'Pressed
            , keChar = Just '&'
            } & applyEvent cache (baseEnv & cursor .~ holeId)
        workArea' <- convertWorkArea cache
        unless (workAreaEq workArea workArea') (fail "bad operator precedence")

workAreaEq ::
    forall a m.
    Eq a =>
    Sugar.WorkArea (Name (T m)) (T m) (T m) a ->
    Sugar.WorkArea (Name (T m)) (T m) (T m) a ->
    Bool
workAreaEq x y =
    x' == unsafeCoerce y
    where
        x' = unsafeCoerce x :: Sugar.WorkArea (Name Unit) Unit Unit a
