{-# LANGUAGE TypeFamilies #-}
module Lamdu.GUI.Expr.HoleEdit.ValTerms
    ( expr
    , binder
    , allowedSearchTermCommon
    , allowedFragmentSearchTerm
    , getSearchStringRemainder
    , verifyInjectSuffix
    , definitePart
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import           Hyper
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..), Collision(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

collisionText :: Name.Collision -> Text
collisionText NoCollision = ""
collisionText (Collision i) = Text.pack (show i)
collisionText UnknownCollision = "?"

ofName :: Name -> [Text]
ofName Name.Unnamed{} = []
ofName (Name.AutoGenerated text) = [text]
ofName (Name.NameTag x) =
    [ displayName
        <> collisionText textCollision
        <> collisionText (x ^. Name.tnTagCollision)
    ]
    where
        Name.TagText displayName textCollision = x ^. Name.tnDisplayText

expr ::
    ( Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Annotated a # Term v Name i o -> [Text]
expr env = ofBody env . (^. hVal)

ofBody ::
    ( Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Term v Name i o # Annotated a -> [Text]
ofBody env =
    \case
    BodyLam {} ->
        [ env ^. has . Texts.lambda
        , "\\", "Λ", "λ", "->", "→"
        ]
    BodySimpleApply x ->
        env ^. has . Texts.apply : x ^. htraverse1 . Lens.to (expr env)
    BodyLabeledApply x ->
        env ^. has . Texts.apply
        : ofName (x ^. aFunc . hVal . Lens._Wrapped . bvNameRef . nrName)
        ++ (x ^.. aAnnotatedArgs . Lens.folded . aaTag . tagName >>= ofName)
    BodyRecord {} ->
        -- We don't allow completing a record by typing one of its
        -- field names/vals
        ["{}", "()", "[]"]
    BodyGetField gf ->
        ofName (gf ^. gfTag . tagRefTag . tagName) <&> ("." <>)
    BodyCase cas ->
        [ env ^. has . Texts.case_
        , env ^. has . Texts.of_
        ] ++
        case cas of
            Case LambdaCase (Composite [] [] ClosedComposite{} _) ->
                [env ^. has . Texts.absurd]
            _ -> []
    BodyIfElse {} -> [env ^. has . Texts.if_, ":"]
    -- An inject "base expr" can have various things in its val filled
    -- in, so the result group based on it may have both nullary
    -- inject (".") and value inject (":"). Thus, an inject must match
    -- both.
    -- So these terms are used to filter the whole group, and then
    -- isExactMatch (see below) is used to filter each entry.
    BodyInject (Inject tag _) ->
        (<>) <$> ofName (tag ^. tagRefTag . tagName) <*> [":", "."]
    BodyLiteral {} -> []
    BodyGetVar GetParamsRecord {} -> [env ^. has . Texts.paramsRecordOpener]
    BodyGetVar (GetParam x) -> ofName (x ^. pNameRef . nrName)
    BodyGetVar (GetBinder x) ->
        ofName (x ^. bvNameRef . nrName)
        & maybePrefixDot x
    BodyToNom (Nominal tid b) ->
        ofName (tid ^. tidName)
        ++ b ^. SugarLens.binderResultExpr . Lens.asIndex . Lens.to (ofBody env)
    BodyFromNom tid ->
        ofName (tid ^. tidName) <>
        -- The hole's "extra" apply-form results will be an
        -- IfElse, but we give val terms only to the base expr
        -- which looks like this:
        [ env ^. has . Texts.if_
        | tid ^. tidTId == Builtins.boolTid
        ]
    BodyHole {} -> []
    BodyFragment {} -> []
    BodyPlaceHolder {} -> []
    where
        maybePrefixDot bv =
            case bv ^. bvForm of
            GetDefinition _
                | bv ^. bvNameRef . nrName & Name.isOperator & not -> (>>= \x -> [x, "." <> x])
            _ -> id

binder ::
    ( Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Binder v Name i o # Annotated a -> [Text]
binder env BinderLet{} = [env ^. has . Texts.let_]
binder env (BinderTerm x) = ofBody env x

type Suffix = Char

allowedSearchTermCommon :: [Suffix] -> Text -> Bool
allowedSearchTermCommon suffixes searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , isAlphaNumericName
    , (`Text.isPrefixOf` "{}")
    , (== "\\")
    , Lens.has (Lens.reversed . Lens._Cons . Lens.filtered inj)
    , -- Allow typing records in wrong direction of keyboard input,
      -- for example when editing in right-to-left but not switching the input language.
      -- Then the '}' key would had inserted a '{' but inserts a '}'.
      -- In this case it would probably help to still allow it
      -- as the user intended to create a record.
      (== "}")
    ]
    where
        inj (lastChar, revInit) =
            lastChar `elem` suffixes && Text.all Char.isAlphaNum revInit

isAlphaNumericName :: Text -> Bool
isAlphaNumericName t =
    case Text.uncons t of
    Nothing -> True
    Just ('.', xs) -> isAlphaNumericSuffix xs
    Just _ -> isAlphaNumericSuffix t
    where
        isAlphaNumericSuffix suffix =
            case Text.uncons suffix of
            Nothing -> True
            Just (x, xs) -> Char.isAlpha x && Text.all Char.isAlphaNum xs

allowedFragmentSearchTerm :: Text -> Bool
allowedFragmentSearchTerm searchTerm =
    allowedSearchTermCommon ":" searchTerm || isGetField searchTerm
    where
        isGetField t =
            case Text.uncons t of
            Just (c, rest) -> c == '.' && Text.all Char.isAlphaNum rest
            Nothing -> False

-- | Given a hole result sugared expression, determine which part of
-- the search term is a remainder and which belongs inside the hole
-- result expr
getSearchStringRemainder :: SearchMenu.ResultsContext -> Term v name i o # Ann a -> Text
getSearchStringRemainder ctx holeResult
    | isA _BodyInject = ""
      -- NOTE: This is wrong for operator search terms like ".." which
      -- should NOT have a remainder, but do. We might want to correct
      -- that.  However, this does not cause any bug because search
      -- string remainders are genreally ignored EXCEPT in
      -- apply-operator, which does not occur when the search string
      -- already is an operator.
    | isSuffixed ":" = ":"
    | isSuffixed "." = "."
    | otherwise = ""
    where
        isSuffixed suffix = Text.isSuffixOf suffix (ctx ^. SearchMenu.rSearchTerm)
        fragmentExpr = _BodyFragment . fExpr
        isA x = any (`Lens.has` holeResult) [x, fragmentExpr . hVal . x]

verifyInjectSuffix :: Text -> Term v name i o f -> Bool
verifyInjectSuffix searchTerm x =
    case suffix of
    Just ':' | Lens.has (injectContent . _InjectNullary) x -> False
    Just '.' | Lens.has (injectContent . _InjectVal) x -> False
    _ -> True
    where
        suffix = searchTerm ^? Lens.reversed . Lens._Cons . _1
        injectContent = _BodyInject . iContent

-- | Returns the part of the search term that is DEFINITELY part of
-- it. Some of the stripped suffix may be part of the search term,
-- depending on the val.
definitePart :: Text -> Text
definitePart searchTerm
    | Text.any Char.isAlphaNum searchTerm
    && any (`Text.isSuffixOf` searchTerm) [":", "."] = Text.init searchTerm
    | otherwise = searchTerm
