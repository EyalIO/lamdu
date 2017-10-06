{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.Name
    ( Stored
    , Collision(..), _NoCollision, _Collision
    , visible
    , unnamedText
    , Form(..), _AutoGenerated, _Unnamed, _Stored
    , Name(..), form, setName
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Precedence (HasPrecedence(..))

import           Lamdu.Prelude

type Stored = Text

data Collision
    = NoCollision
    | Collision {-Disambiguator:-} Int
    | UnknownCollision -- we have a collision but unknown suffix (inside hole result)
    deriving (Show)

data Form
    = AutoGenerated Text
    | Unnamed Collision
    | Stored Text Collision
    deriving (Show)

unnamedText :: Text
unnamedText = "Unnamed"

visible :: Form -> (Text, Collision)
visible (Unnamed suffix) = (unnamedText, suffix)
visible (Stored name suffix) = (name, suffix)
visible (AutoGenerated name) = (name, NoCollision)

data Name m = Name
    { _form :: Form
    , _setName :: Text -> m ()
    }

Lens.makeLenses ''Name
Lens.makePrisms ''Collision
Lens.makePrisms ''Form

instance Show (Name m) where
    show name = concat [ "(Name ", show (name ^. form), " ", ")" ]

instance HasPrecedence Form where
    -- | Returns a precedence between 0..10
    precedence name =
        visible name ^? _1 . Lens.ix 0 . Lens.to Chars.precedence & fromMaybe 10

instance HasPrecedence (Name m) where precedence = precedence . _form
