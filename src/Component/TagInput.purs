-- | Conduit has little functionality to warrant encapsulated state, but the tag input is one.
-- | This component manages a text input that supports key events to manage a list of tags on
-- | an article.
module Conduit.Component.TagInput where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Widgets (textInputEnter)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set

data Query
  = InsertTag String
  | RemoveTag Tag

type State =
  { text :: String
  , tags :: Set Tag
  }

newtype Tag = Tag String

derive instance newtypeTag :: Newtype Tag _
derive instance eqTag :: Eq Tag
derive instance ordTag :: Ord Tag

component :: Set Tag -> Widget HTML (Set Tag)
component tags = do
  axn <- D.fieldset [ P.className "form-group" ]
    [ InsertTag <$> textInputEnter "" true [P.placeholder "Enter tags", P.className "form-control"]
    , D.div [ P.className "tag-list" ]
      (map renderTag (Set.toUnfoldable tags))
    ]
  case axn of
    InsertTag s -> pure $ Set.insert (Tag s) tags
    RemoveTag t -> pure $ Set.delete t tags
  where
  -- renderTag :: Tag -> H.ComponentHTML Query
  renderTag tag = D.span
    [ P.className "tag-default tag-pill" ]
    [ D.i
      [ P.className "ion-close-round"
      , RemoveTag tag <$ P.onClick
      ] [ ]
    , D.text (unwrap tag)
    ]
