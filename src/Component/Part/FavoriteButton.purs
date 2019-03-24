-- | The favorite buttons in Conduit don't have enough encapsulated state or behaviors to be a full
-- | component, but do need to trigger certain actions in a parent component. To avoid writing the
-- | same query handler over and over again, we'll export both the pure HTML function and a default
-- | handle from this module.
-- |
-- | For a more in-depth example of this pattern, see the `Conduit.Component.Part.FollowButton`
-- | module.
module Conduit.Component.Part.FavoriteButton
  ( ButtonSize(..)
  , favoriteButton
  , favorite
  , unfavorite
  ) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Conduit.Capability.Resource.Article (class ManageArticle, favoriteArticle, unfavoriteArticle)
import Conduit.Data.Article (ArticleWithMetadata)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as S
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Data.Maybe (Maybe)
import Slug (Slug)

-- A simple way to control button sizes

data ButtonSize
  = Icon
  | Medium

derive instance eqButtonSize :: Eq ButtonSize

favoriteButton
  :: forall a
   . ButtonSize
  -> a
  -> a
  -> ArticleWithMetadata
  -> Widget HTML a -- (p Unit)
favoriteButton buttonSize favoriteQuery unfavoriteQuery article =
  D.button
    [ P.className $ "btn btn-sm " <> if article.favorited then "btn-primary" else "btn-outline-primary"
    , (if article.favorited then unfavoriteQuery else favoriteQuery) <$ P.onClick
    ]
    [ D.i
      [ P.className "ion-heart" ]
      []
    , D.span'
      [ D.text $ case article.favorited, buttonSize of
          true, Medium -> " Unfavorite Article"
          _, Medium -> " Favorite Article"
          _, _ -> " "
      ]
    , D.span
      [ P.className "counter" ]
      [ D.text $ case buttonSize of
          Icon -> " " <> show article.favoritesCount
          _ -> " (" <> show article.favoritesCount <> ")" ]
    ]

-- Eval

favorite
  :: forall s m
   . ManageArticle m
  => MonadState s m
  => Traversal' s ArticleWithMetadata
  -> m Unit
favorite _article = act (not <<< _.favorited) favoriteArticle _article

unfavorite
  :: forall s m
   . ManageArticle m
  => MonadState s m
  => Traversal' s ArticleWithMetadata
  -> m Unit
unfavorite _article = act _.favorited unfavoriteArticle _article

-- This will be kept internal.

act
  :: forall s m
   . ManageArticle m
  => MonadState s m
  => (ArticleWithMetadata -> Boolean)
  -> (Slug -> m (Maybe ArticleWithMetadata))
  -> Traversal' s ArticleWithMetadata
  -> m Unit
act cond f _article = do
  st <- S.get
  for_ (preview _article st) \article -> do
    when (cond article) do
      mbArticle <- f article.slug
      for_ mbArticle $ S.modify_ <<< set _article
