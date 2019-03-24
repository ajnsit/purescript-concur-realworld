-- | The follow buttons in Conduit don't have enough encapsulated state or behaviors to be a full
-- | component, but do need to trigger certain actions in a parent component. To avoid writing the
-- | same query handler over and over again, we'll export both the pure HTML function and a default
-- | handle from this module.
module Conduit.Component.Part.FollowButton
  ( followButton
  , follow
  , unfollow
  ) where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Conduit.Capability.Resource.User (class ManageUser, followUser, unfollowUser)
import Conduit.Data.Profile (Author, Relation(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Class as S
import Data.Foldable (for_)
import Data.Lens (Traversal', preview, set)
import Data.Maybe (Maybe)

-- | Our follow button will have behavior that depends on the author we are interacting with.
-- | Since the author's type already includes information about whether we follow this author, we
-- | can use that to control the behavior of this HTML with the author type and some embedded
-- | queries alone.
followButton
  :: forall p
   . p
  -> p
  -> Author
  -> Widget HTML p
followButton followQuery unfollowQuery author = case author.relation of
  Following ->
    D.button
      [ P.className "btn btn-sm action-btn btn-secondary"
      , unfollowQuery <$ P.onClick
      ]
      [ D.text $ " Unfollow " <> Username.toString author.username ]
  NotFollowing ->
    D.button
      [ P.className "btn btn-sm action-btn btn-outline-secondary"
      , followQuery <$ P.onClick
      ]
      [ D.i
        [ P.className "ion-plus-round"]
        []
      , D.text $ " Follow " <> Username.toString author.username
      ]
  You -> D.text ""


-- | In addition to this pure HTML renderer, however, we'd also like to supply the logic that will
-- | work with the queries we've embedded. These two functions will take care of everything we need
-- | in `eval` for a component which loads an author and then performs follow / unfollow actions
-- | on it.
-- |
-- | In most cases I don't make assumptions about what is in state nor modify it, but in this case
-- | I'm willing to adopt the convention that somewhere in state is an author that can be modified.
-- |
-- | The following two functions will handle safely making the request, logging errors, and updating
-- | state with the result.

follow
  :: forall s m
   . ManageUser m
  => MonadState s m
  => Traversal' s Author
  -> m Unit
follow _author = act (eq NotFollowing <<< _.relation) followUser _author

unfollow
  :: forall s m
   . ManageUser m
  => MonadState s m
  => Traversal' s Author
  -> m Unit
unfollow _author = act (eq Following <<< _.relation) unfollowUser _author

-- | This will be kept internal, as it is only used to implement `follow` and `unfollow`.
act
  :: forall s m
   . ManageUser m
  => MonadState s m
  => (Author -> Boolean)
  -> (Username -> m (Maybe Author))
  -> Traversal' s Author
  -> m Unit
act cond f _author = do
  st <- S.get
  for_ (preview _author st) \author -> do
    when (cond author) do
      mbProfile <- f author.username
      for_ mbProfile $ S.modify_ <<< set _author
