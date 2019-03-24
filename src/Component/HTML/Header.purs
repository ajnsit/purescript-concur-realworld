-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Conduit.Component.HTML.Header where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Conduit.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Profile (ProfileRep)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)


-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not. We'll allow for any profile record type so
-- | long as it has our core fields -- this makes the header reusable across pages despite which
-- | variation on `Profile` they use.
header :: forall r a. Maybe { | ProfileRep r } -> Route -> Widget HTML a
header currentUser route =
  D.nav
    [ P.className "navbar navbar-light" ]
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "navbar-brand"
        , safeHref Home
        ]
        [ D.text "conduit" ]
      , D.ul
        [ P.className "nav navbar-nav pull-xs-right" ]
        [ navItem Home
            [ D.text "Home" ]
        , whenElem (isJust currentUser) \_ ->
            navItem Editor
              [ D.i
                [ P.className "ion-compose" ]
                [ D.text " New Post" ]
              ]
        , whenElem (isJust currentUser) \_ ->
            navItem Settings
              [ D.i
                [ P.className "ion-gear-a" ]
                [ D.text " Settings" ]
              ]
        , maybeElem currentUser \profile ->
            navItem (Profile profile.username)
              [ D.img
                [ P.className "user-pic"
                , P.src $ Avatar.toStringWithDefault profile.image
                ]
              , D.text $ Username.toString profile.username
              ]
        , whenElem (isNothing currentUser) \_ ->
            navItem Login
              [ D.text "Log in" ]
        , whenElem (isNothing currentUser) \_ ->
            navItem Register
              [ D.text "Sign up" ]
        ]
      ]
    ]

  where

  navItem r html =
    D.li
      [ P.className "nav-item" ]
      [ D.a
        [ P.className $ "nav-link" <> guard (route == r) " active"
        , safeHref r
        ]
        html
      ]
