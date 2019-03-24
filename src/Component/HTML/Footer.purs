-- | This module exports a pure HTML function to render a consistent footer throughout the app.
module Conduit.Component.HTML.Footer where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P

footer :: forall a. Widget HTML a
footer =
  D.footer'
    [ D.div
      [ P.className "container" ]
      [ D.a
        [ P.className "logo-font"
        , P.href "/"
        ]
        [ D.text "conduit" ]
      , D.span
        [ P.className "attribution" ]
        [ D.text "An interactive learning project from "
        , D.a
          [ P.href "https://thinkster.io" ]
          [ D.text "Thinkster" ]
        , D.text ". Code & design licensed under MIT. Implemented by "
        , D.a
          [ P.href "https://thomashoneyman.com" ]
          [ D.text "Thomas Honeyman" ]
        , D.text "."
        ]
      ]
    ]
