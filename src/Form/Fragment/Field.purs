-- | This module provides small form fragments for building form fields
-- | In a framework like React, little bundles of functionality like this might be individual
-- | components. In Concur, they're simple pure functions which produce HTML.
module Conduit.Form.Fragment.Field where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (Props)
import Concur.React.Props as P
import Conduit.Component.HTML.Utils (maybeElem)
import Conduit.Form.Validation (FormError, errorToString)
import Data.Maybe (Maybe)

submit :: String -> Widget HTML Unit
submit buttonText =
  void $ D.button
    [ P.className "btn btn-lg btn-primary pull-xs-right"
    , P.onClick
    ]
    [ D.text buttonText ]

input :: forall e r. Show e ⇒ {input :: String, error :: Maybe FormError | r} -> Array (Props String) -> Widget HTML String
input form props =
  D.fieldset
    [ P.className "form-group" ]
    [ D.input
      ( append
          [ P.className "form-control form-control-lg"
          , P.value $ form.input
          , P.unsafeTargetValue <$> P.onInput
          ]
          props
      )
    , maybeElem form.error (\err ->
        D.div
          [ P.className "error-messages" ]
          [ D.text $ errorToString err ])
    ]
