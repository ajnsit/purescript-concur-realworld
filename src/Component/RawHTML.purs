-- | Concur does not support writing an HTML string to the DOM. This component allows us to do this
-- | at a particular controlled HTML node.
module Conduit.Component.RawHTML where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (createRef, refGetter)
import Concur.React.Props as P
import Conduit.Foreign.Marked (RawHTML, marked)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import React (ReactRef)

-- | For an explanation of how to properly use the PureScript FFI with JavaScript, please see the
-- | `src/Foreign/Marked.js` file and the `Conduit.Foreign.Marked` module.
foreign import unsafeSetInnerHTML :: ReactRef -> RawHTML -> Effect Unit

-- A widget with custom handling
-- Rerenders the markdown everytime the handler is called
markdown :: forall m a. MonadEffect m
  => MultiAlternative m
  => ShiftMap (Widget HTML) m
  => m {wid :: m a, handler :: String -> Effect Unit}
markdown = do
  r <- liftEffect createRef
  let wid = D.div [ P.refProp r P.ref ] []
  let handler str = do
        mref' <- refGetter r
        let mref = join (map toMaybe mref')
        case mref of
          Nothing -> pure unit
          Just elem -> unsafeSetInnerHTML elem $ marked str
  pure {wid, handler}
