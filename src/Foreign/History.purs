-- | This module is a little wrapper over some needed bits of the HTML5 history api
-- | TODO: Use purescript-concur-react-router instead
module Conduit.Foreign.History
  ( setUrl
  ) where

-- | The `setUrl` function is a native JavaScript function we're importing into PureScript.
-- | This function will use the HTML5 History API to set the current page URL. Note that this
-- | will only change the URL bar, it will not cause the browser to load the URL passed, or even check that it exists.
import Data.Unit (Unit)
import Effect (Effect)

-- | The `setUrl` function is a native JavaScript function we're importing into PureScript.
-- | This function will use the HTML5 History API to set the current page URL. Note that this
-- | will only change the URL bar, it will not cause the browser to load the URL passed, or even check that it exists.
foreign import setUrl :: String -> Effect Unit
