module Conduit.Component.HTML.Utils where

import Prelude

import Concur.Core (Widget)
import Concur.React.Props (Props)
import Concur.React.Props as P
import Conduit.Data.Route (Route, routeCodec)
import Control.Alternative (empty)
import Data.Maybe (Maybe(..))
import Routing.Duplex (print)

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
-- AJNOTE: Concur props are not as typesafe
safeHref :: forall a. Route -> Props a
safeHref = P.href <<< append "#" <<< print routeCodec

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall v a i. Monoid v => Maybe a -> (a -> Widget v i) -> Widget v i
maybeElem (Just x) f = f x
-- AJNOTE: Concur has a better empty element than (D.text "")
maybeElem _ _ = empty -- D.text ""

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElem :: forall v i. Monoid v => Boolean -> (Unit -> Widget v i) -> Widget v i
whenElem cond f = if cond then f unit else empty
