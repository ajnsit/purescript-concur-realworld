module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello Sailor!"
  log "You should add some tests."
