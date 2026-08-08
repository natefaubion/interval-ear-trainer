module EarTrainer.Capability.PitchInput
  ( Monitor
  , Sample
  , start
  , stop
  ) where

import Effect (Effect)
import Prelude (Unit)

foreign import data Monitor :: Type

type Sample =
  { clarity :: Number
  , decibels :: Number
  , frequency :: Number
  , time :: Number
  }

foreign import start :: (Sample -> Effect Unit) -> (String -> Effect Unit) -> Effect Monitor
foreign import stop :: Monitor -> Effect Unit
