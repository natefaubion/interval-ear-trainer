module EarTrainer.Capability.PitchInput
  ( Monitor
  , Sample
  , start
  , stop
  ) where

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude (Unit)

foreign import data Monitor :: Type

type Sample =
  { clarity :: Number
  , decibels :: Number
  , frequency :: Number
  , time :: Number
  }

foreign import startImpl :: (Sample -> Effect Unit) -> EffectFnAff Monitor
foreign import stop :: Monitor -> Effect Unit

start :: (Sample -> Effect Unit) -> Aff Monitor
start onSample = fromEffectFnAff (startImpl onSample)
