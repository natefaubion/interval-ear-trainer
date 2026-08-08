module EarTrainer.Capability.PitchInput
  ( Monitor
  , Sample
  , decibelsFromRms
  , start
  , stop
  ) where

import Prelude

import Data.Number (log)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Monitor :: Type

type Sample =
  { clarity :: Number
  , decibels :: Number
  , frequency :: Number
  , time :: Number
  }

type RawSample =
  { clarity :: Number
  , frequency :: Number
  , rms :: Number
  , time :: Number
  }

foreign import startImpl :: (RawSample -> Effect Unit) -> EffectFnAff Monitor
foreign import stop :: Monitor -> Effect Unit

start :: (Sample -> Effect Unit) -> Aff Monitor
start onSample = fromEffectFnAff (startImpl (onSample <<< fromRawSample))

fromRawSample :: RawSample -> Sample
fromRawSample sample =
  { clarity: sample.clarity
  , decibels: decibelsFromRms sample.rms
  , frequency: sample.frequency
  , time: sample.time
  }

decibelsFromRms :: Number -> Number
decibelsFromRms rms = 20.0 * log (max rms 1.0e-8) / log 10.0
