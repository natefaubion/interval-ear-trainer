module EarTrainer.Capability.PitchInput
  ( Monitor
  , PitchCandidate
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
  { candidates :: Array PitchCandidate
  , decibels :: Number
  , time :: Number
  }

type PitchCandidate =
  { clarity :: Number
  , frequency :: Number
  , windowSize :: Int
  }

type RawSample =
  { candidates :: Array PitchCandidate
  , rms :: Number
  , time :: Number
  }

foreign import startImpl :: (RawSample -> Effect Unit) -> EffectFnAff Monitor
foreign import stop :: Monitor -> Effect Unit

start :: (Sample -> Effect Unit) -> Aff Monitor
start onSample = fromEffectFnAff (startImpl (onSample <<< fromRawSample))

fromRawSample :: RawSample -> Sample
fromRawSample sample =
  { candidates: sample.candidates
  , decibels: decibelsFromRms sample.rms
  , time: sample.time
  }

decibelsFromRms :: Number -> Number
decibelsFromRms rms = 20.0 * log (max rms 1.0e-8) / log 10.0
