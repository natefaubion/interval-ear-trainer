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
  { analyzedAt :: Number
  , clarity :: Number
  , frequency :: Number
  , windowSize :: Int
  }

type AnalysisPlan =
  { minimumIntervalMilliseconds :: Number
  , windowSize :: Int
  }

type RawSample =
  { candidates :: Array PitchCandidate
  , rms :: Number
  , time :: Number
  }

foreign import startImpl :: Array AnalysisPlan -> (RawSample -> Effect Unit) -> EffectFnAff Monitor
foreign import stop :: Monitor -> Effect Unit

start :: (Sample -> Effect Unit) -> Aff Monitor
start onSample = fromEffectFnAff (startImpl defaultAnalysisPlans (onSample <<< fromRawSample))

defaultAnalysisPlans :: Array AnalysisPlan
defaultAnalysisPlans =
  [ { minimumIntervalMilliseconds: 16.0, windowSize: 2048 }
  , { minimumIntervalMilliseconds: 33.0, windowSize: 4096 }
  , { minimumIntervalMilliseconds: 50.0, windowSize: 8192 }
  ]

fromRawSample :: RawSample -> Sample
fromRawSample sample =
  { candidates: sample.candidates
  , decibels: decibelsFromRms sample.rms
  , time: sample.time
  }

decibelsFromRms :: Number -> Number
decibelsFromRms rms = 20.0 * log (max rms 1.0e-8) / log 10.0
