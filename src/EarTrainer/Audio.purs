module EarTrainer.Audio
  ( Sampler
  , createSampler
  , playInterval
  , playbackDurationMilliseconds
  , playRoot
  , rootPlaybackDurationMilliseconds
  , stop
  ) where

import Prelude

import EarTrainer.Music (Pitch, PlaybackMode(..), midiNumber)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Sampler :: Type

type NoteEvent =
  { durationMilliseconds :: Number
  , notes :: Array Int
  , startMilliseconds :: Number
  }

foreign import createSampler :: Effect Sampler
foreign import playImpl :: Sampler -> Array NoteEvent -> Number -> EffectFnAff Unit
foreign import stop :: Sampler -> Effect Unit

playInterval :: Sampler -> PlaybackMode -> Pitch -> Pitch -> Aff Unit
playInterval sampler mode root target =
  fromEffectFnAff (playImpl sampler (intervalPlan mode (midiNumber root) (midiNumber target)) (playbackDurationMilliseconds mode))

playRoot :: Sampler -> Pitch -> Aff Unit
playRoot sampler root =
  fromEffectFnAff
    ( playImpl sampler
        [ { durationMilliseconds: rootPlaybackDurationMilliseconds
          , notes: [ midiNumber root ]
          , startMilliseconds: 0.0
          }
        ]
        rootPlaybackDurationMilliseconds
    )

intervalPlan :: PlaybackMode -> Int -> Int -> Array NoteEvent
intervalPlan Harmonic root target =
  [ { durationMilliseconds: 900.0
    , notes: [ root, target ]
    , startMilliseconds: 0.0
    }
  ]
intervalPlan _ root target =
  [ { durationMilliseconds: 650.0
    , notes: [ root ]
    , startMilliseconds: 0.0
    }
  , { durationMilliseconds: 650.0
    , notes: [ target ]
    , startMilliseconds: 800.0
    }
  ]

playbackDurationMilliseconds :: PlaybackMode -> Number
playbackDurationMilliseconds Harmonic = 900.0
playbackDurationMilliseconds _ = 1450.0

rootPlaybackDurationMilliseconds :: Number
rootPlaybackDurationMilliseconds = 900.0
