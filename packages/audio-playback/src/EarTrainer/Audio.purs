module EarTrainer.Audio
  ( Sampler
  , createSampler
  , playInterval
  , playbackDurationMilliseconds
  , stop
  ) where

import Prelude

import Effect (Effect)
import EarTrainer.Music (Pitch, PlaybackMode(..), midiNumber)

foreign import data Sampler :: Type

foreign import createSampler :: Effect Sampler
foreign import playIntervalImpl :: Sampler -> Int -> Int -> String -> Effect Unit
foreign import stop :: Sampler -> Effect Unit

playInterval :: Sampler -> PlaybackMode -> Pitch -> Pitch -> Effect Unit
playInterval sampler mode root target =
  playIntervalImpl sampler (midiNumber root) (midiNumber target) (modeCode mode)

modeCode :: PlaybackMode -> String
modeCode MelodicAscending = "melodic"
modeCode MelodicDescending = "melodic"
modeCode Harmonic = "harmonic"

playbackDurationMilliseconds :: PlaybackMode -> Number
playbackDurationMilliseconds Harmonic = 900.0
playbackDurationMilliseconds _ = 1450.0
