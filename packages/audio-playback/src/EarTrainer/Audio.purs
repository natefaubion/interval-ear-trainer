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

import Effect (Effect)
import EarTrainer.Music (Pitch, PlaybackMode(..), midiNumber)

foreign import data Sampler :: Type

foreign import createSampler :: Effect Sampler
foreign import playIntervalImpl
  :: Sampler
  -> Int
  -> Int
  -> String
  -> Effect Unit
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import playRootImpl
  :: Sampler
  -> Int
  -> Effect Unit
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import stop :: Sampler -> Effect Unit

playInterval
  :: Sampler
  -> PlaybackMode
  -> Pitch
  -> Pitch
  -> Effect Unit
  -> (String -> Effect Unit)
  -> Effect Unit
playInterval sampler mode root target onStarted onError =
  playIntervalImpl sampler (midiNumber root) (midiNumber target) (modeCode mode) onStarted onError

playRoot :: Sampler -> Pitch -> Effect Unit -> (String -> Effect Unit) -> Effect Unit
playRoot sampler root onStarted onError =
  playRootImpl sampler (midiNumber root) onStarted onError

modeCode :: PlaybackMode -> String
modeCode MelodicAscending = "melodic"
modeCode MelodicDescending = "melodic"
modeCode Harmonic = "harmonic"

playbackDurationMilliseconds :: PlaybackMode -> Number
playbackDurationMilliseconds Harmonic = 900.0
playbackDurationMilliseconds _ = 1450.0

rootPlaybackDurationMilliseconds :: Number
rootPlaybackDurationMilliseconds = 900.0
