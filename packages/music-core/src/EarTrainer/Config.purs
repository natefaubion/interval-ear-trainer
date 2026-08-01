module EarTrainer.Config
  ( ExerciseConfig
  , defaultConfig
  , isValid
  , toggleInterval
  , togglePlaybackMode
  , toggleRootPitchClass
  ) where

import Prelude

import Data.Array as Array
import EarTrainer.Music
  ( Interval
  , OctavePolicy(..)
  , PitchClass
  , PlaybackMode
  , VocalRangePreset(..)
  , allPlaybackModes
  , defaultIntervals
  , defaultRootPitchClasses
  )

type ExerciseConfig =
  { intervals :: Array Interval
  , octavePolicy :: OctavePolicy
  , playbackModes :: Array PlaybackMode
  , rootPitchClasses :: Array PitchClass
  , vocalRange :: VocalRangePreset
  }

defaultConfig :: ExerciseConfig
defaultConfig =
  { intervals: defaultIntervals
  , octavePolicy: AnyOctave
  , playbackModes: allPlaybackModes
  , rootPitchClasses: defaultRootPitchClasses
  , vocalRange: Tenor
  }

toggleMember :: forall a. Eq a => a -> Array a -> Array a
toggleMember value values
  | Array.elem value values = Array.delete value values
  | otherwise = Array.snoc values value

toggleInterval :: Interval -> ExerciseConfig -> ExerciseConfig
toggleInterval interval config = config { intervals = toggleMember interval config.intervals }

togglePlaybackMode :: PlaybackMode -> ExerciseConfig -> ExerciseConfig
togglePlaybackMode mode config = config { playbackModes = toggleMember mode config.playbackModes }

toggleRootPitchClass :: PitchClass -> ExerciseConfig -> ExerciseConfig
toggleRootPitchClass pitchClass config = config { rootPitchClasses = toggleMember pitchClass config.rootPitchClasses }

isValid :: ExerciseConfig -> Boolean
isValid config =
  not (Array.null config.intervals)
    && not (Array.null config.playbackModes)
    && not (Array.null config.rootPitchClasses)
