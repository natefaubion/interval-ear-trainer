module Test.Music (run) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , PitchClass(..)
  , VocalRangePreset(..)
  , allMajorKeyPresets
  , allRootPitchClasses
  , intervalBetween
  , midiNumber
  , pitch
  , pitchFromMidiLike
  , presetRange
  , transpose
  )
import Effect (Effect)
import Test.Assert (assertEqual)

run :: Effect Unit
run = do
  assertEqual { actual: midiNumber (pitch A (Accidental 0) 4), expected: 69 }
  assertEqual
    { actual:
        [ transpose Ascending MajorThird (pitch C (Accidental 1) 4) == pitch E (Accidental 1) 4
        , transpose Descending MajorSecond (pitch C (Accidental 0) 4) == pitch B (Accidental (-1)) 3
        , transpose Ascending AugmentedFourth (pitch C (Accidental 0) 4) == pitch F (Accidental 1) 4
        , transpose Ascending DiminishedFifth (pitch C (Accidental 0) 4) == pitch G (Accidental (-1)) 4
        , transpose Ascending AugmentedFifth (pitch C (Accidental 0) 4) == pitch G (Accidental 1) 4
        , transpose Ascending MinorThird (pitch D (Accidental (-1)) 4) == pitch F (Accidental (-1)) 4
        ]
    , expected: Array.replicate 6 true
    }
  assertEqual
    { actual:
        [ intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch E (Accidental 0) 4) == Just MajorThird
        , intervalBetween Ascending (pitch D (Accidental 0) 4) (pitch F (Accidental 0) 4) == Just MinorThird
        , intervalBetween Ascending (pitch F (Accidental 0) 4) (pitch B (Accidental 0) 4) == Just AugmentedFourth
        , intervalBetween Ascending (pitch B (Accidental 0) 4) (pitch F (Accidental 0) 5) == Just DiminishedFifth
        , intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch G (Accidental 1) 4) == Just AugmentedFifth
        ]
    , expected: Array.replicate 5 true
    }
  assertEqual
    { actual:
        [ Array.elem (PitchClass D (Accidental (-1))) allRootPitchClasses
        , Array.elem (PitchClass C (Accidental (-1))) allRootPitchClasses
        , Array.elem (PitchClass F (Accidental (-1))) allRootPitchClasses
        , Array.elem (PitchClass E (Accidental 1)) allRootPitchClasses
        , Array.elem (PitchClass B (Accidental 1)) allRootPitchClasses
        , Array.length allMajorKeyPresets == 15
        ]
    , expected: Array.replicate 6 true
    }
  assertEqual
    { actual: presetRange ExtraWide
    , expected: { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
    }
  assertEqual
    { actual:
        [ pitchFromMidiLike (pitch D (Accidental (-1)) 4) 61 == pitch D (Accidental (-1)) 4
        , pitchFromMidiLike (pitch C (Accidental 1) 4) 61 == pitch C (Accidental 1) 4
        ]
    , expected: [ true, true ]
    }
