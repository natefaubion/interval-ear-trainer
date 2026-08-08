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
import Test.Assert (assertEqual, assertTrue')

run :: Effect Unit
run = do
  assertEqual { actual: midiNumber (pitch A (Accidental 0) 4), expected: 69 }
  assertEqual { actual: transpose Ascending MajorThird (pitch C (Accidental 1) 4), expected: pitch E (Accidental 1) 4 }
  assertEqual { actual: transpose Descending MajorSecond (pitch C (Accidental 0) 4), expected: pitch B (Accidental (-1)) 3 }
  assertEqual { actual: transpose Ascending AugmentedFourth (pitch C (Accidental 0) 4), expected: pitch F (Accidental 1) 4 }
  assertEqual { actual: transpose Ascending DiminishedFifth (pitch C (Accidental 0) 4), expected: pitch G (Accidental (-1)) 4 }
  assertEqual { actual: transpose Ascending AugmentedFifth (pitch C (Accidental 0) 4), expected: pitch G (Accidental 1) 4 }
  assertEqual { actual: transpose Ascending MinorThird (pitch D (Accidental (-1)) 4), expected: pitch F (Accidental (-1)) 4 }
  assertTrue' "C to E is a major third"
    (intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch E (Accidental 0) 4) == Just MajorThird)
  assertTrue' "D to F is a minor third"
    (intervalBetween Ascending (pitch D (Accidental 0) 4) (pitch F (Accidental 0) 4) == Just MinorThird)
  assertTrue' "F to B is an augmented fourth"
    (intervalBetween Ascending (pitch F (Accidental 0) 4) (pitch B (Accidental 0) 4) == Just AugmentedFourth)
  assertTrue' "B to F is a diminished fifth"
    (intervalBetween Ascending (pitch B (Accidental 0) 4) (pitch F (Accidental 0) 5) == Just DiminishedFifth)
  assertTrue' "C to G-sharp is an augmented fifth"
    (intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch G (Accidental 1) 4) == Just AugmentedFifth)
  assertTrue' "D-flat is selectable" (Array.elem (PitchClass D (Accidental (-1))) allRootPitchClasses)
  assertTrue' "C-flat is selectable" (Array.elem (PitchClass C (Accidental (-1))) allRootPitchClasses)
  assertTrue' "F-flat is selectable" (Array.elem (PitchClass F (Accidental (-1))) allRootPitchClasses)
  assertTrue' "E-sharp is selectable" (Array.elem (PitchClass E (Accidental 1)) allRootPitchClasses)
  assertTrue' "B-sharp is selectable" (Array.elem (PitchClass B (Accidental 1)) allRootPitchClasses)
  assertEqual { actual: Array.length allMajorKeyPresets, expected: 15 }
  assertEqual
    { actual: presetRange ExtraWide
    , expected: { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
    }
  assertEqual { actual: pitchFromMidiLike (pitch D (Accidental (-1)) 4) 61, expected: pitch D (Accidental (-1)) 4 }
  assertEqual { actual: pitchFromMidiLike (pitch C (Accidental 1) 4) 61, expected: pitch C (Accidental 1) 4 }
