module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import EarTrainer.Config (defaultConfig, isValid, toggleInterval)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , midiNumber
  , pitch
  , transpose
  )
import EarTrainer.Quiz (Event(..), Phase(..), transition)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  assertEqual
    { actual: midiNumber (pitch A (Accidental 0) 4)
    , expected: 69
    }
  assertEqual
    { actual: transpose Ascending MajorThird (pitch C (Accidental 1) 4)
    , expected: pitch E (Accidental 1) 4
    }
  assertEqual
    { actual: transpose Descending MajorSecond (pitch C (Accidental 0) 4)
    , expected: pitch B (Accidental (-1)) 3
    }
  assertEqual
    { actual: transpose Ascending AugmentedFourth (pitch C (Accidental 0) 4)
    , expected: pitch F (Accidental 1) 4
    }
  assertEqual
    { actual: transpose Ascending DiminishedFifth (pitch C (Accidental 0) 4)
    , expected: pitch G (Accidental (-1)) 4
    }
  assertEqual
    { actual: isValid (toggleInterval MinorThird defaultConfig)
    , expected: true
    }
  assertEqual
    { actual: transition SingingFirstNote FirstPitchAccepted
    , expected: Just AwaitingRearticulation
    }
  assertEqual
    { actual: transition AwaitingRearticulation SecondPitchAccepted
    , expected: Nothing
    }
  assertEqual
    { actual: transition AwaitingRearticulation VoiceReleased
    , expected: Just SingingSecondNote
    }
