module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import EarTrainer.Quiz (Event(..), Phase(..), transition)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
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
