module Test.Main where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import EarTrainer.Config (defaultConfig, isValid, toggleInterval)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , OctavePolicy(..)
  , PlaybackMode(..)
  , midiNumber
  , pitch
  , presetRange
  , transpose
  )
import EarTrainer.PitchDetection
  ( RecognitionPhase(..)
  , defaultRecognitionSettings
  , initialRecognition
  , nearestMidi
  , relativeMidi
  , stepRecognition
  )
import EarTrainer.Quiz (Event(..), Phase(..), makeChoices, makePrompt, transition)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  let
    c4 = pitch C (Accidental 0) 4
    e4 = pitch E (Accidental 0) 4
    c4Sample = { frequency: 261.625565, clarity: 0.98 }
    c3Sample = { frequency: 130.812783, clarity: 0.98 }
    c5Sample = { frequency: 523.251131, clarity: 0.98 }
    e4Sample = { frequency: 329.627557, clarity: 0.98 }
    silence = { frequency: 0.0, clarity: 0.0 }
    step sample recognition =
      stepRecognition defaultRecognitionSettings AnyOctave c4 e4 sample recognition
    afterFirst = foldl (\recognition _ -> step c4Sample recognition) initialRecognition (Array.replicate 12 unit)
    afterRelease = foldl (\recognition _ -> step silence recognition) afterFirst (Array.replicate 5 unit)
    afterSecond = foldl (\recognition _ -> step e4Sample recognition) afterRelease (Array.replicate 12 unit)
    anyOctaveFirst = foldl (\recognition _ -> step c5Sample recognition) initialRecognition (Array.replicate 12 unit)
    octaveBelowFirst = foldl (\recognition _ -> step c3Sample recognition) initialRecognition (Array.replicate 12 unit)
    writtenOctaveFirst = foldl
      ( \recognition _ ->
          stepRecognition defaultRecognitionSettings WrittenOctave c4 e4 c5Sample recognition
      )
      initialRecognition
      (Array.replicate 12 unit)
    descendingConfig = defaultConfig { playbackModes = [ MelodicDescending ] }
    generatedPrompt = makePrompt 128 descendingConfig
    generatedChoices = makeChoices 128 generatedPrompt
    generatedRange = presetRange descendingConfig.vocalRange
  assertEqual
    { actual: nearestMidi 440.0
    , expected: 69
    }
  assertEqual
    { actual: afterFirst.phase
    , expected: WaitingForRelease
    }
  assertEqual
    { actual: afterRelease.phase
    , expected: WaitingForSecond
    }
  assertEqual
    { actual: afterSecond.phase
    , expected: RecognitionComplete
    }
  assertEqual
    { actual: anyOctaveFirst.phase
    , expected: WaitingForRelease
    }
  assertEqual
    { actual: writtenOctaveFirst.phase
    , expected: WaitingForFirst
    }
  assertEqual
    { actual: relativeMidi AnyOctave c4 octaveBelowFirst 57
    , expected: 69
    }
  assertEqual
    { actual: relativeMidi WrittenOctave c4 octaveBelowFirst 57
    , expected: 57
    }
  assertEqual
    { actual: Array.length generatedChoices
    , expected: 4
    }
  assertEqual
    { actual: Array.length (Array.filter (\choice -> choice.interval == generatedPrompt.interval) generatedChoices)
    , expected: 1
    }
  assertEqual
    { actual:
        midiNumber generatedPrompt.root >= midiNumber generatedRange.low
          && midiNumber generatedPrompt.target >= midiNumber generatedRange.low
          && midiNumber generatedPrompt.root <= midiNumber generatedRange.high
          && midiNumber generatedPrompt.target <= midiNumber generatedRange.high
    , expected: true
    }
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
