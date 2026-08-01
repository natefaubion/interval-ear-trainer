module EarTrainer.Quiz
  ( Event(..)
  , IntervalChoice
  , Phase(..)
  , Prompt
  , makeChoices
  , makePrompt
  , transition
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import EarTrainer.Config (ExerciseConfig)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , allIntervals
  , midiNumber
  , presetRange
  , transpose
  )

type Prompt =
  { interval :: Interval
  , mode :: PlaybackMode
  , root :: Pitch
  , target :: Pitch
  }

type IntervalChoice =
  { interval :: Interval
  , target :: Pitch
  }

directionFor :: PlaybackMode -> Direction
directionFor MelodicDescending = Descending
directionFor _ = Ascending

pick :: forall a. a -> Int -> Array a -> a
pick fallback seed values =
  fromMaybe fallback (Array.index values (abs seed `mod` max 1 (Array.length values)))

makePrompt :: Int -> ExerciseConfig -> Prompt
makePrompt seed config =
  let
    interval = pick MinorThird seed config.intervals
    mode = pick MelodicAscending (seed `div` 7) config.playbackModes
    direction = directionFor mode
    range = presetRange config.vocalRange
    Pitch _ lowOctave = range.low
    Pitch _ highOctave = range.high
    roots = do
      octave <- Array.range (lowOctave - 1) (highOctave + 1)
      pitchClass <- config.rootPitchClasses
      let
        candidateRoot = Pitch pitchClass octave
        target = transpose direction interval candidateRoot
      if
        midiNumber candidateRoot >= midiNumber range.low
          && midiNumber candidateRoot <= midiNumber range.high
          && midiNumber target >= midiNumber range.low
          && midiNumber target <= midiNumber range.high then
        pure candidateRoot
      else
        []
    fallbackRoot = Pitch (PitchClass C (Accidental 0)) lowOctave
    root = pick fallbackRoot (seed `div` 13) roots
  in
    { interval
    , mode
    , root
    , target: transpose direction interval root
    }

makeChoices :: Int -> Prompt -> Array IntervalChoice
makeChoices seed prompt =
  let
    distractorPool = Array.filter (_ /= prompt.interval) allIntervals
    poolLength = Array.length distractorPool
    offset = abs (seed `div` 17) `mod` max 1 poolLength
    rotated = Array.drop offset distractorPool <> Array.take offset distractorPool
    intervals = Array.take 3 rotated
    correctPosition = abs (seed `div` 31) `mod` 4
    withCorrect = fromMaybe intervals (Array.insertAt correctPosition prompt.interval intervals)
    direction = directionFor prompt.mode
  in
    map
      (\interval -> { interval, target: transpose direction interval prompt.root })
      withCorrect

data Phase
  = Configuring
  | ShowingPrompt
  | PlayingInterval
  | WaitingForSilence
  | SingingFirstNote
  | AwaitingRearticulation
  | SingingSecondNote
  | ChoosingNotation
  | RevealingAnswer

derive instance Eq Phase

instance Show Phase where
  show Configuring = "Configuring"
  show ShowingPrompt = "ShowingPrompt"
  show PlayingInterval = "PlayingInterval"
  show WaitingForSilence = "WaitingForSilence"
  show SingingFirstNote = "SingingFirstNote"
  show AwaitingRearticulation = "AwaitingRearticulation"
  show SingingSecondNote = "SingingSecondNote"
  show ChoosingNotation = "ChoosingNotation"
  show RevealingAnswer = "RevealingAnswer"

data Event
  = BeginQuiz
  | PromptReady
  | PlaybackFinished
  | RoomIsQuiet
  | FirstPitchAccepted
  | VoiceReleased
  | SecondPitchAccepted
  | ChoiceSubmitted Boolean
  | Continue

transition :: Phase -> Event -> Maybe Phase
transition Configuring BeginQuiz = Just ShowingPrompt
transition ShowingPrompt PromptReady = Just PlayingInterval
transition PlayingInterval PlaybackFinished = Just WaitingForSilence
transition WaitingForSilence RoomIsQuiet = Just SingingFirstNote
transition SingingFirstNote FirstPitchAccepted = Just AwaitingRearticulation
transition AwaitingRearticulation VoiceReleased = Just SingingSecondNote
transition SingingSecondNote SecondPitchAccepted = Just ChoosingNotation
transition ChoosingNotation (ChoiceSubmitted _) = Just RevealingAnswer
transition RevealingAnswer Continue = Just ShowingPrompt
transition _ _ = Nothing
