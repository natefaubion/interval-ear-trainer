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
import EarTrainer.Config (AnswerCount(..), ExerciseConfig, QuizMode(..), exerciseRange)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , midiNumber
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
    availableModes =
      if config.quizMode == Audiation then Array.filter (_ /= Harmonic) config.playbackModes
      else config.playbackModes
    mode = pick MelodicAscending (seed `div` 7) availableModes
    direction = directionFor mode
    range = exerciseRange config
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

makeChoices :: Int -> ExerciseConfig -> Prompt -> Array IntervalChoice
makeChoices seed config prompt =
  let
    distractorPool = Array.filter (_ /= prompt.interval) config.intervals
    poolLength = Array.length distractorPool
    offset = abs (seed `div` 17) `mod` max 1 poolLength
    rotated = Array.drop offset distractorPool <> Array.take offset distractorPool
    intervals = case config.answerCount of
      AFew -> Array.take 3 rotated
      AllSelected -> rotated
    correctPosition = abs (seed `div` 31) `mod` (Array.length intervals + 1)
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
  | IntervalError
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
  show IntervalError = "IntervalError"
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
  | PitchRejected
  | ChoiceSubmitted Boolean
  | Continue

transition :: Phase -> Event -> Maybe Phase
transition Configuring BeginQuiz = Just ShowingPrompt
transition ShowingPrompt PromptReady = Just PlayingInterval
transition PlayingInterval PlaybackFinished = Just WaitingForSilence
transition WaitingForSilence RoomIsQuiet = Just SingingFirstNote
transition SingingFirstNote FirstPitchAccepted = Just AwaitingRearticulation
transition SingingFirstNote PitchRejected = Just IntervalError
transition AwaitingRearticulation VoiceReleased = Just SingingSecondNote
transition SingingSecondNote SecondPitchAccepted = Just ChoosingNotation
transition SingingSecondNote PitchRejected = Just IntervalError
transition IntervalError Continue = Just PlayingInterval
transition ChoosingNotation (ChoiceSubmitted _) = Just RevealingAnswer
transition RevealingAnswer Continue = Just ShowingPrompt
transition _ _ = Nothing
