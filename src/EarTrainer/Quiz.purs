module EarTrainer.Quiz
  ( Event(..)
  , IntervalChoice
  , Phase(..)
  , Prompt
  , PromptSet
  , availableExactIntervals
  , availableIntervalSizes
  , isPlayable
  , makeChoices
  , makePrompt
  , promptSet
  , transition
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord (abs)
import EarTrainer.Config (AnswerCount(..), ExerciseConfig, IntervalSystem(..), QuizMode(..), exerciseRange, isValid)
import EarTrainer.Music
  ( Direction(..)
  , Interval
  , IntervalSize(..)
  , Pitch(..)
  , PlaybackMode(..)
  , VocalRange
  , allIntervals
  , intervalBetween
  , intervalNumber
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

newtype PromptSet = PromptSet (NonEmptyArray.NonEmptyArray Prompt)

promptSet :: ExerciseConfig -> Maybe PromptSet
promptSet = map PromptSet <<< NonEmptyArray.fromArray <<< promptCandidates

isPlayable :: ExerciseConfig -> Boolean
isPlayable config =
  isValid config && isJust (promptSet config)

directionFor :: PlaybackMode -> Direction
directionFor MelodicDescending = Descending
directionFor _ = Ascending

pick :: forall a. a -> Int -> Array a -> a
pick fallback seed values =
  fromMaybe fallback (Array.index values (abs seed `mod` max 1 (Array.length values)))

makePrompt :: Int -> PromptSet -> Prompt
makePrompt seed (PromptSet prompts) =
  pick (NonEmptyArray.head prompts) seed (NonEmptyArray.toArray prompts)

promptCandidates :: ExerciseConfig -> Array Prompt
promptCandidates config = case config.intervalSystem of
  ExactIntervals -> do
    interval <- Array.filter (flip Array.elem (availableExactIntervals config)) config.intervals
    exactCandidates config interval
  FromSelectedNotes ->
    derivedCandidates config
      (Array.filter (flip Array.elem (availableIntervalSizes config)) config.availableIntervals)

availableModes :: ExerciseConfig -> Array PlaybackMode
availableModes config =
  if config.quizMode == Audiation then Array.filter (_ /= Harmonic) config.playbackModes
  else config.playbackModes

exactCandidates :: ExerciseConfig -> Interval -> Array Prompt
exactCandidates config interval = do
  mode <- availableModes config
  let
    direction = directionFor mode
    range = exerciseRange config
    Pitch _ lowOctave = range.low
    Pitch _ highOctave = range.high
  octave <- Array.range (lowOctave - 1) (highOctave + 1)
  pitchClass <- config.rootPitchClasses
  let
    root = Pitch pitchClass octave
    target = transpose direction interval root
  if pitchInRange range root && pitchInRange range target then
    pure { interval, mode, root, target }
  else []

availableExactIntervals :: ExerciseConfig -> Array Interval
availableExactIntervals config =
  Array.filter (not <<< Array.null <<< exactCandidates config) allIntervals

derivedCandidates :: ExerciseConfig -> Array IntervalSize -> Array Prompt
derivedCandidates config sizes = do
  mode <- availableModes config
  size <- sizes
  let
    direction = directionFor mode
    range = exerciseRange config
    Pitch _ lowOctave = range.low
    Pitch _ highOctave = range.high
  rootOctave <- Array.range (lowOctave - 1) (highOctave + 1)
  rootClass <- config.rootPitchClasses
  targetOctave <- [ rootOctave - 1, rootOctave, rootOctave + 1 ]
  targetClass <- config.rootPitchClasses
  let
    root = Pitch rootClass rootOctave
    target = Pitch targetClass targetOctave
  case intervalBetween direction root target of
    Just interval
      | intervalNumber interval == intervalSizeNumber size
          && pitchInRange range root
          && pitchInRange range target ->
          pure { interval, mode, root, target }
    _ -> []

availableIntervalSizes :: ExerciseConfig -> Array IntervalSize
availableIntervalSizes config =
  Array.filter
    (\size -> not (Array.null (derivedCandidates config [ size ])))
    [ SizeUnison, SizeSecond, SizeThird, SizeFourth, SizeFifth, SizeSixth, SizeSeventh, SizeOctave ]

intervalSizeNumber :: IntervalSize -> Int
intervalSizeNumber SizeUnison = 1
intervalSizeNumber SizeSecond = 2
intervalSizeNumber SizeThird = 3
intervalSizeNumber SizeFourth = 4
intervalSizeNumber SizeFifth = 5
intervalSizeNumber SizeSixth = 6
intervalSizeNumber SizeSeventh = 7
intervalSizeNumber SizeOctave = 8

pitchInRange :: VocalRange -> Pitch -> Boolean
pitchInRange range pitch =
  midiNumber pitch >= midiNumber range.low && midiNumber pitch <= midiNumber range.high

makeChoices :: Int -> ExerciseConfig -> Prompt -> Array IntervalChoice
makeChoices seed config prompt =
  let
    direction = directionFor prompt.mode
    soundMidi interval = midiNumber (transpose direction interval prompt.root)
    sameSound left right =
      soundMidi left == soundMidi right
    compareSound left right = compare (soundMidi left) (soundMidi right)
    configuredIntervals = case config.intervalSystem of
      ExactIntervals -> Array.filter (flip Array.elem (availableExactIntervals config)) config.intervals
      FromSelectedNotes ->
        Array.nub (map _.interval (derivedCandidates config config.availableIntervals))
    distractorPool =
      Array.nubBy compareSound
        (Array.filter (not <<< sameSound prompt.interval) configuredIntervals)
    poolLength = Array.length distractorPool
    offset = abs (seed `div` 17) `mod` max 1 poolLength
    rotated = Array.drop offset distractorPool <> Array.take offset distractorPool
    intervals = case config.answerCount of
      AFew -> Array.take 3 rotated
      AllSelected -> rotated
    correctPosition = abs (seed `div` 31) `mod` (Array.length intervals + 1)
    withCorrect = fromMaybe intervals (Array.insertAt correctPosition prompt.interval intervals)
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
