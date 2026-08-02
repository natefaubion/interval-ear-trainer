module EarTrainer.Quiz
  ( Event(..)
  , IntervalChoice
  , Phase(..)
  , Prompt
  , availableIntervalSizes
  , makeChoices
  , makePrompt
  , transition
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import EarTrainer.Config (AnswerCount(..), ExerciseConfig, IntervalSystem(..), QuizMode(..), exerciseRange)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , IntervalSize(..)
  , Letter(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRange
  , midiNumber
  , intervalBetween
  , intervalNumber
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
makePrompt seed config = case config.intervalSystem of
  ExactIntervals -> makeExactPrompt seed config
  FromSelectedNotes -> makeDerivedPrompt seed config

makeExactPrompt :: Int -> ExerciseConfig -> Prompt
makeExactPrompt seed config =
  let
    interval = pick MinorThird seed config.intervals
    modes = availableModes config
    mode = pick MelodicAscending (seed `div` 7) modes
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
      if pitchInRange range candidateRoot && pitchInRange range target then pure candidateRoot else []
    fallbackRoot = Pitch (PitchClass C (Accidental 0)) lowOctave
    root = pick fallbackRoot (seed `div` 13) roots
  in
    { interval, mode, root, target: transpose direction interval root }

makeDerivedPrompt :: Int -> ExerciseConfig -> Prompt
makeDerivedPrompt seed config =
  let
    range = exerciseRange config
    Pitch _ lowOctave = range.low
    fallbackRoot = Pitch (PitchClass C (Accidental 0)) lowOctave
    fallback =
      { interval: MinorThird
      , mode: MelodicAscending
      , root: fallbackRoot
      , target: transpose Ascending MinorThird fallbackRoot
      }
    validSizes = Array.filter (flip Array.elem (availableIntervalSizes config)) config.availableIntervals
    size = pick SizeThird seed validSizes
    candidatesForSize = derivedCandidates config [ size ]
    validModes = Array.filter (\candidateMode -> Array.any (\prompt -> prompt.mode == candidateMode) candidatesForSize) (availableModes config)
    mode = pick MelodicAscending (seed `div` 7) validModes
    candidates = Array.filter (\prompt -> prompt.mode == mode) candidatesForSize
  in
    pick fallback (seed `div` 13) candidates

availableModes :: ExerciseConfig -> Array PlaybackMode
availableModes config =
  if config.quizMode == Audiation then Array.filter (_ /= Harmonic) config.playbackModes
  else config.playbackModes

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
      ExactIntervals -> config.intervals
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
