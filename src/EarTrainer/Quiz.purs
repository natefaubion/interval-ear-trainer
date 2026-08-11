module EarTrainer.Quiz
  ( IntervalChoice
  , MelodyPrompt
  , Prompt
  , PromptMode(..)
  , PromptSet
  , availableExactIntervals
  , availableIntervalSizes
  , isPlayable
  , melodyPitches
  , makeChoices
  , makePrompt
  , promptSet
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord (abs)
import EarTrainer.Config
  ( AnswerCount(..)
  , ExerciseConfig
  , IntervalSystem(..)
  , QuizMode(..)
  , exerciseRange
  , isValid
  , melodyLength
  )
import EarTrainer.Music
  ( Direction(..)
  , Interval
  , IntervalSize(..)
  , Pitch(..)
  , PitchClass
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

newtype MelodyPrompt = MelodyPrompt (NonEmptyArray.NonEmptyArray Pitch)

derive newtype instance Eq MelodyPrompt

melodyPitches :: MelodyPrompt -> NonEmptyArray.NonEmptyArray Pitch
melodyPitches (MelodyPrompt pitches) = pitches

data PromptMode
  = IntervalPrompt Prompt
  | MelodyPromptMode MelodyPrompt

data PromptSet
  = IntervalPromptSet (NonEmptyArray.NonEmptyArray Prompt)
  | MelodyPromptSet ExerciseConfig (NonEmptyArray.NonEmptyArray Pitch)

promptSet :: ExerciseConfig -> Maybe PromptSet
promptSet config
  | config.quizMode == MelodyImitation = do
      starts <- NonEmptyArray.fromArray
        (Array.filter (canComplete config (melodyLength config.melodyLength - 1)) (startingPitches config))
      pure (MelodyPromptSet config starts)
  | otherwise = map IntervalPromptSet (NonEmptyArray.fromArray (promptCandidates config))

isPlayable :: ExerciseConfig -> Boolean
isPlayable config =
  isValid config && isJust (promptSet config)

directionFor :: PlaybackMode -> Direction
directionFor = case _ of
  MelodicDescending -> Descending
  _ -> Ascending

pick :: forall a. a -> Int -> Array a -> a
pick fallback seed values =
  fromMaybe fallback (Array.index values (abs seed `mod` max 1 (Array.length values)))

makePrompt :: Int -> PromptSet -> PromptMode
makePrompt seed = case _ of
  IntervalPromptSet prompts ->
    IntervalPrompt (pick (NonEmptyArray.head prompts) seed (NonEmptyArray.toArray prompts))
  MelodyPromptSet config starts -> do
    let
      start = pick (NonEmptyArray.head starts) seed (NonEmptyArray.toArray starts)
      pitches = fromMaybe [ start ] (buildMelody config (melodyLength config.melodyLength - 1) seed start)
    case NonEmptyArray.fromArray pitches of
      Just melody -> MelodyPromptMode (MelodyPrompt melody)
      Nothing -> MelodyPromptMode (MelodyPrompt (NonEmptyArray.singleton start))

startingPitches :: ExerciseConfig -> Array Pitch
startingPitches config = pitchesForClasses (exerciseRange config) config.rootPitchClasses

pitchesForClasses :: VocalRange -> Array PitchClass -> Array Pitch
pitchesForClasses range pitchClasses = do
  let
    Pitch _ lowOctave = range.low
    Pitch _ highOctave = range.high
  octave <- Array.range (lowOctave - 1) (highOctave + 1)
  pitchClass <- pitchClasses
  let candidate = Pitch pitchClass octave
  if pitchInRange range candidate then pure candidate else []

nextPitches :: ExerciseConfig -> Pitch -> Array Pitch
nextPitches config current = Array.nub case config.intervalSystem of
  ExactIntervals -> do
    interval <- config.intervals
    direction <- [ Ascending, Descending ]
    let candidate = transpose direction interval current
    if pitchInRange (exerciseRange config) candidate then pure candidate else []
  FromSelectedNotes -> do
    candidate <- pitchesForClasses (exerciseRange config) config.rootPitchClasses
    let
      direction = if midiNumber candidate < midiNumber current then Descending else Ascending
    case intervalBetween direction current candidate of
      Just interval
        | Array.elem (intervalSize interval) config.availableIntervals -> pure candidate
      _ -> []

intervalSize :: Interval -> IntervalSize
intervalSize interval = case intervalNumber interval of
  1 -> SizeUnison
  2 -> SizeSecond
  3 -> SizeThird
  4 -> SizeFourth
  5 -> SizeFifth
  6 -> SizeSixth
  7 -> SizeSeventh
  _ -> SizeOctave

canComplete :: ExerciseConfig -> Int -> Pitch -> Boolean
canComplete config remaining current
  | remaining <= 0 = true
  | otherwise = Array.any (canComplete config (remaining - 1)) (nextPitches config current)

buildMelody :: ExerciseConfig -> Int -> Int -> Pitch -> Maybe (Array Pitch)
buildMelody config remaining seed current
  | remaining <= 0 = Just [ current ]
  | otherwise = tryCandidates (weightedMelodyCandidates seed current (nextPitches config current))
      where
      tryCandidates candidates = case Array.uncons candidates of
        Nothing -> Nothing
        Just { head, tail } -> case buildMelody config (remaining - 1) (nextSeed seed) head of
          Nothing -> tryCandidates tail
          Just suffix -> Just (Array.cons current suffix)

weightedMelodyCandidates :: Int -> Pitch -> Array Pitch -> Array Pitch
weightedMelodyCandidates seed current candidates = do
  let soundingCandidates = Array.nubBy (comparing midiNumber) candidates
  orderGroups seed (melodyCandidateGroups current soundingCandidates)

melodyCandidateGroups
  :: Pitch
  -> Array Pitch
  -> Array { candidates :: Array Pitch, size :: IntervalSize }
melodyCandidateGroups current candidates = Array.mapMaybe groupFor melodyIntervalSizes
  where
  groupFor size = do
    let matching = Array.filter (transitionSize current >>> (_ == Just size)) candidates
    if Array.null matching then Nothing else Just { candidates: matching, size }

orderGroups
  :: Int
  -> Array { candidates :: Array Pitch, size :: IntervalSize }
  -> Array Pitch
orderGroups seed groups = case Array.uncons groups of
  Nothing -> []
  Just { head, tail } -> do
    let
      weighted = Array.concatMap (\group -> Array.replicate (melodyIntervalWeight group.size) group) groups
      selected = pick head seed weighted
      remaining = Array.filter (_.size >>> (_ /= selected.size)) (Array.cons head tail)
      groupCandidates = rotate (nextSeed seed) selected.candidates
    groupCandidates <> orderGroups (nextSeed (nextSeed seed)) remaining

transitionSize :: Pitch -> Pitch -> Maybe IntervalSize
transitionSize current candidate = do
  let direction = if midiNumber candidate < midiNumber current then Descending else Ascending
  map intervalSize (intervalBetween direction current candidate)

melodyIntervalSizes :: Array IntervalSize
melodyIntervalSizes =
  [ SizeUnison
  , SizeSecond
  , SizeThird
  , SizeFourth
  , SizeFifth
  , SizeSixth
  , SizeSeventh
  , SizeOctave
  ]

melodyIntervalWeight :: IntervalSize -> Int
melodyIntervalWeight = case _ of
  SizeUnison -> 2
  SizeSecond -> 8
  SizeThird -> 6
  SizeFourth -> 2
  SizeFifth -> 2
  SizeSixth -> 1
  SizeSeventh -> 1
  SizeOctave -> 1

rotate :: forall a. Int -> Array a -> Array a
rotate seed values = do
  let offset = abs seed `mod` max 1 (Array.length values)
  Array.drop offset values <> Array.take offset values

nextSeed :: Int -> Int
nextSeed seed = seed * 1664525 + 1013904223

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
availableExactIntervals config
  | config.quizMode == MelodyImitation = Array.filter
      ( \interval -> Array.any
          (not <<< Array.null <<< nextPitches (config { intervals = [ interval ], intervalSystem = ExactIntervals }))
          (startingPitches config)
      )
      allIntervals
  | otherwise = Array.filter (not <<< Array.null <<< exactCandidates config) allIntervals

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
    ( \size ->
        if config.quizMode == MelodyImitation then
          Array.any
            (not <<< Array.null <<< nextPitches (config { availableIntervals = [ size ], intervalSystem = FromSelectedNotes }))
            (startingPitches config)
        else not (Array.null (derivedCandidates config [ size ]))
    )
    [ SizeUnison, SizeSecond, SizeThird, SizeFourth, SizeFifth, SizeSixth, SizeSeventh, SizeOctave ]

intervalSizeNumber :: IntervalSize -> Int
intervalSizeNumber = case _ of
  SizeUnison -> 1
  SizeSecond -> 2
  SizeThird -> 3
  SizeFourth -> 4
  SizeFifth -> 5
  SizeSixth -> 6
  SizeSeventh -> 7
  SizeOctave -> 8

pitchInRange :: VocalRange -> Pitch -> Boolean
pitchInRange range pitch =
  midiNumber pitch >= midiNumber range.low && midiNumber pitch <= midiNumber range.high

makeChoices :: Int -> ExerciseConfig -> Prompt -> Array IntervalChoice
makeChoices seed config prompt = do
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
  map
    (\interval -> { interval, target: transpose direction interval prompt.root })
    withCorrect
