module EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , RangeBoundary(..)
  , defaultConfig
  , exerciseRange
  , isValid
  , quizModeUsesRecognition
  , quizModeUsesSinging
  , selectMajorKey
  , selectedMajorKeyId
  , setCustomPitchClass
  , setCustomPitchOctave
  , toggleInterval
  , toggleIntervalSize
  , togglePlaybackMode
  , toggleRootPitchClass
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import EarTrainer.Music
  ( Accidental(..)
  , Interval
  , IntervalSize(..)
  , Letter(..)
  , OctavePolicy(..)
  , Pitch
  , PitchClass
  , PlaybackMode(..)
  , VocalRange
  , VocalRangePreset(..)
  , allMajorKeyPresets
  , defaultIntervals
  , defaultRootPitchClasses
  , midiNumber
  , pitch
  , pitchFromMidi
  , presetRange
  )

data GhostMode = GhostOff | GhostOn | GhostPersist

derive instance Eq GhostMode

data IntervalSystem = ExactIntervals | FromSelectedNotes

derive instance Eq IntervalSystem

data AnswerDisplay = AnswerNotation | AnswerName | AnswerBoth

derive instance Eq AnswerDisplay

data AnswerCount = AFew | AllSelected

derive instance Eq AnswerCount

data QuizMode = SingingOnly | RecognitionOnly | SingingAndRecognition | Audiation

derive instance Eq QuizMode

data QuizProgression = ManualProgression | AutomaticProgression

derive instance Eq QuizProgression

data RangeBoundary = Lowest | Highest

derive instance Eq RangeBoundary

quizModeUsesSinging :: QuizMode -> Boolean
quizModeUsesSinging = case _ of
  RecognitionOnly -> false
  _ -> true

quizModeUsesRecognition :: QuizMode -> Boolean
quizModeUsesRecognition = case _ of
  SingingOnly -> false
  Audiation -> false
  _ -> true

type ExerciseConfig =
  { answerCount :: AnswerCount
  , answerDisplay :: AnswerDisplay
  , availableIntervals :: Array IntervalSize
  , customRange :: VocalRange
  , ghostMode :: GhostMode
  , intervals :: Array Interval
  , intervalSystem :: IntervalSystem
  , octavePolicy :: OctavePolicy
  , playbackModes :: Array PlaybackMode
  , showPitchTuner :: Boolean
  , quizMode :: QuizMode
  , quizProgression :: QuizProgression
  , rootPitchClasses :: Array PitchClass
  , vocalRange :: VocalRangePreset
  }

defaultConfig :: ExerciseConfig
defaultConfig =
  { answerCount: AFew
  , answerDisplay: AnswerNotation
  , availableIntervals: [ SizeThird, SizeFourth, SizeFifth, SizeOctave ]
  , customRange: { low: pitch C (Accidental 0) 3, high: pitch G (Accidental 0) 5 }
  , ghostMode: GhostOn
  , intervals: defaultIntervals
  , intervalSystem: FromSelectedNotes
  , octavePolicy: AnyOctave
  , playbackModes: [ MelodicAscending ]
  , showPitchTuner: true
  , quizMode: SingingAndRecognition
  , quizProgression: AutomaticProgression
  , rootPitchClasses: defaultRootPitchClasses
  , vocalRange: Tenor
  }

toggleMember :: forall a. Eq a => a -> Array a -> Array a
toggleMember value values
  | Array.elem value values = Array.delete value values
  | otherwise = Array.snoc values value

toggleInterval :: Interval -> ExerciseConfig -> ExerciseConfig
toggleInterval interval config = config { intervals = toggleMember interval config.intervals }

toggleIntervalSize :: IntervalSize -> ExerciseConfig -> ExerciseConfig
toggleIntervalSize interval config = config { availableIntervals = toggleMember interval config.availableIntervals }

togglePlaybackMode :: PlaybackMode -> ExerciseConfig -> ExerciseConfig
togglePlaybackMode mode config = config { playbackModes = toggleMember mode config.playbackModes }

toggleRootPitchClass :: PitchClass -> ExerciseConfig -> ExerciseConfig
toggleRootPitchClass pitchClass config = config { rootPitchClasses = toggleMember pitchClass config.rootPitchClasses }

isValid :: ExerciseConfig -> Boolean
isValid config =
  ( if config.intervalSystem == ExactIntervals then not (Array.null config.intervals)
    else not (Array.null config.availableIntervals)
  )
    && not
      ( Array.null
          ( if config.quizMode == Audiation then
              Array.filter (_ /= Harmonic) config.playbackModes
            else config.playbackModes
          )
      )
    && not (Array.null config.rootPitchClasses)
    && midiNumber (exerciseRange config).low <= midiNumber (exerciseRange config).high

exerciseRange :: ExerciseConfig -> VocalRange
exerciseRange config =
  if config.vocalRange == Custom then config.customRange
  else presetRange config.vocalRange

selectedMajorKeyId :: Array PitchClass -> String
selectedMajorKeyId roots = case Array.find (\preset -> samePitchClasses preset.roots roots) allMajorKeyPresets of
  Just preset -> preset.id
  Nothing -> "custom"

selectMajorKey :: String -> ExerciseConfig -> ExerciseConfig
selectMajorKey presetId config = case Array.find (\preset -> preset.id == presetId) allMajorKeyPresets of
  Just preset -> config { rootPitchClasses = preset.roots }
  Nothing -> config

setCustomPitchClass :: RangeBoundary -> Int -> ExerciseConfig -> ExerciseConfig
setCustomPitchClass boundary pitchClass config = do
  let
    current = customBoundary boundary config
    octave = midiNumber current `div` 12 - 1
  setCustomBoundary boundary (pitchFromMidi (12 * (octave + 1) + pitchClass)) config

setCustomPitchOctave :: RangeBoundary -> Int -> ExerciseConfig -> ExerciseConfig
setCustomPitchOctave boundary octave config = do
  let
    current = customBoundary boundary config
    pitchClass = midiNumber current `mod` 12
  setCustomBoundary boundary (pitchFromMidi (12 * (octave + 1) + pitchClass)) config

customBoundary :: RangeBoundary -> ExerciseConfig -> Pitch
customBoundary boundary config = case boundary of
  Lowest -> config.customRange.low
  Highest -> config.customRange.high

setCustomBoundary :: RangeBoundary -> Pitch -> ExerciseConfig -> ExerciseConfig
setCustomBoundary boundary value config = case boundary of
  Lowest -> config { customRange = config.customRange { low = value } }
  Highest -> config { customRange = config.customRange { high = value } }

samePitchClasses :: Array PitchClass -> Array PitchClass -> Boolean
samePitchClasses left right =
  Array.length left == Array.length right && Array.all (flip Array.elem right) left
