module EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
  , exerciseRange
  , isValid
  , quizModeUsesRecognition
  , quizModeUsesSinging
  , toggleInterval
  , togglePlaybackMode
  , toggleRootPitchClass
  ) where

import Prelude

import Data.Array as Array
import EarTrainer.Music
  ( Interval
  , OctavePolicy(..)
  , PitchClass
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , VocalRange
  , defaultIntervals
  , defaultRootPitchClasses
  , midiNumber
  , pitch
  , Letter(..)
  , Accidental(..)
  , presetRange
  )

data GhostMode = GhostOff | GhostOn | GhostPersist

derive instance Eq GhostMode

data AnswerDisplay = AnswerNotation | AnswerName | AnswerBoth

derive instance Eq AnswerDisplay

data AnswerCount = AFew | AllSelected

derive instance Eq AnswerCount

data QuizMode = SingingOnly | RecognitionOnly | SingingAndRecognition | Audiation

derive instance Eq QuizMode

data QuizProgression = ManualProgression | AutomaticProgression

derive instance Eq QuizProgression

quizModeUsesSinging :: QuizMode -> Boolean
quizModeUsesSinging RecognitionOnly = false
quizModeUsesSinging _ = true

quizModeUsesRecognition :: QuizMode -> Boolean
quizModeUsesRecognition SingingOnly = false
quizModeUsesRecognition Audiation = false
quizModeUsesRecognition _ = true

type ExerciseConfig =
  { answerCount :: AnswerCount
  , answerDisplay :: AnswerDisplay
  , customRange :: VocalRange
  , ghostMode :: GhostMode
  , intervals :: Array Interval
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
  , customRange: { low: pitch C (Accidental 0) 3, high: pitch G (Accidental 0) 5 }
  , ghostMode: GhostOn
  , intervals: defaultIntervals
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

togglePlaybackMode :: PlaybackMode -> ExerciseConfig -> ExerciseConfig
togglePlaybackMode mode config = config { playbackModes = toggleMember mode config.playbackModes }

toggleRootPitchClass :: PitchClass -> ExerciseConfig -> ExerciseConfig
toggleRootPitchClass pitchClass config = config { rootPitchClasses = toggleMember pitchClass config.rootPitchClasses }

isValid :: ExerciseConfig -> Boolean
isValid config =
  not (Array.null config.intervals)
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
