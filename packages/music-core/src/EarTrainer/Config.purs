module EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
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
  , PlaybackMode
  , VocalRangePreset(..)
  , allPlaybackModes
  , defaultIntervals
  , defaultRootPitchClasses
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
  , ghostMode :: GhostMode
  , intervals :: Array Interval
  , octavePolicy :: OctavePolicy
  , playbackModes :: Array PlaybackMode
  , quizMode :: QuizMode
  , quizProgression :: QuizProgression
  , rootPitchClasses :: Array PitchClass
  , vocalRange :: VocalRangePreset
  }

defaultConfig :: ExerciseConfig
defaultConfig =
  { answerCount: AFew
  , answerDisplay: AnswerNotation
  , ghostMode: GhostOn
  , intervals: defaultIntervals
  , octavePolicy: AnyOctave
  , playbackModes: allPlaybackModes
  , quizMode: SingingAndRecognition
  , quizProgression: ManualProgression
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
    && (config.quizMode == Audiation || not (Array.null config.playbackModes))
    && not (Array.null config.rootPitchClasses)
