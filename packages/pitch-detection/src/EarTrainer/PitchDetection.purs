module EarTrainer.PitchDetection
  ( Feedback
  , Monitor
  , PitchSample
  , Recognition
  , RecognitionPhase(..)
  , RecognitionSettings
  , defaultRecognitionSettings
  , initialRecognition
  , midiFrequency
  , nearestMidi
  , phaseInstruction
  , start
  , stepRecognition
  , stop
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Math
import Effect (Effect)
import EarTrainer.Music (OctavePolicy(..), Pitch, midiNumber)

type PitchSample =
  { clarity :: Number
  , frequency :: Number
  }

type Feedback =
  { cents :: Number
  , clarity :: Number
  , midi :: Int
  }

data RecognitionPhase
  = WaitingForFirst
  | WaitingForRelease
  | WaitingForSecond
  | RecognitionComplete

derive instance Eq RecognitionPhase

instance Show RecognitionPhase where
  show WaitingForFirst = "WaitingForFirst"
  show WaitingForRelease = "WaitingForRelease"
  show WaitingForSecond = "WaitingForSecond"
  show RecognitionComplete = "RecognitionComplete"

type Recognition =
  { candidate :: Maybe Int
  , feedback :: Maybe Feedback
  , phase :: RecognitionPhase
  , releaseFrames :: Int
  , stableFrames :: Int
  }

type RecognitionSettings =
  { clarityThreshold :: Number
  , releaseFramesRequired :: Int
  , stableFramesRequired :: Int
  , toleranceCents :: Number
  }

defaultRecognitionSettings :: RecognitionSettings
defaultRecognitionSettings =
  { clarityThreshold: 0.9
  , releaseFramesRequired: 5
  , stableFramesRequired: 12
  , toleranceCents: 35.0
  }

initialRecognition :: Recognition
initialRecognition =
  { candidate: Nothing
  , feedback: Nothing
  , phase: WaitingForFirst
  , releaseFrames: 0
  , stableFrames: 0
  }

foreign import data Monitor :: Type

foreign import start :: (PitchSample -> Effect Unit) -> (String -> Effect Unit) -> Effect Monitor
foreign import stop :: Monitor -> Effect Unit

midiFrequency :: Int -> Number
midiFrequency midi = 440.0 * Math.pow 2.0 (Int.toNumber (midi - 69) / 12.0)

nearestMidi :: Number -> Int
nearestMidi frequency = Int.round (69.0 + 12.0 * Math.log (frequency / 440.0) / Math.log 2.0)

feedbackFor :: PitchSample -> Maybe Feedback
feedbackFor sample
  | sample.frequency <= 0.0 = Nothing
  | otherwise =
      let
        midi = nearestMidi sample.frequency
        cents = 1200.0 * Math.log (sample.frequency / midiFrequency midi) / Math.log 2.0
      in
        Just { cents, clarity: sample.clarity, midi }

matchesExpected :: RecognitionSettings -> OctavePolicy -> Int -> Feedback -> Boolean
matchesExpected settings octavePolicy expected feedback =
  let
    octaveMatches = case octavePolicy of
      AnyOctave -> feedback.midi `mod` 12 == expected `mod` 12
      WrittenOctave -> feedback.midi == expected
  in
    octaveMatches && absolute feedback.cents <= settings.toleranceCents

absolute :: Number -> Number
absolute value = if value < 0.0 then -value else value

stepStable :: Int -> Feedback -> Recognition -> Recognition
stepStable required feedback recognition =
  let
    frames = if recognition.candidate == Just feedback.midi then recognition.stableFrames + 1 else 1
  in
    recognition
      { candidate = Just feedback.midi
      , feedback = Just feedback
      , stableFrames = min required frames
      }

resetStable :: Maybe Feedback -> Recognition -> Recognition
resetStable feedback recognition = recognition { candidate = Nothing, feedback = feedback, stableFrames = 0 }

stepRecognition
  :: RecognitionSettings
  -> OctavePolicy
  -> Pitch
  -> Pitch
  -> PitchSample
  -> Recognition
  -> Recognition
stepRecognition settings octavePolicy firstPitch secondPitch sample recognition =
  let
    feedback = feedbackFor sample
    clear = sample.clarity >= settings.clarityThreshold
    matches pitch = case feedback of
      Nothing -> false
      Just current -> clear && matchesExpected settings octavePolicy (midiNumber pitch) current
  in
    case recognition.phase of
      RecognitionComplete -> recognition { feedback = feedback }
      WaitingForFirst
        | matches firstPitch ->
            let
              next = stepStable settings.stableFramesRequired (unwrapFeedback feedback) recognition
            in
              if next.stableFrames >= settings.stableFramesRequired then
                next { phase = WaitingForRelease, releaseFrames = 0 }
              else next
        | otherwise -> resetStable feedback recognition
      WaitingForRelease
        | matches firstPitch -> recognition { feedback = feedback, releaseFrames = 0 }
        | otherwise ->
            let
              frames = recognition.releaseFrames + 1
            in
              if frames >= settings.releaseFramesRequired then
                (resetStable feedback recognition) { phase = WaitingForSecond, releaseFrames = frames }
              else recognition { feedback = feedback, releaseFrames = frames }
      WaitingForSecond
        | matches secondPitch ->
            let
              next = stepStable settings.stableFramesRequired (unwrapFeedback feedback) recognition
            in
              if next.stableFrames >= settings.stableFramesRequired then
                next { phase = RecognitionComplete }
              else next
        | otherwise -> resetStable feedback recognition

unwrapFeedback :: Maybe Feedback -> Feedback
unwrapFeedback (Just feedback) = feedback
unwrapFeedback Nothing = { cents: 0.0, clarity: 0.0, midi: 0 }

phaseInstruction :: RecognitionPhase -> String
phaseInstruction WaitingForFirst = "Sing the first note"
phaseInstruction WaitingForRelease = "Release or move away from the first note"
phaseInstruction WaitingForSecond = "Sing the second note"
phaseInstruction RecognitionComplete = "Both notes accepted"
