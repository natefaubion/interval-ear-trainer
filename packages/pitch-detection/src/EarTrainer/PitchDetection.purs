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
  , relativeMidi
  , start
  , stepRecognition
  , stop
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Math
import EarTrainer.Music (OctavePolicy(..), Pitch, midiNumber)
import Effect (Effect)

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
  | RecognitionIncorrect
  | RecognitionComplete

derive instance Eq RecognitionPhase

instance Show RecognitionPhase where
  show WaitingForFirst = "WaitingForFirst"
  show WaitingForRelease = "WaitingForRelease"
  show WaitingForSecond = "WaitingForSecond"
  show RecognitionIncorrect = "RecognitionIncorrect"
  show RecognitionComplete = "RecognitionComplete"

type Recognition =
  { candidate :: Maybe Int
  , feedback :: Maybe Feedback
  , firstMidi :: Maybe Int
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
  , stableFramesRequired: 36
  , toleranceCents: 35.0
  }

initialRecognition :: Recognition
initialRecognition =
  { candidate: Nothing
  , feedback: Nothing
  , firstMidi: Nothing
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

feedbackFor :: Boolean -> Int -> PitchSample -> Maybe Feedback
feedbackFor allowOctaveEquivalent expectedMidi sample
  | sample.frequency <= 0.0 = Nothing
  | otherwise =
      let
        detectedMidi = 69.0 + 12.0 * Math.log (sample.frequency / 440.0) / Math.log 2.0
        comparisonMidi =
          if allowOctaveEquivalent then
            expectedMidi + 12 * Int.round ((detectedMidi - Int.toNumber expectedMidi) / 12.0)
          else
            expectedMidi
        cents = 100.0 * (detectedMidi - Int.toNumber comparisonMidi)
        midi = Int.round detectedMidi
      in
        Just { cents, clarity: sample.clarity, midi }

matchesExpected :: RecognitionSettings -> Boolean -> Int -> Feedback -> Boolean
matchesExpected settings allowOctaveEquivalent expected feedback =
  matchesPitchIdentity allowOctaveEquivalent expected feedback
    && absolute feedback.cents <= settings.toleranceCents

matchesPitchIdentity :: Boolean -> Int -> Feedback -> Boolean
matchesPitchIdentity allowOctaveEquivalent expected feedback =
  if allowOctaveEquivalent then feedback.midi `mod` 12 == expected `mod` 12
  else feedback.midi == expected

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
    writtenFirst = midiNumber firstPitch
    writtenSecond = midiNumber secondPitch
    normalizedSecond = case octavePolicy, recognition.firstMidi of
      AnyOctave, Just firstMidi -> firstMidi + writtenSecond - writtenFirst
      _, _ -> writtenSecond
    expectation = case recognition.phase of
      WaitingForFirst -> { allowOctaveEquivalent: octavePolicy == AnyOctave, midi: writtenFirst }
      WaitingForRelease ->
        { allowOctaveEquivalent: false
        , midi: case recognition.firstMidi of
            Just firstMidi -> firstMidi
            Nothing -> writtenFirst
        }
      WaitingForSecond -> { allowOctaveEquivalent: false, midi: normalizedSecond }
      RecognitionIncorrect -> { allowOctaveEquivalent: false, midi: normalizedSecond }
      RecognitionComplete -> { allowOctaveEquivalent: false, midi: normalizedSecond }
    feedback = feedbackFor expectation.allowOctaveEquivalent expectation.midi sample
    clear = sample.clarity >= settings.clarityThreshold
    matches = case feedback of
      Nothing -> false
      Just current ->
        clear && matchesExpected settings expectation.allowOctaveEquivalent expectation.midi current
  in
    case recognition.phase of
      RecognitionComplete -> recognition { feedback = feedback }
      RecognitionIncorrect -> recognition { feedback = feedback }
      WaitingForFirst
        | clear -> case feedback of
            Just current ->
              let
                next = stepStable settings.stableFramesRequired current recognition
              in
                if next.stableFrames < settings.stableFramesRequired then next
                else if matches then
                  next { firstMidi = Just current.midi, phase = WaitingForRelease, releaseFrames = 0 }
                else if matchesPitchIdentity expectation.allowOctaveEquivalent expectation.midi current then next
                else next { phase = RecognitionIncorrect }
            Nothing -> resetStable feedback recognition
        | otherwise -> resetStable feedback recognition
      WaitingForRelease
        | matches -> recognition { feedback = feedback, releaseFrames = 0 }
        | otherwise ->
            let
              frames = recognition.releaseFrames + 1
            in
              if frames >= settings.releaseFramesRequired then
                (resetStable feedback recognition) { phase = WaitingForSecond, releaseFrames = frames }
              else recognition { feedback = feedback, releaseFrames = frames }
      WaitingForSecond
        | clear -> case feedback of
            Just current ->
              let
                next = stepStable settings.stableFramesRequired current recognition
              in
                if next.stableFrames < settings.stableFramesRequired then next
                else if matches then next { phase = RecognitionComplete }
                else if matchesPitchIdentity expectation.allowOctaveEquivalent expectation.midi current then next
                else next { phase = RecognitionIncorrect }
            Nothing -> resetStable feedback recognition
        | otherwise -> resetStable feedback recognition

phaseInstruction :: RecognitionPhase -> String
phaseInstruction WaitingForFirst = "Sing the first note."
phaseInstruction WaitingForRelease = "Sing the second note."
phaseInstruction WaitingForSecond = "Sing the second note."
phaseInstruction RecognitionIncorrect = "Incorrect pitch."
phaseInstruction RecognitionComplete = "Both notes accepted."

relativeMidi :: OctavePolicy -> Pitch -> Recognition -> Int -> Int
relativeMidi WrittenOctave _ _ detectedMidi = detectedMidi
relativeMidi AnyOctave root recognition detectedMidi = case recognition.firstMidi of
  Just firstMidi -> detectedMidi + midiNumber root - firstMidi
  Nothing ->
    if detectedMidi `mod` 12 == midiNumber root `mod` 12 then midiNumber root
    else detectedMidi
