module EarTrainer.Recognition
  ( CaptureSettings
  , Feedback
  , Observation
  , PitchSample
  , Recognition
  , RecognitionPhase(..)
  , RecognitionSettings
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , feedback
  , initialObservation
  , initialRecognition
  , midiFrequency
  , nearestMidi
  , observePitch
  , phase
  , phaseInstruction
  , relativeMidi
  , stepRecognition
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Math
import EarTrainer.Capability.PitchInput as PitchInput
import EarTrainer.Music (OctavePolicy(..), Pitch, midiNumber)

type PitchSample =
  { clarity :: Number
  , frequency :: Number
  }

type TimedPitchSample =
  { clarity :: Number
  , frequency :: Number
  , time :: Number
  }

newtype Observation = Observation
  { lastValidAt :: Number
  , samples :: Array TimedPitchSample
  }

type CaptureSettings =
  { clarityThreshold :: Number
  , maximumFrequency :: Number
  , minimumFrequency :: Number
  , minimumSamples :: Int
  , sampleWindowMilliseconds :: Number
  , silenceMilliseconds :: Number
  , volumeThresholdDb :: Number
  }

defaultCaptureSettings :: CaptureSettings
defaultCaptureSettings =
  { clarityThreshold: 0.9
  , maximumFrequency: 1200.0
  , minimumFrequency: 70.0
  , minimumSamples: 4
  , sampleWindowMilliseconds: 300.0
  , silenceMilliseconds: 180.0
  , volumeThresholdDb: -50.0
  }

initialObservation :: Observation
initialObservation = Observation { lastValidAt: 0.0, samples: [] }

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
  show = case _ of
    WaitingForFirst -> "WaitingForFirst"
    WaitingForRelease -> "WaitingForRelease"
    WaitingForSecond -> "WaitingForSecond"
    RecognitionIncorrect -> "RecognitionIncorrect"
    RecognitionComplete -> "RecognitionComplete"

type StablePitch =
  { candidate :: Maybe Int
  , feedback :: Maybe Feedback
  , stableFrames :: Int
  }

data Recognition
  = SingingFirst StablePitch
  | ReleasingFirst
      { feedback :: Maybe Feedback
      , firstMidi :: Int
      , releaseFrames :: Int
      }
  | SingingSecond
      { firstMidi :: Int
      , stable :: StablePitch
      }
  | IncorrectFirst (Maybe Feedback)
  | IncorrectSecond
      { feedback :: Maybe Feedback
      , firstMidi :: Int
      }
  | Complete
      { feedback :: Maybe Feedback
      , firstMidi :: Int
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
initialRecognition = SingingFirst initialStablePitch

initialStablePitch :: StablePitch
initialStablePitch = { candidate: Nothing, feedback: Nothing, stableFrames: 0 }

phase :: Recognition -> RecognitionPhase
phase = case _ of
  SingingFirst _ -> WaitingForFirst
  ReleasingFirst _ -> WaitingForRelease
  SingingSecond _ -> WaitingForSecond
  IncorrectFirst _ -> RecognitionIncorrect
  IncorrectSecond _ -> RecognitionIncorrect
  Complete _ -> RecognitionComplete

feedback :: Recognition -> Maybe Feedback
feedback = case _ of
  SingingFirst stable -> stable.feedback
  ReleasingFirst state -> state.feedback
  SingingSecond state -> state.stable.feedback
  IncorrectFirst current -> current
  IncorrectSecond state -> state.feedback
  Complete state -> state.feedback

observePitch
  :: CaptureSettings
  -> PitchInput.Sample
  -> Observation
  -> { observation :: Observation, sample :: Maybe PitchSample }
observePitch settings raw (Observation observation) = do
  let
    valid =
      raw.clarity >= settings.clarityThreshold
        && raw.decibels >= settings.volumeThresholdDb
        && raw.frequency >= settings.minimumFrequency
        && raw.frequency <= settings.maximumFrequency
    samples = Array.filter
      (\timed -> raw.time - timed.time <= settings.sampleWindowMilliseconds)
      if valid then
        Array.snoc observation.samples
          { clarity: raw.clarity
          , frequency: raw.frequency
          , time: raw.time
          }
      else
        observation.samples
    lastValidAt = if valid then raw.time else observation.lastValidAt
    sample
      | Array.length samples >= settings.minimumSamples = Just
          { clarity: median (map _.clarity samples)
          , frequency: median (map _.frequency samples)
          }
      | raw.time - lastValidAt >= settings.silenceMilliseconds = Just
          { clarity: 0.0, frequency: 0.0 }
      | otherwise = Nothing
  { observation: Observation { lastValidAt, samples }
  , sample
  }

median :: Array Number -> Number
median values = do
  let
    sorted = Array.sort values
    middle = Array.length sorted / 2
  if Array.length sorted `mod` 2 == 1 then fromMaybe 0.0 (Array.index sorted middle)
  else
    ( fromMaybe 0.0 (Array.index sorted (middle - 1))
        + fromMaybe 0.0 (Array.index sorted middle)
    ) / 2.0

midiFrequency :: Int -> Number
midiFrequency midi = 440.0 * Math.pow 2.0 (Int.toNumber (midi - 69) / 12.0)

nearestMidi :: Number -> Int
nearestMidi frequency = Int.round (69.0 + 12.0 * Math.log (frequency / 440.0) / Math.log 2.0)

feedbackFor :: Boolean -> Int -> PitchSample -> Maybe Feedback
feedbackFor allowOctaveEquivalent expectedMidi sample
  | sample.frequency <= 0.0 = Nothing
  | otherwise = do
      let
        detectedMidi = 69.0 + 12.0 * Math.log (sample.frequency / 440.0) / Math.log 2.0
        comparisonMidi =
          if allowOctaveEquivalent then
            expectedMidi + 12 * Int.round ((detectedMidi - Int.toNumber expectedMidi) / 12.0)
          else
            expectedMidi
        cents = 100.0 * (detectedMidi - Int.toNumber comparisonMidi)
        midi = Int.round detectedMidi
      Just { cents, clarity: sample.clarity, midi }

matchesExpected :: RecognitionSettings -> Boolean -> Int -> Feedback -> Boolean
matchesExpected settings allowOctaveEquivalent expected current =
  matchesPitchIdentity allowOctaveEquivalent expected current
    && absolute current.cents <= settings.toleranceCents

matchesPitchIdentity :: Boolean -> Int -> Feedback -> Boolean
matchesPitchIdentity allowOctaveEquivalent expected current =
  if allowOctaveEquivalent then current.midi `mod` 12 == expected `mod` 12
  else current.midi == expected

absolute :: Number -> Number
absolute value = if value < 0.0 then -value else value

stepStable :: Int -> Feedback -> StablePitch -> StablePitch
stepStable required current stable = do
  let
    frames = if stable.candidate == Just current.midi then stable.stableFrames + 1 else 1
  stable
    { candidate = Just current.midi
    , feedback = Just current
    , stableFrames = min required frames
    }

resetStable :: Maybe Feedback -> StablePitch
resetStable current = initialStablePitch { feedback = current }

stepRecognition
  :: RecognitionSettings
  -> OctavePolicy
  -> Pitch
  -> Pitch
  -> PitchSample
  -> Recognition
  -> Recognition
stepRecognition settings octavePolicy firstPitch secondPitch sample recognition = do
  let
    writtenFirst = midiNumber firstPitch
    writtenSecond = midiNumber secondPitch
    clear = sample.clarity >= settings.clarityThreshold
    normalizedSecond acceptedMidi = case octavePolicy of
      AnyOctave -> acceptedMidi + writtenSecond - writtenFirst
      WrittenOctave -> writtenSecond
    currentFeedback allowOctaveEquivalent expectedMidi =
      feedbackFor allowOctaveEquivalent expectedMidi sample
    matches allowOctaveEquivalent expectedMidi = case currentFeedback allowOctaveEquivalent expectedMidi of
      Just current -> clear && matchesExpected settings allowOctaveEquivalent expectedMidi current
      Nothing -> false
    updateStable expectedMidi allowOctaveEquivalent stable continue =
      case currentFeedback allowOctaveEquivalent expectedMidi of
        Just current | clear -> do
          let
            next = stepStable settings.stableFramesRequired current stable
          if next.stableFrames < settings.stableFramesRequired then SingingFirst next
          else continue current next
        current -> SingingFirst (resetStable current)
  case recognition of
    Complete state -> Complete state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    IncorrectFirst _ -> IncorrectFirst (currentFeedback (octavePolicy == AnyOctave) writtenFirst)
    IncorrectSecond state -> IncorrectSecond state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    SingingFirst stable -> do
      let
        allowOctaveEquivalent = octavePolicy == AnyOctave
      updateStable writtenFirst allowOctaveEquivalent stable \current next ->
        if matches allowOctaveEquivalent writtenFirst then
          ReleasingFirst { feedback: next.feedback, firstMidi: current.midi, releaseFrames: 0 }
        else if matchesPitchIdentity allowOctaveEquivalent writtenFirst current then SingingFirst next
        else IncorrectFirst next.feedback
    ReleasingFirst state
      | matches false state.firstMidi -> ReleasingFirst state
          { feedback = currentFeedback false state.firstMidi
          , releaseFrames = 0
          }
      | otherwise -> do
          let
            frames = state.releaseFrames + 1
            current = currentFeedback false state.firstMidi
          if frames >= settings.releaseFramesRequired then
            SingingSecond { firstMidi: state.firstMidi, stable: resetStable current }
          else ReleasingFirst state { feedback = current, releaseFrames = frames }
    SingingSecond state -> do
      let
        expectedMidi = normalizedSecond state.firstMidi
      case currentFeedback false expectedMidi of
        Just current | clear -> do
          let
            next = stepStable settings.stableFramesRequired current state.stable
          if next.stableFrames < settings.stableFramesRequired then SingingSecond state { stable = next }
          else if matches false expectedMidi then Complete { feedback: next.feedback, firstMidi: state.firstMidi }
          else if matchesPitchIdentity false expectedMidi current then SingingSecond state { stable = next }
          else IncorrectSecond { feedback: next.feedback, firstMidi: state.firstMidi }
        current -> SingingSecond state { stable = resetStable current }

phaseInstruction :: RecognitionPhase -> String
phaseInstruction = case _ of
  WaitingForFirst -> "Sing the first note."
  WaitingForRelease -> "Sing the second note."
  WaitingForSecond -> "Sing the second note."
  RecognitionIncorrect -> "Incorrect pitch."
  RecognitionComplete -> "Both notes accepted."

relativeMidi :: OctavePolicy -> Pitch -> Recognition -> Int -> Int
relativeMidi octavePolicy root recognition detectedMidi = case octavePolicy of
  WrittenOctave -> detectedMidi
  AnyOctave -> case firstMidi recognition of
    Just accepted -> detectedMidi + midiNumber root - accepted
    Nothing ->
      if detectedMidi `mod` 12 == midiNumber root `mod` 12 then midiNumber root
      else detectedMidi

firstMidi :: Recognition -> Maybe Int
firstMidi = case _ of
  SingingFirst _ -> Nothing
  ReleasingFirst state -> Just state.firstMidi
  SingingSecond state -> Just state.firstMidi
  IncorrectFirst _ -> Nothing
  IncorrectSecond state -> Just state.firstMidi
  Complete state -> Just state.firstMidi
