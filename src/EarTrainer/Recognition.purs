module EarTrainer.Recognition
  ( CaptureSettings
  , Feedback
  , Observation
  , PitchSample
  , Recognition
  , RecognitionPhase(..)
  , RecognitionSettings
  , SequencePhase(..)
  , SequenceRecognition
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , feedback
  , initialObservation
  , initialRecognition
  , initialSequenceRecognition
  , midiFrequency
  , nearestMidi
  , observePitch
  , phase
  , phaseInstruction
  , relativeMidi
  , sequenceAcceptedCount
  , sequenceFeedback
  , sequencePhase
  , sequenceRelativeMidi
  , stepSequenceRecognition
  , stepRecognition
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
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
  , silenceMilliseconds: 35.0
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
  = MatchingFirst StablePitch
  | ReleasingFirst
      { feedback :: Maybe Feedback
      , firstMidi :: Int
      , releaseFrames :: Int
      }
  | MatchingSecond
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
  , releaseFramesRequired: 2
  , stableFramesRequired: 36
  , toleranceCents: 35.0
  }

initialRecognition :: Recognition
initialRecognition = MatchingFirst initialStablePitch

initialStablePitch :: StablePitch
initialStablePitch = { candidate: Nothing, feedback: Nothing, stableFrames: 0 }

phase :: Recognition -> RecognitionPhase
phase = case _ of
  MatchingFirst _ -> WaitingForFirst
  ReleasingFirst _ -> WaitingForRelease
  MatchingSecond _ -> WaitingForSecond
  IncorrectFirst _ -> RecognitionIncorrect
  IncorrectSecond _ -> RecognitionIncorrect
  Complete _ -> RecognitionComplete

feedback :: Recognition -> Maybe Feedback
feedback = case _ of
  MatchingFirst stable -> stable.feedback
  ReleasingFirst state -> state.feedback
  MatchingSecond state -> state.stable.feedback
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
    breakDetected = not valid && raw.time - lastValidAt >= settings.silenceMilliseconds
    nextSamples = if breakDetected then [] else samples
    sample
      | breakDetected = Just
          { clarity: 0.0, frequency: 0.0 }
      | Array.length samples >= settings.minimumSamples = Just
          { clarity: median (map _.clarity samples)
          , frequency: median (map _.frequency samples)
          }
      | otherwise = Nothing
  { observation: Observation { lastValidAt, samples: nextSamples }
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
          if next.stableFrames < settings.stableFramesRequired then MatchingFirst next
          else continue current next
        current -> MatchingFirst (resetStable current)
  case recognition of
    Complete state -> Complete state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    IncorrectFirst _ -> IncorrectFirst (currentFeedback (octavePolicy == AnyOctave) writtenFirst)
    IncorrectSecond state -> IncorrectSecond state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    MatchingFirst stable -> do
      let
        allowOctaveEquivalent = octavePolicy == AnyOctave
      updateStable writtenFirst allowOctaveEquivalent stable \current next ->
        if matches allowOctaveEquivalent writtenFirst then
          ReleasingFirst { feedback: next.feedback, firstMidi: current.midi, releaseFrames: 0 }
        else if matchesPitchIdentity allowOctaveEquivalent writtenFirst current then MatchingFirst next
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
            MatchingSecond { firstMidi: state.firstMidi, stable: resetStable current }
          else ReleasingFirst state { feedback = current, releaseFrames = frames }
    MatchingSecond state -> do
      let
        expectedMidi = normalizedSecond state.firstMidi
      case currentFeedback false expectedMidi of
        Just current | clear -> do
          let
            next = stepStable settings.stableFramesRequired current state.stable
          if next.stableFrames < settings.stableFramesRequired then MatchingSecond state { stable = next }
          else if matches false expectedMidi then Complete { feedback: next.feedback, firstMidi: state.firstMidi }
          else if matchesPitchIdentity false expectedMidi current then MatchingSecond state { stable = next }
          else IncorrectSecond { feedback: next.feedback, firstMidi: state.firstMidi }
        current -> MatchingSecond state { stable = resetStable current }

phaseInstruction :: RecognitionPhase -> String
phaseInstruction = case _ of
  WaitingForFirst -> "Sing or play the first note."
  WaitingForRelease -> "Release, then sing or play the second note."
  WaitingForSecond -> "Sing or play the second note."
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
  MatchingFirst _ -> Nothing
  ReleasingFirst state -> Just state.firstMidi
  MatchingSecond state -> Just state.firstMidi
  IncorrectFirst _ -> Nothing
  IncorrectSecond state -> Just state.firstMidi
  Complete state -> Just state.firstMidi

data SequencePhase
  = SequenceMatching
  | SequenceReleasing
  | SequenceIncorrect
  | SequenceComplete

derive instance Eq SequencePhase

instance Show SequencePhase where
  show = case _ of
    SequenceMatching -> "SequenceMatching"
    SequenceReleasing -> "SequenceReleasing"
    SequenceIncorrect -> "SequenceIncorrect"
    SequenceComplete -> "SequenceComplete"

data SequenceRecognition
  = MatchingSequence
      { acceptedMidi :: Array Int
      , stable :: StablePitch
      }
  | ReleasingSequence
      { acceptedMidi :: Array Int
      , feedback :: Maybe Feedback
      , lastMidi :: Int
      , releaseFrames :: Int
      }
  | IncorrectSequence
      { acceptedMidi :: Array Int
      , feedback :: Maybe Feedback
      }
  | CompleteSequence
      { acceptedMidi :: Array Int
      , feedback :: Maybe Feedback
      }

initialSequenceRecognition :: SequenceRecognition
initialSequenceRecognition = MatchingSequence { acceptedMidi: [], stable: initialStablePitch }

sequencePhase :: SequenceRecognition -> SequencePhase
sequencePhase = case _ of
  MatchingSequence _ -> SequenceMatching
  ReleasingSequence _ -> SequenceReleasing
  IncorrectSequence _ -> SequenceIncorrect
  CompleteSequence _ -> SequenceComplete

sequenceAcceptedCount :: SequenceRecognition -> Int
sequenceAcceptedCount = Array.length <<< case _ of
  MatchingSequence state -> state.acceptedMidi
  ReleasingSequence state -> state.acceptedMidi
  IncorrectSequence state -> state.acceptedMidi
  CompleteSequence state -> state.acceptedMidi

sequenceFeedback :: SequenceRecognition -> Maybe Feedback
sequenceFeedback = case _ of
  MatchingSequence state -> state.stable.feedback
  ReleasingSequence state -> state.feedback
  IncorrectSequence state -> state.feedback
  CompleteSequence state -> state.feedback

stepSequenceRecognition
  :: RecognitionSettings
  -> OctavePolicy
  -> NonEmptyArray.NonEmptyArray Pitch
  -> PitchSample
  -> SequenceRecognition
  -> SequenceRecognition
stepSequenceRecognition settings octavePolicy expected sample recognition = do
  let
    expectedPitches = NonEmptyArray.toArray expected
    accepted = case recognition of
      MatchingSequence state -> state.acceptedMidi
      ReleasingSequence state -> state.acceptedMidi
      IncorrectSequence state -> state.acceptedMidi
      CompleteSequence state -> state.acceptedMidi
    writtenFirst = midiNumber (NonEmptyArray.head expected)
    offset = case octavePolicy, Array.head accepted of
      AnyOctave, Just first -> first - writtenFirst
      _, _ -> 0
    expectedMidi = map ((+) offset <<< midiNumber) (Array.index expectedPitches (Array.length accepted))
    clear = sample.clarity >= settings.clarityThreshold
    currentFeedback allowOctaveEquivalent midi = feedbackFor allowOctaveEquivalent midi sample
  case recognition of
    IncorrectSequence state -> IncorrectSequence state
      { feedback = expectedMidi >>= currentFeedback false }
    CompleteSequence state -> CompleteSequence state
      { feedback = map midiNumber (Array.last expectedPitches) >>= currentFeedback false }
    ReleasingSequence state -> do
      let
        current = currentFeedback false state.lastMidi
        stillProducing = case current of
          Just currentPitch -> clear && matchesPitchIdentity false state.lastMidi currentPitch
          Nothing -> false
        frames = if stillProducing then 0 else state.releaseFrames + 1
      if frames >= settings.releaseFramesRequired then
        MatchingSequence { acceptedMidi: state.acceptedMidi, stable: resetStable current }
      else ReleasingSequence state { feedback = current, releaseFrames = frames }
    MatchingSequence state -> case expectedMidi of
      Nothing -> CompleteSequence { acceptedMidi: state.acceptedMidi, feedback: state.stable.feedback }
      Just midi -> do
        let
          allowOctaveEquivalent = octavePolicy == AnyOctave && Array.null state.acceptedMidi
          current = currentFeedback allowOctaveEquivalent midi
        case current of
          Just currentPitch | clear -> do
            let next = stepStable settings.stableFramesRequired currentPitch state.stable
            if next.stableFrames < settings.stableFramesRequired then
              MatchingSequence state { stable = next }
            else if matchesExpected settings allowOctaveEquivalent midi currentPitch then do
              let nextAccepted = Array.snoc state.acceptedMidi currentPitch.midi
              if Array.length nextAccepted == Array.length expectedPitches then
                CompleteSequence { acceptedMidi: nextAccepted, feedback: next.feedback }
              else
                ReleasingSequence
                  { acceptedMidi: nextAccepted
                  , feedback: next.feedback
                  , lastMidi: currentPitch.midi
                  , releaseFrames: 0
                  }
            else if matchesPitchIdentity allowOctaveEquivalent midi currentPitch then
              MatchingSequence state { stable = next }
            else IncorrectSequence { acceptedMidi: state.acceptedMidi, feedback: next.feedback }
          _ -> MatchingSequence state { stable = resetStable current }

sequenceRelativeMidi :: OctavePolicy -> NonEmptyArray.NonEmptyArray Pitch -> SequenceRecognition -> Int -> Int
sequenceRelativeMidi octavePolicy expected recognition detectedMidi = case octavePolicy of
  WrittenOctave -> detectedMidi
  AnyOctave -> do
    let first = NonEmptyArray.head expected
    case firstAcceptedMidi recognition of
      Just accepted -> detectedMidi - (accepted - midiNumber first)
      Nothing ->
        if detectedMidi `mod` 12 == midiNumber first `mod` 12 then midiNumber first
        else detectedMidi

firstAcceptedMidi :: SequenceRecognition -> Maybe Int
firstAcceptedMidi = Array.head <<< case _ of
  MatchingSequence state -> state.acceptedMidi
  ReleasingSequence state -> state.acceptedMidi
  IncorrectSequence state -> state.acceptedMidi
  CompleteSequence state -> state.acceptedMidi
