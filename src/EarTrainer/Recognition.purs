module EarTrainer.Recognition
  ( CaptureSettings
  , CapturePhase(..)
  , Feedback
  , Observation
  , PitchObservation(..)
  , PitchExpectation(..)
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
  , intervalPitchExpectation
  , midiFrequency
  , nearestMidi
  , observePitch
  , phase
  , phaseInstruction
  , relativeMidi
  , sequenceAcceptedCount
  , sequenceFeedback
  , sequencePitchExpectation
  , sequencePhase
  , sequenceRelativeMidi
  , stepSequenceRecognition
  , stepRecognition
  , selectPitchCandidate
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
  , time :: Number
  }

data PitchObservation
  = NoEvidence
  | ObservedPitch PitchSample
  | ArticulationBreak Number

derive instance Eq PitchObservation

instance Show PitchObservation where
  show = case _ of
    NoEvidence -> "NoEvidence"
    ObservedPitch sample -> "ObservedPitch " <> show sample
    ArticulationBreak time -> "ArticulationBreak " <> show time

data PitchExpectation
  = ExactPitch Int
  | OctaveEquivalentPitch Int

derive instance Eq PitchExpectation

instance Show PitchExpectation where
  show = case _ of
    ExactPitch midi -> "ExactPitch " <> show midi
    OctaveEquivalentPitch midi -> "OctaveEquivalentPitch " <> show midi

data CapturePhase
  = DetectingPitch
  | AwaitingArticulation

derive instance Eq CapturePhase

type TimedPitchSample =
  { clarity :: Number
  , frequency :: Number
  , time :: Number
  }

newtype Observation = Observation
  { lastValidAt :: Number
  , lastOnsetAt :: Number
  , previousDecibels :: Maybe Number
  , samples :: Array TimedPitchSample
  }

type CaptureSettings =
  { clarityThreshold :: Number
  , maximumFrequency :: Number
  , minimumFrequency :: Number
  , minimumSamples :: Int
  , minimumOnsetIntervalMilliseconds :: Number
  , onsetRiseDecibels :: Number
  , sampleWindowMilliseconds :: Number
  , silenceMilliseconds :: Number
  , volumeThresholdDb :: Number
  }

defaultCaptureSettings :: CaptureSettings
defaultCaptureSettings =
  { clarityThreshold: 0.8
  , maximumFrequency: 4200.0
  , minimumFrequency: 40.0
  , minimumSamples: 2
  , minimumOnsetIntervalMilliseconds: 80.0
  , onsetRiseDecibels: 9.0
  , sampleWindowMilliseconds: 100.0
  , silenceMilliseconds: 35.0
  , volumeThresholdDb: -50.0
  }

initialObservation :: Observation
initialObservation = Observation
  { lastOnsetAt: 0.0
  , lastValidAt: 0.0
  , previousDecibels: Nothing
  , samples: []
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
  show = case _ of
    WaitingForFirst -> "WaitingForFirst"
    WaitingForRelease -> "WaitingForRelease"
    WaitingForSecond -> "WaitingForSecond"
    RecognitionIncorrect -> "RecognitionIncorrect"
    RecognitionComplete -> "RecognitionComplete"

type StablePitch =
  { candidate :: Maybe Int
  , confidenceMilliseconds :: Number
  , feedback :: Maybe Feedback
  , lastObservedAt :: Maybe Number
  }

data Recognition
  = MatchingFirst StablePitch
  | ReleasingFirst
      { feedback :: Maybe Feedback
      , firstMidi :: Int
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
  , incorrectMillisecondsRequired :: Number
  , maximumObservationGapMilliseconds :: Number
  , stableMillisecondsRequired :: Number
  , toleranceCents :: Number
  }

defaultRecognitionSettings :: RecognitionSettings
defaultRecognitionSettings =
  { clarityThreshold: 0.8
  , incorrectMillisecondsRequired: 350.0
  , maximumObservationGapMilliseconds: 50.0
  , stableMillisecondsRequired: 120.0
  , toleranceCents: 35.0
  }

initialRecognition :: Recognition
initialRecognition = MatchingFirst initialStablePitch

initialStablePitch :: StablePitch
initialStablePitch =
  { candidate: Nothing
  , confidenceMilliseconds: 0.0
  , feedback: Nothing
  , lastObservedAt: Nothing
  }

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
  -> CapturePhase
  -> PitchExpectation
  -> PitchInput.Sample
  -> Observation
  -> { event :: PitchObservation, observation :: Observation }
observePitch settings capturePhase expectation raw (Observation observation) = do
  let
    candidate = selectPitchCandidate settings expectation raw.candidates
    clarity = fromMaybe 0.0 (map _.clarity candidate)
    frequency = fromMaybe 0.0 (map _.frequency candidate)
    valid =
      clarity >= settings.clarityThreshold
        && raw.decibels >= settings.volumeThresholdDb
        && frequency >= settings.minimumFrequency
        && frequency <= settings.maximumFrequency
    samples = Array.filter
      (\timed -> raw.time - timed.time <= settings.sampleWindowMilliseconds)
      if valid then
        Array.snoc observation.samples
          { clarity
          , frequency
          , time: raw.time
          }
      else
        observation.samples
    lastValidAt = if valid then raw.time else observation.lastValidAt
    onsetDetected = case capturePhase, observation.previousDecibels of
      AwaitingArticulation, Just previous ->
        raw.decibels >= settings.volumeThresholdDb
          && raw.decibels - previous >= settings.onsetRiseDecibels
          && raw.time - observation.lastOnsetAt >= settings.minimumOnsetIntervalMilliseconds
      _, _ -> false
    breakDetected = onsetDetected || (not valid && raw.time - lastValidAt >= settings.silenceMilliseconds)
    lastOnsetAt = if onsetDetected then raw.time else observation.lastOnsetAt
    nextSamples = if breakDetected then [] else samples
    event
      | breakDetected = ArticulationBreak raw.time
      | Array.length samples >= settings.minimumSamples = ObservedPitch
          { clarity: median (map _.clarity samples)
          , frequency: median (map _.frequency samples)
          , time: raw.time
          }
      | otherwise = NoEvidence
  { observation: Observation
      { lastOnsetAt
      , lastValidAt
      , previousDecibels: Just raw.decibels
      , samples: nextSamples
      }
  , event
  }

selectPitchCandidate
  :: CaptureSettings
  -> PitchExpectation
  -> Array PitchInput.PitchCandidate
  -> Maybe PitchInput.PitchCandidate
selectPitchCandidate settings expectation candidates =
  Array.foldl preferCandidate Nothing
    ( Array.filter
        (\candidate -> candidate.frequency >= settings.minimumFrequency && candidate.frequency <= settings.maximumFrequency)
        candidates
    )
  where
  preferCandidate current candidate = case current of
    Just previous | candidateScore previous >= candidateScore candidate -> current
    _ -> Just candidate

  candidateScore candidate = do
    let
      agreement = Array.length
        ( Array.filter
            (\other -> absolute (centsBetween candidate.frequency other.frequency) <= 35.0)
            candidates
        )
      expectedBonus = if expectationDistance expectation candidate.frequency <= 60.0 then 2.0 else 0.0
    candidate.clarity + expectedBonus + Int.toNumber agreement * 0.1

expectationDistance :: PitchExpectation -> Number -> Number
expectationDistance expectation frequency = do
  let
    detectedMidi = 69.0 + 12.0 * Math.log (frequency / 440.0) / Math.log 2.0
    expectedMidi = case expectation of
      ExactPitch midi -> Int.toNumber midi
      OctaveEquivalentPitch midi ->
        Int.toNumber midi + 12.0 * Int.toNumber (Int.round ((detectedMidi - Int.toNumber midi) / 12.0))
  absolute (100.0 * (detectedMidi - expectedMidi))

centsBetween :: Number -> Number -> Number
centsBetween left right = 1200.0 * Math.log (left / right) / Math.log 2.0

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

observationSample :: PitchObservation -> Maybe PitchSample
observationSample = case _ of
  ObservedPitch sample -> Just sample
  _ -> Nothing

observationTime :: PitchObservation -> Maybe Number
observationTime = case _ of
  ObservedPitch sample -> Just sample.time
  ArticulationBreak time -> Just time
  NoEvidence -> Nothing

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

stepStable :: RecognitionSettings -> Number -> Feedback -> StablePitch -> StablePitch
stepStable settings observedAt current stable = do
  let
    sameCandidate = stable.candidate == Just current.midi
    elapsed = case stable.lastObservedAt of
      Just previous | sameCandidate -> min settings.maximumObservationGapMilliseconds (max 0.0 (observedAt - previous))
      _ -> 0.0
    confidence = if sameCandidate then stable.confidenceMilliseconds + elapsed else 0.0
  stable
    { candidate = Just current.midi
    , confidenceMilliseconds = min
        (max settings.stableMillisecondsRequired settings.incorrectMillisecondsRequired)
        confidence
    , feedback = Just current
    , lastObservedAt = Just observedAt
    }

resetStable :: Maybe Feedback -> StablePitch
resetStable current = initialStablePitch { feedback = current }

stepRecognition
  :: RecognitionSettings
  -> OctavePolicy
  -> Pitch
  -> Pitch
  -> PitchObservation
  -> Recognition
  -> Recognition
stepRecognition settings octavePolicy firstPitch secondPitch observation recognition = do
  let
    writtenFirst = midiNumber firstPitch
    writtenSecond = midiNumber secondPitch
    sample = observationSample observation
    observedAt = fromMaybe 0.0 (observationTime observation)
    clear = case sample of
      Just current -> current.clarity >= settings.clarityThreshold
      Nothing -> false
    normalizedSecond acceptedMidi = case octavePolicy of
      AnyOctave -> acceptedMidi + writtenSecond - writtenFirst
      WrittenOctave -> writtenSecond
    currentFeedback allowOctaveEquivalent expectedMidi =
      sample >>= feedbackFor allowOctaveEquivalent expectedMidi
    matches allowOctaveEquivalent expectedMidi = case currentFeedback allowOctaveEquivalent expectedMidi of
      Just current -> clear && matchesExpected settings allowOctaveEquivalent expectedMidi current
      Nothing -> false
    updateStable expectedMidi allowOctaveEquivalent stable continue =
      case currentFeedback allowOctaveEquivalent expectedMidi of
        Just current | clear -> do
          let
            next = stepStable settings observedAt current stable
          continue current next
        current -> MatchingFirst (resetStable current)
  case observation, recognition of
    NoEvidence, _ -> recognition
    _, Complete state -> Complete state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    _, IncorrectFirst _ -> IncorrectFirst (currentFeedback (octavePolicy == AnyOctave) writtenFirst)
    _, IncorrectSecond state -> IncorrectSecond state { feedback = currentFeedback false (normalizedSecond state.firstMidi) }
    _, MatchingFirst stable -> do
      let
        allowOctaveEquivalent = octavePolicy == AnyOctave
      updateStable writtenFirst allowOctaveEquivalent stable \current next ->
        if
          matches allowOctaveEquivalent writtenFirst
            && next.confidenceMilliseconds >= settings.stableMillisecondsRequired then
          ReleasingFirst { feedback: next.feedback, firstMidi: current.midi }
        else if
          not (matchesPitchIdentity allowOctaveEquivalent writtenFirst current)
            && next.confidenceMilliseconds >= settings.incorrectMillisecondsRequired then
          IncorrectFirst next.feedback
        else MatchingFirst next
    ArticulationBreak _, ReleasingFirst state ->
      MatchingSecond { firstMidi: state.firstMidi, stable: initialStablePitch }
    ObservedPitch _, ReleasingFirst state -> ReleasingFirst state
      { feedback = currentFeedback false state.firstMidi }
    _, MatchingSecond state -> do
      let
        expectedMidi = normalizedSecond state.firstMidi
      case currentFeedback false expectedMidi of
        Just current | clear -> do
          let
            next = stepStable settings observedAt current state.stable
          if matches false expectedMidi && next.confidenceMilliseconds >= settings.stableMillisecondsRequired then
            Complete { feedback: next.feedback, firstMidi: state.firstMidi }
          else if
            not (matchesPitchIdentity false expectedMidi current)
              && next.confidenceMilliseconds >= settings.incorrectMillisecondsRequired then
            IncorrectSecond { feedback: next.feedback, firstMidi: state.firstMidi }
          else MatchingSecond state { stable = next }
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

intervalPitchExpectation :: OctavePolicy -> Pitch -> Pitch -> Recognition -> PitchExpectation
intervalPitchExpectation octavePolicy firstPitch secondPitch recognition = do
  let
    writtenFirst = midiNumber firstPitch
    writtenSecond = midiNumber secondPitch
    firstExpectation = case octavePolicy of
      AnyOctave -> OctaveEquivalentPitch writtenFirst
      WrittenOctave -> ExactPitch writtenFirst
    secondExpectation acceptedFirst = case octavePolicy of
      AnyOctave -> ExactPitch (acceptedFirst + writtenSecond - writtenFirst)
      WrittenOctave -> ExactPitch writtenSecond
  case recognition of
    MatchingFirst _ -> firstExpectation
    IncorrectFirst _ -> firstExpectation
    ReleasingFirst state -> secondExpectation state.firstMidi
    MatchingSecond state -> secondExpectation state.firstMidi
    IncorrectSecond state -> secondExpectation state.firstMidi
    Complete state -> secondExpectation state.firstMidi

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
  -> PitchObservation
  -> SequenceRecognition
  -> SequenceRecognition
stepSequenceRecognition settings octavePolicy expected observation recognition = do
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
    sample = observationSample observation
    observedAt = fromMaybe 0.0 (observationTime observation)
    clear = case sample of
      Just current -> current.clarity >= settings.clarityThreshold
      Nothing -> false
    currentFeedback allowOctaveEquivalent midi = sample >>= feedbackFor allowOctaveEquivalent midi
  case observation, recognition of
    NoEvidence, _ -> recognition
    _, IncorrectSequence state -> IncorrectSequence state
      { feedback = expectedMidi >>= currentFeedback false }
    _, CompleteSequence state -> CompleteSequence state
      { feedback = map midiNumber (Array.last expectedPitches) >>= currentFeedback false }
    ArticulationBreak _, ReleasingSequence state ->
      MatchingSequence { acceptedMidi: state.acceptedMidi, stable: initialStablePitch }
    ObservedPitch _, ReleasingSequence state -> ReleasingSequence state
      { feedback = currentFeedback false state.lastMidi }
    _, MatchingSequence state -> case expectedMidi of
      Nothing -> CompleteSequence { acceptedMidi: state.acceptedMidi, feedback: state.stable.feedback }
      Just midi -> do
        let
          allowOctaveEquivalent = octavePolicy == AnyOctave && Array.null state.acceptedMidi
          current = currentFeedback allowOctaveEquivalent midi
        case current of
          Just currentPitch | clear -> do
            let next = stepStable settings observedAt currentPitch state.stable
            if
              matchesExpected settings allowOctaveEquivalent midi currentPitch
                && next.confidenceMilliseconds >= settings.stableMillisecondsRequired then do
              let nextAccepted = Array.snoc state.acceptedMidi currentPitch.midi
              if Array.length nextAccepted == Array.length expectedPitches then
                CompleteSequence { acceptedMidi: nextAccepted, feedback: next.feedback }
              else
                ReleasingSequence
                  { acceptedMidi: nextAccepted
                  , feedback: next.feedback
                  , lastMidi: currentPitch.midi
                  }
            else if
              not (matchesPitchIdentity allowOctaveEquivalent midi currentPitch)
                && next.confidenceMilliseconds >= settings.incorrectMillisecondsRequired then
              IncorrectSequence { acceptedMidi: state.acceptedMidi, feedback: next.feedback }
            else MatchingSequence state { stable = next }
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

sequencePitchExpectation
  :: OctavePolicy
  -> NonEmptyArray.NonEmptyArray Pitch
  -> SequenceRecognition
  -> PitchExpectation
sequencePitchExpectation octavePolicy expected recognition = do
  let
    pitches = NonEmptyArray.toArray expected
    accepted = case recognition of
      MatchingSequence state -> state.acceptedMidi
      ReleasingSequence state -> state.acceptedMidi
      IncorrectSequence state -> state.acceptedMidi
      CompleteSequence state -> state.acceptedMidi
    first = midiNumber (NonEmptyArray.head expected)
    offset = case octavePolicy, Array.head accepted of
      AnyOctave, Just acceptedFirst -> acceptedFirst - first
      _, _ -> 0
    current = fromMaybe (midiNumber (NonEmptyArray.last expected))
      (map ((+) offset <<< midiNumber) (Array.index pitches (Array.length accepted)))
  case octavePolicy, Array.null accepted of
    AnyOctave, true -> OctaveEquivalentPitch first
    _, _ -> ExactPitch current
