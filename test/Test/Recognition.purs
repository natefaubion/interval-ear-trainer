module Test.Recognition (run) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import EarTrainer.Music (Accidental(..), Letter(..), OctavePolicy(..), pitch)
import EarTrainer.Recognition
  ( RecognitionPhase(..)
  , SequencePhase(..)
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , initialObservation
  , initialRecognition
  , initialSequenceRecognition
  , nearestMidi
  , observePitch
  , phase
  , relativeMidi
  , sequenceAcceptedCount
  , sequencePhase
  , stepRecognition
  , stepSequenceRecognition
  )
import Effect (Effect)
import Test.Assert (assertEqual)

run :: Effect Unit
run = do
  let
    c4 = pitch C (Accidental 0) 4
    nonEmpty values = fromMaybe (NonEmptyArray.singleton c4) (NonEmptyArray.fromArray values)
    e4 = pitch E (Accidental 0) 4
    d4 = pitch D (Accidental 0) 4
    b4 = pitch B (Accidental 0) 4
    c4Sample = { frequency: 261.625565, clarity: 0.98 }
    c3Sample = { frequency: 130.812783, clarity: 0.98 }
    c5Sample = { frequency: 523.251131, clarity: 0.98 }
    c4SharpFortyCentsSample = { frequency: 267.744, clarity: 0.98 }
    e4Sample = { frequency: 329.627557, clarity: 0.98 }
    e3Sample = { frequency: 164.813778, clarity: 0.98 }
    d4Sample = { frequency: 293.664768, clarity: 0.98 }
    d3Sample = { frequency: 146.832384, clarity: 0.98 }
    e4SharpFortyCentsSample = { frequency: 337.337, clarity: 0.98 }
    b2Sample = { frequency: 123.470825, clarity: 0.98 }
    b3Sample = { frequency: 246.941651, clarity: 0.98 }
    silence = { frequency: 0.0, clarity: 0.0 }
    stableSamples = defaultRecognitionSettings.stableFramesRequired
    releaseSamples = defaultRecognitionSettings.releaseFramesRequired
    step sample recognition = stepRecognition defaultRecognitionSettings AnyOctave c4 e4 sample recognition
    advance sample recognition = foldl (\current _ -> step sample current) recognition (Array.replicate stableSamples unit)
    beforeFirst = foldl (\current _ -> step c4Sample current) initialRecognition
      (Array.replicate (stableSamples - 1) unit)
    afterFirst = advance c4Sample initialRecognition
    afterRelease = foldl (\current _ -> step silence current) afterFirst (Array.replicate releaseSamples unit)
    afterSecond = advance e4Sample afterRelease
    wrongFirst = advance e4Sample initialRecognition
    wrongSecond = advance b3Sample afterRelease
    detunedFirst = advance c4SharpFortyCentsSample initialRecognition
    detunedSecond = advance e4SharpFortyCentsSample afterRelease
    anyOctaveFirst = advance c5Sample initialRecognition
    octaveBelowFirst = advance c3Sample initialRecognition
    majorSeventhStep sample recognition =
      stepRecognition defaultRecognitionSettings AnyOctave c4 b4 sample recognition
    majorSeventhFirst = foldl (\current _ -> majorSeventhStep c3Sample current) initialRecognition
      (Array.replicate stableSamples unit)
    majorSeventhRelease = foldl (\current _ -> majorSeventhStep silence current) majorSeventhFirst
      (Array.replicate releaseSamples unit)
    wrongOctaveSecond = foldl (\current _ -> majorSeventhStep b2Sample current) majorSeventhRelease
      (Array.replicate stableSamples unit)
    correctNormalizedSecond = foldl (\current _ -> majorSeventhStep b3Sample current) majorSeventhRelease
      (Array.replicate stableSamples unit)
    writtenOctaveFirst = foldl
      (\current _ -> stepRecognition defaultRecognitionSettings WrittenOctave c4 e4 c5Sample current)
      initialRecognition
      (Array.replicate stableSamples unit)
    sequenceStep sample recognition = stepSequenceRecognition
      defaultRecognitionSettings
      AnyOctave
      (nonEmpty [ c4, e4, d4 ])
      sample
      recognition
    sequenceAdvance sample recognition = foldl (\current _ -> sequenceStep sample current) recognition
      (Array.replicate stableSamples unit)
    sequenceFirst = sequenceAdvance c4Sample initialSequenceRecognition
    sequenceFirstRelease = foldl (\current _ -> sequenceStep silence current) sequenceFirst
      (Array.replicate releaseSamples unit)
    sequenceSecond = sequenceAdvance e4Sample sequenceFirstRelease
    sequenceSecondRelease = foldl (\current _ -> sequenceStep silence current) sequenceSecond
      (Array.replicate releaseSamples unit)
    sequenceComplete = sequenceAdvance d4Sample sequenceSecondRelease
    sequenceContinuedFirst = sequenceStep c4Sample sequenceFirst
    sequenceWrongMiddle = sequenceAdvance b3Sample sequenceFirstRelease
    sequenceWrongFinal = sequenceAdvance b3Sample sequenceSecondRelease
    sequencePartial = foldl (\current _ -> sequenceStep c4Sample current) initialSequenceRecognition
      (Array.replicate (stableSamples - 1) unit)
    sequencePartialSilence = sequenceStep silence sequencePartial
    sequenceAfterInterruptedPitch = foldl (\current _ -> sequenceStep c4Sample current) sequencePartialSilence
      (Array.replicate (stableSamples - 1) unit)
    repeatedStep sample recognition = stepSequenceRecognition
      defaultRecognitionSettings
      AnyOctave
      (nonEmpty [ c4, c4 ])
      sample
      recognition
    repeatedFirst = foldl (\current _ -> repeatedStep c4Sample current) initialSequenceRecognition
      (Array.replicate stableSamples unit)
    repeatedContinued = repeatedStep c4Sample repeatedFirst
    repeatedRelease = foldl (\current _ -> repeatedStep silence current) repeatedFirst
      (Array.replicate releaseSamples unit)
    repeatedComplete = foldl (\current _ -> repeatedStep c4Sample current) repeatedRelease
      (Array.replicate stableSamples unit)
    writtenSequenceFirst = foldl
      ( \current _ -> stepSequenceRecognition
          defaultRecognitionSettings
          WrittenOctave
          (nonEmpty [ c4, e4 ])
          c5Sample
          current
      )
      initialSequenceRecognition
      (Array.replicate stableSamples unit)
    octaveSequenceStep sample recognition = stepSequenceRecognition
      defaultRecognitionSettings
      AnyOctave
      (nonEmpty [ c4, e4, d4 ])
      sample
      recognition
    octaveFirst = foldl (\current _ -> octaveSequenceStep c3Sample current) initialSequenceRecognition
      (Array.replicate stableSamples unit)
    octaveFirstRelease = foldl (\current _ -> octaveSequenceStep silence current) octaveFirst
      (Array.replicate releaseSamples unit)
    octaveSecond = foldl (\current _ -> octaveSequenceStep e3Sample current) octaveFirstRelease
      (Array.replicate stableSamples unit)
    octaveSecondRelease = foldl (\current _ -> octaveSequenceStep silence current) octaveSecond
      (Array.replicate releaseSamples unit)
    octaveComplete = foldl (\current _ -> octaveSequenceStep d3Sample current) octaveSecondRelease
      (Array.replicate stableSamples unit)
    octaveShiftedSecond = foldl (\current _ -> octaveSequenceStep e4Sample current) octaveFirstRelease
      (Array.replicate stableSamples unit)
    completeRepeatedSequence count = do
      let
        expected = nonEmpty (Array.replicate count c4)
        stepExpected sample recognition = stepSequenceRecognition
          defaultRecognitionSettings
          AnyOctave
          expected
          sample
          recognition
        singNote recognition = foldl (\current _ -> stepExpected c4Sample current) recognition
          (Array.replicate stableSamples unit)
        releaseNote recognition = foldl (\current _ -> stepExpected silence current) recognition
          (Array.replicate releaseSamples unit)
      foldl
        ( \recognition index -> do
            let accepted = singNote recognition
            if index == count - 1 then accepted else releaseNote accepted
        )
        initialSequenceRecognition
        (Array.range 0 (count - 1))
    sequenceLengths = map (sequencePhase <<< completeRepeatedSequence) (Array.range 3 8)
    rawPitch time frequency = { clarity: 0.96, decibels: -20.0, frequency, time }
    observed1 = observePitch defaultCaptureSettings (rawPitch 10.0 100.0) initialObservation
    observed2 = observePitch defaultCaptureSettings (rawPitch 20.0 200.0) observed1.observation
    observed3 = observePitch defaultCaptureSettings (rawPitch 30.0 300.0) observed2.observation
    observed4 = observePitch defaultCaptureSettings (rawPitch 40.0 400.0) observed3.observation
    observedSilence = observePitch defaultCaptureSettings
      { clarity: 0.99, decibels: -80.0, frequency: 440.0, time: 400.0 }
      observed4.observation
    observedConsonant = observePitch defaultCaptureSettings
      { clarity: 0.2, decibels: -18.0, frequency: 180.0, time: 76.0 }
      observed4.observation
    observedBriefDrop = observePitch defaultCaptureSettings
      { clarity: 0.2, decibels: -18.0, frequency: 180.0, time: 60.0 }
      observed4.observation
    observedNew1 = observePitch defaultCaptureSettings (rawPitch 80.0 500.0) observedConsonant.observation
    observedNew2 = observePitch defaultCaptureSettings (rawPitch 90.0 500.0) observedNew1.observation
    observedNew3 = observePitch defaultCaptureSettings (rawPitch 100.0 500.0) observedNew2.observation
    observedNew4 = observePitch defaultCaptureSettings (rawPitch 110.0 500.0) observedNew3.observation
  assertEqual { actual: nearestMidi 440.0, expected: 69 }
  assertEqual { actual: phase beforeFirst, expected: WaitingForFirst }
  assertEqual { actual: phase afterFirst, expected: WaitingForRelease }
  assertEqual { actual: phase afterRelease, expected: WaitingForSecond }
  assertEqual { actual: phase afterSecond, expected: RecognitionComplete }
  assertEqual { actual: phase wrongFirst, expected: RecognitionIncorrect }
  assertEqual { actual: phase wrongSecond, expected: RecognitionIncorrect }
  assertEqual { actual: phase detunedFirst, expected: WaitingForFirst }
  assertEqual { actual: phase detunedSecond, expected: WaitingForSecond }
  assertEqual { actual: phase anyOctaveFirst, expected: WaitingForRelease }
  assertEqual { actual: phase writtenOctaveFirst, expected: RecognitionIncorrect }
  assertEqual { actual: phase wrongOctaveSecond, expected: RecognitionIncorrect }
  assertEqual { actual: phase correctNormalizedSecond, expected: RecognitionComplete }
  assertEqual { actual: relativeMidi AnyOctave c4 octaveBelowFirst 57, expected: 69 }
  assertEqual { actual: relativeMidi AnyOctave c4 wrongFirst 64, expected: 64 }
  assertEqual { actual: relativeMidi WrittenOctave c4 octaveBelowFirst 57, expected: 57 }
  assertEqual { actual: observed1.sample, expected: Nothing }
  assertEqual { actual: observed4.sample, expected: Just { clarity: 0.96, frequency: 250.0 } }
  assertEqual { actual: observedSilence.sample, expected: Just { clarity: 0.0, frequency: 0.0 } }
  assertEqual { actual: observedConsonant.sample, expected: Just { clarity: 0.0, frequency: 0.0 } }
  assertEqual { actual: observedBriefDrop.sample, expected: Just { clarity: 0.96, frequency: 250.0 } }
  assertEqual { actual: observedNew3.sample, expected: Nothing }
  assertEqual { actual: observedNew4.sample, expected: Just { clarity: 0.96, frequency: 500.0 } }
  assertEqual { actual: sequenceAcceptedCount sequenceSecond, expected: 2 }
  assertEqual { actual: sequencePhase sequenceComplete, expected: SequenceComplete }
  assertEqual { actual: sequencePhase sequenceContinuedFirst, expected: SequenceReleasing }
  assertEqual { actual: sequencePhase sequenceWrongMiddle, expected: SequenceIncorrect }
  assertEqual { actual: sequenceAcceptedCount sequenceWrongMiddle, expected: 1 }
  assertEqual { actual: sequencePhase sequenceWrongFinal, expected: SequenceIncorrect }
  assertEqual { actual: sequenceAcceptedCount sequenceWrongFinal, expected: 2 }
  assertEqual { actual: sequenceAcceptedCount sequenceAfterInterruptedPitch, expected: 0 }
  assertEqual { actual: sequencePhase repeatedContinued, expected: SequenceReleasing }
  assertEqual { actual: sequencePhase repeatedComplete, expected: SequenceComplete }
  assertEqual { actual: sequencePhase writtenSequenceFirst, expected: SequenceIncorrect }
  assertEqual { actual: sequencePhase octaveComplete, expected: SequenceComplete }
  assertEqual { actual: sequencePhase octaveShiftedSecond, expected: SequenceIncorrect }
  assertEqual { actual: sequenceLengths, expected: Array.replicate 6 SequenceComplete }
  assertEqual { actual: sequenceAcceptedCount initialSequenceRecognition, expected: 0 }
