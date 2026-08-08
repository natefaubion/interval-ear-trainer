module Test.Recognition (run) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import EarTrainer.Music (Accidental(..), Letter(..), OctavePolicy(..), pitch)
import EarTrainer.Recognition
  ( RecognitionPhase(..)
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , initialObservation
  , initialRecognition
  , nearestMidi
  , observePitch
  , phase
  , relativeMidi
  , stepRecognition
  )
import Effect (Effect)
import Test.Assert (assertEqual)

run :: Effect Unit
run = do
  let
    c4 = pitch C (Accidental 0) 4
    e4 = pitch E (Accidental 0) 4
    b4 = pitch B (Accidental 0) 4
    c4Sample = { frequency: 261.625565, clarity: 0.98 }
    c3Sample = { frequency: 130.812783, clarity: 0.98 }
    c5Sample = { frequency: 523.251131, clarity: 0.98 }
    c4SharpFortyCentsSample = { frequency: 267.744, clarity: 0.98 }
    e4Sample = { frequency: 329.627557, clarity: 0.98 }
    e4SharpFortyCentsSample = { frequency: 337.337, clarity: 0.98 }
    b2Sample = { frequency: 123.470825, clarity: 0.98 }
    b3Sample = { frequency: 246.941651, clarity: 0.98 }
    silence = { frequency: 0.0, clarity: 0.0 }
    stableSamples = defaultRecognitionSettings.stableFramesRequired
    step sample recognition = stepRecognition defaultRecognitionSettings AnyOctave c4 e4 sample recognition
    advance sample recognition = foldl (\current _ -> step sample current) recognition (Array.replicate stableSamples unit)
    beforeFirst = foldl (\current _ -> step c4Sample current) initialRecognition
      (Array.replicate (stableSamples - 1) unit)
    afterFirst = advance c4Sample initialRecognition
    afterRelease = foldl (\current _ -> step silence current) afterFirst (Array.replicate 5 unit)
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
      (Array.replicate 5 unit)
    wrongOctaveSecond = foldl (\current _ -> majorSeventhStep b2Sample current) majorSeventhRelease
      (Array.replicate stableSamples unit)
    correctNormalizedSecond = foldl (\current _ -> majorSeventhStep b3Sample current) majorSeventhRelease
      (Array.replicate stableSamples unit)
    writtenOctaveFirst = foldl
      (\current _ -> stepRecognition defaultRecognitionSettings WrittenOctave c4 e4 c5Sample current)
      initialRecognition
      (Array.replicate stableSamples unit)
    rawPitch time frequency = { clarity: 0.96, decibels: -20.0, frequency, time }
    observed1 = observePitch defaultCaptureSettings (rawPitch 10.0 100.0) initialObservation
    observed2 = observePitch defaultCaptureSettings (rawPitch 20.0 200.0) observed1.observation
    observed3 = observePitch defaultCaptureSettings (rawPitch 30.0 300.0) observed2.observation
    observed4 = observePitch defaultCaptureSettings (rawPitch 40.0 400.0) observed3.observation
    observedSilence = observePitch defaultCaptureSettings
      { clarity: 0.99, decibels: -80.0, frequency: 440.0, time: 400.0 }
      observed4.observation
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
