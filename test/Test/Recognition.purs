module Test.Recognition (run) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import EarTrainer.Music (Accidental(..), Letter(..), OctavePolicy(..), pitch)
import EarTrainer.Recognition
  ( CapturePhase(..)
  , PitchExpectation(..)
  , PitchObservation(..)
  , RecognitionPhase(..)
  , SequencePhase(..)
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , initialObservation
  , initialRecognition
  , initialSequenceRecognition
  , intervalPitchExpectation
  , nearestMidi
  , observePitch
  , phase
  , relativeMidi
  , selectPitchCandidate
  , sequenceAcceptedCount
  , sequencePhase
  , sequencePitchExpectation
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
    pitchSample frequency = { frequency, clarity: 0.98, time: 0.0 }
    at time sample = sample { time = time }
    c4Sample = pitchSample 261.625565
    c3Sample = pitchSample 130.812783
    c5Sample = pitchSample 523.251131
    c4SharpFortyCentsSample = pitchSample 267.744
    e4Sample = pitchSample 329.627557
    e3Sample = pitchSample 164.813778
    d4Sample = pitchSample 293.664768
    d3Sample = pitchSample 146.832384
    e4SharpFortyCentsSample = pitchSample 337.337
    b2Sample = pitchSample 123.470825
    b3Sample = pitchSample 246.941651
    silence = { frequency: 0.0, clarity: 0.0, time: 0.0 }
    observation sample =
      if sample.frequency <= 0.0 then ArticulationBreak sample.time
      else ObservedPitch sample
    testSettings = defaultRecognitionSettings
      { maximumObservationGapMilliseconds = 20.0
      , releaseMillisecondsRequired = 10.0
      , stableMillisecondsRequired = 20.0
      }
    stableTimes start = [ start, start + 10.0, start + 20.0 ]
    releaseTimes start = [ start, start + 10.0 ]
    step sample recognition = stepRecognition testSettings AnyOctave c4 e4 (observation sample) recognition
    advanceAt start sample recognition = foldl (\current time -> step (at time sample) current) recognition
      (stableTimes start)
    releaseAt start recognition = foldl (\current time -> step (at time silence) current) recognition
      (releaseTimes start)
    advance = advanceAt 0.0
    beforeFirst = foldl (\current time -> step (at time c4Sample) current) initialRecognition [ 0.0, 10.0 ]
    afterFirst = advance c4Sample initialRecognition
    afterRelease = releaseAt 30.0 afterFirst
    afterSecond = advanceAt 50.0 e4Sample afterRelease
    wrongFirst = advance e4Sample initialRecognition
    wrongSecond = advance b3Sample afterRelease
    detunedFirst = advance c4SharpFortyCentsSample initialRecognition
    detunedSecond = advance e4SharpFortyCentsSample afterRelease
    anyOctaveFirst = advance c5Sample initialRecognition
    octaveBelowFirst = advance c3Sample initialRecognition
    majorSeventhStep sample recognition = stepRecognition testSettings AnyOctave c4 b4 (observation sample) recognition
    majorSeventhAdvance start sample recognition = foldl
      (\current time -> majorSeventhStep (at time sample) current)
      recognition
      (stableTimes start)
    majorSeventhFirst = majorSeventhAdvance 0.0 c3Sample initialRecognition
    majorSeventhRelease = foldl (\current time -> majorSeventhStep (at time silence) current)
      majorSeventhFirst
      (releaseTimes 30.0)
    wrongOctaveSecond = majorSeventhAdvance 50.0 b2Sample majorSeventhRelease
    correctNormalizedSecond = majorSeventhAdvance 50.0 b3Sample majorSeventhRelease
    writtenOctaveFirst = foldl
      (\current time -> stepRecognition testSettings WrittenOctave c4 e4 (ObservedPitch (at time c5Sample)) current)
      initialRecognition
      (stableTimes 0.0)
    sequenceStep sample recognition = stepSequenceRecognition
      testSettings
      AnyOctave
      (nonEmpty [ c4, e4, d4 ])
      (observation sample)
      recognition
    sequenceAdvance start sample recognition = foldl (\current time -> sequenceStep (at time sample) current) recognition
      (stableTimes start)
    sequenceRelease start recognition = foldl (\current time -> sequenceStep (at time silence) current) recognition
      (releaseTimes start)
    sequenceFirst = sequenceAdvance 0.0 c4Sample initialSequenceRecognition
    sequenceFirstRelease = sequenceRelease 30.0 sequenceFirst
    sequenceSecond = sequenceAdvance 50.0 e4Sample sequenceFirstRelease
    sequenceSecondRelease = sequenceRelease 80.0 sequenceSecond
    sequenceComplete = sequenceAdvance 100.0 d4Sample sequenceSecondRelease
    sequenceContinuedFirst = sequenceStep (at 30.0 c4Sample) sequenceFirst
    sequenceWrongMiddle = sequenceAdvance 50.0 b3Sample sequenceFirstRelease
    sequenceWrongFinal = sequenceAdvance 100.0 b3Sample sequenceSecondRelease
    sequencePartial = foldl (\current time -> sequenceStep (at time c4Sample) current) initialSequenceRecognition
      [ 0.0, 10.0 ]
    sequencePartialSilence = sequenceStep (at 20.0 silence) sequencePartial
    sequenceAfterInterruptedPitch = foldl (\current time -> sequenceStep (at time c4Sample) current)
      sequencePartialSilence
      [ 30.0, 40.0 ]
    repeatedStep sample recognition = stepSequenceRecognition
      testSettings
      AnyOctave
      (nonEmpty [ c4, c4 ])
      (observation sample)
      recognition
    repeatedAdvance start sample recognition = foldl (\current time -> repeatedStep (at time sample) current) recognition
      (stableTimes start)
    repeatedFirst = repeatedAdvance 0.0 c4Sample initialSequenceRecognition
    repeatedContinued = repeatedStep (at 30.0 c4Sample) repeatedFirst
    repeatedRelease = foldl (\current time -> repeatedStep (at time silence) current) repeatedFirst
      (releaseTimes 30.0)
    repeatedComplete = repeatedAdvance 50.0 c4Sample repeatedRelease
    writtenSequenceFirst = foldl
      ( \current time -> stepSequenceRecognition
          testSettings
          WrittenOctave
          (nonEmpty [ c4, e4 ])
          (ObservedPitch (at time c5Sample))
          current
      )
      initialSequenceRecognition
      (stableTimes 0.0)
    octaveSequenceStep sample recognition = stepSequenceRecognition
      testSettings
      AnyOctave
      (nonEmpty [ c4, e4, d4 ])
      (observation sample)
      recognition
    octaveAdvance start sample recognition = foldl
      (\current time -> octaveSequenceStep (at time sample) current)
      recognition
      (stableTimes start)
    octaveRelease start recognition = foldl
      (\current time -> octaveSequenceStep (at time silence) current)
      recognition
      (releaseTimes start)
    octaveFirst = octaveAdvance 0.0 c3Sample initialSequenceRecognition
    octaveFirstRelease = octaveRelease 30.0 octaveFirst
    octaveSecond = octaveAdvance 50.0 e3Sample octaveFirstRelease
    octaveSecondRelease = octaveRelease 80.0 octaveSecond
    octaveComplete = octaveAdvance 100.0 d3Sample octaveSecondRelease
    octaveShiftedSecond = octaveAdvance 50.0 e4Sample octaveFirstRelease
    completeRepeatedSequence count = do
      let
        expected = nonEmpty (Array.replicate count c4)
        stepExpected sample recognition = stepSequenceRecognition
          testSettings
          AnyOctave
          expected
          (observation sample)
          recognition
        singNote index recognition = foldl
          (\current time -> stepExpected (at time c4Sample) current)
          recognition
          (stableTimes (Int.toNumber index * 50.0))
        releaseNote index recognition = foldl
          (\current time -> stepExpected (at time silence) current)
          recognition
          (releaseTimes (Int.toNumber index * 50.0 + 30.0))
      foldl
        ( \recognition index -> do
            let accepted = singNote index recognition
            if index == count - 1 then accepted else releaseNote index accepted
        )
        initialSequenceRecognition
        (Array.range 0 (count - 1))
    sequenceLengths = map (sequencePhase <<< completeRepeatedSequence) (Array.range 3 8)
    acceptedAtRate interval count = foldl
      ( \current index -> stepRecognition
          defaultRecognitionSettings
          AnyOctave
          c4
          e4
          (ObservedPitch (at (Int.toNumber index * interval) c4Sample))
          current
      )
      initialRecognition
      (Array.range 0 count)
    acceptedAt30Hz = acceptedAtRate (1000.0 / 30.0) 4
    acceptedAt60Hz = acceptedAtRate (1000.0 / 60.0) 8
    acceptedAt120Hz = acceptedAtRate (1000.0 / 120.0) 15
    afterSchedulingGap = foldl
      ( \current time -> stepRecognition
          defaultRecognitionSettings
          AnyOctave
          c4
          e4
          (ObservedPitch (at time c4Sample))
          current
      )
      initialRecognition
      [ 0.0, 1000.0 ]
    rawPitch time frequency =
      { candidates: [ { clarity: 0.96, frequency, windowSize: 2048 } ]
      , decibels: -20.0
      , time
      }
    expectedC = OctaveEquivalentPitch 60
    observe = observePitch defaultCaptureSettings DetectingPitch expectedC
    observed1 = observe (rawPitch 10.0 100.0) initialObservation
    observed2 = observe (rawPitch 20.0 200.0) observed1.observation
    observed3 = observe (rawPitch 30.0 300.0) observed2.observation
    observed4 = observe (rawPitch 40.0 400.0) observed3.observation
    observedSilence = observe
      { candidates: [ { clarity: 0.99, frequency: 440.0, windowSize: 2048 } ]
      , decibels: -80.0
      , time: 400.0
      }
      observed4.observation
    observedConsonant = observe
      { candidates: [ { clarity: 0.2, frequency: 180.0, windowSize: 2048 } ]
      , decibels: -18.0
      , time: 76.0
      }
      observed4.observation
    observedBriefDrop = observe
      { candidates: [ { clarity: 0.2, frequency: 180.0, windowSize: 2048 } ]
      , decibels: -18.0
      , time: 60.0
      }
      observed4.observation
    observedNew1 = observe (rawPitch 80.0 500.0) observedConsonant.observation
    observedNew2 = observe (rawPitch 90.0 500.0) observedNew1.observation
    observedNew3 = observe (rawPitch 100.0 500.0) observedNew2.observation
    observedNew4 = observe (rawPitch 110.0 500.0) observedNew3.observation
    multiWindow1 = observe
      { candidates:
          [ { clarity: 0.91, frequency: 220.0, windowSize: 2048 }
          , { clarity: 0.98, frequency: 110.0, windowSize: 8192 }
          ]
      , decibels: -20.0
      , time: 10.0
      }
      initialObservation
    multiWindow2 = observe
      ((rawPitch 20.0 110.0) { candidates = [ { clarity: 0.98, frequency: 110.0, windowSize: 8192 } ] })
      multiWindow1.observation
    multiWindow3 = observe
      ((rawPitch 30.0 110.0) { candidates = [ { clarity: 0.98, frequency: 110.0, windowSize: 8192 } ] })
      multiWindow2.observation
    multiWindow4 = observe
      ((rawPitch 40.0 110.0) { candidates = [ { clarity: 0.98, frequency: 110.0, windowSize: 8192 } ] })
      multiWindow3.observation
    lowFundamental = { clarity: 0.82, frequency: 130.812783, windowSize: 8192 }
    octaveHarmonic = { clarity: 0.99, frequency: 261.625565, windowSize: 2048 }
    expectedLowCandidate = selectPitchCandidate defaultCaptureSettings
      (ExactPitch 48)
      [ octaveHarmonic, lowFundamental ]
    octaveEquivalentCandidate = selectPitchCandidate defaultCaptureSettings
      (OctaveEquivalentPitch 48)
      [ octaveHarmonic, lowFundamental ]
    beforeOnset = observePitch defaultCaptureSettings DetectingPitch expectedC
      ((rawPitch 100.0 261.625565) { decibels = -42.0 })
      initialObservation
    detectedOnset = observePitch defaultCaptureSettings AwaitingArticulation expectedC
      ((rawPitch 200.0 261.625565) { decibels = -20.0 })
      beforeOnset.observation
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
  assertEqual { actual: observed1.event, expected: NoEvidence }
  assertEqual { actual: observed4.event, expected: ObservedPitch { clarity: 0.96, frequency: 250.0, time: 40.0 } }
  assertEqual { actual: observedSilence.event, expected: ArticulationBreak 400.0 }
  assertEqual { actual: observedConsonant.event, expected: ArticulationBreak 76.0 }
  assertEqual { actual: observedBriefDrop.event, expected: ObservedPitch { clarity: 0.96, frequency: 250.0, time: 60.0 } }
  assertEqual { actual: observedNew1.event, expected: NoEvidence }
  assertEqual { actual: observedNew2.event, expected: ObservedPitch { clarity: 0.96, frequency: 500.0, time: 90.0 } }
  assertEqual { actual: observedNew4.event, expected: ObservedPitch { clarity: 0.96, frequency: 500.0, time: 110.0 } }
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
  assertEqual { actual: phase acceptedAt30Hz, expected: WaitingForRelease }
  assertEqual { actual: phase acceptedAt60Hz, expected: WaitingForRelease }
  assertEqual { actual: phase acceptedAt120Hz, expected: WaitingForRelease }
  assertEqual { actual: phase afterSchedulingGap, expected: WaitingForFirst }
  assertEqual
    { actual: multiWindow4.event
    , expected: ObservedPitch { clarity: 0.98, frequency: 110.0, time: 40.0 }
    }
  assertEqual { actual: expectedLowCandidate, expected: Just lowFundamental }
  assertEqual { actual: octaveEquivalentCandidate, expected: Just octaveHarmonic }
  assertEqual { actual: detectedOnset.event, expected: ArticulationBreak 200.0 }
  assertEqual
    { actual: intervalPitchExpectation AnyOctave c4 e4 initialRecognition
    , expected: OctaveEquivalentPitch 60
    }
  assertEqual
    { actual: intervalPitchExpectation AnyOctave c4 e4 afterFirst
    , expected: ExactPitch 64
    }
  assertEqual
    { actual: sequencePitchExpectation AnyOctave (nonEmpty [ c4, e4, d4 ]) sequenceFirst
    , expected: ExactPitch 64
    }
