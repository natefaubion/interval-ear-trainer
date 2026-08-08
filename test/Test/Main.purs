module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import EarTrainer.Audio as Audio
import EarTrainer.Config
  ( AnswerCount(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
  , isValid
  , quizModeUsesRecognition
  , quizModeUsesSinging
  , toggleInterval
  )
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , IntervalSize(..)
  , Letter(..)
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , allMajorKeyPresets
  , allRootPitchClasses
  , intervalBetween
  , intervalNumber
  , midiNumber
  , pitch
  , pitchFromMidiLike
  , presetRange
  , transpose
  )
import EarTrainer.Quiz (Event(..), Phase(..), availableExactIntervals, availableIntervalSizes, makeChoices, makePrompt, transition)
import EarTrainer.Recognition
  ( RecognitionPhase(..)
  , defaultCaptureSettings
  , defaultRecognitionSettings
  , initialObservation
  , initialRecognition
  , nearestMidi
  , observePitch
  , relativeMidi
  , stepRecognition
  )
import EarTrainer.Settings (DecodeError(..), decodeStoredAppData)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  let
    c4 = pitch C (Accidental 0) 4
    e4 = pitch E (Accidental 0) 4
    melodicPlan = Audio.intervalPlan MelodicAscending c4 e4
    harmonicPlan = Audio.intervalPlan Harmonic c4 e4
    rootPlan = Audio.rootPlan c4
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
    rawPitch time frequency = { clarity: 0.96, decibels: -20.0, frequency, time }
    observed1 = observePitch defaultCaptureSettings (rawPitch 10.0 100.0) initialObservation
    observed2 = observePitch defaultCaptureSettings (rawPitch 20.0 200.0) observed1.observation
    observed3 = observePitch defaultCaptureSettings (rawPitch 30.0 300.0) observed2.observation
    observed4 = observePitch defaultCaptureSettings (rawPitch 40.0 400.0) observed3.observation
    observedSilence = observePitch defaultCaptureSettings
      { clarity: 0.99, decibels: -80.0, frequency: 440.0, time: 400.0 }
      observed4.observation
    stableSamples = defaultRecognitionSettings.stableFramesRequired
    step sample recognition =
      stepRecognition defaultRecognitionSettings AnyOctave c4 e4 sample recognition
    beforeFirst = foldl
      (\recognition _ -> step c4Sample recognition)
      initialRecognition
      (Array.replicate (stableSamples - 1) unit)
    afterFirst = foldl
      (\recognition _ -> step c4Sample recognition)
      initialRecognition
      (Array.replicate stableSamples unit)
    afterRelease = foldl (\recognition _ -> step silence recognition) afterFirst (Array.replicate 5 unit)
    afterSecond = foldl (\recognition _ -> step e4Sample recognition) afterRelease (Array.replicate stableSamples unit)
    wrongFirst = foldl (\recognition _ -> step e4Sample recognition) initialRecognition (Array.replicate stableSamples unit)
    wrongSecond = foldl (\recognition _ -> step b3Sample recognition) afterRelease (Array.replicate stableSamples unit)
    detunedFirst = foldl
      (\recognition _ -> step c4SharpFortyCentsSample recognition)
      initialRecognition
      (Array.replicate stableSamples unit)
    detunedSecond = foldl
      (\recognition _ -> step e4SharpFortyCentsSample recognition)
      afterRelease
      (Array.replicate stableSamples unit)
    anyOctaveFirst = foldl (\recognition _ -> step c5Sample recognition) initialRecognition (Array.replicate stableSamples unit)
    octaveBelowFirst = foldl (\recognition _ -> step c3Sample recognition) initialRecognition (Array.replicate stableSamples unit)
    majorSeventhStep sample recognition =
      stepRecognition defaultRecognitionSettings AnyOctave c4 b4 sample recognition
    majorSeventhFirst = foldl
      (\recognition _ -> majorSeventhStep c3Sample recognition)
      initialRecognition
      (Array.replicate stableSamples unit)
    majorSeventhRelease = foldl
      (\recognition _ -> majorSeventhStep silence recognition)
      majorSeventhFirst
      (Array.replicate 5 unit)
    wrongOctaveSecond = foldl
      (\recognition _ -> majorSeventhStep b2Sample recognition)
      majorSeventhRelease
      (Array.replicate stableSamples unit)
    correctNormalizedSecond = foldl
      (\recognition _ -> majorSeventhStep b3Sample recognition)
      majorSeventhRelease
      (Array.replicate stableSamples unit)
    writtenOctaveFirst = foldl
      ( \recognition _ ->
          stepRecognition defaultRecognitionSettings WrittenOctave c4 e4 c5Sample recognition
      )
      initialRecognition
      (Array.replicate stableSamples unit)
    descendingConfig = defaultConfig
      { intervalSystem = ExactIntervals
      , playbackModes = [ MelodicDescending ]
      }
    generatedPrompt = makePrompt 128 descendingConfig
    generatedChoices = makeChoices 128 descendingConfig generatedPrompt
    allAnswersConfig = descendingConfig { answerCount = AllSelected }
    allSelectedChoices = makeChoices 128 allAnswersConfig generatedPrompt
    generatedRange = presetRange descendingConfig.vocalRange
    audiationConfig = descendingConfig { quizMode = Audiation }
    audiationPrompt = makePrompt 128 audiationConfig
    ascendingAudiationPrompt = makePrompt 128 (defaultConfig { quizMode = Audiation, playbackModes = [ MelodicAscending ] })
    collectionConfig = defaultConfig
      { availableIntervals = [ SizeThird ]
      , intervalSystem = FromSelectedNotes
      , playbackModes = [ MelodicAscending ]
      }
    collectionPrompts = map (flip makePrompt collectionConfig) (Array.range 0 80)
    collectionChoices = makeChoices 24 collectionConfig (makePrompt 24 collectionConfig)
    allCollectionChoices =
      makeChoices 24
        (collectionConfig { answerCount = AllSelected })
        (makePrompt 24 collectionConfig)
    descendingCollectionConfig = collectionConfig { playbackModes = [ MelodicDescending ] }
    descendingCollectionPrompts = map (flip makePrompt descendingCollectionConfig) (Array.range 0 40)
    pitchClassOf (Pitch pitchClass _) = pitchClass
    sparseCollectionConfig = collectionConfig
      { availableIntervals = [ SizeSecond ]
      , rootPitchClasses =
          [ PitchClass C (Accidental 0)
          , PitchClass E (Accidental 0)
          , PitchClass G (Accidental 0)
          ]
      }
    legacySettings = unsafeToForeign
      { intervals: [ "major-third" ]
      , octavePolicy: "any-octave"
      , playbackModes: [ "melodic-ascending" ]
      , rootPitchClasses: [ { accidental: 0, letter: "C" } ]
      , vocalRange: "tenor"
      }
    validLegacyPreset = unsafeToForeign
      { id: "legacy-preset"
      , name: "Legacy preset"
      , settings: legacySettings
      }
    legacyStoredData = unsafeToForeign
      { activePresetId: "legacy-preset"
      , presets: [ validLegacyPreset, unsafeToForeign "invalid preset" ]
      , settings: legacySettings
      }
    decodedLegacyData = case decodeStoredAppData legacyStoredData of
      Left _ -> [ false, false, false, false ]
      Right value ->
        [ value.activePresetId == Just "legacy-preset"
        , value.config.answerCount == AFew
        , value.config.quizProgression == AutomaticProgression
        , Array.length value.presets == 1
        ]
    rejectsFutureData = case decodeStoredAppData (unsafeToForeign { version: 2 }) of
      Left (UnsupportedStoredVersion 2) -> true
      _ -> false
    rejectsMalformedData = case decodeStoredAppData (unsafeToForeign { version: 1, settings: "invalid" }) of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsMalformedVersion = case decodeStoredAppData (unsafeToForeign { version: "one" }) of
      Left (MalformedStoredData _) -> true
      _ -> false
    enharmonicConfig = defaultConfig
      { answerCount = AllSelected
      , intervalSystem = ExactIntervals
      , intervals = [ AugmentedFourth, DiminishedFifth, PerfectFourth, PerfectFifth, AugmentedFifth, MinorSixth, MajorThird ]
      , playbackModes = [ MelodicAscending ]
      }
    augmentedFourthPrompt =
      { interval: AugmentedFourth
      , mode: MelodicAscending
      , root: c4
      , target: transpose Ascending AugmentedFourth c4
      }
    diminishedFifthPrompt =
      { interval: DiminishedFifth
      , mode: MelodicAscending
      , root: c4
      , target: transpose Ascending DiminishedFifth c4
      }
    augmentedFourthChoices = makeChoices 17 enharmonicConfig augmentedFourthPrompt
    diminishedFifthChoices = makeChoices 17 enharmonicConfig diminishedFifthPrompt
    augmentedFifthPrompt =
      { interval: AugmentedFifth
      , mode: MelodicAscending
      , root: c4
      , target: transpose Ascending AugmentedFifth c4
      }
    augmentedFifthChoices = makeChoices 17 enharmonicConfig augmentedFifthPrompt
    narrowExactConfig = defaultConfig
      { customRange = { low: pitch C (Accidental 0) 4, high: pitch B (Accidental 0) 4 }
      , intervalSystem = ExactIntervals
      , intervals = [ MajorSeventh, PerfectOctave ]
      , playbackModes = [ MelodicAscending ]
      , rootPitchClasses = [ PitchClass C (Accidental 0) ]
      , vocalRange = Custom
      }
    narrowExactPrompt = makePrompt 0 narrowExactConfig
  assertEqual
    { actual: nearestMidi 440.0
    , expected: 69
    }
  assertEqual
    { actual: beforeFirst.phase
    , expected: WaitingForFirst
    }
  assertEqual
    { actual: afterFirst.phase
    , expected: WaitingForRelease
    }
  assertEqual
    { actual: afterRelease.phase
    , expected: WaitingForSecond
    }
  assertEqual
    { actual: afterSecond.phase
    , expected: RecognitionComplete
    }
  assertEqual
    { actual: [ wrongFirst.phase, wrongSecond.phase ]
    , expected: [ RecognitionIncorrect, RecognitionIncorrect ]
    }
  assertEqual
    { actual: [ detunedFirst.phase, detunedSecond.phase ]
    , expected: [ WaitingForFirst, WaitingForSecond ]
    }
  assertEqual
    { actual: anyOctaveFirst.phase
    , expected: WaitingForRelease
    }
  assertEqual
    { actual: writtenOctaveFirst.phase
    , expected: RecognitionIncorrect
    }
  assertEqual
    { actual: relativeMidi AnyOctave c4 octaveBelowFirst 57
    , expected: 69
    }
  assertEqual
    { actual: relativeMidi WrittenOctave c4 octaveBelowFirst 57
    , expected: 57
    }
  assertEqual
    { actual: wrongOctaveSecond.phase
    , expected: RecognitionIncorrect
    }
  assertEqual
    { actual: correctNormalizedSecond.phase
    , expected: RecognitionComplete
    }
  assertEqual
    { actual: Array.length generatedChoices
    , expected: 4
    }
  assertEqual
    { actual:
        [ Array.elem MajorSeventh (availableExactIntervals narrowExactConfig)
        , Array.elem PerfectOctave (availableExactIntervals narrowExactConfig)
        , narrowExactPrompt.interval == MajorSeventh
        , midiNumber narrowExactPrompt.root >= midiNumber narrowExactConfig.customRange.low
        , midiNumber narrowExactPrompt.target <= midiNumber narrowExactConfig.customRange.high
        ]
    , expected: [ true, false, true, true, true ]
    }
  assertEqual
    { actual:
        [ Array.elem AugmentedFourth (map _.interval augmentedFourthChoices)
        , Array.elem DiminishedFifth (map _.interval augmentedFourthChoices)
        , Array.length (Array.nub (map (\choice -> midiNumber choice.target) augmentedFourthChoices))
            == Array.length augmentedFourthChoices
        , Array.elem DiminishedFifth (map _.interval diminishedFifthChoices)
        , Array.elem AugmentedFourth (map _.interval diminishedFifthChoices)
        , Array.length (Array.nub (map (\choice -> midiNumber choice.target) diminishedFifthChoices))
            == Array.length diminishedFifthChoices
        , Array.elem AugmentedFifth (map _.interval augmentedFifthChoices)
        , Array.elem MinorSixth (map _.interval augmentedFifthChoices)
        , Array.length (Array.nub (map (\choice -> midiNumber choice.target) augmentedFifthChoices))
            == Array.length augmentedFifthChoices
        ]
    , expected: [ true, false, true, true, false, true, true, false, true ]
    }
  assertEqual
    { actual: Array.length (Array.filter (\choice -> choice.interval == generatedPrompt.interval) generatedChoices)
    , expected: 1
    }
  assertEqual
    { actual: Array.length allSelectedChoices
    , expected: Array.length allAnswersConfig.intervals
    }
  assertEqual
    { actual:
        Array.all
          (\interval -> Array.elem interval (map _.interval allSelectedChoices))
          allAnswersConfig.intervals
    , expected: true
    }
  assertEqual
    { actual:
        [ Array.all (\prompt -> midiNumber prompt.target < midiNumber prompt.root) descendingCollectionPrompts
        , Array.length collectionChoices == 2
        , Array.length
            (Array.filter (\choice -> choice.interval == (makePrompt 24 collectionConfig).interval) collectionChoices) == 1
        , Array.all (\choice -> intervalNumber choice.interval == 3) allCollectionChoices
        ]
    , expected: [ true, true, true, true ]
    }
  assertEqual
    { actual:
        midiNumber generatedPrompt.root >= midiNumber generatedRange.low
          && midiNumber generatedPrompt.target >= midiNumber generatedRange.low
          && midiNumber generatedPrompt.root <= midiNumber generatedRange.high
          && midiNumber generatedPrompt.target <= midiNumber generatedRange.high
    , expected: true
    }
  assertEqual
    { actual: midiNumber (pitch A (Accidental 0) 4)
    , expected: 69
    }
  assertEqual
    { actual: transpose Ascending MajorThird (pitch C (Accidental 1) 4)
    , expected: pitch E (Accidental 1) 4
    }
  assertEqual
    { actual: transpose Descending MajorSecond (pitch C (Accidental 0) 4)
    , expected: pitch B (Accidental (-1)) 3
    }
  assertEqual
    { actual: transpose Ascending AugmentedFourth (pitch C (Accidental 0) 4)
    , expected: pitch F (Accidental 1) 4
    }
  assertEqual
    { actual: transpose Ascending DiminishedFifth (pitch C (Accidental 0) 4)
    , expected: pitch G (Accidental (-1)) 4
    }
  assertEqual
    { actual: transpose Ascending AugmentedFifth (pitch C (Accidental 0) 4)
    , expected: pitch G (Accidental 1) 4
    }
  assertEqual
    { actual: Array.elem (PitchClass D (Accidental (-1))) allRootPitchClasses
    , expected: true
    }
  assertEqual
    { actual:
        [ Array.elem (PitchClass C (Accidental (-1))) allRootPitchClasses
        , Array.elem (PitchClass F (Accidental (-1))) allRootPitchClasses
        , Array.elem (PitchClass E (Accidental 1)) allRootPitchClasses
        , Array.elem (PitchClass B (Accidental 1)) allRootPitchClasses
        , Array.length allMajorKeyPresets == 15
        ]
    , expected: [ true, true, true, true, true ]
    }
  assertEqual
    { actual: presetRange ExtraWide
    , expected: { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
    }
  assertEqual
    { actual:
        [ quizModeUsesSinging SingingOnly
        , quizModeUsesRecognition SingingOnly
        , quizModeUsesSinging RecognitionOnly
        , quizModeUsesRecognition RecognitionOnly
        , quizModeUsesSinging SingingAndRecognition
        , quizModeUsesRecognition SingingAndRecognition
        , quizModeUsesSinging Audiation
        , quizModeUsesRecognition Audiation
        ]
    , expected: [ true, false, false, true, true, true, true, false ]
    }
  assertEqual
    { actual: defaultConfig.quizProgression == AutomaticProgression
    , expected: true
    }
  assertEqual
    { actual:
        Array.all
          ( \prompt ->
              intervalNumber prompt.interval == 3
                && Array.elem (pitchClassOf prompt.root) collectionConfig.rootPitchClasses
                && Array.elem (pitchClassOf prompt.target) collectionConfig.rootPitchClasses
          )
          collectionPrompts
    , expected: true
    }
  assertEqual
    { actual:
        [ intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch E (Accidental 0) 4) == Just MajorThird
        , intervalBetween Ascending (pitch D (Accidental 0) 4) (pitch F (Accidental 0) 4) == Just MinorThird
        , intervalBetween Ascending (pitch F (Accidental 0) 4) (pitch B (Accidental 0) 4) == Just AugmentedFourth
        , intervalBetween Ascending (pitch B (Accidental 0) 4) (pitch F (Accidental 0) 5) == Just DiminishedFifth
        , intervalBetween Ascending (pitch C (Accidental 0) 4) (pitch G (Accidental 1) 4) == Just AugmentedFifth
        ]
    , expected: [ true, true, true, true, true ]
    }
  assertEqual
    { actual:
        [ Array.elem SizeThird (availableIntervalSizes collectionConfig)
        , Array.elem SizeSecond (availableIntervalSizes sparseCollectionConfig)
        ]
    , expected: [ true, false ]
    }
  assertEqual
    { actual:
        [ midiNumber audiationPrompt.target < midiNumber audiationPrompt.root
        , midiNumber ascendingAudiationPrompt.target > midiNumber ascendingAudiationPrompt.root
        , isValid (defaultConfig { quizMode = Audiation, playbackModes = [ Harmonic ] })
        ]
    , expected: [ true, true, false ]
    }
  assertEqual
    { actual: transpose Ascending MinorThird (pitch D (Accidental (-1)) 4)
    , expected: pitch F (Accidental (-1)) 4
    }
  assertEqual
    { actual: pitchFromMidiLike (pitch D (Accidental (-1)) 4) 61
    , expected: pitch D (Accidental (-1)) 4
    }
  assertEqual
    { actual: pitchFromMidiLike (pitch C (Accidental 1) 4) 61
    , expected: pitch C (Accidental 1) 4
    }
  assertEqual
    { actual: isValid (toggleInterval MinorThird defaultConfig)
    , expected: true
    }
  assertEqual
    { actual: transition SingingFirstNote FirstPitchAccepted
    , expected: Just AwaitingRearticulation
    }
  assertEqual
    { actual: transition AwaitingRearticulation SecondPitchAccepted
    , expected: Nothing
    }
  assertEqual
    { actual: transition AwaitingRearticulation VoiceReleased
    , expected: Just SingingSecondNote
    }
  assertEqual
    { actual:
        [ transition SingingFirstNote PitchRejected
        , transition SingingSecondNote PitchRejected
        , transition IntervalError Continue
        ]
    , expected: [ Just IntervalError, Just IntervalError, Just PlayingInterval ]
    }
  assertEqual
    { actual: decodedLegacyData <> [ rejectsFutureData, rejectsMalformedData, rejectsMalformedVersion ]
    , expected: [ true, true, true, true, true, true, true ]
    }
  assertEqual
    { actual:
        [ melodicPlan.durationMilliseconds == 1450.0
        , map _.startMilliseconds melodicPlan.events == [ 0.0, 800.0 ]
        , map _.notes melodicPlan.events == [ [ 60 ], [ 64 ] ]
        , harmonicPlan.durationMilliseconds == 900.0
        , map _.notes harmonicPlan.events == [ [ 60, 64 ] ]
        , map _.notes rootPlan.events == [ [ 60 ] ]
        ]
    , expected: [ true, true, true, true, true, true ]
    }
  assertEqual
    { actual:
        [ observed1.sample == Nothing
        , observed4.sample == Just { clarity: 0.96, frequency: 250.0 }
        , observedSilence.sample == Just { clarity: 0.0, frequency: 0.0 }
        ]
    , expected: [ true, true, true ]
    }
