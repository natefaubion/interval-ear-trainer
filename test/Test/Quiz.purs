module Test.Quiz (run) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord (abs)
import EarTrainer.Config
  ( AnswerCount(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , RangeBoundary(..)
  , defaultConfig
  , isValid
  , quizModeUsesRecognition
  , quizModeUsesSinging
  , selectMajorKey
  , selectedMajorKeyId
  , setCustomPitchClass
  , setCustomPitchOctave
  , toggleInterval
  )
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , IntervalSize(..)
  , Letter(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , intervalNumber
  , midiNumber
  , pitch
  , presetRange
  , transpose
  )
import EarTrainer.Quiz
  ( PromptMode(..)
  , availableExactIntervals
  , availableIntervalSizes
  , isPlayable
  , makeChoices
  , makePrompt
  , melodyPitches
  , promptSet
  )
import Effect (Effect)
import Effect.Exception (throw)
import Test.Assert (assertEqual, assertFalse', assertTrue')

run :: Effect Unit
run = do
  defaultPrompts <- case promptSet defaultConfig of
    Nothing -> throw "Default configuration must produce prompts."
    Just prompts -> pure prompts
  let
    promptsFor config = fromMaybe defaultPrompts (promptSet config)
    c4 = pitch C (Accidental 0) 4
    makeIntervalPrompt seed prompts = case makePrompt seed prompts of
      IntervalPrompt prompt -> prompt
      MelodyPromptMode _ -> { interval: PerfectUnison, mode: MelodicAscending, root: c4, target: c4 }
    narrowConfig = defaultConfig
      { customRange = { low: c4, high: c4 }
      , intervalSystem = ExactIntervals
      , intervals = [ MajorThird ]
      , rootPitchClasses = [ PitchClass C (Accidental 0) ]
      , vocalRange = Custom
      }
    selectedGMajor = selectMajorKey "g" defaultConfig
    classAdjustedRange = setCustomPitchClass Lowest 1 defaultConfig
    octaveAdjustedRange = setCustomPitchOctave Highest 6 defaultConfig
    descendingConfig = defaultConfig
      { intervalSystem = ExactIntervals
      , playbackModes = [ MelodicDescending ]
      }
    generatedPrompt = makeIntervalPrompt 128 (promptsFor descendingConfig)
    generatedChoices = makeChoices 128 descendingConfig generatedPrompt
    allAnswersConfig = descendingConfig { answerCount = AllSelected }
    allSelectedChoices = makeChoices 128 allAnswersConfig generatedPrompt
    generatedRange = presetRange descendingConfig.vocalRange
    audiationConfig = descendingConfig { quizMode = Audiation }
    audiationPrompt = makeIntervalPrompt 128 (promptsFor audiationConfig)
    ascendingAudiationPrompt = makeIntervalPrompt 128
      (promptsFor (defaultConfig { quizMode = Audiation, playbackModes = [ MelodicAscending ] }))
    collectionConfig = defaultConfig
      { availableIntervals = [ SizeThird ]
      , intervalSystem = FromSelectedNotes
      , playbackModes = [ MelodicAscending ]
      }
    collectionPrompts = map (flip makeIntervalPrompt (promptsFor collectionConfig)) (Array.range 0 80)
    collectionChoices = makeChoices 24 collectionConfig (makeIntervalPrompt 24 (promptsFor collectionConfig))
    allCollectionChoices = makeChoices 24
      (collectionConfig { answerCount = AllSelected })
      (makeIntervalPrompt 24 (promptsFor collectionConfig))
    descendingCollectionConfig = collectionConfig { playbackModes = [ MelodicDescending ] }
    descendingCollectionPrompts = map (flip makeIntervalPrompt (promptsFor descendingCollectionConfig)) (Array.range 0 40)
    pitchClassOf (Pitch pitchClass _) = pitchClass
    sparseCollectionConfig = collectionConfig
      { availableIntervals = [ SizeSecond ]
      , rootPitchClasses =
          [ PitchClass C (Accidental 0)
          , PitchClass E (Accidental 0)
          , PitchClass G (Accidental 0)
          ]
      }
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
    augmentedFifthPrompt =
      { interval: AugmentedFifth
      , mode: MelodicAscending
      , root: c4
      , target: transpose Ascending AugmentedFifth c4
      }
    augmentedFourthChoices = makeChoices 17 enharmonicConfig augmentedFourthPrompt
    diminishedFifthChoices = makeChoices 17 enharmonicConfig diminishedFifthPrompt
    augmentedFifthChoices = makeChoices 17 enharmonicConfig augmentedFifthPrompt
    narrowExactConfig = defaultConfig
      { customRange = { low: pitch C (Accidental 0) 4, high: pitch B (Accidental 0) 4 }
      , intervalSystem = ExactIntervals
      , intervals = [ MajorSeventh, PerfectOctave ]
      , playbackModes = [ MelodicAscending ]
      , rootPitchClasses = [ PitchClass C (Accidental 0) ]
      , vocalRange = Custom
      }
    narrowExactPrompt = makeIntervalPrompt 0 (promptsFor narrowExactConfig)
    melodyConfig = defaultConfig { quizMode = MelodyImitation, playbackModes = [] }
    melody = case promptSet melodyConfig of
      Nothing -> []
      Just prompts -> case makePrompt 42 prompts of
        MelodyPromptMode prompt -> NonEmptyArray.toArray (melodyPitches prompt)
        IntervalPrompt _ -> []
    exactMelodyConfig = defaultConfig
      { intervalSystem = ExactIntervals
      , intervals = [ MajorThird ]
      , playbackModes = []
      , quizMode = MelodyImitation
      , rootPitchClasses = [ PitchClass C (Accidental 0) ]
      }
    exactMelody = case promptSet exactMelodyConfig of
      Nothing -> []
      Just prompts -> case makePrompt 73 prompts of
        MelodyPromptMode prompt -> NonEmptyArray.toArray (melodyPitches prompt)
        IntervalPrompt _ -> []
  assertEqual { actual: Array.length generatedChoices, expected: 4 }
  assertTrue' "major seventh fits the narrow range" (Array.elem MajorSeventh (availableExactIntervals narrowExactConfig))
  assertFalse' "octave does not fit the narrow range" (Array.elem PerfectOctave (availableExactIntervals narrowExactConfig))
  assertTrue' "narrow prompt is a major seventh" (narrowExactPrompt.interval == MajorSeventh)
  assertTrue' "generated root is in range" (midiNumber narrowExactPrompt.root >= midiNumber narrowExactConfig.customRange.low)
  assertTrue' "generated target is in range" (midiNumber narrowExactPrompt.target <= midiNumber narrowExactConfig.customRange.high)
  assertTrue' "augmented fourth is offered" (Array.elem AugmentedFourth (map _.interval augmentedFourthChoices))
  assertFalse' "enharmonic duplicate diminished fifth is omitted"
    (Array.elem DiminishedFifth (map _.interval augmentedFourthChoices))
  assertEqual
    { actual: Array.length (Array.nub (map (midiNumber <<< _.target) augmentedFourthChoices))
    , expected: Array.length augmentedFourthChoices
    }
  assertTrue' "diminished fifth is offered" (Array.elem DiminishedFifth (map _.interval diminishedFifthChoices))
  assertFalse' "enharmonic duplicate augmented fourth is omitted"
    (Array.elem AugmentedFourth (map _.interval diminishedFifthChoices))
  assertEqual
    { actual: Array.length (Array.nub (map (midiNumber <<< _.target) diminishedFifthChoices))
    , expected: Array.length diminishedFifthChoices
    }
  assertTrue' "augmented fifth is offered" (Array.elem AugmentedFifth (map _.interval augmentedFifthChoices))
  assertFalse' "enharmonic duplicate minor sixth is omitted" (Array.elem MinorSixth (map _.interval augmentedFifthChoices))
  assertEqual
    { actual: Array.length (Array.nub (map (midiNumber <<< _.target) augmentedFifthChoices))
    , expected: Array.length augmentedFifthChoices
    }
  assertEqual
    { actual: Array.length (Array.filter (\choice -> choice.interval == generatedPrompt.interval) generatedChoices)
    , expected: 1
    }
  assertEqual { actual: Array.length allSelectedChoices, expected: Array.length allAnswersConfig.intervals }
  assertTrue' "all selected intervals are offered"
    (Array.all (\interval -> Array.elem interval (map _.interval allSelectedChoices)) allAnswersConfig.intervals)
  assertTrue' "descending prompts descend"
    (Array.all (\prompt -> midiNumber prompt.target < midiNumber prompt.root) descendingCollectionPrompts)
  assertEqual { actual: Array.length collectionChoices, expected: 2 }
  assertEqual
    { actual: Array.length $ Array.filter
        (\choice -> choice.interval == (makeIntervalPrompt 24 (promptsFor collectionConfig)).interval)
        collectionChoices
    , expected: 1
    }
  assertTrue' "collection choices are thirds" (Array.all (\choice -> intervalNumber choice.interval == 3) allCollectionChoices)
  assertTrue' "generated root is above range floor" (midiNumber generatedPrompt.root >= midiNumber generatedRange.low)
  assertTrue' "generated target is above range floor" (midiNumber generatedPrompt.target >= midiNumber generatedRange.low)
  assertTrue' "generated root is below range ceiling" (midiNumber generatedPrompt.root <= midiNumber generatedRange.high)
  assertTrue' "generated target is below range ceiling" (midiNumber generatedPrompt.target <= midiNumber generatedRange.high)
  assertTrue' "singing mode uses singing" (quizModeUsesSinging SingingOnly)
  assertFalse' "singing mode does not use recognition" (quizModeUsesRecognition SingingOnly)
  assertFalse' "recognition mode does not use singing" (quizModeUsesSinging RecognitionOnly)
  assertTrue' "recognition mode uses recognition" (quizModeUsesRecognition RecognitionOnly)
  assertTrue' "combined mode uses singing" (quizModeUsesSinging SingingAndRecognition)
  assertTrue' "combined mode uses recognition" (quizModeUsesRecognition SingingAndRecognition)
  assertTrue' "audiation uses singing" (quizModeUsesSinging Audiation)
  assertFalse' "audiation does not use recognition" (quizModeUsesRecognition Audiation)
  assertTrue' "melody imitation uses singing" (quizModeUsesSinging MelodyImitation)
  assertFalse' "melody imitation does not use interval recognition" (quizModeUsesRecognition MelodyImitation)
  assertTrue' "automatic progression is the default" (defaultConfig.quizProgression == AutomaticProgression)
  assertTrue' "collection prompts use selected thirds and pitch classes" $ Array.all
    ( \prompt ->
        intervalNumber prompt.interval == 3
          && Array.elem (pitchClassOf prompt.root) collectionConfig.rootPitchClasses
          && Array.elem (pitchClassOf prompt.target) collectionConfig.rootPitchClasses
    )
    collectionPrompts
  assertTrue' "selected interval size is available" (Array.elem SizeThird (availableIntervalSizes collectionConfig))
  assertFalse' "unplayable interval size is unavailable" (Array.elem SizeSecond (availableIntervalSizes sparseCollectionConfig))
  assertTrue' "descending audiation descends" (midiNumber audiationPrompt.target < midiNumber audiationPrompt.root)
  assertTrue' "ascending audiation ascends" (midiNumber ascendingAudiationPrompt.target > midiNumber ascendingAudiationPrompt.root)
  assertFalse' "harmonic-only audiation is invalid" (isValid (defaultConfig { quizMode = Audiation, playbackModes = [ Harmonic ] }))
  assertTrue' "removing one default interval remains valid" (isValid (toggleInterval MinorThird defaultConfig))
  assertEqual { actual: selectedMajorKeyId defaultConfig.rootPitchClasses, expected: "c" }
  assertEqual { actual: selectedMajorKeyId selectedGMajor.rootPitchClasses, expected: "g" }
  assertEqual { actual: midiNumber classAdjustedRange.customRange.low, expected: 49 }
  assertEqual { actual: midiNumber octaveAdjustedRange.customRange.high, expected: 91 }
  assertTrue' "narrow configuration is structurally valid" (isValid narrowConfig)
  assertFalse' "narrow configuration has no playable prompt" (isPlayable narrowConfig)
  assertTrue' "descending configuration has prompts" (isJust (promptSet descendingConfig))
  assertTrue' "audiation configuration has prompts" (isJust (promptSet audiationConfig))
  assertTrue' "collection configuration has prompts" (isJust (promptSet collectionConfig))
  assertTrue' "descending collection has prompts" (isJust (promptSet descendingCollectionConfig))
  assertTrue' "narrow exact configuration has prompts" (isJust (promptSet narrowExactConfig))
  assertTrue' "melody mode ignores interval playback orientations" (isPlayable melodyConfig)
  assertEqual { actual: Array.length melody, expected: 4 }
  assertTrue' "melody pitches stay in the configured range"
    ( Array.all
        ( \note ->
            midiNumber note >= midiNumber (presetRange melodyConfig.vocalRange).low
              && midiNumber note <= midiNumber (presetRange melodyConfig.vocalRange).high
        )
        melody
    )
  assertEqual { actual: Array.length exactMelody, expected: 4 }
  assertTrue' "exact melodies use the selected interval between every adjacent pair"
    ( Array.all identity
        ( Array.zipWith
            (\left right -> abs (midiNumber right - midiNumber left) == 4)
            exactMelody
            (Array.drop 1 exactMelody)
        )
    )
