module Test.Quiz (run) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
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
  ( Event(..)
  , Phase(..)
  , availableExactIntervals
  , availableIntervalSizes
  , isPlayable
  , makeChoices
  , makePrompt
  , promptSet
  , transition
  )
import Effect (Effect)
import Effect.Exception (throw)
import Test.Assert (assertEqual)

run :: Effect Unit
run = do
  defaultPrompts <- case promptSet defaultConfig of
    Nothing -> throw "Default configuration must produce prompts."
    Just prompts -> pure prompts
  let
    promptsFor config = fromMaybe defaultPrompts (promptSet config)
    c4 = pitch C (Accidental 0) 4
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
    generatedPrompt = makePrompt 128 (promptsFor descendingConfig)
    generatedChoices = makeChoices 128 descendingConfig generatedPrompt
    allAnswersConfig = descendingConfig { answerCount = AllSelected }
    allSelectedChoices = makeChoices 128 allAnswersConfig generatedPrompt
    generatedRange = presetRange descendingConfig.vocalRange
    audiationConfig = descendingConfig { quizMode = Audiation }
    audiationPrompt = makePrompt 128 (promptsFor audiationConfig)
    ascendingAudiationPrompt = makePrompt 128
      (promptsFor (defaultConfig { quizMode = Audiation, playbackModes = [ MelodicAscending ] }))
    collectionConfig = defaultConfig
      { availableIntervals = [ SizeThird ]
      , intervalSystem = FromSelectedNotes
      , playbackModes = [ MelodicAscending ]
      }
    collectionPrompts = map (flip makePrompt (promptsFor collectionConfig)) (Array.range 0 80)
    collectionChoices = makeChoices 24 collectionConfig (makePrompt 24 (promptsFor collectionConfig))
    allCollectionChoices = makeChoices 24
      (collectionConfig { answerCount = AllSelected })
      (makePrompt 24 (promptsFor collectionConfig))
    descendingCollectionConfig = collectionConfig { playbackModes = [ MelodicDescending ] }
    descendingCollectionPrompts = map (flip makePrompt (promptsFor descendingCollectionConfig)) (Array.range 0 40)
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
    narrowExactPrompt = makePrompt 0 (promptsFor narrowExactConfig)
  assertEqual { actual: Array.length generatedChoices, expected: 4 }
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
        , not (Array.elem DiminishedFifth (map _.interval augmentedFourthChoices))
        , Array.length (Array.nub (map (midiNumber <<< _.target) augmentedFourthChoices)) == Array.length augmentedFourthChoices
        , Array.elem DiminishedFifth (map _.interval diminishedFifthChoices)
        , not (Array.elem AugmentedFourth (map _.interval diminishedFifthChoices))
        , Array.length (Array.nub (map (midiNumber <<< _.target) diminishedFifthChoices)) == Array.length diminishedFifthChoices
        , Array.elem AugmentedFifth (map _.interval augmentedFifthChoices)
        , not (Array.elem MinorSixth (map _.interval augmentedFifthChoices))
        , Array.length (Array.nub (map (midiNumber <<< _.target) augmentedFifthChoices)) == Array.length augmentedFifthChoices
        ]
    , expected: Array.replicate 9 true
    }
  assertEqual
    { actual:
        [ Array.length (Array.filter (\choice -> choice.interval == generatedPrompt.interval) generatedChoices) == 1
        , Array.length allSelectedChoices == Array.length allAnswersConfig.intervals
        , Array.all (\interval -> Array.elem interval (map _.interval allSelectedChoices)) allAnswersConfig.intervals
        , Array.all (\prompt -> midiNumber prompt.target < midiNumber prompt.root) descendingCollectionPrompts
        , Array.length collectionChoices == 2
        , Array.length
            ( Array.filter
                (\choice -> choice.interval == (makePrompt 24 (promptsFor collectionConfig)).interval)
                collectionChoices
            ) == 1
        , Array.all (\choice -> intervalNumber choice.interval == 3) allCollectionChoices
        ]
    , expected: Array.replicate 7 true
    }
  assertEqual
    { actual:
        [ midiNumber generatedPrompt.root >= midiNumber generatedRange.low
        , midiNumber generatedPrompt.target >= midiNumber generatedRange.low
        , midiNumber generatedPrompt.root <= midiNumber generatedRange.high
        , midiNumber generatedPrompt.target <= midiNumber generatedRange.high
        ]
    , expected: Array.replicate 4 true
    }
  assertEqual
    { actual:
        [ quizModeUsesSinging SingingOnly
        , not (quizModeUsesRecognition SingingOnly)
        , not (quizModeUsesSinging RecognitionOnly)
        , quizModeUsesRecognition RecognitionOnly
        , quizModeUsesSinging SingingAndRecognition
        , quizModeUsesRecognition SingingAndRecognition
        , quizModeUsesSinging Audiation
        , not (quizModeUsesRecognition Audiation)
        , defaultConfig.quizProgression == AutomaticProgression
        ]
    , expected: Array.replicate 9 true
    }
  assertEqual
    { actual:
        [ Array.all
            ( \prompt ->
                intervalNumber prompt.interval == 3
                  && Array.elem (pitchClassOf prompt.root) collectionConfig.rootPitchClasses
                  && Array.elem (pitchClassOf prompt.target) collectionConfig.rootPitchClasses
            )
            collectionPrompts
        , Array.elem SizeThird (availableIntervalSizes collectionConfig)
        , not (Array.elem SizeSecond (availableIntervalSizes sparseCollectionConfig))
        , midiNumber audiationPrompt.target < midiNumber audiationPrompt.root
        , midiNumber ascendingAudiationPrompt.target > midiNumber ascendingAudiationPrompt.root
        , not (isValid (defaultConfig { quizMode = Audiation, playbackModes = [ Harmonic ] }))
        , isValid (toggleInterval MinorThird defaultConfig)
        ]
    , expected: Array.replicate 7 true
    }
  assertEqual
    { actual:
        [ selectedMajorKeyId defaultConfig.rootPitchClasses == "c"
        , selectedMajorKeyId selectedGMajor.rootPitchClasses == "g"
        , midiNumber classAdjustedRange.customRange.low == 49
        , midiNumber octaveAdjustedRange.customRange.high == 91
        , isValid narrowConfig
        , not (isPlayable narrowConfig)
        ]
    , expected: Array.replicate 6 true
    }
  assertEqual
    { actual: map (isJust <<< promptSet)
        [ descendingConfig, audiationConfig, collectionConfig, descendingCollectionConfig, narrowExactConfig ]
    , expected: Array.replicate 5 true
    }
  assertEqual
    { actual:
        [ transition SingingFirstNote FirstPitchAccepted == Just AwaitingRearticulation
        , transition AwaitingRearticulation SecondPitchAccepted == Nothing
        , transition AwaitingRearticulation VoiceReleased == Just SingingSecondNote
        , transition SingingFirstNote PitchRejected == Just IntervalError
        , transition SingingSecondNote PitchRejected == Just IntervalError
        , transition IntervalError Continue == Just PlayingInterval
        ]
    , expected: Array.replicate 6 true
    }
