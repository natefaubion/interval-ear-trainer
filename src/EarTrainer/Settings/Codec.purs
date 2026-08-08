module EarTrainer.Settings.Codec
  ( AppData
  , DecodeError(..)
  , Preset
  , StoredAppData
  , decodeStoredAppData
  , emptyAppData
  , encodeStoredAppData
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
  , isValid
  )
import EarTrainer.Music
  ( Accidental(..)
  , Interval(..)
  , IntervalSize(..)
  , Letter(..)
  , OctavePolicy(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , midiNumber
  , pitchFromMidi
  )
import EarTrainer.Settings.PresetId (PresetId, presetId, presetIdString)
import Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readInt, readString, readUndefined, renderForeignError)
import Foreign as Foreign
import Foreign.Index (readProp)
import Foreign.Keys (keys)

type Preset =
  { config :: ExerciseConfig
  , id :: PresetId
  , name :: String
  }

type AppData =
  { activePresetId :: Maybe PresetId
  , config :: ExerciseConfig
  , presets :: Array Preset
  }

data DecodeError
  = MalformedStoredData (Array String)
  | UnsupportedStoredVersion Int

derive instance Eq DecodeError

instance Show DecodeError where
  show (MalformedStoredData errors) = "MalformedStoredData " <> show errors
  show (UnsupportedStoredVersion version) = "UnsupportedStoredVersion " <> show version

type StoredPitchClass =
  { accidental :: Int
  , letter :: String
  }

type StoredSettings =
  { answerCount :: String
  , answerDisplay :: String
  , availableIntervals :: Array String
  , customHighMidi :: Int
  , customLowMidi :: Int
  , ghostMode :: String
  , intervals :: Array String
  , intervalSystem :: String
  , octavePolicy :: String
  , playbackModes :: Array String
  , showPitchTuner :: Boolean
  , quizMode :: String
  , quizProgression :: String
  , rootPitchClasses :: Array StoredPitchClass
  , vocalRange :: String
  }

type StoredPreset =
  { id :: String
  , name :: String
  , settings :: StoredSettings
  }

newtype StoredAppData = StoredAppData
  { activePresetId :: String
  , presets :: Array StoredPreset
  , settings :: StoredSettings
  , version :: Int
  }

emptyAppData :: AppData
emptyAppData = { activePresetId: Nothing, config: defaultConfig, presets: [] }

encodeStoredAppData :: AppData -> StoredAppData
encodeStoredAppData value =
  StoredAppData
    { activePresetId: case value.activePresetId of
        Nothing -> ""
        Just id -> presetIdString id
    , presets: map encodePreset value.presets
    , settings: encodeSettings value.config
    , version: currentStoredVersion
    }

currentStoredVersion :: Int
currentStoredVersion = 1

decodeStoredAppData :: Foreign -> Either DecodeError AppData
decodeStoredAppData value = do
  version <- mapDecodeErrors (runExcept (readStoredVersion value))
  if version > currentStoredVersion then Left (UnsupportedStoredVersion version)
  else mapDecodeErrors (runExcept (decodeAppData value))

readStoredVersion :: Foreign -> F Int
readStoredVersion value = do
  storedVersion <- readProp "version" value >>= readUndefined
  case storedVersion of
    Nothing -> pure 0
    Just version -> readInt version

mapDecodeErrors :: forall a. Either (NonEmptyList.NonEmptyList ForeignError) a -> Either DecodeError a
mapDecodeErrors = case _ of
  Left errors -> Left (MalformedStoredData (map renderForeignError (Array.fromFoldable errors)))
  Right value -> Right value

decodeAppData :: Foreign -> F AppData
decodeAppData value = do
  config <- readProp "settings" value >>= decodeSettings
  storedPresets <- readProp "presets" value >>= readArray
  presets <- traverse decodePreset storedPresets
  storedActivePresetId <- optionalProperty readString "activePresetId" "" value
  let
    activePresetId =
      if Array.any (\preset -> presetIdString preset.id == storedActivePresetId) presets then Just (presetId storedActivePresetId)
      else Nothing
  pure { activePresetId, config, presets }

encodePreset :: Preset -> StoredPreset
encodePreset preset = { id: presetIdString preset.id, name: preset.name, settings: encodeSettings preset.config }

decodePreset :: Foreign -> F Preset
decodePreset value = do
  id <- requiredProperty readString "id" value
  name <- requiredProperty readString "name" value
  config <- readProp "settings" value >>= decodeSettings
  pure { id: presetId id, name, config }

encodeSettings :: ExerciseConfig -> StoredSettings
encodeSettings config =
  { answerCount: encodeAnswerCount config.answerCount
  , answerDisplay: encodeAnswerDisplay config.answerDisplay
  , availableIntervals: map encodeIntervalSize config.availableIntervals
  , customHighMidi: midiNumber config.customRange.high
  , customLowMidi: midiNumber config.customRange.low
  , ghostMode: encodeGhostMode config.ghostMode
  , intervals: map encodeInterval config.intervals
  , intervalSystem: encodeIntervalSystem config.intervalSystem
  , octavePolicy: encodeOctavePolicy config.octavePolicy
  , playbackModes: map encodePlaybackMode config.playbackModes
  , showPitchTuner: config.showPitchTuner
  , quizMode: encodeQuizMode config.quizMode
  , quizProgression: encodeQuizProgression config.quizProgression
  , rootPitchClasses: map encodePitchClass config.rootPitchClasses
  , vocalRange: encodeVocalRange config.vocalRange
  }

decodeSettings :: Foreign -> F ExerciseConfig
decodeSettings value =
  do
    intervals <- requiredProperty (readTagArray "interval" decodeInterval) "intervals" value
    octavePolicy <- requiredProperty (readTag "octave policy" decodeOctavePolicy) "octavePolicy" value
    playbackModes <- requiredProperty (readTagArray "playback mode" decodePlaybackMode) "playbackModes" value
    rootPitchClasses <- requiredProperty readPitchClassArray "rootPitchClasses" value
    vocalRange <- requiredProperty (readTag "vocal range" decodeVocalRange) "vocalRange" value
    answerCount <- optionalProperty (readTag "answer count" decodeAnswerCount) "answerCount" defaultConfig.answerCount value
    answerDisplay <- optionalProperty (readTag "answer display" decodeAnswerDisplay) "answerDisplay" defaultConfig.answerDisplay value
    availableIntervals <- optionalProperty (readTagArray "interval size" decodeIntervalSize) "availableIntervals"
      defaultConfig.availableIntervals
      value
    customHighMidi <- optionalProperty readInt "customHighMidi" (midiNumber defaultConfig.customRange.high) value
    customLowMidi <- optionalProperty readInt "customLowMidi" (midiNumber defaultConfig.customRange.low) value
    ghostMode <- optionalProperty (readTag "ghost mode" decodeGhostMode) "ghostMode" defaultConfig.ghostMode value
    intervalSystem <- optionalProperty (readTag "interval system" decodeIntervalSystem) "intervalSystem" defaultConfig.intervalSystem value
    showPitchTuner <- optionalProperty readBoolean "showPitchTuner" defaultConfig.showPitchTuner value
    quizMode <- optionalProperty (readTag "quiz mode" decodeQuizMode) "quizMode" defaultConfig.quizMode value
    quizProgression <- optionalProperty (readTag "quiz progression" decodeQuizProgression) "quizProgression"
      defaultConfig.quizProgression
      value
    validateExerciseConfig $ defaultConfig
      { answerCount = answerCount
      , answerDisplay = answerDisplay
      , availableIntervals = availableIntervals
      , customRange = { low: pitchFromMidi customLowMidi, high: pitchFromMidi customHighMidi }
      , ghostMode = ghostMode
      , intervals = intervals
      , intervalSystem = intervalSystem
      , octavePolicy = octavePolicy
      , playbackModes = playbackModes
      , showPitchTuner = showPitchTuner
      , quizMode = quizMode
      , quizProgression = quizProgression
      , rootPitchClasses = rootPitchClasses
      , vocalRange = vocalRange
      }

validateExerciseConfig :: ExerciseConfig -> F ExerciseConfig
validateExerciseConfig config
  | isValid config = pure config
  | otherwise = Foreign.fail (ForeignError "Invalid exercise configuration")

requiredProperty :: forall a. (Foreign -> F a) -> String -> Foreign -> F a
requiredProperty read name value = readProp name value >>= read

optionalProperty :: forall a. (Foreign -> F a) -> String -> a -> Foreign -> F a
optionalProperty read name fallback value = do
  properties <- keys value
  if Array.elem name properties then requiredProperty read name value
  else pure fallback

readTag :: forall a. String -> (String -> Maybe a) -> Foreign -> F a
readTag name decode value = do
  tag <- readString value
  case decode tag of
    Nothing -> Foreign.fail (ForeignError ("Unknown " <> name <> " tag: " <> tag))
    Just result -> pure result

readTagArray :: forall a. String -> (String -> Maybe a) -> Foreign -> F (Array a)
readTagArray name decode value = readArray value >>= traverse (readTag name decode)

readPitchClassArray :: Foreign -> F (Array PitchClass)
readPitchClassArray value = readArray value >>= traverse readPitchClass

readPitchClass :: Foreign -> F PitchClass
readPitchClass value = do
  accidental <- requiredProperty readInt "accidental" value
  letter <- requiredProperty (readTag "pitch letter" decodeLetter) "letter" value
  pure (PitchClass letter (Accidental accidental))

encodeQuizMode :: QuizMode -> String
encodeQuizMode SingingOnly = "singing"
encodeQuizMode RecognitionOnly = "recognition"
encodeQuizMode SingingAndRecognition = "singing-and-recognition"
encodeQuizMode Audiation = "audiation"

decodeQuizMode :: String -> Maybe QuizMode
decodeQuizMode "singing" = Just SingingOnly
decodeQuizMode "recognition" = Just RecognitionOnly
decodeQuizMode "singing-and-recognition" = Just SingingAndRecognition
decodeQuizMode "audiation" = Just Audiation
decodeQuizMode _ = Nothing

encodeQuizProgression :: QuizProgression -> String
encodeQuizProgression ManualProgression = "manual"
encodeQuizProgression AutomaticProgression = "automatic"

decodeQuizProgression :: String -> Maybe QuizProgression
decodeQuizProgression "manual" = Just ManualProgression
decodeQuizProgression "automatic" = Just AutomaticProgression
decodeQuizProgression _ = Nothing

encodeAnswerCount :: AnswerCount -> String
encodeAnswerCount AFew = "few"
encodeAnswerCount AllSelected = "all-selected"

decodeAnswerCount :: String -> Maybe AnswerCount
decodeAnswerCount "few" = Just AFew
decodeAnswerCount "all-selected" = Just AllSelected
decodeAnswerCount _ = Nothing

encodeGhostMode :: GhostMode -> String
encodeGhostMode GhostOff = "off"
encodeGhostMode GhostOn = "on"
encodeGhostMode GhostPersist = "persist"

decodeGhostMode :: String -> Maybe GhostMode
decodeGhostMode "off" = Just GhostOff
decodeGhostMode "on" = Just GhostOn
decodeGhostMode "persist" = Just GhostPersist
decodeGhostMode _ = Nothing

encodeAnswerDisplay :: AnswerDisplay -> String
encodeAnswerDisplay AnswerNotation = "notation"
encodeAnswerDisplay AnswerName = "name"
encodeAnswerDisplay AnswerBoth = "both"

decodeAnswerDisplay :: String -> Maybe AnswerDisplay
decodeAnswerDisplay "notation" = Just AnswerNotation
decodeAnswerDisplay "name" = Just AnswerName
decodeAnswerDisplay "both" = Just AnswerBoth
decodeAnswerDisplay _ = Nothing

encodeIntervalSystem :: IntervalSystem -> String
encodeIntervalSystem ExactIntervals = "exact"
encodeIntervalSystem FromSelectedNotes = "from-selected-notes"

decodeIntervalSystem :: String -> Maybe IntervalSystem
decodeIntervalSystem "exact" = Just ExactIntervals
decodeIntervalSystem "from-selected-notes" = Just FromSelectedNotes
decodeIntervalSystem _ = Nothing

encodeIntervalSize :: IntervalSize -> String
encodeIntervalSize SizeUnison = "unison"
encodeIntervalSize SizeSecond = "second"
encodeIntervalSize SizeThird = "third"
encodeIntervalSize SizeFourth = "fourth"
encodeIntervalSize SizeFifth = "fifth"
encodeIntervalSize SizeSixth = "sixth"
encodeIntervalSize SizeSeventh = "seventh"
encodeIntervalSize SizeOctave = "octave"

decodeIntervalSize :: String -> Maybe IntervalSize
decodeIntervalSize "unison" = Just SizeUnison
decodeIntervalSize "second" = Just SizeSecond
decodeIntervalSize "third" = Just SizeThird
decodeIntervalSize "fourth" = Just SizeFourth
decodeIntervalSize "fifth" = Just SizeFifth
decodeIntervalSize "sixth" = Just SizeSixth
decodeIntervalSize "seventh" = Just SizeSeventh
decodeIntervalSize "octave" = Just SizeOctave
decodeIntervalSize _ = Nothing

encodeInterval :: Interval -> String
encodeInterval PerfectUnison = "perfect-unison"
encodeInterval MinorSecond = "minor-second"
encodeInterval MajorSecond = "major-second"
encodeInterval MinorThird = "minor-third"
encodeInterval MajorThird = "major-third"
encodeInterval PerfectFourth = "perfect-fourth"
encodeInterval AugmentedFourth = "augmented-fourth"
encodeInterval DiminishedFifth = "diminished-fifth"
encodeInterval PerfectFifth = "perfect-fifth"
encodeInterval AugmentedFifth = "augmented-fifth"
encodeInterval MinorSixth = "minor-sixth"
encodeInterval MajorSixth = "major-sixth"
encodeInterval MinorSeventh = "minor-seventh"
encodeInterval MajorSeventh = "major-seventh"
encodeInterval PerfectOctave = "perfect-octave"

decodeInterval :: String -> Maybe Interval
decodeInterval "perfect-unison" = Just PerfectUnison
decodeInterval "minor-second" = Just MinorSecond
decodeInterval "major-second" = Just MajorSecond
decodeInterval "minor-third" = Just MinorThird
decodeInterval "major-third" = Just MajorThird
decodeInterval "perfect-fourth" = Just PerfectFourth
decodeInterval "augmented-fourth" = Just AugmentedFourth
decodeInterval "diminished-fifth" = Just DiminishedFifth
decodeInterval "perfect-fifth" = Just PerfectFifth
decodeInterval "augmented-fifth" = Just AugmentedFifth
decodeInterval "minor-sixth" = Just MinorSixth
decodeInterval "major-sixth" = Just MajorSixth
decodeInterval "minor-seventh" = Just MinorSeventh
decodeInterval "major-seventh" = Just MajorSeventh
decodeInterval "perfect-octave" = Just PerfectOctave
decodeInterval _ = Nothing

encodePlaybackMode :: PlaybackMode -> String
encodePlaybackMode MelodicAscending = "melodic-ascending"
encodePlaybackMode MelodicDescending = "melodic-descending"
encodePlaybackMode Harmonic = "harmonic"

decodePlaybackMode :: String -> Maybe PlaybackMode
decodePlaybackMode "melodic-ascending" = Just MelodicAscending
decodePlaybackMode "melodic-descending" = Just MelodicDescending
decodePlaybackMode "harmonic" = Just Harmonic
decodePlaybackMode _ = Nothing

encodeOctavePolicy :: OctavePolicy -> String
encodeOctavePolicy AnyOctave = "any-octave"
encodeOctavePolicy WrittenOctave = "written-octave"

decodeOctavePolicy :: String -> Maybe OctavePolicy
decodeOctavePolicy "any-octave" = Just AnyOctave
decodeOctavePolicy "written-octave" = Just WrittenOctave
decodeOctavePolicy _ = Nothing

encodeVocalRange :: VocalRangePreset -> String
encodeVocalRange Bass = "bass"
encodeVocalRange Baritone = "baritone"
encodeVocalRange Tenor = "tenor"
encodeVocalRange Alto = "alto"
encodeVocalRange MezzoSoprano = "mezzo-soprano"
encodeVocalRange Soprano = "soprano"
encodeVocalRange ExtraWide = "extra-wide"
encodeVocalRange Custom = "custom"

decodeVocalRange :: String -> Maybe VocalRangePreset
decodeVocalRange "bass" = Just Bass
decodeVocalRange "baritone" = Just Baritone
decodeVocalRange "tenor" = Just Tenor
decodeVocalRange "alto" = Just Alto
decodeVocalRange "mezzo-soprano" = Just MezzoSoprano
decodeVocalRange "soprano" = Just Soprano
decodeVocalRange "extra-wide" = Just ExtraWide
decodeVocalRange "custom" = Just Custom
decodeVocalRange _ = Nothing

encodePitchClass :: PitchClass -> StoredPitchClass
encodePitchClass (PitchClass letter (Accidental accidental)) =
  { accidental, letter: encodeLetter letter }

encodeLetter :: Letter -> String
encodeLetter C = "C"
encodeLetter D = "D"
encodeLetter E = "E"
encodeLetter F = "F"
encodeLetter G = "G"
encodeLetter A = "A"
encodeLetter B = "B"

decodeLetter :: String -> Maybe Letter
decodeLetter "C" = Just C
decodeLetter "D" = Just D
decodeLetter "E" = Just E
decodeLetter "F" = Just F
decodeLetter "G" = Just G
decodeLetter "A" = Just A
decodeLetter "B" = Just B
decodeLetter _ = Nothing
