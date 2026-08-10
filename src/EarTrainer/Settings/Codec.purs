module EarTrainer.Settings.Codec
  ( AppData
  , DecodeError(..)
  , StoredAppData
  , decodeStoredAppData
  , emptyAppData
  , encodeStoredAppData
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
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
  , melodyLength
  , mkMelodyLength
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
import EarTrainer.Settings.Preset (Preset)
import EarTrainer.Settings.PresetId (PresetId, presetId, presetIdString)
import Foreign (F, Foreign, ForeignError(..), readArray, readBoolean, readInt, readString, readUndefined, renderForeignError)
import Foreign as Foreign
import Foreign.Index (readProp)
import Foreign.Keys (keys)

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
  show = case _ of
    MalformedStoredData errors -> "MalformedStoredData " <> show errors
    UnsupportedStoredVersion version -> "UnsupportedStoredVersion " <> show version

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
  , melodyLength :: Int
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
currentStoredVersion = 2

newtype Version0AppData = Version0AppData
  { activePresetId :: Maybe PresetId
  , config :: ExerciseConfig
  , presets :: Array Preset
  }

decodeStoredAppData :: Foreign -> Either DecodeError AppData
decodeStoredAppData value = do
  version <- mapDecodeErrors (runExcept (readStoredVersion value))
  case version of
    0 -> map migrateVersion0ToVersion1 $ mapDecodeErrors $ runExcept $ decodeVersion0 value
    1 -> mapDecodeErrors $ runExcept $ decodeAppData decodeSettingsVersion1 value
    2 -> mapDecodeErrors $ runExcept $ decodeAppData decodeSettingsVersion2 value
    unsupported -> Left (UnsupportedStoredVersion unsupported)

decodeVersion0 :: Foreign -> F Version0AppData
decodeVersion0 = map Version0AppData <<< decodeAppData decodeSettingsVersion0

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

decodeAppData :: (Foreign -> F ExerciseConfig) -> Foreign -> F AppData
decodeAppData decodeStoredSettings value = do
  config <- readProp "settings" value >>= decodeStoredSettings
  storedPresets <- readProp "presets" value >>= readArray
  presets <- traverse (decodePreset decodeStoredSettings) storedPresets
  storedActivePresetId <- optionalProperty readString "activePresetId" "" value
  let
    activePresetId =
      if Array.any (\preset -> presetIdString preset.id == storedActivePresetId) presets then Just (presetId storedActivePresetId)
      else Nothing
  pure { activePresetId, config, presets }

migrateVersion0ToVersion1 :: Version0AppData -> AppData
migrateVersion0ToVersion1 (Version0AppData legacy) =
  { activePresetId: legacy.activePresetId
  , config: legacy.config
  , presets: legacy.presets
  }

encodePreset :: Preset -> StoredPreset
encodePreset preset = { id: presetIdString preset.id, name: preset.name, settings: encodeSettings preset.config }

decodePreset :: (Foreign -> F ExerciseConfig) -> Foreign -> F Preset
decodePreset decodeStoredSettings value = do
  id <- requiredProperty readString "id" value
  name <- requiredProperty readString "name" value
  config <- readProp "settings" value >>= decodeStoredSettings
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
  , melodyLength: melodyLength config.melodyLength
  , octavePolicy: encodeOctavePolicy config.octavePolicy
  , playbackModes: map encodePlaybackMode config.playbackModes
  , showPitchTuner: config.showPitchTuner
  , quizMode: encodeQuizMode config.quizMode
  , quizProgression: encodeQuizProgression config.quizProgression
  , rootPitchClasses: map encodePitchClass config.rootPitchClasses
  , vocalRange: encodeVocalRange config.vocalRange
  }

decodeSettingsVersion0 :: Foreign -> F ExerciseConfig
decodeSettingsVersion0 = decodeSettings

decodeSettingsVersion1 :: Foreign -> F ExerciseConfig
decodeSettingsVersion1 value = do
  requireProperties
    [ "answerCount"
    , "answerDisplay"
    , "availableIntervals"
    , "customHighMidi"
    , "customLowMidi"
    , "ghostMode"
    , "intervalSystem"
    , "quizMode"
    , "quizProgression"
    , "showPitchTuner"
    ]
    value
  decodeSettings value

decodeSettingsVersion2 :: Foreign -> F ExerciseConfig
decodeSettingsVersion2 value = do
  requireProperties [ "melodyLength" ] value
  decodeSettingsVersion1 value

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
    customHighMidi <- optionalProperty readMidi "customHighMidi" (midiNumber defaultConfig.customRange.high) value
    customLowMidi <- optionalProperty readMidi "customLowMidi" (midiNumber defaultConfig.customRange.low) value
    ghostMode <- optionalProperty (readTag "ghost mode" decodeGhostMode) "ghostMode" defaultConfig.ghostMode value
    intervalSystem <- optionalProperty (readTag "interval system" decodeIntervalSystem) "intervalSystem" defaultConfig.intervalSystem value
    melodyLengthValue <- optionalProperty (readBoundedInt "melody length" 3 8) "melodyLength"
      (melodyLength defaultConfig.melodyLength)
      value
    melodyLength <- case mkMelodyLength melodyLengthValue of
      Nothing -> Foreign.fail (ForeignError "Melody length must be between 3 and 8")
      Just length -> pure length
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
      , melodyLength = melodyLength
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

requireProperties :: Array String -> Foreign -> F Unit
requireProperties names value = do
  properties <- keys value
  traverse_ (requireProperty properties) names
  where
  requireProperty properties name
    | Array.elem name properties = pure unit
    | otherwise = Foreign.fail (ForeignError ("Missing required property: " <> name))

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
  accidental <- requiredProperty (readBoundedInt "accidental" (-2) 2) "accidental" value
  letter <- requiredProperty (readTag "pitch letter" decodeLetter) "letter" value
  pure (PitchClass letter (Accidental accidental))

readMidi :: Foreign -> F Int
readMidi = readBoundedInt "MIDI note" 0 127

readBoundedInt :: String -> Int -> Int -> Foreign -> F Int
readBoundedInt name lower upper value = do
  number <- readInt value
  if number >= lower && number <= upper then pure number
  else Foreign.fail (ForeignError (name <> " must be between " <> show lower <> " and " <> show upper))

encodeQuizMode :: QuizMode -> String
encodeQuizMode = case _ of
  SingingOnly -> "singing"
  RecognitionOnly -> "recognition"
  SingingAndRecognition -> "singing-and-recognition"
  Audiation -> "audiation"
  MelodyImitation -> "melody-imitation"

decodeQuizMode :: String -> Maybe QuizMode
decodeQuizMode = case _ of
  "singing" -> Just SingingOnly
  "recognition" -> Just RecognitionOnly
  "singing-and-recognition" -> Just SingingAndRecognition
  "audiation" -> Just Audiation
  "melody-imitation" -> Just MelodyImitation
  _ -> Nothing

encodeQuizProgression :: QuizProgression -> String
encodeQuizProgression = case _ of
  ManualProgression -> "manual"
  AutomaticProgression -> "automatic"

decodeQuizProgression :: String -> Maybe QuizProgression
decodeQuizProgression = case _ of
  "manual" -> Just ManualProgression
  "automatic" -> Just AutomaticProgression
  _ -> Nothing

encodeAnswerCount :: AnswerCount -> String
encodeAnswerCount = case _ of
  AFew -> "few"
  AllSelected -> "all-selected"

decodeAnswerCount :: String -> Maybe AnswerCount
decodeAnswerCount = case _ of
  "few" -> Just AFew
  "all-selected" -> Just AllSelected
  _ -> Nothing

encodeGhostMode :: GhostMode -> String
encodeGhostMode = case _ of
  GhostOff -> "off"
  GhostOn -> "on"
  GhostPersist -> "persist"

decodeGhostMode :: String -> Maybe GhostMode
decodeGhostMode = case _ of
  "off" -> Just GhostOff
  "on" -> Just GhostOn
  "persist" -> Just GhostPersist
  _ -> Nothing

encodeAnswerDisplay :: AnswerDisplay -> String
encodeAnswerDisplay = case _ of
  AnswerNotation -> "notation"
  AnswerName -> "name"
  AnswerBoth -> "both"

decodeAnswerDisplay :: String -> Maybe AnswerDisplay
decodeAnswerDisplay = case _ of
  "notation" -> Just AnswerNotation
  "name" -> Just AnswerName
  "both" -> Just AnswerBoth
  _ -> Nothing

encodeIntervalSystem :: IntervalSystem -> String
encodeIntervalSystem = case _ of
  ExactIntervals -> "exact"
  FromSelectedNotes -> "from-selected-notes"

decodeIntervalSystem :: String -> Maybe IntervalSystem
decodeIntervalSystem = case _ of
  "exact" -> Just ExactIntervals
  "from-selected-notes" -> Just FromSelectedNotes
  _ -> Nothing

encodeIntervalSize :: IntervalSize -> String
encodeIntervalSize = case _ of
  SizeUnison -> "unison"
  SizeSecond -> "second"
  SizeThird -> "third"
  SizeFourth -> "fourth"
  SizeFifth -> "fifth"
  SizeSixth -> "sixth"
  SizeSeventh -> "seventh"
  SizeOctave -> "octave"

decodeIntervalSize :: String -> Maybe IntervalSize
decodeIntervalSize = case _ of
  "unison" -> Just SizeUnison
  "second" -> Just SizeSecond
  "third" -> Just SizeThird
  "fourth" -> Just SizeFourth
  "fifth" -> Just SizeFifth
  "sixth" -> Just SizeSixth
  "seventh" -> Just SizeSeventh
  "octave" -> Just SizeOctave
  _ -> Nothing

encodeInterval :: Interval -> String
encodeInterval = case _ of
  PerfectUnison -> "perfect-unison"
  MinorSecond -> "minor-second"
  MajorSecond -> "major-second"
  MinorThird -> "minor-third"
  MajorThird -> "major-third"
  PerfectFourth -> "perfect-fourth"
  AugmentedFourth -> "augmented-fourth"
  DiminishedFifth -> "diminished-fifth"
  PerfectFifth -> "perfect-fifth"
  AugmentedFifth -> "augmented-fifth"
  MinorSixth -> "minor-sixth"
  MajorSixth -> "major-sixth"
  MinorSeventh -> "minor-seventh"
  MajorSeventh -> "major-seventh"
  PerfectOctave -> "perfect-octave"

decodeInterval :: String -> Maybe Interval
decodeInterval = case _ of
  "perfect-unison" -> Just PerfectUnison
  "minor-second" -> Just MinorSecond
  "major-second" -> Just MajorSecond
  "minor-third" -> Just MinorThird
  "major-third" -> Just MajorThird
  "perfect-fourth" -> Just PerfectFourth
  "augmented-fourth" -> Just AugmentedFourth
  "diminished-fifth" -> Just DiminishedFifth
  "perfect-fifth" -> Just PerfectFifth
  "augmented-fifth" -> Just AugmentedFifth
  "minor-sixth" -> Just MinorSixth
  "major-sixth" -> Just MajorSixth
  "minor-seventh" -> Just MinorSeventh
  "major-seventh" -> Just MajorSeventh
  "perfect-octave" -> Just PerfectOctave
  _ -> Nothing

encodePlaybackMode :: PlaybackMode -> String
encodePlaybackMode = case _ of
  MelodicAscending -> "melodic-ascending"
  MelodicDescending -> "melodic-descending"
  Harmonic -> "harmonic"

decodePlaybackMode :: String -> Maybe PlaybackMode
decodePlaybackMode = case _ of
  "melodic-ascending" -> Just MelodicAscending
  "melodic-descending" -> Just MelodicDescending
  "harmonic" -> Just Harmonic
  _ -> Nothing

encodeOctavePolicy :: OctavePolicy -> String
encodeOctavePolicy = case _ of
  AnyOctave -> "any-octave"
  WrittenOctave -> "written-octave"

decodeOctavePolicy :: String -> Maybe OctavePolicy
decodeOctavePolicy = case _ of
  "any-octave" -> Just AnyOctave
  "written-octave" -> Just WrittenOctave
  _ -> Nothing

encodeVocalRange :: VocalRangePreset -> String
encodeVocalRange = case _ of
  Bass -> "bass"
  Baritone -> "baritone"
  Tenor -> "tenor"
  Alto -> "alto"
  MezzoSoprano -> "mezzo-soprano"
  Soprano -> "soprano"
  ExtraWide -> "extra-wide"
  Custom -> "custom"

decodeVocalRange :: String -> Maybe VocalRangePreset
decodeVocalRange = case _ of
  "bass" -> Just Bass
  "baritone" -> Just Baritone
  "tenor" -> Just Tenor
  "alto" -> Just Alto
  "mezzo-soprano" -> Just MezzoSoprano
  "soprano" -> Just Soprano
  "extra-wide" -> Just ExtraWide
  "custom" -> Just Custom
  _ -> Nothing

encodePitchClass :: PitchClass -> StoredPitchClass
encodePitchClass (PitchClass letter (Accidental accidental)) =
  { accidental, letter: encodeLetter letter }

encodeLetter :: Letter -> String
encodeLetter = case _ of
  C -> "C"
  D -> "D"
  E -> "E"
  F -> "F"
  G -> "G"
  A -> "A"
  B -> "B"

decodeLetter :: String -> Maybe Letter
decodeLetter = case _ of
  "C" -> Just C
  "D" -> Just D
  "E" -> Just E
  "F" -> Just F
  "G" -> Just G
  "A" -> Just A
  "B" -> Just B
  _ -> Nothing
