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
import Data.Either (Either(..), hush)
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
import Foreign (F, Foreign, ForeignError, readArray, readBoolean, readInt, readString, readUndefined, renderForeignError)
import Foreign.Index (readProp)

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
  let presets = Array.mapMaybe (hush <<< runExcept <<< decodePreset) storedPresets
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
    intervals <- requiredProperty readStringArray "intervals" value
    octavePolicy <- requiredProperty readString "octavePolicy" value
    playbackModes <- requiredProperty readStringArray "playbackModes" value
    rootPitchClasses <- requiredProperty readPitchClassArray "rootPitchClasses" value
    vocalRange <- requiredProperty readString "vocalRange" value
    answerCount <- optionalProperty readString "answerCount" (encodeAnswerCount defaultConfig.answerCount) value
    answerDisplay <- optionalProperty readString "answerDisplay" (encodeAnswerDisplay defaultConfig.answerDisplay) value
    availableIntervals <- optionalProperty readStringArray "availableIntervals"
      (map encodeIntervalSize defaultConfig.availableIntervals)
      value
    customHighMidi <- optionalProperty readInt "customHighMidi" (midiNumber defaultConfig.customRange.high) value
    customLowMidi <- optionalProperty readInt "customLowMidi" (midiNumber defaultConfig.customRange.low) value
    ghostMode <- optionalProperty readString "ghostMode" (encodeGhostMode defaultConfig.ghostMode) value
    intervalSystem <- optionalProperty readString "intervalSystem" (encodeIntervalSystem defaultConfig.intervalSystem) value
    showPitchTuner <- optionalProperty readBoolean "showPitchTuner" defaultConfig.showPitchTuner value
    quizMode <- optionalProperty readString "quizMode" (encodeQuizMode defaultConfig.quizMode) value
    quizProgression <- optionalProperty readString "quizProgression"
      (encodeQuizProgression defaultConfig.quizProgression)
      value
    pure $ defaultConfig
      { answerCount = decodeAnswerCount answerCount
      , answerDisplay = decodeAnswerDisplay answerDisplay
      , availableIntervals = Array.mapMaybe decodeIntervalSize availableIntervals
      , customRange = { low: pitchFromMidi customLowMidi, high: pitchFromMidi customHighMidi }
      , ghostMode = decodeGhostMode ghostMode
      , intervals = Array.mapMaybe decodeInterval intervals
      , intervalSystem = decodeIntervalSystem intervalSystem
      , octavePolicy = decodeOctavePolicy octavePolicy
      , playbackModes = Array.mapMaybe decodePlaybackMode playbackModes
      , showPitchTuner = showPitchTuner
      , quizMode = decodeQuizMode quizMode
      , quizProgression = decodeQuizProgression quizProgression
      , rootPitchClasses = rootPitchClasses
      , vocalRange = decodeVocalRange vocalRange
      }

requiredProperty :: forall a. (Foreign -> F a) -> String -> Foreign -> F a
requiredProperty read name value = readProp name value >>= read

optionalProperty :: forall a. (Foreign -> F a) -> String -> a -> Foreign -> F a
optionalProperty read name fallback value = case runExcept (requiredProperty read name value) of
  Left _ -> pure fallback
  Right result -> pure result

readStringArray :: Foreign -> F (Array String)
readStringArray value = readArray value >>= traverse readString

readPitchClassArray :: Foreign -> F (Array PitchClass)
readPitchClassArray value = readArray value >>= traverse readPitchClass

readPitchClass :: Foreign -> F PitchClass
readPitchClass value = do
  accidental <- requiredProperty readInt "accidental" value
  letter <- requiredProperty readString "letter" value
  pure (PitchClass (decodeLetter letter) (Accidental accidental))

encodeQuizMode :: QuizMode -> String
encodeQuizMode SingingOnly = "singing"
encodeQuizMode RecognitionOnly = "recognition"
encodeQuizMode SingingAndRecognition = "singing-and-recognition"
encodeQuizMode Audiation = "audiation"

decodeQuizMode :: String -> QuizMode
decodeQuizMode "singing" = SingingOnly
decodeQuizMode "recognition" = RecognitionOnly
decodeQuizMode "audiation" = Audiation
decodeQuizMode _ = SingingAndRecognition

encodeQuizProgression :: QuizProgression -> String
encodeQuizProgression ManualProgression = "manual"
encodeQuizProgression AutomaticProgression = "automatic"

decodeQuizProgression :: String -> QuizProgression
decodeQuizProgression "automatic" = AutomaticProgression
decodeQuizProgression _ = ManualProgression

encodeAnswerCount :: AnswerCount -> String
encodeAnswerCount AFew = "few"
encodeAnswerCount AllSelected = "all-selected"

decodeAnswerCount :: String -> AnswerCount
decodeAnswerCount "all-selected" = AllSelected
decodeAnswerCount _ = AFew

encodeGhostMode :: GhostMode -> String
encodeGhostMode GhostOff = "off"
encodeGhostMode GhostOn = "on"
encodeGhostMode GhostPersist = "persist"

decodeGhostMode :: String -> GhostMode
decodeGhostMode "off" = GhostOff
decodeGhostMode "persist" = GhostPersist
decodeGhostMode _ = GhostOn

encodeAnswerDisplay :: AnswerDisplay -> String
encodeAnswerDisplay AnswerNotation = "notation"
encodeAnswerDisplay AnswerName = "name"
encodeAnswerDisplay AnswerBoth = "both"

decodeAnswerDisplay :: String -> AnswerDisplay
decodeAnswerDisplay "name" = AnswerName
decodeAnswerDisplay "both" = AnswerBoth
decodeAnswerDisplay _ = AnswerNotation

encodeIntervalSystem :: IntervalSystem -> String
encodeIntervalSystem ExactIntervals = "exact"
encodeIntervalSystem FromSelectedNotes = "from-selected-notes"

decodeIntervalSystem :: String -> IntervalSystem
decodeIntervalSystem "exact" = ExactIntervals
decodeIntervalSystem "from-selected-notes" = FromSelectedNotes
decodeIntervalSystem _ = FromSelectedNotes

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

decodeOctavePolicy :: String -> OctavePolicy
decodeOctavePolicy "written-octave" = WrittenOctave
decodeOctavePolicy _ = AnyOctave

encodeVocalRange :: VocalRangePreset -> String
encodeVocalRange Bass = "bass"
encodeVocalRange Baritone = "baritone"
encodeVocalRange Tenor = "tenor"
encodeVocalRange Alto = "alto"
encodeVocalRange MezzoSoprano = "mezzo-soprano"
encodeVocalRange Soprano = "soprano"
encodeVocalRange ExtraWide = "extra-wide"
encodeVocalRange Custom = "custom"

decodeVocalRange :: String -> VocalRangePreset
decodeVocalRange "bass" = Bass
decodeVocalRange "baritone" = Baritone
decodeVocalRange "alto" = Alto
decodeVocalRange "mezzo-soprano" = MezzoSoprano
decodeVocalRange "soprano" = Soprano
decodeVocalRange "extra-wide" = ExtraWide
decodeVocalRange "custom" = Custom
decodeVocalRange _ = Tenor

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

decodeLetter :: String -> Letter
decodeLetter "D" = D
decodeLetter "E" = E
decodeLetter "F" = F
decodeLetter "G" = G
decodeLetter "A" = A
decodeLetter "B" = B
decodeLetter _ = C
