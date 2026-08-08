module Test.Settings (run) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EarTrainer.Config (AnswerCount(..), QuizProgression(..), defaultConfig)
import EarTrainer.Settings
  ( DecodeError(..)
  , NameError(..)
  , decodeStoredAppData
  , encodeStoredAppData
  , presetId
  , presetName
  , validatePresetName
  )
import Effect (Effect)
import Foreign (unsafeToForeign)
import Test.Assert (assertEqual, assertTrue')

run :: Effect Unit
run = do
  let
    existingPreset = { config: defaultConfig, id: presetId "existing", name: "Warmup" }
    legacySettingsRecord =
      { answerCount: "few"
      , intervals: [ "major-third" ]
      , octavePolicy: "any-octave"
      , playbackModes: [ "melodic-ascending" ]
      , rootPitchClasses: [ { accidental: 0, letter: "C" } ]
      , vocalRange: "tenor"
      }
    legacySettings = unsafeToForeign legacySettingsRecord
    legacySettingsWithRange low high = unsafeToForeign
      { answerCount: "few"
      , customHighMidi: high
      , customLowMidi: low
      , intervals: [ "major-third" ]
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
      , presets: [ validLegacyPreset ]
      , settings: legacySettings
      }
    malformedPresetData = unsafeToForeign
      { activePresetId: ""
      , presets: [ unsafeToForeign "invalid preset" ]
      , settings: legacySettings
      }
    storedDataWith settings = unsafeToForeign
      { activePresetId: ""
      , presets: []
      , settings
      }
    rejectsFutureData = case decodeStoredAppData (unsafeToForeign { version: 2 }) of
      Left (UnsupportedStoredVersion 2) -> true
      _ -> false
    rejectsMalformedData = case decodeStoredAppData (unsafeToForeign { version: 1, settings: "invalid" }) of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsMalformedPreset = case decodeStoredAppData malformedPresetData of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsMalformed = case _ of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsUnknownVocalRange = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (unsafeToForeign (legacySettingsRecord { vocalRange = "contralto" })))
    rejectsUnknownAnswerCount = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (unsafeToForeign (legacySettingsRecord { answerCount = "many" })))
    rejectsUnknownInterval = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (unsafeToForeign (legacySettingsRecord { intervals = [ "mystery-interval" ] })))
    rejectsInvalidConfig =
      case
        decodeStoredAppData
          (storedDataWith (unsafeToForeign (legacySettingsRecord { playbackModes = [] })))
        of
        Left (MalformedStoredData _) -> true
        _ -> false
    rejectsMalformedVersion = case decodeStoredAppData (unsafeToForeign { version: "one" }) of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsNegativeVersion = case decodeStoredAppData (unsafeToForeign { version: -1 }) of
      Left (UnsupportedStoredVersion (-1)) -> true
      _ -> false
    rejectsIncompleteVersion1 = rejectsMalformed $ decodeStoredAppData $ unsafeToForeign
      { activePresetId: ""
      , presets: []
      , settings: legacySettings
      , version: 1
      }
    rejectsInvalidAccidental = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (unsafeToForeign (legacySettingsRecord { rootPitchClasses = [ { accidental: 3, letter: "C" } ] })))
    rejectsLowMidi = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (legacySettingsWithRange (-1) 72))
    rejectsHighMidi = rejectsMalformed $ decodeStoredAppData
      (storedDataWith (legacySettingsWithRange 48 128))
    currentData =
      { activePresetId: Nothing
      , config: defaultConfig
      , presets: [ existingPreset ]
      }
  assertTrue' "blank preset names are rejected" (validatePresetName [] "   " == Left EmptyName)
  assertTrue' "preset names are unique ignoring case"
    (validatePresetName [ existingPreset ] " warmUP " == Left DuplicateName)
  assertTrue' "preset names are trimmed"
    (map presetName (validatePresetName [ existingPreset ] "  Daily thirds  ") == Right "Daily thirds")
  case decodeStoredAppData legacyStoredData of
    Left _ -> assertTrue' "legacy settings decode" false
    Right value -> do
      assertTrue' "active preset ID is decoded" (value.activePresetId == Just (presetId "legacy-preset"))
      assertTrue' "answer count is decoded" (value.config.answerCount == AFew)
      assertTrue' "missing progression receives its migration default"
        (value.config.quizProgression == AutomaticProgression)
      assertEqual { actual: Array.length value.presets, expected: 1 }
  case decodeStoredAppData (unsafeToForeign (encodeStoredAppData currentData)) of
    Left _ -> assertTrue' "current settings decode" false
    Right value -> assertEqual { actual: Array.length value.presets, expected: 1 }
  assertTrue' "future versions are rejected" rejectsFutureData
  assertTrue' "malformed settings are rejected" rejectsMalformedData
  assertTrue' "malformed presets are rejected" rejectsMalformedPreset
  assertTrue' "malformed versions are rejected" rejectsMalformedVersion
  assertTrue' "negative versions are rejected" rejectsNegativeVersion
  assertTrue' "version 1 requires its complete schema" rejectsIncompleteVersion1
  assertTrue' "unknown vocal ranges are rejected" rejectsUnknownVocalRange
  assertTrue' "unknown answer counts are rejected" rejectsUnknownAnswerCount
  assertTrue' "unknown intervals are rejected" rejectsUnknownInterval
  assertTrue' "invalid exercise configurations are rejected" rejectsInvalidConfig
  assertTrue' "out-of-range accidentals are rejected" rejectsInvalidAccidental
  assertTrue' "MIDI notes below zero are rejected" rejectsLowMidi
  assertTrue' "MIDI notes above 127 are rejected" rejectsHighMidi
