module Test.Settings (run) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EarTrainer.Config (AnswerCount(..), QuizProgression(..), defaultConfig)
import EarTrainer.Settings (DecodeError(..), NameError(..), decodeStoredAppData, presetId, presetName, validatePresetName)
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
  assertTrue' "future versions are rejected" rejectsFutureData
  assertTrue' "malformed settings are rejected" rejectsMalformedData
  assertTrue' "malformed presets are rejected" rejectsMalformedPreset
  assertTrue' "malformed versions are rejected" rejectsMalformedVersion
  assertTrue' "unknown vocal ranges are rejected" rejectsUnknownVocalRange
  assertTrue' "unknown answer counts are rejected" rejectsUnknownAnswerCount
  assertTrue' "unknown intervals are rejected" rejectsUnknownInterval
  assertTrue' "invalid exercise configurations are rejected" rejectsInvalidConfig
