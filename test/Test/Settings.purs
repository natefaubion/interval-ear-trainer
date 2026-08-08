module Test.Settings (run) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EarTrainer.Config (AnswerCount(..), QuizProgression(..), defaultConfig)
import EarTrainer.Settings (DecodeError(..), NameError(..), decodeStoredAppData, presetId, presetName, validatePresetName)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Test.Assert (assertEqual)

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
    decodedLegacyData = case decodeStoredAppData legacyStoredData of
      Left _ -> [ false, false, false, false ]
      Right value ->
        [ value.activePresetId == Just (presetId "legacy-preset")
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
    rejectsMalformedPreset = case decodeStoredAppData malformedPresetData of
      Left (MalformedStoredData _) -> true
      _ -> false
    rejectsUnknownTags = map
      ( case _ of
          Left (MalformedStoredData _) -> true
          _ -> false
      )
      [ decodeStoredAppData (storedDataWith (unsafeToForeign (legacySettingsRecord { vocalRange = "contralto" })))
      , decodeStoredAppData (storedDataWith (unsafeToForeign (legacySettingsRecord { answerCount = "many" })))
      , decodeStoredAppData (storedDataWith (unsafeToForeign (legacySettingsRecord { intervals = [ "mystery-interval" ] })))
      ]
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
  assertEqual
    { actual:
        [ validatePresetName [] "   " == Left EmptyName
        , validatePresetName [ existingPreset ] " warmUP " == Left DuplicateName
        , map presetName (validatePresetName [ existingPreset ] "  Daily thirds  ") == Right "Daily thirds"
        ]
    , expected: [ true, true, true ]
    }
  assertEqual
    { actual:
        decodedLegacyData
          <> [ rejectsFutureData, rejectsMalformedData, rejectsMalformedPreset, rejectsMalformedVersion ]
          <> rejectsUnknownTags
          <> [ rejectsInvalidConfig ]
    , expected: [ true, true, true, true, true, true, true, true, true, true, true, true ]
    }
