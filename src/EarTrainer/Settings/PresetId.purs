module EarTrainer.Settings.PresetId
  ( PresetId
  , newPresetId
  , presetId
  , presetIdString
  ) where

import Prelude

import Effect (Effect)

newtype PresetId = PresetId String

derive newtype instance Eq PresetId

presetId :: String -> PresetId
presetId = PresetId

presetIdString :: PresetId -> String
presetIdString (PresetId value) = value

foreign import newPresetIdString :: Effect String

newPresetId :: Effect PresetId
newPresetId = map PresetId newPresetIdString
