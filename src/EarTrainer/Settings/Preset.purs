module EarTrainer.Settings.Preset
  ( NameError(..)
  , Preset
  , PresetName
  , presetName
  , validatePresetName
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String.Common as String
import EarTrainer.Config (ExerciseConfig)
import EarTrainer.Settings.PresetId (PresetId)

type Preset =
  { config :: ExerciseConfig
  , id :: PresetId
  , name :: String
  }

newtype PresetName = PresetName String

derive instance Eq PresetName

data NameError
  = EmptyName
  | DuplicateName

derive instance Eq NameError

presetName :: PresetName -> String
presetName (PresetName name) = name

validatePresetName :: Array Preset -> String -> Either NameError PresetName
validatePresetName presets input = do
  let
    name = String.trim input
    duplicate = Array.any (\preset -> String.toLower preset.name == String.toLower name) presets
  if name == "" then Left EmptyName
  else if duplicate then Left DuplicateName
  else Right (PresetName name)
