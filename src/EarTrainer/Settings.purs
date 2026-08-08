module EarTrainer.Settings
  ( module Codec
  , load
  , newPresetId
  , requestPersistence
  , save
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EarTrainer.Settings.Codec
  ( AppData
  , DecodeError(..)
  , Preset
  , StoredAppData
  , decodeStoredAppData
  , emptyAppData
  , encodeStoredAppData
  ) as Codec
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)

foreign import loadImpl
  :: (Foreign -> Maybe Foreign)
  -> Maybe Foreign
  -> EffectFnAff (Maybe Foreign)

foreign import saveImpl :: Codec.StoredAppData -> EffectFnAff Unit
foreign import newPresetId :: Effect String
foreign import requestPersistenceImpl :: EffectFnAff Boolean

load :: Aff (Either Codec.DecodeError Codec.AppData)
load = do
  stored <- fromEffectFnAff (loadImpl Just Nothing)
  pure case stored of
    Nothing -> Right Codec.emptyAppData
    Just value -> Codec.decodeStoredAppData value

save :: Codec.AppData -> Aff Unit
save = fromEffectFnAff <<< saveImpl <<< Codec.encodeStoredAppData

requestPersistence :: Aff Boolean
requestPersistence = fromEffectFnAff requestPersistenceImpl
