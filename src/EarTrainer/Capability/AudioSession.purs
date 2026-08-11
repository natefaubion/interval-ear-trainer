module EarTrainer.Capability.AudioSession
  ( AudioSessionType(..)
  , setType
  , withPlayback
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, finally)
import Effect.Class (liftEffect)

data AudioSessionType
  = Auto
  | Playback
  | PlayAndRecord

foreign import setTypeImpl :: String -> Effect Unit

setType :: AudioSessionType -> Effect Unit
setType sessionType = setTypeImpl case sessionType of
  Auto -> "auto"
  Playback -> "playback"
  PlayAndRecord -> "play-and-record"

withPlayback :: forall a. Aff a -> Aff a
withPlayback action = finally (liftEffect (setType Auto)) do
  liftEffect (setType Playback)
  action
