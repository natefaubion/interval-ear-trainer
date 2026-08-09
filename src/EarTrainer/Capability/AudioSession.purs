module EarTrainer.Capability.AudioSession
  ( AudioSessionType(..)
  , setType
  ) where

import Prelude

import Effect (Effect)

data AudioSessionType
  = Playback
  | PlayAndRecord

foreign import setTypeImpl :: String -> Effect Unit

setType :: AudioSessionType -> Effect Unit
setType sessionType = setTypeImpl case sessionType of
  Playback -> "playback"
  PlayAndRecord -> "play-and-record"
