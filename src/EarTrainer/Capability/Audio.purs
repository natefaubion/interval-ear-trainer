module EarTrainer.Capability.Audio
  ( SamplerConfig
  , Sampler
  , createSampler
  , defaultSamplerConfig
  , play
  , start
  , stop
  ) where

import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio (NoteEvent, PlaybackPlan)
import EarTrainer.Capability.AudioSession as AudioSession
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude (Unit)

type SamplerConfig =
  { releaseMilliseconds :: Milliseconds
  }

foreign import data Sampler :: Type

foreign import createSampler :: SamplerConfig -> Effect Sampler
foreign import playImpl :: Sampler -> Array NoteEvent -> Number -> EffectFnAff Unit
foreign import startImpl :: EffectFnAff Unit
foreign import stop :: Sampler -> Effect Unit

defaultSamplerConfig :: SamplerConfig
defaultSamplerConfig =
  { releaseMilliseconds: Milliseconds 200.0
  }

play :: Sampler -> PlaybackPlan -> Aff Unit
play sampler plan = AudioSession.withPlayback do
  fromEffectFnAff (playImpl sampler plan.events plan.durationMilliseconds)

start :: Aff Unit
start = fromEffectFnAff startImpl
