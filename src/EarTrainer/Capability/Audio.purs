module EarTrainer.Capability.Audio
  ( Sampler
  , createSampler
  , play
  , start
  , stop
  ) where

import EarTrainer.Audio (NoteEvent, PlaybackPlan)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude (Unit)

foreign import data Sampler :: Type

foreign import createSampler :: Effect Sampler
foreign import playImpl :: Sampler -> Array NoteEvent -> Number -> EffectFnAff Unit
foreign import startImpl :: EffectFnAff Unit
foreign import stop :: Sampler -> Effect Unit

play :: Sampler -> PlaybackPlan -> Aff Unit
play sampler plan = fromEffectFnAff (playImpl sampler plan.events plan.durationMilliseconds)

start :: Aff Unit
start = fromEffectFnAff startImpl
