module EarTrainer.Music
  ( PlaybackMode(..)
  ) where

import Prelude

data PlaybackMode
  = MelodicAscending
  | MelodicDescending
  | Harmonic

derive instance Eq PlaybackMode
