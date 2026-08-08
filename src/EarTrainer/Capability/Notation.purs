module EarTrainer.Capability.Notation
  ( render
  ) where

import Prelude

import EarTrainer.Notation (Appearance(..), Clef(..), EngravedNote, Score)
import Effect (Effect)
import Web.DOM.Element (Element)

type ForeignEvent =
  { appearance :: String
  , notes :: Array EngravedNote
  }

foreign import renderScoreImpl :: Element -> String -> Int -> Array ForeignEvent -> Effect Unit

render :: Element -> Score -> Effect Unit
render element score =
  renderScoreImpl element (encodeClef score.clef) score.width
    (map (\event -> { appearance: encodeAppearance event.appearance, notes: event.notes }) score.events)

encodeClef :: Clef -> String
encodeClef Treble = "treble"
encodeClef Bass = "bass"

encodeAppearance :: Appearance -> String
encodeAppearance Normal = "normal"
encodeAppearance Dim = "dim"
encodeAppearance Accepted = "accepted"
encodeAppearance Incorrect = "incorrect"
encodeAppearance Hidden = "hidden"
