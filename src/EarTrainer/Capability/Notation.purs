module EarTrainer.Capability.Notation
  ( render
  ) where

import Prelude

import EarTrainer.Notation (Appearance(..), Clef(..), EngravedEvent, EngravedNote, Score)
import Effect (Effect)
import Web.DOM.Element (Element)

type ForeignEvent =
  { color :: String
  , hidden :: Boolean
  , highlighted :: Boolean
  , notes :: Array EngravedNote
  }

foreign import renderScoreImpl :: Element -> String -> Int -> Array ForeignEvent -> Effect Unit

render :: Element -> Score -> Effect Unit
render element score =
  renderScoreImpl element (encodeClef score.clef) score.width
    (map encodeEvent score.events)

encodeClef :: Clef -> String
encodeClef = case _ of
  Treble -> "treble"
  Bass -> "bass"

encodeEvent :: EngravedEvent -> ForeignEvent
encodeEvent event = case event.appearance of
  Normal -> foreignEvent "" false false
  Dim -> foreignEvent "#aeb4b0" false false
  Accepted -> foreignEvent "#20a65a" false true
  Incorrect -> foreignEvent "#b83b35" false true
  Hidden -> foreignEvent "transparent" true false
  where
  foreignEvent color hidden highlighted =
    { color, hidden, highlighted, notes: event.notes }
