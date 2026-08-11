module EarTrainer.Capability.Notation
  ( render
  ) where

import Prelude

import EarTrainer.Notation (Appearance(..), Clef(..), EngravedEvent, EngravedNote, NotationLayout(..), OctaveDisplacement(..), Score)
import Effect (Effect)
import Web.DOM.Element (Element)

type ForeignEvent =
  { color :: String
  , hidden :: Boolean
  , highlighted :: Boolean
  , notes :: Array ForeignNote
  }

type ForeignNote =
  { accidental :: String
  , key :: String
  , octaveMark :: String
  , octavePosition :: String
  }

foreign import renderScoreImpl :: Element -> String -> String -> Int -> Array ForeignEvent -> Effect Unit

render :: Element -> Score -> Effect Unit
render element score =
  renderScoreImpl element (encodeLayout score.layout) (encodeClef score.clef) score.width
    (map encodeEvent score.events)

encodeLayout :: NotationLayout -> String
encodeLayout = case _ of
  Full -> "full"
  Compact -> "compact"

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
    { color, hidden, highlighted, notes: map encodeNote event.notes }

encodeNote :: EngravedNote -> ForeignNote
encodeNote note = case note.octaveDisplacement of
  AtPitch -> foreignNote "" "top"
  OctaveAbove -> foreignNote "8va" "top"
  OctaveBelow -> foreignNote "8vb" "bottom"
  where
  foreignNote mark octavePosition =
    { accidental: note.accidental, key: note.key, octaveMark: mark, octavePosition }
