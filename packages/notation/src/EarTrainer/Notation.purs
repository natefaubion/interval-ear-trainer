module EarTrainer.Notation
  ( renderNotes
  ) where

import Prelude

import Effect (Effect)
import EarTrainer.Music (Accidental(..), Letter(..), Pitch(..), PitchClass(..))
import Web.DOM.Element (Element)

type EngravedNote =
  { accidental :: String
  , key :: String
  }

foreign import renderNotesImpl :: Element -> Array EngravedNote -> Effect Unit

renderNotes :: Element -> Array Pitch -> Effect Unit
renderNotes element notes = renderNotesImpl element (map engravedNote notes)

engravedNote :: Pitch -> EngravedNote
engravedNote (Pitch (PitchClass letter accidental) octave) =
  { accidental: vexAccidental accidental
  , key: vexLetter letter <> "/" <> show octave
  }

vexLetter :: Letter -> String
vexLetter C = "c"
vexLetter D = "d"
vexLetter E = "e"
vexLetter F = "f"
vexLetter G = "g"
vexLetter A = "a"
vexLetter B = "b"

vexAccidental :: Accidental -> String
vexAccidental (Accidental (-2)) = "bb"
vexAccidental (Accidental (-1)) = "b"
vexAccidental (Accidental 1) = "#"
vexAccidental (Accidental 2) = "##"
vexAccidental _ = ""
