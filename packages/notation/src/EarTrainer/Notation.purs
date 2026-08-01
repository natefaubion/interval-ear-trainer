module EarTrainer.Notation
  ( renderGhost
  , renderIntervalChoice
  , renderNotes
  , renderPrompt
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import EarTrainer.Music (Accidental(..), Letter(..), Pitch(..), PitchClass(..), midiNumber)
import Web.DOM.Element (Element)

type EngravedNote =
  { accidental :: String
  , key :: String
  }

type EngravedEvent =
  { dim :: Boolean
  , notes :: Array EngravedNote
  }

foreign import renderScoreImpl :: Element -> String -> Int -> Array EngravedEvent -> Effect Unit

renderNotes :: Element -> Array Pitch -> Effect Unit
renderNotes element notes =
  renderScoreImpl element (selectClef notes) (if Array.length notes <= 1 then 280 else 360)
    (map (\note -> engravedEvent false [ note ]) notes)

renderPrompt :: Element -> Pitch -> Pitch -> Effect Unit
renderPrompt element root target =
  renderScoreImpl element (selectClef [ root, target ]) 280
    [ engravedEvent false [ root ] ]

renderGhost :: Element -> Pitch -> Pitch -> Pitch -> Effect Unit
renderGhost element root target detected =
  renderScoreImpl element (selectClef [ root, target ]) 360
    [ engravedEvent false [ root ]
    , engravedEvent true [ detected ]
    ]

renderIntervalChoice :: Element -> Pitch -> Pitch -> Effect Unit
renderIntervalChoice element root target =
  renderScoreImpl element (selectClef [ root, target ]) 430
    [ engravedEvent false [ root, target ]
    , engravedEvent false [ root ]
    , engravedEvent false [ target ]
    ]

engravedEvent :: Boolean -> Array Pitch -> EngravedEvent
engravedEvent dim notes = { dim, notes: map engravedNote notes }

selectClef :: Array Pitch -> String
selectClef notes =
  let
    midi = map midiNumber notes
    lowest = Array.foldl min 127 midi
    highest = Array.foldl max 0 midi
  in
    if highest > 67 then "treble"
    else if lowest < 53 then "bass"
    else if lowest + highest >= 120 then "treble"
    else "bass"

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
