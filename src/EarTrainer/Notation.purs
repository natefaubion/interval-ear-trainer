module EarTrainer.Notation
  ( renderCompleted
  , renderGhost
  , renderIncorrect
  , renderIntervalChoice
  , renderNotes
  , renderPrompt
  ) where

import Prelude

import Data.Array as Array
import EarTrainer.Music (Accidental(..), Letter(..), Pitch(..), PitchClass(..), midiNumber)
import Effect (Effect)
import Web.DOM.Element (Element)

type EngravedNote =
  { accidental :: String
  , key :: String
  }

type EngravedEvent =
  { appearance :: String
  , notes :: Array EngravedNote
  }

foreign import renderScoreImpl :: Element -> String -> Int -> Array EngravedEvent -> Effect Unit

renderNotes :: Element -> Array Pitch -> Effect Unit
renderNotes element notes =
  renderScoreImpl element (selectClef notes) (if Array.length notes <= 1 then 240 else 300)
    (map (\note -> engravedEvent "normal" [ note ]) notes)

renderPrompt :: Element -> Pitch -> Pitch -> Boolean -> Effect Unit
renderPrompt element root target rootAccepted =
  renderScoreImpl element (selectClef [ root, target ]) 300
    [ engravedEvent (if rootAccepted then "accepted" else "normal") [ root ]
    , engravedEvent "hidden" [ target ]
    ]

renderCompleted :: Element -> Pitch -> Pitch -> Effect Unit
renderCompleted element root target =
  renderScoreImpl element (selectClef [ root, target ]) 300
    [ engravedEvent "accepted" [ root ]
    , engravedEvent "accepted" [ target ]
    ]

renderGhost :: Element -> Pitch -> Pitch -> Pitch -> Boolean -> Effect Unit
renderGhost element root target detected rootAccepted =
  renderScoreImpl element (selectClef [ root, target ]) 300
    [ engravedEvent (if rootAccepted then "accepted" else "normal") [ root ]
    , engravedEvent "dim" [ detected ]
    ]

renderIncorrect :: Element -> Pitch -> Pitch -> Pitch -> Boolean -> Effect Unit
renderIncorrect element root target detected rootAccepted =
  renderScoreImpl element (selectClef [ root, target ]) 300
    [ engravedEvent (if rootAccepted then "accepted" else "normal") [ root ]
    , engravedEvent "incorrect" [ detected ]
    ]

renderIntervalChoice :: Element -> Pitch -> Pitch -> Effect Unit
renderIntervalChoice element root target =
  renderScoreImpl element (selectClef [ root, target ]) 300
    [ engravedEvent "normal" [ root ]
    , engravedEvent "normal" [ target ]
    , engravedEvent "normal" [ root, target ]
    ]

engravedEvent :: String -> Array Pitch -> EngravedEvent
engravedEvent appearance notes = { appearance, notes: map engravedNote notes }

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
