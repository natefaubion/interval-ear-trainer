module EarTrainer.Notation
  ( Appearance(..)
  , Clef(..)
  , EngravedEvent
  , EngravedNote
  , Score
  , completed
  , ghost
  , incorrect
  , intervalChoice
  , notes
  , prompt
  ) where

import Prelude

import Data.Array as Array
import EarTrainer.Music (Accidental(..), Letter(..), Pitch(..), PitchClass(..), midiNumber)

data Appearance
  = Normal
  | Dim
  | Accepted
  | Incorrect
  | Hidden

data Clef = Treble | Bass

derive instance Eq Appearance
derive instance Eq Clef

type EngravedNote =
  { accidental :: String
  , key :: String
  }

type EngravedEvent =
  { appearance :: Appearance
  , notes :: Array EngravedNote
  }

type Score =
  { clef :: Clef
  , events :: Array EngravedEvent
  , width :: Int
  }

notes :: Array Pitch -> Score
notes pitches =
  { clef: selectClef pitches
  , events: map (engravedEvent Normal <<< Array.singleton) pitches
  , width: if Array.length pitches <= 1 then 240 else 300
  }

prompt :: Pitch -> Pitch -> Boolean -> Score
prompt root target rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Hidden [ target ]
      ]
  , width: 300
  }

completed :: Pitch -> Pitch -> Score
completed root target =
  { clef: selectClef [ root, target ]
  , events: [ engravedEvent Accepted [ root ], engravedEvent Accepted [ target ] ]
  , width: 300
  }

ghost :: Pitch -> Pitch -> Pitch -> Boolean -> Score
ghost root target detected rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Dim [ detected ]
      ]
  , width: 300
  }

incorrect :: Pitch -> Pitch -> Pitch -> Boolean -> Score
incorrect root target detected rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Incorrect [ detected ]
      ]
  , width: 300
  }

intervalChoice :: Pitch -> Pitch -> Score
intervalChoice root target =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent Normal [ root ]
      , engravedEvent Normal [ target ]
      , engravedEvent Normal [ root, target ]
      ]
  , width: 300
  }

engravedEvent :: Appearance -> Array Pitch -> EngravedEvent
engravedEvent appearance pitches = { appearance, notes: map engravedNote pitches }

selectClef :: Array Pitch -> Clef
selectClef pitches =
  let
    midi = map midiNumber pitches
    lowest = Array.foldl min 127 midi
    highest = Array.foldl max 0 midi
  in
    if highest > 67 then Treble
    else if lowest < 53 then Bass
    else if lowest + highest >= 120 then Treble
    else Bass

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
