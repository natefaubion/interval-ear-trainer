module EarTrainer.Notation
  ( Appearance(..)
  , Clef(..)
  , CurrentNote
  , NotationLayout(..)
  , EngravedEvent
  , EngravedNote
  , Score
  , completed
  , ghost
  , incorrect
  , intervalChoice
  , sequenceScore
  , notes
  , prompt
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import EarTrainer.Music (Accidental(..), Letter(..), Pitch(..), PitchClass(..), midiNumber)

data Appearance
  = Normal
  | Dim
  | Accepted
  | Incorrect
  | Hidden

data Clef = Treble | Bass

data NotationLayout = Full | Compact

derive instance Eq Appearance
derive instance Eq Clef
derive instance Eq NotationLayout

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
  , layout :: NotationLayout
  , width :: Int
  }

type CurrentNote =
  { appearance :: Appearance
  , pitch :: Pitch
  }

notes :: NotationLayout -> Array Pitch -> Score
notes layout pitches =
  { clef: selectClef pitches
  , events: map (engravedEvent Normal <<< Array.singleton) pitches
  , layout
  , width: if Array.length pitches <= 1 then 200 else 240
  }

prompt :: NotationLayout -> Pitch -> Pitch -> Boolean -> Score
prompt layout root target rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Hidden [ target ]
      ]
  , layout
  , width: 240
  }

completed :: NotationLayout -> Pitch -> Pitch -> Score
completed layout root target =
  { clef: selectClef [ root, target ]
  , events: [ engravedEvent Accepted [ root ], engravedEvent Accepted [ target ] ]
  , layout
  , width: 240
  }

ghost :: NotationLayout -> Pitch -> Pitch -> Pitch -> Boolean -> Score
ghost layout root target detected rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Dim [ detected ]
      ]
  , layout
  , width: 240
  }

incorrect :: NotationLayout -> Pitch -> Pitch -> Pitch -> Boolean -> Score
incorrect layout root target detected rootAccepted =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent Incorrect [ detected ]
      ]
  , layout
  , width: 240
  }

intervalChoice :: NotationLayout -> Pitch -> Pitch -> Score
intervalChoice layout root target =
  { clef: selectClef [ root, target ]
  , events:
      [ engravedEvent Normal [ root ]
      , engravedEvent Normal [ target ]
      , engravedEvent Normal [ root, target ]
      ]
  , layout
  , width: 240
  }

sequenceScore :: NotationLayout -> NonEmptyArray.NonEmptyArray Pitch -> Int -> Maybe CurrentNote -> Boolean -> Score
sequenceScore layout melody acceptedCount current revealFirst = do
  let pitches = NonEmptyArray.toArray melody
  { clef: selectClef pitches
  , events: Array.mapWithIndex renderPitch pitches
  , layout
  , width: max 240 (120 + Array.length pitches * 52)
  }
  where
  renderPitch index expected
    | index < acceptedCount = engravedEvent Accepted [ expected ]
    | index == acceptedCount = case current of
        Just note -> engravedEvent note.appearance [ note.pitch ]
        Nothing
          | index == 0 && revealFirst -> engravedEvent Normal [ expected ]
          | otherwise -> engravedEvent Hidden [ expected ]
    | otherwise = engravedEvent Hidden [ expected ]

engravedEvent :: Appearance -> Array Pitch -> EngravedEvent
engravedEvent appearance pitches = { appearance, notes: map engravedNote pitches }

selectClef :: Array Pitch -> Clef
selectClef pitches = do
  let
    midi = map midiNumber pitches
    lowest = Array.foldl min 127 midi
    highest = Array.foldl max 0 midi
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
vexLetter = case _ of
  C -> "c"
  D -> "d"
  E -> "e"
  F -> "f"
  G -> "g"
  A -> "a"
  B -> "b"

vexAccidental :: Accidental -> String
vexAccidental = case _ of
  Accidental (-2) -> "bb"
  Accidental (-1) -> "b"
  Accidental 1 -> "#"
  Accidental 2 -> "##"
  _ -> ""
