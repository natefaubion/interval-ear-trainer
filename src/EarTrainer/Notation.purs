module EarTrainer.Notation
  ( Appearance(..)
  , Clef(..)
  , CurrentNote
  , OctaveDisplacement(..)
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

data OctaveDisplacement
  = AtPitch
  | OctaveAbove
  | OctaveBelow

derive instance Eq Appearance
derive instance Eq Clef
derive instance Eq NotationLayout
derive instance Eq OctaveDisplacement

type EngravedNote =
  { accidental :: String
  , key :: String
  , octaveDisplacement :: OctaveDisplacement
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
notes layout pitches = do
  let clef = selectClef pitches
  { clef
  , events: map (engravedEvent layout clef Normal <<< Array.singleton) pitches
  , layout
  , width: if Array.length pitches <= 1 then 200 else 240
  }

prompt :: NotationLayout -> Pitch -> Pitch -> Boolean -> Score
prompt layout root target rootAccepted = do
  let clef = selectClef [ root, target ]
  { clef
  , events:
      [ engravedEvent layout clef (if rootAccepted then Accepted else Normal) [ root ]
      , engravedEvent layout clef Hidden [ target ]
      ]
  , layout
  , width: 240
  }

completed :: NotationLayout -> Pitch -> Pitch -> Score
completed layout root target = do
  let clef = selectClef [ root, target ]
  { clef
  , events:
      [ engravedEvent layout clef Accepted [ root ]
      , engravedEvent layout clef Accepted [ target ]
      ]
  , layout
  , width: 240
  }

ghost :: NotationLayout -> Pitch -> Pitch -> Pitch -> Boolean -> Score
ghost layout root target detected rootAccepted = do
  let clef = selectClef [ root, target ]
  { clef
  , events:
      [ engravedEvent layout clef (if rootAccepted then Accepted else Normal) [ root ]
      , detectedEvent layout clef Dim target detected
      ]
  , layout
  , width: 240
  }

incorrect :: NotationLayout -> Pitch -> Pitch -> Pitch -> Boolean -> Score
incorrect layout root target detected rootAccepted = do
  let clef = selectClef [ root, target ]
  { clef
  , events:
      [ engravedEvent layout clef (if rootAccepted then Accepted else Normal) [ root ]
      , detectedEvent layout clef Incorrect target detected
      ]
  , layout
  , width: 240
  }

intervalChoice :: NotationLayout -> Pitch -> Pitch -> Score
intervalChoice layout root target = do
  let clef = selectClef [ root, target ]
  { clef
  , events:
      [ engravedEvent layout clef Normal [ root ]
      , engravedEvent layout clef Normal [ target ]
      , engravedEvent layout clef Normal [ root, target ]
      ]
  , layout
  , width: 240
  }

sequenceScore :: NotationLayout -> NonEmptyArray.NonEmptyArray Pitch -> Int -> Maybe CurrentNote -> Boolean -> Score
sequenceScore layout melody acceptedCount current revealFirst = do
  let pitches = NonEmptyArray.toArray melody
  let clef = selectClef pitches
  { clef
  , events: Array.mapWithIndex (renderSequencePitch layout clef acceptedCount current revealFirst) pitches
  , layout
  , width: max 240 (120 + Array.length pitches * melodyNoteSpacing)
  }

melodyNoteSpacing :: Int
melodyNoteSpacing = 40

renderSequencePitch
  :: NotationLayout
  -> Clef
  -> Int
  -> Maybe CurrentNote
  -> Boolean
  -> Int
  -> Pitch
  -> EngravedEvent
renderSequencePitch layout clef acceptedCount current revealFirst index expected
  | index < acceptedCount = engravedEvent layout clef Accepted [ expected ]
  | index == acceptedCount = case current of
      Just note -> detectedEvent layout clef note.appearance expected note.pitch
      Nothing
        | index == 0 && revealFirst -> engravedEvent layout clef Normal [ expected ]
        | otherwise -> engravedEvent layout clef Hidden [ expected ]
  | otherwise = engravedEvent layout clef Hidden [ expected ]

engravedEvent :: NotationLayout -> Clef -> Appearance -> Array Pitch -> EngravedEvent
engravedEvent layout clef appearance pitches =
  { appearance, notes: map (engravedNote layout clef) pitches }

detectedEvent :: NotationLayout -> Clef -> Appearance -> Pitch -> Pitch -> EngravedEvent
detectedEvent layout clef appearance expected detected = case detectedNote layout clef detected of
  Nothing -> engravedEvent layout clef Hidden [ expected ]
  Just note -> { appearance, notes: [ note ] }

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

engravedNote :: NotationLayout -> Clef -> Pitch -> EngravedNote
engravedNote layout clef pitch = do
  let
    displayed = case displayPitch layout clef pitch of
      Nothing -> { octaveDisplacement: AtPitch, pitch }
      Just value -> value
  let Pitch (PitchClass letter accidental) octave = displayed.pitch
  { accidental: vexAccidental accidental
  , key: vexLetter letter <> "/" <> show octave
  , octaveDisplacement: displayed.octaveDisplacement
  }

detectedNote :: NotationLayout -> Clef -> Pitch -> Maybe EngravedNote
detectedNote layout clef pitch = map engrave (displayPitch layout clef pitch)
  where
  engrave displayed = do
    let Pitch (PitchClass letter accidental) octave = displayed.pitch
    { accidental: vexAccidental accidental
    , key: vexLetter letter <> "/" <> show octave
    , octaveDisplacement: displayed.octaveDisplacement
    }

displayPitch
  :: NotationLayout
  -> Clef
  -> Pitch
  -> Maybe { octaveDisplacement :: OctaveDisplacement, pitch :: Pitch }
displayPitch layout clef pitch = case layout of
  Compact -> Just { octaveDisplacement: AtPitch, pitch }
  Full
    | midiNumber pitch < visibleMinimum clef -> do
        let shifted = shiftOctave 1 pitch
        if midiNumber shifted < visibleMinimum clef then Nothing
        else Just { octaveDisplacement: OctaveBelow, pitch: shifted }
    | midiNumber pitch > visibleMaximum clef -> do
        let shifted = shiftOctave (-1) pitch
        if midiNumber shifted > visibleMaximum clef then Nothing
        else Just { octaveDisplacement: OctaveAbove, pitch: shifted }
    | otherwise -> Just { octaveDisplacement: AtPitch, pitch }

visibleMinimum :: Clef -> Int
visibleMinimum = case _ of
  -- Full notation has a fixed viewport with room for two ledger lines.
  Treble -> 57
  Bass -> 36

visibleMaximum :: Clef -> Int
visibleMaximum = case _ of
  Treble -> 84
  Bass -> 64

shiftOctave :: Int -> Pitch -> Pitch
shiftOctave offset (Pitch pitchClass octave) = Pitch pitchClass (octave + offset)

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
