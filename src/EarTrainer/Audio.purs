module EarTrainer.Audio
  ( NoteEvent
  , PlaybackPlan
  , intervalPlan
  , rootPlan
  ) where

import EarTrainer.Music (Pitch, PlaybackMode(..), midiNumber)

type NoteEvent =
  { durationMilliseconds :: Number
  , notes :: Array Int
  , startMilliseconds :: Number
  }

type PlaybackPlan =
  { durationMilliseconds :: Number
  , events :: Array NoteEvent
  }

intervalPlan :: PlaybackMode -> Pitch -> Pitch -> PlaybackPlan
intervalPlan mode root target = case mode of
  Harmonic ->
    { durationMilliseconds: 900.0
    , events:
        [ { durationMilliseconds: 900.0
          , notes: [ midiNumber root, midiNumber target ]
          , startMilliseconds: 0.0
          }
        ]
    }
  _ ->
    { durationMilliseconds: 1450.0
    , events:
        [ { durationMilliseconds: 650.0
          , notes: [ midiNumber root ]
          , startMilliseconds: 0.0
          }
        , { durationMilliseconds: 650.0
          , notes: [ midiNumber target ]
          , startMilliseconds: 800.0
          }
        ]
    }

rootPlan :: Pitch -> PlaybackPlan
rootPlan root =
  { durationMilliseconds: 900.0
  , events:
      [ { durationMilliseconds: 900.0
        , notes: [ midiNumber root ]
        , startMilliseconds: 0.0
        }
      ]
  }
