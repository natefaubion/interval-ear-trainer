module Test.Media (run) where

import Prelude

import Data.Array as Array
import Data.Number (abs)
import EarTrainer.Audio as Audio
import EarTrainer.Capability.PitchInput as PitchInput
import EarTrainer.Music (Accidental(..), Letter(..), PlaybackMode(..), pitch)
import EarTrainer.Notation as Notation
import Effect (Effect)
import Test.Assert (assertEqual)

run :: Effect Unit
run = do
  let
    c4 = pitch C (Accidental 0) 4
    e4 = pitch E (Accidental 0) 4
    melodicPlan = Audio.intervalPlan MelodicAscending c4 e4
    harmonicPlan = Audio.intervalPlan Harmonic c4 e4
    rootPlan = Audio.rootPlan c4
    promptScore = Notation.prompt c4 e4 false
    acceptedPromptScore = Notation.prompt c4 e4 true
    choiceScore = Notation.intervalChoice c4 e4
  assertEqual
    { actual:
        [ PitchInput.decibelsFromRms 1.0 == 0.0
        , abs (PitchInput.decibelsFromRms 0.1 + 20.0) < 1.0e-9
        , abs (PitchInput.decibelsFromRms 0.0 + 160.0) < 1.0e-9
        ]
    , expected: [ true, true, true ]
    }
  assertEqual
    { actual:
        [ melodicPlan.durationMilliseconds == 1450.0
        , map _.startMilliseconds melodicPlan.events == [ 0.0, 800.0 ]
        , map _.notes melodicPlan.events == [ [ 60 ], [ 64 ] ]
        , harmonicPlan.durationMilliseconds == 900.0
        , map _.notes harmonicPlan.events == [ [ 60, 64 ] ]
        , map _.notes rootPlan.events == [ [ 60 ] ]
        ]
    , expected: Array.replicate 6 true
    }
  assertEqual
    { actual:
        [ promptScore.clef == Notation.Treble
        , map _.appearance promptScore.events == [ Notation.Normal, Notation.Hidden ]
        , map _.appearance acceptedPromptScore.events == [ Notation.Accepted, Notation.Hidden ]
        , Array.length choiceScore.events == 3
        , map _.key (Array.concatMap _.notes choiceScore.events) == [ "c/4", "e/4", "c/4", "e/4" ]
        ]
    , expected: Array.replicate 5 true
    }
