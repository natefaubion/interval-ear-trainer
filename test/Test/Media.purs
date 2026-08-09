module Test.Media (run) where

import Prelude

import Data.Array as Array
import Data.Number (abs)
import EarTrainer.Audio as Audio
import EarTrainer.Capability.PitchInput as PitchInput
import EarTrainer.Music (Accidental(..), Letter(..), PlaybackMode(..), pitch)
import EarTrainer.Notation as Notation
import Effect (Effect)
import Test.Assert (assertEqual, assertTrue')

run :: Effect Unit
run = do
  let
    c4 = pitch C (Accidental 0) 4
    e4 = pitch E (Accidental 0) 4
    melodicPlan = Audio.intervalPlan MelodicAscending c4 e4
    harmonicPlan = Audio.intervalPlan Harmonic c4 e4
    rootPlan = Audio.rootPlan c4
    promptScore = Notation.prompt Notation.Full c4 e4 false
    acceptedPromptScore = Notation.prompt Notation.Full c4 e4 true
    choiceScore = Notation.intervalChoice Notation.Compact c4 e4
  assertEqual { actual: PitchInput.decibelsFromRms 1.0, expected: 0.0 }
  assertTrue' "0.1 RMS is -20 dB" (abs (PitchInput.decibelsFromRms 0.1 + 20.0) < 1.0e-9)
  assertTrue' "silence is clamped to -160 dB" (abs (PitchInput.decibelsFromRms 0.0 + 160.0) < 1.0e-9)
  assertEqual { actual: melodicPlan.durationMilliseconds, expected: 1450.0 }
  assertEqual { actual: map _.startMilliseconds melodicPlan.events, expected: [ 0.0, 800.0 ] }
  assertEqual { actual: map _.notes melodicPlan.events, expected: [ [ 60 ], [ 64 ] ] }
  assertEqual { actual: harmonicPlan.durationMilliseconds, expected: 900.0 }
  assertEqual { actual: map _.notes harmonicPlan.events, expected: [ [ 60, 64 ] ] }
  assertEqual { actual: map _.notes rootPlan.events, expected: [ [ 60 ] ] }
  assertTrue' "prompt uses treble clef" (promptScore.clef == Notation.Treble)
  assertTrue' "prompt target is hidden"
    (map _.appearance promptScore.events == [ Notation.Normal, Notation.Hidden ])
  assertTrue' "accepted prompt root is highlighted"
    (map _.appearance acceptedPromptScore.events == [ Notation.Accepted, Notation.Hidden ])
  assertEqual { actual: promptScore.width, expected: 240 }
  assertEqual { actual: choiceScore.width, expected: 240 }
  assertTrue' "prompt notation uses full layout" (promptScore.layout == Notation.Full)
  assertTrue' "answer notation uses compact layout" (choiceScore.layout == Notation.Compact)
  assertEqual { actual: Array.length choiceScore.events, expected: 3 }
  assertEqual
    { actual: map _.key (Array.concatMap _.notes choiceScore.events)
    , expected: [ "c/4", "e/4", "c/4", "e/4" ]
    }
