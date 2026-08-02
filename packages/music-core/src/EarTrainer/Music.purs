module EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRange(..)
  , VocalRangePreset(..)
  , allIntervals
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , accidentalOffset
  , defaultIntervals
  , defaultRootPitchClasses
  , intervalName
  , intervalNumber
  , intervalSemitones
  , midiNumber
  , pitch
  , pitchFromMidi
  , pitchFromMidiLike
  , pitchClassName
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  , transpose
  ) where

import Prelude

import Data.Array as Array
import Data.String.CodeUnits as String

data Letter = C | D | E | F | G | A | B

derive instance Eq Letter
derive instance Ord Letter

data Accidental = Accidental Int

derive instance Eq Accidental
derive instance Ord Accidental

data PitchClass = PitchClass Letter Accidental

derive instance Eq PitchClass
derive instance Ord PitchClass

instance Show PitchClass where
  show = pitchClassName

data Pitch = Pitch PitchClass Int

derive instance Eq Pitch
derive instance Ord Pitch

instance Show Pitch where
  show = pitchName

data Interval
  = PerfectUnison
  | MinorSecond
  | MajorSecond
  | MinorThird
  | MajorThird
  | PerfectFourth
  | AugmentedFourth
  | DiminishedFifth
  | PerfectFifth
  | MinorSixth
  | MajorSixth
  | MinorSeventh
  | MajorSeventh
  | PerfectOctave

derive instance Eq Interval
derive instance Ord Interval

data Direction = Ascending | Descending

derive instance Eq Direction

data PlaybackMode
  = MelodicAscending
  | MelodicDescending
  | Harmonic

derive instance Eq PlaybackMode
derive instance Ord PlaybackMode

data OctavePolicy = AnyOctave | WrittenOctave

derive instance Eq OctavePolicy

data VocalRangePreset
  = Bass
  | Baritone
  | Tenor
  | Alto
  | MezzoSoprano
  | Soprano
  | ExtraWide

derive instance Eq VocalRangePreset
derive instance Ord VocalRangePreset

type VocalRange =
  { low :: Pitch
  , high :: Pitch
  }

allIntervals :: Array Interval
allIntervals =
  [ PerfectUnison
  , MinorSecond
  , MajorSecond
  , MinorThird
  , MajorThird
  , PerfectFourth
  , AugmentedFourth
  , DiminishedFifth
  , PerfectFifth
  , MinorSixth
  , MajorSixth
  , MinorSeventh
  , MajorSeventh
  , PerfectOctave
  ]

defaultIntervals :: Array Interval
defaultIntervals =
  [ MinorThird
  , MajorThird
  , PerfectFourth
  , PerfectFifth
  , PerfectOctave
  ]

allPlaybackModes :: Array PlaybackMode
allPlaybackModes = [ MelodicAscending, MelodicDescending, Harmonic ]

allRootPitchClasses :: Array PitchClass
allRootPitchClasses =
  [ PitchClass C (Accidental 0)
  , PitchClass C (Accidental 1)
  , PitchClass D (Accidental (-1))
  , PitchClass D (Accidental 0)
  , PitchClass D (Accidental 1)
  , PitchClass E (Accidental (-1))
  , PitchClass E (Accidental 0)
  , PitchClass F (Accidental 0)
  , PitchClass F (Accidental 1)
  , PitchClass G (Accidental (-1))
  , PitchClass G (Accidental 0)
  , PitchClass G (Accidental 1)
  , PitchClass A (Accidental (-1))
  , PitchClass A (Accidental 0)
  , PitchClass A (Accidental 1)
  , PitchClass B (Accidental (-1))
  , PitchClass B (Accidental 0)
  ]

defaultRootPitchClasses :: Array PitchClass
defaultRootPitchClasses = allRootPitchClasses

allVocalRangePresets :: Array VocalRangePreset
allVocalRangePresets = [ Bass, Baritone, Tenor, Alto, MezzoSoprano, Soprano, ExtraWide ]

pitch :: Letter -> Accidental -> Int -> Pitch
pitch letter accidental octave = Pitch (PitchClass letter accidental) octave

pitchFromMidi :: Int -> Pitch
pitchFromMidi midi =
  let
    octave = midi `div` 12 - 1
  in
    case midi `mod` 12 of
      0 -> pitch C (Accidental 0) octave
      1 -> pitch C (Accidental 1) octave
      2 -> pitch D (Accidental 0) octave
      3 -> pitch D (Accidental 1) octave
      4 -> pitch E (Accidental 0) octave
      5 -> pitch F (Accidental 0) octave
      6 -> pitch F (Accidental 1) octave
      7 -> pitch G (Accidental 0) octave
      8 -> pitch G (Accidental 1) octave
      9 -> pitch A (Accidental 0) octave
      10 -> pitch A (Accidental 1) octave
      _ -> pitch B (Accidental 0) octave

pitchFromMidiLike :: Pitch -> Int -> Pitch
pitchFromMidiLike (Pitch (PitchClass _ (Accidental referenceAccidental)) _) midi
  | referenceAccidental < 0 =
      let
        octave = midi `div` 12 - 1
      in
        case midi `mod` 12 of
          0 -> pitch C (Accidental 0) octave
          1 -> pitch D (Accidental (-1)) octave
          2 -> pitch D (Accidental 0) octave
          3 -> pitch E (Accidental (-1)) octave
          4 -> pitch E (Accidental 0) octave
          5 -> pitch F (Accidental 0) octave
          6 -> pitch G (Accidental (-1)) octave
          7 -> pitch G (Accidental 0) octave
          8 -> pitch A (Accidental (-1)) octave
          9 -> pitch A (Accidental 0) octave
          10 -> pitch B (Accidental (-1)) octave
          _ -> pitch B (Accidental 0) octave
  | otherwise = pitchFromMidi midi

accidentalOffset :: Accidental -> Int
accidentalOffset (Accidental offset) = offset

letterIndex :: Letter -> Int
letterIndex C = 0
letterIndex D = 1
letterIndex E = 2
letterIndex F = 3
letterIndex G = 4
letterIndex A = 5
letterIndex B = 6

letterAt :: Int -> Letter
letterAt index = case index `mod` 7 of
  0 -> C
  1 -> D
  2 -> E
  3 -> F
  4 -> G
  5 -> A
  _ -> B

letterSemitones :: Letter -> Int
letterSemitones C = 0
letterSemitones D = 2
letterSemitones E = 4
letterSemitones F = 5
letterSemitones G = 7
letterSemitones A = 9
letterSemitones B = 11

midiNumber :: Pitch -> Int
midiNumber (Pitch (PitchClass letter accidental) octave) =
  12 * (octave + 1) + letterSemitones letter + accidentalOffset accidental

intervalSemitones :: Interval -> Int
intervalSemitones PerfectUnison = 0
intervalSemitones MinorSecond = 1
intervalSemitones MajorSecond = 2
intervalSemitones MinorThird = 3
intervalSemitones MajorThird = 4
intervalSemitones PerfectFourth = 5
intervalSemitones AugmentedFourth = 6
intervalSemitones DiminishedFifth = 6
intervalSemitones PerfectFifth = 7
intervalSemitones MinorSixth = 8
intervalSemitones MajorSixth = 9
intervalSemitones MinorSeventh = 10
intervalSemitones MajorSeventh = 11
intervalSemitones PerfectOctave = 12

intervalNumber :: Interval -> Int
intervalNumber PerfectUnison = 1
intervalNumber MinorSecond = 2
intervalNumber MajorSecond = 2
intervalNumber MinorThird = 3
intervalNumber MajorThird = 3
intervalNumber PerfectFourth = 4
intervalNumber AugmentedFourth = 4
intervalNumber DiminishedFifth = 5
intervalNumber PerfectFifth = 5
intervalNumber MinorSixth = 6
intervalNumber MajorSixth = 6
intervalNumber MinorSeventh = 7
intervalNumber MajorSeventh = 7
intervalNumber PerfectOctave = 8

intervalName :: Interval -> String
intervalName PerfectUnison = "Perfect unison"
intervalName MinorSecond = "Minor second"
intervalName MajorSecond = "Major second"
intervalName MinorThird = "Minor third"
intervalName MajorThird = "Major third"
intervalName PerfectFourth = "Perfect fourth"
intervalName AugmentedFourth = "Augmented fourth"
intervalName DiminishedFifth = "Diminished fifth"
intervalName PerfectFifth = "Perfect fifth"
intervalName MinorSixth = "Minor sixth"
intervalName MajorSixth = "Major sixth"
intervalName MinorSeventh = "Minor seventh"
intervalName MajorSeventh = "Major seventh"
intervalName PerfectOctave = "Perfect octave"

playbackModeName :: PlaybackMode -> String
playbackModeName MelodicAscending = "Melodic ascending"
playbackModeName MelodicDescending = "Melodic descending"
playbackModeName Harmonic = "Harmonic"

transpose :: Direction -> Interval -> Pitch -> Pitch
transpose direction interval root@(Pitch (PitchClass rootLetter _) rootOctave) =
  let
    sign = case direction of
      Ascending -> 1
      Descending -> -1
    targetDiatonic = rootOctave * 7 + letterIndex rootLetter + sign * (intervalNumber interval - 1)
    targetLetter = letterAt targetDiatonic
    targetOctave = targetDiatonic `div` 7
    targetMidi = midiNumber root + sign * intervalSemitones interval
    naturalTargetMidi = midiNumber (pitch targetLetter (Accidental 0) targetOctave)
  in
    pitch targetLetter (Accidental (targetMidi - naturalTargetMidi)) targetOctave

letterName :: Letter -> String
letterName C = "C"
letterName D = "D"
letterName E = "E"
letterName F = "F"
letterName G = "G"
letterName A = "A"
letterName B = "B"

accidentalName :: Accidental -> String
accidentalName (Accidental offset)
  | offset == 0 = ""
  | offset > 0 = String.fromCharArray (Array.replicate offset '♯')
  | otherwise = String.fromCharArray (Array.replicate (-offset) '♭')

pitchClassName :: PitchClass -> String
pitchClassName (PitchClass letter accidental) = letterName letter <> accidentalName accidental

pitchName :: Pitch -> String
pitchName (Pitch pitchClass octave) = pitchClassName pitchClass <> show octave

presetName :: VocalRangePreset -> String
presetName Bass = "Bass"
presetName Baritone = "Baritone"
presetName Tenor = "Tenor"
presetName Alto = "Alto"
presetName MezzoSoprano = "Mezzo-soprano"
presetName Soprano = "Soprano"
presetName ExtraWide = "Extra wide"

presetRange :: VocalRangePreset -> VocalRange
presetRange Bass = { low: pitch E (Accidental 0) 2, high: pitch E (Accidental 0) 4 }
presetRange Baritone = { low: pitch A (Accidental 0) 2, high: pitch A (Accidental 0) 4 }
presetRange Tenor = { low: pitch C (Accidental 0) 3, high: pitch C (Accidental 0) 5 }
presetRange Alto = { low: pitch F (Accidental 0) 3, high: pitch F (Accidental 0) 5 }
presetRange MezzoSoprano = { low: pitch A (Accidental 0) 3, high: pitch A (Accidental 0) 5 }
presetRange Soprano = { low: pitch C (Accidental 0) 4, high: pitch C (Accidental 0) 6 }
presetRange ExtraWide = { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
