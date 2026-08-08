module EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , IntervalSize(..)
  , Letter(..)
  , MajorKeyPreset
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRange(..)
  , VocalRangePreset(..)
  , allIntervals
  , allIntervalSizes
  , allMajorKeyPresets
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , accidentalOffset
  , defaultIntervals
  , defaultRootPitchClasses
  , intervalName
  , intervalBetween
  , intervalNumber
  , intervalSizeName
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
import Data.Maybe (Maybe(..))
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
  | AugmentedFifth
  | MinorSixth
  | MajorSixth
  | MinorSeventh
  | MajorSeventh
  | PerfectOctave

derive instance Eq Interval
derive instance Ord Interval

data IntervalSize
  = SizeUnison
  | SizeSecond
  | SizeThird
  | SizeFourth
  | SizeFifth
  | SizeSixth
  | SizeSeventh
  | SizeOctave

derive instance Eq IntervalSize
derive instance Ord IntervalSize

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
  | Custom

derive instance Eq VocalRangePreset
derive instance Ord VocalRangePreset

type VocalRange =
  { low :: Pitch
  , high :: Pitch
  }

type MajorKeyPreset =
  { id :: String
  , name :: String
  , roots :: Array PitchClass
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
  , AugmentedFifth
  , MinorSixth
  , MajorSixth
  , MinorSeventh
  , MajorSeventh
  , PerfectOctave
  ]

allIntervalSizes :: Array IntervalSize
allIntervalSizes =
  [ SizeUnison
  , SizeSecond
  , SizeThird
  , SizeFourth
  , SizeFifth
  , SizeSixth
  , SizeSeventh
  , SizeOctave
  ]

defaultIntervals :: Array Interval
defaultIntervals =
  [ MajorThird
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
  , PitchClass C (Accidental (-1))
  , PitchClass F (Accidental (-1))
  , PitchClass E (Accidental 1)
  , PitchClass B (Accidental 1)
  ]

defaultRootPitchClasses :: Array PitchClass
defaultRootPitchClasses =
  [ PitchClass C (Accidental 0)
  , PitchClass D (Accidental 0)
  , PitchClass E (Accidental 0)
  , PitchClass F (Accidental 0)
  , PitchClass G (Accidental 0)
  , PitchClass A (Accidental 0)
  , PitchClass B (Accidental 0)
  ]

allMajorKeyPresets :: Array MajorKeyPreset
allMajorKeyPresets =
  [ majorKey "c-flat" "C♭ major"
      [ pc C (-1), pc D (-1), pc E (-1), pc F (-1), pc G (-1), pc A (-1), pc B (-1) ]
  , majorKey "g-flat" "G♭ major"
      [ pc G (-1), pc A (-1), pc B (-1), pc C (-1), pc D (-1), pc E (-1), pc F 0 ]
  , majorKey "d-flat" "D♭ major"
      [ pc D (-1), pc E (-1), pc F 0, pc G (-1), pc A (-1), pc B (-1), pc C 0 ]
  , majorKey "a-flat" "A♭ major"
      [ pc A (-1), pc B (-1), pc C 0, pc D (-1), pc E (-1), pc F 0, pc G 0 ]
  , majorKey "e-flat" "E♭ major"
      [ pc E (-1), pc F 0, pc G 0, pc A (-1), pc B (-1), pc C 0, pc D 0 ]
  , majorKey "b-flat" "B♭ major"
      [ pc B (-1), pc C 0, pc D 0, pc E (-1), pc F 0, pc G 0, pc A 0 ]
  , majorKey "f" "F major"
      [ pc F 0, pc G 0, pc A 0, pc B (-1), pc C 0, pc D 0, pc E 0 ]
  , majorKey "c" "C major"
      [ pc C 0, pc D 0, pc E 0, pc F 0, pc G 0, pc A 0, pc B 0 ]
  , majorKey "g" "G major"
      [ pc G 0, pc A 0, pc B 0, pc C 0, pc D 0, pc E 0, pc F 1 ]
  , majorKey "d" "D major"
      [ pc D 0, pc E 0, pc F 1, pc G 0, pc A 0, pc B 0, pc C 1 ]
  , majorKey "a" "A major"
      [ pc A 0, pc B 0, pc C 1, pc D 0, pc E 0, pc F 1, pc G 1 ]
  , majorKey "e" "E major"
      [ pc E 0, pc F 1, pc G 1, pc A 0, pc B 0, pc C 1, pc D 1 ]
  , majorKey "b" "B major"
      [ pc B 0, pc C 1, pc D 1, pc E 0, pc F 1, pc G 1, pc A 1 ]
  , majorKey "f-sharp" "F♯ major"
      [ pc F 1, pc G 1, pc A 1, pc B 0, pc C 1, pc D 1, pc E 1 ]
  , majorKey "c-sharp" "C♯ major"
      [ pc C 1, pc D 1, pc E 1, pc F 1, pc G 1, pc A 1, pc B 1 ]
  ]

majorKey :: String -> String -> Array PitchClass -> MajorKeyPreset
majorKey id name roots = { id, name, roots }

pc :: Letter -> Int -> PitchClass
pc letter accidental = PitchClass letter (Accidental accidental)

allVocalRangePresets :: Array VocalRangePreset
allVocalRangePresets = [ Bass, Baritone, Tenor, Alto, MezzoSoprano, Soprano, ExtraWide, Custom ]

pitch :: Letter -> Accidental -> Int -> Pitch
pitch letter accidental octave = Pitch (PitchClass letter accidental) octave

pitchFromMidi :: Int -> Pitch
pitchFromMidi midi = do
  let
    octave = midi `div` 12 - 1
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
  | referenceAccidental < 0 = do
      let
        octave = midi `div` 12 - 1
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
letterIndex = case _ of
  C -> 0
  D -> 1
  E -> 2
  F -> 3
  G -> 4
  A -> 5
  B -> 6

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
letterSemitones = case _ of
  C -> 0
  D -> 2
  E -> 4
  F -> 5
  G -> 7
  A -> 9
  B -> 11

midiNumber :: Pitch -> Int
midiNumber (Pitch (PitchClass letter accidental) octave) =
  12 * (octave + 1) + letterSemitones letter + accidentalOffset accidental

intervalSemitones :: Interval -> Int
intervalSemitones = case _ of
  PerfectUnison -> 0
  MinorSecond -> 1
  MajorSecond -> 2
  MinorThird -> 3
  MajorThird -> 4
  PerfectFourth -> 5
  AugmentedFourth -> 6
  DiminishedFifth -> 6
  PerfectFifth -> 7
  AugmentedFifth -> 8
  MinorSixth -> 8
  MajorSixth -> 9
  MinorSeventh -> 10
  MajorSeventh -> 11
  PerfectOctave -> 12

intervalNumber :: Interval -> Int
intervalNumber = case _ of
  PerfectUnison -> 1
  MinorSecond -> 2
  MajorSecond -> 2
  MinorThird -> 3
  MajorThird -> 3
  PerfectFourth -> 4
  AugmentedFourth -> 4
  DiminishedFifth -> 5
  PerfectFifth -> 5
  AugmentedFifth -> 5
  MinorSixth -> 6
  MajorSixth -> 6
  MinorSeventh -> 7
  MajorSeventh -> 7
  PerfectOctave -> 8

intervalSizeName :: IntervalSize -> String
intervalSizeName = case _ of
  SizeUnison -> "Unison"
  SizeSecond -> "Second"
  SizeThird -> "Third"
  SizeFourth -> "Fourth"
  SizeFifth -> "Fifth"
  SizeSixth -> "Sixth"
  SizeSeventh -> "Seventh"
  SizeOctave -> "Octave"

intervalBetween :: Direction -> Pitch -> Pitch -> Maybe Interval
intervalBetween direction rootPitch@(Pitch (PitchClass rootLetter _) rootOctave) rootTarget@(Pitch (PitchClass targetLetter _) targetOctave) = do
  let
    generic = case direction of
      Ascending -> (targetOctave - rootOctave) * 7 + letterIndex targetLetter - letterIndex rootLetter + 1
      Descending -> (rootOctave - targetOctave) * 7 + letterIndex rootLetter - letterIndex targetLetter + 1
    semitones = case direction of
      Ascending -> midiNumber rootTarget - midiNumber rootPitch
      Descending -> midiNumber rootPitch - midiNumber rootTarget
  case generic, semitones of
    1, 0 -> Just PerfectUnison
    2, 1 -> Just MinorSecond
    2, 2 -> Just MajorSecond
    3, 3 -> Just MinorThird
    3, 4 -> Just MajorThird
    4, 5 -> Just PerfectFourth
    4, 6 -> Just AugmentedFourth
    5, 6 -> Just DiminishedFifth
    5, 7 -> Just PerfectFifth
    5, 8 -> Just AugmentedFifth
    6, 8 -> Just MinorSixth
    6, 9 -> Just MajorSixth
    7, 10 -> Just MinorSeventh
    7, 11 -> Just MajorSeventh
    8, 12 -> Just PerfectOctave
    _, _ -> Nothing

intervalName :: Interval -> String
intervalName = case _ of
  PerfectUnison -> "Perfect unison"
  MinorSecond -> "Minor second"
  MajorSecond -> "Major second"
  MinorThird -> "Minor third"
  MajorThird -> "Major third"
  PerfectFourth -> "Perfect fourth"
  AugmentedFourth -> "Augmented fourth"
  DiminishedFifth -> "Diminished fifth"
  PerfectFifth -> "Perfect fifth"
  AugmentedFifth -> "Augmented fifth"
  MinorSixth -> "Minor sixth"
  MajorSixth -> "Major sixth"
  MinorSeventh -> "Minor seventh"
  MajorSeventh -> "Major seventh"
  PerfectOctave -> "Perfect octave"

playbackModeName :: PlaybackMode -> String
playbackModeName = case _ of
  MelodicAscending -> "Ascending"
  MelodicDescending -> "Descending"
  Harmonic -> "Harmonic"

transpose :: Direction -> Interval -> Pitch -> Pitch
transpose direction interval root@(Pitch (PitchClass rootLetter _) rootOctave) = do
  let
    sign = case direction of
      Ascending -> 1
      Descending -> -1
    targetDiatonic = rootOctave * 7 + letterIndex rootLetter + sign * (intervalNumber interval - 1)
    targetLetter = letterAt targetDiatonic
    targetOctave = targetDiatonic `div` 7
    targetMidi = midiNumber root + sign * intervalSemitones interval
    naturalTargetMidi = midiNumber (pitch targetLetter (Accidental 0) targetOctave)
  pitch targetLetter (Accidental (targetMidi - naturalTargetMidi)) targetOctave

letterName :: Letter -> String
letterName = case _ of
  C -> "C"
  D -> "D"
  E -> "E"
  F -> "F"
  G -> "G"
  A -> "A"
  B -> "B"

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
presetName = case _ of
  Bass -> "Bass"
  Baritone -> "Baritone"
  Tenor -> "Tenor"
  Alto -> "Alto"
  MezzoSoprano -> "Mezzo-soprano"
  Soprano -> "Soprano"
  ExtraWide -> "Extra wide"
  Custom -> "Custom"

presetRange :: VocalRangePreset -> VocalRange
presetRange = case _ of
  Bass -> { low: pitch E (Accidental 0) 2, high: pitch E (Accidental 0) 4 }
  Baritone -> { low: pitch A (Accidental 0) 2, high: pitch A (Accidental 0) 4 }
  Tenor -> { low: pitch C (Accidental 0) 3, high: pitch C (Accidental 0) 5 }
  Alto -> { low: pitch F (Accidental 0) 3, high: pitch F (Accidental 0) 5 }
  MezzoSoprano -> { low: pitch A (Accidental 0) 3, high: pitch A (Accidental 0) 5 }
  Soprano -> { low: pitch C (Accidental 0) 4, high: pitch C (Accidental 0) 6 }
  ExtraWide -> { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
  Custom -> { low: pitch C (Accidental 0) 3, high: pitch G (Accidental 0) 5 }
