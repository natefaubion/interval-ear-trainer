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
intervalSemitones AugmentedFifth = 8
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
intervalNumber AugmentedFifth = 5
intervalNumber MinorSixth = 6
intervalNumber MajorSixth = 6
intervalNumber MinorSeventh = 7
intervalNumber MajorSeventh = 7
intervalNumber PerfectOctave = 8

intervalSizeName :: IntervalSize -> String
intervalSizeName SizeUnison = "Unison"
intervalSizeName SizeSecond = "Second"
intervalSizeName SizeThird = "Third"
intervalSizeName SizeFourth = "Fourth"
intervalSizeName SizeFifth = "Fifth"
intervalSizeName SizeSixth = "Sixth"
intervalSizeName SizeSeventh = "Seventh"
intervalSizeName SizeOctave = "Octave"

intervalBetween :: Direction -> Pitch -> Pitch -> Maybe Interval
intervalBetween direction rootPitch@(Pitch (PitchClass rootLetter _) rootOctave) rootTarget@(Pitch (PitchClass targetLetter _) targetOctave) =
  let
    generic = case direction of
      Ascending -> (targetOctave - rootOctave) * 7 + letterIndex targetLetter - letterIndex rootLetter + 1
      Descending -> (rootOctave - targetOctave) * 7 + letterIndex rootLetter - letterIndex targetLetter + 1
    semitones = case direction of
      Ascending -> midiNumber rootTarget - midiNumber rootPitch
      Descending -> midiNumber rootPitch - midiNumber rootTarget
  in
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
intervalName PerfectUnison = "Perfect unison"
intervalName MinorSecond = "Minor second"
intervalName MajorSecond = "Major second"
intervalName MinorThird = "Minor third"
intervalName MajorThird = "Major third"
intervalName PerfectFourth = "Perfect fourth"
intervalName AugmentedFourth = "Augmented fourth"
intervalName DiminishedFifth = "Diminished fifth"
intervalName PerfectFifth = "Perfect fifth"
intervalName AugmentedFifth = "Augmented fifth"
intervalName MinorSixth = "Minor sixth"
intervalName MajorSixth = "Major sixth"
intervalName MinorSeventh = "Minor seventh"
intervalName MajorSeventh = "Major seventh"
intervalName PerfectOctave = "Perfect octave"

playbackModeName :: PlaybackMode -> String
playbackModeName MelodicAscending = "Ascending"
playbackModeName MelodicDescending = "Descending"
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
presetName Custom = "Custom"

presetRange :: VocalRangePreset -> VocalRange
presetRange Bass = { low: pitch E (Accidental 0) 2, high: pitch E (Accidental 0) 4 }
presetRange Baritone = { low: pitch A (Accidental 0) 2, high: pitch A (Accidental 0) 4 }
presetRange Tenor = { low: pitch C (Accidental 0) 3, high: pitch C (Accidental 0) 5 }
presetRange Alto = { low: pitch F (Accidental 0) 3, high: pitch F (Accidental 0) 5 }
presetRange MezzoSoprano = { low: pitch A (Accidental 0) 3, high: pitch A (Accidental 0) 5 }
presetRange Soprano = { low: pitch C (Accidental 0) 4, high: pitch C (Accidental 0) 6 }
presetRange ExtraWide = { low: pitch C (Accidental 0) 1, high: pitch C (Accidental 0) 7 }
presetRange Custom = { low: pitch C (Accidental 0) 3, high: pitch G (Accidental 0) 5 }
