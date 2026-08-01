module EarTrainer.Settings
  ( load
  , save
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import EarTrainer.Config (ExerciseConfig, defaultConfig)
import EarTrainer.Music
  ( Accidental(..)
  , Interval(..)
  , Letter(..)
  , OctavePolicy(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset(..)
  )
import Effect (Effect)

type StoredPitchClass =
  { accidental :: Int
  , letter :: String
  }

type StoredSettings =
  { intervals :: Array String
  , octavePolicy :: String
  , playbackModes :: Array String
  , rootPitchClasses :: Array StoredPitchClass
  , vocalRange :: String
  }

foreign import loadImpl
  :: (StoredSettings -> Maybe StoredSettings)
  -> Maybe StoredSettings
  -> Effect (Maybe StoredSettings)

foreign import saveImpl :: StoredSettings -> Effect Unit

load :: Effect ExerciseConfig
load = do
  stored <- loadImpl Just Nothing
  pure case stored of
    Nothing -> defaultConfig
    Just value ->
      defaultConfig
        { intervals = Array.mapMaybe decodeInterval value.intervals
        , octavePolicy = decodeOctavePolicy value.octavePolicy
        , playbackModes = Array.mapMaybe decodePlaybackMode value.playbackModes
        , rootPitchClasses = map decodePitchClass value.rootPitchClasses
        , vocalRange = decodeVocalRange value.vocalRange
        }

save :: ExerciseConfig -> Effect Unit
save config = saveImpl
  { intervals: map encodeInterval config.intervals
  , octavePolicy: encodeOctavePolicy config.octavePolicy
  , playbackModes: map encodePlaybackMode config.playbackModes
  , rootPitchClasses: map encodePitchClass config.rootPitchClasses
  , vocalRange: encodeVocalRange config.vocalRange
  }

encodeInterval :: Interval -> String
encodeInterval PerfectUnison = "perfect-unison"
encodeInterval MinorSecond = "minor-second"
encodeInterval MajorSecond = "major-second"
encodeInterval MinorThird = "minor-third"
encodeInterval MajorThird = "major-third"
encodeInterval PerfectFourth = "perfect-fourth"
encodeInterval AugmentedFourth = "augmented-fourth"
encodeInterval DiminishedFifth = "diminished-fifth"
encodeInterval PerfectFifth = "perfect-fifth"
encodeInterval MinorSixth = "minor-sixth"
encodeInterval MajorSixth = "major-sixth"
encodeInterval MinorSeventh = "minor-seventh"
encodeInterval MajorSeventh = "major-seventh"
encodeInterval PerfectOctave = "perfect-octave"

decodeInterval :: String -> Maybe Interval
decodeInterval "perfect-unison" = Just PerfectUnison
decodeInterval "minor-second" = Just MinorSecond
decodeInterval "major-second" = Just MajorSecond
decodeInterval "minor-third" = Just MinorThird
decodeInterval "major-third" = Just MajorThird
decodeInterval "perfect-fourth" = Just PerfectFourth
decodeInterval "augmented-fourth" = Just AugmentedFourth
decodeInterval "diminished-fifth" = Just DiminishedFifth
decodeInterval "perfect-fifth" = Just PerfectFifth
decodeInterval "minor-sixth" = Just MinorSixth
decodeInterval "major-sixth" = Just MajorSixth
decodeInterval "minor-seventh" = Just MinorSeventh
decodeInterval "major-seventh" = Just MajorSeventh
decodeInterval "perfect-octave" = Just PerfectOctave
decodeInterval _ = Nothing

encodePlaybackMode :: PlaybackMode -> String
encodePlaybackMode MelodicAscending = "melodic-ascending"
encodePlaybackMode MelodicDescending = "melodic-descending"
encodePlaybackMode Harmonic = "harmonic"

decodePlaybackMode :: String -> Maybe PlaybackMode
decodePlaybackMode "melodic-ascending" = Just MelodicAscending
decodePlaybackMode "melodic-descending" = Just MelodicDescending
decodePlaybackMode "harmonic" = Just Harmonic
decodePlaybackMode _ = Nothing

encodeOctavePolicy :: OctavePolicy -> String
encodeOctavePolicy AnyOctave = "any-octave"
encodeOctavePolicy WrittenOctave = "written-octave"

decodeOctavePolicy :: String -> OctavePolicy
decodeOctavePolicy "written-octave" = WrittenOctave
decodeOctavePolicy _ = AnyOctave

encodeVocalRange :: VocalRangePreset -> String
encodeVocalRange Bass = "bass"
encodeVocalRange Baritone = "baritone"
encodeVocalRange Tenor = "tenor"
encodeVocalRange Alto = "alto"
encodeVocalRange MezzoSoprano = "mezzo-soprano"
encodeVocalRange Soprano = "soprano"

decodeVocalRange :: String -> VocalRangePreset
decodeVocalRange "bass" = Bass
decodeVocalRange "baritone" = Baritone
decodeVocalRange "alto" = Alto
decodeVocalRange "mezzo-soprano" = MezzoSoprano
decodeVocalRange "soprano" = Soprano
decodeVocalRange _ = Tenor

encodePitchClass :: PitchClass -> StoredPitchClass
encodePitchClass (PitchClass letter (Accidental accidental)) =
  { accidental, letter: encodeLetter letter }

decodePitchClass :: StoredPitchClass -> PitchClass
decodePitchClass stored = PitchClass (decodeLetter stored.letter) (Accidental stored.accidental)

encodeLetter :: Letter -> String
encodeLetter C = "C"
encodeLetter D = "D"
encodeLetter E = "E"
encodeLetter F = "F"
encodeLetter G = "G"
encodeLetter A = "A"
encodeLetter B = "B"

decodeLetter :: String -> Letter
decodeLetter "D" = D
decodeLetter "E" = E
decodeLetter "F" = F
decodeLetter "G" = G
decodeLetter "A" = A
decodeLetter "B" = B
decodeLetter _ = C
