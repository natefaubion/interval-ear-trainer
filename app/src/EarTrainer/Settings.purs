module EarTrainer.Settings
  ( load
  , save
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
  )
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
  { answerCount :: String
  , answerDisplay :: String
  , ghostMode :: String
  , intervals :: Array String
  , octavePolicy :: String
  , playbackModes :: Array String
  , quizMode :: String
  , quizProgression :: String
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
        { answerCount = decodeAnswerCount value.answerCount
        , answerDisplay = decodeAnswerDisplay value.answerDisplay
        , ghostMode = decodeGhostMode value.ghostMode
        , intervals = Array.mapMaybe decodeInterval value.intervals
        , octavePolicy = decodeOctavePolicy value.octavePolicy
        , playbackModes = Array.mapMaybe decodePlaybackMode value.playbackModes
        , quizMode = decodeQuizMode value.quizMode
        , quizProgression = decodeQuizProgression value.quizProgression
        , rootPitchClasses = map decodePitchClass value.rootPitchClasses
        , vocalRange = decodeVocalRange value.vocalRange
        }

save :: ExerciseConfig -> Effect Unit
save config = saveImpl
  { answerCount: encodeAnswerCount config.answerCount
  , answerDisplay: encodeAnswerDisplay config.answerDisplay
  , ghostMode: encodeGhostMode config.ghostMode
  , intervals: map encodeInterval config.intervals
  , octavePolicy: encodeOctavePolicy config.octavePolicy
  , playbackModes: map encodePlaybackMode config.playbackModes
  , quizMode: encodeQuizMode config.quizMode
  , quizProgression: encodeQuizProgression config.quizProgression
  , rootPitchClasses: map encodePitchClass config.rootPitchClasses
  , vocalRange: encodeVocalRange config.vocalRange
  }

encodeQuizMode :: QuizMode -> String
encodeQuizMode SingingOnly = "singing"
encodeQuizMode RecognitionOnly = "recognition"
encodeQuizMode SingingAndRecognition = "singing-and-recognition"
encodeQuizMode Audiation = "audiation"

decodeQuizMode :: String -> QuizMode
decodeQuizMode "singing" = SingingOnly
decodeQuizMode "recognition" = RecognitionOnly
decodeQuizMode "audiation" = Audiation
decodeQuizMode _ = SingingAndRecognition

encodeQuizProgression :: QuizProgression -> String
encodeQuizProgression ManualProgression = "manual"
encodeQuizProgression AutomaticProgression = "automatic"

decodeQuizProgression :: String -> QuizProgression
decodeQuizProgression "automatic" = AutomaticProgression
decodeQuizProgression _ = ManualProgression

encodeAnswerCount :: AnswerCount -> String
encodeAnswerCount AFew = "few"
encodeAnswerCount AllSelected = "all-selected"

decodeAnswerCount :: String -> AnswerCount
decodeAnswerCount "all-selected" = AllSelected
decodeAnswerCount _ = AFew

encodeGhostMode :: GhostMode -> String
encodeGhostMode GhostOff = "off"
encodeGhostMode GhostOn = "on"
encodeGhostMode GhostPersist = "persist"

decodeGhostMode :: String -> GhostMode
decodeGhostMode "off" = GhostOff
decodeGhostMode "persist" = GhostPersist
decodeGhostMode _ = GhostOn

encodeAnswerDisplay :: AnswerDisplay -> String
encodeAnswerDisplay AnswerNotation = "notation"
encodeAnswerDisplay AnswerName = "name"
encodeAnswerDisplay AnswerBoth = "both"

decodeAnswerDisplay :: String -> AnswerDisplay
decodeAnswerDisplay "name" = AnswerName
decodeAnswerDisplay "both" = AnswerBoth
decodeAnswerDisplay _ = AnswerNotation

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
encodeVocalRange ExtraWide = "extra-wide"

decodeVocalRange :: String -> VocalRangePreset
decodeVocalRange "bass" = Bass
decodeVocalRange "baritone" = Baritone
decodeVocalRange "alto" = Alto
decodeVocalRange "mezzo-soprano" = MezzoSoprano
decodeVocalRange "soprano" = Soprano
decodeVocalRange "extra-wide" = ExtraWide
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
