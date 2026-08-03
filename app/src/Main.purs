module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.Common as String
import Data.Time.Duration (Milliseconds(..))
import EarTrainer.Audio as Audio
import EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , defaultConfig
  , isValid
  , quizModeUsesRecognition
  , quizModeUsesSinging
  , toggleInterval
  , toggleIntervalSize
  , togglePlaybackMode
  , toggleRootPitchClass
  )
import EarTrainer.Music
  ( Accidental(..)
  , Interval
  , IntervalSize
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , allIntervals
  , allIntervalSizes
  , allMajorKeyPresets
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , intervalSizeName
  , midiNumber
  , pitchClassName
  , pitchFromMidiLike
  , pitchFromMidi
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  )
import EarTrainer.Notation as Notation
import EarTrainer.PitchDetection as Detection
import EarTrainer.Quiz as Quiz
import EarTrainer.Settings as Settings
import Effect (Effect)
import Effect.Aff (attempt, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLDialogElement as HTMLDialogElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data Screen = Setup | Practice

derive instance Eq Screen

data CaptureStatus
  = ReadyToPlay
  | PlayingAudio
  | Listening
  | CaptureFailed String
  | PlaybackFailed String
  | IntervalError
  | ChoosingAnswer
  | AnswerComplete

derive instance Eq CaptureStatus

type State =
  { answerCorrect :: Boolean
  , activityRevision :: Int
  , automaticAdvancePending :: Boolean
  , captureStatus :: CaptureStatus
  , choices :: Array Quiz.IntervalChoice
  , config :: ExerciseConfig
  , ghostMidi :: Maybe Int
  , ghostRevision :: Int
  , monitor :: Maybe Detection.Monitor
  , prompt :: Quiz.Prompt
  , promptRevision :: Int
  , recognition :: Detection.Recognition
  , revealedChoices :: Array Interval
  , resumeAnswersAfterPlayback :: Boolean
  , sampler :: Maybe Audio.Sampler
  , screen :: Screen
  }

data Action
  = Initialize
  | Finalize
  | ToggleInterval Interval
  | SelectAllIntervals
  | ClearIntervals
  | TogglePlaybackMode PlaybackMode
  | ToggleRoot PitchClass
  | SelectAllRoots
  | ClearRoots
  | SelectRange VocalRangePreset
  | SelectOctavePolicy OctavePolicy
  | SelectGhostMode GhostMode
  | SelectPitchTuner Boolean
  | SelectAnswerCount AnswerCount
  | SelectAnswerDisplay AnswerDisplay
  | SelectQuizMode QuizMode
  | SelectQuizProgression QuizProgression
  | BeginPractice
  | PlayPrompt
  | PlaybackStarted Int
  | AudioFailed Int String
  | StartListening Int
  | PitchDetected Int Detection.PitchSample
  | ClearGhost Int
  | FinishSinging Int
  | MicrophoneFailed Int String
  | ChooseInterval Interval
  | NextPrompt
  | AdvanceAutomatically Int
  | RetryAutomatically Int
  | EditSetup

notationRef :: H.RefLabel
notationRef = H.RefLabel "prompt-notation"

savePresetDialogRef :: H.RefLabel
savePresetDialogRef = H.RefLabel "save-preset-dialog"

deletePresetDialogRef :: H.RefLabel
deletePresetDialogRef = H.RefLabel "delete-preset-dialog"

practiceContentRef :: H.RefLabel
practiceContentRef = H.RefLabel "practice-content"

choiceNotationRef :: Int -> H.RefLabel
choiceNotationRef index = H.RefLabel ("choice-notation-" <> show index)

type PracticeInput =
  { config :: ExerciseConfig
  , sampler :: Audio.Sampler
  , seed :: Int
  }

data PracticeOutput = BackToSetup

data PracticeQuery :: Type -> Type
data PracticeQuery a

data SetupOutput
  = SetupDataChanged Settings.AppData
  | SetupBeginRequested
  | SetupPersistenceRequested

data SetupQuery :: Type -> Type
data SetupQuery a

type RootState =
  { activePresetId :: Maybe String
  , config :: ExerciseConfig
  , practice :: Maybe PracticeInput
  , presets :: Array Settings.Preset
  , sampler :: Maybe Audio.Sampler
  , storageError :: Maybe String
  }

type SetupState =
  { activePresetId :: Maybe String
  , config :: ExerciseConfig
  , presetName :: String
  , presetNameError :: Maybe String
  , presets :: Array Settings.Preset
  , sampler :: Maybe Audio.Sampler
  , storageError :: Maybe String
  }

data RootAction
  = RootInitialize
  | RootToggleInterval Interval
  | RootToggleIntervalSize IntervalSize
  | RootSelectAllIntervals
  | RootClearIntervals
  | RootSelectAllIntervalSizes
  | RootClearIntervalSizes
  | RootSelectIntervalSystem IntervalSystem
  | RootTogglePlaybackMode PlaybackMode
  | RootToggleRoot PitchClass
  | RootSelectMajorKey String
  | RootSelectAllRoots
  | RootClearRoots
  | RootSelectRange VocalRangePreset
  | RootSelectCustomLowClass Int
  | RootSelectCustomLowOctave Int
  | RootSelectCustomHighClass Int
  | RootSelectCustomHighOctave Int
  | RootSelectOctavePolicy OctavePolicy
  | RootSelectGhostMode GhostMode
  | RootSelectPitchTuner Boolean
  | RootSelectAnswerCount AnswerCount
  | RootSelectAnswerDisplay AnswerDisplay
  | RootSelectQuizMode QuizMode
  | RootSelectQuizProgression QuizProgression
  | RootOpenSavePreset
  | RootCloseSavePreset
  | RootSetPresetName String
  | RootPresetKeyDown String
  | RootSavePreset
  | RootSelectPreset String
  | RootOpenDeletePreset
  | RootCloseDeletePreset
  | RootConfirmDeletePreset
  | RootResetDefaults
  | RootBeginPractice
  | RootPracticeOutput PracticeOutput
  | RootSetupOutput SetupOutput
  | RootReceiveSetup RootState

type RootSlots =
  ( practice :: H.Slot PracticeQuery PracticeOutput Unit
  , setup :: H.Slot SetupQuery SetupOutput Unit
  )

practiceSlot :: Proxy "practice"
practiceSlot = Proxy

setupSlot :: Proxy "setup"
setupSlot = Proxy

rootComponent :: forall query input output m. MonadAff m => H.Component query input output m
rootComponent =
  H.mkComponent
    { initialState: const
        { activePresetId: Nothing
        , config: defaultConfig
        , practice: Nothing
        , presets: []
        , sampler: Nothing
        , storageError: Nothing
        }
    , render: renderRoot
    , eval: H.mkEval H.defaultEval { handleAction = handleRootAction, initialize = Just RootInitialize }
    }
  where
  renderRoot :: RootState -> H.ComponentHTML RootAction RootSlots m
  renderRoot state =
    HH.main
      [ HP.class_ (H.ClassName "app-shell") ]
      [ case state.practice of
          Just input -> HH.slot practiceSlot unit component input RootPracticeOutput
          Nothing -> HH.slot setupSlot unit setupComponent state RootSetupOutput
      ]

  setupComponent :: H.Component SetupQuery RootState SetupOutput m
  setupComponent =
    H.mkComponent
      { initialState: \input ->
          { activePresetId: input.activePresetId
          , config: input.config
          , presetName: ""
          , presetNameError: Nothing
          , presets: input.presets
          , sampler: input.sampler
          , storageError: input.storageError
          }
      , render: renderRootSetup
      , eval:
          H.mkEval H.defaultEval
            { handleAction = handleSetupAction
            , receive = Just <<< RootReceiveSetup
            }
      }

  renderRootSetup :: SetupState -> H.ComponentHTML RootAction () m
  renderRootSetup state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.h2_ [ HH.text "Exercise setup" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ H.ClassName "secondary-button", H.ClassName "save-preset-button" ]
              , HE.onClick \_ -> RootOpenSavePreset
              ]
              [ HH.text "Save preset" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "setup-content") ]
          [ case state.storageError of
              Nothing -> HH.text ""
              Just message -> HH.p [ HP.class_ (H.ClassName "storage-error") ] [ HH.text message ]
          , if Array.null state.presets then HH.text ""
            else
              rootSettingGroup
                "Presets"
                "Apply a saved exercise setup."
                ( [ rootChoiceButton (isNothing state.activePresetId) (RootSelectPreset "") "Custom" ]
                    <> map
                      (\preset -> rootChoiceButton (state.activePresetId == Just preset.id) (RootSelectPreset preset.id) preset.name)
                      state.presets
                    <>
                      [ HH.div
                          [ HP.class_ (H.ClassName "selection-actions") ]
                          [ HH.button
                              [ HP.type_ HP.ButtonButton
                              , HP.class_ (H.ClassName "small-text-button")
                              , HP.disabled (isNothing state.activePresetId)
                              , HE.onClick \_ -> RootOpenDeletePreset
                              ]
                              [ HH.text "Delete preset" ]
                          ]
                      ]
                )
                Nothing
          , rootSettingGroup
              "Quiz mode"
              "Choose which parts of the exercise to practice."
              [ rootChoiceButton
                  (state.config.quizMode == SingingAndRecognition)
                  (RootSelectQuizMode SingingAndRecognition)
                  "Singing and recognition"
              , rootChoiceButton (state.config.quizMode == SingingOnly) (RootSelectQuizMode SingingOnly) "Singing only"
              , rootChoiceButton (state.config.quizMode == RecognitionOnly) (RootSelectQuizMode RecognitionOnly) "Recognition only"
              , rootChoiceButton (state.config.quizMode == Audiation) (RootSelectQuizMode Audiation) "Audiation"
              ]
              Nothing
          , rootSettingGroup
              "Quiz progression"
              "Choose how the next interval begins after a correct answer."
              [ rootChoiceButton
                  (state.config.quizProgression == AutomaticProgression)
                  (RootSelectQuizProgression AutomaticProgression)
                  "Automatic"
              , rootChoiceButton
                  (state.config.quizProgression == ManualProgression)
                  (RootSelectQuizProgression ManualProgression)
                  "Manual"
              ]
              Nothing
          , rootSettingGroup
              "Note selection"
              "Choose which notes may begin an exercise, or use a major-key preset."
              ( map (rootRootButton state.config) allRootPitchClasses
                  <>
                    [ rootSelectionActions
                        RootSelectAllRoots
                        RootClearRoots
                        (Array.length state.config.rootPitchClasses == Array.length allRootPitchClasses)
                        (Array.null state.config.rootPitchClasses)
                    , rootMajorKeySelector state.config
                    ]
              )
              (if Array.null state.config.rootPitchClasses then Just "Select at least one note." else Nothing)
          , let
              availableOrientations =
                if state.config.quizMode == Audiation then
                  Array.filter (_ /= Harmonic) allPlaybackModes
                else allPlaybackModes
              selectedOrientations = Array.filter (flip Array.elem state.config.playbackModes) availableOrientations
            in
              rootSettingGroup
                "Interval orientation"
                "Choose the directions or forms intervals may take."
                (map (rootModeButton state.config) availableOrientations)
                (if Array.null selectedOrientations then Just "Select at least one interval orientation." else Nothing)
          , rootSettingGroup
              "Interval system"
              "Choose exact interval qualities or derive their qualities from the selected notes."
              [ rootChoiceButton
                  (state.config.intervalSystem == FromSelectedNotes)
                  (RootSelectIntervalSystem FromSelectedNotes)
                  "From selected notes"
              , rootChoiceButton
                  (state.config.intervalSystem == ExactIntervals)
                  (RootSelectIntervalSystem ExactIntervals)
                  "Exact intervals"
              ]
              Nothing
          , let
              possibleExactIntervals = Quiz.availableExactIntervals state.config
              possibleSizes = Quiz.availableIntervalSizes state.config
              selectedPossibleExactIntervals = Array.filter (flip Array.elem possibleExactIntervals) state.config.intervals
              selectedPossibleSizes = Array.filter (flip Array.elem possibleSizes) state.config.availableIntervals
            in
              rootSettingGroup
                "Intervals"
                ( if state.config.intervalSystem == ExactIntervals then
                    "Choose the exact intervals that may appear in an exercise."
                  else
                    "Choose interval numbers; each quality is determined by the selected note pair."
                )
                ( if state.config.intervalSystem == ExactIntervals then
                    map (rootIntervalButton state.config possibleExactIntervals) allIntervals
                      <>
                        [ rootSelectionActions
                            RootSelectAllIntervals
                            RootClearIntervals
                            ( Array.length state.config.intervals == Array.length possibleExactIntervals
                                && Array.all (flip Array.elem possibleExactIntervals) state.config.intervals
                            )
                            (Array.null state.config.intervals)
                        ]
                  else
                    map (rootIntervalSizeButton state.config possibleSizes) allIntervalSizes
                      <>
                        [ rootSelectionActions
                            RootSelectAllIntervalSizes
                            RootClearIntervalSizes
                            ( Array.length state.config.availableIntervals == Array.length possibleSizes
                                && Array.all (flip Array.elem possibleSizes) state.config.availableIntervals
                            )
                            (Array.null state.config.availableIntervals)
                        ]
                )
                ( if state.config.intervalSystem == ExactIntervals && Array.null selectedPossibleExactIntervals then
                    Just "Select at least one interval available in this range."
                  else if state.config.intervalSystem == FromSelectedNotes && Array.null selectedPossibleSizes then
                    Just "Select at least one interval available from these notes."
                  else Nothing
                )
          , rootSettingGroup
              "Playback range"
              "Choose the written and playback register."
              ( map (rootRangeButton state.config) allVocalRangePresets
                  <> if state.config.vocalRange == Custom then [ rootCustomRangeControls state.config ] else []
              )
              ( if
                  state.config.vocalRange == Custom
                    && midiNumber state.config.customRange.low > midiNumber state.config.customRange.high then
                  Just "The lowest note must not be above the highest note."
                else Nothing
              )
          , rootSettingGroup
              "Octave matching"
              "Choose whether the first sung note must match the written register. The second note must always form the written interval from it."
              [ rootChoiceButton
                  (state.config.octavePolicy == AnyOctave)
                  (RootSelectOctavePolicy AnyOctave)
                  "Any octave"
              , rootChoiceButton
                  (state.config.octavePolicy == WrittenOctave)
                  (RootSelectOctavePolicy WrittenOctave)
                  "Written octave"
              ]
              Nothing
          , rootSettingGroup
              "Sung pitch on staff"
              "Choose how the pitch you are currently singing appears on the staff."
              [ rootChoiceButton (state.config.ghostMode == GhostOn) (RootSelectGhostMode GhostOn) "Show briefly"
              , rootChoiceButton
                  (state.config.ghostMode == GhostPersist)
                  (RootSelectGhostMode GhostPersist)
                  "Keep visible"
              , rootChoiceButton (state.config.ghostMode == GhostOff) (RootSelectGhostMode GhostOff) "Hidden"
              ]
              Nothing
          , rootSettingGroup
              "Pitch tuner"
              "Choose whether to show cents feedback while singing."
              [ rootChoiceButton state.config.showPitchTuner (RootSelectPitchTuner true) "Shown"
              , rootChoiceButton (not state.config.showPitchTuner) (RootSelectPitchTuner false) "Hidden"
              ]
              Nothing
          , if quizModeUsesRecognition state.config.quizMode then
              rootSettingGroup
                "Number of available answers"
                "Choose how many interval choices are shown."
                [ rootChoiceButton (state.config.answerCount == AFew) (RootSelectAnswerCount AFew) "A few"
                , rootChoiceButton
                    (state.config.answerCount == AllSelected)
                    (RootSelectAnswerCount AllSelected)
                    "All selected choices"
                ]
                Nothing
            else HH.text ""
          , if quizModeUsesRecognition state.config.quizMode then
              rootSettingGroup
                "Answer display"
                "Choose how interval choices are presented."
                [ rootChoiceButton
                    (state.config.answerDisplay == AnswerNotation)
                    (RootSelectAnswerDisplay AnswerNotation)
                    "Notation"
                , rootChoiceButton
                    (state.config.answerDisplay == AnswerName)
                    (RootSelectAnswerDisplay AnswerName)
                    "Interval name"
                , rootChoiceButton
                    (state.config.answerDisplay == AnswerBoth)
                    (RootSelectAnswerDisplay AnswerBoth)
                    "Both"
                ]
                Nothing
            else HH.text ""
          , HH.aside
              [ HP.class_ (H.ClassName "credits-section") ]
              [ HH.h3_ [ HH.text "Credits" ]
              , HH.p_
                  [ HH.text "Original application code: "
                  , HH.a [ HP.href "LICENSE.txt" ] [ HH.text "GPL version 3 or later" ]
                  , HH.text "."
                  ]
              , HH.p_
                  [ HH.text "Piano samples: "
                  , HH.a
                      [ HP.href "https://archive.org/details/SalamanderGrandPianoV3"
                      , HP.target "_blank"
                      , HP.rel "noreferrer"
                      ]
                      [ HH.text "Salamander Grand Piano V3" ]
                  , HH.text " by Alexander Holm, licensed under "
                  , HH.a
                      [ HP.href "https://creativecommons.org/licenses/by/3.0/"
                      , HP.target "_blank"
                      , HP.rel "noreferrer"
                      ]
                      [ HH.text "CC BY 3.0" ]
                  , HH.text "."
                  ]
              , HH.a
                  [ HP.href "third-party-notices.html" ]
                  [ HH.text "Third-party software notices" ]
              ]
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "setup-footer") ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "secondary-button")
              , HP.disabled (state.config == defaultConfig)
              , HE.onClick \_ -> RootResetDefaults
              ]
              [ HH.text "Reset to defaults" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button")
              , HP.disabled (not (rootConfigValid state.config) || isNothing state.sampler)
              , HE.onClick \_ -> RootBeginPractice
              ]
              [ HH.text "Begin practice" ]
          ]
      , renderSavePresetDialog state
      , renderDeletePresetDialog state
      ]

  renderSavePresetDialog state =
    HH.dialog
      [ HP.ref savePresetDialogRef
      , HP.class_ (H.ClassName "preset-dialog")
      , HP.attr (H.AttrName "aria-labelledby") "save-preset-title"
      ]
      [ HH.h3 [ HP.id "save-preset-title" ] [ HH.text "Save preset" ]
      , HH.label
          [ HP.class_ (H.ClassName "dialog-field") ]
          [ HH.span_ [ HH.text "Preset name" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.value state.presetName
              , HP.autofocus true
              , HP.required true
              , HE.onValueInput RootSetPresetName
              , HE.onKeyDown (RootPresetKeyDown <<< KeyboardEvent.key)
              ]
          ]
      , case state.presetNameError of
          Nothing -> HH.text ""
          Just message -> HH.p [ HP.class_ (H.ClassName "setting-error") ] [ HH.text message ]
      , HH.div
          [ HP.class_ (H.ClassName "dialog-actions") ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "secondary-button")
              , HE.onClick \_ -> RootCloseSavePreset
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button")
              , HE.onClick \_ -> RootSavePreset
              ]
              [ HH.text "Save" ]
          ]
      ]

  renderDeletePresetDialog state =
    HH.dialog
      [ HP.ref deletePresetDialogRef
      , HP.class_ (H.ClassName "preset-dialog")
      , HP.attr (H.AttrName "aria-labelledby") "delete-preset-title"
      ]
      [ HH.h3
          [ HP.id "delete-preset-title" ]
          [ HH.text case activePreset state of
              Nothing -> "Delete preset?"
              Just preset -> "Delete “" <> preset.name <> "”?"
          ]
      , HH.div
          [ HP.class_ (H.ClassName "dialog-actions") ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "secondary-button")
              , HE.onClick \_ -> RootCloseDeletePreset
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ H.ClassName "primary-button", H.ClassName "danger-button" ]
              , HP.disabled (isNothing state.activePresetId)
              , HE.onClick \_ -> RootConfirmDeletePreset
              ]
              [ HH.text "Delete" ]
          ]
      ]

  activePreset state = case state.activePresetId of
    Nothing -> Nothing
    Just id -> Array.find (\preset -> preset.id == id) state.presets

  rootSettingGroup title description controls validation =
    HH.fieldset
      [ HP.class_ (H.ClassName "setting-group") ]
      [ HH.legend_ [ HH.text title ]
      , HH.p [ HP.class_ (H.ClassName "setting-description") ] [ HH.text description ]
      , HH.div [ HP.class_ (H.ClassName "choice-grid") ] controls
      , case validation of
          Nothing -> HH.text ""
          Just message -> HH.p [ HP.class_ (H.ClassName "setting-error") ] [ HH.text message ]
      ]

  rootChoiceButton selected action label =
    HH.button
      [ HP.type_ HP.ButtonButton
      , HP.classes
          if selected then [ H.ClassName "choice-chip", H.ClassName "selected" ]
          else [ H.ClassName "choice-chip" ]
      , HE.onClick \_ -> action
      ]
      [ HH.text label ]

  rootSelectionActions selectAction clearAction selectDisabled clearDisabled =
    HH.div
      [ HP.class_ (H.ClassName "selection-actions") ]
      [ HH.button
          [ HP.type_ HP.ButtonButton
          , HP.class_ (H.ClassName "small-text-button")
          , HP.disabled selectDisabled
          , HE.onClick \_ -> selectAction
          ]
          [ HH.text "Select All" ]
      , HH.button
          [ HP.type_ HP.ButtonButton
          , HP.class_ (H.ClassName "small-text-button")
          , HP.disabled clearDisabled
          , HE.onClick \_ -> clearAction
          ]
          [ HH.text "Clear" ]
      ]

  rootModeButton config mode =
    rootChoiceButton (Array.elem mode config.playbackModes) (RootTogglePlaybackMode mode) (playbackModeName mode)

  rootIntervalButton config possible interval =
    HH.button
      [ HP.type_ HP.ButtonButton
      , HP.classes
          if Array.elem interval config.intervals then
            [ H.ClassName "choice-chip", H.ClassName "selected" ]
          else [ H.ClassName "choice-chip" ]
      , HP.disabled (not (Array.elem interval possible))
      , HE.onClick \_ -> RootToggleInterval interval
      ]
      [ HH.text (intervalName interval) ]

  rootIntervalSizeButton config possible interval =
    HH.button
      [ HP.type_ HP.ButtonButton
      , HP.classes
          if Array.elem interval config.availableIntervals then
            [ H.ClassName "choice-chip", H.ClassName "selected" ]
          else [ H.ClassName "choice-chip" ]
      , HP.disabled (not (Array.elem interval possible))
      , HE.onClick \_ -> RootToggleIntervalSize interval
      ]
      [ HH.text (intervalSizeName interval) ]

  rootRootButton config root =
    rootChoiceButton (Array.elem root config.rootPitchClasses) (RootToggleRoot root) (pitchClassName root)

  rootMajorKeySelector config =
    HH.label
      [ HP.class_ (H.ClassName "major-key-preset") ]
      [ HH.span_ [ HH.text "Major-key preset" ]
      , HH.select
          [ HP.value (selectedMajorKeyId config.rootPitchClasses)
          , HE.onValueChange RootSelectMajorKey
          ]
          ( [ HH.option [ HP.value "custom" ] [ HH.text "Custom" ] ]
              <> map rootMajorKeyOption allMajorKeyPresets
          )
      ]

  rootMajorKeyOption preset =
    HH.option [ HP.value preset.id ] [ HH.text preset.name ]

  selectedMajorKeyId roots = case Array.find (\preset -> samePitchClasses preset.roots roots) allMajorKeyPresets of
    Just preset -> preset.id
    Nothing -> "custom"

  samePitchClasses left right =
    Array.length left == Array.length right && Array.all (flip Array.elem right) left

  rootConfigValid config =
    isValid config
      &&
        if config.intervalSystem == ExactIntervals then
          not
            ( Array.null
                (Array.filter (flip Array.elem (Quiz.availableExactIntervals config)) config.intervals)
            )
        else
          not
            ( Array.null
                (Array.filter (flip Array.elem (Quiz.availableIntervalSizes config)) config.availableIntervals)
            )

  rootRangeButton config preset =
    let
      label = case preset of
        Custom -> "Custom"
        _ ->
          let
            range = presetRange preset
          in
            presetName preset <> " · " <> pitchName range.low <> "–" <> pitchName range.high
    in
      rootChoiceButton (config.vocalRange == preset) (RootSelectRange preset) label

  rootCustomRangeControls config =
    HH.div
      [ HP.class_ (H.ClassName "custom-range-row") ]
      [ HH.div
          [ HP.class_ (H.ClassName "custom-range") ]
          [ rootPitchBoundary "Lowest note" config.customRange.low RootSelectCustomLowClass RootSelectCustomLowOctave
          , rootPitchBoundary "Highest note" config.customRange.high RootSelectCustomHighClass RootSelectCustomHighOctave
          ]
      ]

  rootPitchBoundary label current selectClass selectOctave =
    let
      currentMidi = midiNumber current
      currentClass = currentMidi `mod` 12
      currentOctave = currentMidi `div` 12 - 1
    in
      HH.label
        [ HP.class_ (H.ClassName "custom-range-boundary") ]
        [ HH.span_ [ HH.text label ]
        , HH.div
            [ HP.class_ (H.ClassName "custom-range-selects") ]
            [ HH.select
                [ HP.value (show currentClass)
                , HE.onValueChange (selectClass <<< fromMaybe currentClass <<< Int.fromString)
                ]
                (Array.mapWithIndex (rootPitchClassOption currentClass) customPitchClassLabels)
            , HH.select
                [ HP.value (show currentOctave)
                , HE.onValueChange (selectOctave <<< fromMaybe currentOctave <<< Int.fromString)
                ]
                (map (rootOctaveOption currentOctave) (Array.range 1 7))
            ]
        ]

  rootPitchClassOption selected index label =
    HH.option
      [ HP.value (show index), HP.selected (index == selected) ]
      [ HH.text label ]

  rootOctaveOption selected octave =
    HH.option
      [ HP.value (show octave), HP.selected (octave == selected) ]
      [ HH.text (show octave) ]

  customPitchClassLabels = [ "C", "C♯ / D♭", "D", "D♯ / E♭", "E", "F", "F♯ / G♭", "G", "G♯ / A♭", "A", "A♯ / B♭", "B" ]

  handleRootAction = case _ of
    RootInitialize -> do
      loaded <- H.liftAff (attempt Settings.load)
      let
        appData = case loaded of
          Left _ -> { activePresetId: Nothing, config: defaultConfig, presets: [] }
          Right value -> value
        storageError = case loaded of
          Left _ -> Just "Saved settings could not be loaded on this device."
          Right _ -> Nothing
      sampler <- H.liftEffect Audio.createSampler
      H.modify_ _
        { activePresetId = appData.activePresetId
        , config = appData.config
        , presets = appData.presets
        , sampler = Just sampler
        , storageError = storageError
        }
    RootToggleInterval interval -> rootUpdateConfig (toggleInterval interval)
    RootToggleIntervalSize interval -> rootUpdateConfig (toggleIntervalSize interval)
    RootSelectAllIntervals -> do
      state <- H.get
      rootUpdateConfig (_ { intervals = Quiz.availableExactIntervals state.config })
    RootClearIntervals -> rootUpdateConfig (_ { intervals = [] })
    RootSelectAllIntervalSizes -> do
      state <- H.get
      rootUpdateConfig (_ { availableIntervals = Quiz.availableIntervalSizes state.config })
    RootClearIntervalSizes -> rootUpdateConfig (_ { availableIntervals = [] })
    RootSelectIntervalSystem system -> rootUpdateConfig (_ { intervalSystem = system })
    RootTogglePlaybackMode mode -> rootUpdateConfig (togglePlaybackMode mode)
    RootToggleRoot root -> rootUpdateConfig (toggleRootPitchClass root)
    RootSelectMajorKey presetId -> rootUpdateConfig (selectMajorKey presetId)
    RootSelectAllRoots -> rootUpdateConfig (_ { rootPitchClasses = allRootPitchClasses })
    RootClearRoots -> rootUpdateConfig (_ { rootPitchClasses = [] })
    RootSelectRange preset -> rootUpdateConfig (_ { vocalRange = preset })
    RootSelectCustomLowClass pitchClass -> rootUpdateConfig (setCustomLowClass pitchClass)
    RootSelectCustomLowOctave octave -> rootUpdateConfig (setCustomLowOctave octave)
    RootSelectCustomHighClass pitchClass -> rootUpdateConfig (setCustomHighClass pitchClass)
    RootSelectCustomHighOctave octave -> rootUpdateConfig (setCustomHighOctave octave)
    RootSelectOctavePolicy policy -> rootUpdateConfig (_ { octavePolicy = policy })
    RootSelectGhostMode mode -> rootUpdateConfig (_ { ghostMode = mode })
    RootSelectPitchTuner shown -> rootUpdateConfig (_ { showPitchTuner = shown })
    RootSelectAnswerCount count -> rootUpdateConfig (_ { answerCount = count })
    RootSelectAnswerDisplay display -> rootUpdateConfig (_ { answerDisplay = display })
    RootSelectQuizMode mode -> rootUpdateConfig (_ { quizMode = mode })
    RootSelectQuizProgression progression -> rootUpdateConfig (_ { quizProgression = progression })
    RootResetDefaults -> rootUpdateConfig (const defaultConfig)
    RootBeginPractice -> do
      state <- H.get
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> do
          seed <- H.liftEffect (randomInt 0 2147483647)
          H.modify_ _ { practice = Just { config: state.config, sampler, seed } }
    RootPracticeOutput BackToSetup -> H.modify_ _ { practice = Nothing }
    RootSetupOutput (SetupDataChanged appData) -> do
      H.modify_ _
        { activePresetId = appData.activePresetId
        , config = appData.config
        , presets = appData.presets
        }
      persistRootData
    RootSetupOutput SetupBeginRequested -> handleRootAction RootBeginPractice
    RootSetupOutput SetupPersistenceRequested -> void $ H.fork do
      void $ H.liftAff Settings.requestPersistence
    RootReceiveSetup _ -> pure unit
    RootOpenSavePreset -> pure unit
    RootCloseSavePreset -> pure unit
    RootSetPresetName _ -> pure unit
    RootPresetKeyDown _ -> pure unit
    RootSavePreset -> pure unit
    RootSelectPreset _ -> pure unit
    RootOpenDeletePreset -> pure unit
    RootCloseDeletePreset -> pure unit
    RootConfirmDeletePreset -> pure unit

  handleSetupAction = case _ of
    RootToggleInterval interval -> setupUpdateConfig (toggleInterval interval)
    RootToggleIntervalSize interval -> setupUpdateConfig (toggleIntervalSize interval)
    RootSelectAllIntervals -> do
      state <- H.get
      setupUpdateConfig (_ { intervals = Quiz.availableExactIntervals state.config })
    RootClearIntervals -> setupUpdateConfig (_ { intervals = [] })
    RootSelectAllIntervalSizes -> do
      state <- H.get
      setupUpdateConfig (_ { availableIntervals = Quiz.availableIntervalSizes state.config })
    RootClearIntervalSizes -> setupUpdateConfig (_ { availableIntervals = [] })
    RootSelectIntervalSystem system -> setupUpdateConfig (_ { intervalSystem = system })
    RootTogglePlaybackMode mode -> setupUpdateConfig (togglePlaybackMode mode)
    RootToggleRoot root -> setupUpdateConfig (toggleRootPitchClass root)
    RootSelectMajorKey presetId -> setupUpdateConfig (selectMajorKey presetId)
    RootSelectAllRoots -> setupUpdateConfig (_ { rootPitchClasses = allRootPitchClasses })
    RootClearRoots -> setupUpdateConfig (_ { rootPitchClasses = [] })
    RootSelectRange preset -> setupUpdateConfig (_ { vocalRange = preset })
    RootSelectCustomLowClass pitchClass -> setupUpdateConfig (setCustomLowClass pitchClass)
    RootSelectCustomLowOctave octave -> setupUpdateConfig (setCustomLowOctave octave)
    RootSelectCustomHighClass pitchClass -> setupUpdateConfig (setCustomHighClass pitchClass)
    RootSelectCustomHighOctave octave -> setupUpdateConfig (setCustomHighOctave octave)
    RootSelectOctavePolicy policy -> setupUpdateConfig (_ { octavePolicy = policy })
    RootSelectGhostMode mode -> setupUpdateConfig (_ { ghostMode = mode })
    RootSelectPitchTuner shown -> setupUpdateConfig (_ { showPitchTuner = shown })
    RootSelectAnswerCount count -> setupUpdateConfig (_ { answerCount = count })
    RootSelectAnswerDisplay display -> setupUpdateConfig (_ { answerDisplay = display })
    RootSelectQuizMode mode -> setupUpdateConfig (_ { quizMode = mode })
    RootSelectQuizProgression progression -> setupUpdateConfig (_ { quizProgression = progression })
    RootOpenSavePreset -> do
      H.modify_ _ { presetName = "", presetNameError = Nothing }
      showDialog savePresetDialogRef
    RootCloseSavePreset -> closeDialog savePresetDialogRef
    RootSetPresetName name -> H.modify_ _ { presetName = name, presetNameError = Nothing }
    RootPresetKeyDown key -> when (key == "Enter") (handleSetupAction RootSavePreset)
    RootSavePreset -> do
      state <- H.get
      let
        name = String.trim state.presetName
        duplicate = Array.any (\preset -> String.toLower preset.name == String.toLower name) state.presets
      if name == "" then
        H.modify_ _ { presetNameError = Just "Enter a preset name." }
      else if duplicate then
        H.modify_ _ { presetNameError = Just "A preset with this name already exists." }
      else do
        id <- H.liftEffect Settings.newPresetId
        let
          preset = { config: state.config, id, name }
          appData = { activePresetId: Just id, config: state.config, presets: Array.snoc state.presets preset }
        H.modify_ _
          { activePresetId = appData.activePresetId
          , presets = appData.presets
          , presetNameError = Nothing
          }
        closeDialog savePresetDialogRef
        H.raise (SetupDataChanged appData)
        H.raise SetupPersistenceRequested
    RootSelectPreset id -> do
      state <- H.get
      if id == "" then do
        H.modify_ _ { activePresetId = Nothing }
        H.raise (SetupDataChanged { activePresetId: Nothing, config: state.config, presets: state.presets })
      else case Array.find (\preset -> preset.id == id) state.presets of
        Nothing -> pure unit
        Just preset -> do
          H.modify_ _ { activePresetId = Just id, config = preset.config }
          H.raise (SetupDataChanged { activePresetId: Just id, config: preset.config, presets: state.presets })
    RootOpenDeletePreset -> showDialog deletePresetDialogRef
    RootCloseDeletePreset -> closeDialog deletePresetDialogRef
    RootConfirmDeletePreset -> do
      state <- H.get
      case state.activePresetId of
        Nothing -> closeDialog deletePresetDialogRef
        Just id -> do
          let
            presets = Array.filter (\preset -> preset.id /= id) state.presets
            appData = { activePresetId: Nothing, config: state.config, presets }
          H.modify_ _ { activePresetId = Nothing, presets = presets }
          closeDialog deletePresetDialogRef
          H.raise (SetupDataChanged appData)
    RootResetDefaults -> setupUpdateConfig (const defaultConfig)
    RootBeginPractice -> H.raise SetupBeginRequested
    RootReceiveSetup input -> H.modify_ _
      { activePresetId = input.activePresetId
      , config = input.config
      , presets = input.presets
      , sampler = input.sampler
      , storageError = input.storageError
      }
    RootInitialize -> pure unit
    RootPracticeOutput _ -> pure unit
    RootSetupOutput _ -> pure unit

  setupUpdateConfig update = do
    H.modify_ \state -> state { activePresetId = Nothing, config = update state.config }
    state <- H.get
    H.raise (SetupDataChanged { activePresetId: Nothing, config: state.config, presets: state.presets })

  rootUpdateConfig update = do
    H.modify_ \state -> state { activePresetId = Nothing, config = update state.config }
    persistRootData

  persistRootData = do
    state <- H.get
    result <- H.liftAff $ attempt $ Settings.save
      { activePresetId: state.activePresetId
      , config: state.config
      , presets: state.presets
      }
    case result of
      Left _ -> H.modify_ _ { storageError = Just "Changes could not be saved on this device." }
      Right _ -> H.modify_ _ { storageError = Nothing }

  showDialog ref = do
    maybeElement <- H.getHTMLElementRef ref
    for_ (maybeElement >>= HTMLDialogElement.fromHTMLElement) \dialog ->
      H.liftEffect (HTMLDialogElement.showModal dialog)

  closeDialog ref = do
    maybeElement <- H.getHTMLElementRef ref
    for_ (maybeElement >>= HTMLDialogElement.fromHTMLElement) \dialog ->
      H.liftEffect (HTMLDialogElement.close Nothing dialog)

  setCustomLowClass pitchClass config =
    let
      octave = midiNumber config.customRange.low `div` 12 - 1
    in
      config { customRange = config.customRange { low = pitchFromMidi (12 * (octave + 1) + pitchClass) } }

  setCustomLowOctave octave config =
    let
      pitchClass = midiNumber config.customRange.low `mod` 12
    in
      config { customRange = config.customRange { low = pitchFromMidi (12 * (octave + 1) + pitchClass) } }

  setCustomHighClass pitchClass config =
    let
      octave = midiNumber config.customRange.high `div` 12 - 1
    in
      config { customRange = config.customRange { high = pitchFromMidi (12 * (octave + 1) + pitchClass) } }

  setCustomHighOctave octave config =
    let
      pitchClass = midiNumber config.customRange.high `mod` 12
    in
      config { customRange = config.customRange { high = pitchFromMidi (12 * (octave + 1) + pitchClass) } }

  selectMajorKey presetId config = case Array.find (\preset -> preset.id == presetId) allMajorKeyPresets of
    Just preset -> config { rootPitchClasses = preset.roots }
    Nothing -> config

component :: forall query m. MonadAff m => H.Component query PracticeInput PracticeOutput m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval H.defaultEval
          { handleAction = handleAction
          , initialize = Just Initialize
          , finalize = Just Finalize
          }
    }
  where
  initialState input =
    let
      prompt = Quiz.makePrompt input.seed input.config
    in
      { answerCorrect: false
      , activityRevision: 0
      , automaticAdvancePending: false
      , captureStatus: ReadyToPlay
      , choices: Quiz.makeChoices input.seed input.config prompt
      , config: input.config
      , ghostMidi: Nothing
      , ghostRevision: 0
      , monitor: Nothing
      , prompt: prompt
      , promptRevision: 0
      , recognition: Detection.initialRecognition
      , revealedChoices: []
      , resumeAnswersAfterPlayback: false
      , sampler: Just input.sampler
      , screen: Practice
      }

  render :: State -> H.ComponentHTML Action () m
  render state =
    if state.screen == Setup then renderSetup state else renderPractice state

  renderSetup state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.h2_ [ HH.text "Exercise setup" ] ]
      , HH.div
          [ HP.class_ (H.ClassName "setup-content") ]
          [ settingGroup
              "Quiz mode"
              "Choose which parts of the exercise to practice."
              [ choiceButton
                  (state.config.quizMode == SingingAndRecognition)
                  (SelectQuizMode SingingAndRecognition)
                  "Singing and recognition"
              , choiceButton (state.config.quizMode == SingingOnly) (SelectQuizMode SingingOnly) "Singing only"
              , choiceButton (state.config.quizMode == RecognitionOnly) (SelectQuizMode RecognitionOnly) "Recognition only"
              , choiceButton (state.config.quizMode == Audiation) (SelectQuizMode Audiation) "Audiation"
              ]
              Nothing
          , settingGroup
              "Quiz progression"
              "Choose how the next interval begins after a correct answer."
              [ choiceButton
                  (state.config.quizProgression == AutomaticProgression)
                  (SelectQuizProgression AutomaticProgression)
                  "Automatic"
              , choiceButton
                  (state.config.quizProgression == ManualProgression)
                  (SelectQuizProgression ManualProgression)
                  "Manual"
              ]
              Nothing
          , let
              availableOrientations =
                if state.config.quizMode == Audiation then
                  Array.filter (_ /= Harmonic) allPlaybackModes
                else allPlaybackModes
              selectedOrientations = Array.filter (flip Array.elem state.config.playbackModes) availableOrientations
            in
              settingGroup
                "Interval orientation"
                "Choose the directions or forms intervals may take."
                (map (modeButton state.config) availableOrientations)
                (if Array.null selectedOrientations then Just "Select at least one interval orientation." else Nothing)
          , settingGroup
              "Intervals"
              "Choose the intervals that may appear in an exercise."
              ( map (intervalButton state.config) allIntervals
                  <>
                    [ selectionActions
                        SelectAllIntervals
                        ClearIntervals
                        (Array.length state.config.intervals == Array.length allIntervals)
                        (Array.null state.config.intervals)
                    ]
              )
              (if Array.null state.config.intervals then Just "Select at least one interval." else Nothing)
          , settingGroup
              "Playback range"
              "Choose the written and playback register."
              (map (rangeButton state.config) allVocalRangePresets)
              Nothing
          , settingGroup
              "Root notes"
              "Choose the individual pitch classes that may begin an exercise."
              ( map (rootButton state.config) allRootPitchClasses
                  <>
                    [ selectionActions
                        SelectAllRoots
                        ClearRoots
                        (Array.length state.config.rootPitchClasses == Array.length allRootPitchClasses)
                        (Array.null state.config.rootPitchClasses)
                    ]
              )
              (if Array.null state.config.rootPitchClasses then Just "Select at least one root note." else Nothing)
          , settingGroup
              "Octave matching"
              "Choose whether the detected pitch must be in the written octave."
              [ choiceButton
                  (state.config.octavePolicy == AnyOctave)
                  (SelectOctavePolicy AnyOctave)
                  "Any comfortable octave"
              , choiceButton
                  (state.config.octavePolicy == WrittenOctave)
                  (SelectOctavePolicy WrittenOctave)
                  "Written octave only"
              ]
              Nothing
          , settingGroup
              "Ghost note"
              "The ghost note shows the pitch you are currently singing on the staff."
              [ choiceButton (state.config.ghostMode == GhostOn) (SelectGhostMode GhostOn) "Shown briefly"
              , choiceButton (state.config.ghostMode == GhostPersist) (SelectGhostMode GhostPersist) "Kept visible"
              , choiceButton (state.config.ghostMode == GhostOff) (SelectGhostMode GhostOff) "Hidden"
              ]
              Nothing
          , settingGroup
              "Pitch tuner"
              "Choose whether to show cents feedback while singing."
              [ choiceButton state.config.showPitchTuner (SelectPitchTuner true) "Shown"
              , choiceButton (not state.config.showPitchTuner) (SelectPitchTuner false) "Hidden"
              ]
              Nothing
          , if not (quizModeUsesRecognition state.config.quizMode) then HH.text ""
            else
              settingGroup
                "Available answers"
                "Choose how many interval choices are shown for each question."
                [ choiceButton (state.config.answerCount == AFew) (SelectAnswerCount AFew) "A few"
                , choiceButton
                    (state.config.answerCount == AllSelected)
                    (SelectAnswerCount AllSelected)
                    "All selected choices"
                ]
                Nothing
          , if not (quizModeUsesRecognition state.config.quizMode) then HH.text ""
            else
              settingGroup
                "Answer display"
                "Choose how interval answers are presented."
                [ choiceButton
                    (state.config.answerDisplay == AnswerNotation)
                    (SelectAnswerDisplay AnswerNotation)
                    "Notation"
                , choiceButton
                    (state.config.answerDisplay == AnswerName)
                    (SelectAnswerDisplay AnswerName)
                    "Interval name"
                , choiceButton
                    (state.config.answerDisplay == AnswerBoth)
                    (SelectAnswerDisplay AnswerBoth)
                    "Both"
                ]
                Nothing
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "setup-footer") ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button")
              , HP.disabled (not (isValid state.config))
              , HE.onClick \_ -> BeginPractice
              ]
              [ HH.text "Begin practice" ]
          ]
      ]

  renderPractice state =
    HH.section
      [ HP.class_ (H.ClassName "practice-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "practice-toolbar") ]
          [ HH.div_
              [ HH.h2_ [ HH.text (quizModeTitle state.config.quizMode) ] ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ H.ClassName "secondary-button", H.ClassName "icon-button" ]
              , HP.attr (H.AttrName "aria-label") "Setup"
              , HP.title "Setup"
              , HE.onClick \_ -> EditSetup
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "settings-icon")
                  , HP.attr (H.AttrName "aria-hidden") "true"
                  ]
                  []
              ]
          ]
      , HH.div
          [ HP.ref practiceContentRef
          , HP.class_ (H.ClassName "practice-content")
          ]
          [ HH.div
              [ HP.class_ (H.ClassName "notation-panel") ]
              [ HH.div
                  [ HP.ref notationRef
                  , HP.class_ (H.ClassName "notation-canvas")
                  ]
                  []
              , if shouldShowIntervalName state then
                  HH.p
                    [ HP.class_ (H.ClassName "completed-interval-name") ]
                    [ HH.text (promptIntervalLabel state) ]
                else
                  HH.text ""
              , renderPitchMeter state
              , renderIntervalChoices state
              ]
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "practice-actions") ]
          [ HH.p_
              [ HH.text (practiceInstruction state) ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button play-button")
              , HP.disabled (footerButtonDisabled state)
              , HE.onClick \_ -> footerButtonAction state
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "play-icon") ]
                  [ HH.text (footerButtonIcon state) ]
              , HH.text (footerButtonLabel state)
              ]
          ]
      ]

  renderPitchMeter state
    | state.config.quizMode == RecognitionOnly = HH.text ""
    | not state.config.showPitchTuner = HH.text ""
    | state.captureStatus == IntervalError = HH.text ""
    | state.captureStatus == ChoosingAnswer || state.captureStatus == AnswerComplete = HH.text ""
    | otherwise =
        HH.div_
          [ HH.div
              [ HP.class_ (H.ClassName "pitch-feedback") ]
              [ HH.span
                  [ HP.class_ (H.ClassName "tuner-readout") ]
                  [ HH.text (feedbackName state.recognition.feedback) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "tuner-track") ]
              [ HH.div [ HP.class_ (H.ClassName "tuner-center") ] []
              , case state.recognition.feedback of
                  Nothing -> HH.text ""
                  Just feedback ->
                    HH.div
                      [ HP.classes
                          ( [ H.ClassName "tuner-dot" ]
                              <> if feedbackInRange feedback then [ H.ClassName "in-range" ] else []
                          )
                      , HP.style ("left: " <> show (feedbackPosition feedback.cents) <> "%")
                      ]
                      []
              ]
          ]

  renderIntervalChoices state
    | not (quizModeUsesRecognition state.config.quizMode) = HH.text ""
    | state.captureStatus /= ChoosingAnswer
        && state.captureStatus /= AnswerComplete
        && not (state.captureStatus == PlayingAudio && state.resumeAnswersAfterPlayback) = HH.text ""
    | otherwise =
        HH.section
          [ HP.class_ (H.ClassName "answer-panel") ]
          [ HH.div
              [ HP.class_ (H.ClassName "answer-grid") ]
              (Array.mapWithIndex (renderIntervalChoice state) state.choices)
          ]

  renderIntervalChoice state index choice =
    let
      revealed = Array.elem choice.interval state.revealedChoices
      correct = revealed && choice.interval == state.prompt.interval
      incorrect = revealed && not correct
      showNotation = state.config.answerDisplay /= AnswerName
      showName = state.config.answerDisplay /= AnswerNotation || revealed
      resultClasses =
        if correct then [ H.ClassName "answer-correct" ]
        else if incorrect then [ H.ClassName "answer-incorrect" ]
        else []
    in
      HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes
            ( [ H.ClassName "interval-answer" ]
                <>
                  ( if state.config.answerDisplay == AnswerName then [ H.ClassName "name-only" ]
                    else []
                  )
                <>
                  ( if state.config.answerDisplay /= AnswerNotation then [ H.ClassName "names-visible" ]
                    else []
                  )
                <> resultClasses
            )
        , HP.disabled (state.answerCorrect || state.captureStatus == PlayingAudio)
        , HE.onClick \_ -> ChooseInterval choice.interval
        ]
        [ if revealed then
            HH.span
              [ HP.classes
                  [ H.ClassName "choice-result-icon"
                  , H.ClassName if correct then "result-correct" else "result-incorrect"
                  ]
              ]
              []
          else
            HH.text ""
        , if showNotation then
            HH.div
              [ HP.ref (choiceNotationRef index)
              , HP.class_ (H.ClassName "choice-notation")
              ]
              []
          else
            HH.text ""
        , HH.span
            [ HP.class_ (H.ClassName "choice-label") ]
            [ HH.text if showName then intervalName choice.interval else "Interval hidden" ]
        ]

  footerButtonAction state =
    if state.captureStatus == AnswerComplete then NextPrompt else PlayPrompt

  footerButtonDisabled state =
    state.automaticAdvancePending
      || state.captureStatus == PlayingAudio
      || (state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete)

  footerButtonLabel state
    | state.captureStatus == PlayingAudio = "Playing…"
    | state.captureStatus == AnswerComplete = "Next interval"
    | state.captureStatus == Listening && state.recognition.phase == Detection.RecognitionComplete = "Next interval"
    | state.config.quizMode == Audiation = "Play root"
    | otherwise = "Play interval"

  footerButtonIcon state
    | state.captureStatus == PlayingAudio = ""
    | footerButtonLabel state == "Next interval" = "→"
    | otherwise = "▶"

  settingGroup title description controls validation =
    HH.fieldset
      [ HP.class_ (H.ClassName "setting-group") ]
      [ HH.legend_ [ HH.text title ]
      , HH.p
          [ HP.class_ (H.ClassName "setting-description") ]
          [ HH.text description ]
      , HH.div
          [ HP.class_ (H.ClassName "choice-grid") ]
          controls
      , case validation of
          Nothing -> HH.text ""
          Just message ->
            HH.p
              [ HP.class_ (H.ClassName "setting-error") ]
              [ HH.text message ]
      ]

  choiceButton selected action label =
    HH.button
      [ HP.type_ HP.ButtonButton
      , HP.classes
          if selected then
            [ H.ClassName "choice-chip", H.ClassName "selected" ]
          else
            [ H.ClassName "choice-chip" ]
      , HE.onClick \_ -> action
      ]
      [ HH.text label ]

  selectionActions selectAction clearAction selectDisabled clearDisabled =
    HH.div
      [ HP.class_ (H.ClassName "selection-actions") ]
      [ HH.button
          [ HP.type_ HP.ButtonButton
          , HP.class_ (H.ClassName "small-text-button")
          , HP.disabled selectDisabled
          , HE.onClick \_ -> selectAction
          ]
          [ HH.text "Select All" ]
      , HH.button
          [ HP.type_ HP.ButtonButton
          , HP.class_ (H.ClassName "small-text-button")
          , HP.disabled clearDisabled
          , HE.onClick \_ -> clearAction
          ]
          [ HH.text "Clear" ]
      ]

  modeButton config mode =
    choiceButton
      (Array.elem mode config.playbackModes)
      (TogglePlaybackMode mode)
      (playbackModeName mode)

  intervalButton config interval =
    choiceButton
      (Array.elem interval config.intervals)
      (ToggleInterval interval)
      (intervalName interval)

  rootButton config root =
    choiceButton
      (Array.elem root config.rootPitchClasses)
      (ToggleRoot root)
      (pitchClassName root)

  rangeButton config preset =
    let
      range = presetRange preset
      label = presetName preset <> " · " <> pitchName range.low <> "–" <> pitchName range.high
    in
      choiceButton
        (config.vocalRange == preset)
        (SelectRange preset)
        label

  practiceInstruction state = case state.captureStatus of
    ReadyToPlay ->
      if state.config.quizMode == Audiation then
        "Listen to the root, then sing the interval."
      else if quizModeUsesSinging state.config.quizMode then
        "Listen to the interval, then sing."
      else
        "Listen to the interval, then choose."
    PlayingAudio -> "Listen carefully."
    Listening -> Detection.phaseInstruction state.recognition.phase
    CaptureFailed message -> "Microphone unavailable: " <> message
    PlaybackFailed message -> "Audio playback failed: " <> message
    IntervalError -> "Incorrect pitch."
    ChoosingAnswer ->
      if Array.null state.revealedChoices then
        "Choose the matching interval."
      else
        "Not quite. Compare the choices and try again."
    AnswerComplete -> "Correct!"

  feedbackName = case _ of
    Nothing -> "—"
    Just feedback ->
      let
        cents = Int.round feedback.cents
      in
        if cents >= -3 && cents <= 3 then
          "◆ 0¢"
        else if cents > 0 then
          "↓ " <> show cents <> "¢"
        else
          "↑ " <> show (-cents) <> "¢"

  feedbackPosition cents = 50.0 + max (-50.0) (min 50.0 cents)

  quizModeTitle SingingOnly = "Singing"
  quizModeTitle RecognitionOnly = "Recognition"
  quizModeTitle SingingAndRecognition = "Singing & Recognition"
  quizModeTitle Audiation = "Audiation"

  shouldShowIntervalName state =
    state.config.quizMode == Audiation
      || state.config.quizMode == SingingOnly

  promptIntervalLabel state =
    intervalName state.prompt.interval
      <>
        if state.config.quizMode == Audiation || state.config.quizMode == SingingOnly then
          " · " <> playbackModeName state.prompt.mode
        else ""

  feedbackInRange feedback =
    feedback.clarity >= Detection.defaultRecognitionSettings.clarityThreshold
      && feedback.cents >= -Detection.defaultRecognitionSettings.toleranceCents
      && feedback.cents <= Detection.defaultRecognitionSettings.toleranceCents

  handleAction :: Action -> H.HalogenM State Action () PracticeOutput m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      renderPromptNotation state.prompt false
      when (state.config.quizProgression == AutomaticProgression) do
        handleAction PlayPrompt
    Finalize -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
    ToggleInterval interval ->
      updateConfig (toggleInterval interval)
    SelectAllIntervals ->
      updateConfig (_ { intervals = allIntervals })
    ClearIntervals ->
      updateConfig (_ { intervals = [] })
    TogglePlaybackMode mode ->
      updateConfig (togglePlaybackMode mode)
    ToggleRoot root ->
      updateConfig (toggleRootPitchClass root)
    SelectAllRoots ->
      updateConfig (_ { rootPitchClasses = allRootPitchClasses })
    ClearRoots ->
      updateConfig (_ { rootPitchClasses = [] })
    SelectRange preset ->
      updateConfig (_ { vocalRange = preset })
    SelectOctavePolicy policy ->
      updateConfig (_ { octavePolicy = policy })
    SelectGhostMode mode ->
      updateConfig (_ { ghostMode = mode })
    SelectPitchTuner shown ->
      updateConfig (_ { showPitchTuner = shown })
    SelectAnswerCount count ->
      updateConfig (_ { answerCount = count })
    SelectAnswerDisplay display ->
      updateConfig (_ { answerDisplay = display })
    SelectQuizMode mode ->
      updateConfig (_ { quizMode = mode })
    SelectQuizProgression progression ->
      updateConfig (_ { quizProgression = progression })
    BeginPractice -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
      seed <- H.liftEffect (randomInt 0 2147483647)
      sampler <- case state.sampler of
        Just existing -> pure existing
        Nothing -> H.liftEffect Audio.createSampler
      let
        prompt = Quiz.makePrompt seed state.config
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { answerCorrect = false
        , activityRevision = state.activityRevision + 1
        , automaticAdvancePending = false
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostMidi = Nothing
        , ghostRevision = state.ghostRevision + 1
        , prompt = prompt
        , promptRevision = state.promptRevision + 1
        , recognition = Detection.initialRecognition
        , revealedChoices = []
        , resumeAnswersAfterPlayback = false
        , sampler = Just sampler
        , screen = Practice
        }
      renderPromptNotation prompt false
      when (state.config.quizProgression == AutomaticProgression) do
        handleAction PlayPrompt
    PlayPrompt -> do
      state <- H.get
      stopMonitor state.monitor
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> do
          let activityRevision = state.activityRevision + 1
          H.modify_ _
            { activityRevision = activityRevision
            , automaticAdvancePending = false
            , captureStatus = PlayingAudio
            , ghostMidi = Nothing
            , ghostRevision = state.ghostRevision + 1
            , monitor = Nothing
            , recognition = Detection.initialRecognition
            , resumeAnswersAfterPlayback = state.captureStatus == ChoosingAnswer
            }
          renderPromptNotation state.prompt
            (state.captureStatus == ChoosingAnswer && quizModeUsesSinging state.config.quizMode)
          { emitter, listener } <- H.liftEffect HS.create
          void (H.subscribe emitter)
          if state.config.quizMode == Audiation then
            H.liftEffect $ Audio.playRoot sampler state.prompt.root
              (HS.notify listener (PlaybackStarted activityRevision))
              (HS.notify listener <<< AudioFailed activityRevision)
          else
            H.liftEffect $ Audio.playInterval sampler state.prompt.mode state.prompt.root state.prompt.target
              (HS.notify listener (PlaybackStarted activityRevision))
              (HS.notify listener <<< AudioFailed activityRevision)
    PlaybackStarted activityRevision -> do
      state <- H.get
      when
        ( state.screen == Practice
            && state.captureStatus == PlayingAudio
            && state.activityRevision == activityRevision
        )
        do
          void $ H.fork do
            let
              playbackMilliseconds =
                if state.config.quizMode == Audiation then Audio.rootPlaybackDurationMilliseconds
                else Audio.playbackDurationMilliseconds state.prompt.mode
            H.liftAff (delay (Milliseconds (playbackMilliseconds + 350.0)))
            handleAction (StartListening activityRevision)
    AudioFailed activityRevision message -> do
      state <- H.get
      when (state.activityRevision == activityRevision) do
        H.modify_ _ { captureStatus = PlaybackFailed message }
    StartListening activityRevision -> do
      state <- H.get
      when
        ( state.screen == Practice
            && state.captureStatus == PlayingAudio
            && state.activityRevision == activityRevision
        )
        do
          if state.resumeAnswersAfterPlayback || not (quizModeUsesSinging state.config.quizMode) then do
            H.modify_ _ { captureStatus = ChoosingAnswer, resumeAnswersAfterPlayback = false }
            renderChoiceNotation state.prompt.root state.choices
          else do
            { emitter, listener } <- H.liftEffect HS.create
            void (H.subscribe emitter)
            monitor <- H.liftEffect $ Detection.start
              (HS.notify listener <<< PitchDetected activityRevision)
              (HS.notify listener <<< MicrophoneFailed activityRevision)
            H.modify_ _ { captureStatus = Listening, monitor = Just monitor }
    PitchDetected activityRevision sample -> do
      state <- H.get
      when (state.captureStatus == Listening && state.activityRevision == activityRevision) do
        let
          detectedMidi =
            if sample.frequency > 0.0 then Just (Detection.nearestMidi sample.frequency) else Nothing
          next = Detection.stepRecognition
            Detection.defaultRecognitionSettings
            state.config.octavePolicy
            state.prompt.root
            state.prompt.target
            sample
            state.recognition
          detectedGhost =
            if state.config.ghostMode == GhostOff then Nothing
            else map (Detection.relativeMidi state.config.octavePolicy state.prompt.root next) detectedMidi
          nextGhost = case detectedGhost of
            Nothing -> state.ghostMidi
            Just midi -> Just midi
          revision = if detectedGhost == Nothing then state.ghostRevision else state.ghostRevision + 1
          completed =
            state.recognition.phase /= Detection.RecognitionComplete
              && next.phase == Detection.RecognitionComplete
          incorrect =
            state.recognition.phase /= Detection.RecognitionIncorrect
              && next.phase == Detection.RecognitionIncorrect
          firstAccepted = next.firstMidi /= Nothing
          firstJustAccepted = state.recognition.firstMidi == Nothing && firstAccepted
        H.modify_ _ { ghostMidi = nextGhost, ghostRevision = revision, recognition = next }
        when ((detectedGhost /= Nothing && detectedGhost /= state.ghostMidi) || firstJustAccepted) do
          case nextGhost of
            Just midi ->
              let
                spellingReference = case state.prompt.root, state.prompt.target of
                  Pitch (PitchClass _ (Accidental rootAccidental)) _,
                  Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
                    if rootAccidental /= 0 then state.prompt.root
                    else if targetAccidental /= 0 then state.prompt.target
                    else state.prompt.root
              in
                renderGhostNotation state.prompt (pitchFromMidiLike spellingReference midi) firstAccepted
            Nothing -> renderPromptNotation state.prompt firstAccepted
        when
          ( state.config.ghostMode == GhostOn
              && detectedGhost == Nothing
              && state.ghostMidi /= Nothing
          )
          do
            void $ H.fork do
              H.liftAff (delay (Milliseconds 700.0))
              handleAction (ClearGhost revision)
        when completed do
          stopMonitor state.monitor
          H.modify_ _ { monitor = Nothing }
          handleAction (FinishSinging revision)
        when incorrect do
          stopMonitor state.monitor
          H.modify_ _
            { captureStatus = IntervalError
            , monitor = Nothing
            , recognition = next
            }
          case next.feedback of
            Just feedback -> do
              let
                midi = Detection.relativeMidi state.config.octavePolicy state.prompt.root next feedback.midi
                spellingReference = case state.prompt.root, state.prompt.target of
                  Pitch (PitchClass _ (Accidental rootAccidental)) _,
                  Pitch (PitchClass _ (Accidental targetAccidental)) _ ->
                    if rootAccidental /= 0 then state.prompt.root
                    else if targetAccidental /= 0 then state.prompt.target
                    else state.prompt.root
              renderIncorrectNotation
                state.prompt
                (pitchFromMidiLike spellingReference midi)
                firstAccepted
            Nothing -> pure unit
          scheduleAutomaticRetry state
    ClearGhost revision -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.ghostRevision == revision
            && state.ghostMidi /= Nothing
        )
        do
          when (state.config.ghostMode == GhostOn) do
            H.modify_ _ { ghostMidi = Nothing }
            renderPromptNotation state.prompt (state.recognition.phase /= Detection.WaitingForFirst)
    FinishSinging revision -> do
      state <- H.get
      when
        ( state.captureStatus == Listening
            && state.recognition.phase == Detection.RecognitionComplete
            && state.ghostRevision == revision
        )
        do
          let persistGhost = state.config.ghostMode == GhostPersist
          if not (quizModeUsesRecognition state.config.quizMode) then do
            H.modify_ _ { captureStatus = AnswerComplete, ghostMidi = Nothing }
            renderCompletedNotation state.prompt
            scheduleAutomaticAdvance state
          else do
            H.modify_ _
              { captureStatus = ChoosingAnswer
              , ghostMidi = if persistGhost then state.ghostMidi else Nothing
              }
            unless persistGhost (renderPromptNotation state.prompt true)
            renderChoiceNotation state.prompt.root state.choices
    MicrophoneFailed activityRevision message -> do
      state <- H.get
      when (state.activityRevision == activityRevision) do
        H.modify_ _ { captureStatus = CaptureFailed message, monitor = Nothing }
    ChooseInterval interval -> do
      state <- H.get
      when (state.captureStatus == ChoosingAnswer && not (Array.elem interval state.revealedChoices)) do
        case state.sampler, Array.find (\choice -> choice.interval == interval) state.choices of
          Just sampler, Just choice ->
            H.liftEffect $ Audio.playInterval sampler state.prompt.mode state.prompt.root choice.target
              (pure unit)
              (\_ -> pure unit)
          _, _ -> pure unit
        let
          correct = interval == state.prompt.interval
          revealed = Array.snoc state.revealedChoices interval
        H.modify_ _
          { answerCorrect = correct
          , captureStatus = if correct then AnswerComplete else ChoosingAnswer
          , revealedChoices = revealed
          }
        when correct (scheduleAutomaticAdvance state)
    NextPrompt -> do
      state <- H.get
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
      seed <- H.liftEffect (randomInt 0 2147483647)
      let
        prompt = Quiz.makePrompt seed state.config
        choices = Quiz.makeChoices seed state.config prompt
      H.modify_ _
        { answerCorrect = false
        , activityRevision = state.activityRevision + 1
        , automaticAdvancePending = false
        , captureStatus = ReadyToPlay
        , choices = choices
        , ghostMidi = Nothing
        , ghostRevision = state.ghostRevision + 1
        , prompt = prompt
        , promptRevision = state.promptRevision + 1
        , recognition = Detection.initialRecognition
        , revealedChoices = []
        , resumeAnswersAfterPlayback = false
        }
      resetPracticeScroll
      renderPromptNotation prompt false
    AdvanceAutomatically revision -> do
      state <- H.get
      when
        ( state.screen == Practice
            && state.automaticAdvancePending
            && state.promptRevision == revision
            && state.captureStatus == AnswerComplete
        )
        do
          handleAction NextPrompt
          handleAction PlayPrompt
    RetryAutomatically revision -> do
      state <- H.get
      when
        ( state.screen == Practice
            && state.automaticAdvancePending
            && state.promptRevision == revision
            && state.captureStatus == IntervalError
        )
        do
          handleAction PlayPrompt
    EditSetup -> do
      H.raise BackToSetup

  stopMonitor = case _ of
    Nothing -> pure unit
    Just monitor -> H.liftEffect (Detection.stop monitor)

  resetPracticeScroll = do
    maybeElement <- H.getHTMLElementRef practiceContentRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement -> H.liftEffect (Element.setScrollTop 0.0 (HTMLElement.toElement htmlElement))

  updateConfig update = do
    H.modify_ \state -> state { config = update state.config }

  scheduleAutomaticAdvance state =
    when (state.config.quizProgression == AutomaticProgression) do
      H.modify_ _ { automaticAdvancePending = true }
      void $ H.fork do
        let
          waitMilliseconds =
            if not (quizModeUsesRecognition state.config.quizMode) then 1200.0
            else Audio.playbackDurationMilliseconds state.prompt.mode + 500.0
        H.liftAff (delay (Milliseconds waitMilliseconds))
        handleAction (AdvanceAutomatically state.promptRevision)

  scheduleAutomaticRetry state =
    when (state.config.quizProgression == AutomaticProgression) do
      H.modify_ _ { automaticAdvancePending = true }
      void $ H.fork do
        H.liftAff (delay (Milliseconds 1200.0))
        handleAction (RetryAutomatically state.promptRevision)

  renderPromptNotation prompt rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderPrompt (HTMLElement.toElement htmlElement) prompt.root prompt.target rootAccepted)

  renderCompletedNotation prompt = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderCompleted (HTMLElement.toElement htmlElement) prompt.root prompt.target)

  renderGhostNotation prompt detected rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect (Notation.renderGhost (HTMLElement.toElement htmlElement) prompt.root prompt.target detected rootAccepted)

  renderIncorrectNotation prompt detected rootAccepted = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement ->
        H.liftEffect
          (Notation.renderIncorrect (HTMLElement.toElement htmlElement) prompt.root prompt.target detected rootAccepted)

  renderChoiceNotation root choices =
    for_ (Array.mapWithIndex (\index choice -> { choice, index }) choices) \item -> do
      maybeElement <- H.getHTMLElementRef (choiceNotationRef item.index)
      case maybeElement of
        Nothing -> pure unit
        Just htmlElement ->
          H.liftEffect (Notation.renderIntervalChoice (HTMLElement.toElement htmlElement) root item.choice.target)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body
