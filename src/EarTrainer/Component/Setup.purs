module EarTrainer.Component.Setup
  ( Input
  , Output(..)
  , Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import EarTrainer.Config
  ( AnswerCount(..)
  , AnswerDisplay(..)
  , ExerciseConfig
  , GhostMode(..)
  , IntervalSystem(..)
  , QuizMode(..)
  , QuizProgression(..)
  , RangeBoundary(..)
  , defaultConfig
  , quizModeUsesRecognition
  , selectMajorKey
  , selectedMajorKeyId
  , setCustomPitchClass
  , setCustomPitchOctave
  , toggleInterval
  , toggleIntervalSize
  , togglePlaybackMode
  , toggleRootPitchClass
  )
import EarTrainer.Music
  ( Interval
  , IntervalSize
  , OctavePolicy(..)
  , PitchClass
  , PlaybackMode(..)
  , VocalRangePreset(..)
  , allIntervalSizes
  , allIntervals
  , allMajorKeyPresets
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , intervalSizeName
  , midiNumber
  , pitchClassName
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  )
import EarTrainer.Quiz as Quiz
import EarTrainer.Settings as Settings
import EarTrainer.UI.Button as Button
import EarTrainer.UI.Dialog as Dialog
import EarTrainer.UI.SettingGroup as SettingGroup
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLDialogElement as HTMLDialogElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data Output
  = DataChanged Settings.AppData
  | BeginRequested
  | PersistenceRequested

data Query :: Type -> Type
data Query a

type Input =
  { activePresetId :: Maybe String
  , config :: ExerciseConfig
  , presets :: Array Settings.Preset
  , samplerReady :: Boolean
  , storageError :: Maybe String
  }

type State =
  { activePresetId :: Maybe String
  , config :: ExerciseConfig
  , presetName :: String
  , presetNameError :: Maybe String
  , presets :: Array Settings.Preset
  , samplerReady :: Boolean
  , storageError :: Maybe String
  }

data Action
  = RootToggleInterval Interval
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
  | RootReceiveSetup Input

savePresetDialogRef :: H.RefLabel
savePresetDialogRef = H.RefLabel "save-preset-dialog"

deletePresetDialogRef :: H.RefLabel
deletePresetDialogRef = H.RefLabel "delete-preset-dialog"

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input ->
        { activePresetId: input.activePresetId
        , config: input.config
        , presetName: ""
        , presetNameError: Nothing
        , presets: input.presets
        , samplerReady: input.samplerReady
        , storageError: input.storageError
        }
    , render
    , eval:
        H.mkEval H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< RootReceiveSetup
          }
    }
  where

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.h2_ [ HH.text "Exercise setup" ]
          , Button.button
              { action: RootOpenSavePreset
              , classes: [ H.ClassName "save-preset-button" ]
              , disabled: false
              , variant: Button.Secondary
              }
              [ HH.text "Save preset" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "setup-content") ]
          [ case state.storageError of
              Nothing -> HH.text ""
              Just message -> HH.p [ HP.class_ (H.ClassName "storage-error") ] [ HH.text message ]
          , if Array.null state.presets then HH.text ""
            else
              SettingGroup.settingGroup
                { description: "Apply a saved exercise setup."
                , title: "Presets"
                , validation: Nothing
                }
                ( [ rootChoiceButton (isNothing state.activePresetId) (RootSelectPreset "") "Custom" ]
                    <> map
                      (\preset -> rootChoiceButton (state.activePresetId == Just preset.id) (RootSelectPreset preset.id) preset.name)
                      state.presets
                    <>
                      [ HH.div
                          [ HP.class_ (H.ClassName "selection-actions") ]
                          [ Button.button
                              { action: RootOpenDeletePreset
                              , classes: []
                              , disabled: isNothing state.activePresetId
                              , variant: Button.SmallText
                              }
                              [ HH.text "Delete preset" ]
                          ]
                      ]
                )
          , SettingGroup.settingGroup
              { description: "Choose which parts of the exercise to practice."
              , title: "Quiz mode"
              , validation: Nothing
              }
              [ rootChoiceButton
                  (state.config.quizMode == SingingAndRecognition)
                  (RootSelectQuizMode SingingAndRecognition)
                  "Singing and recognition"
              , rootChoiceButton (state.config.quizMode == SingingOnly) (RootSelectQuizMode SingingOnly) "Singing only"
              , rootChoiceButton (state.config.quizMode == RecognitionOnly) (RootSelectQuizMode RecognitionOnly) "Recognition only"
              , rootChoiceButton (state.config.quizMode == Audiation) (RootSelectQuizMode Audiation) "Audiation"
              ]
          , SettingGroup.settingGroup
              { description: "Choose how the next interval begins after a correct answer."
              , title: "Quiz progression"
              , validation: Nothing
              }
              [ rootChoiceButton
                  (state.config.quizProgression == AutomaticProgression)
                  (RootSelectQuizProgression AutomaticProgression)
                  "Automatic"
              , rootChoiceButton
                  (state.config.quizProgression == ManualProgression)
                  (RootSelectQuizProgression ManualProgression)
                  "Manual"
              ]
          , if quizModeUsesRecognition state.config.quizMode then
              SettingGroup.settingGroup
                { description: "Choose how many interval choices are shown."
                , title: "Number of available answers"
                , validation: Nothing
                }
                [ rootChoiceButton (state.config.answerCount == AFew) (RootSelectAnswerCount AFew) "A few"
                , rootChoiceButton
                    (state.config.answerCount == AllSelected)
                    (RootSelectAnswerCount AllSelected)
                    "All selected intervals"
                ]
            else HH.text ""
          , if quizModeUsesRecognition state.config.quizMode then
              SettingGroup.settingGroup
                { description: "Choose how interval choices are presented."
                , title: "Answer display"
                , validation: Nothing
                }
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
            else HH.text ""
          , SettingGroup.settingGroup
              { description: "Choose which notes may begin an exercise, or use a major-key preset."
              , title: "Note selection"
              , validation: if Array.null state.config.rootPitchClasses then Just "Select at least one note." else Nothing
              }
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
          , SettingGroup.settingGroup
              { description: "Choose exact interval qualities or derive their qualities from the selected notes."
              , title: "Interval system"
              , validation: Nothing
              }
              [ rootChoiceButton
                  (state.config.intervalSystem == FromSelectedNotes)
                  (RootSelectIntervalSystem FromSelectedNotes)
                  "From selected notes"
              , rootChoiceButton
                  (state.config.intervalSystem == ExactIntervals)
                  (RootSelectIntervalSystem ExactIntervals)
                  "Exact intervals"
              ]
          , let
              possibleExactIntervals = Quiz.availableExactIntervals state.config
              possibleSizes = Quiz.availableIntervalSizes state.config
              selectedPossibleExactIntervals = Array.filter (flip Array.elem possibleExactIntervals) state.config.intervals
              selectedPossibleSizes = Array.filter (flip Array.elem possibleSizes) state.config.availableIntervals
            in
              SettingGroup.settingGroup
                { description:
                    if state.config.intervalSystem == ExactIntervals then
                      "Choose the exact intervals that may appear in an exercise."
                    else
                      "Choose the basic intervals that may appear in an exercise. Each exact quality is determined by the selected note pair."
                , title: "Intervals"
                , validation:
                    if state.config.intervalSystem == ExactIntervals && Array.null selectedPossibleExactIntervals then
                      Just "Select at least one interval available in this range."
                    else if state.config.intervalSystem == FromSelectedNotes && Array.null selectedPossibleSizes then
                      Just "Select at least one interval available from these notes."
                    else Nothing
                }
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
          , let
              availableOrientations =
                if state.config.quizMode == Audiation then
                  Array.filter (_ /= Harmonic) allPlaybackModes
                else allPlaybackModes
              selectedOrientations = Array.filter (flip Array.elem state.config.playbackModes) availableOrientations
            in
              SettingGroup.settingGroup
                { description: "Choose the directions or forms intervals may take."
                , title: "Interval orientation"
                , validation: if Array.null selectedOrientations then Just "Select at least one interval orientation." else Nothing
                }
                (map (rootModeButton state.config) availableOrientations)
          , SettingGroup.settingGroup
              { description: "Choose the written and playback register."
              , title: "Playback range"
              , validation:
                  if
                    state.config.vocalRange == Custom
                      && midiNumber state.config.customRange.low > midiNumber state.config.customRange.high then
                    Just "The lowest note must not be above the highest note."
                  else Nothing
              }
              ( map (rootRangeButton state.config) allVocalRangePresets
                  <> if state.config.vocalRange == Custom then [ rootCustomRangeControls state.config ] else []
              )
          , SettingGroup.settingGroup
              { description: "Choose whether the first sung note must match the written register. The second note must always form the written interval from it."
              , title: "Octave matching"
              , validation: Nothing
              }
              [ rootChoiceButton
                  (state.config.octavePolicy == AnyOctave)
                  (RootSelectOctavePolicy AnyOctave)
                  "Any octave"
              , rootChoiceButton
                  (state.config.octavePolicy == WrittenOctave)
                  (RootSelectOctavePolicy WrittenOctave)
                  "Written octave"
              ]
          , SettingGroup.settingGroup
              { description: "Choose how the pitch you are currently singing appears on the staff."
              , title: "Sung pitch on staff"
              , validation: Nothing
              }
              [ rootChoiceButton (state.config.ghostMode == GhostOn) (RootSelectGhostMode GhostOn) "Show briefly"
              , rootChoiceButton
                  (state.config.ghostMode == GhostPersist)
                  (RootSelectGhostMode GhostPersist)
                  "Keep visible"
              , rootChoiceButton (state.config.ghostMode == GhostOff) (RootSelectGhostMode GhostOff) "Hidden"
              ]
          , SettingGroup.settingGroup
              { description: "Choose whether to show cents feedback while singing."
              , title: "Pitch tuner"
              , validation: Nothing
              }
              [ rootChoiceButton state.config.showPitchTuner (RootSelectPitchTuner true) "Shown"
              , rootChoiceButton (not state.config.showPitchTuner) (RootSelectPitchTuner false) "Hidden"
              ]

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
          [ Button.button
              { action: RootResetDefaults
              , classes: []
              , disabled: state.config == defaultConfig
              , variant: Button.Secondary
              }
              [ HH.text "Reset to defaults" ]
          , Button.button
              { action: RootBeginPractice
              , classes: []
              , disabled: not (Quiz.isPlayable state.config) || not state.samplerReady
              , variant: Button.Primary
              }
              [ HH.text "Begin practice" ]
          ]
      , renderSavePresetDialog state
      , renderDeletePresetDialog state
      ]

  renderSavePresetDialog state =
    Dialog.dialog
      { classes: [ H.ClassName "preset-dialog" ]
      , labelledBy: "save-preset-title"
      , ref: savePresetDialogRef
      }
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
          [ Button.button
              { action: RootCloseSavePreset, classes: [], disabled: false, variant: Button.Secondary }
              [ HH.text "Cancel" ]
          , Button.button
              { action: RootSavePreset, classes: [], disabled: false, variant: Button.Primary }
              [ HH.text "Save" ]
          ]
      ]

  renderDeletePresetDialog state =
    Dialog.dialog
      { classes: [ H.ClassName "preset-dialog" ]
      , labelledBy: "delete-preset-title"
      , ref: deletePresetDialogRef
      }
      [ HH.h3
          [ HP.id "delete-preset-title" ]
          [ HH.text case activePreset state of
              Nothing -> "Delete preset?"
              Just preset -> "Delete “" <> preset.name <> "”?"
          ]
      , HH.div
          [ HP.class_ (H.ClassName "dialog-actions") ]
          [ Button.button
              { action: RootCloseDeletePreset, classes: [], disabled: false, variant: Button.Secondary }
              [ HH.text "Cancel" ]
          , Button.button
              { action: RootConfirmDeletePreset
              , classes: [ H.ClassName "danger-button" ]
              , disabled: isNothing state.activePresetId
              , variant: Button.Primary
              }
              [ HH.text "Delete" ]
          ]
      ]

  activePreset state = case state.activePresetId of
    Nothing -> Nothing
    Just id -> Array.find (\preset -> preset.id == id) state.presets

  rootChoiceButton selected action label =
    Button.button
      { action, classes: [], disabled: false, variant: Button.Choice selected }
      [ HH.text label ]

  rootSelectionActions selectAction clearAction selectDisabled clearDisabled =
    HH.div
      [ HP.class_ (H.ClassName "selection-actions") ]
      [ Button.button
          { action: selectAction, classes: [], disabled: selectDisabled, variant: Button.SmallText }
          [ HH.text "Select All" ]
      , Button.button
          { action: clearAction, classes: [], disabled: clearDisabled, variant: Button.SmallText }
          [ HH.text "Clear" ]
      ]

  rootModeButton config mode =
    rootChoiceButton (Array.elem mode config.playbackModes) (RootTogglePlaybackMode mode) (playbackModeName mode)

  rootIntervalButton config possible interval =
    Button.button
      { action: RootToggleInterval interval
      , classes: []
      , disabled: not (Array.elem interval possible)
      , variant: Button.Choice (Array.elem interval config.intervals)
      }
      [ HH.text (intervalName interval) ]

  rootIntervalSizeButton config possible interval =
    Button.button
      { action: RootToggleIntervalSize interval
      , classes: []
      , disabled: not (Array.elem interval possible)
      , variant: Button.Choice (Array.elem interval config.availableIntervals)
      }
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

  handleAction = case _ of
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
    RootSelectCustomLowClass pitchClass -> setupUpdateConfig (setCustomPitchClass Lowest pitchClass)
    RootSelectCustomLowOctave octave -> setupUpdateConfig (setCustomPitchOctave Lowest octave)
    RootSelectCustomHighClass pitchClass -> setupUpdateConfig (setCustomPitchClass Highest pitchClass)
    RootSelectCustomHighOctave octave -> setupUpdateConfig (setCustomPitchOctave Highest octave)
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
    RootPresetKeyDown key -> when (key == "Enter") (handleAction RootSavePreset)
    RootSavePreset -> do
      state <- H.get
      case Settings.validatePresetName state.presets state.presetName of
        Left Settings.EmptyName ->
          H.modify_ _ { presetNameError = Just "Enter a preset name." }
        Left Settings.DuplicateName ->
          H.modify_ _ { presetNameError = Just "A preset with this name already exists." }
        Right validName -> do
          let name = Settings.presetName validName
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
          H.raise (DataChanged appData)
          H.raise PersistenceRequested
    RootSelectPreset id -> do
      state <- H.get
      if id == "" then do
        H.modify_ _ { activePresetId = Nothing }
        H.raise (DataChanged { activePresetId: Nothing, config: state.config, presets: state.presets })
      else case Array.find (\preset -> preset.id == id) state.presets of
        Nothing -> pure unit
        Just preset -> do
          H.modify_ _ { activePresetId = Just id, config = preset.config }
          H.raise (DataChanged { activePresetId: Just id, config: preset.config, presets: state.presets })
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
          H.raise (DataChanged appData)
    RootResetDefaults -> setupUpdateConfig (const defaultConfig)
    RootBeginPractice -> H.raise BeginRequested
    RootReceiveSetup input -> H.modify_ _
      { activePresetId = input.activePresetId
      , config = input.config
      , presets = input.presets
      , samplerReady = input.samplerReady
      , storageError = input.storageError
      }
  setupUpdateConfig update = do
    H.modify_ \state -> state { activePresetId = Nothing, config = update state.config }
    state <- H.get
    H.raise (DataChanged { activePresetId: Nothing, config: state.config, presets: state.presets })

  showDialog ref = do
    maybeElement <- H.getHTMLElementRef ref
    for_ (maybeElement >>= HTMLDialogElement.fromHTMLElement) \dialog ->
      H.liftEffect (HTMLDialogElement.showModal dialog)

  closeDialog ref = do
    maybeElement <- H.getHTMLElementRef ref
    for_ (maybeElement >>= HTMLDialogElement.fromHTMLElement) \dialog ->
      H.liftEffect (HTMLDialogElement.close Nothing dialog)
