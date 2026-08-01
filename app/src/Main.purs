module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import EarTrainer.Audio as Audio
import EarTrainer.Config (ExerciseConfig, defaultConfig, isValid, toggleInterval, togglePlaybackMode, toggleRootPitchClass)
import EarTrainer.Music
  ( Accidental(..)
  , Direction(..)
  , Interval(..)
  , Letter(..)
  , OctavePolicy(..)
  , Pitch(..)
  , PitchClass(..)
  , PlaybackMode(..)
  , VocalRangePreset
  , allIntervals
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , midiNumber
  , pitchClassName
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  , transpose
  )
import EarTrainer.Notation as Notation
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement as HTMLElement

data Screen = Setup | Practice

derive instance Eq Screen

type Prompt =
  { interval :: Interval
  , mode :: PlaybackMode
  , root :: Pitch
  , target :: Pitch
  }

type State =
  { config :: ExerciseConfig
  , prompt :: Prompt
  , sampler :: Maybe Audio.Sampler
  , screen :: Screen
  }

data Action
  = ToggleInterval Interval
  | TogglePlaybackMode PlaybackMode
  | ToggleRoot PitchClass
  | SelectRange VocalRangePreset
  | SelectOctavePolicy OctavePolicy
  | BeginPractice
  | PlayPrompt
  | EditSetup

notationRef :: H.RefLabel
notationRef = H.RefLabel "prompt-notation"

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState =
    { config: defaultConfig
    , prompt: makePrompt defaultConfig
    , sampler: Nothing
    , screen: Setup
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.main
      [ HP.class_ (H.ClassName "app-shell") ]
      [ HH.header
          [ HP.class_ (H.ClassName "hero") ]
          [ HH.p
              [ HP.class_ (H.ClassName "eyebrow") ]
              [ HH.text "Interval practice" ]
          , HH.h1_ [ HH.text "Train the space between notes." ]
          , HH.p
              [ HP.class_ (H.ClassName "lede") ]
              [ HH.text "Hear an interval, sing both notes, then connect what you sang to notation." ]
          ]
      , if state.screen == Setup then renderSetup state else renderPractice state
      ]

  renderSetup state =
    HH.section
      [ HP.class_ (H.ClassName "setup-card") ]
      [ HH.div
          [ HP.class_ (H.ClassName "setup-heading") ]
          [ HH.div_
              [ HH.p
                  [ HP.class_ (H.ClassName "step-label") ]
                  [ HH.text "Exercise setup" ]
              , HH.h2_ [ HH.text "Choose what to practice" ]
              ]
          , HH.p_
              [ HH.text "Select at least one option in each group. These settings stay editable between questions." ]
          ]
      , settingGroup
          "Playback"
          "How the interval is played before the microphone begins listening."
          (map (modeButton state.config) allPlaybackModes)
      , settingGroup
          "Intervals"
          "The interval name is shown here only during setup. Quiz choices use notation."
          (map (intervalButton state.config) allIntervals)
      , settingGroup
          "Singing range"
          "A preset controls the written and playback register; octave-equivalent singing is accepted by default."
          (map (rangeButton state.config) allVocalRangePresets)
      , settingGroup
          "Root notes"
          "Choose the individual pitch classes that may begin an exercise."
          (map (rootButton state.config) allRootPitchClasses)
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
      , HH.footer
          [ HP.class_ (H.ClassName "setup-footer") ]
          [ HH.p
              [ HP.class_ (H.ClassName "setup-message") ]
              [ HH.text
                  if isValid state.config then
                    "Ready to begin."
                  else
                    "Select at least one playback mode, interval, and root note."
              ]
          , HH.button
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
              [ HH.p
                  [ HP.class_ (H.ClassName "step-label") ]
                  [ HH.text "Listen, then sing" ]
              , HH.h2_ [ HH.text (playbackModeName state.prompt.mode) ]
              ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "text-button")
              , HE.onClick \_ -> EditSetup
              ]
              [ HH.text "Edit setup" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "notation-panel") ]
          [ HH.div
              [ HP.ref notationRef
              , HP.class_ (H.ClassName "notation-canvas")
              ]
              []
          , HH.p_
              [ HH.text ("Sing " <> pitchName state.prompt.root <> " first. The second note will appear when pitch detection accepts it.") ]
          ]
      , HH.footer
          [ HP.class_ (H.ClassName "practice-actions") ]
          [ HH.p_
              [ HH.text "Playback stops before microphone input begins." ]
          , HH.button
              [ HP.type_ HP.ButtonButton
              , HP.class_ (H.ClassName "primary-button play-button")
              , HE.onClick \_ -> PlayPrompt
              ]
              [ HH.span
                  [ HP.class_ (H.ClassName "play-icon") ]
                  [ HH.text "▶" ]
              , HH.text "Play interval"
              ]
          ]
      ]

  settingGroup title description controls =
    HH.fieldset
      [ HP.class_ (H.ClassName "setting-group") ]
      [ HH.legend_ [ HH.text title ]
      , HH.p
          [ HP.class_ (H.ClassName "setting-description") ]
          [ HH.text description ]
      , HH.div
          [ HP.class_ (H.ClassName "choice-grid") ]
          controls
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

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ToggleInterval interval ->
      H.modify_ \state -> state { config = toggleInterval interval state.config }
    TogglePlaybackMode mode ->
      H.modify_ \state -> state { config = togglePlaybackMode mode state.config }
    ToggleRoot root ->
      H.modify_ \state -> state { config = toggleRootPitchClass root state.config }
    SelectRange preset ->
      H.modify_ \state -> state { config = state.config { vocalRange = preset } }
    SelectOctavePolicy policy ->
      H.modify_ \state -> state { config = state.config { octavePolicy = policy } }
    BeginPractice -> do
      state <- H.get
      sampler <- case state.sampler of
        Just existing -> pure existing
        Nothing -> H.liftEffect Audio.createSampler
      let prompt = makePrompt state.config
      H.modify_ _ { prompt = prompt, sampler = Just sampler, screen = Practice }
      renderPrompt prompt
    PlayPrompt -> do
      state <- H.get
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.playInterval sampler state.prompt.mode state.prompt.root state.prompt.target)
    EditSetup -> do
      state <- H.get
      case state.sampler of
        Nothing -> pure unit
        Just sampler -> H.liftEffect (Audio.stop sampler)
      H.modify_ _ { screen = Setup }

  renderPrompt prompt = do
    maybeElement <- H.getHTMLElementRef notationRef
    case maybeElement of
      Nothing -> pure unit
      Just htmlElement -> H.liftEffect (Notation.renderNotes (HTMLElement.toElement htmlElement) [ prompt.root ])

makePrompt :: ExerciseConfig -> Prompt
makePrompt config =
  let
    interval = fromMaybe MinorThird (Array.head config.intervals)
    mode = fromMaybe MelodicAscending (Array.head config.playbackModes)
    selectedRoot = fromMaybe (PitchClass C (Accidental 0)) (Array.head config.rootPitchClasses)
    range = presetRange config.vocalRange
    Pitch _ lowOctave = range.low
    lowCandidate = Pitch selectedRoot lowOctave
    root =
      if midiNumber lowCandidate < midiNumber range.low then
        Pitch selectedRoot (lowOctave + 1)
      else
        lowCandidate
    direction = case mode of
      MelodicDescending -> Descending
      _ -> Ascending
  in
    { interval
    , mode
    , root
    , target: transpose direction interval root
    }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
