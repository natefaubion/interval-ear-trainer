module Main where

import Prelude

import Data.Array as Array
import EarTrainer.Config (ExerciseConfig, defaultConfig, isValid, toggleInterval, togglePlaybackMode, toggleRootPitchClass)
import EarTrainer.Music
  ( Interval
  , OctavePolicy(..)
  , PitchClass
  , PlaybackMode
  , VocalRangePreset
  , allIntervals
  , allPlaybackModes
  , allRootPitchClasses
  , allVocalRangePresets
  , intervalName
  , pitchClassName
  , pitchName
  , playbackModeName
  , presetName
  , presetRange
  )
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State =
  { config :: ExerciseConfig
  , ready :: Boolean
  }

data Action
  = ToggleInterval Interval
  | TogglePlaybackMode PlaybackMode
  | ToggleRoot PitchClass
  | SelectRange VocalRangePreset
  | SelectOctavePolicy OctavePolicy
  | BeginPractice

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState: const { config: defaultConfig, ready: false }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
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
      , HH.section
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
                  [ HP.class_ (H.ClassName if state.ready then "setup-message success" else "setup-message") ]
                  [ HH.text
                      if state.ready then
                        "Setup saved. The notation and audio exercise comes next."
                      else if isValid state.config then
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
                  [ HH.text if state.ready then "Setup saved" else "Begin practice" ]
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
      H.modify_ \state -> state { config = toggleInterval interval state.config, ready = false }
    TogglePlaybackMode mode ->
      H.modify_ \state -> state { config = togglePlaybackMode mode state.config, ready = false }
    ToggleRoot root ->
      H.modify_ \state -> state { config = toggleRootPitchClass root state.config, ready = false }
    SelectRange preset ->
      H.modify_ \state -> state { config = state.config { vocalRange = preset }, ready = false }
    SelectOctavePolicy policy ->
      H.modify_ \state -> state { config = state.config { octavePolicy = policy }, ready = false }
    BeginPractice -> H.modify_ _ { ready = true }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
